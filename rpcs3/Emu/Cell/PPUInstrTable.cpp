#include "stdafx.h"

// Aren't these just PPC opcodes?
#include "Emu/Cell/PPUOpcodes.h"
#include "Emu/Cell/PPUInstrTable.h"
#include "Emu/Cell/PPUDisAsm.h"
#include "Emu/Cell/PPUInterpreter.h"

// Design plan:
//   Get to the interp/jit/etc. func as quick as possible.
//   Individual interp/jit/disasm funcs should be short and direct as possible.
//   Needs to take native byte order - too many args to win by using BE.

class PPUInterpreter;

namespace PPU_instr
{
	inline static u8 FlipShift(u8 v)
	{
		return 31 - v;
	}

	inline static u32 MakeMask(u8 from, u8 to)
	{
		const u8 numBits = to - from + 1;
		return (1 << numBits) - 1;
	}

	struct InstructionTable
	{
		const u8 shift;
		const u32 mask;
		std::vector<Instruction> instructions;

		// From / to in big endian order.
		InstructionTable(u8 from, u8 to, const InstructionTable *fullTable = nullptr);

		void DoAdd(u32 index, const Instruction &&instr);

		const Instruction *Lookup(Opcode op) const
		{
			return &instructions[(op >> shift) & mask];
		}

		const Instruction *operator[](u32 index) const
		{
			return &instructions[index];
		}
	};

	Instruction::Instruction()
		: subTable(nullptr), interpret(nullptr), compile(nullptr), disasm(nullptr), name(), flags(0)
	{
	}

	Instruction::Instruction(const Instruction &other)
		: subTable(other.subTable), interpret(other.interpret),
			compile(other.compile), disasm(other.disasm), name(other.name), flags(other.flags)
	{
	}

	Instruction::Instruction(InterpretFunc i, CompileFunc c, DisasmFunc d, const std::string &n, u32 f)
		: subTable(nullptr), interpret(i), compile(c), disasm(d), name(n), flags(f)
	{
	}

	Instruction::Instruction(const InstructionTable *t)
		: subTable(t), interpret(nullptr), compile(nullptr), disasm(nullptr), name(), flags(0)
	{
	}

	inline static Instruction Invalid()
	{
		// TODO: Could link to unimpl funcs here, then would always have a func.
		return Instruction();
	}

	inline static Instruction Table(const InstructionTable *subTable)
	{
		return Instruction(subTable);
	}

	void InstructionTable::DoAdd(u32 index, const Instruction &&instr)
	{
		instructions[index] = instr;
	}

	InstructionTable::InstructionTable(u8 from, u8 to, const InstructionTable *fullTable)
		: shift(FlipShift(to)), mask(MakeMask(from, to))
	{
		// This will truncate/pad with invalid (or the larger lookup table, if specified.)
		instructions.resize(mask + 1, fullTable != nullptr ? Table(fullTable) : Invalid());
	}

	template <void (PPUInterpreter::*Interpret)(), void (PPUCompiler::*Compile)(), void (PPUDisAsm::*Disasm)()>
	inline static Instruction Instr(const std::string &n, u32 flags = 0)
	{
		const InterpretFunc interpret = &PPUInterpreter::InstructionProxy<PPUInterpreter, Interpret>;
		const CompileFunc compile = &PPUCompiler::InstructionProxy<PPUCompiler, Compile>;
		const DisasmFunc disasm = &PPUDisAsm::InstructionProxy<PPUDisAsm, Disasm>;
		return Instruction(interpret, compile, disasm, n, flags);
	}

	template <typename A0, void (PPUInterpreter::*Interpret)(typename A0::Type), void (PPUCompiler::*Compile)(typename A0::Type), void (PPUDisAsm::*Disasm)(typename A0::Type)>
	inline static Instruction Instr(const std::string &n, u32 flags = 0)
	{
		flags |= A0::flags;
		const InterpretFunc interpret = &PPUInterpreter::InstructionProxy<A0, PPUInterpreter, Interpret>;
		const CompileFunc compile = &PPUCompiler::InstructionProxy<A0, PPUCompiler, Compile>;
		const DisasmFunc disasm = &PPUDisAsm::InstructionProxy<A0, PPUDisAsm, Disasm>;
		return Instruction(interpret, compile, disasm, n, flags);
	}

	template <typename A0, typename A1, void (PPUInterpreter::*Interpret)(typename A0::Type, typename A1::Type), void (PPUCompiler::*Compile)(typename A0::Type, typename A1::Type), void (PPUDisAsm::*Disasm)(typename A0::Type, typename A1::Type)>
	inline static Instruction Instr(const std::string &n, u32 flags = 0)
	{
		flags |= A0::flags | A1::flags;
		const InterpretFunc interpret = &PPUInterpreter::InstructionProxy<A0, A1, PPUInterpreter, Interpret>;
		const CompileFunc compile = &PPUCompiler::InstructionProxy<A0, A1, PPUCompiler, Compile>;
		const DisasmFunc disasm = &PPUDisAsm::InstructionProxy<A0, A1, PPUDisAsm, Disasm>;
		return Instruction(interpret, compile, disasm, n, flags);
	}

	template <typename A0, typename A1, typename A2, void (PPUInterpreter::*Interpret)(typename A0::Type, typename A1::Type, typename A2::Type), void (PPUCompiler::*Compile)(typename A0::Type, typename A1::Type, typename A2::Type), void (PPUDisAsm::*Disasm)(typename A0::Type, typename A1::Type, typename A2::Type)>
	inline static Instruction Instr(const std::string &n, u32 flags = 0)
	{
		flags |= A0::flags | A1::flags | A2::flags;
		const InterpretFunc interpret = &PPUInterpreter::InstructionProxy<A0, A1, A2, PPUInterpreter, Interpret>;
		const CompileFunc compile = &PPUCompiler::InstructionProxy<A0, A1, A2, PPUCompiler, Compile>;
		const DisasmFunc disasm = &PPUDisAsm::InstructionProxy<A0, A1, A2, PPUDisAsm, Disasm>;
		return Instruction(interpret, compile, disasm, n, flags);
	}

	template <typename A0, typename A1, typename A2, typename A3, void (PPUInterpreter::*Interpret)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type), void (PPUCompiler::*Compile)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type), void (PPUDisAsm::*Disasm)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type)>
	inline static Instruction Instr(const std::string &n, u32 flags = 0)
	{
		flags |= A0::flags | A1::flags | A2::flags | A3::flags;
		const InterpretFunc interpret = &PPUInterpreter::InstructionProxy<A0, A1, A2, A3, PPUInterpreter, Interpret>;
		const CompileFunc compile = &PPUCompiler::InstructionProxy<A0, A1, A2, A3, PPUCompiler, Compile>;
		const DisasmFunc disasm = &PPUDisAsm::InstructionProxy<A0, A1, A2, A3, PPUDisAsm, Disasm>;
		return Instruction(interpret, compile, disasm, n, flags);
	}

	template <typename A0, typename A1, typename A2, typename A3, typename A4, void (PPUInterpreter::*Interpret)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type, typename A4::Type), void (PPUCompiler::*Compile)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type, typename A4::Type), void (PPUDisAsm::*Disasm)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type, typename A4::Type)>
	inline static Instruction Instr(const std::string &n, u32 flags = 0)
	{
		flags |= A0::flags | A1::flags | A2::flags | A3::flags | A4::flags;
		const InterpretFunc interpret = &PPUInterpreter::InstructionProxy<A0, A1, A2, A3, A4, PPUInterpreter, Interpret>;
		const CompileFunc compile = &PPUCompiler::InstructionProxy<A0, A1, A2, A3, A4, PPUCompiler, Compile>;
		const DisasmFunc disasm = &PPUDisAsm::InstructionProxy<A0, A1, A2, A3, A4, PPUDisAsm, Disasm>;
		return Instruction(interpret, compile, disasm, n, flags);
	}

	template <typename A0, typename A1, typename A2, typename A3, typename A4, typename A5, void (PPUInterpreter::*Interpret)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type, typename A4::Type, typename A5::Type), void (PPUCompiler::*Compile)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type, typename A4::Type, typename A5::Type), void (PPUDisAsm::*Disasm)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type, typename A4::Type, typename A5::Type)>
	inline static Instruction Instr(const std::string &n, u32 flags = 0)
	{
		flags |= A0::flags | A1::flags | A2::flags | A3::flags | A4::flags | A5::flags;
		const InterpretFunc interpret = &PPUInterpreter::InstructionProxy<A0, A1, A2, A3, A4, A5, PPUInterpreter, Interpret>;
		const CompileFunc compile = &PPUCompiler::InstructionProxy<A0, A1, A2, A3, A4, A5, PPUCompiler, Compile>;
		const DisasmFunc disasm = &PPUDisAsm::InstructionProxy<A0, A1, A2, A3, A4, A5, PPUDisAsm, Disasm>;
		return Instruction(interpret, compile, disasm, n, flags);
	}

	// Bits from an opcode.
	template <u8 from, u8 to = from, u32 flags = 0>
	struct CodeField
	{
		static const u32 size = to - from + 1;
		static const u32 shift = 31 - to;
		static const u32 mask = (1 << (to - from + 1)) - 1;
		static const u32 flags = flags;

		typedef u32 Type;

		static Type Decode(Opcode data)
		{
			return (data >> shift) & mask;
		}
	};

	// Single bit flag from an opcode.
	template <u8 bit, u32 flags = 0>
	struct CodeBit : public CodeField<bit, bit, flags>
	{
		typedef bool Type;

		static Type Decode(Opcode data)
		{
			return CodeField<bit, bit, flags>::Decode(data) != 0;
		}
	};

	// Bits from an opcode that are sign extended.
	template <u8 from, u8 to = from, u32 flags = 0>
	struct CodeFieldSigned : public CodeField<from, to, flags>
	{
		typedef s32 Type;

		static Type Decode(Opcode data)
		{
			u32 uval = CodeField<from, to, flags>::Decode(data);
			// If it's negative, fill all the sign bits.
			if (uval & (1 << size))
				uval |= 0xFFFFFFFF << size;
			return uval;
		}
	};

	// Bits from a split field in an opcode (combined via a shift and or.)
	template <u8 from1, u8 to1, u8 from2, u8 to2 = from2, u8 offset = 0, u32 flags = 0>
	struct DoubleCodeField : public CodeField<from1, to1, flags>
	{
		typedef u32 Type;

		static Type Decode(Opcode data)
		{
			const u32 uval1 = CodeField<from1, to1, flags>::Decode(data);
			const u32 uval2 = CodeField<from2, to2, flags>::Decode(data);
			return uval1 | (uval2 << offset);
		}
	};

	// Bits from an opcode that are sign extended and shifted via an offset.
	template <u8 from, u8 to, u8 offset, u32 flags = 0>
	struct CodeFieldSignedOffset : public CodeFieldSigned<from, to, flags>
	{
		typedef s32 Type;

		static Type Decode(Opcode data)
		{
			return CodeFieldSigned<from, to, flags>::Decode(data) << offset;
		}
	};

	// This field is used in rotate instructions to specify the first 1 bit of a 64-bit mask
	// Lowercase is a full bit number, uppercase is offset by 32 upon use.
	typedef DoubleCodeField<21, 25, 26, 26, 5> mb;
	typedef CodeField<21, 25> MB;

	// This field is used in rotate instructions to specify the last 1 bit of a 64-bit mask
	// Lowercase is a full bit number, uppercase is offset by 32 upon use.
	typedef DoubleCodeField<21, 25, 26, 26, 5> me;
	typedef CodeField<26, 30> ME;

	// This field is used to specify a shift amount (lowercase mb is a split field with an extra bit.)
	typedef DoubleCodeField<16, 20, 30, 30, 5> sh;
	typedef CodeField<16, 20> SH;

	// This field is used to specify a special-purpose register for the mtspr and mfspr instructions
	typedef CodeField<11, 20> SPR;

	// Vector register operands, S=source, D=dest, ABC=extras (A sometimes a second dest.)
	typedef CodeField<6, 10, FIELD_R_VPR> VS;
	typedef CodeField<6, 10, FIELD_R_VPR> VD;
	typedef CodeField<11, 15, FIELD_R_VPR> VA;
	typedef CodeField<16, 20, FIELD_R_VPR> VB;
	typedef CodeField<21, 25, FIELD_R_VPR> VC;

	// Vector instructions have smaller immediates.
	typedef CodeField<11, 15> VUIMM;
	typedef CodeFieldSigned<11, 15> VSIMM;

	// Vector shift for vsldoi.
	typedef CodeField<22, 25> VSH;

	// General register operands, S=source, D=dest, AB=extras (A sometimes a second dest.)
	typedef CodeField<6, 10, FIELD_R_GPR> RD;
	typedef CodeField<6, 10, FIELD_R_GPR> RS;
	typedef CodeField<11, 15, FIELD_R_GPR> RA;
	typedef CodeField<16, 20, FIELD_R_GPR> RB;

	// This field is used to specify the number of bytes to move in an immediate string load or store
	typedef CodeField<16, 20> NB;

	// Used to specify one of the CR or FPSCR fields, S=source, D=dest.
	typedef CodeField<11, 13, FIELD_R_CR> CRFS;
	typedef CodeField<6, 8, FIELD_R_CR> CRFD;

	// Used to specify a bit in the CR to be used as a source.
	typedef CodeField<11, 15, FIELD_R_CR> CRBA;
	typedef CodeField<16, 20, FIELD_R_CR> CRBB;

	// Used to specify a bit in the CR or FPSCR, as the destination of the result of an instruction.
	typedef CodeField<6, 10, FIELD_R_CR> CRBD;

	// This field is used to specify options for the branch conditional instructions
	typedef CodeField<6, 10> BO;

	// This field is used to specify a bit in the CR to be used as the condition of a branch conditional instruction
	typedef CodeField<11, 15> BI;

	// Branch type hint (indicates how predictable the destination is.)
	typedef CodeField<19, 20> BH;

	// Not sure?
	typedef CodeField<11, 13> BFA;
	
	// Field used by the optional data stream variant of the dcbt instruction.
	typedef CodeField<9, 10> TH;

	// This field is used to specify the conditions on which to trap
	typedef CodeField<6, 10> TO;

	// Immediate field specifying a 14-bit signed two's complement branch displacement that is concatenated on the
	// right with '00' and sign-extended to 64 bits.
	// It's actually (16, 29) << 2, but we can mask the lower bits.  Probably a wash.
	typedef CodeFieldSigned<16, 31, FIELD_BRANCH> BD;

	// The immediate address of the branch target.  Why not the same as BD, we're masking the lower bits anyhow?
	typedef CodeFieldSignedOffset<6, 29, 2, FIELD_BRANCH> LI;

	/*
	Absolute address bit.
	0		The immediate field represents an address relative to the current instruction address (CIA). (For more 
			information on the CIA, see Table 8-3.) The effective (logical) address of the branch is either the sum 
			of the LI field sign-extended to 64 bits and the address of the branch instruction or the sum of the BD 
			field sign-extended to 64 bits and the address of the branch instruction.
	1		The immediate field represents an absolute address. The effective address (EA) of the branch is the 
			LI field sign-extended to 64 bits or the BD field sign-extended to 64 bits.
	*/
	typedef CodeField<30> AA;

	/*
	Link bit.
	0		Does not update the link register (LR).
	1		Updates the LR. If the instruction is a branch instruction, the address of the instruction following the 
			branch instruction is placed into the LR.
	*/
	typedef CodeField<31> LK;

	// This field is used for extended arithmetic to enable setting OV and SO in the XER
	typedef CodeField<21> OE;

	//Field used to specify whether an integer compare instruction is to compare 64-bit numbers or 32-bit numbers
	typedef CodeField<10> L_10;

	// Indicates the newer form of the mtocrf/mfocrf instruction (1) or the older form (0).
	typedef CodeField<11> OCRF; // L_11

	// Indicates the data stream is transient (1) or long term (0).
	typedef CodeField<6> STRM_T; // L_6

	// Indicates whether to stop all data streams (1) or only the specified one (0).
	typedef CodeField<6> STRM_A; // L_6

	// Stream id for data stream instructions like DST.
	typedef CodeField<9, 10> STRM;

	// Specifies which type of memory barrier to apply for sync.
	typedef CodeField<9, 10> SYNC_L; // L_9_10

	// Immediate value used in MTFSFI to update FPCSR.
	typedef CodeField<16, 19> MTFSFI_IMM;

	// Indicates which bits in the MSR to update.
	typedef CodeField<15> MTMSR_L;

	// Indicates which SLB entry to update for MTSR/MFSR/etc.
	typedef CodeField<12, 15> SR;

	// Not sure?
	typedef CodeField<16, 27> DQ;

	// Float register operands, S=source, D=dest, ABC=extras (A sometimes a second dest.)
	typedef CodeField<6, 10, FIELD_R_FPR> FRS;
	typedef CodeField<6, 10, FIELD_R_FPR> FRD;
	typedef CodeField<11, 15, FIELD_R_FPR> FRA;
	typedef CodeField<16, 20, FIELD_R_FPR> FRB;
	typedef CodeField<21, 25, FIELD_R_FPR> FRC;

	// FPCSR field mask for mtfsf.
	typedef CodeField<7, 14> FM;

	// This field mask is used to identify the CR fields that are to be updated by the mtcrf instruction.
	typedef CodeField<12, 19> CRM;

	// Syscall type for SC instruction.
	typedef CodeField<6, 31> SYS;

	// Immediate field specifying a 16-bit signed two's complement displacement.
	typedef CodeFieldSigned<16, 31> d; // D

	// EA displacement with offse for some load/store instructions.
	typedef CodeFieldSignedOffset<16, 29, 2> ds; // DS

	// This immediate field is used to specify a 16-bit signed integer
	typedef CodeFieldSigned<16, 31> simm16;

	// This immediate field is used to specify a 16-bit unsigned integer
	typedef CodeField<16, 31> uimm16;
	
	/*
	Record bit.
	0		Does not update the condition register (CR).
	1		Updates the CR to reflect the result of the operation.
			For integer instructions, CR bits [0-2] are set to reflect the result as a signed quantity and CR bit [3] 
			receives a copy of the summary overflow bit, XER[SO]. The result as an unsigned quantity or a bit 
			string can be deduced from the EQ bit. For floating-point instructions, CR bits [4-7] are set to reflect 
			floating-point exception, floating-point enabled exception, floating-point invalid operation exception, 
			and floating-point overflow exception. 
	*/
	typedef CodeBit<31> Rc;

	// Bit indicating RLDCR (1) or RLDCL (0).
	typedef CodeBit<30> RLDC_R;

	// Primary opcode field
	typedef CodeField<0, 5> OPCD;

	struct MainInstructionTable : public InstructionTable
	{
		MainInstructionTable();
		void Add(PPU_opcodes::PPU_MainOpcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
		}
	};

	struct Sub04InstructionTable : public InstructionTable
	{
		Sub04InstructionTable();
		void Add(PPU_opcodes::G_04_VA_Opcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
		}
	};

	struct Sub04LongInstructionTable : public InstructionTable
	{
		Sub04LongInstructionTable();
		void Add(PPU_opcodes::G_04Opcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
		}
	};

	struct Sub13InstructionTable : public InstructionTable
	{
		Sub13InstructionTable();
		void Add(PPU_opcodes::G_13Opcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
		}
	};

	struct Sub1EInstructionTable : public InstructionTable
	{
		Sub1EInstructionTable();
		void Add(PPU_opcodes::G_1eOpcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
		}
	};

	struct Sub1FInstructionTable : public InstructionTable
	{
		Sub1FInstructionTable();
		void Add(PPU_opcodes::G_1fOpcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
		}
		void AddOE(PPU_opcodes::G_1fOpcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
			// This is for when the OE bit is set, which is the 10th bit from the right.
			return DoAdd(index | (1 << 9), std::forward<const Instruction>(instr));
		}
	};

	struct Sub3AInstructionTable : public InstructionTable
	{
		Sub3AInstructionTable();
		void Add(PPU_opcodes::G_3aOpcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
		}
	};

	struct Sub3BInstructionTable : public InstructionTable
	{
		Sub3BInstructionTable();
		void Add(PPU_opcodes::G_3bOpcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
		}
	};

	struct Sub3EInstructionTable : public InstructionTable
	{
		Sub3EInstructionTable();
		void Add(PPU_opcodes::G_3eOpcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
		}
	};

	struct Sub3FInstructionTable : public InstructionTable
	{
		Sub3FInstructionTable();
		void Add(PPU_opcodes::G_3fOpcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
		}
	};

	struct Sub3FLongInstructionTable : public InstructionTable
	{
		Sub3FLongInstructionTable();
		void Add(PPU_opcodes::G_3fOpcodes index, const Instruction &&instr)
		{
			return DoAdd(index, std::forward<const Instruction>(instr));
		}
	};

	static const MainInstructionTable mainTable;
	static const Sub04InstructionTable sub04Table;
	static const Sub04LongInstructionTable sub04LongTable;
	static const Sub13InstructionTable sub13Table;
	static const Sub1EInstructionTable sub1ETable;
	static const Sub1FInstructionTable sub1FTable;
	static const Sub3AInstructionTable sub3ATable;
	static const Sub3BInstructionTable sub3BTable;
	static const Sub3EInstructionTable sub3ETable;
	static const Sub3FInstructionTable sub3FTable;
	static const Sub3FLongInstructionTable sub3FLongTable;

#define INSTR0(name)        Add(PPU_opcodes::name, Instr<&PPUInterpreter::name, &PPUCompiler::name, &PPUDisAsm::name>(#name))
#define INSTR(name, ...)    Add(PPU_opcodes::name, Instr<##__VA_ARGS__, &PPUInterpreter::name, &PPUCompiler::name, &PPUDisAsm::name>(#name))
#define INSTR_OE(name, ...) AddOE(PPU_opcodes::name, Instr<##__VA_ARGS__, &PPUInterpreter::name, &PPUCompiler::name, &PPUDisAsm::name>(#name));
#define TABLE(index, list)  Add(PPU_opcodes::index, Table(list))

	// xxxxxx ..... ..... ................
	MainInstructionTable::MainInstructionTable() : InstructionTable(0, 5)
	{
		INSTR(TDI, TO, RA, simm16);
		INSTR(TWI, TO, RA, simm16);

		TABLE(G_04, &sub04Table);

		INSTR(MULLI, RD, RA, simm16);
		INSTR(SUBFIC, RD, RA, simm16);
		INSTR(CMPLI, CRFD, L_10, RA, uimm16);
		INSTR(CMPI, CRFD, L_10, RA, simm16);
		INSTR(ADDIC, RD, RA, simm16);
		INSTR(ADDIC_, RD, RA, simm16);
		INSTR(ADDI, RD, RA, simm16);
		INSTR(ADDIS, RD, RA, simm16);
		INSTR(BC, BO, BI, BD, AA, LK);
		INSTR(SC, SYS);
		INSTR(B, LI, AA, LK);

		TABLE(G_13, &sub13Table);

		INSTR(RLWIMI, RA, RS, SH, MB, ME, Rc);
		INSTR(RLWINM, RA, RS, SH, MB, ME, Rc);
		INSTR(RLWNM, RA, RS, RB, MB, ME, Rc);
		INSTR(ORI, RA, RS, uimm16);
		INSTR(ORIS, RA, RS, uimm16);
		INSTR(XORI, RA, RS, uimm16);
		INSTR(XORIS, RA, RS, uimm16);
		INSTR(ANDI_, RA, RS, uimm16);
		INSTR(ANDIS_, RA, RS, uimm16);

		TABLE(G_1e, &sub1ETable);
		TABLE(G_1f, &sub1FTable);

		INSTR(LWZ, RD, RA, d);
		INSTR(LWZU, RD, RA, d);
		INSTR(LBZ, RD, RA, d);
		INSTR(LBZU, RD, RA, d);
		INSTR(STW, RS, RA, d);
		INSTR(STWU, RS, RA, d);
		INSTR(STB, RS, RA, d);
		INSTR(STBU, RS, RA, d);
		INSTR(LHZ, RD, RA, d);
		INSTR(LHZU, RD, RA, d);
		INSTR(LHA, RD, RA, d);
		INSTR(LHAU, RD, RA, d);
		INSTR(STH, RS, RA, d);
		INSTR(STHU, RS, RA, d);
		INSTR(LMW, RD, RA, d);
		INSTR(STMW, RS, RA, d);
		INSTR(LFS, FRD, RA, d);
		INSTR(LFSU, FRD, RA, d);
		INSTR(LFD, FRD, RA, d);
		INSTR(LFDU, FRD, RA, d);
		INSTR(STFS, FRS, RA, d);
		INSTR(STFSU, FRS, RA, d);
		INSTR(STFD, FRS, RA, d);
		INSTR(STFDU, FRS, RA, d);

		TABLE(G_3a, &sub3ATable);
		TABLE(G_3b, &sub3BTable);
		TABLE(G_3e, &sub3ETable);
		TABLE(G_3f, &sub3FTable);
	}

	// 000100 ..... ..... ..... ..... xxxxxx
	Sub04InstructionTable::Sub04InstructionTable() : InstructionTable(26, 31, &sub04LongTable)
	{
		INSTR(VMHADDSHS, VD, VA, VB, VC);
		INSTR(VMHRADDSHS, VD, VA, VB, VC);
		INSTR(VMLADDUHM, VD, VA, VB, VC);
		INSTR(VMSUMUBM, VD, VA, VB, VC);
		INSTR(VMSUMMBM, VD, VA, VB, VC);
		INSTR(VMSUMUHM, VD, VA, VB, VC);
		INSTR(VMSUMUHS, VD, VA, VB, VC);
		INSTR(VMSUMSHM, VD, VA, VB, VC);
		INSTR(VMSUMSHS, VD, VA, VB, VC);
		INSTR(VSEL, VD, VA, VB, VC);
		INSTR(VPERM, VD, VA, VB, VC);
		INSTR(VSLDOI, VD, VA, VB, VSH);
		INSTR(VMADDFP, VD, VA, VC, VB);
		INSTR(VNMSUBFP, VD, VA, VC, VB);
	}

	// 000100 ..... ..... ..... xxxxxxxxxxx
	Sub04LongInstructionTable::Sub04LongInstructionTable() : InstructionTable(21, 31)
	{
	INSTR(MFVSCR, VD);
		INSTR(MTVSCR, VB);
		INSTR(VADDCUW, VD, VA, VB);
		INSTR(VADDFP, VD, VA, VB);
		INSTR(VADDSBS, VD, VA, VB);
		INSTR(VADDSHS, VD, VA, VB);
		INSTR(VADDSWS, VD, VA, VB);
		INSTR(VADDUBM, VD, VA, VB);
		INSTR(VADDUBS, VD, VA, VB);
		INSTR(VADDUHM, VD, VA, VB);
		INSTR(VADDUHS, VD, VA, VB);
		INSTR(VADDUWM, VD, VA, VB);
		INSTR(VADDUWS, VD, VA, VB);
		INSTR(VAND, VD, VA, VB);
		INSTR(VANDC, VD, VA, VB);
		INSTR(VAVGSB, VD, VA, VB);
		INSTR(VAVGSH, VD, VA, VB);
		INSTR(VAVGSW, VD, VA, VB);
		INSTR(VAVGUB, VD, VA, VB);
		INSTR(VAVGUH, VD, VA, VB);
		INSTR(VAVGUW, VD, VA, VB);
		INSTR(VCFSX, VD, VUIMM, VB);
		INSTR(VCFUX, VD, VUIMM, VB);
		INSTR(VCMPBFP, VD, VA, VB);
		INSTR(VCMPBFP_, VD, VA, VB);
		INSTR(VCMPEQFP, VD, VA, VB);
		INSTR(VCMPEQFP_, VD, VA, VB);
		INSTR(VCMPEQUB, VD, VA, VB);
		INSTR(VCMPEQUB_, VD, VA, VB);
		INSTR(VCMPEQUH, VD, VA, VB);
		INSTR(VCMPEQUH_, VD, VA, VB);
		INSTR(VCMPEQUW, VD, VA, VB);
		INSTR(VCMPEQUW_, VD, VA, VB);
		INSTR(VCMPGEFP, VD, VA, VB);
		INSTR(VCMPGEFP_, VD, VA, VB);
		INSTR(VCMPGTFP, VD, VA, VB);
		INSTR(VCMPGTFP_, VD, VA, VB);
		INSTR(VCMPGTSB, VD, VA, VB);
		INSTR(VCMPGTSB_, VD, VA, VB);
		INSTR(VCMPGTSH, VD, VA, VB);
		INSTR(VCMPGTSH_, VD, VA, VB);
		INSTR(VCMPGTSW, VD, VA, VB);
		INSTR(VCMPGTSW_, VD, VA, VB);
		INSTR(VCMPGTUB, VD, VA, VB);
		INSTR(VCMPGTUB_, VD, VA, VB);
		INSTR(VCMPGTUH, VD, VA, VB);
		INSTR(VCMPGTUH_, VD, VA, VB);
		INSTR(VCMPGTUW, VD, VA, VB);
		INSTR(VCMPGTUW_, VD, VA, VB);
		INSTR(VCTSXS, VD, VUIMM, VB);
		INSTR(VCTUXS, VD, VUIMM, VB);
		INSTR(VEXPTEFP, VD, VB);
		INSTR(VLOGEFP, VD, VB);
		INSTR(VMAXFP, VD, VA, VB);
		INSTR(VMAXSB, VD, VA, VB);
		INSTR(VMAXSH, VD, VA, VB);
		INSTR(VMAXSW, VD, VA, VB);
		INSTR(VMAXUB, VD, VA, VB);
		INSTR(VMAXUH, VD, VA, VB);
		INSTR(VMAXUW, VD, VA, VB);
		INSTR(VMINFP, VD, VA, VB);
		INSTR(VMINSB, VD, VA, VB);
		INSTR(VMINSH, VD, VA, VB);
		INSTR(VMINSW, VD, VA, VB);
		INSTR(VMINUB, VD, VA, VB);
		INSTR(VMINUH, VD, VA, VB);
		INSTR(VMINUW, VD, VA, VB);
		INSTR(VMRGHB, VD, VA, VB);
		INSTR(VMRGHH, VD, VA, VB);
		INSTR(VMRGHW, VD, VA, VB);
		INSTR(VMRGLB, VD, VA, VB);
		INSTR(VMRGLH, VD, VA, VB);
		INSTR(VMRGLW, VD, VA, VB);
		INSTR(VMULESB, VD, VA, VB);
		INSTR(VMULESH, VD, VA, VB);
		INSTR(VMULEUB, VD, VA, VB);
		INSTR(VMULEUH, VD, VA, VB);
		INSTR(VMULOSB, VD, VA, VB);
		INSTR(VMULOSH, VD, VA, VB);
		INSTR(VMULOUB, VD, VA, VB);
		INSTR(VMULOUH, VD, VA, VB);
		INSTR(VNOR, VD, VA, VB);
		INSTR(VOR, VD, VA, VB);
		INSTR(VPKPX, VD, VA, VB);
		INSTR(VPKSHSS, VD, VA, VB);
		INSTR(VPKSHUS, VD, VA, VB);
		INSTR(VPKSWSS, VD, VA, VB);
		INSTR(VPKSWUS, VD, VA, VB);
		INSTR(VPKUHUM, VD, VA, VB);
		INSTR(VPKUHUS, VD, VA, VB);
		INSTR(VPKUWUM, VD, VA, VB);
		INSTR(VPKUWUS, VD, VA, VB);
		INSTR(VREFP, VD, VB);
		INSTR(VRFIM, VD, VB);
		INSTR(VRFIN, VD, VB);
		INSTR(VRFIP, VD, VB);
		INSTR(VRFIZ, VD, VB);
		INSTR(VRLB, VD, VA, VB);
		INSTR(VRLH, VD, VA, VB);
		INSTR(VRLW, VD, VA, VB);
		INSTR(VRSQRTEFP, VD, VB);
		INSTR(VSL, VD, VA, VB);
		INSTR(VSLB, VD, VA, VB);
		INSTR(VSLH, VD, VA, VB);
		INSTR(VSLO, VD, VA, VB);
		INSTR(VSLW, VD, VA, VB);
		INSTR(VSPLTB, VD, VUIMM, VB);
		INSTR(VSPLTH, VD, VUIMM, VB);
		INSTR(VSPLTISB, VD, VSIMM);
		INSTR(VSPLTISH, VD, VSIMM);
		INSTR(VSPLTISW, VD, VSIMM);
		INSTR(VSPLTW, VD, VUIMM, VB);
		INSTR(VSR, VD, VA, VB);
		INSTR(VSRAB, VD, VA, VB);
		INSTR(VSRAH, VD, VA, VB);
		INSTR(VSRAW, VD, VA, VB);
		INSTR(VSRB, VD, VA, VB);
		INSTR(VSRH, VD, VA, VB);
		INSTR(VSRO, VD, VA, VB);
		INSTR(VSRW, VD, VA, VB);
		INSTR(VSUBCUW, VD, VA, VB);
		INSTR(VSUBFP, VD, VA, VB);
		INSTR(VSUBSBS, VD, VA, VB);
		INSTR(VSUBSHS, VD, VA, VB);
		INSTR(VSUBSWS, VD, VA, VB);
		INSTR(VSUBUBM, VD, VA, VB);
		INSTR(VSUBUBS, VD, VA, VB);
		INSTR(VSUBUHM, VD, VA, VB);
		INSTR(VSUBUHS, VD, VA, VB);
		INSTR(VSUBUWM, VD, VA, VB);
		INSTR(VSUBUWS, VD, VA, VB);
		INSTR(VSUMSWS, VD, VA, VB);
		INSTR(VSUM2SWS, VD, VA, VB);
		INSTR(VSUM4SBS, VD, VA, VB);
		INSTR(VSUM4SHS, VD, VA, VB);
		INSTR(VSUM4UBS, VD, VA, VB);
		INSTR(VUPKHPX, VD, VB);
		INSTR(VUPKHSB, VD, VB);
		INSTR(VUPKHSH, VD, VB);
		INSTR(VUPKLPX, VD, VB);
		INSTR(VUPKLSB, VD, VB);
		INSTR(VUPKLSH, VD, VB);
		INSTR(VXOR, VD, VA, VB);
	}

	// 010011 ..... ..... ..... xxxxxxxxxx .
	Sub13InstructionTable::Sub13InstructionTable() : InstructionTable(21, 30)
	{
		INSTR(MCRF, CRFD, CRFS);
		INSTR(BCLR, BO, BI, BH, LK);
		//INSTR0(RFID);
		INSTR(CRNOR, CRBD, CRBA, CRBB);
		INSTR(CRANDC, CRBD, CRBA, CRBB);
		INSTR0(ISYNC);
		INSTR(CRXOR, CRBD, CRBA, CRBB);
		INSTR(CRNAND, CRBD, CRBA, CRBB);
		INSTR(CRAND, CRBD, CRBA, CRBB);
		INSTR(CREQV, CRBD, CRBA, CRBB);
		INSTR(CRORC, CRBD, CRBA, CRBB);
		INSTR(CROR, CRBD, CRBA, CRBB);
		INSTR(BCCTR, BO, BI, BH, LK);
	}

	// 011110 ..... ..... ..... ...... xxx . .
	Sub1EInstructionTable::Sub1EInstructionTable() : InstructionTable(27, 29)
	{
		// TODO: Why not 27, 30 and list them twice?
		INSTR(RLDICL, RA, RS, sh, mb, Rc);
		INSTR(RLDICR, RA, RS, sh, me, Rc);
		INSTR(RLDIC, RA, RS, sh, mb, Rc);
		INSTR(RLDIMI, RA, RS, sh, mb, Rc);
		// Technically mb or me depending on L or R.  mb ignores the zero bit.
		INSTR(RLDC_LR, RA, RS, RB, mb, RLDC_R, Rc);
	}

	// 011111 ..... ..... ..... xxxxxxxxxx .
	Sub1FInstructionTable::Sub1FInstructionTable() : InstructionTable(21, 30)
	{
		INSTR(CMP, CRFD, L_10, RA, RB);
		INSTR(TW, TO, RA, RB);
		INSTR(LVSL, VD, RA, RB);
		INSTR(LVEBX, VD, RA, RB);
		INSTR_OE(SUBFC, RD, RA, RB, OE, Rc);
		INSTR(MULHDU, RD, RA, RB, Rc);
		INSTR_OE(ADDC, RD, RA, RB, OE, Rc);
		INSTR(MULHWU, RD, RA, RB, Rc);
		INSTR(MFOCRF, OCRF, RD, CRM);
		INSTR(LWARX, RD, RA, RB);
		INSTR(LDX, RD, RA, RB);
		INSTR(LWZX, RD, RA, RB);
		INSTR(SLW, RA, RS, RB, Rc);
		INSTR(CNTLZW, RA, RS, Rc);
		INSTR(SLD, RA, RS, RB, Rc);
		INSTR(AND, RA, RS, RB, Rc);
		INSTR(CMPL, CRFD, L_10, RA, RB);
		INSTR(LVSR, VD, RA, RB);
		INSTR(LVEHX, VD, RA, RB);
		INSTR_OE(SUBF, RD, RA, RB, OE, Rc);
		INSTR(LDUX, RD, RA, RB);
		INSTR(DCBST, RA, RB);
		INSTR(LWZUX, RD, RA, RB);
		INSTR(CNTLZD, RA, RS, Rc);
		INSTR(ANDC, RA, RS, RB, Rc);
		INSTR(TD, TO, RA, RB);
		INSTR(LVEWX, VD, RA, RB);
		INSTR(MULHD, RD, RA, RB, Rc);
		INSTR(MULHW, RD, RA, RB, Rc);
		//INSTR(MFMSR, RD);
		INSTR(LDARX, RD, RA, RB);
		INSTR(DCBF, RA, RB);
		INSTR(LBZX, RD, RA, RB);
		INSTR(LVX, VD, RA, RB);
		INSTR_OE(NEG, RD, RA, OE, Rc);
		INSTR(LBZUX, RD, RA, RB);
		INSTR(NOR, RA, RS, RB, Rc);
		INSTR(STVEBX, VS, RA, RB);
		INSTR_OE(SUBFE, RD, RA, RB, OE, Rc);
		INSTR_OE(ADDE, RD, RA, RB, OE, Rc);
		INSTR(MTOCRF, OCRF, CRM, RS);
		//INSTR(MTMSR, RS, MTMSR_L);
		INSTR(STDX, RS, RA, RB);
		INSTR(STWCX_, RS, RA, RB);
		INSTR(STWX, RS, RA, RB);
		INSTR(STVEHX, VS, RA, RB);
		//INSTR(MTMSRD, RS, MTMSR_L);
		INSTR(STDUX, RS, RA, RB);
		INSTR(STWUX, RS, RA, RB);
		INSTR(STVEWX, VS, RA, RB);
		INSTR_OE(SUBFZE, RD, RA, OE, Rc);
		INSTR_OE(ADDZE, RD, RA, OE, Rc);
		//INSTR(MTSR, RS, SR);
		INSTR(STDCX_, RS, RA, RB);
		INSTR(STBX, RS, RA, RB);
		INSTR(STVX, VS, RA, RB);
		INSTR_OE(SUBFME, RD, RA, OE, Rc);
		INSTR_OE(MULLD, RD, RA, RB, OE, Rc);
		INSTR_OE(ADDME, RD, RA, OE, Rc);
		INSTR_OE(MULLW, RD, RA, RB, OE, Rc);
		//INSTR(MTSRIN, RS, RB);
		// TH should always be zero in this case.
		INSTR(DCBTST, RA, RB, TH);
		INSTR(STBUX, RS, RA, RB);
		INSTR_OE(ADD, RD, RA, RB, OE, Rc);
		INSTR(DCBT, RA, RB, TH);
		INSTR(LHZX, RD, RA, RB);
		INSTR(EQV, RA, RS, RB, Rc);
		//INSTR(TLBIEL, RB, L_10);
		//INSTR(TLBIE, RB, L_10);
		INSTR(ECIWX, RD, RA, RB);
		INSTR(LHZUX, RD, RA, RB);
		INSTR(XOR, RA, RS, RB, Rc);
		INSTR(MFSPR, RD, SPR);
		INSTR(LWAX, RD, RA, RB);
		// NOTE: No-op.
		INSTR(DST, RA, RB, STRM, STRM_T);
		INSTR(LHAX, RD, RA, RB);
		INSTR(LVXL, VD, RA, RB);
		//INSTR0(TLBIA);
		INSTR(MFTB, RD, SPR);
		INSTR(LWAUX, RD, RA, RB);
		// NOTE: No-op.
		INSTR(DSTST, RA, RB, STRM, STRM_T);
		INSTR(LHAUX, RD, RA, RB);
		INSTR(STHX, RS, RA, RB);
		INSTR(ORC, RA, RS, RB, Rc);
		// sh affects the opcode index, so replicated twice.
		INSTR(SRADI1, RA, RS, sh, Rc);
		INSTR(SRADI2, RA, RS, sh, Rc);
		//INSTR(SLBIE, RB);
		INSTR(ECOWX, RS, RA, RB);
		INSTR(STHUX, RS, RA, RB);
		INSTR(OR, RA, RS, RB, Rc);
		INSTR_OE(DIVDU, RD, RA, RB, OE, Rc);
		INSTR_OE(DIVWU, RD, RA, RB, OE, Rc);
		INSTR(MTSPR, SPR, RS);
		// DCBI is obsolete.
		INSTR(NAND, RA, RS, RB, Rc);
		INSTR(STVXL, VS, RA, RB);
		INSTR_OE(DIVD, RD, RA, RB, OE, Rc);
		INSTR_OE(DIVW, RD, RA, RB, OE, Rc);
		INSTR(LVLX, VD, RA, RB);
		//INSTR0(SLBIA);
		INSTR(LDBRX, RD, RA, RB);
		INSTR(LSWX, RD, RA, RB);
		INSTR(LWBRX, RD, RA, RB);
		INSTR(LFSX, FRD, RA, RB);
		INSTR(SRW, RA, RS, RB, Rc);
		INSTR(SRD, RA, RS, RB, Rc);
		INSTR(LVRX, VD, RA, RB);
		//INSTR0(TLBSYNC);
		INSTR(LFSUX, FRD, RA, RB);
		//INSTR(MFSR, RD, SR);
		INSTR(LSWI, RD, RA, NB);
		INSTR(SYNC, SYNC_L);
		INSTR(LFDX, FRD, RA, RB);
		INSTR(LFDUX, FRD, RA, RB);
		//INSTR(MFSRIN, RD, RB);
		//INSTR(SLBMFEE, RD, RB);
		//INSTR(SLBMFEV, RD, RB);
		//INSTR(SLBMTE, RD, RB);
		INSTR(STVLX, VS, RA, RB);
		INSTR(STSWX, RS, RA, RB);
		INSTR(STWBRX, RS, RA, RB);
		INSTR(STFSX, FRS, RA, RB);
		INSTR(STVRX, VS, RA, RB);
		INSTR(STFSUX, FRS, RA, RB);
		INSTR(STSWI, RS, RA, NB);
		INSTR(STFDX, FRS, RA, RB);
		INSTR(STFDUX, FRS, RA, RB);
		INSTR(LVLXL, VD, RA, RB);
		INSTR(LHBRX, RD, RA, RB);
		INSTR(SRAW, RA, RS, RB, Rc);
		INSTR(SRAD, RA, RS, RB, Rc);
		INSTR(LVRXL, VD, RA, RB);
		// NOTE: No-op.
		INSTR(DSS, STRM, STRM_A);
		INSTR(SRAWI, RA, RS, SH, Rc);
		INSTR0(EIEIO);
		INSTR(STVLXL, VS, RA, RB);
		INSTR(STHBRX, RS, RA, RB);
		INSTR(EXTSH, RA, RS, Rc);
		INSTR(STVRXL, VS, RA, RB);
		INSTR(EXTSB, RA, RS, Rc);
		INSTR(ICBI, RA, RB);
		INSTR(STFIWX, FRS, RA, RB);
		INSTR(EXTSW, RA, RS, Rc);
		INSTR(DCBZ, RA, RB);
	}

	// 111010 ..... ..... .............. xx
	// TODO: 3A/3E - if common, may be cheaper as a func rather than extra table?  Not sure.
	Sub3AInstructionTable::Sub3AInstructionTable() : InstructionTable(30, 31)
	{
		INSTR(LD, RD, RA, ds);
		INSTR(LDU, RD, RA, ds);
		INSTR(LWA, RD, RA, ds);
	}

	// 111011 ..... ..... ..... ..... xxxxx .
	Sub3BInstructionTable::Sub3BInstructionTable() : InstructionTable(26, 30)
	{
		INSTR(FDIVS, FRD, FRA, FRB, Rc);
		INSTR(FSUBS, FRD, FRA, FRB, Rc);
		INSTR(FADDS, FRD, FRA, FRB, Rc);
		INSTR(FMULS, FRD, FRA, FRC, Rc);
		INSTR(FMSUBS, FRD, FRA, FRC, FRB, Rc);
		INSTR(FMADDS, FRD, FRA, FRC, FRB, Rc);
		INSTR(FNMSUBS, FRD, FRA, FRC, FRB, Rc);
		INSTR(FNMADDS, FRD, FRA, FRC, FRB, Rc);
	}

	// 111110 ..... ..... .............. xx
	Sub3EInstructionTable::Sub3EInstructionTable() : InstructionTable(30, 31)
	{
		INSTR(STD, RS, RA, ds);
		INSTR(STDU, RS, RA, ds);
	}

	// 111111 ..... ..... ..... .....xxxxx .
	Sub3FInstructionTable::Sub3FInstructionTable() : InstructionTable(26, 30, &sub3FLongTable)
	{
		INSTR(FSEL, FRD, FRA, FRC, FRB, Rc);
		INSTR(FMUL, FRD, FRA, FRC, Rc);
		INSTR(FMSUB, FRD, FRA, FRC, FRB, Rc);
		INSTR(FMADD, FRD, FRA, FRC, FRB, Rc);
		INSTR(FNMSUB, FRD, FRA, FRC, FRB, Rc);
		INSTR(FNMADD, FRD, FRA, FRC, FRB, Rc);
	}

	// 111111 ..... ..... ..... xxxxxxxxxx .
	Sub3FLongInstructionTable::Sub3FLongInstructionTable() : InstructionTable(21, 30)
	{
		INSTR(FCMPU, CRFD, FRA, FRB);
		INSTR(FRSP, FRD, FRB, Rc);
		INSTR(FCTIW, FRD, FRB, Rc);
		INSTR(FCTIWZ, FRD, FRB, Rc);
		// TODO: Could move these up, save a lookup, but not catch the reserved field.
		INSTR(FDIV, FRD, FRA, FRB, Rc);
		INSTR(FSUB, FRD, FRA, FRB, Rc);
		INSTR(FADD, FRD, FRA, FRB, Rc);
		INSTR(FSQRT, FRD, FRB, Rc);
		INSTR(FRSQRTE, FRD, FRB, Rc);
		INSTR(FCMPO, CRFD, FRA, FRB);
		INSTR(MTFSB1, CRBD, Rc);
		INSTR(FNEG, FRD, FRB, Rc);
		INSTR(MCRFS, CRFD, CRFS);
		INSTR(MTFSB0, CRBD, Rc);
		INSTR(FMR, FRD, FRB, Rc);
		INSTR(MTFSFI, CRFD, MTFSFI_IMM, Rc);
		INSTR(FNABS, FRD, FRB, Rc);
		INSTR(FABS, FRD, FRB, Rc);
		INSTR(MFFS, FRD, Rc);
		INSTR(MTFSF, FM, FRB, Rc);
		INSTR(FCTID, FRD, FRB, Rc);
		INSTR(FCTIDZ, FRD, FRB, Rc);
		INSTR(FCFID, FRD, FRB, Rc);
	}

#undef INSTR
#undef INSTR_OE
#undef INSTR0
#undef TABLE

	const Instruction &GetInstruction(Opcode op)
	{
		// Hardcoded the first shift for speed.  See mainTable.
		const Instruction *instr = mainTable[op >> (31 - 5)];
		while (instr->subTable)
			instr = instr->subTable->Lookup(op);
		return *instr;
	}
};
