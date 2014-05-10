#pragma once
#include "PPCInstrTable.h"
#include "PPCDecoder.h"
#include "PPUOpcodes.h"

class PPUInterpreter;
class PPUDisAsm;

namespace PPU_instr
{
	typedef u32 Opcode;

	// TODO: Implement a compiler, this is just to make things compile.
	// Also, probably need a smart way to have jit fall back to the interpreter.
	typedef PPUInterpreter PPUCompiler;

	typedef void (PPUInterpreter::*InterpretFunc)(Opcode op);
	typedef void (PPUCompiler::*CompileFunc)(Opcode op);
	typedef void (PPUDisAsm::*DisasmFunc)(Opcode op);

	enum InstructionFlags
	{
		// TODO: These are probably not the right flags to have.
		FIELD_IMM,
		FIELD_R_GPR,
		FIELD_R_FPR,
		FIELD_R_VPR,
		FIELD_R_CR,
		FIELD_BRANCH,
	};

	struct InstructionTable;

	struct Instruction
	{
		const InstructionTable *subTable;
		InterpretFunc interpret;
		CompileFunc compile;
		DisasmFunc disasm;
		std::string name;
		u32 flags;

		Instruction();
		Instruction(const Instruction &other);
		explicit Instruction(InterpretFunc i, CompileFunc c, DisasmFunc d, const std::string &n, u32 f = 0);
		explicit Instruction(const InstructionTable *t);
	};

	const Instruction &GetInstruction(Opcode op);

	class InstructionHelpers {
	public:
		template <typename T, void (T::*Func)()>
		void InstructionProxy(u32 op)
		{
			return (static_cast<T *>(this)->*Func)();
		}

		template <typename A0, typename T, void (T::*Func)(typename A0::Type)>
		void InstructionProxy(Opcode op)
		{
			return (static_cast<T *>(this)->*Func)(A0::Decode(op));
		}

		template <typename A0, typename A1, typename T, void (T::*Func)(typename A0::Type, typename A1::Type)>
		void InstructionProxy(Opcode op)
		{
			return (static_cast<T *>(this)->*Func)(A0::Decode(op), A1::Decode(op));
		}

		template <typename A0, typename A1, typename A2, typename T, void (T::*Func)(typename A0::Type, typename A1::Type, typename A2::Type)>
		void InstructionProxy(Opcode op)
		{
			return (static_cast<T *>(this)->*Func)(A0::Decode(op), A1::Decode(op), A2::Decode(op));
		}

		template <typename A0, typename A1, typename A2, typename A3, typename T, void (T::*Func)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type)>
		void InstructionProxy(Opcode op)
		{
			return (static_cast<T *>(this)->*Func)(A0::Decode(op), A1::Decode(op), A2::Decode(op), A3::Decode(op));
		}

		template <typename A0, typename A1, typename A2, typename A3, typename A4, typename T, void (T::*Func)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type, typename A4::Type)>
		void InstructionProxy(Opcode op)
		{
			return (static_cast<T *>(this)->*Func)(A0::Decode(op), A1::Decode(op), A2::Decode(op), A3::Decode(op), A4::Decode(op));
		}

		template <typename A0, typename A1, typename A2, typename A3, typename A4, typename A5, typename T, void (T::*Func)(typename A0::Type, typename A1::Type, typename A2::Type, typename A3::Type, typename A4::Type, typename A5::Type)>
		void InstructionProxy(Opcode op)
		{
			return (static_cast<T *>(this)->*Func)(A0::Decode(op), A1::Decode(op), A2::Decode(op), A3::Decode(op), A4::Decode(op), A5::Decode(op));
		}
	};

};