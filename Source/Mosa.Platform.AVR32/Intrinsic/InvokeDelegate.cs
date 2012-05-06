/*
 * (c) 2008 MOSA - The Managed Operating System Alliance
 *
 * Licensed under the terms of the New BSD License.
 *
 * Authors:
 *  Phil Garcia (tgiphil) <phil@thinkedge.com>
 *  Michael Ruck (grover) <sharpos@michaelruck.de>
 *  Pascal Delprat (pdelprat) <pascal.delprat@online.fr>
 */

using System;
using System.Collections.Generic;
using Mosa.Compiler.Framework;
using Mosa.Compiler.Framework.Operands;
using Mosa.Compiler.Metadata.Signatures;
using Mosa.Compiler.TypeSystem;

namespace Mosa.Platform.AVR32.Intrinsic
{
	/// <summary>
	/// 
	/// </summary>
	public sealed class InvokeDelegate : IIntrinsicMethod
	{

		#region Methods

		/// <summary>
		/// Replaces the intrinsic call site
		/// </summary>
		/// <param name="context">The context.</param>
		/// <param name="typeSystem">The type system.</param>
		void IIntrinsicMethod.ReplaceIntrinsicCall(Context context, ITypeSystem typeSystem, IList<RuntimeParameter> parameters)
		{
			//var result = context.Result;
			//var op1 = context.Operand1;
            //var op2 = context.Operand2;

            //var eax = new RegisterOperand(BuiltInSigType.IntPtr, GeneralPurposeRegister.EAX);
            //var edx = new RegisterOperand(BuiltInSigType.IntPtr, GeneralPurposeRegister.EDX);
            //var esp = new RegisterOperand(BuiltInSigType.IntPtr, GeneralPurposeRegister.ESP);
            //var ebp = new RegisterOperand(BuiltInSigType.IntPtr, GeneralPurposeRegister.EBP);
            //context.SetInstruction(Instruction.SubInstruction, esp, new ConstantOperand(BuiltInSigType.IntPtr, parameters.Count * 4));
            //context.AppendInstruction(Instruction.MovInstruction, edx, esp);

            //var size = parameters.Count * 4;
            //foreach (var parameter in parameters)
            //{
            //    context.AppendInstruction(Instruction.MovInstruction, new MemoryOperand(BuiltInSigType.IntPtr, edx.Register, new IntPtr(size - 4)), new MemoryOperand(BuiltInSigType.IntPtr, ebp.Register, new IntPtr(size + 8)));
            //    size -= 4;
            //}

            //context.AppendInstruction(Instruction.MovInstruction, eax, op2);
            //context.AppendInstruction(Instruction.CallPointerInstruction, null, new RegisterOperand(BuiltInSigType.IntPtr, GeneralPurposeRegister.EAX));
            //context.AppendInstruction(Instruction.AddInstruction, esp, new ConstantOperand(BuiltInSigType.IntPtr, parameters.Count * 4));

            var result = context.Result;
            var op1 = context.Operand1;
            var op2 = context.Operand2;

            var r8 = new RegisterOperand(BuiltInSigType.IntPtr, GeneralPurposeRegister.R8);
            var r9 = new RegisterOperand(BuiltInSigType.IntPtr, GeneralPurposeRegister.R9);
            var sp = new RegisterOperand(BuiltInSigType.IntPtr, GeneralPurposeRegister.SP);
            var r11 = new RegisterOperand(BuiltInSigType.IntPtr, GeneralPurposeRegister.R11);
            var r10 = new RegisterOperand(BuiltInSigType.UInt32, GeneralPurposeRegister.R10);
            context.SetInstruction(Instruction.SubInstruction, sp, new ConstantOperand(BuiltInSigType.IntPtr, parameters.Count * 4));
            context.AppendInstruction(Instruction.MovInstruction, r9, sp);

            var size = parameters.Count * 4;
            foreach (var parameter in parameters)
            {
                context.AppendInstruction(Instruction.LdInstruction, r10, new MemoryOperand(BuiltInSigType.IntPtr, r11.Register, new IntPtr(size + 8)));
                context.AppendInstruction(Instruction.StInstruction, new MemoryOperand(BuiltInSigType.IntPtr, r9.Register, new IntPtr(size - 4)), r10);
                size -= 4;
            }

            context.AppendInstruction(Instruction.MovInstruction, r8, op2);
            context.AppendInstruction(Instruction.IcallInstruction, null, new RegisterOperand(BuiltInSigType.IntPtr, GeneralPurposeRegister.R8));
            context.AppendInstruction(Instruction.MovInstruction, r10, new ConstantOperand(BuiltInSigType.IntPtr, parameters.Count * 4));
            context.AppendInstruction(Instruction.AddInstruction, sp, r10);
		}

		#endregion // Methods

	}
}
