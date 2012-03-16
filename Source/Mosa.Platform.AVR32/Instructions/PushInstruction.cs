/*
 * (c) 2012 MOSA - The Managed Operating System Alliance
 *
 * Licensed under the terms of the New BSD License.
 *
 * Authors:
 *  Phil Garcia (tgiphil) <phil@thinkedge.com>
 *  Pascal Delprat (pdelprat) <pascal.delprat@online.fr>  
 */

using Mosa.Compiler.Framework;
using Mosa.Compiler.Framework.Operands;
using Mosa.Compiler.Metadata.Signatures;
using System;

namespace Mosa.Platform.AVR32.Instructions
{
	/// <summary>
	/// Push Instruction
	/// Substituded by st.w --Rp, Rs
	/// </summary>
	public class PushInstruction : BaseInstruction
	{

		#region Methods

		/// <summary>
		/// Emits the specified platform instruction.
		/// </summary>
		/// <param name="context">The context.</param>
		/// <param name="emitter">The emitter.</param>
		protected override void Emit(Context context, MachineCodeEmitter emitter)
		{
			if (context.Operand1 is RegisterOperand)
			{
				RegisterOperand sp = new RegisterOperand(BuiltInSigType.Int32, GeneralPurposeRegister.SP);
				RegisterOperand register = context.Operand1 as RegisterOperand;
				emitter.EmitTwoRegisterInstructions((byte)0x0D, (byte)sp.Register.RegisterCode, (byte)register.Register.RegisterCode); // st.w --Rp, Rs
			}
			else
				throw new Exception("Not supported combination of operands");
		}

		/// <summary>
		/// Allows visitor based dispatch for this instruction object.
		/// </summary>
		/// <param name="visitor">The visitor object.</param>
		/// <param name="context">The context.</param>
		public override void Visit(IAVR32Visitor visitor, Context context)
		{
			visitor.Push(context);
		}

		#endregion // Methods

	}
}
