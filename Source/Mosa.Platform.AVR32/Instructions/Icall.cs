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
using System;

namespace Mosa.Platform.AVR32.Instructions
{
	/// <summary>
	/// iCall Instruction
    /// Supported format:
    ///     icall Rd
	/// </summary>
	public class Icall : AVR32Instruction
	{

		#region Methods

		/// <summary>
		/// Emits the specified platform instruction.
		/// </summary>
		/// <param name="context">The context.</param>
		/// <param name="emitter">The emitter.</param>
		protected override void Emit(Context context, MachineCodeEmitter emitter)
		{
            if (context.Operand1.IsRegister)
            {         
                 emitter.EmitSingleRegisterInstructions(0x11, (byte)context.Operand1.Register.RegisterCode);   
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
			visitor.Icall(context);
		}

		#endregion // Methods

	}
}
