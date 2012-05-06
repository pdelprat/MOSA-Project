﻿/*
 * (c) 2012 MOSA - The Managed Operating System Alliance
 *
 * Licensed under the terms of the New BSD License.
 *
 * Authors:
 *  Pascal Delprat (pdelprat) <pascal.delprat@online.fr>    
 */

using Mosa.Compiler.Framework;
using Mosa.Compiler.Framework.Operands;
using System;
using Mosa.Compiler.Metadata.Signatures;

namespace Mosa.Platform.AVR32.Instructions
{
    /// <summary>
    /// Ldub Instruction
    /// Supported Format:
    ///     ld.ub Rd, Rp[disp] 3 bits
    ///     ld.ub Rd, Rp[disp] 16 bits
    /// </summary>
    public class LdubInstruction : BaseInstruction
    {
        #region Methods

        /// <summary>
        /// Emits the specified platform instruction.
        /// </summary>
        /// <param name="context">The context.</param>
        /// <param name="emitter">The emitter.</param>
        protected override void Emit(Context context, MachineCodeEmitter emitter)
        {
            // TODO: Remove
            if (context.Operand1 is MemberOperand)
                return;
            if (context.Result is RegisterOperand && context.Operand1 is MemoryOperand)
            {
                RegisterOperand result = context.Result as RegisterOperand;
                MemoryOperand operand = context.Operand1 as MemoryOperand;

                int displacement = operand.Offset.ToInt32();

                if (IsBetween(displacement, 0, 7))
                {
                    emitter.EmitTwoRegisterInstructions((byte)(0x0C & displacement),  (byte)operand.Base.RegisterCode, (byte)result.Register.RegisterCode);
                }
                else
                    if (IsBetween(displacement, -32768, 32767))
                    {
                        emitter.EmitTwoRegistersAndK16(0x13, (byte)operand.Base.RegisterCode, (byte)result.Register.RegisterCode, (short)displacement);
                    }
                    else
                        throw new OverflowException();
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
            visitor.Ldub(context);
        }

        #endregion // Methods

    }
}
