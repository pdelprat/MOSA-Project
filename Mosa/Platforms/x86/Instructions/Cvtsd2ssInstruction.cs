﻿/*
 * (c) 2008 MOSA - The Managed Operating System Alliance
 *
 * Licensed under the terms of the New BSD License.
 *
 * Authors:
 *  Michael Ruck (<mailto:sharpos@michaelruck.de>)
 *
 */

using System;

using Mosa.Runtime.CompilerFramework;
using IR = Mosa.Runtime.CompilerFramework.IR;

namespace Mosa.Platforms.x86.Instructions
{
    /// <summary>
    /// Intermediate representation for the x86 cvtsd2ss instruction.
    /// </summary>
    public class Cvtsd2ssInstruction : IR.TwoOperandInstruction
    {
        #region Construction

        /// <summary>
        /// Initializes a new instance of the <see cref="Cvtsd2ssInstruction"/> class.
        /// </summary>
        public Cvtsd2ssInstruction()
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="Cvtsd2ssInstruction"/> class.
        /// </summary>
        /// <param name="result">The result operand.</param>
        /// <param name="op1">The source operand.</param>
        public Cvtsd2ssInstruction(Operand result, Operand op1) :
            base(result, op1)
        {
        }

        #endregion // Construction

        #region TwoOperandInstruction Overrides

        /// <summary>
        /// Returns a string representation of the instruction.
        /// </summary>
        /// <returns>
        /// A string representation of the instruction in intermediate form.
        /// </returns>
        public override string ToString()
        {
            return String.Format("x86 cvtsd2ss {0}, {1} ; {0} = (float){1}", this.Operand0, this.Operand1);
        }

        /// <summary>
        /// Visits the specified visitor.
        /// </summary>
        /// <typeparam name="ArgType">The type of the rg type.</typeparam>
        /// <param name="visitor">The visitor.</param>
        /// <param name="arg">The arg.</param>
        protected override void Visit<ArgType>(IR.IIRVisitor<ArgType> visitor, ArgType arg)
        {
            IX86InstructionVisitor<ArgType> x86v = visitor as IX86InstructionVisitor<ArgType>;
            if (x86v != null)
            {
                x86v.Cvtsd2ss(this, arg);
            }
            else
            {
                visitor.Visit(this, arg);
            }
        }

        #endregion // TwoOperandInstruction Overrides
    }
}
