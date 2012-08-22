/*
 * (c) 2008 MOSA - The Managed Operating System Alliance
 *
 * Licensed under the terms of the New BSD License.
 *
 * Authors:
 *  Phil Garcia (tgiphil) <phil@thinkedge.com>
 *  Pascal Delprat (pdelprat) <pascal.delprat@online.fr>    
 */

using Mosa.Compiler.Framework;
using Mosa.Compiler.Framework.Platform;
using System.Diagnostics;
using Mosa.Compiler.Metadata;
using System;

namespace Mosa.Platform.AVR32
{
	/// <summary>
	/// 
	/// </summary>
    public abstract class AVR32Instruction : BasePlatformInstruction, IRegisterUsage
	{

        static protected RegisterBitmap NoRegisters = new RegisterBitmap();

		#region Construction

		/// <summary>
		/// Initializes a new instance of the <see cref="AVR32Instruction"/> class.
		/// </summary>
		protected AVR32Instruction()
		{
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AVR32Instruction"/> class.
		/// </summary>
		/// <param name="operandCount">The operand count.</param>
		private AVR32Instruction(byte operandCount)
			: base(operandCount)
		{
		}

		/// <summary>
		/// Initializes a new instance of the <see cref="AVR32Instruction"/> class.
		/// </summary>
		/// <param name="operandCount">The operand count.</param>
		/// <param name="resultCount">The result count.</param>
		protected AVR32Instruction(byte operandCount, byte resultCount)
			: base(operandCount, resultCount)
		{
		}

		#endregion // Construction

		#region Methods

        /// <summary>
        /// Emits the specified platform instruction.
        /// </summary>
        /// <param name="context">The context.</param>
        /// <param name="emitter">The emitter.</param>
        public override void Emit(Context context, ICodeEmitter emitter)
        {
            Emit(context, emitter as MachineCodeEmitter);
        }

		/// <summary>
		/// Computes the opcode.
		/// </summary>
		/// <param name="destination">The destination operand.</param>
		/// <param name="source">The source operand.</param>
		/// <param name="third">The third operand.</param>
		/// <returns></returns>
		protected virtual OpCode ComputeOpCode(Operand destination, Operand source, Operand third)
		{
			throw new System.Exception("opcode not implemented for this instruction");
		}

		/// <summary>
		/// Emits the specified platform instruction.
		/// </summary>
		/// <param name="context">The context.</param>
		/// <param name="emitter">The emitter.</param>
		protected virtual void Emit(Context context, MachineCodeEmitter emitter)
		{
            //OpCode opCode = ComputeOpCode(context.Result, context.Operand1, context.Operand2);
            //emitter.Emit(opCode, context.Result, context.Operand1, context.Operand2);
		}

		#endregion // Methods

		#region Operand Overrides

		/// <summary>
		/// Allows visitor based dispatch for this instruction object.
		/// </summary>
		/// <param name="visitor">The visitor.</param>
		/// <param name="context">The context.</param>
		public virtual void Visit(IAVR32Visitor visitor, Context context)
		{
		}

		/// <summary>
		/// Allows visitor based dispatch for this instruction object.
		/// </summary>
		/// <param name="visitor">The visitor.</param>
		/// <param name="context">The context.</param>
		public override void Visit(IVisitor visitor, Context context)
		{
			if (visitor is IAVR32Visitor)
				Visit(visitor as IAVR32Visitor, context);
		}

        /// <summary>
        /// Returns a string representation of <see cref="ConstantOperand"/>.
        /// </summary>
        /// <returns>A string representation of the operand.</returns>
        public override string ToString()
        {
            return "AVR32." + base.ToString();
        }

		#endregion // Overrides

        #region Typesizes

        protected bool Is8Bit(uint value)
		{
			return ((value & 0x0000FFFF) != value);
		}

		protected bool Is21Bit(uint value)
		{
			return ((value & 0x001FFFFF) != value);
		}

		protected bool IsBetween(int value, int lo, int hi)
		{
			return value >= lo && value <= hi;
		}

		protected bool IsConstantBetween(Operand op, int lo, int hi, out int value)
		{
			Debug.Assert(op.IsConstant);

			value = 0;
			switch (op.Type.Type)
			{
				case CilElementType.I:
					try
					{
						if (op.Value is Token)
						{
							value = ((Token)op.Value).ToInt32();
							return value >= lo && value <= hi;
						}
						else
						{
							value = Convert.ToInt32(op.Value);
							return value >= lo && value <= hi;
						}
					}
					catch (OverflowException)
					{
						return false;
					}
				case CilElementType.I1:
				case CilElementType.I2:
				case CilElementType.I4:
				case CilElementType.U1:
				case CilElementType.Char:
				case CilElementType.U2:
				case CilElementType.Ptr:
				case CilElementType.U4:
					goto case CilElementType.I;
				case CilElementType.I8:
				case CilElementType.U8:
				case CilElementType.R4:
				case CilElementType.R8:
					goto default;
				case CilElementType.Object:
					goto case CilElementType.I;
				default:
					throw new NotSupportedException(String.Format(@"CilElementType.{0} is not supported.", op.Type.Type));
			}
        }

        #endregion

        #region IRegisterUsage

        /// <summary>
        /// Gets the output registers.
        /// </summary>
        /// <param name="context">The context.</param>
        /// <returns></returns>
        public virtual RegisterBitmap GetOutputRegisters(Context context)
        {
            RegisterBitmap registers = new RegisterBitmap();

            if (context.Result.IsRegister)
                registers.Set(context.Result.Register);

            registers.Or(AdditionalOutputRegisters);

            return registers;
        }

        /// <summary>
        /// Gets the input registers.
        /// </summary>
        /// <param name="context">The context.</param>
        /// <returns></returns>
        public virtual RegisterBitmap GetInputRegisters(Context context)
        {
            RegisterBitmap registers = new RegisterBitmap();

            registers.Set(GetRegister(context.Operand1, true));
            registers.Set(GetRegister(context.Operand2, true));
            registers.Set(GetRegister(context.Operand3, true));
            registers.Set(GetRegister(context.Result, ResultIsInput));

            registers.Or(AdditionalInputRegisters);

            return registers;
        }

        /// <summary>
        /// Gets a value indicating whether [result is input].
        /// </summary>
        /// <value>
        ///   <c>true</c> if [result is input]; otherwise, <c>false</c>.
        /// </value>
        public virtual bool ResultIsInput { get { return true; } }

        /// <summary>
        /// Gets the additional output registers.
        /// </summary>
        public virtual RegisterBitmap AdditionalOutputRegisters { get { return NoRegisters; } }

        /// <summary>
        /// Gets the additional input registers.
        /// </summary>
        public virtual RegisterBitmap AdditionalInputRegisters { get { return NoRegisters; } }

        #endregion // IRegisterUsage

        /// <summary>
        /// Gets the register.
        /// </summary>
        /// <param name="operand">The operand.</param>
        /// <returns></returns>
        protected Register GetRegister(Operand operand, bool includeRegister)
        {
            if (operand == null)
                return null;

            if (includeRegister)
            {
                if (operand.IsRegister)
                    return operand.Register;
            }

            if (operand.IsMemoryAddress || operand.IsParameter)
            {
                return operand.Register;
            }

            return null;
        }
    }
}
