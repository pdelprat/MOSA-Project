/*
 * (c) 2008 MOSA - The Managed Operating System Alliance
 *
 * Licensed under the terms of the New BSD License.
 *
 * Authors:
 *  Pascal Delprat (pdelprat) <pascal.delprat@online.fr> 
 */

using System.Diagnostics;

using Mosa.Compiler.Framework;
using Mosa.Compiler.Metadata;
using Mosa.Compiler.Metadata.Signatures;
using Mosa.Compiler.Framework.Platform;
using System;

namespace Mosa.Platform.AVR32
{
   /// <summary>
	/// Transforms operations where displacement is too big for AVR32 instruction.
	/// </summary>
	public sealed class FinalTtransformationStage : BaseTransformationStage, IMethodCompilerStage, IPlatformStage, IPipelineStage
	{

		#region IMethodCompilerStage Members

		/// <summary>
		/// Performs stage specific processing on the compiler context.
		/// </summary>
		public override void Run()
		{
			foreach (BasicBlock block in basicBlocks)
			{
				for (Context ctx = CreateContext(block); !ctx.EndOfInstruction; ctx.GotoNext())
				{
					if (ctx.Instruction != null)
					{
							if (ctx.Instruction is Instructions.Mov && ctx.Operand1.IsConstant)
							{
								int value;
								if (!IsConstantBetween(ctx.Operand1, -1048576, 1048575, out value))
								{
									this.HandleSplitMov(ctx);
								}
							}
						
					}
				}
			}
		}

		#endregion // IMethodCompilerStage Members

		// TODO: this is dupplicate method from BaseInstruction 
		private bool IsConstantBetween(Operand op, int lo, int hi, out int value)
		{
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
                case CilElementType.R4:
					goto case CilElementType.I;
				case CilElementType.I8:
				case CilElementType.U8:
				case CilElementType.R8:
					goto default;
				case CilElementType.Object:
					goto case CilElementType.I;
				default:
					throw new NotSupportedException(String.Format(@"CilElementType.{0} is not supported.", op.Type.Type));
			}
		}

		private void HandleSplitMov(Context context)
		{
			Operand opL, opH;

			SplitFromConstantOperand(context.Operand1, out opL, out opH);
			context.SetInstruction(AVR32.Mov, context.Result, opL);
			context.AppendInstruction(AVR32.Orh, context.Result, opH);
		}

		private static void SplitFromConstantOperand(Operand operand, out Operand operandLow, out Operand operandHigh)
		{
			SigType HighType = (operand.Type.Type == CilElementType.I8) ? BuiltInSigType.Int32 : BuiltInSigType.UInt32;
			SigType U4 = BuiltInSigType.UInt32;

			if (HighType.Type == CilElementType.I4)
			{
				long value = Convert.ToInt64(operand.Value);
                operandLow = Operand.CreateConstant(BuiltInSigType.UInt32, (uint)(value & 0xFFFF));
                operandHigh = Operand.CreateConstant(BuiltInSigType.Int32, (int)(value >> 16));
			}
			else
			{
				uint value = Convert.ToUInt32(operand.Value);
                operandLow = Operand.CreateConstant(BuiltInSigType.UInt32, (uint)(value & 0xFFFF));
                operandHigh = Operand.CreateConstant(BuiltInSigType.UInt32, (uint)(value >> 16));
			}
		}

	}
}
