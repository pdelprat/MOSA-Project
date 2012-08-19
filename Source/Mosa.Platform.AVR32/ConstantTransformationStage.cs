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
    /// Transforms operations where constant is used in AND, ADD (TODO: and more) instruction for AVR32 instruction.
    /// </summary>
    public sealed class ConstantTransformationStage : BaseTransformationStage, IMethodCompilerStage, IPlatformStage, IPipelineStage
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
                        if (ctx.Instruction is Instructions.And && IsConstantOperand(ctx.Operand1))
                        {
                            this.HandleSplitAnd(ctx);
                        }
                        if (ctx.Instruction is Instructions.Add && IsConstantOperand(ctx.Operand1))
                        {
                            this.HandleSplitAdd(ctx);
                        }

                    }
                }
            }
        }

        #endregion // IMethodCompilerStage Members

        private bool IsConstantOperand(Operand op)
        {
            return op.IsConstant;
        }

        private void HandleSplitAnd(Context context)
        {
            Operand r8 = Operand.CreateCPURegister(context.Operand1.Type, GeneralPurposeRegister.R8);

            context.SetInstruction(AVR32.Mov, r8, context.Operand1);
            context.AppendInstruction(AVR32.And, context.Result, r8);
        }

        private void HandleSplitAdd(Context context)
        {
            Operand r8 = Operand.CreateCPURegister(context.Operand1.Type, GeneralPurposeRegister.R8);

            context.SetInstruction(AVR32.Mov, r8, context.Operand1);
            context.AppendInstruction(AVR32.Add, context.Result, r8);
        }

    }
}
