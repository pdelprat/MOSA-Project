﻿/*
 * (c) 2008 MOSA - The Managed Operating System Alliance
 *
 * Licensed under the terms of the New BSD License.
 *
 * Authors:
 *  Phil Garcia (tgiphil) <phil@thinkedge.com>
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using Mosa.Runtime.CompilerFramework.CIL;

namespace Mosa.Runtime.CompilerFramework
{
	/// <summary>
	/// Base class for code generation stages.
	/// </summary>
	public class CodeGenerationStage : BaseMethodCompilerStage, ICodeGenerationStage, IMethodCompilerStage, IPipelineStage
	{

		#region Data members

		/// <summary>
		/// Holds the stream, where code is emitted to.
		/// </summary>
		protected static Stream codeStream;

		/// <summary>
		/// 
		/// </summary>
		protected ICodeEmitter codeEmitter;

		#endregion // Data members

		#region IPipelineStage Members

		/// <summary>
		/// Retrieves the name of the compilation stage.
		/// </summary>
		/// <value></value>
		string IPipelineStage.Name { get { return @"CodeGenerationStage"; } }

		#endregion // IPipelineStage Members

		#region Methods

		/// <summary>
		/// Performs stage specific processing on the compiler context.
		/// </summary>
		public void Run()
		{

			// Retrieve a stream to place the code into
			using (codeStream = methodCompiler.RequestCodeStream())
			{
				// HINT: We need seeking to resolve labels.
				Debug.Assert(codeStream.CanSeek, @"Can't seek code output stream.");
				Debug.Assert(codeStream.CanWrite, @"Can't write to code output stream.");

				if (!codeStream.CanSeek || !codeStream.CanWrite)
					throw new NotSupportedException(@"Code stream doesn't support seeking or writing.");

				// Emit method prologue
				BeginGenerate();

				// Emit all instructions
				EmitInstructions();

				// Emit the method epilogue
				EndGenerate();
			}
		}

		/// <summary>
		/// Called to emit a list of instructions offered by the instruction provider.
		/// </summary>
		protected virtual void EmitInstructions()
		{
			//ExceptionClauseHeader exceptionClauseHeader = this.methodCompiler.Method.ExceptionClauseHeader;
			foreach (BasicBlock block in basicBlocks)
			{
				BlockStart(block);

				for (Context ctx = new Context(InstructionSet, block); !ctx.EndOfInstruction; ctx.GotoNext())
					if (ctx.Instruction != null)
						if (!ctx.Ignore)
						{
							if (block.ExceptionHeaderClause != null)
							{
								block.ExceptionHeaderClause.Update(ctx, codeStream);
								block.ExceptionHeaderClause.AddLabelToCodeStream(codeEmitter);
							}
							IPlatformInstruction instruction = ctx.Instruction as IPlatformInstruction;
							if (instruction != null)
								instruction.Emit(ctx, codeEmitter);
							else
								Debug.WriteLine("Missing Code Transformation: " + ctx.ToString());
						}

				BlockEnd(block);
			}
		}

		/// <summary>
		/// Begins the generate.
		/// </summary>
		protected virtual void BeginGenerate()
		{
			codeEmitter = architecture.GetCodeEmitter();
			codeEmitter.Initialize(methodCompiler, codeStream, methodCompiler.Linker);
		}

		/// <summary>
		/// Start of code generation for a block.
		/// </summary>
		/// <param name="block">The started block.</param>
		protected virtual void BlockStart(BasicBlock block)
		{
			codeEmitter.Label(block.Label);
		}

		/// <summary>
		/// Completion of code generation for a block.
		/// </summary>
		/// <param name="block">The completed block.</param>
		protected virtual void BlockEnd(BasicBlock block)
		{
		}

		/// <summary>
		/// Code generation completed.
		/// </summary>
		protected virtual void EndGenerate()
		{
			codeEmitter.Dispose();
			codeEmitter = null;
		}

		#endregion // Methods

	}
}
