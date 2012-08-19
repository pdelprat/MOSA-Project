﻿/*
 * (c) 2008 MOSA - The Managed Operating System Alliance
 *
 * Licensed under the terms of the New BSD License.
 *
 * Authors:
 *  Michael Ruck (grover) <sharpos@michaelruck.de>
 */


namespace Mosa.Compiler.Framework.IR
{
	/// <summary>
	/// Abstract base class for IR instructions with three operands.
	/// </summary>
	/// <remarks>
	/// The <see cref="ThreeOperandInstruction"/> is the base class for
	/// IR instructions using three operands. It provides properties to
	/// easily access the individual operands.
	/// </remarks>
	public abstract class ThreeOperandInstruction : BaseIRInstruction
	{

		#region Construction

		/// <summary>
		/// Initializes a new instance of <see cref="ThreeOperandInstruction"/>.
		/// </summary>
		public ThreeOperandInstruction() :
			base(2, 1)
		{
		}

		#endregion // Construction

	}
}
