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
using System.Text;

namespace Mosa.Kernel
{
	/// <summary>
	/// Kernel Memory Allocator - This is a pure HACK!
	/// </summary>
	class KernelGCMemory
	{
		static private uint _heap = 0;
		static private uint _size = 0;
		static private uint _used = 0;

		static public uint AllocateMemory(uint size)
		{
			if ((_heap == 0) || (size > (_size - _used)))
			{
				// Go allocate memory

				_size = 1024 * 1024 * 64; // 64Mb
				_heap = X86.ProcessManager.AllocateMemory(0, _size);
				_used = 0;
			}
						
			uint at = _heap + _used;
			_used = _used + _size;
			return at;
		}

	}
}
