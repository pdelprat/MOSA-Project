﻿/*
 * (c) 2008 MOSA - The Managed Operating System Alliance
 *
 * Licensed under the terms of the New BSD License.
 *
 * Authors:
 *  Phil Garcia (tgiphil) <phil@thinkedge.com>
 */

namespace Mosa.Kernel.Memory.X86
{
	/// <summary>
	/// 
	/// </summary>
	public static class PageTable
	{
		private static uint pageDirectory;

		/// <summary>
		/// Sets up the PageManager
		/// </summary>
		public static void Setup()
		{
			SetupPageDirectory();
		}

		/// <summary>
		/// Sets up the page directory.
		/// </summary>
		public static void SetupPageDirectory()
		{
			// Get Page for Page Directory
			pageDirectory = PageAllocator.Allocate();

			// Clear out that page
			Memory.X86.Util.Clear(pageDirectory, 4096);
		}

		/// <summary>
		/// Gets the page table.
		/// </summary>
		/// <param name="location">The location.</param>
		/// <returns></returns>
		public static uint GetPageTable(uint location)
		{
			uint index = location >> 22;

			uint entry = Memory.X86.Util.Get32(location + (index * sizeof(uint)));

			// Check if Page Directory Entry does exist
			if ((entry >> 12) != 0)
				return (uint)(entry & ~(0x03FF));

			// Page Directory Entry does not exists, so create one

			// Get Page for Page Table Entry
			uint pageEntry = PageAllocator.Allocate();

			// Clear out that page
			Memory.X86.Util.Clear(pageEntry, 4096);

			// Set the Page Directory Entry to the new Page Table
			// 0x02 = Read/Write, 0x01 = Present, 0x100 = Available (used to mark this page is not swappable to disk)
			Memory.X86.Util.Set32(location + (index * sizeof(uint)), (uint)(pageEntry & ~(0x03FF)) | 0x02 | 0x01 | 0x100);

			// TODO: Flush TLB?

			return pageEntry;
		}

		/// <summary>
		/// Maps the virtual address to physical.
		/// </summary>
		/// <param name="physicalAddress">The physical address.</param>
		/// <param name="virtualAddress">The virtual address.</param>
		/// <param name="flags">The flags.</param>
		public static void MapVirtualAddressToPhysical(uint physicalAddress, uint virtualAddress, uint flags)
		{
			uint pageTable = GetPageTable(virtualAddress);
			uint pageTableIndex = virtualAddress >> 12 & 0x03FF;

			Memory.X86.Util.Set32(pageTable + (pageTableIndex * 4), (uint)(physicalAddress | (flags & 0xFFF) | 0x01));
		}

		/// <summary>
		/// Releases the virtual address.
		/// </summary>
		/// <param name="virtualAddress">The virtual address.</param>
		public static void ReleaseVirtualAddress(uint virtualAddress)
		{
			uint pageTable = GetPageTable(virtualAddress);
			uint pageTableIndex = virtualAddress >> 12 & 0x03FF;

			Memory.X86.Util.Set32(pageTable + (pageTableIndex * 4), 0x00);
		}

		/// <summary>
		/// Sets up the identity pages.
		/// </summary>
		/// <param name="start">The start.</param>
		/// <param name="end">The end.</param>
		/// <param name="readOnly">if set to <c>true</c> [read only].</param>
		public static void SetupIdentityPages(uint start, uint end, bool readOnly)
		{
			// force alignment to 4Kb Page
			start = (uint)(start & ~(0xFFF));
			end = (uint)(end & ~(0xFFF));

			uint flag = (uint)(readOnly ? 0x00 : 0x02);

			for (uint mem = start; mem <= end; mem = mem + 4096)
				MapVirtualAddressToPhysical(mem, mem, flag);
		}
	}
}


