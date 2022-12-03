/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2017-2021 Maarten L. Hekkelman
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

//
// 	mrc, A simple resource compiler.
//
//	Use this program to make data available to your program without
//	having to distribute this data in separate files.
//
//	Example usage: mrc -o myrsrc.o rsrc/
//
//	This will create an object file called myrsrs.o containing the data for all file found in the rsrc/ directory.

#include <filesystem>
#include <fstream>
#include <iostream>

#if __has_include(<elf.h>)
#include <elf.h>
#elif not defined(EM_NONE)
#define EM_NONE 0
#endif

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <mcfp/mcfp.hpp>

#include "mrsrc.h"
#include "revision.hpp"

#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

namespace fs = std::filesystem;

int VERBOSE = 0;

// --------------------------------------------------------------------

uint32_t AddNameToNameTable(std::string &ioNameTable, const std::string &inName)
{
	uint32_t result = 0;

	const char *p = ioNameTable.c_str();
	const char *e = p + ioNameTable.length();

	while (p < e)
	{
		if (inName == p)
		{
			result = p - ioNameTable.c_str();
			break;
		}

		p += strlen(p) + 1;
	}

	if (p >= e)
	{
		result = ioNameTable.length();
		ioNameTable.append(inName);
		ioNameTable.append("\0", 1);
	}

	return result;
}

// --------------------------------------------------------------------

struct MObjectFileImp
{
	fs::path mFile;
	uint32_t mTextSize;
	uint32_t mDataSize;

	virtual ~MObjectFileImp() {}

	virtual void Write(std::ofstream &inFile) = 0;

	static MObjectFileImp *Create(int machine, int elf_class, int elf_data, int elf_abi, int flags);

  protected:
	friend class MObjectFile;

	struct MGlobal
	{
		std::string name;
		std::string data;
	};

	typedef std::vector<MGlobal> MGlobals;

	MGlobals mGlobals;
};

// --------------------------------------------------------------------
// byte swapping

namespace Swap
{

struct no_swapper
{
	template <typename T>
	T operator()(T inValue) const { return inValue; }
};

struct swapper
{
	template <typename T>
	T operator()(T inValue) const { return inValue; }
};

template <>
inline int16_t swapper::operator()(int16_t inValue) const
{
	return static_cast<int16_t>(
		((inValue & 0xFF00UL) >> 8) |
		((inValue & 0x00FFUL) << 8));
}

template <>
inline uint16_t swapper::operator()(uint16_t inValue) const
{
	return static_cast<uint16_t>(
		((inValue & 0xFF00UL) >> 8) |
		((inValue & 0x00FFUL) << 8));
}

template <>
inline int32_t swapper::operator()(int32_t inValue) const
{
	return static_cast<int32_t>(
		((inValue & 0xFF000000UL) >> 24) |
		((inValue & 0x00FF0000UL) >> 8) |
		((inValue & 0x0000FF00UL) << 8) |
		((inValue & 0x000000FFUL) << 24));
}

template <>
inline uint32_t swapper::operator()(uint32_t inValue) const
{
	return static_cast<uint32_t>(
		((inValue & 0xFF000000UL) >> 24) |
		((inValue & 0x00FF0000UL) >> 8) |
		((inValue & 0x0000FF00UL) << 8) |
		((inValue & 0x000000FFUL) << 24));
}

template <>
inline int64_t swapper::operator()(int64_t inValue) const
{
	return static_cast<int64_t>(
		(((static_cast<uint64_t>(inValue)) << 56) & 0xFF00000000000000ULL) |
		(((static_cast<uint64_t>(inValue)) << 40) & 0x00FF000000000000ULL) |
		(((static_cast<uint64_t>(inValue)) << 24) & 0x0000FF0000000000ULL) |
		(((static_cast<uint64_t>(inValue)) << 8) & 0x000000FF00000000ULL) |
		(((static_cast<uint64_t>(inValue)) >> 8) & 0x00000000FF000000ULL) |
		(((static_cast<uint64_t>(inValue)) >> 24) & 0x0000000000FF0000ULL) |
		(((static_cast<uint64_t>(inValue)) >> 40) & 0x000000000000FF00ULL) |
		(((static_cast<uint64_t>(inValue)) >> 56) & 0x00000000000000FFULL));
}

template <>
inline uint64_t swapper::operator()(uint64_t inValue) const
{
	return static_cast<uint64_t>(
		((((uint64_t)inValue) << 56) & 0xFF00000000000000ULL) |
		((((uint64_t)inValue) << 40) & 0x00FF000000000000ULL) |
		((((uint64_t)inValue) << 24) & 0x0000FF0000000000ULL) |
		((((uint64_t)inValue) << 8) & 0x000000FF00000000ULL) |
		((((uint64_t)inValue) >> 8) & 0x00000000FF000000ULL) |
		((((uint64_t)inValue) >> 24) & 0x0000000000FF0000ULL) |
		((((uint64_t)inValue) >> 40) & 0x000000000000FF00ULL) |
		((((uint64_t)inValue) >> 56) & 0x00000000000000FFULL));
}

#if defined(LITTLE_ENDIAN)
typedef no_swapper lsb_swapper;
typedef swapper msb_swapper;
#elif defined(BIG_ENDIAN)
typedef swapper lsb_swapper;
typedef no_swapper msb_swapper;
#else
#error Undefined endianness
#endif

} // namespace Swap

// --------------------------------------------------------------------
// Write data aligned to some alignment value

uint32_t WriteDataAligned(std::ofstream &inStream, const void *inData, uint32_t inSize, uint32_t inAlignment = 1)
{
	inStream.write(reinterpret_cast<const char *>(inData), inSize);

	if (inAlignment > 1)
	{
		while ((inStream.tellp() % inAlignment) != 0)
			inStream.put('\0');
	}

	return inStream.tellp();
}

// --------------------------------------------------------------------
//
//	Implementation for ELF
#if __has_include(<elf.h>)

template <int ELF_CLASSXXXX>
struct ElfClass;

template <>
struct ElfClass<ELFCLASS32>
{
	using Elf_Ehdr = Elf32_Ehdr;
	using Elf_Shdr = Elf32_Shdr;
	using Elf_Sym = Elf32_Sym;

	using Elf_Word = Elf64_Word;
	using Elf_Half = Elf64_Half;
};

template <>
struct ElfClass<ELFCLASS64>
{
	using Elf_Ehdr = Elf64_Ehdr;
	using Elf_Shdr = Elf64_Shdr;
	using Elf_Sym = Elf64_Sym;

	using Elf_Word = Elf64_Word;
	using Elf_Half = Elf64_Half;
};

template <int ELF_DATAXX>
struct ElfData;

template <>
struct ElfData<ELFDATA2LSB>
{
	using swapper = Swap::msb_swapper;
};

template <>
struct ElfData<ELFDATA2MSB>
{
	using swapper = Swap::lsb_swapper;
};

template <int ELF_CLASSXX, int ELF_DATAXX>
struct MELFObjectFileImp : public MObjectFileImp
{
	using swapper = typename ElfData<ELF_DATAXX>::swapper;

	using Elf_Ehdr = typename ElfClass<ELF_CLASSXX>::Elf_Ehdr;
	using Elf_Shdr = typename ElfClass<ELF_CLASSXX>::Elf_Shdr;
	using Elf_Sym = typename ElfClass<ELF_CLASSXX>::Elf_Sym;

	using Elf_Word = typename ElfClass<ELF_CLASSXX>::Elf_Word;
	using Elf_Half = typename ElfClass<ELF_CLASSXX>::Elf_Half;

	virtual void Write(std::ofstream &inFile) override;

	MELFObjectFileImp(int machine, uint8_t elf_abi, int flags)
		: MObjectFileImp()
		, mMachine(machine)
		, mABI(elf_abi)
		, mFlags(flags)
	{
	}

	Elf_Half mMachine;
	uint8_t mABI;
	Elf_Word mFlags;
};

enum
{
	kNullSection,
	kTextSection,
	kDataSection,
	kBssSection,
	kShStrtabSection,
	kSymtabSection,
	kStrtabSection,

	kSectionCount
};

enum
{
	kNullSymbol,
	kTextSectionSymbol,
	kDataSectionSymbol,
	kBssSectionSymbol,
	kGlobalSymbol,

	kSymbolCount
};

template <int ELF_CLASSXX, int ELF_DATAXX>
void MELFObjectFileImp<ELF_CLASSXX, ELF_DATAXX>::Write(std::ofstream &f)
{
	Elf_Ehdr eh = {
		// e_ident
		{ ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3,
			ELF_CLASSXX, ELF_DATAXX, EV_CURRENT, mABI },
		ET_REL,           // e_type
		mMachine,         // e_machine
		EV_CURRENT,       // e_version
		0,                // e_entry
		0,                // e_phoff
		0,                // e_shoff
		mFlags,           // e_flags
		sizeof(Elf_Ehdr), // e_ehsize
		0,                // e_phentsize
		0,                // e_phnum
		sizeof(Elf_Shdr), // e_shentsize
		kSectionCount,    // e_shnum
		kShStrtabSection  // e_shstrndx
	};

	uint32_t data_offset = WriteDataAligned(f, &eh, sizeof(eh), 16);

	std::string strtab;
	AddNameToNameTable(strtab, ""); // null name

	Elf_Sym sym = {};
	std::vector<Elf_Sym> syms;

	// kNullSymbol
	syms.push_back(sym);

	// text section symbol
	sym.st_info = ELF32_ST_INFO(STB_LOCAL, STT_SECTION);
	sym.st_shndx = kTextSection;
	syms.push_back(sym);

	// data section symbol
	sym.st_info = ELF32_ST_INFO(STB_LOCAL, STT_SECTION);
	sym.st_shndx = kDataSection;
	syms.push_back(sym);

	// bss section symbol
	sym.st_info = ELF32_ST_INFO(STB_LOCAL, STT_SECTION);
	sym.st_shndx = kBssSection;
	syms.push_back(sym);

	uint32_t sym_offset = data_offset;

	for (const auto &[name, data] : mGlobals)
	{
		sym.st_name = AddNameToNameTable(strtab, name);
		sym.st_value = sym_offset - data_offset;
		sym.st_size = data.length();
		sym.st_info = ELF32_ST_INFO(STB_GLOBAL, STT_OBJECT);
		sym.st_shndx = kDataSection;

		syms.push_back(sym);

		sym_offset = WriteDataAligned(f, data.c_str(), data.length(), 8);
	}

	uint32_t data_size = sym_offset;

	uint32_t symtab_off = sym_offset;
	assert((sizeof(Elf_Sym) % 8) == 0);

	uint32_t symtab_size = syms.size() * sizeof(sym);
	uint32_t strtab_off = WriteDataAligned(f, syms.data(), symtab_size, 8);
	uint32_t shstrtab_off = WriteDataAligned(f, strtab.c_str(), strtab.length(), 8);

	std::string shstrtab;
	(void)AddNameToNameTable(shstrtab, ""); // null name
	(void)AddNameToNameTable(shstrtab, ".text");
	(void)AddNameToNameTable(shstrtab, ".rsrc_data");
	(void)AddNameToNameTable(shstrtab, ".bss");
	(void)AddNameToNameTable(shstrtab, ".shstrtab");
	(void)AddNameToNameTable(shstrtab, ".symtab");
	(void)AddNameToNameTable(shstrtab, ".strtab");

	eh.e_shoff = WriteDataAligned(f, shstrtab.c_str(), shstrtab.length(), 16);

	Elf_Shdr sh[kSectionCount] = {
		{
			// kNullSection
		},
		{
			// kTextSection
		    // sh_name
			AddNameToNameTable(shstrtab, ".text"),
			SHT_PROGBITS, // sh_type
		                  // sh_flags
			SHF_ALLOC | SHF_EXECINSTR,
			0,           // sh_addr
			data_offset, // sh_offset
			0,           // sh_size
			0,           // sh_link
			0,           // sh_info
			4,           // sh_addralign
			0            // sh_entsize
		},
		{
			// kDataSection
		    // sh_name
			AddNameToNameTable(shstrtab, ".rsrc_data"),
			SHT_PROGBITS, // sh_type
		                  // sh_flags
			SHF_ALLOC | SHF_WRITE,
			0,           // sh_addr
			data_offset, // sh_offset
			data_size,   // sh_size
			0,           // sh_link
			0,           // sh_info
			4,           // sh_addralign
			0            // sh_entsize
		},
		{
			// kBssSection
		    // sh_name
			AddNameToNameTable(shstrtab, ".bss"),
			SHT_NOBITS, // sh_type
		                // sh_flags
			SHF_ALLOC | SHF_WRITE,
			0,          // sh_addr
			eh.e_shoff, // sh_offset
			0,          // sh_size
			0,          // sh_link
			0,          // sh_info
			4,          // sh_addralign
			0           // sh_entsize
		},
		{
			// kShStrtabSection
		    // sh_name
			AddNameToNameTable(shstrtab, ".shstrtab"),
			SHT_STRTAB,                    // sh_type
			0,                             // sh_flags
			0,                             // sh_addr
			shstrtab_off,                  // sh_offset
			Elf32_Word(shstrtab.length()), // sh_size
			0,                             // sh_link
			0,                             // sh_info
			1,                             // sh_addralign
			0                              // sh_entsize
		},
		{
			// kSymtabSection
		    // sh_name
			AddNameToNameTable(shstrtab, ".symtab"),
			SHT_SYMTAB, // sh_type
		                // sh_flags
			0,
			0,              // sh_addr
			symtab_off,     // sh_offset
			symtab_size,    // sh_size
			6,              // sh_link
			kGlobalSymbol,  // sh_info
			8,              // sh_addralign
			sizeof(Elf_Sym) // sh_entsize
		},
		{
			// kStrtabSection
		    // sh_name
			AddNameToNameTable(shstrtab, ".strtab"),
			SHT_STRTAB, // sh_type
		                // sh_flags
			0,
			0,                           // sh_addr
			strtab_off,                  // sh_offset
			Elf32_Word(strtab.length()), // sh_size
			0,                           // sh_link
			0,                           // sh_info
			1,                           // sh_addralign
			0                            // sh_entsize
		},
	};

	WriteDataAligned(f, sh, sizeof(sh), 1);

	f.flush();

	f.seekp(0);
	WriteDataAligned(f, &eh, sizeof(eh));
}

#endif

// --------------------------------------------------------------------
// PE/COFF

enum COFF_Machine : uint16_t
{
	IMAGE_FILE_MACHINE_AMD64 = 0x8664,
	IMAGE_FILE_MACHINE_I386 = 0x14c,
	IMAGE_FILE_MACHINE_ARM64 = 0xaa64
};

enum COFF_HeaderCharacteristics : uint16_t
{
	IMAGE_FILE_RELOCS_STRIPPED = 0x0001,
	IMAGE_FILE_LINE_NUMS_STRIPPED = 0x0004
};

struct COFF_Header
{
	COFF_Machine machine = IMAGE_FILE_MACHINE_AMD64;
	uint16_t numberOfSections;
	uint32_t timeDateStamp = 0;
	uint32_t pointerToSymbolTable;
	uint32_t numberOfSymbols;
	uint16_t sizeOfOptionalHeader;
	uint16_t characteristics = 0;
};

union COFF_Name
{
	char str[8] = "";
	struct
	{
		uint32_t _filler_;
		uint32_t offset;
	};
};

static_assert(sizeof(COFF_Header) == 20, "COFF_Header size should be 20 bytes");

enum COFF_SectionHeaderCharacteristics : uint32_t
{
	IMAGE_SCN_CNT_CODE = 0x00000020,
	IMAGE_SCN_LNK_INFO = 0x00000200,
	IMAGE_SCN_LNK_REMOVE = 0x00000800,
	IMAGE_SCN_CNT_INITIALIZED_DATA = 0x00000040,
	IMAGE_SCN_CNT_UNINITIALIZED_DATA = 0x00000080,
	IMAGE_SCN_ALIGN_1BYTES = 0x00100000,
	IMAGE_SCN_ALIGN_8BYTES = 0x00400000,
	IMAGE_SCN_ALIGN_16BYTES = 0x00500000,
	IMAGE_SCN_MEM_EXECUTE = 0x20000000,
	IMAGE_SCN_MEM_READ = 0x40000000,
	IMAGE_SCN_MEM_WRITE = 0x80000000
};

struct COFF_SectionHeader
{
	COFF_Name name;
	uint32_t virtualSize;
	uint32_t virtualAddress;
	uint32_t sizeOfRawData;
	uint32_t pointerToRawData;
	uint32_t pointerToRelocations;
	uint32_t pointerToLineNumbers;
	uint16_t numberOfRelocations;
	uint16_t numberOfLineNumbers;
	uint32_t characteristics;
};

static_assert(sizeof(COFF_SectionHeader) == 40, "Section headers should be 40 bytes");

enum COFF_StorageClass : uint8_t
{
	IMAGE_SYM_CLASS_EXTERNAL = 0x02,
	IMAGE_SYM_CLASS_STATIC = 0x03,
	IMAGE_SYM_CLASS_FILE = 0x67
};

/// \brief COFF symbol table entry, should be 18 bytes... right...
struct COFF_Symbol
{
	COFF_Name name;
	uint32_t value;
	int16_t sectionNumber;
	uint16_t type;
	uint8_t storageClass;
	uint8_t numberOfAuxSymbols;
	uint16_t _filler_;
};

static_assert(sizeof(COFF_Symbol) == 20, "Symbols should be 18 bytes, plus that filler of 2 bytes which is not written...");

struct COFF_Relocation
{
	uint32_t virtualAddress;
	uint32_t symbolTableIndex;
	uint16_t type;
};

// --------------------------------------------------------------------

struct MCOFFObjectFileImp : public MObjectFileImp
{
	using swapper = Swap::lsb_swapper;

	virtual void Write(std::ofstream &inFile) override;

	MCOFFObjectFileImp(COFF_Machine machine)
		: MObjectFileImp()
		, mMachine(machine)
	{
	}

	COFF_Machine mMachine;
};

void MCOFFObjectFileImp::Write(std::ofstream &f)
{

	// Start by allocating a header
	COFF_Header header = {
		mMachine, // machine
		0,        // numberOfSections
		0,        // timeDateStamp
		0,        // pointerToSymbolTable
		0,        // numberOfSymbols
		0,        // sizeOfOptionalHeader
		0,        // characteristics
	};

	std::string strtab;

	auto addName = [&strtab](const std::string &name)
	{
		COFF_Name result{};
		if (name.length() <= 8)
			name.copy(result.str, name.length());
		else
			result.offset = 4 + AddNameToNameTable(strtab, name);
		return result;
	};

	auto sectionHeaderStart = WriteDataAligned(f, &header, sizeof(header), 1);

	COFF_SectionHeader sectionHeaders[] = {
		{
			addName(".rdata"), // name
			0,                 // virtualSize
			0,                 // virtualAddress
			0,                 // sizeOfRawData
			0,                 // pointerToRawData
			0,                 // pointerToRelocations
			0,                 // pointerToLineNumbers
			0,                 // numberOfRelocations
			0,                 // numberOfLineNumbers
			IMAGE_SCN_CNT_INITIALIZED_DATA |
				IMAGE_SCN_ALIGN_8BYTES |
				IMAGE_SCN_MEM_READ // characteristics
		},
		{
			addName(".rdata$z"), // name
			0,                   // virtualSize
			0,                   // virtualAddress
			0,                   // sizeOfRawData
			0,                   // pointerToRawData
			0,                   // pointerToRelocations
			0,                   // pointerToLineNumbers
			0,                   // numberOfRelocations
			0,                   // numberOfLineNumbers
			IMAGE_SCN_CNT_INITIALIZED_DATA |
				IMAGE_SCN_ALIGN_1BYTES |
				IMAGE_SCN_MEM_READ // characteristics
		}
	};

	std::vector<COFF_Symbol> symbols{};

	auto rawDataOffset = WriteDataAligned(f, sectionHeaders, sizeof(sectionHeaders));
	auto offset = rawDataOffset;

	for (const auto &[name, data] : mGlobals)
	{
		symbols.emplace_back(
			COFF_Symbol{
				addName(name),
				offset - rawDataOffset,
				1,
				0,
				IMAGE_SYM_CLASS_EXTERNAL });

		offset = WriteDataAligned(f, data.data(), data.size());
	}

	auto dataSize = offset - rawDataOffset;
	auto padding = 8 - dataSize % 8;
	if (padding == 8)
		padding = 0;
	auto rawDataSize = dataSize + padding;
	auto rawData2Offset = WriteDataAligned(f, std::string('\0', 8).data(), padding);

	using namespace std::literals;
	std::string package_string = kProjectName + " "s + kVersionNumber;

	offset = WriteDataAligned(f, package_string.data(), package_string.length());

	auto rawData2Size = offset - rawData2Offset;
	auto symbolTableOffset = offset;

	// We can now fill in the blanks
	sectionHeaders[0].pointerToRawData = rawDataOffset;
	sectionHeaders[0].sizeOfRawData = rawDataSize;

	sectionHeaders[1].pointerToRawData = rawData2Offset;
	sectionHeaders[1].sizeOfRawData = rawData2Size;

	// create the rest of the symbols
	symbols.emplace_back(
		COFF_Symbol{
			addName(".rdata"),
			0,
			1,
			0,
			IMAGE_SYM_CLASS_STATIC,
			1 });
	COFF_Name n1{};
	n1._filler_ = dataSize;
	symbols.emplace_back(COFF_Symbol{ n1 });

	symbols.emplace_back(
		COFF_Symbol{
			addName(".rdata$z"),
			0,
			2,
			0,
			IMAGE_SYM_CLASS_STATIC,
			1 });
	COFF_Name n2{};
	n2._filler_ = dataSize;
	symbols.emplace_back(COFF_Symbol{ n2 });

	for (auto &sym : symbols)
		WriteDataAligned(f, &sym, 18);

	uint32_t strTabSize = strtab.size() + 4;
	WriteDataAligned(f, &strTabSize, sizeof(strTabSize));
	WriteDataAligned(f, strtab.data(), strtab.size() + 1);

	// write section headers
	f.seekp(sectionHeaderStart);
	WriteDataAligned(f, sectionHeaders, sizeof(sectionHeaders));

	// Write header
	f.seekp(0);
	header.numberOfSections = sizeof(sectionHeaders) / sizeof(COFF_SectionHeader);
	header.pointerToSymbolTable = symbolTableOffset;
	header.numberOfSymbols = symbols.size();
	WriteDataAligned(f, &header, sizeof(header));
}

// --------------------------------------------------------------------

#if __has_include(<elf.h>)
MObjectFileImp *MObjectFileImp::Create(int machine, int elf_class, int elf_data, int elf_abi, int flags)
{
	MObjectFileImp *result = nullptr;

	if (elf_class == ELFCLASS32 and elf_data == ELFDATA2LSB)
		result = new MELFObjectFileImp<ELFCLASS32, ELFDATA2LSB>(machine, elf_abi, flags);
	else if (elf_class == ELFCLASS32 and elf_data == ELFDATA2MSB)
		result = new MELFObjectFileImp<ELFCLASS32, ELFDATA2MSB>(machine, elf_abi, flags);
	else if (elf_class == ELFCLASS64 and elf_data == ELFDATA2LSB)
		result = new MELFObjectFileImp<ELFCLASS64, ELFDATA2LSB>(machine, elf_abi, flags);
	else if (elf_class == ELFCLASS64 and elf_data == ELFDATA2MSB)
		result = new MELFObjectFileImp<ELFCLASS64, ELFDATA2MSB>(machine, elf_abi, flags);
	else
	{
		std::cerr << "Unsupported ELF class and/or data " << elf_class << ", " << elf_data << std::endl;
		exit(1);
	}

	return result;
}
#endif

// --------------------------------------------------------------------

class MObjectFile
{
  public:
#if __has_include(<elf.h>)
	MObjectFile(int machine, int elf_class, int elf_data, int elf_abi, int flags)
		: mImpl(MObjectFileImp::Create(machine, elf_class, elf_data, elf_abi, flags))
	{
	}
#endif

	MObjectFile(COFF_Machine machine)
		: mImpl(new MCOFFObjectFileImp(machine))
	{
	}

	~MObjectFile();

	void AddGlobal(const std::string &inName, const void *inData, uint32_t inSize);
	void Write(std::ofstream &inFile);

  private:
	MObjectFileImp *mImpl;
};

MObjectFile::~MObjectFile()
{
	delete mImpl;
}

void MObjectFile::AddGlobal(const std::string &inName, const void *inData, uint32_t inSize)
{
	MObjectFileImp::MGlobal g;
	g.name = inName;
	g.data.assign(reinterpret_cast<const char *>(inData), inSize);

	mImpl->mGlobals.push_back(g);
}

void MObjectFile::Write(std::ofstream &inFile)
{
	if (mImpl == nullptr)
		throw std::runtime_error("nullptr error");
	mImpl->Write(inFile);
}

// --------------------------------------------------------------------

class MResourceFile
{
  public:
	MResourceFile(const std::string &prefix)
		: mPrefix(prefix)
	{
		mIndex.push_back({});
		mName.push_back(0);
	}

	void Write(MObjectFile &objFile);
	void Add(const fs::path &inPath, const fs::path &inFile);

  private:
	void AddEntry(fs::path inPath, const char *inData, uint32_t inSize);

	std::vector<mrsrc::rsrc_imp> mIndex;
	std::vector<char> mData, mName;
	std::string mPrefix;
};

void MResourceFile::AddEntry(fs::path inPath, const char *inData, uint32_t inSize)
{
	uint32_t node = 0; // start at root

	for (fs::path::iterator p = inPath.begin(); p != inPath.end(); ++p)
	{
		if (*p == ".") // flatten
			continue;

		// no such child? Add it and continue
		if (mIndex[node].m_child == 0)
		{
			mrsrc::rsrc_imp child = {};

			child.m_name = mName.size();

			std::string n = p->string();
			copy(n.begin(), n.end(), std::back_inserter(mName));
			mName.push_back(0);

			mIndex[node].m_child = mIndex.size();
			mIndex.push_back(child);

			node = mIndex[node].m_child;
			continue;
		}

		// lookup the path element in the current directory
		uint32_t next = mIndex[node].m_child;
		for (;;)
		{
			const char *name = mName.data() + mIndex[next].m_name;

			// if this is the one we're looking for, break out of the loop
			if (*p == name)
			{
				node = next;
				break;
			}

			// if there is a next element, loop
			if (mIndex[next].m_next != 0)
			{
				next = mIndex[next].m_next;
				continue;
			}

			// not found, create it
			mrsrc::rsrc_imp n = {};

			n.m_name = mName.size();

			std::string s = p->string();
			copy(s.begin(), s.end(), back_inserter(mName));
			mName.push_back(0);

			node = mIndex.size();
			mIndex[next].m_next = node;
			mIndex.push_back(n);

			break;
		}
	}

	assert(node != 0);
	assert(node < mIndex.size());

	mIndex[node].m_size = inSize;
	mIndex[node].m_data = mData.size();

	copy(inData, inData + inSize, back_inserter(mData));
	while ((mData.size() % 8) != 0)
		mData.push_back('\0');
}

void MResourceFile::Add(const fs::path &inPath, const fs::path &inFile)
{
	if (fs::is_directory(inFile))
	{
		fs::path ns = inPath / inFile.filename();

		fs::path cwd = fs::current_path();
		fs::current_path(inFile);

		for (auto i = fs::directory_iterator(fs::current_path()); i != fs::directory_iterator(); ++i)
			Add(ns, i->path().filename());

		fs::current_path(cwd);
	}
	else
	{
		if (VERBOSE)
			std::cerr << "adding " << inFile << " as " << inPath / inFile.filename() << std::endl;

		std::ifstream f(inFile, std::ios::binary);

		if (not f.is_open())
			throw std::runtime_error("Could not open data file \'" + inFile.string() + '\'');

		std::filebuf *b = f.rdbuf();

		uint32_t size = b->pubseekoff(0, std::ios::end, std::ios::in);
		b->pubseekoff(0, std::ios::beg, std::ios::in);

		std::vector<char> text(size);

		b->sgetn(text.data(), size);
		f.close();

		AddEntry(inPath / inFile.filename(), text.data(), size);
	}
}

void MResourceFile::Write(MObjectFile &obj)
{
	obj.AddGlobal(mPrefix + "Index", mIndex.data(), mIndex.size() * sizeof(mrsrc::rsrc_imp));
	obj.AddGlobal(mPrefix + "Data", mData.data(), mData.size());
	obj.AddGlobal(mPrefix + "Name", mName.data(), mName.size());
}

// --------------------------------------------------------------------

int main(int argc, char *argv[])
{
	auto &config = mcfp::config::instance();

	config.init(
		"usage: mrc [options] -o output file1 [file2...]",

		mcfp::make_option("help,h", "Display help message"),
		mcfp::make_option("version", "Print version"),
		mcfp::make_option<std::string>("output,o", "Output file, this file is in the default object file format for this OS."),
		mcfp::make_option("header", "This will print out the header file you need to include in your program to access your resources"),
		mcfp::make_option<std::string>("root", "Root path for the stored data (in the final resource data structure"),
		mcfp::make_option<std::string>("resource-prefix", "gResource", "Prefix for the name of the global variables, default is gResource"),

#if __has_include(<elf.h>)
		mcfp::make_option<int>("elf-machine", "The ELF machine type to use, default is same as this machine. Use one of the values from elf.h"),
		mcfp::make_option<int>("elf-class", "ELF class, default is same as this machine. Acceptable values are 1 (32bit) and 2 (64bit)."),
		mcfp::make_option<int>("elf-data", "ELF endianness, default is same as this machine. Acceptable values are 1 (little-endian, LSB) and 2 (big-endian, MSB)."),
		mcfp::make_option<int>("elf-abi", "ELF OS ABI value, see file elf.h for values (linux = 3, freebsd = 9)"),
		mcfp::make_option<int>("elf-flags", "Processor specific flags in the ELF header, e.g. the EABI version for ARM"),
#endif
		mcfp::make_option<std::string>("coff", "Write a PE/COFF file for Windows, values should be one of x64, x86 or arm64"),

		mcfp::make_option("verbose,v", "Verbose output")
	);

	std::error_code ec;
	config.parse(argc, argv, ec);
	if (ec)
	{
		std::cerr << ec.message() << std::endl;
		exit(1);
	}

	if (config.has("version"))
	{
		write_version_string(std::cout, config.has("verbose"));
		exit(0);
	}

	if (config.has("help") or config.operands().empty() or not config.has("output"))
	{
		std::cout << config << std::endl;
		exit(config.has("help") ? 0 : 1);
	}

	if (config.has("header"))
	{
		mrsrc::rsrc data("mrsrc.h");

		std::string text(data.data(), data.size());

		if (config.has("output"))
		{
			std::ofstream file(config.get<std::string>("output"), std::ios::binary);
			if (not file.is_open())
				throw std::runtime_error("Could not open output file for writing");
			file << text << std::endl;
		}
		else
			std::cout << text << std::endl;

		exit(0);
	}

	VERBOSE = config.count("verbose");

	// --------------------------------------------------------------------
	// find out the native format. Simply look at how we were assembled ourselves

	int elf_machine = EM_NONE, elf_class = 0, elf_data = 0, elf_flags = 0, elf_abi = 0;

#if not defined(_MSC_VER)
	char exePath[PATH_MAX + 1];
#if __linux or __linux__
	elf_abi = ELFOSABI_LINUX;
	int r = readlink("/proc/self/exe", exePath, PATH_MAX);
#elif __FreeBSD__
	elf_abi = ELFOSABI_FREEBSD;
	int r = strlen(argv[0]);
	strcpy(exePath, argv[0]);
#endif

	if (r > 0)
	{
		exePath[r] = 0;

		int fd = open(exePath, O_RDONLY);
		if (fd >= 0)
		{
			unsigned char e_ident[16];

			if (read(fd, e_ident, sizeof(e_ident)) == sizeof(e_ident) and
				e_ident[EI_MAG0] == ELFMAG0 and e_ident[EI_MAG1] == ELFMAG1 and e_ident[EI_MAG2] == ELFMAG2 and e_ident[EI_MAG3] == ELFMAG3)
			{
				// Yes, we're an ELF!

				elf_class = e_ident[EI_CLASS];
				elf_data = e_ident[EI_DATA];
				if (e_ident[EI_ABIVERSION])
					elf_abi = e_ident[EI_ABIVERSION];

				lseek(fd, 0, SEEK_SET);

				switch (elf_class)
				{
					case ELFCLASS32:
					{
						Elf32_Ehdr hdr;
						if (read(fd, &hdr, sizeof(hdr)) == sizeof(Elf32_Ehdr))
						{
							elf_machine = hdr.e_machine;
							elf_flags = hdr.e_flags;
						}
						break;
					}

					case ELFCLASS64:
					{
						Elf64_Ehdr hdr;
						if (read(fd, &hdr, sizeof(hdr)) == sizeof(Elf64_Ehdr))
						{
							elf_machine = hdr.e_machine;
							elf_flags = hdr.e_flags;
						}
						break;
					}

					default:
						std::cerr << "Unknown ELF class" << std::endl;
				}
			}
		}
	}
#endif

	std::string ns;
	if (config.has("root"))
		ns = config.get<std::string>("root");

	std::string prefix = config.get<std::string>("resource-prefix");

	try
	{
		MResourceFile rsrcFile(prefix);

		for (fs::path i : config.operands())
			rsrcFile.Add(ns, i);

		std::ofstream file(config.get<std::string>("output"), std::ios::binary);
		if (not file.is_open())
			throw std::runtime_error("Could not open output file for writing");

		COFF_Machine win_machine = {};
#if defined(_MSC_VER)
#if defined(_M_AMD64)
		win_machine = IMAGE_FILE_MACHINE_AMD64;
#elif defined(_M_ARM64)
		win_machine = IMAGE_FILE_MACHINE_ARM64;
#elif defined(_M_IX86)
		win_machine = IMAGE_FILE_MACHINE_I386;
#endif
#endif

		if (config.has("coff"))
		{
			if (config.get<std::string>("coff") == "x64")
				win_machine = IMAGE_FILE_MACHINE_AMD64;
			else if (config.get<std::string>("coff") == "arm64")
				win_machine = IMAGE_FILE_MACHINE_ARM64;
			else if (config.get<std::string>("coff") == "x86")
				win_machine = IMAGE_FILE_MACHINE_I386;
			else
				throw std::runtime_error("Unsupported machine for COFF: " + config.get<std::string>("coff"));
		}

		bool target_elf = config.has("elf-machine") and config.has("elf-class") and config.has("elf-data") and config.has("elf-flags");
		if (config.has("elf-machine"))
			elf_machine = config.get<int>("elf-machine");

		if (config.has("elf-class"))
			elf_class = config.get<int>("elf-class");

		if (config.has("elf-data"))
			elf_data = config.get<int>("elf-data");

		if (config.has("elf-abi"))
			elf_abi = config.get<int>("elf-abi");

		if (config.has("elf-flags"))
			elf_flags = config.get<int>("elf-flags");

		if (win_machine and not target_elf)
		{
			MObjectFile obj(win_machine);
			rsrcFile.Write(obj);
			obj.Write(file);
		}
		else
#if __has_include(<elf.h>)
		{
			MObjectFile obj(elf_machine, elf_class, elf_data, elf_abi, elf_flags);
			rsrcFile.Write(obj);
			obj.Write(file);
		}
#else
			throw std::runtime_error("Could not create resource file, probably you're trying to create a ELF resource file on Windows?");
#endif
	}
	catch (const std::exception &ex)
	{
		std::cerr << "Error executing mrc: " << ex.what() << std::endl;
		exit(1);
	}

	return 0;
}
