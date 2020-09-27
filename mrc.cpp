/*-
 * SPDX-License-Identifier: BSD-2-Clause
 * 
 * Copyright (c) 2017-2020 Maarten L. Hekkelman
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

#include "mrc.h"

#include <iostream>
#include <filesystem>
#include <fstream>

#include <elf.h>

#include <boost/program_options.hpp>

#include "mrsrc.h"

namespace po = boost::program_options;
namespace fs = std::filesystem;

int VERBOSE = 0;

// --------------------------------------------------------------------

enum MTargetCPU
{
	eCPU_Unknown,
	eCPU_native,
	eCPU_386,
	eCPU_x86_64,
	eCPU_PowerPC_32,
	eCPU_PowerPC_64,
	
	// nieuw!
	eCPU_Arm
};

// --------------------------------------------------------------------

uint32_t AddNameToNameTable(std::string& ioNameTable, const std::string& inName)
{
	uint32_t result = 0;
	
	const char* p = ioNameTable.c_str();
	const char* e = p + ioNameTable.length();
	
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
	fs::path
					mFile;
	uint32_t		mTextSize;
	uint32_t		mDataSize;
					
	virtual			~MObjectFileImp() {}
	
	virtual void	Write(const fs::path& inFile) = 0;

	static MObjectFileImp* Create(MTargetCPU inTarget, int EABI);

  protected:

	friend class MObjectFile;

	struct MGlobal
	{
		std::string	name;
		std::string	data;
	};

	typedef std::vector<MGlobal>	MGlobals;
	
	MGlobals		mGlobals;
};

class MObjectFile
{
  public:
					MObjectFile(MTargetCPU inTarget, int EABI);
					~MObjectFile();

	void			AddGlobal(const std::string& inName, const void* inData, uint32_t inSize);
	void			Write(const fs::path& inFile);

  private:
	MObjectFileImp*	mImpl;
};

MObjectFile::MObjectFile(MTargetCPU inTarget, int EABI)
	: mImpl(MObjectFileImp::Create(inTarget, EABI))
{
}

MObjectFile::~MObjectFile()
{
	delete mImpl;
}

void MObjectFile::AddGlobal(const std::string& inName, const void* inData, uint32_t inSize)
{
	MObjectFileImp::MGlobal g;
	g.name = inName;
	g.data.assign(reinterpret_cast<const char*>(inData), inSize);
	
	mImpl->mGlobals.push_back(g);
}

void MObjectFile::Write(const fs::path& inFile)
{
	if (mImpl == nullptr)
		throw std::runtime_error("nullptr error " + inFile.string());
	mImpl->Write(inFile);
}

// --------------------------------------------------------------------
//
//	Implementation for ELF

namespace Swap
{
struct no_swapper
{
	template<typename T>
	T			operator()(T inValue) const			{ return inValue; }
};

struct swapper
{
	template<typename T>
	T			operator()(T inValue) const
	{
		this_will_not_compile_I_hope(inValue);
	}
};

template<>
inline
bool swapper::operator()(bool inValue) const
{
	return inValue;
}

template<>
inline
int8_t swapper::operator()(int8_t inValue) const
{
	return inValue;
}

template<>
inline
uint8_t swapper::operator()(uint8_t inValue) const
{
	return inValue;
}

template<>
inline
int16_t swapper::operator()(int16_t inValue) const
{
	return static_cast<int16_t>(
		((inValue & 0xFF00UL) >>  8) |
		((inValue & 0x00FFUL) <<  8)
	);
}

template<>
inline
uint16_t swapper::operator()(uint16_t inValue) const
{
	return static_cast<uint16_t>(
		((inValue & 0xFF00UL) >>  8) |
		((inValue & 0x00FFUL) <<  8)
	);
}

template<>
inline
int32_t swapper::operator()(int32_t inValue) const
{
	return static_cast<int32_t>(
		((inValue & 0xFF000000UL) >> 24) |
		((inValue & 0x00FF0000UL) >>  8) |
		((inValue & 0x0000FF00UL) <<  8) |
		((inValue & 0x000000FFUL) << 24)
	);
}

template<>
inline
uint32_t swapper::operator()(uint32_t inValue) const
{
	return static_cast<uint32_t>(
		((inValue & 0xFF000000UL) >> 24) |
		((inValue & 0x00FF0000UL) >>  8) |
		((inValue & 0x0000FF00UL) <<  8) |
		((inValue & 0x000000FFUL) << 24)
	);
}

//template<>
//inline
//long unsigned int swapper::operator()(long unsigned int inValue) const
//{
//	return static_cast<long unsigned int>(
//		((inValue & 0xFF000000UL) >> 24) |
//		((inValue & 0x00FF0000UL) >>  8) |
//		((inValue & 0x0000FF00UL) <<  8) |
//		((inValue & 0x000000FFUL) << 24)
//	);
//}
//
template<>
inline
int64_t swapper::operator()(int64_t inValue) const
{
	return static_cast<int64_t>(
		(((static_cast<uint64_t>(inValue))<<56) & 0xFF00000000000000ULL)  |
		(((static_cast<uint64_t>(inValue))<<40) & 0x00FF000000000000ULL)  |
		(((static_cast<uint64_t>(inValue))<<24) & 0x0000FF0000000000ULL)  |
		(((static_cast<uint64_t>(inValue))<< 8) & 0x000000FF00000000ULL)  |
		(((static_cast<uint64_t>(inValue))>> 8) & 0x00000000FF000000ULL)  |
		(((static_cast<uint64_t>(inValue))>>24) & 0x0000000000FF0000ULL)  |
		(((static_cast<uint64_t>(inValue))>>40) & 0x000000000000FF00ULL)  |
		(((static_cast<uint64_t>(inValue))>>56) & 0x00000000000000FFULL));
}

template<>
inline
uint64_t swapper::operator()(uint64_t inValue) const
{
	return static_cast<uint64_t>(
		((((uint64_t)inValue)<<56) & 0xFF00000000000000ULL)  |
		((((uint64_t)inValue)<<40) & 0x00FF000000000000ULL)  |
		((((uint64_t)inValue)<<24) & 0x0000FF0000000000ULL)  |
		((((uint64_t)inValue)<< 8) & 0x000000FF00000000ULL)  |
		((((uint64_t)inValue)>> 8) & 0x00000000FF000000ULL)  |
		((((uint64_t)inValue)>>24) & 0x0000000000FF0000ULL)  |
		((((uint64_t)inValue)>>40) & 0x000000000000FF00ULL)  |
		((((uint64_t)inValue)>>56) & 0x00000000000000FFULL));
}

#if defined(LITTLE_ENDIAN)
typedef no_swapper	lsb_swapper;
typedef swapper		msb_swapper;

typedef swapper		net_swapper;
#elif defined(BIG_ENDIAN)
typedef swapper		lsb_swapper;
typedef no_swapper	msb_swapper;

typedef no_swapper	net_swapper;
#else
#error Undefined endianness
#endif
	
}

template<MTargetCPU CPU, int FLAGS>
struct MCPUTraits
{
};

template<>
struct MCPUTraits<eCPU_PowerPC_32,0>
{
	typedef Elf32_Ehdr	Elf_Ehdr;
	typedef Elf32_Shdr	Elf_Shdr;
	typedef Elf32_Sym	Elf_Sym;
	
	typedef Swap::msb_swapper	swapper;
	
	enum {
		ElfClass	= ELFCLASS32,
		ElfData		= ELFDATA2MSB,
		ElfMachine	= EM_PPC,
		ElfFlags	= 0
	};
};

template<>
struct MCPUTraits<eCPU_PowerPC_64,0>
{
	typedef Elf64_Ehdr	Elf_Ehdr;
	typedef Elf64_Shdr	Elf_Shdr;
	typedef Elf64_Sym	Elf_Sym;
	
	typedef Swap::msb_swapper	swapper;
	
	enum {
		ElfClass	= ELFCLASS64,
		ElfData		= ELFDATA2MSB,
		ElfMachine	= EM_PPC64,
		ElfFlags	= 0
	};
};

template<>
struct MCPUTraits<eCPU_x86_64,0>
{
	typedef Elf64_Ehdr	Elf_Ehdr;
	typedef Elf64_Shdr	Elf_Shdr;
	typedef Elf64_Sym	Elf_Sym;

	typedef Swap::lsb_swapper	swapper;
	
	enum {
		ElfClass	= ELFCLASS64,
		ElfData		= ELFDATA2LSB,
		ElfMachine	= EM_X86_64,
		ElfFlags	= 0
	};
};

template<>
struct MCPUTraits<eCPU_386,0>
{
	typedef Elf32_Ehdr	Elf_Ehdr;
	typedef Elf32_Shdr	Elf_Shdr;
	typedef Elf32_Sym	Elf_Sym;
	
	typedef Swap::lsb_swapper	swapper;
	
	enum {
		ElfClass	= ELFCLASS32,
		ElfData		= ELFDATA2LSB,
		ElfMachine	= EM_386,
		ElfFlags	= 0
	};
};

template<int EABI>
struct MCPUTraits<eCPU_Arm, EABI>
{
	typedef Elf32_Ehdr	Elf_Ehdr;
	typedef Elf32_Shdr	Elf_Shdr;
	typedef Elf32_Sym	Elf_Sym;
	
	typedef Swap::lsb_swapper	swapper;
	
	enum {
		ElfClass	= ELFCLASS32,
		ElfData		= ELFDATA2LSB,
		ElfMachine	= EM_ARM,
		ElfFlags	= EABI
	};
};

template<
	MTargetCPU CPU,
	int FLAGS = 0,
	typename traits = MCPUTraits<CPU,FLAGS>
>
struct MELFObjectFileImp : public MObjectFileImp
{
	typedef typename traits::swapper	swapper;
	typedef typename traits::Elf_Ehdr	Elf_Ehdr;
	typedef typename traits::Elf_Shdr	Elf_Shdr;
	typedef typename traits::Elf_Sym	Elf_Sym;

	void Write(const fs::path& inFile);
};

MObjectFileImp* CreateELFObjectFileImp(MTargetCPU inTarget);

#if not (defined(__APPLE__) and defined(__MACH__))

using namespace std;

uint32_t WriteDataAligned(ofstream& inStream, const void* inData, uint32_t inSize, uint32_t inAlignment = 1)
{
	inStream.write(reinterpret_cast<const char*>(inData), inSize);

	if (inAlignment > 1)
	{
		while ((inStream.tellp() % inAlignment) != 0)
			inStream.put('\0');
	}

	return inStream.tellp();
}

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

enum {
	kNullSymbol,
	kTextSectionSymbol,
	kDataSectionSymbol,
	kBssSectionSymbol,
	kGlobalSymbol,
	
	kSymbolCount
};

template
<
	MTargetCPU	CPU,
	int			FLAGS,
	typename	traits
>
void MELFObjectFileImp<CPU, FLAGS, traits>::Write(const fs::path& inFile)
{
	std::ofstream f(inFile, ios::binary | ios::trunc);
	if (not f.is_open())
		throw runtime_error("Failed to open object file " + inFile.string() + " for writing");

	Elf_Ehdr eh = {
							// e_ident
		{ ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3,
			traits::ElfClass, traits::ElfData, EV_CURRENT },
		ET_REL,				// e_type
		traits::ElfMachine,	// e_machine
		EV_CURRENT,			// e_version
		0,					// e_entry
		0,					// e_phoff
		0,					// e_shoff
		traits::ElfFlags,	// e_flags
		sizeof(Elf_Ehdr),	// e_ehsize
		0,					// e_phentsize
		0,					// e_phnum
		sizeof(Elf_Shdr),	// e_shentsize
		kSectionCount,		// e_shnum
		kShStrtabSection	// e_shstrndx
	};

	uint32_t data_offset = WriteDataAligned(f, &eh, sizeof(eh), 16);

	std::string strtab;
	AddNameToNameTable(strtab, "");		// null name
	
	Elf_Sym sym = {};
	vector<Elf_Sym> syms;
	
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

	for (MGlobals::iterator g = mGlobals.begin(); g != mGlobals.end(); ++g)
	{
		sym.st_name = AddNameToNameTable(strtab, g->name);
		sym.st_value = sym_offset - data_offset;
		sym.st_size = g->data.length();
		sym.st_info = ELF32_ST_INFO(STB_GLOBAL, STT_OBJECT);
		sym.st_shndx = kDataSection;
		
		syms.push_back(sym);
		
		sym_offset = WriteDataAligned(f, g->data.c_str(), g->data.length(), 8);
	}
	
	uint32_t data_size = sym_offset;

	uint32_t symtab_off = sym_offset;
	assert((sizeof(Elf_Sym) % 8) == 0);
	
	uint32_t symtab_size = syms.size() * sizeof(sym);
	uint32_t strtab_off = WriteDataAligned(f, &syms[0], symtab_size, 8);
	uint32_t shstrtab_off = WriteDataAligned(f, strtab.c_str(), strtab.length(), 8);

	std::string shstrtab;
	(void)AddNameToNameTable(shstrtab, "");		// null name
	(void)AddNameToNameTable(shstrtab, ".text");
	(void)AddNameToNameTable(shstrtab, ".rsrc_data");
	(void)AddNameToNameTable(shstrtab, ".bss");
	(void)AddNameToNameTable(shstrtab, ".shstrtab");
	(void)AddNameToNameTable(shstrtab, ".symtab");
	(void)AddNameToNameTable(shstrtab, ".strtab");
	
	eh.e_shoff = WriteDataAligned(f, shstrtab.c_str(), shstrtab.length(), 16);

	Elf_Shdr sh[kSectionCount] = {
		{			// kNullSection
		},
		{			// kTextSection
							// sh_name
			AddNameToNameTable(shstrtab, ".text"),
			SHT_PROGBITS,	// sh_type
							// sh_flags
			SHF_ALLOC|SHF_EXECINSTR,
			0,				// sh_addr
			data_offset,	// sh_offset
			0,				// sh_size
			0,				// sh_link
			0,				// sh_info
			4,				// sh_addralign
			0				// sh_entsize
		},
		{			// kDataSection
							// sh_name
			AddNameToNameTable(shstrtab, ".rsrc_data"),
			SHT_PROGBITS,	// sh_type
							// sh_flags
			SHF_ALLOC|SHF_WRITE,
			0,				// sh_addr
			data_offset,	// sh_offset
			data_size,		// sh_size
			0,				// sh_link
			0,				// sh_info
			4,				// sh_addralign
			0				// sh_entsize
		},
		{			// kBssSection
							// sh_name
			AddNameToNameTable(shstrtab, ".bss"),
			SHT_NOBITS,		// sh_type
							// sh_flags
			SHF_ALLOC|SHF_WRITE,
			0,				// sh_addr
			eh.e_shoff,		// sh_offset
			0,				// sh_size
			0,				// sh_link
			0,				// sh_info
			4,				// sh_addralign
			0				// sh_entsize
		},
		{			// kShStrtabSection
							// sh_name
			AddNameToNameTable(shstrtab, ".shstrtab"),
			SHT_STRTAB,		// sh_type
			0,				// sh_flags
			0,				// sh_addr
			shstrtab_off,	// sh_offset
			Elf32_Word(shstrtab.length()),// sh_size
			0,				// sh_link
			0,				// sh_info
			1,				// sh_addralign
			0				// sh_entsize
		},
		{			// kSymtabSection
							// sh_name
			AddNameToNameTable(shstrtab, ".symtab"),
			SHT_SYMTAB,		// sh_type
							// sh_flags
			0,
			0,				// sh_addr
			symtab_off,		// sh_offset
			symtab_size,	// sh_size
			6,				// sh_link
			kGlobalSymbol,	// sh_info
			8,				// sh_addralign
			sizeof(Elf_Sym)	// sh_entsize
		},
		{			// kStrtabSection
							// sh_name
			AddNameToNameTable(shstrtab, ".strtab"),
			SHT_STRTAB,		// sh_type
							// sh_flags
			0,
			0,				// sh_addr
			strtab_off,		// sh_offset
			Elf32_Word(strtab.length()),// sh_size
			0,				// sh_link
			0,				// sh_info
			1,				// sh_addralign
			0				// sh_entsize
		},
	};
	
	WriteDataAligned(f, sh, sizeof(sh), 1);
	
	f.flush();
	
	f.seekp(0);
	WriteDataAligned(f, &eh, sizeof(eh));
}

MObjectFileImp* MObjectFileImp::Create(MTargetCPU inTarget, int EABI)
{
	MObjectFileImp* result = nullptr;

	if (inTarget == eCPU_native)
	{
#if defined(__x86_64)
		inTarget = eCPU_x86_64;
#elif defined(__i686) or defined(__i386)
		inTarget = eCPU_386;
#elif defined(__arm__)
		inTarget = eCPU_Arm;
#else
		throw runtime_error("cannot determine native CPU format");
#endif
	}

	switch (inTarget)
	{
		case eCPU_386:
			result = new MELFObjectFileImp<eCPU_386>();
			break;
		
		case eCPU_x86_64:
			result = new MELFObjectFileImp<eCPU_x86_64>();
			break;
		
		case eCPU_PowerPC_32:
			result = new MELFObjectFileImp<eCPU_PowerPC_32>();
			break;
		
		case eCPU_PowerPC_64:
			result = new MELFObjectFileImp<eCPU_PowerPC_64>();
			break;
		
		case eCPU_Arm:
			if (EABI < 1 or EABI > 5)
				throw runtime_error("Need to specify valid EABI for ARM");
				
			switch (EABI)
			{
				case 1:	
					result = new MELFObjectFileImp<eCPU_Arm,EF_ARM_EABI_VER1>();
					break;
				case 2:	
					result = new MELFObjectFileImp<eCPU_Arm,EF_ARM_EABI_VER2>();
					break;
				case 3:	
					result = new MELFObjectFileImp<eCPU_Arm,EF_ARM_EABI_VER3>();
					break;
				case 4:	
					result = new MELFObjectFileImp<eCPU_Arm,EF_ARM_EABI_VER4>();
					break;
				case 5:	
					result = new MELFObjectFileImp<eCPU_Arm,EF_ARM_EABI_VER5>();
					break;
			}
			break;

		default:
			throw runtime_error("Unsupported target");
	}
	
	return result;
}

#endif

// --------------------------------------------------------------------

class MResourceFile
{
  public:
	MResourceFile(MTargetCPU inTarget, int eabi)
		: mTarget(inTarget), mEABI(eabi)
	{
		mIndex.push_back({});
		mName.push_back(0);
	}

	void Write(const fs::path& inFile);
	void Add(const fs::path& inPath, const fs::path& inFile);

  private:

	void AddEntry(fs::path inPath, const char* inData, uint32_t inSize);

	MTargetCPU				mTarget;
	int						mEABI;
	vector<mrsrc::rsrc_imp>	mIndex;
	vector<char>			mData, mName;
};

void MResourceFile::AddEntry(fs::path inPath, const char* inData, uint32_t inSize)
{
	uint32_t node = 0;	// start at root
	
	for (fs::path::iterator p = inPath.begin(); p != inPath.end(); ++p)
	{
		if (*p == ".")	// flatten
			continue;
		
		// no such child? Add it and continue
		if (mIndex[node].m_child == 0)
		{
			mrsrc::rsrc_imp child = {};
			
			child.m_name = mName.size();
			
			std::string n = p->string();
			copy(n.begin(), n.end(), back_inserter(mName));
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
			const char* name = &mName[0] + mIndex[next].m_name;
			
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

void MResourceFile::Add(const fs::path& inPath, const fs::path& inFile)
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
			cerr  << "adding " << inFile << " as " << inPath / inFile.filename() << endl;
				
		std::ifstream f(inFile);
	
		if (not f.is_open())
			throw runtime_error("Could not open data file");
	
		filebuf* b = f.rdbuf();
		
		uint32_t size = b->pubseekoff(0, ios::end, ios::in);
		b->pubseekoff(0, ios::beg, ios::in);
		
		vector<char> text(size);
		
		b->sgetn(&text[0], size);
		f.close();
		
		AddEntry(inPath / inFile.filename(), &text[0], size);
	}
}

void MResourceFile::Write(const fs::path& inFile)
{
	MObjectFile obj(mTarget, mEABI);

	obj.AddGlobal("gResourceIndex", &mIndex[0], mIndex.size() * sizeof(mrsrc::rsrc_imp));
	obj.AddGlobal("gResourceData", &mData[0], mData.size());
	obj.AddGlobal("gResourceName", &mName[0], mName.size());
	
	obj.Write(inFile);
}

int main(int argc, char* argv[])
{
	try
	{
		po::options_description visible_options("mrc " PACKAGE_VERSION " options file1 [file2...]" );
		visible_options.add_options()
			("help,h",								"Display help message")
			("version",								"Print version")
			("output,o",	po::value<std::string>(),	"Output file, this file is in the default object file format for this OS.")
			("cpu",			po::value<std::string>(),	"CPU type, default is native CPU, available values are: i386, x86_64, powerpc32, powerpc64 and arm")
			("header",								"This will print out the header file you need to include in your program to access your resources")
			("root",		po::value<std::string>(),	"Root path for the stored data (in the final resource data structure")
			("arm-eabi",	po::value<int>(),		"EABI version for ARM")
			("verbose,v",							"Verbose output");
	
		po::options_description hidden_options("hidden options");
		hidden_options.add_options()
			("input,i",		po::value<vector<std::string>>(),	"Input files");
	
		po::options_description cmdline_options;
		cmdline_options.add(visible_options).add(hidden_options);
	
		po::positional_options_description p;
		p.add("input", -1);
		
		po::variables_map vm;
		po::store(po::command_line_parser(argc, argv).options(cmdline_options).positional(p).run(), vm);
		po::notify(vm);
		
		if (vm.count("version"))
		{
			cout << PACKAGE_NAME << " version " PACKAGE_VERSION << endl;
			exit(0);
		}
	
		if (vm.count("header"))
		{
			mrsrc::rsrc data("mrsrc.h");
			
			std::string text(data.data(), data.size());
			
			cout << text << endl;
			exit(0);
		}
		
		if (vm.count("help") or vm.count("input") == 0)
		{
			cerr << visible_options << endl
				 << "See man mrc for more help" << endl;
			exit(vm.count("help") == 0);
		}
		
		if (vm.count("verbose"))
			VERBOSE = 1;
		
		std::string ns;
		if (vm.count("root"))
			ns = vm["root"].as<std::string>();
		
		MTargetCPU target = eCPU_native;
		if (vm.count("cpu"))
		{
			std::string cpu = vm["cpu"].as<std::string>();
			if (cpu == "i386")				target = eCPU_386;
			else if (cpu == "x86_64")		target = eCPU_x86_64;
			else if (cpu == "powerpc32")	target = eCPU_PowerPC_32;
			else if (cpu == "powerpc64")	target = eCPU_PowerPC_64;
			else if (cpu == "arm")			target = eCPU_Arm;
			else							throw runtime_error("Unsupported CPU type: " + cpu);
		}
		
		int eabi = 5;
		if (vm.count("eabi"))
			eabi = vm["eabi"].as<int>();
	
		MResourceFile rsrcFile(target, eabi);
		for (fs::path i: vm["input"].as<vector<std::string>>())
			rsrcFile.Add(ns, i);
		
		rsrcFile.Write(vm["output"].as<std::string>());
	}
	catch (const exception& ex)
	{
		cerr << "Error executing mrc: " << ex.what() << endl;
		exit(1);
	}
	
	return 0;
}
