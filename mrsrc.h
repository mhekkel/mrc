/*-
 * SPDX-License-Identifier: BSD-2-Clause
 * 
 * Copyright (c) 2006-2020 Maarten L. Hekkelman
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

#pragma once

#include <string>
#include <list>
#include <exception>
#include <istream>
#include <cassert>
#include <filesystem>

/*
	Resources are data sources for the application.
	
	They are retrieved by name.

	Basic usage:
	
	mrsrc::rsrc rsrc("dialogs/my-cool-dialog.glade");
	
	if (rsrc)
	{
		GladeXML* glade = glade_xml_new_from_buffer(rsrc.data(), rsrc.size(), NULL, "japi");
		
		...
	}

	Alternative, to loop over all resources:

	mrsrc rsrc;		// <- the root resource
	for (rsrc child: rsrc)
		std::cout << child.name() << std::endl;

	-------------------------------------------------

	Stream interface:

	mrsrc::streambuf rb("data/my-text.txt");
	std::istream is(&rb);

	std::string line;
	while (std::gettline(is, line))
		std::cout << line << std::endl;

*/

namespace mrsrc
{
	/// \brief Internal data structure as generated by mrc
	struct rsrc_imp
	{
		unsigned int m_next;
		unsigned int m_child;
		unsigned int m_name;
		unsigned int m_size;
		unsigned int m_data;
	};
} // namespace mrsrc

// The following three variables are generated by mrc:

extern const mrsrc::rsrc_imp gResourceIndex[];
extern const char gResourceData[];
extern const char gResourceName[];

namespace mrsrc
{
	/// \brief Class mrsrc::rsrc contains a pointer to the data in the
	/// resource, as well as offering an iterator interface to its
	/// children.

	class rsrc
	{
	  public:

		rsrc() : m_impl(gResourceIndex) {}

		rsrc(const rsrc& other)
			: m_impl(other.m_impl) {}

		rsrc& operator=(const rsrc& other)
		{
			m_impl = other.m_impl;
			return *this;
		}

		rsrc(std::filesystem::path path);

		std::string name() const { return gResourceName + m_impl->m_name; }

		const char* data() const { return gResourceData + m_impl->m_data; }

		unsigned long size() const { return m_impl->m_size; }

		explicit operator bool() const { return m_impl != NULL and m_impl->m_size > 0; }

		template<typename RSRC>
		class iterator_t
		{
		  public:

			using iterator_category = std::input_iterator_tag;
			using value_type = RSRC;
			using difference_type = std::ptrdiff_t;
			using pointer = value_type*;
			using reference = value_type&;

			iterator_t(const rsrc_imp* cur)
				: m_cur(cur) {}

			iterator_t(const iterator_t& i)
				: m_cur(i.m_cur)
			{
			}
	
			iterator_t& operator=(const iterator_t& i)
			{
				m_cur = i.m_cur;
				return *this;
			}

			reference operator*()		{ return m_cur; }
			pointer operator->()		{ return& m_cur; }

			iterator_t& operator++()
			{
				if (m_cur.m_impl->m_next)
					m_cur.m_impl = gResourceIndex + m_cur.m_impl->m_next;
				else
					m_cur.m_impl = nullptr;
				return *this;
			}

			iterator_t operator++(int)
			{
				auto tmp(*this);
				this->operator++();
				return tmp;
			}

			bool operator==(const iterator_t& rhs) const		{ return m_cur.m_impl == rhs.m_cur.m_impl; }
			bool operator!=(const iterator_t& rhs) const		{ return m_cur.m_impl != rhs.m_cur.m_impl; }

		  private:
			value_type	m_cur;
		};

		using iterator = iterator_t<rsrc>;

		iterator begin() const
		{
			const rsrc_imp* impl = nullptr;
			if (m_impl and m_impl->m_child)
				impl = gResourceIndex + m_impl->m_child;
			return iterator(impl);
		}

		iterator end() const
		{
			return iterator(nullptr);
		}

	  private:
		rsrc(const rsrc_imp* imp)
			: m_impl(imp) {}

		const rsrc_imp *m_impl;
	};

	inline rsrc::rsrc(std::filesystem::path p)
	{
		m_impl = gResourceIndex;

		// using std::filesytem::path would have been natural here of course...
		
		auto pb = p.begin();
		auto pe = p.end();

		while (m_impl != nullptr and pb != pe)
		{
			auto name = *pb++;

			const rsrc_imp* impl = nullptr;
			for (rsrc child: *this)
			{
				if (child.name() == name)
				{
					impl = child.m_impl;
					break;
				}
			}

			m_impl = impl;
		}

		if (pb != pe)	// not found
			m_impl = nullptr;
	}

	// --------------------------------------------------------------------
	
	template<typename CharT, typename Traits>
	class basic_streambuf : public std::basic_streambuf<CharT, Traits>
	{
	  public:

		typedef CharT								char_type;
		typedef Traits								traits_type;
		typedef typename traits_type::int_type		int_type;
		typedef typename traits_type::pos_type		pos_type;
		typedef typename traits_type::off_type		off_type;

		/// \brief constructor taking a \a path to the resource in memory
		basic_streambuf(const std::string& path)
			: m_rsrc(path)
		{
			init();
		}

		/// \brief constructor taking a \a rsrc 
		basic_streambuf(const rsrc& rsrc)
			: m_rsrc(rsrc)
		{
			init();
		}

		basic_streambuf(const basic_streambuf&) = delete;

		basic_streambuf(basic_streambuf&& rhs)
			: basic_streambuf(rhs.m_rsrc)
		{
		}

		basic_streambuf& operator=(const basic_streambuf&) = delete;

		basic_streambuf& operator=(basic_streambuf&& rhs)
		{
			swap(rhs);
			return *this;
		}

		void swap(basic_streambuf& rhs)
		{
			std::swap(m_begin, rhs.m_begin);
			std::swap(m_end, rhs.m_end);
			std::swap(m_current, rhs.m_current);
		}

	  private:

		void init()
		{
			m_begin = reinterpret_cast<const char_type*>(m_rsrc.data());
			m_end = reinterpret_cast<const char_type*>(m_rsrc.data() + m_rsrc.size());
			m_current = m_begin;
		}

		int_type underflow()
		{
			if (m_current == m_end)
				return traits_type::eof();

			return traits_type::to_int_type(*m_current);
		}

		int_type uflow()
		{
			if (m_current == m_end)
				return traits_type::eof();

			return traits_type::to_int_type(*m_current++);
		}

		int_type pbackfail(int_type ch)
		{
			if (m_current == m_begin or (ch != traits_type::eof() and ch != m_current[-1]))
				return traits_type::eof();

			return traits_type::to_int_type(*--m_current);
		}

		std::streamsize showmanyc()
		{
			assert(std::less_equal<const char*>()(m_current, m_end));
			return m_end - m_current;
		}

		pos_type seekoff(off_type off, std::ios_base::seekdir dir, std::ios_base::openmode which)
		{
			switch (dir)
			{
				case std::ios_base::beg:
					m_current = m_begin + off;
					break;

				case std::ios_base::end:
					m_current = m_end + off;
					break;

				case std::ios_base::cur:
					m_current += off;
					break;
				
				default:
					break;
			}

			if (m_current < m_begin)
				m_current = m_begin;
			
			if (m_current > m_end)
				m_current = m_end;

			return m_current - m_begin;
		}

		pos_type seekpos(pos_type pos, std::ios_base::openmode which)
		{
			m_current = m_begin + pos;

			if (m_current < m_begin)
				m_current = m_begin;
			
			if (m_current > m_end)
				m_current = m_end;

			return m_current - m_begin;
		}

	  private:
		rsrc m_rsrc;
		const char_type* m_begin;
		const char_type* m_end;
		const char_type* m_current;
	};

	using streambuf = basic_streambuf<char, std::char_traits<char>>;

	// --------------------------------------------------------------------
	// class mrsrc::istream

	template<typename CharT, typename Traits>
	class basic_istream : public std::basic_istream<CharT, Traits>
	{
	  public:
		typedef CharT	 						char_type;
		typedef Traits	 						traits_type;
		typedef typename traits_type::int_type	int_type;
		typedef typename traits_type::pos_type	pos_type;
		typedef typename traits_type::off_type	off_type;

	  private:

		using __streambuf_type = basic_streambuf<CharT, Traits>;
		using __istream_type = std::basic_istream<CharT, Traits>;

		__streambuf_type m_buffer;
	
	  public:

		basic_istream(const std::string& path)
			: __istream_type(&m_buffer)
			, m_buffer(path)
		{
			this->init(&m_buffer);
		}
		
		basic_istream(rsrc& resource)
			: __istream_type(&m_buffer)
			, m_buffer(resource)\
		{
			this->init(&m_buffer);
		}
		
		basic_istream(const basic_istream&) = delete;

		basic_istream(basic_istream&& rhs)
			: __istream_type(std::move(rhs))
			, m_buffer(std::move(rhs.m_buffer))
		{
			__istream_type::set_rdbuf(&m_buffer);
		}

		basic_istream& operator=(const basic_istream& ) = delete;

		basic_istream& operator=(basic_istream&& rhs)
		{
			__istream_type::operator=(std::move(rhs));
			m_buffer = std::move(rhs.m_buffer);
			return *this;
		}

		void swap(basic_istream& rhs)
		{
			__istream_type::swap(rhs);
			m_buffer.swap(rhs.m_buffer);
		}

		__streambuf_type* rdbuf() const
		{
			return const_cast<__streambuf_type*>(&m_buffer);
		}
	};

	using istream = basic_istream<char, std::char_traits<char>>;

} // namespace mrsrc
