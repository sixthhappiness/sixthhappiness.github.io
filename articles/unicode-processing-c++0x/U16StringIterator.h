// The following code is original work by me.  I now place it in the public domain.  Do not plagiarize.
// 
// Andrew Choi.  Calgary, Canada, October 12, 2010.

#ifndef U16STRINGITERATOR_H
#define U16STRINGITERATOR_H

#include <string>
#include <iostream>
#include <iterator>

static const char16_t high_surrogate_min = 0xd800u;
static const char16_t high_surrogate_max = 0xdbffu;
static const char16_t low_surrogate_min = 0xdc00u;
static const char16_t low_surrogate_max = 0xdfffu;

inline bool is_high_surrogate(char16_t c)
{
  return high_surrogate_min <= c && c <= high_surrogate_max;
}

inline bool is_low_surrogate(char16_t c)
{
  return low_surrogate_min <= c && c <= low_surrogate_max;
}

class u16string_iterator :
  public std::iterator<std::bidirectional_iterator_tag, const char32_t>
{
  std::u16string::const_iterator i;

public:
  u16string_iterator(const std::u16string::const_iterator& u16i) : i(u16i) { }

  u16string_iterator(const u16string_iterator& i2) : i(i2.i) { }

  u16string_iterator& operator++()
  {
    if (is_high_surrogate(*i))
      ++i;
    ++i;
    
    return *this;
  }

  u16string_iterator& operator++(int)
  {
    u16string_iterator& t = *this;
    operator++();
    return t;
  }

  u16string_iterator& operator--()
  {
    --i;
    if (is_low_surrogate(*i))
      --i;
    
    return *this;
  }

  u16string_iterator& operator--(int)
  {
    u16string_iterator& t = *this;
    operator--();
    return t;
  }

  bool operator==(const u16string_iterator& i2) const { return i == i2.i; }

  bool operator!=(const u16string_iterator& i2) const { return i != i2.i; }

  char32_t operator*() const
  {
    if (is_high_surrogate(*i))
      {
	std::u16string::const_iterator t = i;
	unsigned long m = (*t - high_surrogate_min) << 10;
	unsigned long n = *++t - low_surrogate_min;
	return char32_t((m | n) + 0x10000u);
      }
    else
      return char32_t(*i);
  }

  operator std::u16string::const_iterator() const { return i; }
};

extern size_t u16string_codepoint_count(const std::u16string& s);

#endif
