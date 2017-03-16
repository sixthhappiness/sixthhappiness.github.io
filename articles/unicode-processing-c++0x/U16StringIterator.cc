// The following code is original work by me.  I now place it in the public domain.  Do not plagiarize.
// 
// Andrew Choi.  Calgary, Canada, October 12, 2010.

#include "U16StringIterator.h"

size_t u16string_codepoint_count(const std::u16string& s)
{
  size_t j = 0;
  for (u16string_iterator i = s.begin(); i != s.end(); i++)
    j++;

  return j;
}
