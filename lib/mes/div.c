/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
 *
 * This file is part of GNU Mes.
 *
 * GNU Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <mes/lib.h>
#include <stdint.h>
#include <limits.h>
#include <signal.h>

typedef struct
{
  long quot;
  long rem;
} ldiv_t;

int __raise(int);

void
__mesabi_div0 (void)
{
  if (__raise(SIGFPE) < 0) { /* could not raise SIGFPE */
    /* Fail in any way possible */
    unsigned char* x = (unsigned char*) 0;
    *x = 2;
  }
}

/* Compare gcc: __udivmoddi4 */
unsigned long
__mesabi_uldiv (unsigned long a, unsigned long b, unsigned long* remainder)
{
  unsigned long tmp;
  if (!remainder)
    remainder = &tmp;
  *remainder = 0;
  switch (b) {
  case 64UL:
    *remainder = a & 63UL;
    return a >> 6UL;
  case 32UL:
    *remainder = a & 31UL;
    return a >> 5UL;
  case 16UL:
    *remainder = a & 15UL;
    return a >> 4UL;
  case 8UL:
    *remainder = a & 7UL;
    return a >> 3UL;
  case 4UL:
    *remainder = a & 3UL;
    return a >> 2UL;
  case 2UL:
    *remainder = a & 1UL;
    return a >> 1UL;
  case 1UL:
    *remainder = 0;
    return a;
  case 0UL:
    __mesabi_div0();
    return 0UL;
  default:
    {
      unsigned long x;
      for (x = 0; a >= b; a -= b)
        ++x;
      *remainder = a;
      return x;
    }
  }
}

/* Note: Rounds towards zero.
   Maintainer: Be careful to satisfy quot * b + rem == a.
               That means that rem can be negative. */
void
__mesabi_ldiv(long a, long b, ldiv_t* result)
{
  int negate_result = (a < 0) ^ (b < 0);
  if (b == LONG_MIN)
    __mesabi_div0();
  if (a != LONG_MIN)
    {
      int negative_a = (a < 0);
      if (negative_a)
        a = -a;
      if (b < 0)
        b = -b;
      result->quot = __mesabi_uldiv(a, b, &result->rem);
      if (negate_result)
        result->quot = -result->quot;
      if (negative_a)
        result->rem = -result->rem;
    }
  else
    {
      result->rem = 0;
      if (b < 0)
        b = -b;
      if (b == 1)
        {
          result->quot = a;
          /* Since result->quot is already negative, don't negate it again. */
          negate_result = !negate_result;
        }
      else if (b == 0)
        __mesabi_div0();
      else
        {
          long x;
          for (x = 0; a <= -b; a += b)
            ++x;
          result->rem = a; /* negative */
          result->quot = x;
        }
      if (negate_result)
        result->quot = -result->quot;
    }
}

long
__mesabi_imod (long a, long b)
{
  ldiv_t result;
  __mesabi_ldiv(a, b, &result);
  return result.rem;
}

int
__mesabi_idiv (int a, int b)
{
  ldiv_t result;
  __mesabi_ldiv(a, b, &result);
  return result.quot;
}
