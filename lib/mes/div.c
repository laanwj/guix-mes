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
  if (b == 1)
    return a;
  else if (b == 0)
    __mesabi_div0();
  else
    {
      unsigned long x;
      for (x = 0; a >= b; a -= b)
        ++x;
      *remainder = a;
      return x;
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

#if __GNUC__ && !SYSTEM_LIBC && __arm__
// ...-binutils-2.31.1/bin/ld: hash.o: in function `hash_cstring':
// hash.c:(.text+0x56): undefined reference to `__aeabi_idivmod'
// ...-binutils-2.31.1/bin/ld: math.o: in function `divide':
// math.c:(.text+0x516): undefined reference to `__aeabi_idiv'
// ...-binutils-2.31.1/bin/ld: math.o: in function `modulo':
// math.c:(.text+0x5d2): undefined reference to `__aeabi_idivmod'
// ...-binutils-2.31.1/bin/ld: gcc-lib/libc.a(ntoab.o): in function `ntoab':
// ntoab.c:(.text+0x54): undefined reference to `__aeabi_uidivmod'
// ...-binutils-2.31.1/bin/ld: ntoab.c:(.text+0x62): undefined reference to `__aeabi_uidiv'

/* FIXME: This ABI should use registers for the input arguments, too.
Maybe the others below, too. */
long
__aeabi_idiv (long a, long b)
{
  ldiv_t result;
  __mesabi_ldiv(a, b, &result);
  return result.quot;
}

/* Result: r0: quotient; r1: remainder */
long
__aeabi_idivmod (long a, long b)
{
  ldiv_t result;
  __mesabi_ldiv(a, b, &result);
  register long rem_result asm("r1");
  rem_result = result.rem;
  return result.quot;
}

/* Result: r0: quotient; r1: remainder */
unsigned long
__aeabi_uidivmod (unsigned long a, unsigned long b)
{
  unsigned long quot;
  unsigned long rem;
  register unsigned long rem_result asm("r1");
  quot = __mesabi_uldiv (a, b, &rem);
  rem_result = rem;
  return quot;
}

unsigned long
__aeabi_uidiv (unsigned long a, unsigned long b)
{
  return __mesabi_uldiv (a, b, 0);
}
#endif // __GNUC__ && !SYSTEM_LIBC && __arm__
