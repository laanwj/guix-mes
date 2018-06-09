/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>

int errno;

void
longjmp (jmp_buf env, int val)
{
  val = val == 0 ? 1 : val;
  asm ("mov    0xc(%ebp),%eax\n\t"     // val
       "mov    0x8(%ebp),%ebp\n\t"     // env*

       "mov    0x4(%ebp),%ebx\n\t"     // env->__pc
       "mov    0x8(%ebp),%esp\n\t"     // env->__sp
       "mov    0x0(%ebp),%ebp\n\t"     // env->__bp
       "jmp    *%ebx\n\t"              // jmp *PC
       );
  // not reached
  exit (42);
}

#if 0
int
setjmp_debug (jmp_buf env, int val)
{
  int i;
#if 1
  i = env->__bp;
  i = env->__pc;
  i = env->__sp;
#else
  i = env[0].__bp;
  i = env[0].__pc;
  i = env[0].__sp;
#endif
  return val == 0 ? 1 : val;
}
#endif

int
setjmp (jmp_buf env)
{
  int *p = (int*)&env;
  env[0].__bp = p[-2];
  env[0].__pc = p[-1];
  env[0].__sp = (int)&env;
  return 0;
}

unsigned long long
__udivdi3 (unsigned long long a, unsigned long long b)
{
  int ai = a;
  int bi = b;
  return ai / bi;
}

unsigned long long
__umoddi3 (unsigned long long a, unsigned long long b)
{
  int ai = a;
  int bi = b;
  return ai % bi;
}

unsigned long long
__lshrdi3 (unsigned long long a, int b)
{
  return a >> b;
}

long long
__ashldi3 (long long a, int b)
{
  return a << b;
}

long long
__ashrdi3 (long long a, int b)
{
  return a >> b;
}

long double
__floatundixf (unsigned long long a)
{
#if NOISY_FLOATS
  eputs ("__floatundix stub\n");
#endif
  return 0;
}

unsigned long long
__fixunsxfdi (double a1)
{
#if NOISY_FLOATS
  eputs ("__fixunsxfdi stub\n");
#endif
  return 0;
}

unsigned long long
__fixdfdi (double a1)
{
#if NOISY_FLOATS
  eputs ("__fixdfdi stub\n");
#endif
  return 0;
}

unsigned long long
__fixxfdi (double a1)
{
#if NOISY_FLOATS
  eputs ("__fixxfdi stub\n");
#endif
  return 0;
}

unsigned long long
__fixsfdi (double a1)
{
#if NOISY_FLOATS
  eputs ("__fixsfdi stub\n");
#endif
  return 0;
}
