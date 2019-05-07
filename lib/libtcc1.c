/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

double
__divdi3 (double a, double b)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__divdi3 stub\n");
  stub = 1;
  return ((int)a / (int)b);
}

double
__moddi3 (double a, double b)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__moddi3 stub\n");
  stub = 1;
  return ((int) a %(int)b);
}

unsigned long long
__udivdi3 (unsigned long long a, unsigned long long b)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__udivdi3 stub\n");
  stub = 1;
  int ai = a;
  int bi = b;
  return ai / bi;
}

unsigned long long
__umoddi3 (unsigned long long a, unsigned long long b)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__umoddi3 stub\n");
  stub = 1;
  int ai = a;
  int bi = b;
  return ai % bi;
}

unsigned long long
__lshrdi3 (unsigned long long a, int b)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__lshrdi3 stub\n");
  stub = 1;
  return a >> b;
}

long long
__ashldi3 (long long a, int b)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__ashldi3 stub\n");
  stub = 1;
  return a << b;
}

long long
__ashrdi3 (long long a, int b)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__ashrdi3 stub\n");
  stub = 1;
  return a >> b;
}

long double
__floatundixf (unsigned long long a)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__floatundixf stub\n");
  stub = 1;
  return 0;
}

unsigned long long
__fixunsxfdi (double a1)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__fixunsxfdi stub\n");
  stub = 1;
  return 0;
}

#if __TINYC__ == 9226
long
#elif __TINYC__
int
#else // !__TINYCC_
long long
#endif // !__TINYCC_
__fixdfdi (double a1)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__fixdfdi stub\n");
  stub = 1;
  return 0;
}

unsigned long long
__fixxfdi (double a1)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__fixxfdi stub\n");
  stub = 1;
  return 0;
}

long long
__fixsfdi (double a1)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("__fixsfdi stub\n");
  stub = 1;
  return 0;
}
