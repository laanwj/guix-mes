/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2021 W. J. van der Laan <laanwj@protonmail.com>
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
#ifndef __MES_STDARG_H
#define __MES_STDARG_H 1

#if SYSTEM_LIBC
#undef __MES_STDARG_H
#include_next <stdarg.h>

#define va_arg8(ap, type) va_arg(ap, type)

#elif __GNUC__ && __riscv

// GCC on RISC-V always passes arguments in registers. Implementing these macros without
// the use of built-ins would be very involved.
typedef __builtin_va_list va_list;

#define va_start(v,l)   __builtin_va_start(v,l)
#define va_end(v)       __builtin_va_end(v)
#define va_arg(v,l)     __builtin_va_arg(v,l)
#define va_arg8(ap, type) va_arg(ap, type)
#define va_copy(d,s)    __builtin_va_copy(d,s)

#else // ! SYSTEM_LIBC && ! __riscv

#include <sys/types.h>

#if __GNUC__ && __x86_64__
#define __FOO_VARARGS 1
#endif

typedef char *va_list;
#define va_start(ap, last) (void)((ap) = (char*)(&(last) + 1))
#define va_arg(ap, type) (type)(((long*)((ap) = ((ap) + sizeof (void*))))[-1])
#define va_align(ap, alignment) ((char*)((((unsigned long) (ap)) + (alignment) - 1) &~ ((alignment) - 1)))
#define va_arg8(ap, type) (type)(((double*)((ap) = (va_align((ap), 8) + sizeof(double))))[-1])
#define va_end(ap) (void)((ap) = 0)
#define va_copy(dest, src) dest = src

int vexec (char const *file_name, va_list ap);
int vfprintf (FILE * stream, char const *template, va_list ap);
int vfscanf (FILE * stream, char const *template, va_list ap);
int vprintf (char const *format, va_list ap);
int vsprintf (char *str, char const *format, va_list ap);
int vsnprintf (char *str, size_t size, char const *format, va_list ap);
int vsscanf (char const *s, char const *template, va_list ap);

#endif // ! SYSTEM_LIBC

#endif // __MES_STDARG_H
