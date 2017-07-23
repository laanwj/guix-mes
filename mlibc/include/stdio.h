/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_STDIO_H
#define __MES_STDIO_H 1

char **g_environment;
int g_stdin;
int g_stdout;

#define EOF -1
#define NULL 0
#define STDIN 0
#define STDOUT 1
#define STDERR 2

int printf (char const* format, ...);
int sprintf (char *str, char const* format, ...);

#if __GNUC__ && POSIX
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_STDIO_H
#include_next <stdio.h>

int fdputs (char const* s, int fd);

#undef puts
#define puts(x) fdputs(x, STDOUT)
#define eputs(x) fdputs(x, STDERR)
#define fputs fdputs

#ifdef putc
#undef putc
#endif

int getchar ();

int fdputc (int c, int fd);

#define fputc fdputc
#define ungetc fdungetc
int fdungetc (int c, int fd);

#else // ! (__GNUC__ && POSIX)

// Hmm
#define stdin 0
#define stdout 1
#define stderr 2

// TODO: fseek etc
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#if __GNUC__
#undef fputs
#undef fdputs
int fdputs (char const* s, int fd);
#endif // __MES_GNUC__

int eputs (char const* s);
int fputs (char const* s, int fd);
int puts (char const* s);
int putchar (int c);
int fputc (int c, int fd);
int getchar ();
int ungetc (int c, int fd);

typedef int FILE;

#endif // ! (__GNUC__ && POSIX)

#endif // __MES_STDIO_H
