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

#ifndef STDIN
#define STDIN 0
#endif

#ifndef STDOUT
#define STDOUT 1
#endif

#ifndef STDERR
#define STDERR 2
#endif

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

#ifndef EOF
#define EOF -1
#endif

#ifndef NULL
#define NULL 0
#endif

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

typedef int FILE;

#ifndef __MES_SIZE_T
#define __MES_SIZE_T
typedef unsigned long size_t;
#endif

int eputs (char const* s);
FILE *fdopen (int fd, char const *mode);
int fprintf (FILE *stream, char const *format, ...);
int fputc (int c, int fd);
int fputs (char const* s, int fd);
size_t fwrite (void const *ptr, size_t size, size_t nmemb, FILE *stream);
int getchar ();
int printf (char const* format, ...);
int putchar (int c);
int puts (char const* s);
int snprintf(char *str,  size_t size,  char const *format, ...);
int sprintf (char *str, char const* format, ...);
int ungetc (int c, int fd);

#endif // ! (__GNUC__ && POSIX)

#endif // __MES_STDIO_H
