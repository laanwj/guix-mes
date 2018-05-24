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
#ifndef __MES_UNISTD_H
#define __MES_UNISTD_H 1

#if __GNUC__ && POSIX
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_UNISTD_H
#include_next <unistd.h>

#else // ! (__GNUC__ && POSIX)

#ifndef __MES_OFF_T
#define __MES_OFF_T
#undef off_t
typedef unsigned long off_t;
#endif

#ifndef __MES_SIZE_T
#define __MES_SIZE_T
#undef size_t
typedef unsigned long size_t;
#endif

#ifndef __MES_SSIZE_T
#define __MES_SSIZE_T
#undef ssize_t
typedef long ssize_t;
#endif

int access (char const *s, int mode);
int close (int fd);
int execve (char const *file, char *const argv[], char *const env[]);
int execvp (char const *file, char *const argv[]);
int fork ();
char *getcwd (char *buf, size_t size);
int isatty (int fd);
off_t lseek (int fd, off_t offset, int whence);
int read (int fd, void* buf, size_t n);
int unlink (char const *file_name);
int write (int fd, char const* s, int n);
#endif // ! (__GNUC__ && POSIX)

#endif // __MES_UNISTD_H
