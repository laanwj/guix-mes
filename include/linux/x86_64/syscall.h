/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_LINUX_X86_64_SYSCALL_H
#define __MES_LINUX_X86_64_SYSCALL_H 1

// #define SYS_write   0x01
// #define SYS_exit    0x3c

#define SYS_fork    0x39
#define SYS_read    0x00
#define SYS_open    0x02
//#define SYS_waitpid
#define SYS_wait4   0x3d
#define SYS_execve  0x3a
#define SYS_chmod   0x5a
#define SYS_access  0x15
#define SYS_brk     0x0c
#define SYS_ioctl   0x10
#define SYS_fsync   0x4a

#endif // __MES_LINUX_X86_64_SYSCALL_H
