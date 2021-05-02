/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_LINUX_RISCV_SYSCALL_H
#define __MES_LINUX_RISCV_SYSCALL_H 1

/** RISC-V uses the asm-generic syscalls (asm-generic/unistd.h) with the following configuration for 64-bit:
 * #define __ARCH_WANT_NEW_STAT
 * #define __ARCH_WANT_SET_GET_RLIMIT
 * #define __ARCH_WANT_SYS_CLONE3
 */

// libc-mini
// #define SYS_exit        93
// #define SYS_write       64

// libc
#define SYS_clone          220
#define SYS_read           63
#define SYS_openat         56
#define SYS_wait4          260
#define SYS_execve         221
#define SYS_fchmodat       53
#define SYS_faccessat      48
#define SYS_brk            214
#define SYS_ioctl          29
#define SYS_fsync          82

// libc+tcc
#define SYS_close          57
#define SYS_lseek          62
#define SYS_unlinkat       35
#define SYS_gettimeofday   169
#define SYS_getcwd         17

// libc+gnu

#define SYS_chdir          49
#define SYS_linkat         37
#define SYS_getpid         172
#define SYS_getuid         174
#define SYS_kill           129
#define SYS_renameat2      276
#define SYS_mkdirat        34
#define SYS_dup            23
#define SYS_pipe2          59
#define SYS_getgid         176
#define SYS_rt_sigaction   134
#define SYS_fcntl          25
#define SYS_dup3           24
#define SYS_getrusage      165
#define SYS_newfstatat     79
#define SYS_setitimer      103
#define SYS_fstat          80
#define SYS_nanosleep      101
#define SYS_getdents64     61
#define SYS_clock_gettime  113

// bash
#define SYS_setuid         146
#define SYS_geteuid        175
#define SYS_getegid        177
#define SYS_setgid         144
#define SYS_getppid        173

// make+POSIX
#define SYS_rt_sigprocmask 135

// tar
#define SYS_symlinkat      36
#define SYS_readlinkat     78
#define SYS_mknodat        33

#endif // __MES_LINUX_RISCV_SYSCALL_H
