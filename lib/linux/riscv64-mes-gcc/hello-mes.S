/*
 * GNU Mes --- Maxwell Equations of Software
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

/** Code: */

SYS_exit 	= 93		/* Linux syscalls.  */
SYS_write 	= 64
stdout		= 1		/* File descriptor */
	.globl _start		/* Magic linker symbol: --entry-address.  */

	.text			/* Program text. */
hello:	.ascii	"Hello, GNU Mes!\n"
bye:
.equ string_size, (bye - hello)

_start:
	li	a7, SYS_write	/* System call function: write, in a7.  */
	li	a0, stdout	/* 1st parameter: file descriptor, in a0. */
.equ string_offset, (hello - .)
	auipc	a1, 0		/* Load pc for pc-relative address */
	addi	a1, a1, string_offset /* 2nd parameter: address, in a1. */
	li	a2, string_size	/* 3rd parameter: byte count a2. */
	ecall			/* Call system.  */

	li	a7, SYS_exit	/* System call function: exit, in a7.  */
	li	a0, 0		/* 1st parameter: exit status, in a0. */
	ecall			/* Call system.  */
	wfi 			/* Should not be reached.  */
