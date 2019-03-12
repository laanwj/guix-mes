/*
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#ifndef __MES_GNU_HURD_H
#define __MES_GNU_HURD_H

#define _GNU_SOURCE 1
#define __USE_GNU 1

#include <errno.h>
#include <sys/types.h>

#ifndef _BITS_TYPES_H
#ifndef _LOFF_T
#define _LOFF_T
typedef off64_t loff_t;
#endif
#endif

#include <mach/mach_types.h>
#include <mach/message.h>
#include <mach/port.h>

struct hurd_startup_data
  {
    int flags;
    mach_port_t *dtable;
    mach_msg_type_number_t dtablesize;
    mach_port_t *portarray;
    mach_msg_type_number_t portarraysize;
    int *intarray;
    mach_msg_type_number_t intarraysize;
    vm_address_t stack_base;
    vm_size_t stack_size;
    vm_address_t phdr;
    vm_size_t phdrsz;
    vm_address_t user_entry;
  };

#define _HURD_DTABLE_MAX 1024
extern mach_port_t _hurd_dtable[_HURD_DTABLE_MAX];
extern int _hurd_dtable_size;
extern struct hurd_startup_data _hurd_startup_data;

mach_port_t fd_get (int filedes);
error_t fd_write (mach_port_t port, void const *buffer, size_t *size, loff_t offset);
error_t fd_read (mach_port_t port, void *buffer, size_t *size, loff_t offset);

#endif // __MES_GNU_HURD_H
