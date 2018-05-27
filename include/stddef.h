/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_STDDEF_H
#define __MES_STDDEF_H 1

#if __GNUC__ && POSIX
#undef __MES_STDDEF_H
#include_next <stddef.h>
#else // ! (__GNUC__ && POSIX)
#include <stdint.h>
#include <unistd.h>

#ifndef offsetof
#if __MESC__
#define offsetof(type, field) (&((type *)0)->field)
#else // !__MESC__
#define offsetof(type, field) ((size_t)&((type *)0)->field)
#endif // !__MESC__
#endif // offsetof

#endif // ! (__GNUC__ && POSIX)

#endif // __MES_STDDEF_H
