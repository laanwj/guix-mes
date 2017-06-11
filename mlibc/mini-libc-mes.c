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

int g_stdin = 0;
char **g_environment;
int _env ();
int exit ();
int main(int,char*[]);

int
_start ()
{
#if 0
  asm (".byte 0x89 0xe8");      // mov    %ebp,%eax
  asm (".byte 0x83 0xc0 0x08"); // add    $0x8,%eax
  asm (".byte 0x50");           // push   %eax

  asm (".byte 0x89 0xe8");      // mov    %ebp,%eax
  asm (".byte 0x83 0xc0 0x04"); // add    $0x4,%eax
  asm (".byte 0x0f 0xb6 0x00"); // movzbl (%eax),%eax
  asm (".byte 0x50");           // push   %eax

  asm (".byte 0x89 0xe8");      // mov    %ebp,%eax
  asm (".byte 0x83 0xc0 0x04"); // add    $0x4,%eax
  asm (".byte 0x0f 0xb6 0x00"); // movzbl (%eax),%eax
  asm (".byte 0x83 0xc0 0x03"); // add    $0x3,%eax
  asm (".byte 0xc1 0xe0 0x02"); // shl    $0x2,%eax
  asm (".byte 0x01 0xe8");      // add    %ebp,%eax
  asm (".byte 0x50");           // push   %eax

  g_environment = _env ();
  asm (".byte 0x58");
  int r = main ();
  exit (r);
#else
  int r = main ();
  exit (r);
#endif
}

char **
_env (char **e)
{
  return e;
}

void
exit ()
{
  asm (".byte 0x8b 0x5d 0x08");                    // mov    0x8(%ebp),%ebx
  asm (".byte 0xb8 0x01 0x00 0x00 0x00");          // mov    $0x1,%eax
  asm (".byte 0xcd 0x80");                         // int    $0x80
}

void
write ()
{
  asm (".byte 0x8b 0x5d 0x08");                   // mov    0x8(%ebp),%ebx
  asm (".byte 0x8b 0x4d 0x0c");                   // mov    0xc(%ebp),%ecx
  asm (".byte 0x8b 0x55 0x10");                   // mov    0x10(%ebp),%edx

  asm (".byte 0xb8 0x04 0x00 0x00 0x00");         // mov    $0x4,%eax
  asm (".byte 0xcd 0x80");                        // int    $0x80
}

int
strlen (char const* s)
{
  int i = 0;
  while (s[i]) i++;
  return i;
}

int
eputs (char const* s)
{
  int i = strlen (s);
  write (2, s, i);
  return 0;
}
