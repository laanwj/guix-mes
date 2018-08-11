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

#define SYS_link      0x09
#define SYS_getpid    0x14
#define SYS_getuid    0x18
#define SYS_kill      0x25
#define SYS_rename    0x26
#define SYS_mkdir     0x27
#define SYS_dup       0x29
#define SYS_pipe      0x2a
#define SYS_getgid    0x2f
#define SYS_signal    0x30
#define SYS_fcntl     0x37
#define SYS_dup2      0x3f
#define SYS_getrusage 0x4d
#define SYS_lstat     0x6b
#define SYS_setitimer 0x68
#define SYS_fstat     0x6c
#define SYS_nanosleep 0xa2

#include <sys/resource.h>

int
link (char const *old_name, char const *new_name)
{
  return _sys_call2 (SYS_link, (long)old_name, (long)new_name);
}

pid_t
getpid ()
{
  return _sys_call (SYS_getpid);
}

uid_t
getuid ()
{
  return _sys_call (SYS_getuid);
}

int
kill (pid_t pid, int signum)
{
  return _sys_call2 (SYS_kill, (long)pid, (long)signum);
}

int
rename (char const *old_name, char const *new_name)
{
  return _sys_call2 (SYS_rename, (long)old_name, (long)new_name);
}

int
mkdir (char const *file_name, mode_t mode)
{
  return _sys_call2 (SYS_mkdir, (long)file_name, (long)mode);
}

int
dup (int old)
{
  return _sys_call1 (SYS_dup, (long)old);
}

gid_t
getgid ()
{
  return _sys_call (SYS_getgid);
}

#if __MESC__
void *
signal (int signum, void * action)
#else
sighandler_t
signal (int signum, sighandler_t action)
#endif
{
  return _sys_call2 (SYS_signal, signum, action);
}

int
fcntl (int filedes, int command, ...)
{
  va_list ap;
  va_start (ap, command);
  int data = va_arg (ap, int);
  int r = _sys_call3 (SYS_fcntl, (long)filedes, (long)command, (long)data);
  va_end (ap);
  return r;
}

int
pipe (int filedes[2])
{
  return _sys_call1 (SYS_pipe, (long)filedes);
}

int
dup2 (int old, int new)
{
  return _sys_call2 (SYS_dup2, (long)old, (long)new);
}

int
getrusage (int processes, struct rusage *rusage)
{
  return _sys_call2 (SYS_getrusage, (long)processes, (long)rusage);
}

int
lstat (char const *file_name, struct stat *statbuf)
{
  return _sys_call2 (SYS_lstat, (long)file_name, (long)statbuf);
}

int
nanosleep (const struct timespec *requested_time,
           struct timespec *remaining)
{
  return _sys_call2 (SYS_nanosleep, (long)requested_time, (long)remaining);
}

int
setitimer (int which, struct itimerval const *new,
          struct itimerval *old)
{
  return _sys_call3 (SYS_setitimer, (long)which, (long)new, (long)old);
}

int
fstat (int fd, struct stat *statbuf)
{
  return _sys_call2 (SYS_fstat, (long)fd, (long)statbuf);
}
