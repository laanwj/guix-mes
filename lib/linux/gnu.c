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

#if __i386__
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
#elif __x86_64__
sighandler_t
signal (int signum, sighandler_t action)
{
  sighandler_t old;
  _sys_call3 (SYS_rt_sigaction, signum, action, &old);
  return old;
}
#else
#error arch not supported
#endif

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
