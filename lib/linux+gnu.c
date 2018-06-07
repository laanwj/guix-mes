/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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


int
link (char const *old_name, char const *new_name)
{
  return _sys_call2 (SYS_link, (int)old_name, (int)new_name);
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
  return _sys_call2 (SYS_kill, (int)pid, (int)signum);
}

int
rename (char const *old_name, char const *new_name)
{
  return _sys_call2 (SYS_rename, (int)old_name, (int)new_name);
}

int
mkdir (char const *file_name, mode_t mode)
{
  return _sys_call2 (SYS_mkdir, (int)file_name, (int)mode);
}

int
dup (int old)
{
  return _sys_call1 (SYS_dup, (int)old);
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
  int r = _sys_call3 (SYS_fcntl, (int)filedes, (int)command, (int)data);
  va_end (ap);
  return r;
}

int
pipe (int filedes[2])
{
  return _sys_call1 (SYS_pipe, (int)filedes);
}

int
dup2 (int old, int new)
{
  return _sys_call2 (SYS_dup2, (int)old, (int)new);
}

int
getrusage (int processes, struct rusage *rusage)
{
  return _sys_call2 (SYS_getrusage, (int)processes, (int)rusage);
}

int
lstat (char const *file_name, struct stat *statbuf)
{
  return _sys_call2 (SYS_lstat, (int)file_name, (int)statbuf);
}

int
nanosleep (const struct timespec *requested_time,
           struct timespec *remaining)
{
  return _sys_call2 (SYS_nanosleep, (int)requested_time, (int)remaining);
}

int
setitimer (int which, struct itimerval const *new,
          struct itimerval *old)
{
  return _sys_call3 (SYS_setitimer, (int)which, (int)new, (int)old);
}

int
fstat (int fd, struct stat *statbuf)
{
  return _sys_call2 (SYS_fstat, (int)fd, (int)statbuf);
}
