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
#include <time.h>

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
  return _sys_call1 (SYS_dup, (int)old);
}

gid_t
getgid ()
{
  return _sys_call (SYS_getgid);
}

// long _sys_call (long sys_call);
// long _sys_call4 (long sys_call, long one, long two, long three, long four);

#define SA_SIGINFO 4
#define SA_RESTORER 0x04000000

#define SYS_rt_sigreturn 15

void
_restorer (void)
{
  _sys_call (SYS_rt_sigreturn);
}

# define __sigmask(sig) \
  (((unsigned long int) 1) << (((sig) - 1) % (8 * sizeof (unsigned long int))))

sighandler_t
signal (int signum, sighandler_t action)
{
#if __i386__
  return _sys_call2 (SYS_signal, signum, action);
#else
  static struct sigaction setup_action = {-1};
  static struct sigaction old = {0};

  setup_action.sa_handler = action;
  setup_action.sa_restorer = _restorer;
  setup_action.sa_mask = __sigmask (signum);
  old.sa_handler = SIG_DFL;
  setup_action.sa_flags = SA_RESTORER | SA_RESTART;
  int r = _sys_call4 (SYS_rt_sigaction, signum, &setup_action, &old, sizeof (sigset_t));
  if (r)
    return 0;
  return old.sa_handler;
#endif
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
  return _sys_call1 (SYS_pipe, (long)filedes);
}

int
dup2 (int old, int new)
{
  return _sys_call2 (SYS_dup2, (int)old, (int)new);
}

int
getrusage (int processes, struct rusage *rusage)
{
  return _sys_call2 (SYS_getrusage, (int)processes, (long)rusage);
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
fstat (int filedes, struct stat *statbuf)
{
  return _sys_call2 (SYS_fstat, (int)filedes, (long)statbuf);
}

int
getdents (int filedes, char *buffer, size_t nbytes)
{
  return _sys_call3 (SYS_getdents, (int)filedes, (long)buffer, (long)nbytes);
}

int
chdir (char const *file_name)
{
  return _sys_call1 (SYS_chdir, (long)file_name);
}

// bash
uid_t
geteuid ()
{
  return _sys_call (SYS_geteuid);
}

gid_t
getegid ()
{
  return _sys_call (SYS_getegid);
}

int
setuid (uid_t newuid)
{
  return _sys_call1 (SYS_setuid, (long)newuid);
}

int
setgid (gid_t newgid)
{
  return _sys_call1 (SYS_setgid, (long)newgid);
}

// make+POSIX
int
sigprocmask (int how, sigset_t const *set, sigset_t *oldset)
{
#if __i386__
  return _sys_call3 (SYS_sigprocmask, (long)how, (long)set, (long)oldset);
#else
  return _sys_call3 (SYS_rt_sigprocmask, (long)how, (long)set, (long)oldset);
#endif
}
