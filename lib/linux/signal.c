/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#if __x86_64__
void
_restorer (void)
{
  _sys_call (SYS_rt_sigreturn);
}
#endif

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
