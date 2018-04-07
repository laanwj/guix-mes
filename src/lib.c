/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan Nieuwenhuizen <janneke@gnu.org>
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

int g_depth;
SCM fdisplay_ (SCM, int, int);

SCM
display_helper (SCM x, int cont, char* sep, int fd, int write_p)
{
  fputs (sep, fd);
  if (g_depth == 0)
    return cell_unspecified;
  g_depth = g_depth - 1;
  
  switch (TYPE (x))
    {
    case TCHAR:
      {
        if (!write_p)
          fputc (VALUE (x), fd);
        else
          {
            fputs ("#\\", fd);
            switch (VALUE (x))
              {
              case '\0': fputs ("nul", fd); break;
              case '\a': fputs ("alarm", fd); break;
              case '\b': fputs ("backspace", fd); break;
              case '\t': fputs ("tab", fd); break;
              case '\n': fputs ("newline", fd); break;
              case '\v': fputs ("vtab", fd); break;
              case '\f': fputs ("page", fd); break;
              case '\r': fputs ("return", fd); break;
              case ' ': fputs ("space", fd); break;
              default: fputc (VALUE (x), fd);
              }
          }
        break;
      }
    case TCLOSURE:
      {
        fputs ("#<closure ", fd);
        //display_helper (CDR (x), cont, "", fd, 0);
        fputs (">", fd);
        break;
      }
    case TFUNCTION:
      {
        fputs ("#<procedure ", fd);
        char const *p = "?";
        if (FUNCTION (x).name != 0)
          p = FUNCTION (x).name;
        fputs (p, fd);
        fputs ("[", fd);
        fputs (itoa (CDR (x)), fd);
        fputs (",", fd);
        fputs (itoa (x), fd);
        fputs ("]>", fd);
        break;
      }
    case TMACRO:
      {
        fputs ("#<macro ", fd);
        display_helper (CDR (x), cont, "", fd, 0);
        fputs (">", fd);
        break;
      }
    case TVARIABLE:
      {
        fputs ("#<variable ", fd);
        if (VARIABLE_GLOBAL_P (x) == cell_t)
          fputs ("*global* ", fd);
        display_helper (CAR (VARIABLE (x)), cont, "", fd, 0);
        fputs (">", fd);
        break;
      }
    case TNUMBER:
      {
        fputs (itoa (VALUE (x)), fd);
        break;
      }
    case TPAIR:
      {
        if (!cont)
          fputs ("(", fd);
        if (CAR (x) == cell_closure)
          fputs ("*closure* ", fd);
        else
        if (CAAR (x) == cell_closure)
          fputs ("(*closure* ...) ", fd);
        else
        if (CAR (x) == cell_circular)
          {
            fputs ("(*circ* . ", fd);
            int i = 0;
            x = CDR (x);
            while (x != cell_nil && i++ < 10)
              {
                g_depth = 1;
                display_helper (CAAR (x), 0, "", fd, write_p); fputs (" ", fd);
                //fdisplay_ (CAAR (x), fd, write_p); fputs (" ", fd);
                x = CDR (x);
              }
            fputs (" ...)", fd);
          }
        else
          {
            if (x && x != cell_nil)
              fdisplay_ (CAR (x), fd, write_p);
            if (CDR (x) && TYPE (CDR (x)) == TPAIR)
              display_helper (CDR (x), 1, " ", fd, write_p);
            else if (CDR (x) && CDR (x) != cell_nil)
              {
                if (TYPE (CDR (x)) != TPAIR)
                  fputs (" . ", fd);
                fdisplay_ (CDR (x), fd, write_p);
              }
          }
        if (!cont)
          fputs (")", fd);
        break;
      }
    case TKEYWORD:
    case TSPECIAL:
    case TSTRING:
    case TSYMBOL:
      {
        if (TYPE (x) == TKEYWORD)
          fputs ("#:", fd);
        if (write_p && TYPE (x) == TSTRING)
          fputc ('"', fd);
        SCM t = CAR (x);
        while (t && t != cell_nil)
          {
            switch (write_p ? VALUE (CAR (t)) : 0)
              {
              case '\t': fputs ("\\t", fd); break;
              case '\n': fputs ("\\n", fd); break;
              case '\\': fputs ("\\\\", fd); break;
              case '"': fputs ("\\\"", fd); break;
              default: fputc (VALUE (CAR (t)), fd);
              }
            t = CDR (t);
          }
        if (write_p && TYPE (x) == TSTRING)
          fputc ('"', fd);
        break;
      }
    case TVECTOR:
      {
        fputs ("#(", fd);
        SCM t = CAR (x);
        for (int i = 0; i < LENGTH (x); i++)
          {
            if (i)
              fputc (' ', fd);
            fdisplay_ (VECTOR (x) + i, fd, write_p);
          }
        fputc (')', fd);
        break;
      }
    default:
      {
        fputs ("<", fd);
        fputs (itoa (TYPE (x)), fd);
        fputs (":", fd);
        fputs (itoa (x), fd);
        fputs (">", fd);
        break;
      }
    }
  return 0;
}

SCM
display_ (SCM x)
{
  g_depth = 5;
  return display_helper (x, 0, "", g_stdout, 0);
}

SCM
display_error_ (SCM x)
{
  g_depth = 5;
  return display_helper (x, 0, "", STDERR, 0);
}

SCM
display_port_ (SCM x, SCM p)
{
  assert (TYPE (p) == TNUMBER);
  return fdisplay_ (x, VALUE (p), 0);
}

SCM
write_ (SCM x)
{
  g_depth = 5;
  return display_helper (x, 0, "", g_stdout, 1);
}

SCM
write_error_ (SCM x)
{
  g_depth = 5;
  return display_helper (x, 0, "", STDERR, 1);
}

SCM
write_port_ (SCM x, SCM p)
{
  assert (TYPE (p) == TNUMBER);
  return fdisplay_ (x, VALUE (p), 1);
}

SCM
fdisplay_ (SCM x, int fd, int write_p) ///((internal))
{
  g_depth = 5;
  return display_helper (x, 0, "", fd, write_p);
}

SCM
exit_ (SCM x) ///((name . "exit"))
{
  assert (TYPE (x) == TNUMBER);
  exit (VALUE (x));
}

SCM
xassq (SCM x, SCM a) ///for speed in core only
{
  while (a != cell_nil && x != CDAR (a))
    a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}

SCM
memq (SCM x, SCM a)
{
  switch (TYPE (x))
    {
    case TCHAR:
    case TNUMBER:
      {
        SCM v = VALUE (x);
        while (a != cell_nil && v != VALUE (CAR (a)))
          a = CDR (a);
        break;
      }
    case TKEYWORD:
      {
        SCM v = STRING (x);
        while (a != cell_nil && v != STRING (CAR (a)))
          a = CDR (a);
        break;
      }
      // case TSYMBOL:
      // case TSPECIAL:
    default:
      while (a != cell_nil && x != CAR (a))
        a = CDR (a);
    }
  return a != cell_nil ? a : cell_f;
}

SCM
equal2_p (SCM a, SCM b)
{
  if (a == cell_nil && b == cell_nil)
    return cell_t;
  if (TYPE (a) == TPAIR && TYPE (b) == TPAIR)
    return equal2_p (CAR (a), CAR (b)) == cell_t
      && equal2_p (CDR (a), CDR (b)) == cell_t
      ? cell_t : cell_f;
  if (TYPE (a) == TSTRING && TYPE (b) == TSTRING)
    return equal2_p (STRING (a), STRING (b));
  if (TYPE (a) == TVECTOR && TYPE (b) == TVECTOR)
    {
      if (LENGTH (a) != LENGTH (b))
        return cell_f;
      for (int i=0; i < LENGTH (a); i++)
        {
          SCM ai = VECTOR (a) + i;
          SCM bi = VECTOR (b) + i;
          if (TYPE (ai) == TREF)
            ai = REF (ai);
          if (TYPE (bi) == TREF)
            bi = REF (bi);
          if (equal2_p (ai, bi) == cell_f)
            return cell_f;
        }
      return cell_t;
    }
  return eq_p (a, b);
}

SCM
member (SCM x, SCM a)
{
  switch (TYPE (x))
    {
    case TCHAR:
    case TNUMBER:
    case TKEYWORD:
    case TSYMBOL:
    case TSPECIAL:
      return memq (x, a);
    default:
      while (a != cell_nil && equal2_p (x, CAR (a)) != cell_t)
        a = CDR (a);
    }
  return a != cell_nil ? a : cell_f;
}
