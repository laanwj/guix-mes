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

#if __GNUC__
#include "mlibc.c"
#endif
#define assert(x) ((x) ? (void)0 : assert_fail (#x))

struct scm {
  int type;
  int car;
  int cdr;
};

int bla = 1234;
char arena[84];
struct scm *g_cells = arena;
char *g_chars = arena;

int foo () {puts ("t: foo\n"); return 0;};
int bar (int i) {puts ("t: bar\n"); return 0;};
struct function {
  int (*function) (void);
  int arity;
  char *name;
};
struct function g_fun = {&exit,1,"fun"};
struct function g_foo = {&foo,0,"foo"};
struct function g_bar = {&bar,1,"bar"};

//void *functions[2];
int functions[2];

struct function g_functions[2];
int g_function = 0;

enum type_t {TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TREF, TSPECIAL, TSTRING, TSYMBOL, TVALUES, TVECTOR, TBROKEN_HEART};

typedef int SCM;
int g_free = 3;
SCM tmp;
SCM tmp_num;

int ARENA_SIZE = 200;
#define TYPE(x) (g_cells[x].type)
#define CAR(x) g_cells[x].car
#define CDR(x) g_cells[x].cdr
#define VALUE(x) g_cells[x].cdr

#define CAAR(x) CAR (CAR (x))

struct scm scm_fun = {TFUNCTION,0,0};
SCM cell_fun;

#if 1
int
add (int a, int b)
{
  return a + b;
}

int
inc (int i)
{
  return i + 1;
}

int
identity (int i)
{
  return i;
}

int
label (int c)
{
 label:
  if (c == 0) return c;
  c--;
  goto label;
  return 1;
}

int
swits (int c)
{
  int x = -1;

  switch (c)
    {
    case TCHAR: {goto next;}
    case 1: {goto next;}
    case 2: {goto next;}
    default: {goto next;}
    }

  return 1;
 next:
  switch (c)
    {
      case 0:
        {
          x = 0;
          c = 34;
          break;
        }
      case 1:
        {
          x = 1;
          break;
        }
      default:
        {
          x = 2;
          break;
        }
    }
  return x;
}

int g = 48;
int
get ()
{
  int i = g;
  g++;
  return i;
}

int
read_test ()
{
  char *p = (char*)g_chars;
  int i = 0;
  puts ("t: read 0123456789\nt: ");
  int c = get ();
  while (i < 10) {
    *p++ = c;
    putchar (c);
    c = get ();
    i++;
  }
  puts ("\n");
  if (strcmp (g_chars, "0123456789")) return 1;

  puts ("t: ungetc ('A') == getchar ()\n");
  ungetc ('A', STDIN);
  if (getchar () != 'A') return 1;
  ungetc (0, STDIN);
  //ungetc ('\1', STDIN);
  ungetc (1, STDIN);
  puts ("t: ungetc ();ungetc ();getchar ();getchar ()\n");
  if (getchar () != 1) return 1;
  //if (getchar () != '\0') return 1;
  if (getchar () != 0) return 1;

  return 0;
}

int
math_test ()
{
  int i;

  puts ("t: 0 < 0\n");
  if (0 < 0) return 1;

  puts ("t: 2 < 1\n");
  if (2 < 1) return 1;

  puts ("t: -1 < -2\n");
  if (-1 < -2) return 1;

  puts ("t: 0 < -1\n");
  if (0 < -1) return 1;

  puts ("t: 0 > 0\n");
  if (0 > 0) return 1;

  puts ("t: 1 > 2\n");
  if (1 > 2) return 1;

  puts ("t: -2 > -1\n");
  if (-2 > -1) return 1;

  puts ("t: -1 > 0\n");
  if (-1 > 0) return 1;

  puts ("t: 1 == inc (0)\n");
  if (1 == inc (0)) goto ok0;
  return 1;
 ok0:

  puts ("t: 0 < inc (0)\n");
  if (0 < inc (0)) goto ok1;
  return 1;
 ok1:

  puts ("t: 4/2=");
  i = 4 / 2;
  if (i!=2) return 1;
  i += 48;
  putchar (i);
  puts ("\n");

  puts ("t: 3*4=");
  i = 3 * 4;
  if (i!=12) return 1;

  puts ("t: 1 << 3\n");
  if (1 << 3 != 8) return 1 << 3;

  puts ("t: 3 << 4\n");
  if (3 << 4 != 48) return 3 << 4;

  puts ("t: 48 >> 3\n");
  if (48 >> 4 != 3) return 48 >> 4;

  puts ("t: 10 >> 1\n");
  if (10 >> 1 != 5) return 10 >> 1;

  puts ("t: 1 | 4\n");
  if ((1 | 4) != 5) return 1 | 4;

  i = -3;
  puts ("t: -i\n");
  if (-i != 3) return 1;

  puts ("t: -1 + 2\n");
  if (-1 + 2 != 1) return 1;

  return read_test ();
}

SCM
alloc (int n)
{
  SCM x = g_free;
  g_free += n;
  return x;
}

SCM
make_cell (SCM type, SCM car, SCM cdr)
{
  SCM x = alloc (1);
  TYPE (x) = VALUE (type);
  if (VALUE (type) == TCHAR || VALUE (type) == TNUMBER) {
    if (car) CAR (x) = CAR (car);
    if (cdr) CDR(x) = CDR(cdr);
  }
  else if (VALUE (type) == TFUNCTION) {
    if (car) CAR (x) = car;
    if (cdr) CDR(x) = CDR(cdr);
  }
  else {
    CAR (x) = car;
    CDR(x) = cdr;
  }
  return x;
}

SCM
make_cell_test ()
{
  VALUE (tmp_num) = TPAIR;
  make_cell (tmp_num, 0, 1);
  return math_test ();
}

SCM
make_tmps_test (struct scm* cells)
{
  puts ("t: tmp = g_free++\n");
  tmp = g_free++;
  puts ("t: cells[tmp].type = CHAR\n");
  cells[tmp].type = TCHAR;
  tmp_num = g_free++;
  cells[tmp_num].type = TNUMBER;

  return make_cell_test();
}

int
struct_test ()
{
  puts ("t: g_cells[0] = g_cells[1]\n");
  TYPE (1) = 1;
  CAR (1) = 2;
  CDR (1) = 3;
  g_cells[0] = g_cells[1];
  if (TYPE (0) != 1) return 1;
  if (CAR (0) != 2) return 2;
  if (CDR (0) != 3) return 3;

  puts ("t: g_cells[i] = g_cells[j]\n");
  int i = 0;
  int j = 1;
  TYPE (1) = 4;
  CAR (1) = 5;
  CDR (1) = 6;
  g_cells[i] = g_cells[j];
  if (TYPE (0) != 4) return 1;
  if (CAR (0) != 5) return 2;
  if (CDR (0) != 6) return 3;

  puts ("t: g_cells[0+add(0,0] = g_cells[0+inc(0)]\n");
  TYPE (1) = 1;
  CAR (1) = 2;
  CDR (1) = 3;
  g_cells[0+add(0, 0)] = g_cells[0+inc(0)];
  if (TYPE (0) != 1) return 1;
  if (CAR (0) != 2) return 2;
  if (CDR (0) != 3) return 3;

  g_cells[0].type = TNUMBER;
  g_cells[0].car = 0;
  g_cells[0].cdr = 0;
  g_cells[1].type = TNUMBER;
  g_cells[1].car = 0;
  g_cells[1].cdr = 0;

  puts ("t: TYPE (0) != TYPE (1)\n");
  if (TYPE (0) == TYPE (1)) goto ok;
  return 1;
 ok:

  g_cells[0].car = 1;
  g_cells[1].car = 2;

  puts ("t: int c = VALUE (0)\n");
  int c = CAR (0);
  if (c != 1) return 1;

  puts ("t: CAAR (0) != 2\n");
  if (CAAR (0) != 2) return 1;

  puts ("t: 2 != CAAR (0)\n");
  if (2 != CAAR (0)) return 1;

  g_cells[3].type = 0x64;
  if (g_cells[3].type != 0x64)
    return g_cells[3].type;

  TYPE (4) = 4;
  if (TYPE (4) != 4)
    return 4;
  
  CDR (3) = 0x22;
  CDR (4) = 0x23;
  if (CDR (3) != 0x22)
    return CDR (3);

  puts ("t: g_fun.arity != 1;\n");
  if (g_fun.arity != 1) return 1;

  puts ("t: g_fun.function != exit;\n");
  if (g_fun.function != &exit) return 1;

  puts ("t: struct fun = {&exit,1,\"exit\"};\n");
  struct function fun = {&exit,1,"exit"};

  puts ("t: fun.arity != 1;\n");
  if (fun.arity != 1) return 1;

  puts ("t: fun.function != exit;\n");
  if (fun.function != &exit) return 1;

  puts ("t: puts (fun.name)\n");
  if (strcmp (fun.name, "exit")) return 1;

  puts ("t: puts (g_fun.name)\n");
  if (strcmp (g_fun.name, "fun")) return 1;

  puts ("t: g_functions[g_function++] = g_foo;\n");
  g_functions[g_function++] = g_foo;

  int fn = 0;
  puts ("t: g_functions[g_cells[fn].cdr].arity\n");
  if (g_functions[g_cells[fn].cdr].arity) return 1;
  if (g_functions[g_cells[fn].cdr].arity != 0) return 1;

  int (*functionx) (void) = 0;
  functionx = g_functions[0].function;
  puts ("t: functionx == foo\n");
  if (functionx != foo) return 11;

  puts ("t: g_functions[0].name\n");
  if (strcmp (g_functions[0].name, "foo")) return 1;

  puts ("t: (functionx) () == foo\n");
  if ((functionx) () != 0) return 12;

  puts ("t: g_functions[<foo>].arity\n");
  if (g_functions[0].arity != 0) return 17;

  fn++;
  g_functions[fn] = g_bar;
  g_cells[fn].cdr = fn;
  if (g_cells[fn].cdr != fn) return 13;

  puts ("t: g_functions[g_cells[fn].cdr].function\n");
  functionx = g_functions[g_cells[fn].cdr].function;

  puts ("t: g_functions[1].name\n");
  if (strcmp (g_functions[1].name, "bar")) return 1;

  puts ("t: functionx == bar\n");
  if (functionx != bar) return 15;

  puts ("t: (functiony) (1) == bar\n");
  int (*functiony) (int) = 0;
  functiony = g_functions[g_cells[fn].cdr].function;
  if ((functiony) (1) != 0) return 16;

  puts ("t: g_functions[<bar>].arity\n");
  if (g_functions[fn].arity != 1) return 18;

  // fake name
  scm_fun.car = 33;
  scm_fun.cdr = g_function;
  //g_functions[g_function++] = g_fun;
  g_functions[g_function] = g_fun;
  cell_fun = g_free++;
  g_cells[cell_fun] = scm_fun;

  puts ("t: TYPE (cell_fun)\n");
  if (TYPE (cell_fun) != TFUNCTION) return 1;

  puts ("t: CAR (cell_fun)\n");
  if (CAR (cell_fun) != 33) return 1;

  puts ("t: CDR (cell_fun)\n");
  if (CDR (cell_fun) != g_function) return 1;

  return make_tmps_test  (g_cells);
}

int
test (char *p)
{
  int f = 0;
  int t = 1;
  int one = 1;
  char c = 'C';
  int i=0;

  char *x = arena;
  char *y = g_chars;

  puts ("t: for (i=1; i<5; ++i)\n");
  for (i=1; i<5; ++i);
  if (i != 5) return i;

  puts ("t: while (i<3) i++\n");
  i = 1;
  while (i<3) i++;
  if (i != 3) return i;

  puts ("t: do i-- while (i>0)\n");
  do i--; while (i>0);
  if (i != 0) return 1;

  puts ("t: if (0)\n");
  if (0) return 1;

  if (i)
    return 1;
  else
    puts ("t: else 1\n");

  if (i)
    puts ("0");
  else if (i == 1)
    puts ("1");
  else
    puts ("t: else if 2\n");

  puts ("t: if (f)\n");
  if (f) return 1;

  puts ("t: if (one != 1)\n");
  if (one != 1) return 1;

  puts ("t: if (1 != one)\n");
  if (1 != one) return 1;

  puts ("t: if (one > 1)\n");
  if (one > 1) return 1;

  puts ("t: if (one < 0)\n");
  if (one < 0) return 1;

  puts ("t: if (one <= 0)\n");
  if (one <= 0) return 1;

  puts ("t: if (one >= 2)\n");
  if (one >= 2) return 1;

  puts ("t: if (strlen (\"\"))\n");
  if (strlen ("")) return 1;

  puts ("t: if (strlen (p) != 4)\n");
  if (strlen (p) != 4) return 1;

  puts ("t: if (!strlen (\".\"))\n");
  if (!strlen (".")) return 1;

  puts ("t: if (strcmp (p, \"foo\"))\n");
  if (!strcmp (p, "foo")) return 1;

  puts ("t: if (strcmp (p, \"t.c\\n\"))\n");
  if (strcmp (p, "t.c\n")) return 1;

  puts ("t: if (!1)\n");
  if (!1) return 1;

  puts ("t: if (one == 0)\n");
  if (one == 0) return 1;

  puts ("t: if (f != 0)\n");
  if (one != 1) return 1;

  puts ("t: if (1 && 0)\n");
  if (1 && 0) return 1;

  puts ("t: if (!t && f)\n");
  if (!t && f) return 1;

  puts ("t: if (t && !one)\n");
  if (t && !one) return 1;

  puts ("t: if (f || !t)\n");
  if (f || !t) return 1;

  puts ("t: if (i++)\n");
  if (i++) return 1;

  puts ("t: if (--i)\n");
  if (--i) return 1;

  puts ("t: i += 2\n");
  i += 2;
  if (i != 2) return 1;

  puts ("t: i -= 2\n");
  i -= 2;
  if (i != 0) return 1;

  puts ("t: if (f = 0) ?\n");
  if (f = 0) return 1;

  puts ("t: if (!(t = 1)) ?\n");
  if (!(t = 1)) return 1;

  puts ("t: if ((f = 0) != 0) ?\n");
  if ((f = 0) != 0) return 1;

  puts ("t: if ((t = 1) != 1) ?\n");
  if ((t = 1) != 1) return 1;

  puts ("t: (one == 1) ?\n");
  (one == 1) ? 1 : exit (1);

  puts ("t: (f) ?\n");
  (f) ? exit (1) : 1;

  puts ("t: assert (1) ?\n");
  assert (1);

  puts ("t: assert (f==0) ?\n");
  assert (f==0);

  puts ("t: p[0] != 't'\n");
  if (p[0] != 't') return p[0];

  puts ("t: p[i] != 't'\n");
  if (p[i] != 't') return p[i];

  puts ("t: identity (p[i]) != 't'\n");
  if (identity (p[i]) != 't') return identity (p[i]);

  puts ("t: *g_chars != 'A'\n");
  arena[0] = 'A';
  if (*g_chars != 'A') return 1;

  puts ("t: *x != 'A'\n");
  if (*x != 'A') return 1;

  puts ("t: *y != 'A'\n");
  if (*y != 'A') return 1;

  puts ("t: *x != 'Q'\n");
  g_chars[0] = 'Q';
  if (*x != 'Q') return 1;

  puts ("t: *x++ != 'C'\n");
  *x++ = c;
  if (*g_chars != 'C') return 1;

  puts ("t: 1 + 2\n");
  if (1 + 2 != 3) return 1;

  puts ("t: 2 - 1\n");
  if (2 - 1 != 1) return 1;

  puts ("t: 1 << 3\n");
  if (1 << 3 != 8) return 1;

  puts ("t: 8 >> 3\n");
  if (8 >> 3 != 1) return 1;

  puts ("t: 8 / 4\n");
  if (8 / 4 != 2) return 1;

  puts ("t: inc (0)\n");
  if (inc (0) != 1) return 1;

  puts ("t: inc (inc (0))\n");
  if (inc (inc (0)) != 2) return 1;

  puts ("t: inc (inc (inc (0)))\n");
  if (inc (inc (inc (0))) != 3) return 1;

  puts ("t: add (1, 2)\n");
  if (add (1, 2) != 3) return 1;

  puts ("t: add (inc (0), inc (1))\n");
  if (add (inc (0), inc (1)) != 3) return 1;

  puts ("t: add (TSTRING, 3)\n");
  if (add (TSTRING, 3) != 13) return 1;

  puts ("t: add (inc (inc (0)), inc (inc (1)))\n");
  if (add (inc (inc (0)), inc (inc (1))) != 5) return 1;

  puts ("t: goto label\n");
  if (label (1) != 0) return 1;

  puts ("t: switch 0\n");
  if (swits (0) != 0) return swits (0);

  puts ("t: switch 1\n");
  if (swits (1) != 1) return 1;

  puts ("t: switch -1\n");
  if (swits (-1) != 2) return 1;

  puts ("t: if (1)\n");
  if (1) goto ok0;
  return 1;
 ok0:
  
  puts ("t: while (1) { goto label; };\n");
  while (1) {
    goto ok00;
  }
 ok00:

  puts ("t: if (0); return 1; else;\n");
  if (0) return 1; else goto ok01;
 ok01:

  puts ("t: if (t)\n");
  if (t) goto ok1;
  return 1;
 ok1:

  puts ("t: if (one > 0)\n");
  if (one > 0) goto ok2;
  return 1;
 ok2:

  puts ("t: if (one < 2)\n");
  if (one < 2) goto ok3;
  return 1;
 ok3:

  puts ("t: if (one >= 0)\n");
  if (one >= 0) goto ok30;
  return 1;
 ok30:

  puts ("t: if (one >= 1)\n");
  if (one >= 0) goto ok31;
  return 1;
 ok31:

  puts ("t: if (one <= 2)\n");
  if (one <= 2) goto ok32;
  return 1;
 ok32:

  puts ("t: if (one <= 1)\n");
  if (one <= 1) goto ok33;
  return 1;
 ok33:

  puts ("t: if (strlen (\".\"))\n");
  if (strlen (".")) goto ok4;
  return 1;
 ok4:

  puts ("t: if (strlen (p) == 4)\n");
  if (strlen (p) == 4) goto ok40;
 ok40:

  puts ("t: if (!strcmp (p, \"t.c\\n\"))\n");
  if (!strcmp (p, "t.c\n")) goto ok41;
  return 1;
 ok41:

  puts ("t: if (strcmp (p, \"foo\"))\n");
  if (strcmp (p, "foo")) goto ok42;
  return 1;
 ok42:

  puts ("t: if (!0)\n");
  if (!0) goto ok5;
  return 1;
 ok5:

  puts ("t: if (one == 1)\n");
  if (one == 1) goto ok6;
  return 1;
 ok6:

  puts ("t: if (one != 0)\n");
  if (one != 0) goto ok7;
  return 1;
 ok7:

  puts ("t: if (1 && !0)\n");
  if (1 && !0) goto ok8;
  return 1;
 ok8:

  puts ("t: if (f || t)\n");
  if (f || t) goto ok80;
  return 1;
 ok80:

  puts ("t: if (++i)\n");
  if (++i) goto ok9;
  return 1;
 ok9:

  puts ("t: if (i--)\n");
  if (i--) goto ok10;
  return 1;
 ok10:

  puts ("t: *g_chars == 'B'\n");
  arena[0] = 'B';
  if (*g_chars == 'B') goto ok11;
  return 1;
  ok11:

  puts ("t: *x == 'B'\n");
  x = arena;
  if (*x == 'B') goto ok12;
  return 1;
 ok12:

  puts ("t: *y == 'B'\n");
  y = g_chars;
  if (*y == 'B') goto ok13;
  return 1;
 ok13:

  puts ("t: *x == 'R'\n");
  g_chars[0] = 'R';
  if (*x == 'R') goto ok14;
  return 1;
 ok14:

  puts ("t: *x++ == 'C'\n");
  *x++ = c;
  if (*g_chars == 'C') goto ok15;
  return 1;
 ok15:

  puts ("t: itoa (33) == \"33\"\n");
  if (strcmp (itoa (33), "33")) return 1;

  return struct_test ();
}
#endif

int
main (int argc, char *argv[])
{
  char *p = "t.c\n";
  puts ("t.c\n");

  if (argc > 1 && !strcmp (argv[1], "--help")) return 1;
  puts ("t: if (argc > 1 && !strcmp (argv[1], \"--help\")\n");

  // FIXME mescc?!
  if (argc > 1) if (!strcmp (argv[1], "--help")) return 1;

  return test (p);

  return 22;
}

#if __GNUC__
#include "mstart.c"
#endif
