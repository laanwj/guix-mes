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

SCM make_vector__ (long k);
SCM struct_ref_ (SCM x, long i);
SCM struct_set_x_ (SCM x, long i, SCM e);
SCM vector_ref_ (SCM x, long i);
SCM vector_set_x_ (SCM x, long i, SCM e);

SCM
make_initial_module (SCM a) ///((internal))
{
  SCM fields = cell_nil;
  fields = cons (cstring_to_symbol ("globals"), fields);
  fields = cons (cstring_to_symbol ("locals"), fields);
  fields = cons (cstring_to_symbol ("name"), fields);
  fields = cons (cstring_to_symbol ("<module>"), fields);
  SCM module_type = make_struct (cstring_to_symbol ("record-type"), fields, cell_unspecified);
  SCM module_type_name = cstring_to_symbol ("<module>");
  a = acons (module_type_name, module_type, a);
  SCM values = cell_nil;
  SCM name = cons (cstring_to_symbol ("boot"), cell_nil);
  //SCM globals = make_vector__ (28 * 27);
  SCM globals = make_vector__ (30 * 27);
  values = cons (globals, values);
  SCM locals = cell_nil;
  values = cons (locals, values);
  values = cons (name, values);
  SCM module = make_struct (module_type_name, values, cell_module_printer);
  r0 = cell_nil;
  r0 = cons (CAR (a), r0);

  m0 = module;
  while (TYPE (a) == TPAIR)
    {
      if (g_debug > 3)
        {
          eputs ("entry="); display_error_ (CAR (a)); eputs ("\n");
        }
      module_define_x (module, CAAR (a), CDAR (a));
      a = CDR (a);
    }

  return module;
}

SCM
module_printer (SCM module)
{
  eputs ("#<"); display_error_ (struct_ref_ (module, 0)); eputc (' ');
  //eputs ("printer: "); display_error_ (struct_ref_ (module, 1)); eputc (' ');
  eputs ("name: "); display_error_ (struct_ref_ (module, 2)); eputc (' ');
  eputs ("locals: "); display_error_ (struct_ref_ (module, 3)); eputc (' ');
  eputs ("globals:\n  ");
  SCM v = struct_ref_ (m0, 4);
  for (int i=0; i<LENGTH (v); i++)
    {
      SCM e = vector_ref_ (v, i);
      if (e != cell_unspecified)
        {
          eputc ('[');
          while (TYPE (e) == TPAIR)
            {
              display_error_ (CAAR (e));
              e = CDR (e);
              if (TYPE (e) == TPAIR)
                eputc (' ');
            }
          eputs ("]\n  ");
        }
    }
  eputc ('>');
}



int
char_hash (int c)
{
  if (c >= 'a' && c <= 'z')
    return c - 'a';
  return 27;
}

int
module_hash (SCM x) ///((internal))
{
  int hash = char_hash (VALUE (CAR (STRING (x)))) * 27;
  if (TYPE (CDR (STRING (x))) == TPAIR)
    hash = hash + char_hash (VALUE (CADR (STRING (x))));
  else
    hash = hash + char_hash (0);
  assert (hash <= 756);
  return hash;
}

SCM
module_variable (SCM module, SCM name)
{
  //SCM locals = struct_ref_ (module, 3);
  SCM locals = module;
  SCM x = assq (name, locals);
  if (x == cell_f)
    {
      int hash = module_hash (name);
      module = m0;
      SCM globals = struct_ref_ (module, 4);
      SCM bucket = vector_ref_ (globals, hash);
      if (TYPE (bucket) == TPAIR)
        x = assq (name, bucket);
    }
  return x;
}

SCM
module_ref (SCM module, SCM name)
{
  if (g_debug > 4)
    {
      eputs ("module_ref: "); display_error_ (name); eputs ("\n");
    }
  SCM x = module_variable (module, name);
  if (x == cell_f)
    return cell_undefined;
  return CDR (x);
}

SCM
module_define_x (SCM module, SCM name, SCM value)
{
  if (g_debug > 4)
    {
      eputs ("module_define_x: "); display_error_ (name); eputs ("\n");
    }
  int hash = module_hash (name);
  module = m0;
  SCM globals = struct_ref_ (module, 4);
  SCM bucket = vector_ref_ (globals, hash);
  if (TYPE (bucket) != TPAIR)
    bucket = cell_nil;
  bucket = acons (name, value, bucket);
  vector_set_x_ (globals, hash, bucket);
  return cell_t;
}
