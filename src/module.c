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

SCM
make_initial_module (SCM a)
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
  SCM globals = cell_nil;
  values = cons (a, values);
  values = cons (globals, values);
  values = cons (name, values);
  SCM module = make_struct (module_type_name, values, cell_unspecified);
  return module;
}

SCM
module_ref (SCM module, SCM name)
{
  SCM x = module_variable (module, name);
  if (x == cell_f)
    return cell_undefined;
  return CDR (x);
}

SCM
module_variable (SCM module, SCM name)
{
  //SCM locals = struct_ref (module, 4);
  SCM locals = module;
  return assq (name, locals);
}
