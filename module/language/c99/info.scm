;;; -*-scheme-*-

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Mes.
;;;
;;; Mes is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Mes is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Mes.  If not, see <http://www.gnu.org/licenses/>.

;;; info.scm defines [Guile] record data types for compiler.mes

;;; Code:

(define-module (language c99 info)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export (<info>
            make
            make-<info>
            info?

            .types
            .constants
            .functions
            .globals
            .locals
            .function
            .statics
            .text
            .break
            .continue

            <type>
            make-type
            type?
            type:type
            type:size
            type:pointer
            type:description

            <global>
            make-global
            global?
            global:name
            global:type
            global:pointer
            global:array
            global:value
            global:function
            global->string

            <local>
            make-local
            local?
            local:type
            local:pointer
            local:array
            local:id

            <function>
            make-function
            function?
            function:name
            function:type
            function:text))

(cond-expand
 (guile-2)
 (guile
  (use-modules (ice-9 syncase))
  (use-modules (ice-9 optargs)))
 (mes
  (mes-use-module (mes optargs))))

(define-immutable-record-type <info>
  (make-<info> types constants functions globals locals statics function text break continue)
  info?
  (types .types)
  (constants .constants)
  (functions .functions)
  (globals .globals)
  (locals .locals)
  (statics .statics)
  (function .function)
  (text .text)
  (break .break)
  (continue .continue))

(define* (make o #:key (types '()) (constants '()) (functions '()) (globals '()) (locals '()) (statics '()) (function #f) (text '()) (break '()) (continue '()))
  (make-<info> types constants functions globals locals statics function text break continue))

(define-immutable-record-type <type>
  (make-type type size pointer description)
  type?
  (type type:type)
  (size type:size)
  (pointer type:pointer)
  (description type:description))

(define-immutable-record-type <global>
  (make-global name type pointer array value function)
  global?
  (name global:name)
  (type global:type)
  (pointer global:pointer)
  (array global:array)
  (value global:value)
  (function global:function))

(define (global->string o)
  (or (and=> (global:function o) (cut string-append <> "-" (global:name o)))
      (global:name o)))

(define-immutable-record-type <local>
  (make-local type pointer array id)
  local?
  (type local:type)
  (pointer local:pointer)
  (array local:array)
  (id local:id))

(define-immutable-record-type <function>
  (make-function name type text)
  function?
  (name function:name)
  (type function:type)
  (text function:text))
