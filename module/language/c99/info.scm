;;; -*-scheme-*-

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017 Jan Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (system base pmatch)
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
            .text
            .break
            .continue

            make-type
            type?
            type:type
            type:size
            type:pointer
            type:description

            make-global
            global?
            global:type
            global:pointer
            global:value))

(cond-expand
 (guile-2)
 (guile
  (use-modules (ice-9 syncase)))
 (mes))

(define-immutable-record-type <info>
  (make-<info> types constants functions globals locals function text break continue)
  info?
  (types .types)
  (constants .constants)
  (functions .functions)
  (globals .globals)
  (locals .locals)
  (function .function)
  (text .text)
  (break .break)
  (continue .continue))

(define* (make o #:key (types '()) (constants '()) (functions '()) (globals '()) (locals '()) (function #f) (text '()) (break '()) (continue '()))
  (make-<info> types constants functions globals locals function text break continue))

(define-immutable-record-type <type>
  (make-type type size pointer description)
  type?
  (type type:type)
  (size type:size)
  (pointer type:pointer)
  (description type:description))

(define-immutable-record-type <global>
  (make-global type pointer value)
  global?
  (type global:type)
  (pointer global:pointer)
  (value global:value))
