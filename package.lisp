;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2009 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage :qt
  (:use :cl :iterate)
  (:export #:*load-library-function*
           #:load-library
           #:ensure-smoke
           #:qapropos
           #:qdescribe
           #:*qapplication*
           #:*qapplication-create-hooks*
           #:make-qapplication
           #:interpret-call
           #:interpret-call-without-override
           #:optimized-call
           #:call-next-qmethod
           #:interpret-new
           #:optimized-new
           #:qt-class
           #:abstract-qobject
           #:null-qobject
           #:null-qobject-p
           #:qobject
           #:qsignal
           #:qslot
           #:find-qclass
           #:call-next-qmethod
           #:primitive-value
           #:enum-value
           #:emit-signal
           #:qclass-name
           #:list-qclass-superclasses
           #:qmethod-name
           #:dynamic-object
           #:list-qmethod-argument-types
           #:note-deleted
           #:interpret-delete
           #:optimized-delete
           #:find-method-override
           #:windows-version
           #:set-nice-theme
           #:rebirth
           #:with-objects
           #:cancel-finalization
           #:*report-memory-leaks*
           #:new
           #:enum=
           #:enum-or
           #:enum-andc
           #:qsubclassp
           #:qtypep
           #:connect
           #:disconnect
           #:with-signals-blocked
           #:stop-overriding
           #:cast
           #:qobject-deleted
           #:with-main-window
           #:enum-equal)
  (:import-from alexandria named-lambda))
