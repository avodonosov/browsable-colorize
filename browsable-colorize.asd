;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;;
;;;; Copyright (C) 2023 Anton Vodonosov (avodonosov@yandex.ru)

(defsystem "browsable-colorize"
  :description "Helps to turn your package.lisp into an HTML API reference, by generating a colorized version of it, with every symbol being a link to its definition at github. Uses swank/backend:find-definitions and colorize."
  :license "MIT"
  :author "Anton Vodonosov (avodonosov@yandex.ru)"
  :depends-on ("colorize" "swank")
  :serial t
  :components ((:file "browsable-colorize")
               (:file "css")))
