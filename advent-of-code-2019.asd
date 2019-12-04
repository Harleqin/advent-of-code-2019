(in-package #:asdf-user)

(defsystem "advent-of-code-2019"
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :license "public domain/CC0"
  :depends-on ("alexandria"
               "arrows"
               "cl-ppcre"
               "for"
               "let-plus"
               "split-sequence")
  :serial t
  :components ((:file "base")
               (:file "intcode")
               (:file "1")
               (:file "2")
               (:file "3")
               (:file "4")))
