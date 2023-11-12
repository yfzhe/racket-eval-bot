;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((racket-mode
  (eval . (progn
            (put 'call-in-sandbox-context 'racket-indent-function 1)))))
