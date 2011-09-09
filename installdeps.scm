#!/usr/local/bin/csi -s
(define deps
  '(highlighting))

(define depscmd 
  (string-intersperse
    `("cabal-dev install --only-dependencies"
      ,@(map (lambda (sym) (format "--flag \"~A\"" sym)) deps))))

(system depscmd)
