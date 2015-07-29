;;; extensions.el --- my-csharp Layer extensions File for Spacemacs
;;
;; Copyright (c) 2015 Ian Battersby
;;
;; Author: Ian Battersby <ian.battersby@gmail.com>
;; URL: https://github.com/ianbattersby/spacemacs.layers
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq my-csharp-pre-extensions
      '(
        ;; pre extension names go here
        ))

(setq my-csharp-post-extensions
      '(
        ;; post extension names go here
        omnisharp-emacs
        ))

;; For each extension, define a function my-csharp/init-<extension-name>
;;
;; (defun my-csharp/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun my-csharp/init-omnisharp-emacs()
  (defun omnisharp-emacs/init-omnisharp-emacs ()
    "Initialize my extension"
    (require 'omnisharp)
    (setq omnisharp-server-executable-path
          "~/code/omnisharp-roslyn/scripts/Omnisharp")
    (setq omnisharp-debug 't)
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-omnisharp))))
