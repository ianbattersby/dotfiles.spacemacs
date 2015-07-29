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
        omnisharp-dev
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

(defun my-csharp/init-omnisharp-dev()
  (use-package omnisharp
    :commands omnisharp-dev-start
    :config
      (setq omnisharp-debug t)
      (setq omnisharp-server-executable-path
          "/home/ian/code/omnisharp-roslyn/scripts/Omnisharp"))
      ;;(load-file "/home/ian/code/omnisharp-emacs/test/buttercup-tests/setup.el"))
    :init
      (add-to-list 'load-path "/home/ian/code/omnisharp-emacs/")
      (require 'omnisharp)
      (use-omnisharp-package-for-development))
