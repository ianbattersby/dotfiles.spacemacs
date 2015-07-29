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

(defun my-csharp/post-init-omnisharp-emacs()
 (evil-leader/set-key-for-mode 'csharp-mode
    ;; Compile
    "mcc" 'omnisharp-build-in-emacs ;; Only one compile command so use top-level
    ;; Solution/project manipulation
    "mfa" 'omnisharp-add-to-solution-current-file
    "mfA" 'omnisharp-add-to-solution-dired-selected-files
    "mfr" 'omnisharp-remove-from-project-current-file
    "mfR" 'omnisharp-remove-from-project-dired-selected-files
    "mpl" 'omnisharp-add-reference
    ;; Navigation
    "mgg" 'omnisharp-go-to-definition
    "mgG" 'omnisharp-go-to-definition-other-window
    "mgu" 'omnisharp-helm-find-usages
    "mgs" 'omnisharp-helm-find-symbols
    "mgi" 'omnisharp-find-implementations
    "mgr" 'omnisharp-navigate-to-region
    "mgm" 'omnisharp-navigate-to-solution-member
    "mgM" 'omnisharp-navigate-to-solution-member-other-window
    "mgf" 'omnisharp-navigate-to-solution-file
    "mgF" 'omnisharp-navigate-to-solution-file-then-file-member
    ;; Help, documentation, info
    "mht" 'omnisharp-current-type-information
    "mhT" 'omnisharp-current-type-information-to-kill-ring
    ;; Refactoring
    "mrm" 'omnisharp-rename
    "mrr" 'omnisharp-run-code-action-refactoring
    ;; Server manipulation, inspired spacemacs REPL bindings since C# does not provice a REPL
    "mss" 'omnisharp-start-omnisharp-server
    "msS" 'omnisharp-stop-server
    "msr" 'omnisharp-reload-solution
    ;; Tests
    "mta" 'omnisharp-unit-test-all
    "mtb" 'omnisharp-unit-test-fixture
    "mtt" 'omnisharp-unit-test-single
    ;; Code manipulation
    "mu" 'omnisharp-auto-complete-overrides
    "mi" 'omnisharp-fix-usings
    "m=" 'omnisharp-code-format))
