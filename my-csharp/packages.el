;;; packages.el --- my-csharp Layer extensions File for Spacemacs
;;
;; Copyright (c) 2015 Ian Battersby
;;
;; Author: Ian Battersby <ian.battersby@gmail.com>
;; URL: https://github.com/ianbattersby/spacemacs.layers
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-csharp-packages
    '(
      ;; package names go here
      flycheck
      company
      shut-up
      f
      s
      el-mock
      buttercup
      csharp-mode
      ))

;; List of packages to exclude.
(setq my-csharp-excluded-packages '())

;; For each package, define a function my-csharp/init-<package-name>
;;
;; (defun my-csharp/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun my-csharp/init-csharp-mode()
  (add-to-list 'load-path "/home/ian/code/omnisharp-emacs/")
  (require 'flycheck)
  (require 'omnisharp)
  (require 'company-omnisharp)
  (use-package csharp-mode :defer t))

(defun my-csharp/post-init-csharp-mode()
  (use-package omnisharp
    :defer t
    :init
      (push 'company-omnisharp company-backends-csharp-mode)
      (setq omnisharp-debug t)
      (setq omnisharp-server-executable-path
          "/home/ian/code/omnisharp-roslyn/scripts/Omnisharp")
    :config
      (add-hook 'csharp-mode-hook 'omnisharp-mode)
      (add-hook 'omnisharp-mode-hook '(lambda() (interactive) (load-file "/home/ian/code/omnisharp-emacs/test/buttercup-tests/setup.el")))
      (yas-minor-mode)
      (omnisharp-mode)
      (company-mode)
      (flycheck-mode)
      (linum-mode)
      ;; (whole-line-or-region-mode)
      ;; use flex matching for company
      (electric-pair-mode)
      (setq c-basic-offset 4) ; indents 4 chars
      (setq tab-width 4)          ; and 4 char wide for TAB
      (setq indent-tabs-mode nil) ; And force use of spaces
      (setq eldoc-idle-delay 0.1
            flycheck-display-errors-delay 0)
      (turn-on-eldoc-mode)
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
              "mgg"   'omnisharp-go-to-definition
              "mgG"   'omnisharp-go-to-definition-other-window
              "mgu"   'omnisharp-helm-find-usages
              "mgs"   'omnisharp-helm-find-symbols
              "mgi"   'omnisharp-find-implementations
              "mgr"   'omnisharp-navigate-to-region
              "mgm"   'omnisharp-navigate-to-solution-member
              "mgM"   'omnisharp-navigate-to-solution-member-other-window
              "mgf"   'omnisharp-navigate-to-solution-file
              "mgF"   'omnisharp-navigate-to-solution-file-then-file-member
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
              "m=" 'omnisharp-code-format)))

(defun my-csharp/init-shut-up()
  (use-package shut-up :defer t))

(defun my-csharp/init-f()
  (use-package f :defer t))

(defun my-csharp/init-s()
  (use-package s :defer t))

(defun my-csharp/init-el-mock()
  (use-package el-mock :defer t))

(defun my-csharp/init-buttercup()
  (use-package buttercup :defer t))

(defun my-csharp/init-flycheck()
  (use-package flycheck :defer t))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun my-csharp/post-init-company ()
    (spacemacs|add-company-hook csharp-mode)))
