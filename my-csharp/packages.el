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
      prodigy
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
  (use-package csharp-mode :defer t))

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

(defun my-csharp/init-prodigy()
  (use-package prodigy
    :defer nil
    :config
    (def-omnisharp-service
      "omnisharp-roslyn stdio"
      "../../../../code/omnisharp-roslyn/scripts/Omnisharp"
      '("-v" "-s" "test/MinimalSolution/" "--stdio"))

    (def-omnisharp-service
      "omnisharp-emacs integration tests"
      "run-integration-tests.sh")

    (def-omnisharp-service
      "omnisharp-emacs unit tests"
      "run-tests.sh")

    (def-omnisharp-service
      "omnisharp-emacs installation test"
      "run-melpa-build-test.sh")))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun my-csharp/post-init-company ()
    (spacemacs|add-company-hook csharp-mode)))

(defmacro def-omnisharp-service (name command &optional args-to-command)
  (let ((omni-dir "~/.spacemacs.layers/my-csharp/extensions/omnisharp-emacs/"))
    `(prodigy-define-service
       :name ,name
       :args ,args-to-command
       :command (concat ,omni-dir ,command)
       :cwd ,omni-dir
       :stop-signal 'kill
       :kill-process-buffer-on-stop t
       :truncate-output 200
       :tags '(omnisharp))))
