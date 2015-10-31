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
      ;; needed for core operation
      company
;;      shut-up
      prodigy
      csharp-mode

      ;; needed for development mode
      f
      s
      el-mock
      buttercup
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

;;(defun my-csharp/init-shut-up()
;;   (use-package shut-up :defer t))

(defun my-csharp/init-f()
  (use-package f))

(defun my-csharp/init-s()
  (use-package s))

(defun my-csharp/init-el-mock()
  (use-package el-mock))

(defun my-csharp/init-buttercup()
  (use-package buttercup))

(defun my-csharp/post-init-prodigy()
  (use-package prodigy
    :defer nil
    :config
    (prodigy-define-service
       :name "omnisharp-roslyn stdio"
       :args '("-v" "-s" "test/MinimalSolution/" "--stdio")
       :command (expand-file-name omnisharp-load-script)
       :cwd omnisharp-emacs-repo-path
       :stop-signal 'kill
       :kill-process-buffer-on-stop t
       :truncate-output 200
       :tags '(omnisharp)
       :ready-message "FALSE_FIND"
       :on-output (lambda (&rest args)
                    (let ((output (plist-get args :output))
                          (service (plist-get args :service)))
                      (when (and (not (eq (plist-get service :status) 'ready)) (s-matches? "\"Event\":\"started\"" output))
                        (prodigy-set-status service 'ready)
                        (princ 'Omnisharp-roslyn\ stdio\ \(prodigy\)\ has\ started\.)
                        (let ((process (plist-get service :process)))
                          (omnisharp--handle-server-message process output)
                          (set-process-filter process 'omnisharp--handle-server-message)
                          (setq omnisharp--server-info (make-omnisharp--server-info process)))))))

    (prodigy-define-service
      :name "omnisharp-emacs integration tests"
      :command (lambda (&rest args)
                 (let ((service (plist-get args :server))
                       (process (get-process "omnisharp-roslyn stdio")))
                   (load (f-join omnisharp-emacs-repo-path "test" "buttercup-tests" "setup.el") nil t)
                   (setq omnisharp--server-info (make-omnisharp--server-info process))
                   (dolist (file (f-entries (f-join omnisharp-emacs-repo-path "test" "buttercup-tests") (lambda (file) (s-matches? "-test.el" file)) t))
                     (load file nil t))
                   (buttercup-run)))
      :cwd (f-join omnisharp-emacs-repo-path "test" "buttercup-tests")
      :tags '(omnisharp))

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
  (let ((omni-dir omnisharp-emacs-repo-path))
    `(prodigy-define-service
       :name ,name
       :args ,args-to-command
       :command (concat ,omni-dir ,command)
       :cwd ,omni-dir
       :stop-signal 'kill
       :kill-process-buffer-on-stop t
       :truncate-output 200
       :tags '(omnisharp))))
