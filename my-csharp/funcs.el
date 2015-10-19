;;; funcs.el --- my-csharp Layer functions File for Spacemacs
;;
;; Copyright (c) 2015 Ian Battersby
;;
;; Author: Ian Battersby <ian.battersby@gmail.com>
;; URL: https://github.com/ianbattersby/spacemacs.layers
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defun use-omnisharp-package-for-development()
  (interactive)
  (load-file (concatenate 'string omnisharp-emacs-repo-path "test/buttercup-tests/setup.el")))
