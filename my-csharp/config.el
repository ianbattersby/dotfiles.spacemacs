;;; config.el --- my-csharp Layer config File for Spacemacs
;;
;; Copyright (c) 2015 Ian Battersby
;;
;; Author: Ian Battersby <ian.battersby@gmail.com>
;; URL: https://github.com/ianbattersby/spacemacs.layers
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|defvar-company-backends csharp-mode)

(defvar omnisharp-load-script (concat omnisharp-load-script "/omnisharp-roslyn/scripts/Omnisharp")
  "Location of the omnisharp-roslyn load script.")

(defvar omnisharp-emacs-repo-path (concatenate 'string (expand-file-name (car dotspacemacs-configuration-layer-path)) "/my-csharp/extensions/omnisharp-emacs/")
  "Location of the omnisharp-emacs repository.")

;; This is a really bad idea but in verbose mode the responses can be huge :'(
(defvar omnisharp--temporary-buffer nil
  "Temporary buffer for when responses are split.")
