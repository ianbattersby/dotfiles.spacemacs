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

(defun omnisharp-start-prodigy-server (path-to-solution)
  "Starts an OmniSharpServer for a given path to a solution file or a directory via prodigy"
  (interactive
   (list
    (-let [(directory filename . rest) (omnisharp--find-solution-files)]
      (read-file-name "Start omnisharp for solution: "
                      directory
                      nil
                      t
                      filename))))
    (if (equal nil omnisharp-server-executable-path)
      (error "Could not find the omnisharp server. Please set the variable omnisharp-server-executable-path to a valid path")
    (if (omnisharp--valid-solution-path-p path-to-solution)
        (let ((solution (expand-file-name path-to-solution)))
          (let ((solution-dir (file-name-directory solution))
                (solution-comp (omnisharp--longest-common-starting-substring default-directory solution)))
            (let ((service-name (concat "[:omnisharp] " (substring solution (length solution-comp))))
                  (services (--map (mapconcat 'identity (plist-get it ':args) " ") (--filter (plist-member it ':args) (prodigy-services)))))
             (if (= 0 (--count (string-match (concat "-s " solution-dir) it) services))
               (progn
                 (prodigy-define-service
                   :name service-name
                   :cwd solution-dir
                   :args `("-s" ,solution "--stdio")
                   :tags '(omnisharp))
                 (prodigy-start-service (prodigy-find-service service-name) `(lambda () (message (concat "Omnisharp started for " ,service-name)))))
             (message "Prodigy service already defined for specified path."))))))))

(defun omnisharp--longest-common-starting-substring (&rest strings)
  "Find any common prefix between parameter of strings."
  (loop for i from 0 below (apply #'min (mapcar #'length strings))
        while (apply #'char-equal
                     (mapcar (lambda (string) (aref string i))
                             strings))
        finally (return (subseq (first strings) 0 i))))
