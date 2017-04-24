;; start emacs server

(require 'server)
(unless (server-running-p)
  (server-start))

;; recompile configs
(defconst djg/init-dir (concat user-emacs-directory "init.d"))
(add-hook 'kill-emacs-hook
          (lambda ()
            (byte-recompile-directory djg/init-dir 0 t)))

;; packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defconst emacs-major-version-rad 1000000)

(defun has-emacs-version (major minor)
  (<= (+ (* major emacs-major-version-rad) minor)
      (+ (* emacs-major-version emacs-major-version-rad) emacs-minor-version)))

(if (not (has-emacs-version 24 0))
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(eval-when-compile (package-initialize))

(defun require-package (package)
  "refresh package archives, check package presence and install if it's not installed"
  (if (null (require package nil t))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package)))
(require package))))

;; use package
(require-package 'use-package)
(require 'use-package)

;; req-package
(require-package 'req-package)
(require 'req-package)
(req-package--log-set-level 'trace)

;; lisp
(random t)
(req-package load-dir
  :force true
  :init
  (setq force-load-messages nil
	load-dir-debug nil
	load-dir-recursive t)
  :config
  (load-dir-one djg/init-dir)
  (req-package-finish))
