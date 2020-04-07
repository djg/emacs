;; djg
(defun djg/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun djg/select-line ()
  "If the mark is not active, select the current line.
Otherwise, expand the current region to select the lines the region touches."
  (interactive)
  (if mark-active ;; expand the selection to select lines
      (let ((top (= (point) (region-beginning)))
            (p1 (region-beginning))
            (p2 (region-end)))
        (goto-char p1)
        (beginning-of-line)
        (push-mark (point))
        (goto-char p2)
        (unless (looking-back "\n")
          (progn
            (end-of-line)
            (if (< (point) (point-max)) (forward-char))))
        (setq mark-active t
              transient-mark-mode t)
        (if top (exchange-point-and-mark)))
    (progn
      (beginning-of-line)
      (push-mark (point))
      (end-of-line)
      (if (< (point) (point-max)) (forward-char))
      (setq mark-active t
            transient-mark-mode t))))

(defun djg/open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun djg/open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;; I'm always opening a buffer in the wrong window. These function
;; help move the newly opened buffer to another window and restore the
;; buffer I was previously looking at.
(defun djg/buf-move (dir)
  "Move the current buffer in direction dir and replace with previous buffer.
If there is no split, ie no window in the direction requested, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window dir))
         (buf-this-buf (window-buffer (selected-window)))
         (errmsg (format "cannot move buffer %s" (symbol-name dir))))
    (if (null other-win)
        (error errmsg)
      (set-window-buffer other-win buf-this-buf)
      (switch-to-prev-buffer))))

(defun djg/find-other-file-candidate (base exts &optional opt_dirs)
  (let ((dirs (or opt_dirs '(".")))
        file-names)
    (dolist (dir dirs)
      (dolist (ext exts)
        (let ((file (concat (file-name-as-directory dir) base ext)))
          (when (file-exists-p file) (push file file-names)))))
    (car file-names)))

(defun djg/find-other-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (let ((other-file-name nil)
        (base-file-name (file-name-base buffer-file-name))
        (dir-name (file-name-directory buffer-file-name))
        (ext-name (file-name-extension buffer-file-name)))
    (cond
     ((string= ext-name "cpp")
      (setq other-file-name (djg/find-other-file-candidate base-file-name '(".h") '("." "../include"))))
     ((string= ext-name "h")
      (setq other-file-name (djg/find-other-file-candidate base-file-name '(".cpp" ".c") '("." "../src")))))
    (if other-file-name
        (find-file other-file-name)
      (error "Unable to find other file."))))

(defun djg/find-other-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (djg/find-other-file))

(make-variable-buffer-local
 (defvar djg/untabify-on-write t
   "If the buffer should be untabified on write."))

(defun djg/untabify-buffer ()
  (if djg/untabify-on-write
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" nil t)
          (delete-region (match-beginning 0) (match-end 0)))
        (goto-char (point-min))
        (if (search-forward "\t" nil t)
            (untabify (1- (point)) (point-max)))))
  nil)

(defun djg/irony-mode-init ()
  (bind-key [remap completion-at-point] 'irony-completion-at-point-async irony-mode-map)
  (bind-key [remap complete-symbol] 'irony-completion-at-point-async irony-mode-map))

(eval-when-compile
  (require 'browse-url)
  (require 'vc))

;;; http://searchfox.org/mozilla-central/source/dom/media/AccurateSeekTask.cpp#197
(defvar djg/searchfox-server "http://searchfox.org/"
  "The search fox server to use.
This must end in a `/'.")

(defvar djg/searchfox-tree "mozilla-central"
  "The mozilla tree to use.")

(defun djg/searchfox-url-representing-point ()
  (unless (buffer-file-name)
    (error "Buffer is not visiting a file"))
  (let ((root (or (car (projectile-get-project-directories))
		  (error "Could not find VC root directory"))))
    (concat djg/searchfox-server
	    djg/searchfox-tree
	    "/source/"
	    (file-relative-name (buffer-file-name) root)
	    "#"
	    (int-to-string (line-number-at-pos)))))

;;;###autoload
(defun djg/searchfox-browse-url ()
  "Open a searchfox page for the source at point in a web browser.
This uses `djg/searchfox-server' and `djg/searchfox-tree' to compute the URL, and `browse-url'
to open the page in the browser."
  (interactive)
  (browse-url (djg/searchfox-url-representing-point)))

;;;###autoload
(defun djg/searchfox-kill-ring-save ()
  "Save a DXR URL for the source at point in the kill ring.
This uses `djg/searchfox-server' and `djg/searchfox-tree' to compute the URL."
  (interactive)
  (kill-new (djg/searchfox-url-representing-point)))

;;;###autoload
(defun djg/yank-append-lines (&optional without-space)
  "Yank each line of the current kill at the end of each subsequent line.

A space will be added between each line unless WITHOUT-SPACE which can be passed in via a prefix arg."
  (interactive "P")
  (save-excursion
    (let ((lines (split-string (current-kill 0) "\n")))
      (dolist (line lines)
        (goto-char (line-end-position))
        (unless without-space
          (just-one-space))
        (insert line)
        (unless (zerop (forward-line))
          (insert "\n"))))))

(provide 'djg)
