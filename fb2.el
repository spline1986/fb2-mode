(require 'subr-x)
(require 'cl)

(defvar fb2-images-height 500
  "Height of images in fb2-mode buffer.")
(defvar fb2-show-images t
  "Show images in fb2-mode.")
(defvar fb2-replace-hard-space nil
  "Replace hard spaces by spaces in fb2-mode.")

(defun fb2-parse-p (book p &optional face)
  (if (not (member(first p) '(title image)))
      (dolist (subitem (cddr p))
	(if (stringp subitem)
	    (progn
	      (if fb2-replace-hard-space
		  (setq subitem (replace-regexp-in-string " " " " subitem)))
	      (if face
		  (insert (propertize (string-trim subitem) 'face face))
		(insert (string-trim subitem))))
	  (fb2-parse-p book subitem face))))
  (if (listp p)
      (if (member (first p) '(p text-author))
	  (insert "\n\n")
	(if (equal (first p) 'title)
	    (fb2-parse-p book (third p) '((:height 1.5))))
	(if (and fb2-show-images (equal (first p) 'image) (image-type-available-p 'imagemagick))
	    (progn
	      (insert-image (fb2-binary book (replace-regexp-in-string "#" "" (cdr (car (second p))))))
	      (insert "\n\n"))))))

(defun fb2-take-children (node sub)
  (if (listp node)
      (dolist (subitem node)
	(if (and (listp subitem) (equal (first subitem) sub))
	    (return subitem)))))

(defun fb2-description (node)
  (fb2-take-children node 'description))

(defun fb2-title-info (node)
  (fb2-take-children (fb2-description node) 'title-info))

(defun fb2-title (node)
  (fb2-take-children (fb2-title-info node) 'book-title))

(defun fb2-author (node)
  (let (author first last)
    (setq author (fb2-take-children (fb2-title-info node) 'author))
    (setq first (third (fb2-take-children author 'first-name)))
    (setq last (third (fb2-take-children author 'last-name)))
    (concat first " " last)))

(defun fb2-annotation (node)
  (let (annotation)
    (setq annotation (fb2-take-children (fb2-title-info node) 'annotation))
    (fb2-parse-p node annotation 'shadow)))

(defun fb2-body (node)
  (fb2-take-children node 'body))

(defun fb2-sections (node)
  (let (sections)
    (dolist (item (fb2-body node))
      (if (and (listp item) (equal (first item) 'section))
	  (push item sections)))
    (reverse sections)))

(defun fb2-binary (node id)
  (let (title type)
    (dolist (item node)
      (if (and (listp item) (equal (first item) 'binary))
	  (progn
	    (setq title (cdr (first (second item)))
		  type (cdr (second (second item))))
	    (if (equal id title)
		(progn
		  (if (member type '("image/jpeg" "image/png"))
		      (return (create-image (base64-decode-string (third item)) 'imagemagick t :height fb2-images-height :background "white"))))))))))

(defun fb2-read ()
  (let (book title cover filename)
    (setq book (libxml-parse-xml-region (point-min) (point-max))
	  filename buffer-file-name)
    (kill-buffer)
    (setq title (third (fb2-title book)))
    (get-buffer-create title)
    (switch-to-buffer title)
    (visual-line-mode)
    (setq mode-name "FB2-reader"
	  cover (fb2-take-children (fb2-title-info book) 'coverpage)
	  title (concat title "\n"))
    (fb2-parse-p book cover)
    (insert (propertize title 'face '((:height 2.0))))
    (insert (concat (fb2-author book) "\n\n"))
    (fb2-annotation book)
    (insert "\n\n")
    (dolist (section (fb2-sections book))
      (fb2-parse-p book section))
    (read-only-mode)
    (setq buffer-file-name filename)
    (set-buffer-modified-p nil)
    (goto-char 0)))

(provide 'fb2)
