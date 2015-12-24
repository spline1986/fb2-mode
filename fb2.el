(defun fb2-description (node)
  (car (xml-get-children node 'description)))

(defun fb2-title-info (node)
  (car (xml-get-children
	(fb2-description node) 'title-info)))

(defun fb2-book-title (node)
  (car (xml-get-children
	(fb2-title-info node) 'book-title)))

(defun fb2-author (node)
  (car (xml-get-children
	(fb2-title-info node) 'author)))

(defun fb2-author-first-name (node)
  (car (xml-get-children
	(fb2-author node) 'first-name)))

(defun fb2-author-last-name (node)
  (car (xml-get-children
	(fb2-author node) 'last-name)))

(defun fb2-annotation (node)
  (car (xml-get-children
	(fb2-title-info node) 'annotation)))

(defun fb2-body (node)
  (car (xml-get-children node 'body)))

(defun fb2-paragraphs (node)
  (let (result)
    (dolist (paragraph (xml-node-children node))
      (when (listp paragraph)
	(push paragraph result)))
    (reverse result)))

(defun fb2-strong-p (paragraph)
  (when (listp (third paragraph))
    (if (equal (car (third paragraph)) 'strong)
	t
      nil)))

(defun fb2-emphasis-p (paragraph)
  (when (listp (third paragraph))
    (if (equal (car (third paragraph)) 'emphasis)
	t
      nil)))

(defun fb2-sections (node)
  (xml-node-children node))

(defun fb2-paragraph (paragraph)
  (cond ((fb2-emphasis-p paragraph)
	 (propertize (concat (third (third paragraph)) "\n" "\n") 'face 'itallic))
	((fb2-strong-p paragraph)
	 (concat (third (third paragraph)) "\n" "\n"))
	(t
	 (concat (third paragraph) "\n" "\n"))))

(defun fb2-read ()
;  (interactive "fFilename: ")
  (let (book buf-n)
    (setf book (car (xml-parse-region (point-min) (point-max))))
    (setq buf-n (buffer-name))
    (kill-buffer)
    (get-buffer-create buf-n)
    (switch-to-buffer buf-n)
    (visual-line-mode)
    (setq mode-name "FB2-reader")
    (insert (propertize (concat (third (fb2-book-title book)) "\n") 'face '((t (:height 3.0)))))
    (insert (concat (third (fb2-author-first-name book)) " " (third (fb2-author-last-name book)) "\n" "\n"))
    (dolist (paragraph (fb2-paragraphs (fb2-annotation book)))
      (insert (propertize (fb2-paragraph paragraph) 'face 'shadow)))
    (dolist (item (fb2-sections (fb2-body book)))
      (when (and (listp item) (equal (first item) 'section))
	(dolist (subitem item)
	  (when (listp subitem)
	    (when (equal (first subitem) 'title)
	      (insert "\n")
	      (insert (propertize (concat (third (car (xml-get-children subitem 'p))) "\n") 'face '((t (:height 2.0)))))
	      (insert "\n"))
	    (when (equal (first subitem) 'p)
	      (insert (fb2-paragraph subitem)))))))
    (goto-char 0)))

(provide 'fb2)
