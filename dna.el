(defun green-offset (offset)
  (+ offset 13616))

(defun goto-green-offset (offset)
  (interactive "nOffset: ")
  (goto-char (green-offset offset)))

(defun save-fragment (offset length)
  (interactive "nOffset: 
nLength: ")
  (kill-ring-save (green-offset offset) (green-offset (+ offset length))))
