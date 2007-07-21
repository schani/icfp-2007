(load "utils.lisp")
(load "let-match.lisp")

(defun asnat (n &optional (terminator t))
  (cond ((zerop n)
	 (if terminator "P" ""))
	((evenp n)
	 (string-concat "I" (asnat (ash n -1) terminator)))
	(t
	 (string-concat "C" (asnat (ash n -1) terminator)))))

(defun quote-dna (str)
  (apply #'string-concat
	 (map 'list #'(lambda (c)
			(case c
			  (#\I "C")
			  (#\C "F")
			  (#\F "P")
			  (#\P "IC")))
	      str)))

(defvar *skip* "IP")
(defvar *start-group* "IIP")
(defvar *end-group* "IIC")

(defun compile-pattern (pat)
  (labels ((compile (pat)
	     (cond ((numberp pat)
		    (string-concat *skip* (asnat pat)))
		   ((stringp pat)
		    (quote-dna pat))
		   ((eql (car pat) '?)
		    (string-concat "IFI" (quote-dna (cadr pat))))
		   ((eql (car pat) 'rna)
		    (let ((rna (cadr pat)))
		      (assert (= (length rna) 7))
		      (string-concat "III" (cadr pat))))
		   ((eql (car pat) 'group)
		    (string-concat *start-group*
				   (apply #'string-concat (mapcar #'compile (cdr pat)))
				   *end-group*))
		   (t
		    (error "unknown pattern ~A" pat)))))
    (string-concat (apply #'string-concat (mapcar #'compile pat))
		   "IIC")))

(defun compile-template (tpl)
  (labels ((compile (tpl)
	     (cond ((numberp tpl)
		    (string-concat "IIP" (asnat tpl)))
		   ((stringp tpl)
		    (quote-dna tpl))
		   ((eql (car tpl) 'rna)
		    (let ((rna (cadr tpl)))
		      (assert (= (length rna) 7))
		      (string-concat "III" (cadr tpl))))
		   ((eql (cadr tpl) '_)
		    (string-concat "IF" (asnat (caddr tpl)) (asnat (car tpl))))
		   (t
		    (error "unknown template ~A" tpl)))))
    (string-concat (apply #'string-concat (mapcar #'compile tpl))
		   "IIC")))

(defun compile-rule (pat tpl)
  (string-concat (compile-pattern pat) (compile-template tpl)))

(defun dna-replace (src dst)
  (let ((src-len (length src)))
    (compile-rule `((group ,src-len (group (? ,src))))
		  `(,*start-group* ,*skip* 0 ,*end-group* ,*skip* ,(asnat src-len) ,*end-group*
		    ,(compile-template `((0 _ 0) ,dst))
		    (1 _ 0)))))

(defun global-dna-replace (src dst prefix-len)
  (let* ((src-len (length src))
	 (prefix (compile-rule `((group ,prefix-len) (group ,src-len) (group (? ,src)))

			       `(,*start-group* ,*skip* 0 ,*end-group*
				 ,*start-group* ,*skip* 2 ,*end-group*
				 ,*skip* ,(asnat src-len) ,*end-group*

				 ,(compile-template `((0 _ 0) (0 _ 0) (1 _ 0) ,dst))

				 (0 _ 0) (1 _ 0) (2 _ 0))))
	 (new-len (length prefix)))
    (if (= new-len prefix-len)
	(string-concat prefix prefix)
	(global-dna-replace src dst new-len))))

(defun replace-constant (prefix const)
  (let ((str (asnat const nil)))
    (compile-rule `((group (? ,prefix)) ,(make-string (length str) :initial-element #\I))
		  `((0 _ 0) ,str))))
