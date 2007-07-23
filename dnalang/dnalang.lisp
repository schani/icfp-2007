; to use:
;
; clisp
; (load "dnalang.lisp")
; (in-package :dnalang)

(load "utils.lisp")
(load "let-match.lisp")

(defpackage "DNALANG"
  (:use "CL" "EXT" "UTILS" "LET-MATCH"))

(in-package :dnalang)

(defun asnat (n &optional (terminator t))
  (cond ((zerop n)
	 (if terminator "P" ""))
	((evenp n)
	 (string-concat "I" (asnat (ash n -1) terminator)))
	(t
	 (string-concat "C" (asnat (ash n -1) terminator)))))


(defun asnat-fixedwidth (n len)
  (if (< n 0)
      (let ((zk (+ (logxor (1- (ash 1 (1- len))) (- n) 1))))
	(asnat-fixedwidth zk len))
    (let* ((an (asnat n nil))
	   (l  (length an)))
      (assert (<= l len))
      (string-concat an (make-string (- len l 1) :initial-element #\I) "P"))))
  
  
(defun quote-dna (str)
  (reduce #'string-concat
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

(defun guide-page (page-num)
  (replace-constant "IFPCFFP" page-num))

(defun gene-table-page-num (page-num)
  (replace-constant "IPIFPFIP" page-num))

(defun make-page-prefixes (nameprefix num genfunc)
  (dolist (i (integers-upto num))
    (let* ((i (1+ i))
	   (filename (format nil "~A~A.prefix" nameprefix i)))
      (with-open-file (out filename :direction :output :if-exists :supersede)
	(format out (funcall genfunc i))))))

(defvar *greenzonestart* "IFPICFPPCFFPP")

(defun modify-green-zone (offset newval)
  (compile-rule `((group (? ,*greenzonestart*) ,(- offset (length *greenzonestart*))) ,(length newval))
		`((0 _ 0) ,newval)
   ))

(defun prepend-green-fragment (offset len)
  (compile-rule `((group (? ,*greenzonestart*) ,(- offset (length *greenzonestart*)) (group ,len)))
		'((0 _ 0) (1 _ 0))))

(defun compile-activate (offset len)
  (compile-rule '((group (? "IFPICFPPCCC") (group (? "IFPICFPPCCC"))))
		`((0 _ 0) ,(asnat offset) ,(asnat len) (1 _ 0))))

(defun compile-pass-arg (arg) 
  (compile-rule '((group (? "IFPICFPPCFIPP")))
		`((0 _ 0) ,(cond
			    ((stringp arg)
			     arg)
			    ((integerp arg)
			     (asnat-fixedwidth arg 24))
			    (t
			     (error "cannot encode arg ~A" arg))))))
			     
;;(defun compile-polygon-coordinates (list) 
;;  (labels ((asnat-pair (x)
;;	     (string-concat (asnat-fixedwidth (car x) 12)
;;			    (asnat-fixedwidth (cdr x) 12)))
;;	   (add-pair (x y) (+ (car x) (car y)) (cdr x)))
;;    
;;    (let* ((len (length list))
;;	   (str (reduce #'string-concat 
;;			(mapcar #'asnat-pair list)
;;			:initial-value (asnat-fixedwidth len 12))))
;;      str)))
	 
(defvar *greenzonelength* 7509409)

(defvar *universal-return*
  (compile-rule `((? ,*greenzonestart*) 
		  (group ,*greenzonelength*)
		  (group 24)
		  (group 24) "IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII")
		`("IIPIP" (1 _ 0) "IIPIP" (2 _ 0) "IICIICIICIPPPIPPCPIIC" 
		  ,*greenzonestart*
		  (0 _ 0))))
