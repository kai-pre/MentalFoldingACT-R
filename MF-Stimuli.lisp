(defun create-stimuli-block ()
	(let ((matchlist nil) (mismatchlist nil) (directions '("a" "b" "c")))
		(dotimes (matching 2) 										; for both matches and mismatches
			(dolist (level '(1 2 3 4)) 								; for each level 
				(if (eq level 1) (setf directions '("x" "y" "z")) (setf directions '("a" "b" "c")))	; if level 1: set possible direction names to x, y, z
				(dolist (variant '("050" "100" "150" "051" "101" "151")) 	; and each variant
					(dolist (targetdir directions) 					; and each arrow direction
						(if (eq matching 0)							; create a stimulus
							(setf matchlist (append matchlist (list (create-stimulus level variant targetdir targetdir)))) 	; match
							(setf mismatchlist (append mismatchlist (list (create-stimulus level variant targetdir (nth (act-r-random 2) (remove targetdir directions))))))	; mismatch
						)
					)
				)
			)
		)
		(return-from create-stimuli-block (list matchlist mismatchlist))
	)
)

(defun create-stimulus (level variant targetdir cubedir)
	(let ((cubelevel level) (tempcubedir cubedir) (stimname nil))
		(if (and (not (eq targetdir cubedir)) (eq level 1))
			(setf cubelevel (+ (act-r-random 2) 2) tempcubedir (nth (act-r-random 2) '("b" "c")))
		)
		(setf stimname (format nil "~a/~a/~a" (equalp targetdir cubedir) (concatenate 'string (write-to-string level) "_" variant "_" targetdir) (concatenate 'string (write-to-string cubelevel) "_000_" tempcubedir)))
		(list stimname (list (eval (read-from-string (concatenate 'string "exp1-cube-" (write-to-string cubelevel) "_" tempcubedir))) (fold-array (eval (read-from-string (concatenate 'string "exp1-fold-" (write-to-string level) "_" variant "_" targetdir)))) (equalp targetdir cubedir)))	
	)
)

; create function that deletes every third element in stimuli block (and name list) - shift is block number, minus one (shift starts at 0, block at 1)
; FIND A WAY TO DELETE RESPECTIVE ENTRIES ALSO FROM NAME LIST
(defun skim-mismatches (stimuliblock shift)
	(let ((moved-block stimuliblock) (new-block nil))
		(dotimes (i (mod (1- shift) 3)) ; take first element and move it to the end
			(setf moved-block (cdr (reverse (cons (car moved-block) (reverse moved-block)))))
		)
		(dotimes (i (length moved-block)) ; create new list and leave out every third element
			(if (not (eq (mod i 3) 0))
				(setf new-block (append new-block (list (nth i moved-block))))
				;(setf deleted-stims (append deleted-stims (list (+ i (1- shift)))))
			)
		)
		(dotimes (i (mod (1- shift) 3))
			(setf new-block (cons (car (reverse new-block)) (reverse (cdr (reverse new-block)))))
		)
		; (setf new-names (append (subseq *stimuli-names* 0 72) (delete-from-names deleted-stims)))
		; (return-from skim-mismatches (list new-block new-names))
		(return-from skim-mismatches new-block)
	)
)

; Probably not needed anymore but past experiences have shown that I should probably not delete everything immediately
; (defun delete-from-names (indices)
	; (let ((new-names nil) (current nil) (mismatches (nthcdr 72 *stimuli-names*)))
		; (dotimes (i (length mismatches))
			; (setf current (nth i mismatches))
			; (dolist (deleteme indices)
				; (if (eq i deleteme)
					; (setf current nil)
				; )
			; )
			; (if (not (eq current nil))
				; (setf new-names (append new-names (list current)))
			; )
		; )
		; (return-from delete-from-names new-names)
	; )
; )

(defun fold-array (foldlist)
	(make-array '(4 4) :initial-contents foldlist)
)

(defun listprint (list)
	(dolist (i list)
		(print i)
	)
)



; Symbols used: A<, Av, A>, A^, B [Base], X [Plain Face], nil [No Face]
; Cube format: (Front Top Left Back Right Bottom), orientations of arrows are as if face is looked at in Window Representation
; Fold format: 4x4 Array
; Correctness format: t, nil
;;; EXP1 Stimuli
; cubes
(defconstant exp1-cube-1_x (list 'X 'X 'X 'X 'Av 'A>))
(defconstant exp1-cube-1_y (list 'Av 'X 'X 'X 'X 'Av))
(defconstant exp1-cube-1_z (list 'X 'X 'Av 'X 'X 'A<))

(defconstant exp1-cube-2_a (list 'X 'A> 'X 'X 'A^ 'X))
(defconstant exp1-cube-2_b (list 'X 'A< 'X 'X 'A^ 'X))
(defconstant exp1-cube-2_c (list 'X 'A^ 'X 'X 'A^ 'X))

(defconstant exp1-cube-3_a (list 'X 'A> 'X 'X 'A^ 'X))
(defconstant exp1-cube-3_b (list 'X 'A< 'X 'X 'A^ 'X))
(defconstant exp1-cube-3_c (list 'X 'A^ 'X 'X 'A^ 'X))

(defconstant exp1-cube-4_a (list 'X 'A> 'X 'X 'A^ 'X))
(defconstant exp1-cube-4_b (list 'X 'A< 'X 'X 'A^ 'X))
(defconstant exp1-cube-4_c (list 'X 'A^ 'X 'X 'A^ 'X))

; folding patterns
(defconstant exp1-fold-1_050_x '((nil nil X nil) (nil nil BA> A<) (nil nil X nil) (nil X X nil)))
(defconstant exp1-fold-1_050_y '((nil X X nil) (nil nil X nil) (nil nil BAv X) (nil nil A^ nil)))
(defconstant exp1-fold-1_050_z '((nil nil X nil) (nil A> BA< nil) (nil nil X nil) (nil nil X X)))

(defconstant exp1-fold-1_051_x '((nil X X nil) (nil nil X nil) (nil nil BA> A<) (nil nil X nil)))
(defconstant exp1-fold-1_051_y '((nil nil X X) (nil nil X nil) (nil X BAv nil) (nil nil A^ nil)))
(defconstant exp1-fold-1_051_z '((nil nil X X) (nil nil X nil) (nil A> BA< nil) (nil nil X nil)))


(defconstant exp1-fold-1_100_x '((nil nil X nil) (nil X BA> A<) (nil nil X nil) (nil nil X nil)))
(defconstant exp1-fold-1_100_y '((nil X nil nil) (X BAv X X) (nil A^ nil nil) (nil nil nil nil)))
(defconstant exp1-fold-1_100_z '((nil nil X nil) (nil A> BA< X) (nil nil X nil) (nil nil X nil)))

(defconstant exp1-fold-1_101_x '((nil nil X nil) (nil nil X nil) (nil X BA> A<) (nil nil X nil)))
(defconstant exp1-fold-1_101_y '((nil nil nil nil) (nil nil X nil) (X X BAv X) (nil nil A^ nil)))
(defconstant exp1-fold-1_101_z '((nil nil X nil) (nil nil X nil) (nil A> BA< X) (nil nil X nil)))


(defconstant exp1-fold-1_150_x '((nil X nil nil) (nil X BA> A<) (nil nil X nil) (nil nil X nil)))
(defconstant exp1-fold-1_150_y '((nil nil nil nil) (nil nil X X) (X X BAv nil) (nil nil A^ nil)))
(defconstant exp1-fold-1_150_z '((nil nil nil X) (nil A> BA< X) (nil nil X nil) (nil nil X nil)))

(defconstant exp1-fold-1_151_x '((nil nil X nil) (nil nil X nil) (nil X BA> A<) (nil X nil nil)))
(defconstant exp1-fold-1_151_y '((nil nil nil nil) (X X nil nil) (nil BAv X X) (nil A^ nil nil)))
(defconstant exp1-fold-1_151_z '((nil nil X nil) (nil nil X nil) (nil A> BA< X) (nil nil nil X)))



(defconstant exp1-fold-2_050_a '((nil X nil nil) (nil X B A>) (nil nil X nil) (nil nil A> nil)))
(defconstant exp1-fold-2_050_b '((nil X nil nil) (nil X B A>) (nil nil X nil) (nil nil A< nil)))
(defconstant exp1-fold-2_050_c '((nil X nil nil) (nil X B A>) (nil nil X nil) (nil nil Av nil)))

(defconstant exp1-fold-2_051_a '((nil nil A> nil) (nil nil X nil) (nil X B A>) (nil X nil nil)))
(defconstant exp1-fold-2_051_b '((nil nil A< nil) (nil nil X nil) (nil X B A>) (nil X nil nil)))
(defconstant exp1-fold-2_051_c '((nil nil Av nil) (nil nil X nil) (nil X B A>) (nil X nil nil)))


(defconstant exp1-fold-2_100_a '((nil nil A> nil) (nil nil X nil) (nil nil B A>) (nil X X nil)))
(defconstant exp1-fold-2_100_b '((nil nil A< nil) (nil nil X nil) (nil nil B A>) (nil X X nil)))
(defconstant exp1-fold-2_100_c '((nil nil Av nil) (nil nil X nil) (nil nil B A>) (nil X X nil)))

(defconstant exp1-fold-2_101_a '((nil X X nil) (nil nil B A>) (nil nil X nil) (nil nil A> nil)))
(defconstant exp1-fold-2_101_b '((nil X X nil) (nil nil B A>) (nil nil X nil) (nil nil A< nil)))
(defconstant exp1-fold-2_101_c '((nil X X nil) (nil nil B A>) (nil nil X nil) (nil nil Av nil)))
; Wrong originally: last arrow goes in the wrong direction
;(setf exp1-fold-2_101_c '((nil X X nil) (nil nil B A>) (nil nil X nil) (nil nil A^ nil)))


(defconstant exp1-fold-2_150_a '((nil nil X nil) (nil X B A>) (nil nil X nil) (nil nil A> nil)))
(defconstant exp1-fold-2_150_b '((nil nil X nil) (nil X B A>) (nil nil X nil) (nil nil A< nil)))
(defconstant exp1-fold-2_150_c '((nil nil X nil) (nil X B A>) (nil nil X nil) (nil nil Av nil)))

(defconstant exp1-fold-2_151_a '((nil nil A> nil) (nil nil X nil) (nil X B A>) (nil nil X nil)))
(defconstant exp1-fold-2_151_b '((nil nil A< nil) (nil nil X nil) (nil X B A>) (nil nil X nil)))
(defconstant exp1-fold-2_151_c '((nil nil Av nil) (nil nil X nil) (nil X B A>) (nil nil X nil)))



(defconstant exp1-fold-3_050_a '((nil nil A> nil) (nil nil X A^) (nil nil B nil) (nil X X nil)))
(defconstant exp1-fold-3_050_b '((nil nil A< nil) (nil nil X A^) (nil nil B nil) (nil X X nil)))
(defconstant exp1-fold-3_050_c '((nil nil Av nil) (nil nil X A^) (nil nil B nil) (nil X X nil)))

(defconstant exp1-fold-3_051_a '((nil X X nil) (nil nil B nil) (nil nil X Av) (nil nil A> nil)))
(defconstant exp1-fold-3_051_b '((nil X X nil) (nil nil B nil) (nil nil X Av) (nil nil A< nil)))
(defconstant exp1-fold-3_051_c '((nil X X nil) (nil nil B nil) (nil nil X Av) (nil nil Av nil)))


(defconstant exp1-fold-3_100_a '((nil nil A> nil) (nil nil X A^) (nil X B nil) (nil X nil nil)))
(defconstant exp1-fold-3_100_b '((nil nil A< nil) (nil nil X A^) (nil X B nil) (nil X nil nil)))
(defconstant exp1-fold-3_100_c '((nil nil Av nil) (nil nil X A^) (nil X B nil) (nil X nil nil)))

(defconstant exp1-fold-3_101_a '((nil X nil nil) (nil X B nil) (nil nil X Av) (nil nil A> nil)))
(defconstant exp1-fold-3_101_b '((nil X nil nil) (nil X B nil) (nil nil X Av) (nil nil A< nil)))
(defconstant exp1-fold-3_101_c '((nil X nil nil) (nil X B nil) (nil nil X Av) (nil nil Av nil)))


(defconstant exp1-fold-3_150_a '((nil nil A> nil) (nil nil X A^) (nil X B nil) (nil nil X nil)))
(defconstant exp1-fold-3_150_b '((nil nil A< nil) (nil nil X A^) (nil X B nil) (nil nil X nil)))
(defconstant exp1-fold-3_150_c '((nil nil Av nil) (nil nil X A^) (nil X B nil) (nil nil X nil)))

(defconstant exp1-fold-3_151_a '((nil nil X nil) (nil X B nil) (nil nil X Av) (nil nil A> nil)))
(defconstant exp1-fold-3_151_b '((nil nil X nil) (nil X B nil) (nil nil X Av) (nil nil A< nil)))
(defconstant exp1-fold-3_151_c '((nil nil X nil) (nil X B nil) (nil nil X Av) (nil nil Av nil)))


(defconstant exp1-fold-4_050_a '((nil nil nil nil) (X nil nil nil) (X B A> nil) (nil nil X A^)))
(defconstant exp1-fold-4_050_b '((nil nil nil nil) (X nil nil nil) (X B A> nil) (nil nil X Av)))
(defconstant exp1-fold-4_050_c '((nil nil nil nil) (X nil nil nil) (X B A> nil) (nil nil X A>)))

(defconstant exp1-fold-4_051_a '((nil nil nil nil) (nil nil X Av) (X B A> nil) (X nil nil nil)))
(defconstant exp1-fold-4_051_b '((nil nil nil nil) (nil nil X A^) (X B A> nil) (X nil nil nil)))
(defconstant exp1-fold-4_051_c '((nil nil nil nil) (nil nil X A<) (X B A> nil) (X nil nil nil)))


(defconstant exp1-fold-4_100_a '((nil nil nil nil) (nil nil X Av) (X B A> nil) (nil X nil nil)))
(defconstant exp1-fold-4_100_b '((nil nil nil nil) (nil nil X A^) (X B A> nil) (nil X nil nil)))
(defconstant exp1-fold-4_100_c '((nil nil nil nil) (nil nil X A<) (X B A> nil) (nil X nil nil)))

(defconstant exp1-fold-4_101_a '((nil nil nil nil) (nil X nil nil) (X B A> nil) (nil nil X A^)))
(defconstant exp1-fold-4_101_b '((nil nil nil nil) (nil X nil nil) (X B A> nil) (nil nil X Av)))
(defconstant exp1-fold-4_101_c '((nil nil nil nil) (nil X nil nil) (X B A> nil) (nil nil X A>)))


(defconstant exp1-fold-4_150_a '((nil nil nil nil) (nil nil X Av) (nil B A> nil) (X X nil nil)))
(defconstant exp1-fold-4_150_b '((nil nil nil nil) (nil nil X A^) (nil B A> nil) (X X nil nil)))
(defconstant exp1-fold-4_150_c '((nil nil nil nil) (nil nil X A<) (nil B A> nil) (X X nil nil)))

(defconstant exp1-fold-4_151_a '((nil nil nil nil) (X X nil nil) (nil B A> nil) (nil nil X A^)))
(defconstant exp1-fold-4_151_b '((nil nil nil nil) (X X nil nil) (nil B A> nil) (nil nil X Av)))
(defconstant exp1-fold-4_151_c '((nil nil nil nil) (X X nil nil) (nil B A> nil) (nil nil X A>)))



; Symbols used: A<, Av, A>, A^, B [Base], X [Plain Face], nil [No Face]
; Cube format: (Front Top Left Back Right Bottom), orientations of arrows are as if face is looked at in Window Representation
; Fold format: 4x4 Array
; Correctness format: t, nil

;; Set test stimuli
; example-same
(defconstant ex-same (list
	(list 'A^ 'A> 'X 'X 'X 'X)
	(make-array '(4 4) :initial-contents '((nil X nil nil) (X B X A<) (nil Av nil nil) (nil nil nil nil)))
	t
	)
)

; example-different
(defconstant ex-diff (list
	(list 'A> 'X 'X 'X 'A< 'X)
	(make-array '(4 4) :initial-contents '((nil nil nil nil) (nil X nil nil) (X B A> nil) (nil nil A^ X)))
	nil
	)
)

; example-same (with arrow on base)
(defconstant ex-same-base (list
	(list 'X 'X 'X 'X 'A< 'Av)
	(make-array '(4 4) :initial-contents '((nil nil nil nil) (nil X nil nil) (X BAv Av nil) (nil nil X X)))
	t
	)
)

; example-diff (with arrow on base)
(defconstant ex-diff-base (list
	(list 'X 'X 'Av 'X 'X 'A<)
	(make-array '(4 4) :initial-contents '((nil nil nil nil) (nil X nil nil) (Av BA< X nil) (nil nil X X)))
	nil
	)
)

; S&F hard examples
; K (same)
(defconstant ex-k (list
	(list 'X 'A< 'A^ 'X 'X 'X) 
	(fold-array '((nil X nil nil) (A< B X nil) (nil nil X Av) (nil nil nil nil)))
	t
	)
)

; L (same)
(defconstant ex-l (list
	(list 'X 'A< 'A^ 'X 'X 'X) 
	(fold-array '((nil nil A^ nil) (nil X X nil) (nil B NIL NIL) (AV X nil nil)))
	t
	)
)

; X (same)
(defconstant ex-x (list
	(list 'Av 'X 'X 'X 'X 'Av)
	(fold-array '((nil nil nil nil) (A^ X nil nil) (nil X X nil) (nil nil X BAv)))
	t
	)
)




;;; Example Stimulus:
;; (create-stimulus 2 "100" "b" "b")
;; ("T/2_100_b/2_000_b" ((X A< X X A^ X) #2A((NIL NIL A< NIL) (NIL NIL X NIL) (NIL NIL B A>) (NIL X X NIL)) T))
;; Use for debugging: (setq teststim '("T/2_100_b/2_000_b" ((X A< X X A^ X) #2A((NIL NIL A< NIL) (NIL NIL X NIL) (NIL NIL B A>) (NIL X X NIL)) T)))
;; first: name
;; second:
;;    first: cube pattern / second: folding pattern
;; third: match or no match?

; Symbols used: A<, Av, A>, A^, B [Base], X [Plain Face], nil [No Face]
; Cube format: (Front Top Left Back Right Bottom), orientations of arrows are as if face is looked at in Window Representation
; Pattern format: 4x4 Array
; Correctness format: t, nil

; ;(load-stimuli "T/4_051_a/4_000_a")
; (setq fault1 "T/4_051_a/4_000_a")
; (setq fault2 "T/4_101_b/4_000_b")
;(load-and-do-stimuli fault2)
;(setf teststim (list "T/4_051_a/4_000_a" (load-stimuli "T/4_051_a/4_000_a")))

;(do-stimuli teststim)
;(let ((res nil)) (dotimes (i 5) (setf res (append res (list (load-and-do-stimuli fault2)))) (reset)) res)

;(defvar *modeling-data* '((("T/1_050_y/1_000_y-3757996745" 5.315 T) ("T/4_101_c/4_000_c-3757996745" 6.6 T) ("T/1_150_y/1_000_y-3757996745" 5.5649996 T) ("T/2_151_b/2_000_b-3757996745" 6.465 T) ("T/2_150_a/2_000_a-3757996745" 6.0650005 T) ("T/4_100_b/4_000_b-3757996745" 6.999998 T) ("T/4_050_c/4_000_c-3757996745" 7.285 T) ("T/3_100_a/3_000_a-3757996745" 7.2850037 T) ("T/4_101_a/4_000_a-3757996745" 6.7199974 T) ("T/3_100_b/3_000_b-3757996745" 6.750004 T) ("NIL/4_050_a/4_000_c-3757996745" 7.0699997 NIL) ("T/4_050_a/4_000_a-3757996745" 7.364998 T) ("NIL/1_100_x/2_000_b-3757996745" 4.9850006 NIL) ("T/3_150_b/3_000_b-3757996745" 7.0 T) ("T/3_151_c/3_000_c-3757996745" 6.614998 T) ("NIL/1_051_y/2_000_b-3757996745" 4.9800034 NIL) ("T/4_050_b/4_000_b-3757996745" 7.449997 T) ("T/3_050_a/3_000_a-3757996745" 6.5149994 T) ("T/4_151_a/4_000_a-3757996745" 6.6800003 T) ("T/4_100_c/4_000_c-3757996745" 7.4850082 T) ("NIL/4_150_b/4_000_c-3757996745" 6.649994 NIL) ("NIL/2_100_a/2_000_b-3757996746" 6.330002 NIL) ("T/1_050_x/1_000_x-3757996746" 5.349991 T) ("NIL/3_100_c/3_000_b-3757996746" 6.850006 NIL) ("T/1_150_z/1_000_z-3757996746" 5.2299957 T) ("NIL/3_151_a/3_000_c-3757996746" 6.7850037 NIL) ("T/2_100_a/2_000_a-3757996746" 6.600006 T) ("NIL/2_051_b/2_000_c-3757996746" 6.849991 NIL) ("T/3_151_a/3_000_a-3757996746" 7.1650085 T) ("NIL/4_051_c/4_000_a-3757996746" 7.3349915 NIL) ("NIL/2_100_c/2_000_b-3757996746" 5.8650055 NIL) ("NIL/2_150_c/2_000_b-3757996746" 6.25 NIL) ("T/4_151_c/4_000_c-3757996746" 7.1150055 T) ("T/4_051_b/4_000_b-3757996746" 7.0 T) ("NIL/1_151_y/3_000_b-3757996746" 4.9799957 NIL) ("T/2_100_b/2_000_b-3757996746" 6.9649963 T) ("T/4_150_c/4_000_c-3757996746" 7.199997 T) ("T/2_101_a/2_000_a-3757996746" 6.650009 T) ("T/1_101_x/1_000_x-3757996746" 5.1649933 T) ("NIL/3_101_a/3_000_c-3757996746" 7.470001 NIL) ("NIL/3_150_c/3_000_b-3757996746" 7.100006 NIL) ("T/2_101_c/2_000_c-3757996746" 6.4349976 T) ("T/1_101_y/1_000_y-3757996746" 5.1799927 T) ("T/1_100_z/1_000_z-3757996746" 5.130005 T) ("NIL/3_050_c/3_000_b-3757996746" 6.9849854 NIL) ("NIL/2_051_a/2_000_b-3757996746" 6.715027 NIL) ("NIL/4_050_c/4_000_b-3757996746" 7.0999756 NIL) ("NIL/1_050_y/2_000_c-3757996746" 5.950012 NIL) ("T/4_150_a/4_000_a-3757996746" 7.549988 T) ("NIL/2_050_c/2_000_a-3757996746" 6.735016 NIL) ("NIL/2_050_a/2_000_c-3757996747" 5.8150024 NIL) ("T/4_151_b/4_000_b-3757996747" 7.0849915 T) ("T/4_100_a/4_000_a-3757996747" 6.985016 T) ("NIL/1_101_x/2_000_b-3757996747" 5.3499756 NIL) ("NIL/4_100_b/4_000_c-3757996747" 7.085022 NIL) ("T/1_051_z/1_000_z-3757996747" 5.284973 T) ("NIL/4_100_a/4_000_b-3757996747" 7.350006 NIL) ("T/1_100_x/1_000_x-3757996747" 5.235016 T) ("NIL/4_151_b/4_000_c-3757996747" 6.5650024 NIL) ("T/2_101_b/2_000_b-3757996747" 6.75 T) ("NIL/4_051_a/4_000_c-3757996747" 6.9349976 NIL) ("T/2_100_c/2_000_c-3757996747" 7.299988 T) ("T/2_050_b/2_000_b-3757996747" 6.86499 T) ("T/2_051_b/2_000_b-3757996747" 6.4150085 T) ("NIL/1_050_x/3_000_c-3757996747" 4.950012 NIL) ("NIL/1_150_z/2_000_b-3757996747" 4.7850037 NIL) ("T/1_151_x/1_000_x-3757996747" 5.414978 T) ("T/4_150_b/4_000_b-3757996747" 6.9150085 T) ("NIL/4_101_c/4_000_a-3757996747" 6.529999 NIL) ("NIL/1_151_z/2_000_c-3757996747" 5.36499 NIL) ("T/3_101_a/3_000_a-3757996747" 7.0150146 T) ("NIL/3_100_b/3_000_a-3757996747" 6.600006 NIL) ("NIL/1_151_x/3_000_b-3757996747" 4.86499 NIL) ("T/3_101_c/3_000_c-3757996747" 7.019989 T) ("T/1_150_x/1_000_x-3757996747" 4.985016 T) ("NIL/3_151_b/3_000_a-3757996747" 7.36499 NIL) ("T/3_050_b/3_000_b-3757996747" 6.855011 T) ("NIL/4_101_a/4_000_c-3757996747" 6.654999 NIL) ("NIL/2_151_b/2_000_c-3757996748" 7.1849976 NIL) ("T/3_100_c/3_000_c-3757996748" 6.529999 T) ("T/1_151_z/1_000_z-3757996748" 4.965027 T) ("NIL/2_151_c/2_000_a-3757996748" 6.869995 NIL) ("NIL/1_100_z/2_000_c-3757996748" 5.3150024 NIL) ("NIL/2_101_a/2_000_c-3757996748" 6.049988 NIL) ("NIL/3_051_c/3_000_a-3757996748" 6.7199707 NIL) ("T/1_100_y/1_000_y-3757996748" 5.330017 T) ("T/2_050_c/2_000_c-3757996748" 5.9299927 T) ("T/2_051_c/2_000_c-3757996748" 6.335022 T) ("T/2_151_c/2_000_c-3757996748" 6.950012 T) ("NIL/3_101_c/3_000_b-3757996748" 6.8849487 NIL) ("T/1_051_x/1_000_x-3757996748" 5.800049 T) ("T/3_051_c/3_000_c-3757996748" 6.964966 T) ("T/3_150_c/3_000_c-3757996748" 7.11499 T) ("T/3_051_a/3_000_a-3757996748" 7.1150513 T) ("NIL/4_101_b/4_000_a-3757996748" 7.2649536 NIL) ("T/1_051_y/1_000_y-3757996748" 5.330017 T) ("T/1_151_y/1_000_y-3757996748" 5.3499756 T) ("T/2_150_b/2_000_b-3757996748" 6.585022 T) ("NIL/4_100_c/4_000_b-3757996748" 7.419983 NIL) ("T/2_051_a/2_000_a-3757996748" 6.7700195 T) ("T/2_150_c/2_000_c-3757996748" 6.200012 T) ("T/3_101_b/3_000_b-3757996748" 7.700012 T) ("NIL/3_050_a/3_000_c-3757996749" 7.4349976 NIL) ("T/3_050_c/3_000_c-3757996749" 8.2699585 T) ("NIL/2_101_b/2_000_c-3757996749" 6.61499 NIL) ("T/4_051_c/4_000_c-3757996749" 6.715027 T) ("NIL/1_051_z/3_000_b-3757996749" 5.215027 NIL) ("T/2_151_a/2_000_a-3757996749" 7.0 T) ("NIL/2_150_b/2_000_c-3757996749" 5.9299927 NIL) ("T/2_050_a/2_000_a-3757996749" 6.214966 T) ("NIL/4_151_a/4_000_c-3757996749" 6.88501 NIL) ("NIL/2_051_c/2_000_b-3757996749" 5.715027 NIL) ("T/3_051_b/3_000_b-3757996749" 7.699951 T) ("T/3_151_b/3_000_b-3757996749" 6.965027 T) ("T/3_150_a/3_000_a-3757996749" 7.0650024 T) ("T/1_101_z/1_000_z-3757996749" 4.9799805 T) ("T/4_101_b/4_000_b-3757996749" 7.0200195 T) ("T/4_051_a/4_000_a-3757996749" 6.5999756 T) ("NIL/4_151_c/4_000_b-3757996749" 7.0150146 NIL) ("T/1_050_z/1_000_z-3757996749" 5.1849976 T)) (("NIL/4_051_b/4_000_a-3757996749" 6.950012 NIL) ("NIL/2_150_c/2_000_b-3757996749" 6.914978 NIL) ("T/3_051_c/3_000_c-3757996749" 7.715027 T) ("NIL/4_100_a/4_000_b-3757996749" 7.299988 NIL) ("NIL/2_150_b/2_000_c-3757996749" 15.89502 NIL) ("NIL/4_101_c/4_000_a-3757996750" 6.714966 NIL) ("NIL/1_100_z/2_000_c-3757996750" 4.800049 NIL) ("T/4_051_c/4_000_c-3757996750" 7.4849854 T) ("T/2_050_b/2_000_b-3757996750" 6.4699707 T) ("T/4_051_b/4_000_b-3757996750" 6.9700317 T) ("T/4_101_c/4_000_c-3757996750" 7.2199707 T) ("NIL/2_100_b/2_000_c-3757996750" 6.61499 NIL) ("NIL/3_050_c/3_000_b-3757996750" 6.715027 NIL) ("NIL/3_151_c/3_000_a-3757996750" 7.0999756 NIL) ("NIL/2_150_a/2_000_b-3757996750" 6.450012 NIL) ("NIL/4_050_b/4_000_a-3757996750" 6.9299927 NIL) ("T/4_151_b/4_000_b-3757996750" 7.455017 T) ("T/4_151_a/4_000_a-3757996750" 6.919983 T) ("T/3_100_b/3_000_b-3757996750" 6.36499 T) ("T/2_051_a/2_000_a-3757996750" 5.9300537 T) ("NIL/3_150_a/3_000_b-3757996750" 6.6799927 NIL) ("T/2_050_a/2_000_a-3757996750" 6.8200073 T) ("NIL/4_050_c/4_000_b-3757996750" 7.214966 NIL) ("NIL/3_100_a/3_000_b-3757996750" 6.36499 NIL) ("NIL/2_051_a/2_000_b-3757996750" 6.86499 NIL) ("T/3_101_b/3_000_b-3757996750" 6.630005 T) ("NIL/3_051_a/3_000_c-3757996750" 6.165039 NIL) ("T/2_101_b/2_000_b-3757996751" 7.0999756 T) ("T/1_050_y/1_000_y-3757996751" 5.0300293 T) ("T/1_101_z/1_000_z-3757996751" 5.0149536 T) ("T/2_051_b/2_000_b-3757996751" 6.3500366 T) ("T/3_151_b/3_000_b-3757996751" 7.1499634 T) ("T/2_151_c/2_000_c-3757996751" 6.4850464 T) ("T/3_150_c/3_000_c-3757996751" 6.799988 T) ("T/4_100_b/4_000_b-3757996751" 7.3999634 T) ("T/1_150_z/1_000_z-3757996751" 5.885071 T) ("T/2_151_a/2_000_a-3757996751" 6.3499756 T) ("NIL/1_100_x/2_000_b-3757996751" 5.11499 NIL) ("T/3_150_b/3_000_b-3757996751" 7.2349854 T) ("T/1_050_z/1_000_z-3757996751" 5.920044 T) ("T/4_150_c/4_000_c-3757996751" 7.419922 T) ("T/3_101_a/3_000_a-3757996751" 6.470093 T) ("T/4_050_b/4_000_b-3757996751" 6.949951 T) ("NIL/1_051_y/2_000_b-3757996751" 5.0999756 NIL) ("T/2_101_a/2_000_a-3757996751" 7.1800537 T) ("T/3_100_c/3_000_c-3757996751" 7.11499 T) ("T/4_051_a/4_000_a-3757996751" 6.954956 T) ("T/2_150_a/2_000_a-3757996751" 6.2349854 T) ("NIL/2_151_c/2_000_a-3757996751" 6.9500732 NIL) ("T/2_101_c/2_000_c-3757996751" 6.449951 T) ("T/4_101_b/4_000_b-3757996751" 6.75 T) ("T/1_100_z/1_000_z-3757996752" 5.5 T) ("T/4_150_b/4_000_b-3757996752" 6.9350586 T) ("T/1_151_x/1_000_x-3757996752" 5.130005 T) ("T/1_150_y/1_000_y-3757996752" 4.949951 T) ("NIL/1_100_y/3_000_b-3757996752" 5.8199463 NIL) ("T/2_050_c/2_000_c-3757996752" 6.5 T) ("NIL/3_151_b/3_000_a-3757996752" 6.6051025 NIL) ("NIL/1_050_x/3_000_c-3757996752" 4.829956 NIL) ("T/1_151_z/1_000_z-3757996752" 6.329956 T) ("T/4_050_c/4_000_c-3757996752" 6.75 T) ("T/4_100_c/4_000_c-3757996752" 7.0 T) ("NIL/1_151_z/2_000_c-3757996752" 4.9350586 NIL) ("T/3_051_a/3_000_a-3757996752" 7.949951 T) ("T/4_050_a/4_000_a-3757996752" 7.7349854 T) ("T/3_050_c/3_000_c-3757996752" 6.785034 T) ("NIL/3_101_b/3_000_c-3757996752" 7.2650146 NIL) ("NIL/3_151_a/3_000_c-3757996752" 7.579956 NIL) ("T/1_051_z/1_000_z-3757996752" 5.6800537 T) ("T/3_051_b/3_000_b-3757996752" 6.7299805 T) ("T/1_151_y/1_000_y-3757996753" 5.5999756 T) ("NIL/2_151_a/2_000_b-3757996753" 6.11499 NIL) ("NIL/3_051_c/3_000_a-3757996753" 7.4000244 NIL) ("NIL/1_151_x/3_000_b-3757996753" 4.7800293 NIL) ("T/2_151_b/2_000_b-3757996753" 6.699951 T) ("T/3_151_c/3_000_c-3757996753" 6.380005 T) ("T/2_051_c/2_000_c-3757996753" 6.61499 T) ("NIL/4_050_a/4_000_c-3757996753" 7.050049 NIL) ("T/1_051_x/1_000_x-3757996753" 5.2999268 T) ("NIL/4_100_b/4_000_c-3757996753" 6.6850586 NIL) ("NIL/4_151_b/4_000_c-3757996753" 7.0649414 NIL) ("NIL/2_050_b/2_000_a-3757996753" 7.285034 NIL) ("NIL/4_150_b/4_000_c-3757996753" 6.88501 NIL) ("NIL/3_100_b/3_000_a-3757996753" 6.915039 NIL) ("T/3_100_a/3_000_a-3757996753" 7.834961 T) ("T/3_050_a/3_000_a-3757996753" 6.4799805 T) ("NIL/1_051_z/3_000_b-3757996753" 5.25 NIL) ("T/4_100_a/4_000_a-3757996753" 7.2349854 T) ("NIL/2_101_b/2_000_c-3757996753" 6.5 NIL) ("T/1_050_x/1_000_x-3757996753" 5.2800293 T) ("T/1_150_x/1_000_x-3757996753" 5.61499 T) ("T/1_051_y/1_000_y-3757996753" 5.215088 T) ("T/1_101_y/1_000_y-3757996754" 5.11499 T) ("NIL/2_051_b/2_000_c-3757996754" 5.86499 NIL) ("T/3_151_a/3_000_a-3757996754" 8.204956 T) ("T/2_150_b/2_000_b-3757996754" 6.2299805 T) ("NIL/1_150_y/3_000_c-3757996754" 5.1500244 NIL) ("T/3_101_c/3_000_c-3757996754" 7.300049 T) ("T/2_100_a/2_000_a-3757996754" 6.2199707 T) ("NIL/1_150_x/2_000_c-3757996754" 5.75 NIL) ("T/2_100_b/2_000_b-3757996754" 6.5300293 T) ("NIL/1_101_y/2_000_c-3757996754" 5.5999756 NIL) ("T/2_150_c/2_000_c-3757996754" 7.0200195 T) ("NIL/4_051_a/4_000_c-3757996754" 6.784912 NIL) ("NIL/4_100_c/4_000_b-3757996754" 7.0 NIL) ("T/1_101_x/1_000_x-3757996754" 5.300049 T) ("T/3_150_a/3_000_a-3757996754" 7.084961 T) ("NIL/2_050_c/2_000_a-3757996754" 6.665039 NIL) ("T/2_100_c/2_000_c-3757996754" 6.165039 T) ("T/3_050_b/3_000_b-3757996754" 6.464966 T) ("NIL/1_150_z/2_000_b-3757996754" 5.0150146 NIL) ("NIL/3_150_b/3_000_c-3757996754" 6.449951 NIL) ("T/1_100_y/1_000_y-3757996755" 5.2800293 T) ("NIL/4_151_a/4_000_c-3757996755" 7.165039 NIL) ("T/4_151_c/4_000_c-3757996755" 7.1499023 T) ("NIL/4_150_c/4_000_a-3757996755" 7.1151123 NIL) ("T/4_101_a/4_000_a-3757996755" 7.6849365 T) ("NIL/1_101_x/2_000_b-3757996755" 5.0300293 NIL) ("T/4_150_a/4_000_a-3757996755" 6.88501 T) ("T/1_100_x/1_000_x-3757996755" 5.5 T)) (("NIL/3_100_b/3_000_a-3757996755" 6.75 NIL) ("NIL/1_151_y/3_000_b-3757996755" 4.880005 NIL) ("T/3_151_b/3_000_b-3757996755" 6.7650146 T) ("T/1_051_z/1_000_z-3757996755" 5.0 T) ("T/1_050_z/1_000_z-3757996755" 5.1499023 T) ("T/1_150_y/1_000_y-3757996755" 5.0650635 T) ("T/3_151_c/3_000_c-3757996755" 6.75 T) ("T/4_150_a/4_000_a-3757996755" 6.61499 T) ("T/2_050_a/2_000_a-3757996755" 6.7199707 T) ("NIL/4_151_a/4_000_c-3757996755" 7.0650635 NIL) ("NIL/4_150_a/4_000_c-3757996755" 6.869995 NIL) ("NIL/4_150_c/4_000_a-3757996756" 6.714966 NIL) ("T/4_050_b/4_000_b-3757996756" 6.915039 T) ("NIL/3_100_c/3_000_b-3757996756" 6.834961 NIL) ("T/2_100_c/2_000_c-3757996756" 7.25 T) ("T/2_051_a/2_000_a-3757996756" 6.5649414 T) ("T/3_150_c/3_000_c-3757996756" 6.9300537 T) ("T/2_100_b/2_000_b-3757996756" 6.36499 T) ("NIL/2_101_b/2_000_c-3757996756" 6.449951 NIL) ("T/3_050_c/3_000_c-3757996756" 7.4500732 T) ("NIL/2_051_a/2_000_b-3757996756" 6.5999756 NIL) ("NIL/4_051_a/4_000_c-3757996756" 7.1500244 NIL) ("NIL/3_050_b/3_000_a-3757996756" 6.4799805 NIL) ("NIL/3_100_a/3_000_b-3757996756" 6.920044 NIL) ("T/4_151_a/4_000_a-3757996756" 7.034912 T) ("T/3_100_a/3_000_a-3757996756" 6.61499 T) ("NIL/4_101_c/4_000_a-3757996756" 7.2000732 NIL) ("T/4_100_a/4_000_a-3757996756" 7.3149414 T) ("T/1_051_y/1_000_y-3757996757" 5.0300293 T) ("NIL/1_100_z/2_000_c-3757996757" 4.964966 NIL) ("T/4_050_c/4_000_c-3757996757" 8.119995 T) ("T/3_051_a/3_000_a-3757996757" 6.6500244 T) ("T/2_150_c/2_000_c-3757996757" 6.170044 T) ("T/3_101_a/3_000_a-3757996757" 6.630005 T) ("T/1_150_x/1_000_x-3757996757" 5.035034 T) ("T/4_101_b/4_000_b-3757996757" 6.8999023 T) ("NIL/2_051_c/2_000_b-3757996757" 6.335083 NIL) ("T/2_101_a/2_000_a-3757996757" 7.13501 T) ("NIL/4_150_b/4_000_c-3757996757" 7.5999756 NIL) ("T/4_101_c/4_000_c-3757996757" 7.2199707 T) ("T/3_051_c/3_000_c-3757996757" 7.63501 T) ("NIL/3_150_a/3_000_b-3757996757" 7.4000244 NIL) ("T/4_100_b/4_000_b-3757996757" 7.5499268 T) ("T/3_101_c/3_000_c-3757996757" 7.335083 T) ("NIL/4_151_c/4_000_b-3757996757" 6.8149414 NIL) ("NIL/4_051_c/4_000_a-3757996757" 6.380005 NIL) ("NIL/4_101_b/4_000_a-3757996758" 6.9000244 NIL) ("T/4_051_b/4_000_b-3757996758" 6.8149414 T) ("NIL/3_101_b/3_000_c-3757996758" 6.505005 NIL) ("T/2_051_c/2_000_c-3757996758" 6.7800293 T) ("NIL/3_150_b/3_000_c-3757996758" 7.0999756 NIL) ("T/3_101_b/3_000_b-3757996758" 7.465088 T) ("T/2_101_b/2_000_b-3757996758" 7.169922 T) ("NIL/3_050_c/3_000_b-3757996758" 7.4300537 NIL) ("T/2_050_c/2_000_c-3757996758" 6.380005 T) ("NIL/1_151_z/2_000_c-3757996758" 5.0649414 NIL) ("T/2_150_a/2_000_a-3757996758" 6.61499 T) ("T/4_150_c/4_000_c-3757996758" 7.050049 T) ("NIL/3_050_a/3_000_c-3757996758" 7.8149414 NIL) ("T/3_150_b/3_000_b-3757996758" 7.165039 T) ("NIL/1_101_y/2_000_c-3757996758" 5.464966 NIL) ("T/4_151_c/4_000_c-3757996758" 6.665039 T) ("T/1_050_y/1_000_y-3757996758" 5.050049 T) ("T/1_151_y/1_000_y-3757996759" 5.0499268 T) ("T/4_050_a/4_000_a-3757996759" 6.550049 T) ("NIL/2_100_a/2_000_b-3757996759" 6.8199463 NIL) ("T/2_151_b/2_000_b-3757996759" 6.720093 T) ("T/1_100_y/1_000_y-3757996759" 5.0649414 T) ("T/2_150_b/2_000_b-3757996759" 6.4350586 T) ("NIL/1_151_x/3_000_b-3757996759" 4.8999023 NIL) ("NIL/2_050_b/2_000_a-3757996759" 6.11499 NIL) ("NIL/1_100_x/2_000_b-3757996759" 4.86499 NIL) ("NIL/3_151_c/3_000_a-3757996759" 6.215088 NIL) ("T/1_101_y/1_000_y-3757996759" 5.86499 T) ("NIL/4_101_a/4_000_c-3757996759" 7.669922 NIL) ("T/4_150_b/4_000_b-3757996759" 6.830078 T) ("T/3_151_a/3_000_a-3757996759" 7.3149414 T) ("NIL/4_050_a/4_000_c-3757996759" 6.665039 NIL) ("T/3_100_b/3_000_b-3757996759" 7.199951 T) ("T/2_151_a/2_000_a-3757996759" 5.880127 T) ("T/1_151_x/1_000_x-3757996760" 5.1799316 T) ("NIL/2_151_b/2_000_c-3757996760" 6.534912 NIL) ("T/1_100_z/1_000_z-3757996760" 5.300049 T) ("T/2_100_a/2_000_a-3757996760" 6.784912 T) ("NIL/2_050_c/2_000_a-3757996760" 6.205078 NIL) ("NIL/3_051_b/3_000_c-3757996760" 6.870117 NIL) ("T/4_151_b/4_000_b-3757996760" 7.2700195 T) ("T/1_051_x/1_000_x-3757996760" 5.61499 T) ("T/1_151_z/1_000_z-3757996760" 5.5498047 T) ("NIL/3_051_a/3_000_c-3757996760" 7.3500977 NIL) ("T/4_051_a/4_000_a-3757996760" 6.9799805 T) ("T/2_050_b/2_000_b-3757996760" 6.465088 T) ("T/3_050_b/3_000_b-3757996760" 6.5649414 T) ("T/1_050_x/1_000_x-3757996760" 5.11499 T) ("T/3_150_a/3_000_a-3757996760" 7.4851074 T) ("NIL/1_051_z/3_000_b-3757996760" 4.9299316 NIL) ("NIL/3_151_a/3_000_c-3757996761" 6.379883 NIL) ("NIL/1_051_x/2_000_b-3757996761" 5.180176 NIL) ("T/1_101_x/1_000_x-3757996761" 5.3149414 T) ("T/4_051_c/4_000_c-3757996761" 6.98999 T) ("T/4_101_a/4_000_a-3757996761" 6.5300293 T) ("NIL/2_101_a/2_000_c-3757996761" 5.7648926 NIL) ("NIL/4_100_b/4_000_c-3757996761" 6.88501 NIL) ("NIL/4_050_b/4_000_a-3757996761" 6.5549316 NIL) ("NIL/2_150_b/2_000_c-3757996761" 6.63501 NIL) ("NIL/1_101_z/3_000_b-3757996761" 4.7851562 NIL) ("T/3_051_b/3_000_b-3757996761" 7.449951 T) ("T/2_051_b/2_000_b-3757996761" 6.86499 T) ("NIL/1_150_z/2_000_b-3757996761" 5.5148926 NIL) ("T/4_100_c/4_000_c-3757996761" 6.8151855 T) ("T/2_151_c/2_000_c-3757996762" 6.5998535 T) ("T/1_100_x/1_000_x-3757996762" 4.965088 T) ("NIL/3_051_c/3_000_a-3757996762" 7.300049 NIL) ("T/1_101_z/1_000_z-3757996762" 5.415039 T) ("T/3_050_a/3_000_a-3757996762" 6.5649414 T) ("T/1_150_z/1_000_z-3757996762" 4.9799805 T) ("NIL/1_051_y/2_000_b-3757996762" 4.9799805 NIL) ("NIL/3_101_a/3_000_c-3757996762" 6.3999023 NIL) ("T/3_100_c/3_000_c-3757996762" 7.3500977 T) ("T/2_101_c/2_000_c-3757996762" 6.199951 T)) (("NIL/3_100_b/3_000_a-3757996762" 6.5649414 NIL) ("NIL/4_151_c/4_000_b-3757996762" 6.8500977 NIL) ("NIL/3_100_c/3_000_b-3757996762" 6.4799805 NIL) ("T/1_150_z/1_000_z-3757996763" 5.7651367 T) ("NIL/2_150_c/2_000_b-3757996763" 6.86499 NIL) ("NIL/1_050_y/2_000_c-3757996763" 5.0649414 NIL) ("T/3_100_a/3_000_a-3757996763" 6.9799805 T) ("NIL/2_151_b/2_000_c-3757996763" 6.5649414 NIL) ("T/3_101_a/3_000_a-3757996763" 7.370117 T) ("T/1_101_y/1_000_y-3757996763" 5.0148926 T) ("T/3_050_a/3_000_a-3757996763" 6.2651367 T) ("T/3_151_b/3_000_b-3757996763" 7.3049316 T) ("NIL/4_101_a/4_000_c-3757996763" 6.7148437 NIL) ("T/3_100_b/3_000_b-3757996763" 7.335205 T) ("NIL/2_150_a/2_000_b-3757996763" 6.5498047 NIL) ("T/4_100_c/4_000_c-3757996763" 7.3151855 T) ("NIL/3_101_c/3_000_b-3757996764" 6.829834 NIL) ("T/4_100_a/4_000_a-3757996764" 7.4501953 T) ("NIL/2_101_b/2_000_c-3757996764" 6.129883 NIL) ("T/2_151_b/2_000_b-3757996764" 6.6499023 T) ("T/3_101_c/3_000_c-3757996764" 6.2800293 T) ("T/4_050_b/4_000_b-3757996764" 6.465088 T) ("T/2_100_c/2_000_c-3757996764" 6.3500977 T) ("NIL/4_050_b/4_000_a-3757996764" 6.9848633 NIL) ("T/1_100_x/1_000_x-3757996764" 5.800049 T) ("T/2_101_a/2_000_a-3757996764" 5.8149414 T) ("T/3_101_b/3_000_b-3757996765" 7.165039 T) ("T/2_151_a/2_000_a-3757996765" 6.3500977 T) ("NIL/3_051_b/3_000_c-3757996765" 6.949951 NIL) ("T/4_100_b/4_000_b-3757996765" 6.9299316 T) ("T/2_051_b/2_000_b-3757996765" 6.9851074 T) ("T/3_100_c/3_000_c-3757996765" 6.7348633 T) ("NIL/2_051_b/2_000_c-3757996765" 6.699951 NIL) ("T/1_050_x/1_000_x-3757996765" 5.9501953 T) ("NIL/1_101_y/2_000_c-3757996765" 4.9848633 NIL) ("T/3_150_a/3_000_a-3757996765" 7.6550293 T) ("T/2_100_b/2_000_b-3757996765" 6.380127 T) ("T/2_150_a/2_000_a-3757996765" 6.4799805 T) ("NIL/3_150_a/3_000_b-3757996766" 6.949951 NIL) ("NIL/4_101_c/4_000_a-3757996766" 8.185059 NIL) ("NIL/3_100_a/3_000_b-3757996766" 6.9348145 NIL) ("NIL/4_051_c/4_000_a-3757996766" 6.61499 NIL) ("T/4_101_c/4_000_c-3757996766" 7.61499 T) ("NIL/3_050_b/3_000_a-3757996766" 7.0651855 NIL) ("T/1_051_z/1_000_z-3757996766" 5.329834 T) ("NIL/4_150_a/4_000_c-3757996766" 7.1000977 NIL) ("NIL/1_151_y/3_000_b-3757996766" 5.0300293 NIL) ("T/2_101_c/2_000_c-3757996766" 6.75 T) ("T/2_050_a/2_000_a-3757996766" 6.86499 T) ("T/4_150_c/4_000_c-3757996766" 7.7700195 T) ("NIL/1_050_z/2_000_b-3757996766" 5.2648926 NIL) ("NIL/3_150_c/3_000_b-3757996766" 6.8149414 NIL) ("T/2_151_c/2_000_c-3757996766" 6.215088 T) ("T/4_051_a/4_000_a-3757996767" 7.0 T) ("NIL/3_151_b/3_000_a-3757996767" 7.9350586 NIL) ("T/1_150_y/1_000_y-3757996767" 5.199951 T) ("NIL/4_100_b/4_000_c-3757996767" 7.25 NIL) ("NIL/2_151_a/2_000_b-3757996767" 6.370117 NIL) ("T/4_150_b/4_000_b-3757996767" 7.75 T) ("T/1_101_z/1_000_z-3757996767" 5.0148926 T) ("T/2_050_c/2_000_c-3757996767" 6.449951 T) ("T/4_050_a/4_000_a-3757996767" 6.7351074 T) ("NIL/1_101_x/2_000_b-3757996767" 5.7648926 NIL) ("NIL/1_151_z/2_000_c-3757996767" 5.25 NIL) ("T/1_100_z/1_000_z-3757996767" 5.2299805 T) ("T/2_100_a/2_000_a-3757996767" 5.930176 T) ("T/3_150_c/3_000_c-3757996768" 7.0649414 T) ("T/4_151_b/4_000_b-3757996768" 7.449951 T) ("NIL/2_050_c/2_000_a-3757996768" 6.4299316 NIL) ("T/3_150_b/3_000_b-3757996768" 7.465088 T) ("T/4_051_c/4_000_c-3757996768" 6.4299316 T) ("T/4_150_a/4_000_a-3757996768" 7.13501 T) ("NIL/2_101_c/2_000_b-3757996768" 6.5 NIL) ("NIL/1_101_z/3_000_b-3757996768" 5.13501 NIL) ("T/1_051_y/1_000_y-3757996768" 5.465088 T) ("T/4_101_a/4_000_a-3757996768" 6.949951 T) ("T/2_101_b/2_000_b-3757996768" 6.0549316 T) ("NIL/1_150_z/2_000_b-3757996768" 5.4350586 NIL) ("T/1_050_z/1_000_z-3757996768" 6.13501 T) ("NIL/1_150_x/2_000_c-3757996768" 5.215088 NIL) ("T/2_150_c/2_000_c-3757996768" 6.7148437 T) ("T/4_151_a/4_000_a-3757996768" 7.0649414 T) ("NIL/2_101_a/2_000_c-3757996769" 6.215088 NIL) ("T/1_051_x/1_000_x-3757996769" 5.300049 T) ("NIL/3_151_c/3_000_a-3757996769" 6.834961 NIL) ("T/3_050_c/3_000_c-3757996769" 7.4851074 T) ("NIL/3_151_a/3_000_c-3757996769" 6.379883 NIL) ("NIL/1_150_y/3_000_c-3757996769" 5.2651367 NIL) ("T/2_150_b/2_000_b-3757996769" 6.2148437 T) ("T/4_101_b/4_000_b-3757996769" 7.255127 T) ("NIL/1_051_z/3_000_b-3757996769" 5.25 NIL) ("NIL/2_151_c/2_000_a-3757996769" 6.1499023 NIL) ("T/1_100_y/1_000_y-3757996769" 5.13501 T) ("NIL/2_050_b/2_000_a-3757996769" 6.6000977 NIL) ("T/1_050_y/1_000_y-3757996769" 5.5300293 T) ("T/1_150_x/1_000_x-3757996769" 5.449951 T) ("NIL/4_150_b/4_000_c-3757996769" 7.25 NIL) ("NIL/4_151_b/4_000_c-3757996769" 7.169922 NIL) ("T/4_050_c/4_000_c-3757996770" 7.25 T) ("NIL/4_051_b/4_000_a-3757996770" 6.88501 NIL) ("T/2_050_b/2_000_b-3757996770" 7.0148926 T) ("NIL/2_100_a/2_000_b-3757996770" 6.255127 NIL) ("T/2_051_c/2_000_c-3757996770" 6.915039 T) ("T/3_051_a/3_000_a-3757996770" 7.079834 T) ("NIL/4_100_c/4_000_b-3757996770" 7.300049 NIL) ("NIL/1_051_y/2_000_b-3757996770" 4.834961 NIL) ("NIL/1_100_y/3_000_b-3757996770" 5.3200684 NIL) ("T/1_151_z/1_000_z-3757996770" 5.61499 T) ("T/4_151_c/4_000_c-3757996770" 7.0300293 T) ("T/1_151_y/1_000_y-3757996770" 5.11499 T) ("T/3_051_b/3_000_b-3757996770" 6.6499023 T) ("T/4_051_b/4_000_b-3757996770" 7.0 T) ("T/1_101_x/1_000_x-3757996771" 5.0651855 T) ("T/3_151_a/3_000_a-3757996771" 7.550049 T) ("T/3_050_b/3_000_b-3757996771" 6.379883 T) ("T/1_151_x/1_000_x-3757996771" 5.0 T) ("T/3_051_c/3_000_c-3757996771" 6.61499 T) ("T/3_151_c/3_000_c-3757996771" 7.0649414 T) ("T/2_051_a/2_000_a-3757996771" 6.6000977 T) ("NIL/1_051_x/2_000_b-3757996771" 5.36499 NIL)) (("NIL/1_151_z/2_000_c-3757996771" 4.9799805 NIL) ("T/1_151_y/1_000_y-3757996771" 5.800049 T) ("T/1_151_x/1_000_x-3757996771" 5.800049 T) ("T/1_051_x/1_000_x-3757996771" 5.3498535 T) ("NIL/2_050_c/2_000_a-3757996771" 6.2851562 NIL) ("T/3_150_b/3_000_b-3757996771" 6.9799805 T) ("NIL/1_100_z/2_000_c-3757996771" 5.2299805 NIL) ("T/1_051_y/1_000_y-3757996771" 5.6499023 T) ("NIL/2_151_c/2_000_a-3757996772" 6.4851074 NIL) ("NIL/3_100_a/3_000_b-3757996772" 7.3999023 NIL) ("NIL/3_101_b/3_000_c-3757996772" 7.465088 NIL) ("T/2_151_c/2_000_c-3757996772" 6.569824 T) ("T/2_150_b/2_000_b-3757996772" 6.415039 T) ("NIL/1_150_x/2_000_c-3757996772" 5.0151367 NIL) ("NIL/2_100_b/2_000_c-3757996772" 6.25 NIL) ("T/3_101_c/3_000_c-3757996772" 7.7648926 T) ("T/4_050_c/4_000_c-3757996772" 7.11499 T) ("T/3_050_c/3_000_c-3757996772" 7.25 T) ("T/2_050_b/2_000_b-3757996772" 7.084961 T) ("NIL/1_050_z/2_000_b-3757996773" 4.930176 NIL) ("NIL/1_050_y/2_000_c-3757996773" 4.86499 NIL) ("NIL/1_051_x/2_000_b-3757996773" 5.2998047 NIL) ("T/4_151_b/4_000_b-3757996773" 6.7851562 T) ("T/4_150_a/4_000_a-3757996773" 8.0998535 T) ("NIL/2_051_c/2_000_b-3757996773" 6.080078 NIL) ("T/3_151_b/3_000_b-3757996773" 6.965088 T) ("NIL/1_151_x/3_000_b-3757996773" 4.9648437 NIL) ("T/1_150_z/1_000_z-3757996773" 5.4799805 T) ("NIL/3_050_a/3_000_c-3757996773" 6.2651367 NIL) ("T/2_051_a/2_000_a-3757996773" 6.080078 T) ("T/1_101_z/1_000_z-3757996773" 5.2148437 T) ("NIL/3_150_c/3_000_b-3757996773" 6.965088 NIL) ("NIL/4_051_a/4_000_c-3757996773" 7.0148926 NIL) ("T/2_050_a/2_000_a-3757996773" 6.8500977 T) ("T/3_151_c/3_000_c-3757996774" 7.36499 T) ("NIL/4_150_a/4_000_c-3757996774" 7.050049 NIL) ("NIL/3_051_c/3_000_a-3757996774" 6.379883 NIL) ("T/4_051_c/4_000_c-3757996774" 7.715088 T) ("NIL/3_151_a/3_000_c-3757996774" 6.665039 NIL) ("NIL/2_051_a/2_000_b-3757996774" 6.665039 NIL) ("T/2_100_b/2_000_b-3757996774" 6.7148437 T) ("NIL/1_100_y/3_000_b-3757996774" 5.0151367 NIL) ("NIL/4_100_c/4_000_b-3757996774" 6.5300293 NIL) ("NIL/3_051_a/3_000_c-3757996774" 7.0998535 NIL) ("T/2_051_b/2_000_b-3757996774" 7.0 T) ("T/4_151_c/4_000_c-3757996774" 6.915039 T) ("NIL/3_100_b/3_000_a-3757996774" 6.7800293 NIL) ("NIL/1_101_x/2_000_b-3757996774" 5.584961 NIL) ("T/2_150_a/2_000_a-3757996775" 6.1799316 T) ("T/1_050_x/1_000_x-3757996775" 5.0300293 T) ("NIL/2_150_b/2_000_c-3757996775" 7.0649414 NIL) ("T/2_101_b/2_000_b-3757996775" 6.5200195 T) ("NIL/2_150_a/2_000_b-3757996775" 6.5651855 NIL) ("T/4_051_b/4_000_b-3757996775" 7.8498535 T) ("T/1_101_x/1_000_x-3757996775" 6.4350586 T) ("T/3_051_c/3_000_c-3757996775" 7.6550293 T) ("T/3_101_b/3_000_b-3757996775" 6.784912 T) ("T/4_150_c/4_000_c-3757996775" 7.050049 T) ("T/4_150_b/4_000_b-3757996775" 6.6799316 T) ("NIL/1_051_z/3_000_b-3757996775" 5.0300293 NIL) ("NIL/4_150_b/4_000_c-3757996775" 7.169922 NIL) ("T/3_100_a/3_000_a-3757996775" 7.085205 T) ("T/3_100_c/3_000_c-3757996776" 6.7998047 T) ("T/4_101_b/4_000_b-3757996776" 6.8500977 T) ("NIL/4_101_c/4_000_a-3757996776" 7.120117 NIL) ("NIL/4_151_c/4_000_b-3757996776" 6.279785 NIL) ("NIL/3_150_b/3_000_c-3757996776" 7.2351074 NIL) ("NIL/2_150_c/2_000_b-3757996776" 6.3200684 NIL) ("T/4_050_b/4_000_b-3757996776" 7.86499 T) ("NIL/4_100_b/4_000_c-3757996776" 7.2148437 NIL) ("T/2_150_c/2_000_c-3757996776" 6.580078 T) ("T/4_151_a/4_000_a-3757996776" 7.0200195 T) ("T/2_100_c/2_000_c-3757996776" 6.415039 T) ("T/4_100_c/4_000_c-3757996776" 6.6550293 T) ("T/2_151_b/2_000_b-3757996776" 6.5998535 T) ("T/3_151_a/3_000_a-3757996777" 6.9350586 T) ("NIL/3_100_c/3_000_b-3757996777" 6.7700195 NIL) ("NIL/3_101_c/3_000_b-3757996777" 6.534912 NIL) ("NIL/3_101_a/3_000_c-3757996777" 8.015137 NIL) ("NIL/2_151_a/2_000_b-3757996777" 6.2348633 NIL) ("T/1_100_y/1_000_y-3757996777" 5.215088 T) ("T/1_100_x/1_000_x-3757996777" 5.0 T) ("T/3_150_a/3_000_a-3757996777" 7.88501 T) ("T/2_101_c/2_000_c-3757996777" 6.0700684 T) ("T/1_050_z/1_000_z-3757996777" 5.11499 T) ("NIL/4_100_a/4_000_b-3757996777" 7.11499 NIL) ("T/4_051_a/4_000_a-3757996777" 7.25 T) ("T/3_050_a/3_000_a-3757996777" 7.034912 T) ("T/4_100_a/4_000_a-3757996778" 6.9699707 T) ("T/3_101_a/3_000_a-3757996778" 6.699951 T) ("NIL/4_101_a/4_000_c-3757996778" 6.9350586 NIL) ("T/2_050_c/2_000_c-3757996778" 6.915039 T) ("T/1_151_z/1_000_z-3757996778" 5.0300293 T) ("T/1_051_z/1_000_z-3757996778" 5.0 T) ("NIL/2_050_b/2_000_a-3757996778" 6.6499023 NIL) ("T/3_051_a/3_000_a-3757996778" 7.5 T) ("NIL/1_101_z/3_000_b-3757996778" 5.0151367 NIL) ("T/4_101_c/4_000_c-3757996778" 7.75 T) ("T/1_150_y/1_000_y-3757996779" 5.0148926 T) ("NIL/1_150_y/3_000_c-3757996779" 4.9299316 NIL) ("T/1_100_z/1_000_z-3757996779" 5.25 T) ("NIL/2_151_b/2_000_c-3757996779" 6.9851074 NIL) ("T/4_100_b/4_000_b-3757996779" 8.300049 T) ("NIL/1_051_y/2_000_b-3757996779" 5.4699707 NIL) ("NIL/3_150_a/3_000_b-3757996779" 6.584961 NIL) ("T/2_051_c/2_000_c-3757996779" 7.25 T) ("NIL/4_050_c/4_000_b-3757996779" 6.7800293 NIL) ("T/1_150_x/1_000_x-3757996779" 5.215088 T) ("T/4_050_a/4_000_a-3757996779" 6.7148437 T) ("T/1_101_y/1_000_y-3757996779" 5.550049 T) ("NIL/4_101_b/4_000_a-3757996779" 6.3149414 NIL) ("T/3_050_b/3_000_b-3757996779" 7.699951 T) ("T/3_100_b/3_000_b-3757996780" 6.9350586 T) ("T/3_150_c/3_000_c-3757996780" 7.5151367 T) ("T/1_050_y/1_000_y-3757996780" 5.0148926 T) ("T/4_101_a/4_000_a-3757996780" 6.380127 T) ("T/2_151_a/2_000_a-3757996780" 6.8149414 T) ("T/3_051_b/3_000_b-3757996780" 7.2299805 T) ("T/2_100_a/2_000_a-3757996780" 6.5148926 T) ("T/2_101_a/2_000_a-3757996780" 6.5300293 T))))

; (defun read-puzzle-id (id)
	; (let* ((new-string (split-str id "/")) (target (split-str (second new-string) "_")) (cube (split-str (third new-string) "_")) (same (first new-string)) (level (first target)) (variant (second target)) (targetdir 	(third target)) (cubelevel (first cube)) (cubedir (first (split-str (third cube) "-"))))
		; (concatenate 'string same ";" level ";" variant ";" targetdir ";" cubelevel ";000;" cubedir)))