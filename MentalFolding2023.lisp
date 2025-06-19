;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    "Mental Spatial Transformation: Mental Folding Task", Experiment, GUI & Spatial Model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    July 2019 by Kai Preuß
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	2023 Rework: Revised pattern path encoding, bugfixing, revised path-wise instance-learning, focus on data provided by Hilton et al., 2021
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(written-for-act-r-version "7.14")

(format t "***** ***** ***** ***** ~%This is a model for a variation ~%of the Mental Folding task, originally ~%by Shepard & Feng (1972).~%You can run a random trial with (testr) ~%or 5 * 120 trials with (mf-experiment).~%
For further available commands, please see the program code.~%  -K.P. ~%***** ***** ***** ***** ~%")

; --------------------------------
(defparameter *scale* 15)

(defconstant *arrow-list* (list 'A< 'Av 'A> 'A^))
(defconstant *arrow-vectors* (list (list (/ (- *scale*) 2) 0 0) (list 0 (/ (- *scale*) 2) 0) (list (/ *scale* 2) 0 0) (list 0 (/ *scale* 2) 0)))

; human data, for comparison: 1A 1F 1G 1H 2A 2F 2G 2H 3A ......... 5H

; human data (target onset)
;(defconstant *human-data* '(1.2654347 2.5051811 3.1496520 4.5114348
;							1.0500366 2.1293573 2.5371838 3.6829411
;							0.9544558 1.9400244 2.1969687 3.0079605
;							0.9303298 1.7697596 2.0600923 2.5977602
;							0.8910224 1.6210561 1.8523751 2.3576971))

; human data (reference onset, +1s to target onset)
; (defconstant *human-data* '(2.2654347 3.5051811 4.1496520 5.5114348
							; 2.0500366 3.1293573 3.5371838 4.6829411
							; 1.9544558 2.9400244 3.1969687 4.0079605
							; 1.9303298 2.7697596 3.0600923 3.5977602
							; 1.8910224 2.6210561 2.8523751 3.3576971))
							
; final human data as used in EEG analysis (Hilton et al., 2021), reference onset
(defconstant *human-data* '(1.990813 3.057831 3.265040 3.918048
							1.875912 2.832748 3.007735 3.471235
							1.811087 2.747274 2.826597 3.309653
							1.769658 2.629035 2.716629 3.121595
							1.751164 2.493054 2.585335 3.013850))
; best model parameter values for this: ; lf: 0.2 rt: -1.8 ans: nil egs: 1 alpha: 0.2 s-delay: 0.001 s-complexity-factor: 0.5

; --------------------------------
;; Variables
; User Variables
;
; Where are the program files?
(defparameter *use-custom-directory* nil) ; if user-defined directory should be used. if not, uses ACT-R/folding/ for component-load
(defparameter *custom-directory* "~/folding/") ; user-defined directory

(defparameter *sim-directory* (concatenate 'string *custom-directory* (namestring (pathname "Simulation_Output_Nov2022_250Hz/"))))
(defparameter *source-data* (concatenate 'string *custom-directory* (namestring (pathname "MF-simulation-input.csv"))))

; Run for show or for data collection?
(defparameter *do-in-real-time* t) ; if model should solve puzzle in real time (good for showing off & debugging)
(defparameter *visible-experiment-window* t) ; sets the virtual window to visible (t) or virtual (nil)
(defparameter *enable-trace* t) ; if the CLI trace should be enabled
(defparameter *use-seed-value* nil) ; if the model should always use the same randomization, leading to same outcomes. Past interesting values: 23456, led to weird error [fixed]

(defparameter *reward* 15)
(defparameter *initial-utility* 12) ; 15 reward / 10 IU seems like a good combination
; not in use anymore ;(defparameter *shortcut-utility* 10) ; good value: 8. interestingly, *very* sensitive to retrieval latency / success - if latency is low, no reason to try out shortcuts apparently
(defparameter *enable-sc* t)
(defparameter *enable-utility-learning* t)
(defparameter *utility-noise* 1)
(defparameter *utility-rate* 0.2) ; default: 0.2
(defparameter *enable-production-compilation* nil)
(defparameter *pc-trace* nil)
(defparameter *enable-bl-learning* 0.5) ;0.5
(defparameter *activation-noise* 0.5) ; default: nil, recommended between 0.2 and 0.8
(defparameter *enable-partial-matching* nil)

(defparameter *enable-shortcuts* t) ; allows shortcuts if the arrows (can't) point towards each other and (don't) point to a square
(defparameter *enable-advanced-shortcuts* nil) ; allows shortcuts if the arrows (don't) have a 90 degree relationship with each other. only possible if shortcuts are active!
(defparameter *debug* nil)

; possible parameters: :visual-attention-latency, :lf, :rt, :spatial-delay
(defparameter *visual-latency* 0.085) ; default: 0.085
(defparameter *visual-num-finsts* 10); default 4
(defparameter *visual-finst-span* 10); default 3
(defparameter *declarative-finst-span* 10); default 3
(defparameter *latency* .2) ; default 1, best below .5, but .2 works best by far
(defparameter *retrieval-threshold* -1.8) ; default 0, safely working with -1 and -2
(defparameter *spatial-delay* 0.001) ; default 0.01, best values for folding seem to be between 0.0005 and 0.001
(defparameter *spatial-complexity* 10) ; default 6
(defparameter *spatial-complexity-factor* 0.5) ; default 0.02, best values seems to depend on retrieval threshold. 0.02 - 1 tested successfully, but maybe stick to the lower end so instance retrieval isn't too powerful. 0.1 mayhaps?
(defparameter *angle-threshold* 10)
(defparameter *mental-display-delay* 0); if the display showing spatial operations should slow down, for demonstration purposes

; Run randomly and/or only short durations?
(defparameter *runtime* 30) ; maximum allowed runtime for model in seconds (3600 = 60 minutes)

; window variables
(defparameter *experiment-window* nil) ; experiment window
(defparameter *mental-window* nil)
(defparameter *mental-display* nil)
(defvar *camera-pos* '(0 0 0))
(defvar *camera-dir* '(0 0 0))

(defparameter *stage-markers* nil) ; Variable for collecting markers of cognitive stages
(defparameter *stage-markers-output* nil)

(defparameter *actr-enabled-p* t) ; if human or model should solve puzzle
(defvar *window-width* 640) ; 800
(defvar *window-height* 480) ; 600
(defparameter *focus-distance* 100)
(defparameter *processing-cam-dir* '(0 0 0))
(defparameter *processing-cam-pos* '(0 0 0))
(defparameter *trial-cam-dir* '(15 -10 0))	; default (15 -10 0)
(defparameter *trial-cam-pos* '(18 30 -10))	; default (12 15 30)
(defvar *left-stimulus-position* (list (/ *window-width* 4) (/ *window-height* 2))) ; center of left half of screen
(defvar *right-stimulus-position* (list (* 3 (/ *window-width* 4)) (/ *window-height* 2))) ; center of right half of screen
(defvar *cube-position* nil) ; if cube is left or right
(defvar *pattern-position* nil) ; if folding pattern is left or right
(defvar *cube-stimulus* nil)
(defvar *pattern-stimulus* nil)
(defvar *pattern-paths* nil)
(defparameter *initial-cube-rotation* '(-25 20 -10))

; (current-id (concatenate 'string (first stimulus) (format nil "-~d" (act-r-random 9999999999))))
(defvar *puzzle-identifier* nil) ; Variable for holding puzzle ID
(defvar *response* 0) ; input key
(defvar *correct-response* nil) ; correct key
(defvar *current-stimulus* nil)
(defvar *left-stimulus-drawn* nil) ; If left window side is already taken
(defvar *prompted* nil) ; allows user input
(defvar *done* nil) ; experiment is done
(defvar *visual-feature-list* nil)

; --------------------------------
;; Loads model components either from ACT-R directory or from user-defined directory, depending on *use-custom-directory*
(defun component-load (filename)
  (if *use-custom-directory*
    (load (pathname (concatenate 'string *custom-directory* filename)))
    (load (pathname (concatenate 'string (namestring (pathname "../folding/")) filename))))
)

;; Components to load
(component-load "MF-Stimuli.lisp")
;;
; --------------------------------
(defun display-points (stimulus &optional (window *experiment-window*))
	(let ((displayed-items nil))
		(dolist (object stimulus)
			(let* ((points (car (cdr (car (cdr (member 'points object)))))) (color (cadr (member 'color object))) (centercoords (mapcar #'calculate-projection points))
					(xy (mapcar #'convert-centered-coordinates-to-screen centercoords)))
				(case (length xy)
					; (2 (push (add-line-to-exp-window *experiment-window* (first xy) (second xy) color) displayed-items)) ; maybe comment out later?
					(2 nil) ; do nothing for "fold" objects (the lines between squares)
					(4
						(push (add-line-to-exp-window window (first xy) (second xy) color) displayed-items)
						(push (add-line-to-exp-window window (second xy) (third xy) color) displayed-items)
						(push (add-line-to-exp-window window (third xy) (fourth xy) color) displayed-items)
						(push (add-line-to-exp-window window (fourth xy) (first xy) color) displayed-items))
					(6
						(push (add-line-to-exp-window window (first xy) (second xy) color) displayed-items)
						(push (add-line-to-exp-window window (second xy) (third xy) color) displayed-items)
						(push (add-line-to-exp-window window (third xy) (fourth xy) color) displayed-items)
						(push (add-line-to-exp-window window (fourth xy) (first xy) color) displayed-items)
						(push (add-line-to-exp-window window (fifth xy) (sixth xy) 'red) displayed-items))
					(t
						(one-time-model-warning "unknown point cloud" "Don't know how to display an object with ~a points, so just parsing point cloud as Xs" (length xy))
						(dolist (coord xy)
							(add-text-to-exp-window *experiment-window* "X" :x (first coord) :y (second coord) :color 'black)))
					)))
		displayed-items))
		
(defun update-points (spacechunk stim-display window)
	(let ((displayed-items nil))
		(if stim-display (dolist (item stim-display) (remove-items-from-exp-window window item)))
		(if (or (eq spacechunk 'clear) (eq spacechunk nil) (eq (chunk-slot-value-fct spacechunk 'points) nil)) nil
			(let* ((pts (chunk-slot-value-fct spacechunk 'points)) (delims (chunk-slot-value-fct spacechunk 'delimiters)) (space-objs (degroup-objects pts delims))
					(aspects (chunk-slot-value-fct spacechunk 'aspects)))
				(dolist (object space-objs)
					(let* ((centercoords (mapcar #'(lambda (obj) (calculate-projection obj nil *processing-cam-dir* *processing-cam-pos*)) object)) (xy (mapcar #'convert-centered-coordinates-to-screen centercoords)))
						(case (length xy)
							; (2 nil) ; do nothing for "fold" objects (the lines between squares)
							(2
								(push (add-line-to-exp-window window (first xy) (second xy) 'light-gray) displayed-items)
								; horizonal line to show arrow tip
								(push (add-line-to-exp-window window (-list (second xy) '(10 0)) (+list (second xy) '(10 0)) 'light-gray) displayed-items)
								; vertical line to show arrow tip
								(push (add-line-to-exp-window window (-list (second xy) '(0 10)) (+list (second xy) '(0 10)) 'light-gray) displayed-items)
								; diagonal lines to show arrow tip
								(push (add-line-to-exp-window window (-list (second xy) '(10 0)) (+list (second xy) '(0 10)) 'light-gray) displayed-items)
								(push (add-line-to-exp-window window (+list (second xy) '(0 10)) (+list (second xy) '(10 0)) 'light-gray) displayed-items)
								(push (add-line-to-exp-window window (+list (second xy) '(10 0)) (-list (second xy) '(0 10)) 'light-gray) displayed-items)
								(push (add-line-to-exp-window window (-list (second xy) '(0 10)) (-list (second xy) '(10 0)) 'light-gray) displayed-items)
								)
							(4
								(let ((color (if (eq 'base (nth (position object space-objs) aspects)) 'blue 'black)))
									(push (add-line-to-exp-window window (first xy) (second xy) color) displayed-items)
									(push (add-line-to-exp-window window (second xy) (third xy) color) displayed-items)
									(push (add-line-to-exp-window window (third xy) (fourth xy) color) displayed-items)
									(push (add-line-to-exp-window window (fourth xy) (first xy) color) displayed-items)))
							(6
								(push (add-line-to-exp-window window (first xy) (second xy) 'black) displayed-items)
								(push (add-line-to-exp-window window (second xy) (third xy) 'black) displayed-items)
								(push (add-line-to-exp-window window (third xy) (fourth xy) 'black) displayed-items)
								(push (add-line-to-exp-window window (fourth xy) (first xy) 'black) displayed-items)
								(push (add-line-to-exp-window window (fifth xy) (sixth xy) 'red) displayed-items))
							(t
								(one-time-model-warning "unknown point cloud" "Don't know how to display an object with ~a points, so just parsing point cloud as Xs" (length xy))
								(dolist (coord xy)
									(add-text-to-exp-window window "X" :x (first coord) :y (second coord) :color 'black))))))
				;;;
				;;;
				;;;
				;;;
				;;;
				(if (> *mental-display-delay* 0) (sleep *mental-display-delay*))
				))
		displayed-items))

(defun update-sim (stimulus)
	(setf *mental-display* (update-points stimulus *mental-display* *mental-window*)))
	
(defun update-sim-hook (production)
	(declare (ignore production))
	(setf *mental-display* (update-points (first (suppress-act-r-output (buffer-chunk spatial))) *mental-display* *mental-window*)))


; ; Version with inverting y-Axis
; (defun convert-centered-coordinates-to-screen (coords)
	; (let ((center (list (/ *window-width* 2) (/ *window-height* 2))) (newcoords (list (first coords) (second coords))))
		; (+list center newcoords)))
		
; ; Version with proper y-Axis
(defun convert-centered-coordinates-to-screen (coords)
	(list (+ (/ *window-width* 2) (first coords)) (- (/ *window-height* 2) (second coords))))

; ; Version with inverting y-Axis
; (defun convert-screen-to-centered-coordinates (coords)
	; (let ((center (list (/ *window-width* 2) (/ *window-height* 2))) (newcoords (list (first coords) (second coords))))
		; (-list newcoords center)))
		
; ; Version with proper y-Axis
(defun convert-screen-to-centered-coordinates (coords)
	(list (- (first coords) (/ *window-width* 2)) (+ (second coords) (/ *window-height* 2))))
		
(defun convert-vector-to-angle (vector)
	(/ (* (atan (first vector) (second vector)) 180) pi))
	
(defun convert-angle-to-vector (angle)
	(list (cos (/ (* angle pi) 180)) (sin (/ (* angle pi) 180))))
	
(defun calculate-projection (vector3 &optional (simple nil) (camera-dir *camera-dir*) (camera-pos *camera-pos*) (focal 1024)) ; (focal 1024)
	(if (not (boundp '*camera-pos*)) (defparameter *camera-pos* '(0 0 0)))
	(if (not (boundp '*camera-dir*)) (defparameter *camera-dir* '(0 0 0)))
	(if (not (boundp '*window-width*)) (defparameter *window-width* 200))
	(if (not (boundp '*window-height*)) (defparameter *window-height* 200))
	(if simple (return-from calculate-projection (butlast vector3)))
	(let* ((x3d (- (first vector3) (first camera-pos)))
			(y3d (- (second vector3) (second camera-pos))) (z3d (- (third vector3) (third camera-pos)))
			(cam-angles (mapcar #'(lambda (x) (setf x (/ (* x pi) 180))) camera-dir))
			(cam-angle-x (first cam-angles)) (cam-angle-y (second cam-angles)) (cam-angle-z (third cam-angles))
			(camera-x3d (- (* (cos cam-angle-y) (+ (* (sin cam-angle-z) y3d) (* (cos cam-angle-z) x3d))) (* (sin cam-angle-y) z3d)))
			(camera-y3d (+ (* (sin cam-angle-x) (+ (* (cos cam-angle-y) z3d) (* (sin cam-angle-y) (+ (* (sin cam-angle-z) y3d) (* (cos cam-angle-z) x3d)))))
							(* (cos cam-angle-x) (- (* (cos cam-angle-z) y3d) (* (sin cam-angle-z) x3d)))))
			(camera-z3d (- (* (cos cam-angle-x) (+ (* (cos cam-angle-y) z3d) (* (sin cam-angle-y) (+ (* (sin cam-angle-z) y3d) (* (cos cam-angle-z) x3d)))))
							(* (sin cam-angle-x) (- (* (cos cam-angle-z) y3d) (* (sin cam-angle-z) x3d)))))
			(plane (list 0 0 focal)))
		(if (not (= camera-z3d 0))
			(let ((screen-x (+ (* (/ (third plane) camera-z3d) camera-x3d) (first plane)))
					(screen-y (+ (* (/ (third plane) camera-z3d) camera-y3d) (second plane))))
				(list (round screen-x) (round screen-y)))
			(let ((screen-x 0) (screen-y 0))
				(list screen-x screen-y)))))

(defun foldarray-to-list (array)
  (let ((foldline nil) (foldlist nil))
    (dotimes (j 4)
      (dotimes (i 4)
        (setf foldline (append foldline (list (aref array j i)))))
      (setf foldlist (append foldlist (list foldline)))
      (setf foldline nil))
    foldlist))



; --------------------------------
; Groups together several point clouds and their respective limiting positions in the list.
; Used for transforming objects and their edges (other objects will get folded in separate steps to keep in line with SSC principle)
; Variant A: group together multiple objects and return the group and their point cloud sizes
; (defun group-objects (&rest objects)
  ; (let ((result nil) (limiters nil))
    ; (dolist (object objects)
      ; (setf result (append result object))
      ; (setf limiters (append limiters (list (length object)))))
    ; (list result limiters)))
	
; Variant B: add an object to another object (includes groups), returns group and length of last object
(defun group-objects (&rest objects)
  (let ((result nil))
    (dolist (object objects)
      (setf result (append result object)))
    (list result (length (first (cdr objects))))))

; Degroups objects formerly grouped together into one object for folding purposes.
; Takes the meta object and delimiters (i.e., length of original point clouds) as input.
(defun degroup-objects (object delimiters)
	(let ((result nil) (old-delimiter 0))
		(dolist (delimiter delimiters)
			(setf result (nconc result (list (subseq object old-delimiter (+ old-delimiter delimiter)))))
			(setf old-delimiter (+ old-delimiter delimiter)))
		result))

; Takes a group of objects and flattens their structure, so that all coordinates appear in one single list.	
(defun flatten-group (group)
	(let ((superlist nil))
		(dolist (object group)
			(setf superlist (append superlist object)))
		superlist))

; Checks if two points clouds are connected in at least 2 points
(defun is-connected (obj1 obj2)
	(let ((shared-points 0))
		(dolist (obj1-pts obj1)
			(dolist (obj2-pts obj2)
				(if (equal obj1-pts obj2-pts) (setf shared-points (1+ shared-points)))))
		(if (>= shared-points 2) t nil)))
		
; Returns the points that are equal in two lists of lists, e.g. returns coordinates where two point clouds are connected
(defun is-connected-where (obj1 obj2)
	(let ((shared-points nil))
		(dolist (obj1-pts obj1)
			(dolist (obj2-pts obj2)
				(if (equal obj1-pts obj2-pts) (setf shared-points (append shared-points (list obj1-pts))))))
		shared-points))
		
;;; !safe-eval!		(arrows-meet (return-from-group (all-positions '(arrow base&arrow) =aspects) =delims =points))
	
(defun arrows-meet (arrowlist)
	(equal (mapcar #'floor (nth 5 arrowlist)) (mapcar #'floor (nth 11 arrowlist))))

;;; !safe-eval! (arrows-90degrees-shortcut (=points =aspects))
; Checks to see if the "arrows are in a 90 degree angle and don't point onto a square so they must meet when folded"-shortcut is possible. Black magic!
(defun arrows-90degrees-shortcut (points aspects)
	; are arrows in a 90 / -90 degree angle? AND
	; neither point to the center point of the middle face?
	(if (position-if #'(lambda (x) (or (equal x 'arrow) (equal x 'base&arrow))) aspects) (progn
		(let* ((new-aspects (subseq aspects (position-if #'(lambda (x) (or (equal x 'arrow) (equal x 'base&arrow))) aspects) (length aspects)))
				(cutoff-counter 0)
				(new-cutoff (progn (dolist (aspect new-aspects) (incf cutoff-counter (case aspect (edge 2) (face 4) (base 4) (base&arrow 6) (arrow 6)))) cutoff-counter))
				(new-points (reverse (subseq (reverse points) 0 new-cutoff)))
				(arrow1 (subseq new-points 4 6)) (arrow2 (reverse (subseq (reverse new-points) 0 2)))
				(arrowangle (round (compare-vector-angle (-list (second arrow1) (first arrow1)) (-list (second arrow2) (first arrow2)))))
				(facepts (subseq new-points 8 12)) (facecenter (get-center facepts))
				(eqldim1 (if (= (first (first arrow1)) (first (second arrow1))) 0 (if (= (second (first arrow1)) (second (second arrow1))) 1 nil)))
				(eqldim2 (if (= (first (first arrow2)) (first (second arrow2))) 0 (if (= (second (first arrow2)) (second (second arrow2))) 1 nil))))
			(if (and (not (= (nth eqldim1 (first arrow1)) (nth eqldim1 facecenter))) (not (= (nth eqldim2 (first arrow2)) (nth eqldim2 facecenter))) (= (abs arrowangle) 90)) t nil)))))

; As of right now, these productions still produce ambivalent results - any 90 degree relationship counts
(defun arrows-90degrees (points aspects)
		(if (position-if #'(lambda (x) (or (equal x 'arrow) (equal x 'base&arrow))) aspects) (progn
		(let* ((new-aspects (subseq aspects (position-if #'(lambda (x) (or (equal x 'arrow) (equal x 'base&arrow))) aspects) (length aspects)))
				(cutoff-counter 0)
				(new-cutoff (progn (dolist (aspect new-aspects) (incf cutoff-counter (case aspect (edge 2) (face 4) (base 4) (base&arrow 6) (arrow 6)))) cutoff-counter))
				(new-points (reverse (subseq (reverse points) 0 new-cutoff)))
				(arrow1 (subseq new-points 4 6)) (arrow2 (reverse (subseq (reverse new-points) 0 2)))
				(arrowangle (round (compare-vector-angle (-list (second arrow1) (first arrow1)) (-list (second arrow2) (first arrow2)))))
				; find out distance to see if tips of arrows are closer than nock
				(distance-t (sqrt (+ (sq (- (second (second arrow2)) (second (second arrow1)))) (sq (- (first (second arrow1)) (first (second arrow2)))))))
				(distance-n (sqrt (+ (sq (- (second (first arrow2)) (second (first arrow1)))) (sq (- (first (first arrow1)) (first (first arrow2)))))))
				)
			; (if (= (abs arrowangle) 90) t nil)))))
			(if (and (= (abs arrowangle) 90) (< distance-t distance-n)) t nil)))))
			
(defun arrows-cannot-meet (points aspects)
	(if (position-if #'(lambda (x) (or (equal x 'arrow) (equal x 'base&arrow))) aspects) (progn
	(let* ((new-aspects (subseq aspects (position-if #'(lambda (x) (or (equal x 'arrow) (equal x 'base&arrow))) aspects) (length aspects)))
			(cutoff-counter 0)
			(new-cutoff (progn (dolist (aspect new-aspects) (incf cutoff-counter (case aspect (edge 2) (face 4) (base 4) (base&arrow 6) (arrow 6)))) cutoff-counter))
			(new-points (reverse (subseq (reverse points) 0 new-cutoff)))
			(arrow1 (subseq new-points 4 6)) (arrow2 (reverse (subseq (reverse new-points) 0 2)))
			(facepts (subseq new-points 8 12)) (facecenter (get-center facepts))
			(eqldim1 (if (= (first (first arrow1)) (first (second arrow1))) 0 (if (= (second (first arrow1)) (second (second arrow1))) 1 nil)))
			(eqldim2 (if (= (first (first arrow2)) (first (second arrow2))) 0 (if (= (second (first arrow2)) (second (second arrow2))) 1 nil))))
		(if (and (= (nth eqldim1 (first arrow1)) (nth eqldim1 facecenter)) (= (nth eqldim2 (first arrow2)) (nth eqldim2 facecenter))) t nil)))))
			
; Returns the objects shared by two structures
(defun shares-objects (obj1 delims1 obj2 delims2)
	(let ((obj1-list nil) (obj2-list nil) (counter 0))
		(dolist (delim delims1)
			(setf obj1-list (append obj1-list (list (subseq obj1 counter (+ counter delim)))))
			(setf counter (+ counter delim)))
		(setf counter 0)
		(dolist (delim delims2)
			(setf obj2-list (append obj2-list (list (subseq obj2 counter (+ counter delim)))))
			(setf counter (+ counter delim)))
		(flatten-group (is-connected-where obj1-list obj2-list))))

; Returns points belonging to the delimiters at the indexed positions
(defun return-from-group (indices delims points)
	(let ((delcounter 0) (new-points nil))
		(dotimes (n (length delims))
			(dolist (i indices)
				(if (= n i)
					(setf new-points (append new-points (subseq points delcounter (+ delcounter (nth n delims)))))))
		(setf delcounter (+ delcounter (nth n delims))))
    new-points))

; Reverses a spatial structure while keeping its objects intact
(defun spatial-reverse (lst delims)
	(let ((new-list nil) (current-nth 0))
		(dolist (delim delims)
		;(print (subseq lst current-nth (+ current-nth delim)))
			(setf new-list (append (subseq lst current-nth (+ current-nth delim)) new-list))
			(setf current-nth (+ current-nth delim)))
		new-list))

; Creates csv-style strings out of list elements
(defun list-to-csv (list)
	(let ((csv-string ""))
		(dotimes (n (length list))
			(if (not (eq n (- (length list) 1)))
				(setf csv-string (concatenate 'string csv-string (write-to-string (nth n list)) ";"))
				(setf csv-string (concatenate 'string csv-string (write-to-string (nth n list))))))
		csv-string))
		
(defun csv-to-list (line)
	(mapcar (lambda (str) (if (not (ignore-errors (parse-integer (string-trim "\"" str)))) (string-trim "\"" str) (parse-integer (string-trim "\"" str)))) (split-str line ",")))

; Splits strings by the chosen separator (default: space)
(defun split-str (string &optional (separator " "))
  (split-1 string separator))

; Helper function for correct recursion of split-str
(defun split-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r)) (cons string r))))
	
(defun split-by (string &optional (delimiter #\;))
	(loop for i = 0 then (1+ j)
		as j = (position delimiter string :start i)
		collect (subseq string i j)
		while j))

(defun csv (&optional (source-data *source-data*))
	(read-from-csv source-data))
	
; Reads from csv files and creates a list of list, with each list representing one line (works with IDEs, but not with Standalone for...some reason?)
(defun read-from-csv (filepath)
	(with-open-file (stream filepath :direction :input)
		(let ((newlist nil) (entry nil))
			(loop for line = (read-line stream nil)
				while line do
					;(setf entry (mapcar (lambda (str) (if (not (ignore-errors (parse-integer (string-trim "\"" str)))) (string-trim "\"" str) (parse-integer (string-trim "\"" str)))) (split-str line ",")))
					(setf entry (mapcar (lambda (str) (string-trim "\"" str)) (split-str line ",")))
					(setf newlist (append newlist (list entry))))
			newlist)))

; Creates csv-files out of a list of lists (with each list resulting in a line). Input is considered to be long format ("list of rows").
(defun write-to-csv (results filepath)
	(with-open-file (str (pathname filepath)
		:direction :output
		:if-exists :overwrite
		:if-does-not-exist :create)
		(dolist (line results)
			(format str (concatenate 'string (list-to-csv line) "~%")))) T)

; As above, but input is in wide format ("list of columns").
(defun write-to-csv2 (results filepath)
	(with-open-file (str (pathname filepath)
		:direction :output
		:if-exists :overwrite
		:if-does-not-exist :create)
		(let* ((columns (length results)) (rows (length (first results))) (line nil))
			(dotimes (row rows)
				(dotimes (column columns)
					(setf line (append line (list (nth row (nth column results))))))
				(format str (concatenate 'string (list-to-csv line) "~%"))
				(setf line nil)))) T)
	
; Like 'position', returns the positions of its search elements, but all positions.	
(defun all-positions (search lst)
	(let ((collector nil))
		(dotimes (n (length lst))
			(dolist (elem search)
				(if (eql (nth n lst) elem)
					(setf collector (append collector (list n))))))
		collector))
	
(defun all-nths (indices lst)
	(let ((collector nil))
		(dolist (i indices)
			(setf collector (append collector (list (nth i lst)))))
		collector))



; --------------------------------
; amazing idea i had: first create the cubeface, then transform it into position - no more trouble with correct arrow adjustments! amazing 
; is now current version. also: arrow texture is now part of cube object - regular cube object has 4 points, with arrow texture 6 (slot 'texture' now says 'arrow')!

(defun create-3d-cube (stimulus &optional (bugfix nil))
	(let ((cube-pattern (first (second stimulus))) (face-pos 0) (scale *scale*)
			(offset-translation (append *cube-position* (list *focus-distance*)))
			;(offset-translation (list -20 0 *focus-distance*))
			(offset-rotation *initial-cube-rotation*)
			(visual-chunks nil))
		(dolist (face cube-pattern)
			(let ((cube-face (list `(,(- (* scale 0.5)) ,(- (* scale 0.5)) 0) `(,(+ (* scale 0.5)) ,(- (* scale 0.5)) 0) `(,(+ (* scale 0.5)) ,(+ (* scale 0.5)) 0) `(,(- (* scale 0.5)) ,(+ (* scale 0.5)) 0)))
					(cube-center (list 0 0 0)) (base 'black) (arrow nil))
				(if (search "A" (symbol-name face))
					(progn
						; get arrow vector...
						(setf arrow (nth (position (find-symbol (string-trim "BX" (symbol-name face))) *arrow-list*) *arrow-vectors*))
						; ...and make it a two point list (face center, arrow)...
						(setf arrow (list cube-center (+list cube-center arrow)))
						(if bugfix (format t "Arrow vector is: ~a~%" arrow))
						; ...then append it to rest of cubeface
						(setf cube-face (append cube-face arrow))
						(if bugfix (format t "Arrow found in position ~a: ~a~%" face-pos arrow))))
				(case face-pos
					; Front
					(0 (setf cube-face (mapcar #'(lambda (face) (funcall #'translate-by-xyz face (list 0 0 (- (* scale 0.5))))) cube-face)))
					; Top
					(1 (progn
						(setf cube-face (mapcar #'(lambda (face) (funcall #'rotate-around-x face 90)) cube-face))		;;; check if rotation direction is correct!
						(setf cube-face (mapcar #'(lambda (face) (funcall #'translate-by-xyz face (list 0 (* scale 0.5) 0))) cube-face))))			
					; Left
					(2 (progn
						(setf cube-face (mapcar #'(lambda (face) (funcall #'rotate-around-y face -90)) cube-face))		;;; check if rotation direction is correct!
						(setf cube-face (mapcar #'(lambda (face) (funcall #'translate-by-xyz face (list (- (* scale 0.5)) 0 0))) cube-face))))			
					; Back
					(3 (setf cube-face (mapcar #'(lambda (face) (funcall #'translate-by-xyz face (list 0 0 (* scale 0.5)))) cube-face)))
					; Right
					(4 (progn
						(setf cube-face (mapcar #'(lambda (face) (funcall #'rotate-around-y face -90)) cube-face))		;;; check if rotation direction is correct!
						(setf cube-face (mapcar #'(lambda (face) (funcall #'translate-by-xyz face (list (* scale 0.5) 0 0))) cube-face))))
					; Bottom
					(5 (progn
						(setf cube-face (mapcar #'(lambda (face) (funcall #'rotate-around-x face 90)) cube-face))		;;; check if rotation direction is correct!
						(setf cube-face (mapcar #'(lambda (face) (funcall #'translate-by-xyz face (list 0 (- (* scale 0.5)) 0))) cube-face))
						(setf base 'blue)))
					(t (print "Too many cube faces!")))
				; Offset Rotation
				(setf cube-face (mapcar #'(lambda (face) (funcall #'rotate-around-xyz face offset-rotation)) cube-face))
				; Offset Translation
				(setf cube-face (mapcar #'(lambda (face) (funcall #'translate-by-xyz face offset-translation)) cube-face))
				(incf face-pos)
				; Projecting center on screen to get screen-x and screen-y coordinates
				(setf cube-center (convert-centered-coordinates-to-screen (calculate-projection (get-center cube-face))))

				(setf visual-chunks (append visual-chunks `(
					(isa (spatial-feature spatial-visual) screen-x ,(first cube-center) screen-y ,(second cube-center) kind (cube nil) class (nil cube) type face color ,base
						texture ,(if arrow 'arrow) points ,(list nil cube-face) width ,(list nil scale) height ,(list nil scale)))))))
		visual-chunks))
		


(defun create-3D-folding-pattern (stimulus &optional (bugfix nil))
  (let ((fold-pattern (reverse (foldarray-to-list (second (second stimulus))))) (former-line nil) (former-field nil) (temp-line nil) (temp-field nil) (temp-center nil) (temp-fold nil) (temp-foldcenter nil)
        (base-pos nil) (arrow1-pos) (arrow2-pos) (arrow-dir) (scale (* *scale* 0.5))
		(offset-translation (append *pattern-position* (list *focus-distance*)))
		;(offset-translation (list 10 0 *focus-distance*))
		(visual-chunks nil))
	(dotimes (line (length fold-pattern))
	  ; has Base?
	  (if (position-if #'(lambda (x) (if x (equal "B" (subseq (symbol-name x) 0 1)))) (nth line fold-pattern))
		(setf base-pos (list line (position-if #'(lambda (x) (if x (equal "B" (subseq (symbol-name x) 0 1)))) (nth line fold-pattern)))))
	  ; has Arrow?
	  ; (if (position-if #'(lambda (x) (if x (search "A" (symbol-name x)))) (nth line fold-pattern))
		; (if arrow1-pos
		  ; (setf arrow2-pos (list line (position-if #'(lambda (x) (if x (search "A" (symbol-name x)))) (nth line fold-pattern))))
		  ; (setf arrow1-pos (list line (position-if #'(lambda (x) (if x (search "A" (symbol-name x)))) (nth line fold-pattern)))))))
		(dotimes (field (length (nth line fold-pattern)))
		  (if (if (nth field (nth line fold-pattern)) (search "A" (symbol-name (nth field (nth line fold-pattern)))))
			(if arrow1-pos
			  (setf arrow2-pos (list line field))
			  (setf arrow1-pos (list line field))))))
	(if bugfix (format t "Base position: ~a~%" base-pos))
    (dotimes (line (length fold-pattern))
      (setf former-line temp-line)
      (setf temp-line nil)
      (dotimes (field (length (nth line fold-pattern)))
		(if bugfix (format t "Current line: ~a, current field: ~a~%" line field))
        (setf former-field temp-field)
        (setf temp-field nil)
        (setf temp-fold nil)
        (if (not (eq (nth field (nth line fold-pattern)) nil))
          (progn
			; set "color" to blue if field is base
		    (let ((facecolor (if (equal (list line field) base-pos) 'blue 'black)) (arrow nil))
			  ; set "texture" to arrow variable - which is a directional vector from the field center
			  (if (or (equal (list line field) arrow1-pos) (equal (list line field) arrow2-pos))
				  (setf arrow (nth (position (find-symbol (string-trim "BX" (symbol-name (nth field (nth line fold-pattern))))) *arrow-list*) *arrow-vectors*)))
              ; create object / coordinates
              (setf temp-field (list
                (list (* field scale) (* line scale) 0) ; upper left
                (list (+ (* field scale) scale) (* line scale) 0) ; upper right
				(list (+ (* field scale) scale) (+ (* line scale) scale) 0) ; lower right
                (list (* field scale) (+ (* line scale) scale) 0))) ; lower left

				; Offset to center pattern
				(setf temp-field (mapcar #'(lambda (face) (funcall #'translate-by-xyz face (list (- (* scale 3)) (- (* scale 2)) 0))) temp-field))
				; Offset Translation
				(setf temp-field (mapcar #'(lambda (face) (funcall #'translate-by-xyz face offset-translation)) temp-field))
				; Create arrows
				(setf temp-center (get-center temp-field))
				(if arrow (progn
					(setf arrow (list temp-center (+list temp-center (mapcar #'(lambda (x) (* x 0.5)) arrow))))
					(setf temp-field (append temp-field arrow))))
				; Calculate Screen Coordinates
				(setf temp-center (convert-centered-coordinates-to-screen (calculate-projection temp-center)))
				(setf visual-chunks (append visual-chunks `(
					(isa (spatial-feature spatial-visual) screen-x ,(first temp-center) screen-y ,(second temp-center) kind (pattern nil) class (nil pattern) type face color ,facecolor
						texture ,(if arrow 'arrow) points ,(list nil temp-field) width ,(list nil scale) height ,(list nil scale)))))
						
				(if bugfix (format t "Field created: ~a, with color: ~a and arrow: ~a~%" temp-field facecolor arrow)))
            ;;;;; create horizontal edge
            (if (and (nth field former-line) (not (eq line 0)))
              (progn
                (if (< (- line 1) (first base-pos))
				  ; Pick direction for horizontal axis - if above base field, go from left to right, otherwise from right to left
				  ; ...or that's what i thought! Turns out that due to the whole vertical flipping shenanigans, horizontal axes need to be reversed.....
				  ; ...or do they? idk anymore
				  
				  ; Alternative: if above base field, go from right to left, otherwise from left to right
                  (setf temp-fold (list (list (+ (* field scale) scale) (* line scale) 0) (list (* field scale) (* line scale) 0)) arrow-dir 'left)
				  (setf temp-fold (list (list (* field scale) (* line scale) 0) (list (+ (* field scale) scale) (* line scale) 0)) arrow-dir 'right))
				  
				  ; ;
				  ; ;
				  ; (setf temp-fold (list (list (* field scale) (* line scale) 0) (list (+ (* field scale) scale) (* line scale) 0)) arrow-dir 'right)
				  ; (setf temp-fold (list (list (+ (* field scale) scale) (* line scale) 0) (list (* field scale) (* line scale) 0)) arrow-dir 'left))
				  ; ;
				  ; ;
				
				; Offset to center pattern
				(setf temp-fold (mapcar #'(lambda (face) (funcall #'translate-by-xyz face (list (- (* scale 3)) (- (* scale 2)) 0))) temp-fold))
				; Offset Translation
				(setf temp-fold (mapcar #'(lambda (face) (funcall #'translate-by-xyz face offset-translation)) temp-fold))
				; Calculate Screen Coordinates
				(setf temp-foldcenter (convert-centered-coordinates-to-screen (calculate-projection (get-center temp-fold))))
				
				(setf visual-chunks (append visual-chunks `(
					(isa (spatial-feature spatial-visual) screen-x ,(first temp-foldcenter) screen-y ,(second temp-foldcenter) kind (pattern nil) class (nil pattern) type edge color black
						points ,(list nil temp-fold) width ,(list nil scale) height (nil 1)))))
				(if bugfix (format t "H-Edge created: ~a, direction is ~a~%" temp-fold arrow-dir)))
              )
            ;;;;; create vertical edge
            (if (and former-field (not (eq field 0)))
              (progn
                (if (< (- field 1) (second base-pos))
				  ; Pick direction for vertical axis - if to the left of base field, go from down to up, otherwise from up to down

				  ; Alternative: if to the left of base field, go from up to down, otherwise from down to up
                  (setf temp-fold (list (list (* field scale) (* line scale) 0) (list (* field scale) (+ (* line scale) scale) 0)) arrow-dir 'down)
				  (setf temp-fold (list (list (* field scale) (+ (* line scale) scale) 0) (list (* field scale) (* line scale) 0)) arrow-dir 'up))
				  
				  ; ;
				  ; ;
				  ; (setf temp-fold (list (list (* field scale) (+ (* line scale) scale) 0) (list (* field scale) (* line scale) 0)) arrow-dir 'up)
                  ; (setf temp-fold (list (list (* field scale) (* line scale) 0) (list (* field scale) (+ (* line scale) scale) 0)) arrow-dir 'down))
				  ; ;
				  ; ;
				
				; Offset to center pattern
				(setf temp-fold (mapcar #'(lambda (face) (funcall #'translate-by-xyz face (list (- (* scale 3)) (- (* scale 2)) 0))) temp-fold))
				; Offset Translation				
				(setf temp-fold (mapcar #'(lambda (face) (funcall #'translate-by-xyz face offset-translation)) temp-fold))
				; Calculate Screen Coordinates		  
				(setf temp-foldcenter (convert-centered-coordinates-to-screen (calculate-projection (get-center temp-fold))))

				(setf visual-chunks (append visual-chunks `(
					(isa (spatial-feature spatial-visual) screen-x ,(first temp-foldcenter) screen-y ,(second temp-foldcenter) kind (pattern nil) class (nil pattern) type edge color black
						points ,(list nil temp-fold) width (nil 1) height ,(list nil scale)))))
                (if bugfix (format t "V-Edge created: ~a, direction is ~a~%" temp-fold arrow-dir)))
              )
            ))
        (setf temp-line (append temp-line (list (nth field (nth line fold-pattern)))))))
	(if bugfix (format t "Visual chunks: ~a~%" visual-chunks))
	visual-chunks))
	
; visual chunks of single fields go in, visual chunks of paths go out. Magic!
; example of new path chunk:
	;(SPATIAL-FEATURE17 KIND PATTERN COLOR MIXED SPATIAL T POINTS ((12.5 -7.5 100) (20.0 -7.5 100) (20.0 0.0 100) (12.5 0.0 100) (20.0 0.0 100) (20.0 -7.5 100) (20.0 -7.5 100) (27.5 -7.5 100) (27.5 0.0 100) (20.0 0.0 100) (23.75 -3.75 100) (27.5 -3.75 100.0)) TYPE PATH CLASS PATTERN DELIMITERS (4 2 6) ASPECTS (BASE EDGE ARROW))

(defun extract-paths-from-3d-folding-pattern (pattern)
	(let ((visual-chunks nil) (base-x nil) (base-y nil) (arrow-x nil) (arrow-y nil) (arrow2-x nil) (arrow2-y nil) (paths 0) (scale (* *scale* 0.5)))
		(dolist (object pattern)
			; if object is base, mark as such
			(if (find 'blue object)
				(setf base-x (fourth object) base-y (sixth object)))
				; if object has an arrow, save as such
			(if (find 'arrow object)
				(if arrow-x 
					(setf arrow2-x (fourth object) arrow2-y (sixth object) paths (incf paths))
					(setf arrow-x (fourth object) arrow-y (sixth object) paths (incf paths)))))
		(dotimes (i paths)
			(let ((pathlist nil) (orderedpathlist nil) (screen-x-list nil) (screen-y-list nil) (pointlist nil) (aspectlist nil) (delimiterlist nil))
				(if (not (and (= base-x arrow-x) (= base-y arrow-y)))
					(case (<= base-x arrow-x)
						; if base is LEFT of arrow
						((t)
							(case (<= base-y arrow-y)
							; if base is LEFT and ABOVE of arrow
								((t)
									(dolist (object pattern)
										(if (and (<= (fourth object) arrow-x) (<= (sixth object) arrow-y) (>= (fourth object) base-x) (>= (sixth object) base-y))
											(setf pathlist (append pathlist (list object))))))
								; if base is LEFT and BELOW of arrow
								((nil)
									(dolist (object pattern)
										(if (and (<= (fourth object) arrow-x) (>= (sixth object) arrow-y) (>= (fourth object) base-x) (<= (sixth object) base-y))
											(setf pathlist (append pathlist (list object))))))))
						; if base is RIGHT of arrow 
						((nil)
							(case (< base-y arrow-y)
							; if base is RIGHT and ABOVE of arrow
								((t)
									(dolist (object pattern)
										(if (and (>= (fourth object) arrow-x) (<= (sixth object) arrow-y) (<= (fourth object) base-x) (>= (sixth object) base-y))
											(setf pathlist (append pathlist (list object))))))
								; if base is RIGHT and BELOW of arrow
								((nil)
									(dolist (object pattern)
										(if (and (>= (fourth object) arrow-x) (>= (sixth object) arrow-y) (<= (fourth object) base-x) (<= (sixth object) base-y))
											(setf pathlist (append pathlist (list object)))))))))
					(dolist (object pattern)
						(if (and (= (fourth object) arrow-x) (= (sixth object) arrow-y) (= (fourth object) base-x) (= (sixth object) base-y))
							(setf pathlist (append pathlist (list object))))))

				; sort pathlist by euclidean distance to base
				(setf orderedpathlist (sort (copy-list pathlist) #'< :key (lambda (object) (sqrt (+ (* (- (fourth object) base-x) (- (fourth object) base-x)) (* (- (sixth object) base-y) (- (sixth object) base-y)))))))
				(dolist (object orderedpathlist)
					; append screen-x to screen-x-list
					(setf screen-x-list (append screen-x-list (list (fourth object))))
					; append screen-y to screen-y-list
					(setf screen-y-list (append screen-y-list (list (sixth object))))
					; append points to pointlist
					(setf pointlist (append pointlist (second (nth (+ 1 (position 'points object)) object))))
					; append size of points to delimiterlist
					(setf delimiterlist (append delimiterlist (list (length (second (nth (+ 1 (position 'points object)) object))))))
					; append aspect to aspectlist
					(setf aspectlist (append aspectlist (list
						(if (and (find 'arrow object) (find 'blue object))
							'base&arrow
							(if (find 'arrow object)
								'arrow
								(if (find 'blue object)
									'base
									(nth (+ 1 (position 'type object)) object))))))))
				
				(setf visual-chunks (append visual-chunks `(
					(isa (spatial-feature spatial-visual) screen-x ,(round (/ (apply #'+ screen-x-list) (length screen-x-list))) screen-y ,(round (/ (apply #'+ screen-y-list) (length screen-y-list)))
					kind (pattern nil) class (nil pattern) type path color mixed aspects ,(list nil aspectlist)
					delimiters ,(list nil delimiterlist) points ,(list nil pointlist) width (nil 1) height ,(list nil scale)))))
				; CHANGE arrow-x to value of arrow2-x and clear pathlist, then let the dotimes loop run again
				(setf arrow-x arrow2-x arrow-y arrow2-y)))
		visual-chunks))
		
(defun add-to-stage-markers (stagestring)
	(push (list (mp-time) stagestring) *stage-markers*))

(defun finish-stage-markers (trialtime)
	(let* ((markers (reverse *stage-markers*)) (new-markers (mapcar #'(lambda (lst) (setf (first lst) (- (first lst) trialtime)) lst) markers)))
		new-markers))

;;
; --------------------------------
(defun build-trial (stimuli)
	(remove-visual-finsts)
	(reset-declarative-finsts)
	(setf *left-stimulus-position* ;(list (/ *window-width* 4) (/ *window-height* 2)))
		(list -20 0))
	(setf *right-stimulus-position* ;(list (* 3 (/ *window-width* 4)) (/ *window-height* 2)))
		(list 20 0))
	(setf *prompted* nil)
	
	(setf *processing-cam-pos* '(0 0 0))
	(setf *processing-cam-dir* '(0 0 0))
	(setf *cube-position* nil) ; if cube is left or right
	(setf *pattern-position* nil) ; if folding pattern is left or right

	(setf *left-stimulus-drawn* nil) ; If left window side is already taken
	(setf *mental-display* nil)
	
	;(suppress-act-r-output (delete-all-visicon-features))
	;(delete-visicon-features *cube-stimulus* *pattern-stimulus*)
	
	; Fenster-Maße werden festgelegt
	(if *experiment-window*
		(clear-exp-window *experiment-window*)
		(setf *experiment-window* (open-exp-window "Mental Folding"
							 :visible *visible-experiment-window* ; change this parameter for "hidden" windows for batch learning
							 :width *window-width*
							 :height *window-height*
							 :x 0
							 :y 0)))

	(if *mental-window*
		(clear-exp-window *mental-window*)
		(setf *mental-window* (open-exp-window "Mental Folding - Mental Spatial Representation"
							 :visible *visible-experiment-window* ; change this parameter for "hidden" windows for batch learning
							 :width *window-width*
							 :height *window-height*
							 :x *window-width*
							 :y 0)))
							 
	(delete-all-visicon-features)
							 
	;(if *mental-display* (dolist (item *mental-display*) (suppress-warnings (remove-items-from-exp-window *mental-window* item))))
	
	
	;(if *cube-stimulus* (dolist (item *cube-stimulus*) (suppress-warnings (remove-items-from-exp-window *experiment-window* item))))
	;(if *pattern-stimulus* (dolist (item *pattern-stimulus*) (suppress-warnings (remove-items-from-exp-window *experiment-window* item))))
	
	; Entscheidung über Item-Positionen
	(if (= (act-r-random 2) 0)
		(progn
			(setf *cube-position* *left-stimulus-position*)
			(setf *pattern-position* *right-stimulus-position*))
		(progn
			(setf *cube-position* *right-stimulus-position*)
			(setf *pattern-position* *left-stimulus-position*)))

	; Lege einen Würfel fest
	(setf *cube-stimulus* (create-3d-cube stimuli))
	; (print *cube-stimulus*)
			
	; Lege ein Faltmuster fest
	(setf *pattern-stimulus* (create-3d-folding-pattern stimuli))
	(setf *pattern-paths* (extract-paths-from-3d-folding-pattern *pattern-stimulus*))
	; (print *pattern-stimulus*)
 
	; Lege die korrekte Antwort fest
	(setf *correct-response* (third (second stimuli)))
		 
	; ; Fenster-Maße werden festgelegt
	; (setf *mental-window* (open-exp-window "Mental Folding - Mental Simulation"
						 ; :visible *visible-experiment-window* ; change this parameter for "hidden" windows for batch learning
						 ; :width *window-width*
						 ; :height *window-height*
						 ; :x 700
						 ; :y 300))

	; (install-device '("motor" "keyboard"))
	
	; Features werden ins Visicon gestellt
	; (create-visicon *visual-feature-list*)

	; Füge Pfeile als erlaubte Zeichen in Wörtern hinzu (sonst getrennte vis-loc items)
	(add-word-characters #\< #\> #\^))

; Draw Cube
(defun build-trial-draw-cube ()
	(mapcar #'add-visicon-features *cube-stimulus*)
	(setf *cube-stimulus* (display-points *cube-stimulus*)))

; Draw Folding Pattern
(defun build-trial-draw-pattern ()
	(mapcar #'add-visicon-features *pattern-stimulus*)
	(mapcar #'add-visicon-features *pattern-paths*)
	(setf *pattern-stimulus* (display-points *pattern-stimulus*)))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mf-key-event-handler (model keypress)
	(declare (ignorable model))
	(case (char keypress 0)
		(#\r (setf *response* t))
		(#\w (setf *response* nil))
		(otherwise (print "Keypress registered but unknown")))
	(if (or (equal *response* nil) (equal *response* t)) (schedule-break-relative 0 :details "Trial finished"))
	(if (equal *response* *correct-response*)
		; if answer is correct 
		(progn
			(model-output "correct")
			(if *enable-utility-learning* (trigger-reward *reward*))
			)
		; if answer is wrong
		(progn
			(model-output "wrong")
			(if *enable-utility-learning* (trigger-reward 0))
			))
	(setf *done* t))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; --------------------------------
(defun do-stimuli (stimuli)
	(if (not *puzzle-identifier*)
		(setq *puzzle-identifier* (format nil "debug-~d" (act-r-random 9999999999))))
	(setf *current-stimulus* stimuli)
	(setf *stage-markers* nil)
	
	(build-trial stimuli)
	
	; Fenster wird mit Modell verknüpft
	(if (not (member *experiment-window* (current-devices "vision"))) (install-device *experiment-window*))
	
	(build-trial-draw-cube)
	
	(add-act-r-command "mf-key-event-handler" 'mf-key-event-handler "Key monitor for Mental Folding (Spatial Edition)")
	(monitor-act-r-command "output-key" "mf-key-event-handler")
	
	(schedule-event-relative 1 'build-trial-draw-pattern)
	
	(setf *prompted* t)
	(if *actr-enabled-p*
		(do-experiment-model)
		(do-experiment-person))
	
	(remove-act-r-command-monitor "output-key" "mf-key-event-handler")
	(remove-act-r-command "mf-key-event-handler")
	(schedule-clear-buffer 'retrieval 0)
	(schedule-clear-buffer 'spatial 0)
	(schedule-clear-buffer 'spatial-action 0)
	(eq *response* *correct-response*))
		
(defun load-stimuli (name)
	(let ((same nil) (fold nil) (cube nil))
		(setf same (subseq name 0 (search "/" name)))
		(setf fold (subseq name (1+ (search "/" name)) (search "/" name :from-end t)))
		(setf cube (subseq name (1+ (search "/" name :from-end t)) (length name)))
		; (format t "~a ~a ~a" same fold cube)
		(list (eval (read-from-string (concatenate 'string "exp1-cube-" (subseq cube 0 1) "_" (subseq cube 6 7))))
			(fold-array (eval (read-from-string (concatenate 'string "exp1-fold-" fold)))) (read-from-string same))))
			
(defun load-and-do-stimuli (name)
	(do-stimuli (list name (load-stimuli name))))

(defun testr ()
	(let ((sameness (act-r-random 2)) (stimuli nil) (stimblock (create-stimuli-block)))
		;(setq stimuli (nth (act-r-random (1+ (length (nth sameness stimblock)))) (nth sameness stimblock)))
		(setq stimuli (nth (act-r-random (length (nth sameness stimblock))) (nth sameness stimblock)))
		(setq *puzzle-identifier* (format nil "~a-~d" (first stimuli) (act-r-random 9999999999)))
		(do-stimuli stimuli)))
		
(defun testr-n (runs)
	(let ((results nil))
		(dotimes (i runs) (format t "~a~%" i) (setf results (append results (list (testr)))))
		results))
		
(defun test-n (test runs)
	(let ((results nil))
		(dotimes (i runs) (setf results (append results (list (funcall test)))))
		results))

; Private function! Starts experiment with model
(defun do-experiment-model ()
  (setf *done* nil *response* 0)
  (goal-focus start-goal)
  ;(schedule-module-request 'temporal (define-chunk-spec ticks 0) 0)	; use this if using productions to give up after a certain perceived time has passed
  (mod-focus-fct `(currentpuzzle ,*puzzle-identifier*))
  (run *runtime* *do-in-real-time*)
  ;(run-until-condition (lambda (x) *done*) *do-in-real-time*) ; key handler already provides a "break" event, which is cleaner. Otherwise, reward isn't propagated correctly
)

; Private function! Starts experiment with human
(defun do-experiment-person ()
	(setf *done* nil *response* 0)
	;(while (not *done*)
	;	(allow-event-manager *experiment-window*))
	(sleep 1)
)

; Simulates fixation cross, for between-trial periods
(defun do-fixation-cross ()
	(run-full-time 1.0 *do-in-real-time*))

(defun tsame ()
	(setq *puzzle-identifier* (format nil "-~d" (act-r-random 9999999999)))
	(do-stimuli (list "same-test" ex-same))
)
		
(defun tdiff ()
	(setq *puzzle-identifier* (format nil "-~d" (act-r-random 9999999999)))
	(do-stimuli (list "diff-test" ex-diff))
)
		
(defun tsame-b ()
	(setq *puzzle-identifier* (format nil "-~d" (act-r-random 9999999999)))
	(do-stimuli (list "same-test-b" ex-same-base))
)
		
(defun tdiff-b ()
	(setq *puzzle-identifier* (format nil "-~d" (act-r-random 9999999999)))
	(do-stimuli (list "diff-test-b" ex-diff-base))
)

(defun testk ()
	(setq *puzzle-identifier* (format nil "-~d" (act-r-random 9999999999)))
	(do-stimuli (list "ex-level-k" ex-k))
)

(defun testl ()
	(setq *puzzle-identifier* (format nil "-~d" (act-r-random 9999999999)))
	(do-stimuli (list "ex-level-l" ex-l))
)

(defun testx ()
	(setq *puzzle-identifier* (format nil "-~d" (act-r-random 9999999999)))
	(do-stimuli (list "ex-level-x" ex-x))
)

(defun tblock (&optional (output nil))
	(mf-block (create-stimuli-block) 1 output))



;(defvar *complete-stimuli-block* (create-stimuli-block))
(defun mf-block (stimuli number &optional (output t))
	(let ((tempstimuli nil) (blockdata nil) (counter 0))
		(setf tempstimuli (skim-mismatches (permute-list (second stimuli)) number))
		(setf tempstimuli (permute-list (append (first stimuli) tempstimuli)))
		(dolist (i tempstimuli)
			(let ((puzzledata nil) (puzzletime (mp-time)))
				(setf *puzzle-identifier* (concatenate 'string (first i) (format nil "-~d" (act-r-random 9999999))))
				(if output
					(format t "~&[(~3,5$) B ~a / T ~3a] ~27a" (mp-time) number (+ counter 1) *puzzle-identifier*))
				(incf counter)
				(do-stimuli i)
				(if output
					(format t " - Time needed: ~9as - Correct? ~a" (- (mp-time) puzzletime) (if (equal *response* *correct-response*) "yes" "no")))
				(setf puzzledata (list *puzzle-identifier* number counter (- (mp-time) puzzletime) *response* (equal *response* *correct-response*)))
				(setf blockdata (append blockdata (list puzzledata)))))
		blockdata))

(defun mf-experiment (&optional (output t))
	;(let ((block (create-stimuli-block)) (exp-data nil))
	(let ((block (create-stimuli-block)) (exp-data (list (list 'ID 'Block_number 'Trial_number 'RT 'Response 'Correct))))
		(dotimes (i 5)
			(let ((blockdata nil))
				(setf blockdata (mf-block block (1+ i) output))
				(setf exp-data (append exp-data (list blockdata)))))
		exp-data))

;;; PARAMETER FITTING
(defun combinations (lists)
	(if (listp (car lists))
		(if (car lists)
			(mapcan (lambda (inner-val)
				(mapcar (lambda (outer-val) (cons outer-val inner-val)) (car lists)))
				(apply #'combinations (list (cdr lists))))
			(list nil))
		lists))
	
(defun split-part (list parts part)
  (if (> parts part)
    (let ((partlength (ceiling (/ (length list) parts)))
          (listpart '()))
      (if (> (length list) (* (+ 1 part) partlength))
        (setq listpart (subseq list (* part partlength) (* (+ 1 part) partlength)))
        (setq listpart (subseq list (* part partlength))))
	(return-from split-part listpart))))

;;;;;;;;;; Parameter Fitting
; Parameters to be fitted: latency factor, retrieval threshold, activation noise, egs (utility noise), s-delay, s-complexity-factor
(setf *fitting-test* '((0.2 0.5) (-2.0 -1.0 0) (nil) (2) (0.2) (0.005 0.01 0.05) (0.02 0.2)))
(setf *fitting-test-2* '((0.02 0.05 0.1) (-2.0 -1.0) (nil) (1) (0.2)(0.0005 0.001) (0.05 0.1 0.5)))
(setf *fitting-values* '((0.2 0.5 1.0) (-1.0 -1.5 -2.0) (nil 0.5) (1 2) (0.2) (0.01 0.025 0.05) (0 0.02 0.1)))

(setf *single-fit* '((0.2) (-1.8) (nil) (0.5 0.5 1 1) (0.2) (0.001 0.002 0.003) (0.5)))
; best values so far:
; '((0.2) (-1.8) (0.5) (1) (0.2) (0.001) (0.5))

(setf *fine-fitting-values* '((0.02 0.05 0.1 0.2) (-1.5 -2) (nil) (1) (0.0005 0.001 0.005 0.01) (0.05 0.1 0.5 1)))
(setf *fitting-values-combinations* (combinations *fitting-values*))

; example of parameter combination splitting:
;(setf *fitting1* (split-part *fitting-values-combinations* 5 0))
;(setf *fitting2* (split-part *fitting-values-combinations* 5 1))
;(setf *fitting3* (split-part *fitting-values-combinations* 5 2))
;(setf *fitting4* (split-part *fitting-values-combinations* 5 3))
;(setf *fitting5* (split-part *fitting-values-combinations* 5 4))

; Quick commands:
; (setf *fitting-data* (mf-fitting *fitting-test*))
; (setf *fitting-data* (mf-fitting *fitting-test-2*))
; (setf *fitting-data* (mf-fitting *fitting-values*))
; (setf *fitting-data* (mf-fitting *single-fit*))
; (setf *fitting-data* (mf-fitting *fine-fitting-values*))


(defun mf-fitting (fitting-values &optional (already-combi? nil) (start-at 0) (humandata *human-data*))
	(let ((parameters (subseq (if already-combi? fitting-values (combinations fitting-values)) start-at))
			(all-aggregates nil))
		(dotimes (n (length parameters))
			(let* ((params (nth n parameters)) (exp-data nil) (blockdata nil) (aggregated-exp-data nil) (block (create-stimuli-block))
				; define wanted parameters here and pick them out of params list, e.g. (rt (third params))
				(lf (first params))
				(rt (second params))
				(ans (third params))
				(egs (fourth params))
				(alpha (fifth params))
				(s-delay (sixth params))
				(s-complexity-factor (seventh params))
				)
				(format t "~&[Run ~a of ~a] Parameters used: lf - ~a; rt - ~a; ans - ~a; egs - ~a; u-alpha - ~a; s-delay - ~a; s-compl.-factor - ~a~%" (+ 1 n) (length parameters) lf rt ans egs alpha s-delay s-complexity-factor) ; add parameters here for output
				(reset)
				(sgp-fct `(:lf ,lf :rt ,rt :ans ,ans :egs ,egs :alpha ,alpha :s-delay ,s-delay :s-complexity-factor ,s-complexity-factor))
				;;;;;;;;;;;			
				(dotimes (i 5)
					(setf blockdata (mf-block block (+ 1 i) nil))
					(dolist (trial blockdata)
						; debug info: (format t "~&Debugging: trial list: ~a~%" trial)
						(let* ((levelpos (+ 1 (position '/ (first trial) :test #'string-equal))) (level (subseq (first trial) levelpos (+ levelpos 1)))
								(key (parse-integer (concatenate 'string (write-to-string (+ i 1)) level))) (entry (assoc key exp-data)))
								; debug info: (format t "~&trial: ~a level: ~a key: ~a entry: ~a~%" (first trial) level key entry)
							(if entry
								;(rplacd entry (push (second trial) (cdr entry)))
								;(rplacd entry (nconc (list (cdr entry)) (list (second trial))))
								(rplacd entry (append (cdr entry) (list (fourth trial))))
								;(setf exp-data (acons key (second trial) exp-data))))))
								(setf exp-data (acons key (list (fourth trial)) exp-data))))))
								
				(setf exp-data (stable-sort exp-data #'< :key #'car))
				(dolist (aggregate exp-data)
					(setf aggregated-exp-data (append aggregated-exp-data (list (/ (apply #'+ (cdr aggregate)) (length (cdr aggregate)))))))
				(setf all-aggregates (append all-aggregates (list aggregated-exp-data)))
				
				(correlation aggregated-exp-data humandata)
				(mean-deviation aggregated-exp-data humandata)
				(format t "~&~a~%" aggregated-exp-data)))
		(list parameters all-aggregates)))
;;;;;;;;;;
				
				
;;;;; SIMULATION FUNCTIONS - here be dragons
; Gets Module Activity and resamples it for comparison with EEG data
(defun resample-trial-activity (t-start t-end human-trialtime samplerate)
	(let* ((model-trialtime (- t-end t-start)) (model-human-ratio (/ model-trialtime human-trialtime)) (stretched-samplerate (/ samplerate model-human-ratio)) (stretched-stepsize (/ 1 stretched-samplerate)))
		;(module-demand-functions (get-history-data "buffer-trace") :start t-start :end t-end :step stretched-stepsize)))
		(module-demand-functions :start t-start :end t-end :step stretched-stepsize)))
	

;;; Simulates a number of participants in the mental folding task with their actual order of trials, for comparison with EEG data
;;; Outputs a list of results for each trial: Subject, Block, Trial, Identifier, Model RT, Human RT, Model Correctness, Human Correctness
;;; Quicker reading of csv files if enabled, but incompatible with older ACT-R due to unfulfillable package dependencies:
; (ql:quickload :cl-csv) ; using a csv reader package. needs quicklisp, which YOU should have already. Comment out if using an old ACT-R version
;;; Test z.B. mit:
;;; (mf-simulation *source-data* 4)
;;; Aufruf z.B. mit:
;;; (mf-simulation *source-data*)

;;; Individually simulates each listed subject
;;; Test z.B. mit:
; (sim-individually '(4) t t t 100)
;;; Aufruf z.B. mit:
; (sim-individually '(12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30) t t t 250)

; full list of MF subjects: 4 7 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49
; done: 4 7 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 31 32 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49
; pending: 28 29 30 ... 
(defun sim-individually (&optional (sublist t) (block t) (trial t) (output t) (samplerate 20))
	(if (eq sublist t) (setf sublist '(4 7 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49)))
	(dolist (sub sublist)
		(let* ((subname (write-to-string sub)) (subdata (mf-simulation *source-data* sub block trial output samplerate)))
			(write-to-csv2 subdata (concatenate 'string *sim-directory* "simdata_" subname ".csv"))
			;(cl-csv:write-csv subdata :stream (pathname (concatenate 'string *sim-directory* "simdata_" subname ".csv"))) # results in transposed list
			(write-to-csv *stage-markers-output* (concatenate 'string *sim-directory* "simdata_stagemarkers_" subname ".csv")))))

; Reworked from MR model file. A few notes:
; - side is chosen randomly
; - sample rate is 250Hz
(defun mf-simulation (filepath &optional (subject t) (block t) (trial t) (output t) (samplerate 250))
	(record-history "buffer-trace" "goal" "production" "imaginal" "imaginal-action" "retrieval" "manual" "vocal" "aural" "aural-location" "visual" "visual-location" "temporal" "spatial" "spatial-action")
	; Possibilities for CSV loading:
	; (1) (table (cl-csv:read-csv (pathname filepath))) ; for ACT-R versions with up-to-date quicklisp (looking at you, 7.5).
	; (2) (table (read-from-csv filepath)) ; for everything else. Using my custom csv-reader, which is slow, but reliable, like an old horse.
	
	(setf *stage-markers-output* '((Subject Block Trial Trialtime Out Markertime Marker)))
	;(let* ((table (cl-csv:read-csv (pathname filepath))) (header (first table)) (currentsubject 0)
	(let* ((table (read-from-csv filepath)) (header (first table)) (currentsubject 0)
			(col-subj (position "subject" header :test #'string-equal))
			(col-block (position "block" header :test #'string-equal))
			(col-trial (position "trial" header :test #'string-equal))
			(col-ref (position "reference" header :test #'string-equal))
			(col-tar (position "target" header :test #'string-equal))
			(col-level (position "level" header :test #'string-equal))
			(col-match (position "match" header :test #'string-equal))
			(col-correct (position "correct" header :test #'string-equal))
			(col-out (position "out" header :test #'string-equal))
			(col-rt (position "RT" header :test #'string-equal)) ;correct
			;(simdata '((Timestep) (Subject) (Block) (Trial) (ID) (Degrees) (Match) (Model_RT) (Human_RT) (Model_Correctness) (Human_Correctness) (Equal_Correctness) (Out) (Goal) (Production) (Imaginal) (Imaginal-Action) (Retrieval) (Manual) (Vocal) (Aural) (Aural-Location) (Visual) (Visual-Location) (Temporal) (Spatial) (Spatial-Action))))
			(simdata '((Timestep) (Subject) (Block) (Trial) (ID) (Degrees) (Match) (Model_RT) (Human_RT) (Model_Correctness) (Human_Correctness) (Equal_Correctness) (Out) (Imaginal-Action) (Imaginal) (Visual) (Production) (Retrieval) (Visual-Location) (Vocal) (Manual) (Aural-Location) (Aural) (Temporal) (Spatial-Action) (Spatial) (Goal))))
		;(format t "DEBUG! ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a" col-subj col-block col-trial col-ref col-tar col-level col-match col-correct col-out col-rt)
			;;; output of header: (IMAGINAL-ACTION IMAGINAL VISUAL PRODUCTION RETRIEVAL VISUAL-LOCATION VOCAL MANUAL AURAL-LOCATION AURAL TEMPORAL SPATIAL-ACTION SPATIAL GOAL)
			;;; as long as no modules are added or taken from this function, the above list is the correct module order.
			;;; Sadly, there is not really a way to automate it, since it requires module activity to be generated and that happens after the first line to simdata is written, for loop reasons. :(
		(dolist (trialdata (cdr table)) ; each line of the CSV file except the first (header) is now looped over
			; if subject is either t (meaning all subjects) or this specific subject, simulate from CSV, otherwise skip
			(if (and (or (eq subject t) (eq (read-from-string (nth col-subj trialdata)) subject)) (or (eq block t) (eq (read-from-string (nth col-block trialdata)) block))
					(or (eq trial t) (eq (read-from-string (nth col-trial trialdata)) trial)))
				(progn
					(if (not (eq currentsubject (read-from-string (nth col-subj trialdata)))) (progn (setf currentsubject (read-from-string (nth col-subj trialdata))) (reset)))
					(let* ((puzzledata nil) (puzzletime (mp-time)) (stimuli (concatenate 'string (nth col-match trialdata) "/" (nth col-tar trialdata) "/" (nth col-ref trialdata))))
						(setf *puzzle-identifier* (concatenate 'string stimuli (format nil "-~d" (get-universal-time))))
						(if output
							(format t "~&[S ~a // B ~a // T ~3a]: ~19a, ~1a, ~3a" (nth col-subj trialdata) (nth col-block trialdata) (nth col-trial trialdata) stimuli (nth col-level trialdata) (nth col-match trialdata)))
						;(do-stimuli stimuli)
						(load-and-do-stimuli stimuli)
						(let* ((trialtime (- (mp-time) puzzletime)) (humantime (float (read-from-string (nth col-rt trialdata))))
								(correctness (equal *response* *correct-response*)) (step (/ 1 samplerate))
								(humancorrectness (read-from-string (nth col-correct trialdata))) (outness (read-from-string (nth col-out trialdata)))
								(raw-activity (module-demand-functions :start puzzletime :end (mp-time) :step step))
								(activity (mapcar #'cdr raw-activity)) (activity-header (mapcar #'car raw-activity)) (act-length (length (car activity)))
								;;;
								;;; above: maybe source of naming confusion of activity output - might implicitly reverse module order, try reverse?
								;;;
								(newcol-step (loop :for n :below act-length :collect (float (* (+ n 1) step)))) ;;;;;;;;;;;;;;;;;;;
								(newcol-subj (make-list act-length :initial-element (read-from-string (nth col-subj trialdata))))
								(newcol-block (make-list act-length :initial-element (read-from-string (nth col-block trialdata)))) 
								(newcol-trial (make-list act-length :initial-element (read-from-string (nth col-trial trialdata))))
								(newcol-stimuli (make-list act-length :initial-element stimuli))
								(newcol-level (make-list act-length :initial-element (read-from-string (nth col-level trialdata))))
								(newcol-match (make-list act-length :initial-element (read-from-string (nth col-match trialdata))))
								(newcol-trialtime (make-list act-length :initial-element trialtime))
								(newcol-humantime (make-list act-length :initial-element humantime))
								(newcol-correctness (make-list act-length :initial-element correctness))
								(newcol-humancorrectness (make-list act-length :initial-element humancorrectness))
								(newcol-equalresult (make-list act-length :initial-element (eq correctness humancorrectness)))
								(newcol-outness (make-list act-length :initial-element outness))
								;(extraheader '(Subject Block Trial ID Model_RT Human_RT Model_Correctness Human_Correctness Equal_Correctness Out))
								;(newheader (append act-header extraheader))
								(newcolumns (list newcol-step newcol-subj newcol-block newcol-trial newcol-stimuli newcol-level newcol-match newcol-trialtime newcol-humantime
										newcol-correctness newcol-humancorrectness newcol-equalresult newcol-outness))
								(trialmarkers (finish-stage-markers puzzletime))
								(extended-trialmarkers (mapcar #'(lambda (marker) (append (list currentsubject (read-from-string (nth col-block trialdata)) (read-from-string (nth col-trial trialdata)) trialtime outness) marker)) trialmarkers)))
							(declare (ignorable activity-header))
							(setf *stage-markers-output* (append *stage-markers-output* extended-trialmarkers))
							(if output (format t " - RT: ~as - Correct? ~a" trialtime (if correctness "yes" "no")))
							;;;
							;;;
							;;;
							(setf puzzledata (append newcolumns activity)))
							;;;
							;;;
							;;;
							(stop-recording-history) ; not sure if necessary or even  D A N G E R O U S  to add this, but I don't want the "fixation cross" activity to mess things up
							;;;
							(do-fixation-cross)
							;;;
							(record-history "buffer-trace" "goal" "production" "imaginal" "imaginal-action" "retrieval" "manual" "vocal" "aural" "aural-location" "visual" "visual-location" "temporal" "spatial" "spatial-action")
							;;;
							;(setf puzzledata (list (read-from-string (nth col-subj trialdata)) (read-from-string (nth col-block trialdata)) (read-from-string (nth col-trial trialdata)) *puzzle-identifier* trialtime humantime correctness humancorrectness outness)))
						(setf simdata (mapcar #'append simdata puzzledata))))))
		simdata))

;;;;;



; Purifies model of past sins
(clear-all)

; --------------------------------
; define model

(define-model MentalFolding_v3_2022
	(if *use-seed-value* (sgp-fct `(:seed (,*use-seed-value* 5)))) ;randomizer with seed-value for predictable events, default (sgp :seed (12345 5))
	(sgp :show-focus t :trace-detail medium ;window parameters
		:record-ticks nil ;if temporal ticks should appear in the the buffer trace
		:ncnar nil ;"normalizing chunk names after run", default t. May save time/memory if set to nil or delete (extreme case, may be unstable) for multiple runs
		:er t ;further randomization to break "ties" - most effective if :esc is nil
		:use-tree t ; generates a decision tree based on production requirements before running. May be quicker, but needs more memory
		;:bll 0.5
	)
	(sgp-fct `(:v ,*enable-trace* :s-delay ,*spatial-delay* :s-complexity ,*spatial-complexity*
				:declarative-finst-span ,*declarative-finst-span* :visual-num-finsts ,*visual-num-finsts*
				:visual-finst-span ,*visual-finst-span* :visual-attention-latency ,*visual-latency*
				:s-complexity-factor ,*spatial-complexity-factor*
				:esc ,*enable-sc* :lf ,*latency* :rt ,*retrieval-threshold*
				:ul ,*enable-utility-learning* :iu ,(if *enable-utility-learning* *initial-utility* 0) :egs ,*utility-noise* :alpha ,*utility-rate*
				:epl ,*enable-production-compilation* :bll ,*enable-bl-learning*
				:ans ,*activation-noise*
				:mp ,*enable-partial-matching*
				:pct ,*pc-trace*))

	(chunk-type cube-folding-task currentpuzzle state patternbase-x patternbase-y cubebaseknown
		arrowsmeet? arrowonbase?
		cubearrow1 cubearrow2 patternarrow1 patternarrow2
		firstpathfound secondpathfound
		firstcomparisonvalid?
		angle-threshold)
		
	;(chunk-type spatial-object class points transformations attached-to (origin T))
	(chunk-type (spatial-structure (:include spatial-object)) aspects delimiters trial-id)
	(chunk-type (spatial-association (:include spatial-object)) aspects delimiters vispoints visaspects visdelimiters trial-id)
	
	(chunk-type (cube-object (:include spatial-structure)) type texture color)
	(chunk-type (pattern-object (:include spatial-structure)) type texture color current-axis foldedpoints foldedaspects foldeddelimiters queued)
	
	(chunk-type visual-focus focus-x focus-y next-object trial-id)
	(chunk-type overlap-check overlap trial-id)
	
	(chunk-type (pattern-instance (:include pattern-object)) overlap) ; overlap slot added because it starts out as an overlap-check object, then gets changed; gives warnings otherwise

	; visual objects
	;(chunk-type visual-object screen-pos value status color height width)
	(chunk-type (spatial-visual (:include visual-object)) (spatial t) class points type texture aspects delimiters)

	; visual locations
	;(chunk-type visual-location screen-x screen-y distance kind color value height width size)
	(chunk-type (spatial-feature (:include visual-location)) (kind spatial) type texture)

	;(chunk-type mental-folding original-x original-y currentimage currentstep currentfolding currentface currenttransformation uniqueid)

	(add-dm
		(start) (find-base) (encode-base) (find-arrow) (attend-arrow) (started-spatial-structure) (find-object-for-structure) (find-arrow-for-structure) (find-base-for-structure) ; ....................... goal slot chunks
		(find-pattern-path) (build-path-start) (build-path-encode-arrow) (find-object-for-path-structure) (encode-into-structure) (build-path-find-next)
		(harvest-path) (path-harvested) (retrieve-path) (encode-path)
		(find-instance) (retrieve-instance) 
		(start-folding) (find-path-overlap) (folding) (prune-folded-structure) (folded) (pruned-folded-structure) (retrieve-other-path-for-folding) (remember-path-structure) (remember-folded-structure)
		(attend-path-for-association) (associate-structure-with-path)
		(compare-stimuli) (combined) (remember-cube) (reencode-cube) (reencoded-compare) (first-arrow-compared) (second-arrow-compared) (answered) (mixed) (path)
		
		(rotate-around-xyz) (rotate-around-axis) (compare-vector-angle)
		
		(cube) (pattern) (reference) (target) (edge) (face) (base) (arrow) (base&arrow) (no) (associated) (completepattern) (timechunk ISA time); ....................... possible chunk slot values
	)
	
	(set-all-base-levels 1000 -1000)
	(set-visual-center-point (/ *window-width* 2) (/ *window-height* 2))
	(add-dm-fct (list `(start-goal ISA cube-folding-task state start angle-threshold ,*angle-threshold* currentpuzzle ,*puzzle-identifier*)))
	
	(sgp :cycle-hook update-sim-hook)


;; Productions

;; STEP 1: OVERVIEW - Perceive and Encode  arrows of both folding pattern and cube, and paths from arrows to base square on folding pattern 
	
	(P find-base
		=goal>
			state	start
	;		patternbase-x	nil
	==>
		!safe-bind! =currentpuzzle	*puzzle-identifier* ; only included so ACT-R doesn't throw unnecessary warnings - actual setting happens in trial function
		!safe-bind! =threshold *angle-threshold*
		=goal>
			state	find-base
			currentpuzzle	=currentpuzzle
			angle-threshold	=threshold
		+visual-location>
			:attended	nil
			color	blue
			type	face
			kind	cube
			; kind	pattern	; cube? pattern?
	)
	
		(P attend-base-for-cube
			=goal>
				state	find-base
			=visual-location>
				screen-x	=x
				screen-y	=y
				kind	cube
			?visual>
				state	free
		==>
			!safe-eval!		(add-to-stage-markers "encoding_cube")
			=goal>
				state	encode-base
			=visual-location>
			+visual>
				cmd		move-attention
				screen-pos	=visual-location
		)
		
		(P encode-base-for-cube
			=goal>
				state	encode-base
				currentpuzzle	=id
			=visual>
				class	cube
				points	=points
				texture		nil
			?spatial>
				state	free
			?spatial-action>
				state	free
		==>
			!safe-bind!	=new-aspects	(list 'base)
			!safe-bind!	=new-delims	(list (length =points))
			=goal>
				state	started-spatial-structure
				cubebaseknown	t	
				arrowonbase?	nil
			=visual>
			+spatial>
				isa spatial-structure
				class	cube
				points	=points
				aspects		=new-aspects
				trial-id	=id
				delimiters	=new-delims
				origin	reference
		)
		
		(P encode-base-with-arrow-for-cube
			=goal>
				state	encode-base
				currentpuzzle	=id
			=visual>
				class	cube
				points	=points
				texture		arrow
			?spatial>
				state	free
			?spatial-action>
				state	free
		==>
			!safe-bind!	=new-aspects	(list 'base&arrow)
			!safe-bind!	=new-delims	(list (length =points))
			=goal>
				state	started-spatial-structure
				cubebaseknown	t
				arrowonbase?	t
				cubearrow1		t
			=visual>
			+spatial>
				isa spatial-structure
				class	cube
				points	=points
				aspects		=new-aspects
				trial-id	=id
				delimiters	=new-delims
				origin	reference
		)
		
	(P find-pattern-path
		=goal>
			state	find-pattern-path
			- firstpathfound	t
	==>
		=goal>
			firstpathfound		t
		+visual-location>
			:attended	nil
			kind	pattern
			color	mixed
			type	path
	)
	
		(P waiting-for-pattern
			=goal>
				state	find-pattern-path
			?visual-location>
				state	error
		==>
			+visual-location>
				:attended	nil
				kind	pattern
				color	mixed
				type	path
		)
		
		(P attend-pattern-path
			=goal>
				state	find-pattern-path
			=visual-location>
				screen-x	=x
				screen-y	=y
				kind	pattern
				type	path
			?visual>
				state	free
		==>
			!safe-eval!		(add-to-stage-markers "encoding_path")
			=goal>
				state	encode-path
			+visual>
				cmd		move-attention
				screen-pos	=visual-location
		)
		
		; example of new path chunk:
		;(SPATIAL-FEATURE17 KIND PATTERN COLOR MIXED SPATIAL T POINTS ((12.5 -7.5 100) (20.0 -7.5 100) (20.0 0.0 100) (12.5 0.0 100) (20.0 0.0 100) (20.0 -7.5 100) (20.0 -7.5 100) (27.5 -7.5 100) (27.5 0.0 100) (20.0 0.0 100) (23.75 -3.75 100) (27.5 -3.75 100.0)) TYPE PATH CLASS PATTERN DELIMITERS (4 2 6) ASPECTS (BASE EDGE ARROW))

		; example of spatial structure:
		;(chunk-type spatial-object class points transformations attached-to (origin T))
		;(chunk-type (spatial-structure (:include spatial-object)) aspects delimiters trial-id)
		
		; newly added: production now starts a retrieval process to try and bypass the folding process with a known instance
		(P encode-pattern-path
			=goal>
				state	encode-path
				currentpuzzle	=id
			=visual>
				class	pattern
				color	mixed
				points	=points
				aspects		=aspects
				delimiters	=delims
			?retrieval>
				state	free
			?spatial>
				state	free
			?spatial-action>
				state	free
			!safe-eval!	(> (length =aspects) 1)
		==>
			;
			!safe-eval!	(setf *processing-cam-pos* *trial-cam-pos* *processing-cam-dir* *trial-cam-dir*) ; turn camera to make it look cool, bro ; *processing-cam-pos* '(25 15 30) *processing-cam-dir* '(15 -10 0)
			;
			=goal>
				state	start-folding
			-visual>
			+retrieval>
				vispoints	=points
				visaspects	=aspects
				visdelimiters	=delims
				origin	associated
			+spatial>
				isa spatial-structure
				class	pattern
				points	=points
				aspects		=aspects
				delimiters	=delims
				trial-id	=id
				origin	target
		)
		
		(P encode-pattern-path-is-base
			=goal>
				state	encode-path
				currentpuzzle	=id
			=visual>
				class	pattern
				color	mixed
				points	=points
				aspects		=aspects
				delimiters	=delims
			?spatial>
				state	free
			?spatial-action>
				state	free
			!safe-eval!	(and (= (length =aspects) 1) (find 'base&arrow =aspects))
		==>
			=goal>
				state	pruned-folded-structure
			-visual>
			+spatial>
				isa spatial-structure
				class	pattern
				points	=points
				aspects		=aspects
				delimiters	=delims
				trial-id	=id
				origin	folded
		)
		
	;;; Shortcuts: visual check if arrow is on base for both or neither
	(if *enable-shortcuts* (progn
		(P basearrow-on-cube-not-on-pattern-mismatch-shortcut
			=goal>
				state		encode-path
				currentpuzzle	=id
				arrowonbase?	t
			=visual>
				class	pattern
				color	mixed
				aspects		=aspects
			?manual>
				state	free
			!safe-eval!		(not (find 'base&arrow =aspects))
		==>
			!safe-eval!		(add-to-stage-markers "shortcut_cubebasearrowmismatch")
			+goal>
				state	answered
			-visual>
			+manual>
				cmd		press-key
				key		"w"
		)
		
		(P basearrow-on-pattern-not-on-cube-mismatch-shortcut
			=goal>
				state	encode-path
				currentpuzzle	=id
				- arrowonbase?	t
			=visual>
				class	pattern
				color	mixed
				aspects		=aspects
			?manual>
				state	free
			!safe-eval!		(find 'base&arrow =aspects)
		==>
			!safe-eval!		(add-to-stage-markers "shortcut_patternbasearrowmismatch")
			+goal>
				state	answered
			-visual>
			+manual>
				cmd		press-key
				key		"w"
		)
	))
	;;; Shortcuts end

;;;;;
;;;;;
;;;;;

;;;;; STEP 1 (cont.) - Cube Spatial Structure

	(P continue-structure-for-cube-find-arrow ; could be generalizable for both pattern and cube, but not with the "cubearrow2"-slot which so far is used to check if structure is complete
		=goal>
			state	started-spatial-structure
			cubearrow2	nil
		=spatial>
			class	cube
		=visual>
			points	=oldpoints
		?visual-location>
			state   free
	==>
		=goal>
			state	find-object-for-structure
		-visual>
		+visual-location>
		;	:attended	nil
			:nearest	current
		;	class	cube
		  - points	=oldpoints
			kind	cube
			texture	arrow
		=spatial>
	)
	
	; reactivate if start does other things than look for bases - so far this isn't used
	; (P continue-structure-for-cube-find-base
		; =goal>
			; state	started-spatial-structure
			; cubebaseknown	nil
		; =spatial>
			; class	cube
		; ?visual-location>
			; state   free
	; ==>
		; =goal>
			; state	find-object-for-structure
		; +visual-location>
			; :attended	nil
			; :nearest	current
			; class	cube
			; color	blue
		; =spatial>
	; )
	
	(P attend-object-for-structure
		=goal>
			state	find-object-for-structure
		=spatial>
			class	=class
		=visual-location>
			kind	=class
		?visual>
			state	free
	==>
		=goal>
			state	encode-into-structure
		-visual-location>
		+visual>
			cmd		move-attention
			screen-pos	=visual-location
		=spatial>
	)
	
		; see above
		; (P encode-base-for-cube-structure
			; =goal>
				; state	encode-into-structure
			; =spatial>
				; points	=oldpoints
				; class	cube
				; aspects	=aspects
				; delimiters	=old-delims
			; =visual>
				; points	=obj-points
				; class	cube
				; color	blue
		; ==>
			; !safe-bind!	=new-aspects	(append =aspects (list 'base))
			; !safe-bind!	=new-group		(group-objects =oldpoints =obj-points)
			; !safe-bind!	=new-points		(first =new-group)		
			; !safe-bind!	=new-delims	(append =old-delims (list (second =new-group)))
			; =goal>
				; state	started-spatial-structure
				; cubebaseknown	t
			; =spatial>
				; points	=new-points
				; delimiters	=new-delims
				; aspects		=new-aspects
			; -visual>
		; )
		
		(P encode-first-arrow-for-cube-structure
			=goal>
				state	encode-into-structure
				cubearrow1	nil
			=spatial>
				points	=oldpoints
				class	cube
				aspects	=aspects
				delimiters	=old-delims
			=visual>
				points	=obj-points
				class	cube
				texture		arrow
		==>
			!safe-bind!	=new-aspects	(append =aspects (list 'arrow))
			!safe-bind!	=new-group		(group-objects =oldpoints =obj-points)
			!safe-bind!	=new-points		(first =new-group)		
			!safe-bind!	=new-delims		(append =old-delims (list (second =new-group)))
			=goal>
				state	started-spatial-structure
				cubearrow1		t
			=spatial>
				points	=new-points
				delimiters	=new-delims
				aspects		=new-aspects
			=visual>
		)
			
		(P encode-second-arrow-for-cube-structure
			=goal>
				state	encode-into-structure
				cubearrow1	t
				cubearrow2	nil
			=spatial>
				points	=oldpoints
				class	cube
				aspects	=aspects
				delimiters	=old-delims
			=visual>
				points	=obj-points
				class	cube
				texture		arrow
		==>
			!safe-bind!	=new-aspects	(append =aspects (list 'arrow))
			!safe-bind!	=new-group		(group-objects =oldpoints =obj-points)
			!safe-bind!	=new-points		(first =new-group)
			!safe-bind!	=new-delims		(append =old-delims (list (second =new-group)))
			=goal>
				state	started-spatial-structure
				cubearrow2		t
			=spatial>
				points	=new-points
				delimiters	=new-delims
				aspects		=new-aspects
			=visual>
		)

	(P harvest-cube-structure-arrows-meet
		=goal>
			state	started-spatial-structure
			cubearrow1	t
			cubearrow2	t
			cubebaseknown	t
		  - secondpathfound	t
		=spatial>
			class	cube
			points	=points
			delimiters	=delims
			aspects		=aspects
		?spatial-action>
			state	free
		!safe-eval!		(arrows-meet (return-from-group (all-positions '(arrow base&arrow) =aspects) =delims =points))
	==>
		; set an outrageous activation level so chunk definitely gets retrieved in the same trial ~ good luck figuring this out in the future, future me
		!safe-eval!		(set-base-levels-fct `(,`(,(car (add-dm-chunks-fct `(,(copy-chunk-fct `,=spatial)))) 1000 -1000)))
		=goal>
			state	find-pattern-path
			arrowsmeet?		t
		-spatial>
	)

	(P harvest-cube-structure
		=goal>
			state	started-spatial-structure
			cubearrow1	t
			cubearrow2	t
			cubebaseknown	t
		  - secondpathfound	t
		=spatial>
			class	cube
			points	=points
			delimiters	=delims
			aspects		=aspects
		?spatial-action>
			state	free
		!safe-eval!		(not (arrows-meet (return-from-group (all-positions '(arrow base&arrow) =aspects) =delims =points)))
	==>
		; set an outrageous activation level so chunk definitely gets retrieved in the same trial ~ good luck figuring this out in the future, future me
		!safe-eval!		(set-base-levels-fct `(,`(,(car (add-dm-chunks-fct `(,(copy-chunk-fct `,=spatial)))) 1000 -1000)))
		=goal>
			state	find-pattern-path
		-spatial>
	)

	(P harvest-cube-structure-for-reencoding
		=goal>
			state	started-spatial-structure
			cubearrow1	t
			cubearrow2	t
			cubebaseknown	t
	   		secondpathfound	t
	   		currentpuzzle	=id
		=spatial>
			class	cube
			points	=points
			delimiters	=delims
			aspects		=aspects
		?spatial-action>
			state	free
	==>
		=goal>
			state	reencode-cube
	   	-spatial>
	   	+retrieval>
	   	;	:recently-retrieved	t
	   		class	pattern
	   		origin	combined
	   		trial-id	=id
	)
	
	(P cube-reencoded-remember-pattern
	   	=goal>
	   		state	reencode-cube
	   	=retrieval>
	   		trial-id	=id
			class	pattern
			points	=points
			delimiters	=delims
			aspects		=aspects
	   		origin	combined
	   	?spatial>
	   		state	free
		?spatial-action>
			state	free
	==>
	   	=goal>
	   		state	reencoded-compare
	   	+spatial>
	   		isa		spatial-structure
	   		trial-id	=id
			class	pattern
			points	=points
			delimiters	=delims
			aspects		=aspects
	   		origin	combined
		-retrieval>
	)
	
		(P cannot-remember-pattern-after-cube-reencoding
			=goal>
				state	reencode-cube
			?retrieval>
				state	error
		==>
			!safe-eval!		(add-to-stage-markers "reencode_path")
			=goal>
				state	find-pattern-path
				firstpathfound	nil
			+visual-location>
				- screen-x	current
				- screen-y	current
				kind	pattern
				color	mixed
				type	path
		)
	
	(P remember-reencoded-cube-structure
		=goal>
			state	reencoded-compare
			currentpuzzle	=id
		=spatial>
		?retrieval>
			state	free
	==>
		=goal>
			state	remember-cube
		=spatial>
		+retrieval>
			class	cube
			origin	reference
			trial-id	=id
	)


	
;;;;; STEP 1 (cont.) - Pattern Path

	;;; Shortcuts - visual check if arrow pattern aligns with easily identifiable match or mismatch patterns
	(if *enable-shortcuts* (progn
		(P arrows-meet-match-shortcut
			=goal>
				state	start-folding
				arrowsmeet?		t
			=spatial>
				class	pattern
				points	=points
				delimiters	=delims
				aspects		=aspects
				foldedpoints	nil
			!safe-eval!		(arrows-meet (return-from-group (all-positions '(arrow base&arrow) =aspects) =delims =points))
			?manual>
				state	free
		==>
			!safe-eval!		(add-to-stage-markers "shortcut_arrowsmeet")
			+goal>
				state	answered
			-spatial>
			+manual>
				cmd		press-key
				key		"r"
		)
		
		(P arrows-cannot-meet-mismatch-shortcut
			=goal>
				state	start-folding
				arrowsmeet?		t
			=spatial>
				class	pattern
				points	=points
				aspects		=aspects
				foldedpoints	nil
			!safe-eval!	(arrows-90degrees =points =aspects)
			!safe-eval!	(arrows-cannot-meet =points =aspects)
			?manual>
				state	free
		==>
			!safe-eval!		(add-to-stage-markers "shortcut_arrowscannotmeet")
			+goal>
				state	answered
			-spatial>
			+manual>
				cmd		press-key
				key		"w"
		)
		
		(if *enable-advanced-shortcuts* (progn
			(P arrows-90-degrees-will-meet-but-do-not-on-cube-mismatch-shortcut
				=goal>
					state	start-folding
					arrowsmeet?		nil
				=spatial>
					class	pattern
					points	=points
					aspects		=aspects
					foldedpoints	nil
				; !safe-eval!		(arrows-90degrees-shortcut =points =aspects)
				!safe-eval!	(arrows-90degrees =points =aspects)
				!safe-eval!	(not (arrows-cannot-meet =points =aspects))
				?manual>
					state	free
			==>
				!safe-eval!		(add-to-stage-markers "shortcut_arrows90degreesmismatch")
				+goal>
					state	answered
				-spatial>
				+manual>
					cmd		press-key
					key		"w"
			)
			
			(P arrows-90-degrees-will-meet-match-shortcut
				=goal>
					state	start-folding
					arrowsmeet?		t
				=spatial>
					class	pattern
					points	=points
					aspects		=aspects
					foldedpoints	nil
				; !safe-eval!		(arrows-90degrees-shortcut =points =aspects)
				!safe-eval!	(arrows-90degrees =points =aspects)
				!safe-eval!	(not (arrows-cannot-meet =points =aspects))
				?manual>
					state	free
			==>
				!safe-eval!		(add-to-stage-markers "shortcut_arrows90degreesmatch")
				+goal>
					state	answered
				-spatial>
				+manual>
					cmd		press-key
					key		"r"
			)
		))
	))
	;;; Shortcuts end



;; STEP 3: MENTAL FOLDING

	(P start-path-traversal
		=goal>
			state	start-folding
		=spatial>
			points	=points
			aspects	=aspects
			delimiters	=delimiters
			foldedpoints	nil
		?spatial-action>			; as a check to make sure the rotation process is finished
			state	free
		!safe-eval!	(> (length =aspects) 1)
	==>
		!safe-eval!		(add-to-stage-markers "folding_start")
		!safe-bind!	=step	(position 'edge =aspects)
		!safe-bind! =new-aspects	(member 'edge =aspects)
		!safe-bind! =foldedaspects	(ldiff =aspects =new-aspects)
		!safe-bind! =new-delims (nthcdr =step =delimiters)
		!safe-bind! =foldeddelims (ldiff =delimiters =new-delims)
		!safe-bind! =new-points (nthcdr (first =delimiters) =points)
		!safe-bind! =foldedpoints (ldiff =points =new-points)
		!safe-bind! =axis (subseq =new-points 0 2)
		=goal>
			state	folding
		=spatial>
			points	=new-points
			aspects		=new-aspects
			delimiters	=new-delims
			foldedpoints	=foldedpoints
			foldedaspects	=foldedaspects
			foldeddelimiters	=foldeddelims
			current-axis	=axis
	)
	
	(P fold-structure
		=goal>
			state	folding
		=spatial>
			current-axis	=axis
			points	=points
			aspects	=aspects
			delimiters	=delimiters
			foldedpoints	=foldedpoints
			foldedaspects	=foldedaspects
			foldeddelimiters	=foldeddelims
		?spatial-action>
			state	free
	==>
		!safe-bind! =removedaspects	(cdr =aspects)
		!safe-bind! =newaspects	(append =foldedaspects (ldiff =aspects =removedaspects))
		!safe-bind! =removeddelims (cdr =delimiters)
		!safe-bind! =newdelims (append =foldeddelims (ldiff =delimiters =removeddelims))	
		!safe-bind! =removedpoints (nthcdr 2 =points)
		!safe-bind! =newpoints (append =foldedpoints (ldiff =points =removedpoints))
		; Weird bugfix hack for z-direction weirdness. Don't do this at home. In fact, don't tell anybody - this will be our little secret ;°
		;
		;
		;
		; !safe-bind! =rotationdirection (if (or (equal =axis '((12.5d0 0.0d0 100.0d0) (12.5d0 0.0d0 92.5d0))) (equal =axis '((-27.5d0 0.0d0 100.0d0) (-27.5d0 0.0d0 92.5d0)))) -1 1)
		; !safe-bind! =rotationdirection (if (and (= (third (first =axis)) 100.0d0) (= (third (second =axis)) 92.5d0) (>= (second (first =axis)) 0.0d0) (>= (second (second =axis)) 0.0d0)) -1 1)
		; !safe-bind! =rotationvalue (* -90 =rotationdirection)
		; !safe-eval!	(print =axis)
		; !output!	=rotationdirection
		; !output!	=rotationvalue
		;
		;
		;
		=goal>
			state	start-folding
		=spatial>
			current-axis	nil
			points	=removedpoints
			aspects	=removedaspects
			delimiters	=removeddelims
			foldedpoints	=newpoints
			foldedaspects	=newaspects
			foldeddelimiters	=newdelims
		+spatial-action>
			cmd   rotate-around-axis
			value  -90
			;value	=rotationvalue
			axis	=axis
	)
	
	(P continue-path-traversal
		=goal>
			state	start-folding
		=spatial>
			points	=points
			aspects	=aspects
			delimiters	=delimiters
			foldedpoints	=foldedpoints
			foldedaspects	=foldedaspects
			foldeddelimiters	=foldeddelims
		?spatial-action>
			state	free
		!safe-eval!	(> (length =aspects) 1)
	==>
		!safe-bind!	=step	(position 'edge =aspects)
		!safe-bind! =removedaspects	(member 'edge =aspects)
		!safe-bind! =newaspects (append =foldedaspects (ldiff =aspects =removedaspects))
		!safe-bind! =removeddelims (nthcdr =step =delimiters)
		!safe-bind! =newdelims (append =foldeddelims (ldiff =delimiters =removeddelims))
		!safe-bind! =removedpoints (nthcdr (first =delimiters) =points)
		!safe-bind! =newpoints (append =foldedpoints (ldiff =points =removedpoints))
		!safe-bind! =axis (subseq =removedpoints 0 2)
		=goal>
			state	folding
		=spatial>
			points	=removedpoints
			aspects		=removedaspects
			delimiters	=removeddelims
			foldedpoints	=newpoints
			foldedaspects	=newaspects
			foldeddelimiters	=newdelims
			current-axis	=axis
	)
	
	(P end-path-traversal
		=goal>
			state	start-folding
		=spatial>
			points	=points
			aspects	=aspects
			delimiters	=delimiters
			foldedpoints	=foldedpoints
			foldedaspects	=foldedaspects
			foldeddelimiters	=foldeddelims
		?spatial-action>
			state	free
		!safe-eval!	(= (length =aspects) 1)
	==>
		!safe-eval!		(add-to-stage-markers "folding_end")
		!safe-bind! =newaspects (append =foldedaspects =aspects)
		!safe-bind! =newdelims (append =foldeddelims =delimiters)
		!safe-bind! =newpoints (append =foldedpoints =points)
		=goal>
			state	prune-folded-structure
		=spatial>
			points	=newpoints
			aspects		=newaspects
			delimiters	=newdelims
			foldedpoints	nil
			foldedaspects	nil
			foldeddelimiters	nil
			origin	folded
	)
	
	(P remember-and-skip-folding
		=goal>
		=spatial>
			- foldedpoints	nil
			- origin	folded
		=retrieval>
			origin	associated
			points	=points
			aspects		=aspects
			delimiters	=delims
		?spatial-action>
			state	free
	==>
		!safe-eval!		(add-to-stage-markers "instance_retrieval")
		=goal>
			state	pruned-folded-structure
		=spatial>
			points	=points
			aspects		=aspects
			delimiters	=delims
			foldedpoints	nil
			foldedaspects	nil
			foldeddelimiters	nil
			origin	folded
		-retrieval>
	)

	; Replaced with new production chain that prunes and then re-encodes the original visual path to create a retrievable association
	; (P prune-folded-structure
		; =goal>
			; state	prune-folded-structure
		; =spatial>
			; points	=points
			; aspects	=aspects
			; delimiters	=delims
	; ==>
		; !safe-bind!	=positions	(all-positions '(base arrow base&arrow) =aspects)
		; !safe-bind! =pruned-points (return-from-group =positions =delims =points)
		; !safe-bind! =pruned-aspects (all-nths =positions =aspects)
		; !safe-bind! =pruned-delims (all-nths =positions =delims)
		; =goal>
			; state	pruned-folded-structure
		; =spatial>
			; points	=pruned-points
			; aspects	=pruned-aspects
			; delimiters	=pruned-delims
	; )
	
	(P prune-folded-structure
		=goal>
			state	prune-folded-structure
		=spatial>
			points	=points
			aspects	=aspects
			delimiters	=delims
	==>
		!safe-bind!	=positions	(all-positions '(base arrow base&arrow) =aspects)
		!safe-bind! =pruned-points (return-from-group =positions =delims =points)
		!safe-bind! =pruned-aspects (all-nths =positions =aspects)
		!safe-bind! =pruned-delims (all-nths =positions =delims)
		=goal>
			state	attend-path-for-association
		=spatial>
			points	=pruned-points
			aspects	=pruned-aspects
			delimiters	=pruned-delims
		+visual-location>
			;:attended	t
			screen-x	current
			screen-y	current
			kind	pattern
			color	mixed
			type	path
	)
	
	(P attend-original-path-for-association
		=goal>
			state	attend-path-for-association
		=spatial>
		=visual-location>
			kind	pattern
			type	path
		?visual>
			state	free
	==>
		=goal>
			state	associate-structure-with-path
		=spatial>
		+visual>
			cmd		move-attention
			screen-pos	=visual-location
	)
	
	(P associate-pruned-structure-with-original-path
		=goal>
			state	associate-structure-with-path
		=spatial>
			points		=newpoints
			aspects		=newaspects
			delimiters	=newdelims
		=visual>
			points		=oldpoints
			aspects		=oldaspects
			delimiters	=olddelims
		?imaginal>
			state	free
	==>
		!safe-eval!		(add-to-stage-markers "instance_learning")
		=goal>
			state	pruned-folded-structure
		=spatial>
		-visual>
		+imaginal>
			isa		spatial-association
			vispoints	=oldpoints
			visaspects	=oldaspects
			visdelimiters	=olddelims
			points	=newpoints
			aspects		=newaspects
			delimiters	=newdelims
			origin	associated
		-imaginal>
	)
		;;;;;
		; commented out because higher difficulty level are way too quickly solved with this activated
		;;;;;
		; (P both-arrows-on-structure
			; =goal>
				; state	pruned-folded-structure
				; currentpuzzle	=id
				; firstpathfound		t
				; secondpathfound	nil
			; =spatial>
				; points	=points
				; aspects	=aspects
			; !safe-eval!	(> (+ (count 'base&arrow =aspects) (count 'arrow =aspects)) 1)
		; ==>
			; =goal>
				; state	compare-stimuli
				; secondpathfound	t
			; =spatial>
				; origin	combined
		; )
		
		(P structure-already-merged
			=goal>
				state	pruned-folded-structure
				currentpuzzle	=id
			;	firstpathfound		nil
				secondpathfound	t
			=spatial>
				points	=points
				aspects	=aspects
			!safe-eval!	(> (+ (count 'base&arrow =aspects) (count 'arrow =aspects)) 1)
		==>
			=goal>
				state	compare-stimuli
			=spatial>
				origin	combined
		)
		;;;;;
		;
		;;;;;
	
	(P attend-other-path-for-folding
		=goal>
			state	pruned-folded-structure
			currentpuzzle	=id
			firstpathfound		t
			secondpathfound	nil
		=spatial>
			points	=points
			aspects	=aspects
		?spatial-action>
			state	free
		;!safe-eval!	(= (+ (count 'base&arrow =aspects) (count 'arrow =aspects)) 1)
	==>
		; set an outrageous activation level so chunk definitely gets retrieved in the same trial ~ good luck figuring this out in the future, future me
		!safe-eval!		(set-base-levels-fct `(,`(,(car (add-dm-chunks-fct `(,(copy-chunk-fct `,=spatial)))) 1000 -1000)))
		=goal>
			state	find-pattern-path
			secondpathfound	t
		-spatial>
		+visual-location>
			:attended	nil
			kind	pattern
			color	mixed
			type	path
	)

;; STEP 3.5: Merging folded structures into one for comparison	
	
	(P retrieve-other-folded-structure
		=goal>
			state	pruned-folded-structure
			secondpathfound	t
			currentpuzzle	=id
		=spatial>
			points	=points
			aspects		=aspects
		?retrieval>
			state	free
		!safe-eval!	(= (+ (count 'base&arrow =aspects) (count 'arrow =aspects)) 1)
	==>
		=goal>
			state	remember-folded-structure
		=spatial>
		+retrieval>
		  - points	nil
		  - points	=points
			class	pattern
			origin	folded
			trial-id	=id
	)
	
		(P cannot-remember-first-path-so-reencode
			=goal>
				state	remember-folded-structure
				secondpathfound	t
			=spatial>
				points	=points
				aspects		=aspects
			?retrieval>
				state	error
		==>
			!safe-eval!		(add-to-stage-markers "reencode_path")
			=goal>
				state	find-pattern-path
				firstpathfound	nil
			-spatial>
			+visual-location>
			;	:attended	nil
				- screen-x	current
				- screen-y	current
				kind	pattern
				color	mixed
				type	path
		)
		
		(P merge-folded-structures
			=goal>
				state	remember-folded-structure
			=spatial>
				points	=points
				aspects	=aspects
				delimiters	=delims
				origin	folded
			=retrieval>
				points	=points2
				delimiters	=delims2
				origin	folded
		==>
		;	!safe-bind!	=other-points (nreverse (subseq (nreverse =points2) 0 6))
			!safe-bind!	=other-points (reverse (subseq (reverse =points2) 0 6))
			!safe-bind!	=new-points	(append =points =other-points)
			!safe-bind! =new-aspects (append =aspects (list 'arrow))
			!safe-bind!	=new-delims	(append =delims (list 6))
			=goal>
				state	compare-stimuli
			=spatial>
				points	=new-points
				aspects	=new-aspects
				delimiters	=new-delims
				origin	combined
			-retrieval>
		)

;;;; TEST
; (setf testpts '((12.5 0.0 100) (20.0 0.0 100) (20.0 7.5 100) (12.5 7.5 100) (20.0D0 -4.592425496802575D-16 92.5D0) (20.0D0 7.499999999999999D0 92.5D0) (20.0D0 7.5D0 100.0D0) (20.0D0 0.0D0 100.0D0) (20.0D0 3.7499999999999996D0 96.25D0) (20.0D0 3.7499999999999996D0 92.5D0)))
; (nreverse (subseq (nreverse testpts) 0 6))
; (reverse (subseq (reverse testpts) 0 6))

		
;; STEP 4: COMPARISON

	; *initial-cube-rotation* is '(-25 20 -10))
	(P rotate-structure-and-remember-cube-structure
		=goal>
			state	compare-stimuli
			currentpuzzle	=id
		=spatial>
			aspects	=aspects
		?spatial-action>
			state	free
		?retrieval>
			state	free
		!safe-eval!	(> (+ (count 'base&arrow =aspects) (count 'arrow =aspects)) 1)
	==>
		;
		!safe-eval!	(setf *processing-cam-pos* *camera-pos* *processing-cam-dir* *camera-dir*) ; return camera to default position
		;
		!safe-bind!	=rotation (+list *initial-cube-rotation* '(90 0 0)) ; rotate structure to align with cube rotation (maybe better after first path and merging)
		;
		=goal>
			state	remember-cube
		=spatial>
		+spatial-action>	; Rotating structure so it aligns with reference cube
			cmd		rotate-around-xyz
			value	=rotation
		+retrieval>
			class	cube
			origin	reference
			trial-id	=id
	)
	
		(P cannot-retrieve-cube-so-reencode
			=goal>
				state	remember-cube
			=spatial>
			?spatial-action>
				state	free
			?retrieval>
				state	error
			?visual-location>
				state	free
		==>
			!safe-eval!		(add-to-stage-markers "reencode_cube")
			=goal>
				state	find-base
				cubebaseknown	nil
				cubearrow1	nil
				cubearrow2	nil
			+visual-location>
				color	blue
				type	face
				kind	cube
			-spatial>
		)
	
	(P compare-first-arrow-try-first
		=goal>
			state	remember-cube
		=spatial>
			class	pattern
			points	=patternpoints
			aspects	=patternaspects
			delimiters	=patterndelims
		=retrieval>
			class	cube
			points	=cubepoints
			aspects =cubeaspects
			delimiters	=cubedelims
		?spatial-action>
			state	free
	==>
		!safe-bind!	=first-patternarrow (nthcdr 4 (return-from-group (list (position 'arrow =patternaspects)) =patterndelims =patternpoints))
		!safe-bind!	=firstpatternvector	(-list (second =first-patternarrow) (first =first-patternarrow)) 
		!safe-bind!	=first-cubearrow 	(nthcdr 4 (return-from-group (list (position 'arrow =cubeaspects)) =cubedelims =cubepoints))
		!safe-bind!	=firstcubevector	(-list (second =first-cubearrow) (first =first-cubearrow)) 
		=goal>
			state	first-arrow-compared
		=spatial>
		=retrieval>
		+spatial-action>
			cmd		compare-vector-angle
			value	=firstpatternvector
			axis	=firstcubevector
	)

		(P compare-first-arrow-try-second
			=goal>
				state	first-arrow-compared
				angle-threshold	=threshold
			    firstcomparisonvalid?	nil
			=spatial-action>
			  > result	=threshold
				result	=result
			=spatial>
				points	=patternpoints
				aspects	=patternaspects
				delimiters	=patterndelims
			=retrieval>
				points	=cubepoints
				aspects =cubeaspects
				delimiters	=cubedelims
			?spatial-action>
				state	free
		==>
		;	!eval!	(format t "Result of vector comparison: ~a Units of Deviation~%" =result)
			!safe-bind!	=first-patternarrow (nthcdr 4 (return-from-group (list (position 'arrow =patternaspects)) =patterndelims =patternpoints))
			!safe-bind!	=firstpatternvector	(-list (second =first-patternarrow) (first =first-patternarrow)) 
			!safe-bind!	=first-cubearrow 	(nthcdr 4 (return-from-group (list (position 'arrow =cubeaspects :from-end t)) =cubedelims =cubepoints))
			!safe-bind!	=firstcubevector	(-list (second =first-cubearrow) (first =first-cubearrow)) 
			=goal>
				state	first-arrow-compared
				firstcomparisonvalid?	no
			=spatial>
			=retrieval>
			+spatial-action>
				cmd		compare-vector-angle
				value	=firstpatternvector
				axis	=firstcubevector
		)
		
		(P comparison-mismatch
			=goal>
				state	first-arrow-compared
				firstcomparisonvalid?	no
				angle-threshold	=threshold
			=spatial-action>
			  > result	=threshold
			  result	=result
			=spatial>
			=retrieval>
			?manual>
				state	free
		==>
			!safe-eval!		(add-to-stage-markers "comparison_mismatch")
		;	!eval!	(format t "Result of vector comparison: ~a Units of Deviation~%" =result)
			+goal>
				state	answered
			-spatial-action>
			-spatial>
			-retrieval>
			+manual>
				cmd		press-key
				key		"w"
		)
		
	(P compare-second-arrow-try-first
		=goal>
			state	first-arrow-compared
			firstcomparisonvalid?	no
			angle-threshold	=threshold
		=spatial-action>
		  < result	=threshold
			result	=result
		=spatial>
			points	=patternpoints
			aspects	=patternaspects
			delimiters	=patterndelims
		=retrieval>
			points	=cubepoints
			aspects =cubeaspects
			delimiters	=cubedelims
		?spatial-action>
			state	free
	==>
	;	!eval!	(format t "Result of vector comparison: ~a Units of Deviation~%" =result)
		!safe-bind!	=first-patternarrow (nthcdr 4 (return-from-group (list (position 'arrow =patternaspects :from-end t)) =patterndelims =patternpoints))
		!safe-bind!	=firstpatternvector	(-list (second =first-patternarrow) (first =first-patternarrow)) 
		!safe-bind!	=first-cubearrow 	(nthcdr 4 (return-from-group (list (position 'arrow =cubeaspects)) =cubedelims =cubepoints))
		!safe-bind!	=firstcubevector	(-list (second =first-cubearrow) (first =first-cubearrow)) 
		=goal>
			state	second-arrow-compared
			firstcomparisonvalid?	nil
		=spatial>
		=retrieval>
		+spatial-action>
			cmd		compare-vector-angle
			value	=firstpatternvector
			axis	=firstcubevector
	)

	(P compare-second-arrow-try-second
		=goal>
			state	first-arrow-compared
			firstcomparisonvalid?	nil
			angle-threshold	=threshold
		=spatial-action>
		  < result	=threshold
			result	=result
		=spatial>
			points	=patternpoints
			aspects	=patternaspects
			delimiters	=patterndelims
		=retrieval>
			points	=cubepoints
			aspects =cubeaspects
			delimiters	=cubedelims
		?spatial-action>
			state	free
	==>
	;	!eval!	(format t "Result of vector comparison: ~a Units of Deviation~%" =result)
		!safe-bind!	=first-patternarrow (nthcdr 4 (return-from-group (list (position 'arrow =patternaspects :from-end t)) =patterndelims =patternpoints))
		!safe-bind!	=firstpatternvector	(-list (second =first-patternarrow) (first =first-patternarrow)) 
		!safe-bind!	=first-cubearrow 	(nthcdr 4 (return-from-group (list (position 'arrow =cubeaspects :from-end t)) =cubedelims =cubepoints))
		!safe-bind!	=firstcubevector	(-list (second =first-cubearrow) (first =first-cubearrow)) 
		=goal>
			state	second-arrow-compared
			; firstcomparisonvalid?	no
		=spatial>
		=retrieval>
		+spatial-action>
			cmd		compare-vector-angle
			value	=firstpatternvector
			axis	=firstcubevector
	)
		
	(P comparison-mismatch-on-second
		=goal>
			state	second-arrow-compared
		;	firstcomparisonvalid?	no
			angle-threshold	=threshold
		=spatial-action>
		  > result	=threshold
		  result	=result
		=spatial>
		=retrieval>
		?manual>
			state	free
	==>
		!safe-eval!		(add-to-stage-markers "comparison_mismatch")
	;	!eval!	(format t "Result of vector comparison: ~a Units of Deviation~%" =result)
		+goal>
			state	answered
		-spatial-action>
		-spatial>
		-retrieval>
		+manual>
			cmd		press-key
			key		"w"
	)
	
	(P comparison-match
		=goal>
			state	second-arrow-compared
			angle-threshold	=threshold
		=spatial-action>
		  < result =threshold
			result	=result
		=spatial>
		=retrieval>
		?manual>
			state	free
	==>
		!safe-eval!		(add-to-stage-markers "comparison_match")
	;	!eval!	(format t "Result of vector comparison: ~a Units of Deviation~%" =result)
		+goal>
			state	answered
		-spatial-action>
		-spatial>
		-retrieval>
		+manual>
			cmd		press-key
			key		"r"
	)
	
	; (P give-up-and-guess-match
		; =goal>
		; =temporal>
		  ; > ticks	50
		; ?manual>
			; state	free
	; ==>
		; -temporal>
		; =goal>
			; state	answered
		; +manual>
			; cmd		press-key
			; key		"r"
	; )
	
	; (P give-up-and-guess-mismatch
		; =goal>
		; =temporal>
		  ; > ticks	50
		; ?manual>
			; state	free
	; ==>
		; -temporal>
		; =goal>
			; state	answered
		; +manual>
			; cmd		press-key
			; key		"w"
	; )	
	
	(if *enable-utility-learning* (spp find-base :reward nil))
)






; Space for scrolling past end of code
















