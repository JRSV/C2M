;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Chord to Measure or C2M is a collection of methods that produce 
;; musical material according to shuffling rules that determin the structure
;; pitch / rhythm / form 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions used in C2M class
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-by-idx (idx list)
  (loop for elt in list
        for i from 0
     unless (= i idx) collect elt))
(defun chord-place-in-measure (in-chord in-order)
  (let* ((chord in-chord)
	 (map-with in-order)
	 (len)
	 (dist '()))
    (setq len (length chord))
    (loop for i from 0 to (- len 1) do
       (setq dist (econs dist (position i map-with))))
    dist))
(defun ts-from-chord-length (amount-of-beats rthm-fig)
  (let* ((base rthm-fig)
	 (chord amount-of-beats)
	 (number-of-units)
	 (ts '()))
    (setq number-of-units (length chord))
    (setq ts (push base ts))
    (setq ts (push number-of-units ts))
    ts))
(defun pack-in-measures (in-list-of-notes in-measure-length)
  (let* ((notes in-list-of-notes)
	 (measure-len in-measure-length)
	 (measure '())
	 (music '())
	 grouping group grp)	 
    (setq grouping (split-groups (length notes) measure-len))
    (setq grp 0)
    (loop for note in notes
       for place from 1 to (length notes) do
	 (push note measure)
	 (setq group (nth grp grouping))
	 ;;(format t "~% ~a ~a" group note)
	 (cond ((and (= (length measure) group)
		     (= (length measure) measure-len))
		(setq measure (nreverse measure))
		(push measure music)
		(setq measure '())
		(setq grp (1+ grp)))
	       ((and (= (length measure) group)
		     (not (equal (length measure) measure-len)))
		(loop repeat (- measure-len (length measure)) do
				(push 'r measure))
		(setq measure (nreverse measure))
		(push measure music)
		(setq measure '())
		(setq grp (1+ grp))
		)))
    (setq music (nreverse music))
    music))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The DATA STRUCTURE of the C2M class
;;___________________________________________________________________________
(defclass C2M ()
  ((chord :accessor C2M-chord)
   (unit :accessor C2M-beat-unit)
   (order :accessor C2M-order)
   (measures :accessor C2M-measures)
   (time-sig :accessor C2M-time-sig)
   (req-ratio :accessor C2M-req-ratio)

;; initform (C2M-time-sig )) ;; need to figure out initform
;; Methods

   (one-note-out :reader one-note-out)
   (shrink-chord :reader shrink-chord)
   (grab-and-pass :reader grab-and-pass)
   (deal-from-chord :reader deal-from-chord)
   (requantize :reader requantize)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Places the chord in the squence, according to the order given. It uses a 
;; distribution list based on the function "chord-place-in-measure" which 
;; should populate a slot in the C2M structure
;; ;wooden-box class inherits the box class  
;;___________________________________________________________________________
(defmethod chord-in-order ((object C2M))
  (let* ((chord (C2M-chord object))
	 (order (C2M-order object))
	 (beat-dur (C2M-beat-unit object))
	 (time-sig (C2M-time-sig object))
	 (bar '())
	 (dist '())
	 (measures))
    (setq time-sig (ts-from-chord-length chord beat-dur))
    (setq dist (chord-place-in-measure chord order))
    (loop repeat (length chord) for n from 0 do
       (setq bar '())
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (loop for i from 0 to (- (length chord) 1) do
	  (if (= (nth n dist) i)
	      (progn (format nil "~%PLACE IN : ~a CYCLE : ~a" (nth n dist) i)
		;;;;; here we process what we want;;;;;;;;;;;;;;;;;;;;;;;;;;
		     (setq bar (push chord bar)))
	      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	      (setq bar (push 'R bar))))
       (setq bar (nreverse bar))
       (setq measures (push bar measures)))
    (setq measures (nreverse measures))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Places the (chord - 1 note) according to "place-in-measure"
;; allows the possibility of using the "missing note" for the vibe
;;___________________________________________________________________________
(defmethod one-note-out ((object C2M))
  (let* ((chord (C2M-chord object))
	 (order (C2M-order object))
	 (time-sig (C2M-time-sig object))
	 (beat-dur (C2M-beat-unit object))
	 (bar '())
	 (dist '())
	 (measures)
	 note-out-chord)
    (setq time-sig (ts-from-chord-length chord beat-dur))
    (setq dist (chord-place-in-measure chord order))
    (loop repeat (length chord) for n from 0 do
       (setq bar '())       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (loop for i from 0 to (- (length chord) 1) do
	  (format nil "~%PLACE IN : ~a CYCLE : ~a" (nth n dist) n)
	  (if (= (nth n dist) i)
	      (progn
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;; process what we want we build the chord as we need
       		;; and make the event at the bottom 
		(setq note-out-chord (remove-by-idx n chord))
		(setq bar (push note-out-chord bar)))
	      (setq bar (push 'R bar))))
       (setq bar (nreverse bar))
       (setq measures (push bar measures)))
    (setq measures (nreverse measures))
    ))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M Shrinks the chord by (-1) pitch in each cycle
;;___________________________________________________________________________
(defmethod shrink-chord ((object C2M))
  (let* ((chord (C2M-chord object))
	 (order (C2M-order object))
	 (time-sig (C2M-time-sig object))
	 (beat-dur (C2M-beat-unit object))
	 (bar '())
	 (dist '())
	 (measures '())
	 len
	 (new-chord)
	 (deleted-note))
    (setq len (- (length chord) 1))
    (setq time-sig (ts-from-chord-length chord beat-dur))
    (setq dist (chord-place-in-measure chord order))
    (setq new-chord chord)
    (loop repeat (length chord) for n from 0 do ;; bar number
       (setq bar '())
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
       (loop for i from 0 to (- (length chord) 1) do
	  (if (= (nth n dist) i)
	      (setq bar (push new-chord bar))
	      (setq bar (push 'R bar))
		))
       (setq deleted-note (nth n chord))
       (setq new-chord (remove deleted-note new-chord))
       (setq bar (nreverse bar))
       (push bar measures))
  (setq measures (nreverse measures))
  ))
;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M will place the chord in the determined place but will leave behind 1
;;___________________________________________________________________________
(defmethod grab-and-pass ((object C2M))
  (let* ((chord (C2M-chord object))
	 (order (C2M-order object))
	 (time-sig (C2M-time-sig object))
	 (beat-dur (C2M-beat-unit object))
	 (bar '())
	 (dist '())
	 (measures '())
	 (len)
	 (rest-of-chord)
	 (place-to-edit)
	 (cur-measure)
	 (prev-measure)
	 beat-to-rmv)
    (setq len (- (length chord) 1))
    (setq time-sig (ts-from-chord-length chord beat-dur))
    (setq dist (chord-place-in-measure chord order))
    (setq rest-of-chord chord)
    (setq prev-measure (loop for rest from 0 to len
			  collect 'r))  
    (loop for m-number from 0 to len do
       (setq place-to-edit (nth m-number dist))
       (setq cur-measure prev-measure)
       (loop for beat from 0 to len do
	    (when (= beat place-to-edit)
	      ;; beat to extract from rest-of-chord
	      (setq beat-to-rmv (nth m-number chord))
	      ;; insert chord in "place-to-edit"
	    (setf (nth place-to-edit cur-measure) rest-of-chord)
	    (setq bar (copy-list cur-measure))
	    (setf (nth beat cur-measure) beat-to-rmv) 
	    (setq rest-of-chord (remove beat-to-rmv rest-of-chord))	    
	    ))
       (push bar measures))
    (setq measures (nreverse measures))
    ))
;;
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M chord remains fixed in position but deals a note in each cycle
;;___________________________________________________________________________
(defmethod deal-from-chord  ((object C2M))
  (let* ((chord (C2M-chord object))
	 (order (C2M-order object))
	 (time-sig (C2M-time-sig object))
	 (beat-dur (C2M-beat-unit object))
	 (dist '())
	 (measures '())
	 len
	 (rest-of-chord)
	 (place-to-edit)
	 (cur-measure)
	 (prev-measure)
	 (measure '())
	 note-to-rmv)
    (setq len (- (length chord) 1))
    (setq time-sig (ts-from-chord-length chord beat-dur))
    (print (setq dist (chord-place-in-measure chord order)))
    (setq rest-of-chord chord)
    ;;create the base measure, full of rests
    (setq prev-measure (loop for rest from 0 to len
			  collect 'R))
    (loop for m-number from 0 to len do
       (setq place-to-edit (nth m-number dist))    
       (setq cur-measure prev-measure)
       ;; inserts complete chord in beat of first itiration.
       (if (= m-number 0)
	   (progn ;;(format t "~%MEASURE Number ~a" m-number)
		  (setf (nth (nth 0 dist) cur-measure) chord)
		  ;;(format t "~%--- ~a ---" cur-measure)
		  ;;(setq measures (push cur-measure measures))
		  )
	   (progn ;;(format t "~%MEASURE Number ~a" m-number)
		  (setq note-to-rmv (nth 1 rest-of-chord))
		  (setq rest-of-chord (remove note-to-rmv rest-of-chord))
		  (setf (nth (nth 0 dist) cur-measure) rest-of-chord)
		  (setf (nth (nth m-number dist) cur-measure) note-to-rmv)
		  ))
       (setq measure (copy-list cur-measure))
       (setq measures (push measure measures)))
    (setq measures (nreverse measures))
    ))
;;
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M REQUANTIZE
;;___________________________________________________________________________
(defmethod requantize ((object C2M))
  (let* ((measures (C2M-measures object))
	 ;;(beat-dur (C2M-beat-unit object))
	 ;;(time-sig (C2M-time-sig object))
	 (req-ratio (C2M-req-ratio object))
	 (out-measures '())
	 bt bar
	 (bars '())) 
    (loop for measure in measures do
	 (setq bar '())
	 (loop for beat in measure do
	      (cond ((not(equal beat 'R))
		     ;; we get rid of "chords of 1 note"
		     (if (and (listp beat) (= (length beat) 1))
			 (setq bt (first beat))
			 (setq bt beat))
		     (loop repeat req-ratio do
			  (setq bar (push bt bar))))
		    ((equal beat 'R)
		     (loop repeat req-ratio do
			  (setq bar (push 'R bar)))
		     )))    	 
	 (setq bar (nreverse bar))
	 (push bar bars))
    (loop for bar in bars do
	 (loop for beat in bar do
	      (push beat out-measures)
	      ))
    out-measures
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C2M bar format -> Slippery Chicken -> Lilypond. It provides a sc
;; object full of events and will be named as it is in. Renders the
;; music to a local
;; file.
;;______________________________________________________________________
(defmethod render-local  ((object C2M))
  (let* ((measures (C2M-measures object))
	 ;;(beat-dur (C2M-beat-unit object))
	 (time-sig (C2M-time-sig object))
	 ;;(render-name in-render-name)
	 (out-measures '())
	 ;;(tempo in-tempo)
	 (ties '())
	 (prev-tie 0)
	 (next-tie 0)
	 (prev-note-event 0)
	 time-sig bar measure-noTS
	 output-score midifile-name
	 event-number base-path
	 note-event tie
	 beat dur)   
    ;;(setq base-path (format base-path "~a/score/" path))
    ;;(setq midifile-name (format nil "~a/midi/~a.mid" base-path name))
    ;; (loop for voice in measures
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (print time-sig)
    (setq ties '())
    (loop for measure in measures
       for m-number from 0 do
	 (setq beat-dur (* (/ (length measure) (nth 0 time-sig))
			   (nth 1 time-sig)))
	 (setq bar '())
	 (loop for beat in measure
	    for b-number from 0 do
	      (setq tie '())
	      (setq tie (push b-number tie))
	      (cond ((not (equal beat 'R))		    
		     (setq bar (push (make-event (2-note beat) beat-dur)
				     bar))
		     (setq tie (push 1 tie)
			   tie (push (2-note beat) tie)))
		    ((equal beat 'R)
		     (setq bar (push (make-event nil beat-dur
						 :is-rest t)
				     bar))
		     (setq tie (push 0 tie)
			   tie (push beat tie))))
	      (setq tie (nreverse tie))
	      (push tie ties))
	 (setq bar (nreverse bar)
	       bar (make-rthm-seq-bar (push time-sig bar)))
	 (push bar out-measures)
	 )
    (setq out-measures (nreverse out-measures))
    (setq output-score (bars-to-sc out-measures
				   :sc-name  (make-symbol render-name)
				   ;;:player render-name
	 			   :instrument 'piano
					;:tempo tempo
				   ))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TIE NOTES POST-GEN
    (setq ties (nreverse ties))
    (next-event output-score 'player-one nil t)
    (setq event-number 0)
    ;;This loop turns on the corresponding Ties
    (loop for next-e = (next-event output-score 'player-one)
       while next-e for tie in ties do
    	 (if (slot-value next-e 'is-rest)
    	     (progn (format nil "============= SILENCE")
    		    (format nil "~%Event: ~a Rest: ~a"
    			    (incf event-number)
			    (slot-value next-e 'is-rest)))
    	     (if (get-pitch-symbol next-e)
    		 (progn (format nil "*******************************NOTE")
    			(setq next-tie (nth (+ event-number 1) ties))
    			(format nil "~%E#: ~a | Prev Tie: ~a | Tie: ~a | Next Tie: ~a |"
    				event-number (nth 1 prev-tie) (nth 1 tie)(nth 1 next-tie))
    			(cond ((and (not (equal (nth 2 prev-tie) (nth 2 tie)))
    				   (equal (nth 2 tie) (nth 2 next-tie)))
    			       (format nil " ---- FIRST of Repeted Group ----")
    			       (format nil "~%Prev: ~a~%Curr:  ~a~%Next:     ~a~%"
    				       (nth 2 prev-tie)
    				       (nth 2 tie)
    				       (nth 2 next-tie))
    			       (setf (is-tied-from next-e) t)
    			       (setf (is-tied-to next-e) t)))   
    			(cond ((and (equal (nth 2 prev-tie) (nth 2 tie))
    				    (equal (nth 2 tie) (nth 2 next-tie)))
    			      (format nil " ------- MIDDLE Group -------")
    			      (format nil "~%Prev: ~a~%Curr:  ~a~%Next:     ~a~%"
    				      (nth 2 prev-tie)
    				      (nth 2 tie)
    				      (nth 2 next-tie))
    			      (setf (is-tied-from next-e) t)
    			      (setf (is-tied-to next-e) t)))
    			(cond ((and (equal (nth 2 prev-tie) (nth 2 tie))
    				    (not (equal (nth 2 tie) (nth 2 next-tie))))
    			       (format nil " ---- LAST of Repeted Group ----")
    			       (format nil "~%Prev: ~a~%Curr:  ~a~%Next:     ~a~%"
    				       (nth 2 prev-tie)
    				       (nth 2 tie)
    				       (nth 2 next-tie))
    			       (setf (is-tied-from next-e) nil)
    			       (setf (is-tied-to next-e) t)))
    			(setq prev-note-event note-event)
    			(incf event-number)
    		 	)))
    	 (setq prev-tie tie))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SCORE GENERATION
    (handle-ties output-score)
    (rebar output-score)
    ;;(setf (title output-score) (format nil "~a" render-name))
    ;;(midi-play output-score :midi-file midifile-name)
    ;;(write-lp-data-for-all output-score)
    ;;(lp-display output-score :base-path base-path)
    output-score
    ))


;;; - examples - ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq C2M_example_seq (make-instance 'C2M))
(setf (C2M-chord C2M_example_seq) '(60 62 69 76 81))
(setf (C2M-order C2M_example_seq) '(4 2 0 3 1))
(setf (C2M-time-sig C2M_example_seq) '(7 8))
(setf (C2M-beat-unit C2M_example_seq) 8)


(chord-in-order C2M_example_seq)
(grab-and-pass C2M_example_seq)
(shrink-chord C2M_example_seq)
(one-note-out C2M_example_seq)
(deal-from-chord C2M_example_seq)

;; to operate on a sequence produced, the "measures" slot must be populated

;; I call a method to generate a sequence
(setq measures (chord-in-order C2M_example_seq))
;; then I populate that slot with the previous result
;; so my data structure  has the measure slot full
(setf (C2M-measures C2M_example_seq) measures)

(setf (C2M-req-ratio C2M_example_seq) 3)
;; I can call the requantize method to expand
(setf (C2M-measures C2M_example_seq) (pack-in-measures (requantize C2M_example_seq) 3))

(C2M-measures C2M_example_seq)
(C2M-time-sig C2M_example_seq)
;;(render-local C2M_example_seq)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
