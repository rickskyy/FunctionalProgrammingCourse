
(defun is-break (char)
  (or (eq #\. char) (eq #\! char) (eq #\? char))
)

(defun get-break-list ()
	(list #\! #\? #\.)
)

;;; construct a string of punctuation marks that ends sentences in text (e. g. ".!.?.")
(defun extract-breaks (text)
  (remove-if-not 'is-break text))

;;; read all file content into a single string
(defun get-file-content (filename)
  (with-open-file (stream filename)
    (first (loop for line = (read-line stream nil)
          while line
          collect line))))

(defun split (chars str &optional (lst nil) (accm ""))
  (cond
    ((= (length str) 0) (reverse (cons accm lst)))
    (t
     (let ((c (char str 0)))
       (if (member c chars)
    (split chars (subseq str 1) (cons accm lst) "")
    (split chars (subseq str 1) 
                        lst 
                        (concatenate 'string
           accm
         (string c))))
            ))))

;;; find the number of sentences which ends with question mark
(defun number-question-marks (slist)
	(setq i 0)
	(loop for symbol across slist do
		(if (eq symbol #\?)
			(setq i (+ i 1))
		)
	)
	(return-from number-question-marks i)
)

;;; create an array with only quest
(defun create-qsentence-array (origText sentenceEnds)
	(setq senList (make-array (number-question-marks sentenceEnds) :adjustable T))

	(setq i 0)
	(setq k 0)
	(loop for sentence in (split (get-break-list) origText) do
		(if (and (< i (length sentenceEnds)) (eq (aref sentenceEnds i) #\?))
			(progn 
				(setf (aref senList k) sentence) 
				(setq k (+ k 1))
			)
		)
		(setq i (+ i 1))
	)
	(return-from create-qsentence-array senList)
)

;;; print to console lists of unique words
(defun print-unique-words (senList n)
	(loop for i from 0 to (- (length senList) 1) do
		(defparameter sentence_set ())
		(loop for word in (split '(#\Space) (aref senList i)) do
			(if (eq (length word) n)
				(pushnew (intern word) sentence_set)
			)
		)
		(print sentence_set)
	)
)

(defun main ()
	(setq origText "")
	(setq origText (get-file-content "./text.txt"))
	(setq sentenceEnds (extract-breaks origText))
	(print sentenceEnds)
	(setq senList (create-qsentence-array origText sentenceEnds))
	(setq n 3)
	(print-unique-words senList n)
)

(main)