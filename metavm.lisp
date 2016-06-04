;; Virtual machine for META II. Steve Bagley, Sept 2015,
;; stevenbagley@fastmail.fm. This is based on Val Schorre, A Syntax
;; Oriented Compiler Writing Language, and especially James Neighbors,
;; http://www.bayfronttechnologies.com/mc_tutorial.html

;; Depends on: quicklisp and cl-ppcre

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'cl-ppcre)
  )

;; Utilities, and code for parsing a file into lisp-based tokens.

(defun string-append (&rest strings)
  (apply #'concatenate 'string strings))

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; does leaf appear in tree?
  (defun has-leaf? (leaf tree)
    (cond ((null tree) nil)
          ((eq leaf tree) t)
          ((atom tree) nil)
          ((or (has-leaf? leaf (car tree))
               (has-leaf? leaf (cdr tree))))))
  )

(defun file-as-string (file)
  (with-open-file (stream file :direction :input)
    (let ((s (make-string (file-length stream))))
      (read-sequence s stream)
      s)))

;; change all single quotes to double quotes
(defun convert-single-to-double (string)
  (cl-ppcre:regex-replace-all "'" string "\""))

;; "foo" -> ("foo"), "foo bar" -> ("foo" "bar")
(defun split-line-at-space (line)
  (let* ((line2 (string-trim '(#\space #\tab) line))
         (pos (position #\space line2)))
    (if (null pos)
        (list line2)
        (list (subseq line2 0 pos)
              (subseq line2 (1+ pos))))))

;; handle single quotes in strings
;; "'foo'" -> "foo", "foo" -> foo
(defun convert-string (string)
  (if (eql #\' (char string 0))
      (string-trim '(#\') string)
      (intern string)))

(defun parse-line (line)
  (cond ((eql #\space (char line 0))
         (mapcar #'convert-string (split-line-at-space line)))
        (:else
         (intern (string-trim '(#\space #\tab) line)))))

;; read file containing META code, return list of sexprs.
(defun parse-file (file)
  (with-open-file (s file :direction :input)
    (loop for line = (read-line s nil :eof)
          until (eq line :eof)
          collect (parse-line line))))

;; convert from lists of exprs to a vector of instructions. exprs are
;; contiguous expressions, separated by labels (symbols). returns list
;; of (label-map block), where block is a vector of instructions, and
;; label-map maps from labels to instruction locations (vector index).
(defun build-instructions (exprs)
  (loop with index = 0
        and label-map = nil
        and block = nil
        for expr in exprs
        do (cond ((symbolp expr)
                  ;; record index of label
                  (push (list expr index) label-map))
                 (:else
                  (push expr block)
                  (incf index)))
        finally (return (list label-map (make-array (length block)
                                                    :element-type t
                                                    :initial-contents (nreverse block))))))

;; build a register machine using lisp globals.

(defvar *switch*)
(defvar *label1*)
(defvar *label2*)
(defvar *output-buffer*)
(defvar *output-column*)
(defvar *buffer*)
(defvar *buffer-index*)
(defvar *token*)
(defvar *call-stack*)
(defvar *label-map*)
(defvar *block*)
(defvar *program-counter*)

(defvar *instruction-table* (make-hash-table :size 20 :test #'eq))

(defvar *label-counter* 0)

(defun generate-label ()
  (prog1
      (intern (format nil "L~D" *label-counter*))
    (incf *label-counter*)))

(defun check-buffer-index ()
  (when (>= *buffer-index* (length *buffer*))
    (format t "~&Hit end of input buffer, exiting.")
    (throw 'run-loop nil)))

(defun run-inst (inst)
  (let* ((op (car inst))
         (proc (gethash op *instruction-table*)))
    (when (null proc)
      (error (format nil "missing proc for ~S" op)))
    (apply proc (cdr inst))))

;; Looks up label in label-map, and returns the vector index of that
;; label.
(defun get-label-index (label label-map)
  (let ((entry (assq label label-map)))
    (if entry
        (cadr entry)
        (error "can't find label ~S in label-map ~S" label label-map))))

(defun print-machine-state ()
  (format t "~%")
  (format t "~&call-stack: ~S" *call-stack*)
  (format t "~&program-counter: ~D" *program-counter*)
  (format t "~&buffer: ~S" (subseq *buffer* *buffer-index* (min (+ *buffer-index* 20) (length *buffer*))))
  (format t "~&buffer-index: ~S" *buffer-index*)
  (format t "~&label1: ~S" *label1*)
  (format t "~&label2: ~S" *label2*)
  (format t "~&switch: ~S" *switch*)
  (format t "~&token: ~S" *token*)
  (format t "~&output-buffer: ~S" *output-buffer*)
  (format t "~&output-column: ~S" *output-column*)
  (format t "~%"))

;; top level function, runs instructions (from build-instructions),
;; taking input from string.
(defun run (instructions string &optional (trace nil))
  (setq *label-counter* 0)
  (setq *switch* :none)
  (setq *label1* nil)
  (setq *label2* nil)
  (setq *buffer* string)
  (setq *buffer-index* 0)
  (setq *output-buffer* nil)
  (setq *output-column* 0)
  (setq *token* nil)
  (setq *label-map* (car instructions))
  (setq *block* (cadr instructions))
  (setq *call-stack* nil)
  (when trace
    (pprint (coerce *block* 'list)))
  (setq *program-counter* 0)
  (catch 'run-loop
    (loop with n = (length *block*)
          do (let ((inst (aref *block* *program-counter*)))
               (when trace
                 (format t "~&~%RUN-INST: ~S" inst))
               (run-inst inst)
               (when trace
                 (print-machine-state)
                 (read-char))
               (cond ((null *program-counter*)
                      (format t "Trying to return from subroutine when call stack is empty. Exiting.")
                      (loop-finish))
                     ((>= *program-counter* n)
                      (loop-finish))
                     ((and (null *call-stack*)
                           (null *output-buffer*)
                           (= *buffer-index* (length *buffer*)))
                      (format t "~&~%Hit EOF in buffer, exiting.")
                      (loop-finish))))))
  'done)

;; macro for defining the machine instructions. Each instruction names
;; a procedure stored in hash table. Note that this adds code to
;; increment the program counter unless the instruction body already
;; does something to that variable.
(defmacro definst (inst operand &body body)
  (let* ((uses-pc? (has-leaf? '*program-counter* body))
         (new-body (if uses-pc?
                       body
                       (append body '((incf *program-counter*))))))
    `(setf (gethash ',inst *instruction-table*)
                  (lambda ,operand ,@new-body))))

;; the following are all the machine instructions.

;; name of program, so just jump to starting label
(definst ADR (label)
  (setq *program-counter* (get-label-index label *label-map*)))

;; end of program, do nothing
(definst END ())

(defun test-for-string (target-string)
  (check-buffer-index)
  (multiple-value-bind (match-start match-end)
      (cl-ppcre:scan (string-append "^\\s*" (cl-ppcre:quote-meta-chars target-string)) *buffer* :start *buffer-index*)
    (cond (match-start
           (setq *buffer-index* match-end)
           (setq *switch* t))
          (:else
           (setq *switch* nil)))))

(definst TST (target-string)
  (test-for-string target-string))

(defun look-for-identifier-token ()
  (check-buffer-index)
  (multiple-value-bind (match-string)
      (cl-ppcre:scan-to-strings "^\\s*[A-Za-z][A-Za-z0-9]*" *buffer* :start *buffer-index*)
    (cond (match-string
           (incf *buffer-index* (length match-string))
           (setq match-string (string-left-trim '(#\space #\tab #\newline) match-string))
           (setq *token* match-string)
           (setq *switch* t))
          (:else
           (setq *switch* nil)))))

(definst ID ()
  (look-for-identifier-token))

(defun look-for-number-token ()
  (check-buffer-index)
  (multiple-value-bind (match-string)
      (cl-ppcre:scan-to-strings "^\\s*[0-9]+" *buffer* :start *buffer-index*)
    (cond (match-string
           (incf *buffer-index* (length match-string))
           (setq match-string (string-left-trim '(#\space #\tab #\newline) match-string))
           (setq *token* match-string)
           (setq *switch* t))
          (:else
           (setq *switch* nil)))))

(definst NUM ()
  (look-for-number-token))

(defun look-for-string-token ()
  (check-buffer-index)
  (multiple-value-bind (match-string)
      (cl-ppcre:scan-to-strings "^\\s*\"[^\"]+\"" *buffer* :start *buffer-index*)
    (cond (match-string
           (incf *buffer-index* (length match-string))
           (setq match-string (string-left-trim '(#\space #\tab #\newline) match-string))
           (setq *token* match-string)
           (setq *switch* t))
          (:else
           (setq *switch* nil)))))

(definst SR ()
  (look-for-string-token))

;; call subroutine
(definst CLL (label)
  (progn
    ;; save the current control state
    (push :blank *label1*)
    (push :blank *label2*)
    (push *program-counter* *call-stack*)
    ;; jump to label
    (setq *program-counter* (get-label-index label *label-map*))))

;; return from subroutine
(definst R ()
  (progn
    ;; pop stack
    (pop *label1*)
    (pop *label2*)
    ;; jump to instruction following previous program location
    (let ((addr (pop *call-stack*)))
      (if addr
          (setq *program-counter* (1+ addr))
          (setq *program-counter* nil)))))

;; set switch.
(definst SET ()
  (setq *switch* t))

;; unconditional branch
(definst B (label)
  (setq *program-counter* (get-label-index label *label-map*)))

;; branch if false
(definst BF (label)
  (if (not *switch*)
      (setq *program-counter* (get-label-index label *label-map*))
      (incf *program-counter*)))

;; branch if true
(definst BT (label)
  (if *switch*
      (setq *program-counter* (get-label-index label *label-map*))
      (incf *program-counter*)))

;; branch to error if false
(definst BE ()
  (cond ((not *switch*)
         (format t "~&META-II error. Halting.")
         (throw 'run-loop nil))
        (:else
         (incf *program-counter*))))

;; copy literal to output
(definst CL (string)
  (push string *output-buffer*))

;; copy input to output
(definst CI ()
  (push *token* *output-buffer*))

;; generate label1
(definst GN1 ()
  (progn
    (if (eq :blank (car *label1*))
        (push (generate-label) *label1*))
    (push (car *label1*) *output-buffer*)))

;; generate label2
(definst GN2 ()
  (progn
    (if (eq :blank (car *label2*))
        (push (generate-label) *label2*))
    (push (car *label2*) *output-buffer*)))

;; move output pointer to label field
(definst LB ()
  (setq *output-column* 0))

;; print the current output buffer, then reset
(definst OUT ()
  (progn
    (loop repeat *output-column* do (princ " "))
    (loop for string in (reverse *output-buffer*)
          do (princ string))
    (terpri)
    (setq *output-buffer* nil)
    (setq *output-column* 8)))
