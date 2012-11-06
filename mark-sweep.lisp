(defclass <lobject> ()
  ((mark :initform nil :initarg :mark :accessor mark)
   (next :initform nil :initarg :next :accessor next)))

(defclass <cons> (<lobject>)
  ((car-ref :initarg :car-ref :accessor car-ref)
   (cdr-ref :initarg :cdr-ref :accessor cdr-ref)))

(defun create-cons (car-lobj cdr-lobj)
  (make-instance '<cons>
                 :car-ref car-lobj
                 :cdr-ref cdr-lobj))

(defclass <atom> (<lobject>)
  ((val :initform nil :initarg :val :accessor val)))

(defparameter *max-atom* 25)
(defparameter *max-cons* 25)

(defclass <bind-table> ()
  ((bind-table :initform nil :accessor bind-table)))

(defmethod bind ((table <bind-table>) (symb Symbol) (lobj <lobject>))
  (push
    (cons symb lobj)
    (bind-table table)))

(defmethod unbind ((table <bind-table>)
                   (symb Symbol))
  (setf (bind-table table)
        (remove-if (lambda (bind)
                     (eql (car bind) symb))
                   (bind-table table))))

(defmethod lookup ((table <bind-table>) (symb Symbol))
  (assoc symb (bind-table table)))

(defmethod obj-boundp ((table <bind-table>) (obj <lobject>))
  (find obj (bind-table table)
        :key #'cdr
        :test #'equal))

(defclass <lisp-vm> ()
  ((free-atoms :initform 
               (make-array *max-atom*
                           :initial-contents
                           (loop repeat *max-atom*
                                 for new-atom = (make-instance '<atom>)
                                 collect new-atom))
               :accessor free-atoms)
   (free-cells :initform
               (make-array *max-cons*
                           :initial-contents
                           (loop repeat *max-cons*
                                 for new-cons = (make-instance '<cons>)
                                 collect new-cons))
                :accessor free-cells)
   (bind-table :initform 
               (make-instance '<bind-table>)
               :accessor bind-table)
   (free-cell-top :accessor free-cell-top)
   (free-cell-bottom :accessor free-cell-bottom)
   (free-atom-top :accessor free-atom-top)
   (free-atom-bottom :accessor free-atom-bottom)))

(defmethod init-vm ((vm <lisp-vm>))
  (with-slots (free-cells free-atoms
                  free-cell-top free-cell-bottom
                  free-atom-top free-atom-bottom) vm
    ;;Initialize free area
    (init-free-cells free-cells)
    (init-free-atoms free-atoms)
    (setf (bind-table (bind-table vm)) nil)
    ;;Setup pointers
    (setf free-cell-top (aref free-cells 0))
    (setf free-cell-bottom (aref free-cells 
                                 (1- (length free-cells))))
    (setf free-atom-top (aref free-atoms 0))
    (setf free-atom-bottom (aref free-atoms
                                 (1- (length free-atoms))))))

(defun init-free-cells (cells)
  (loop for idx from 0 upto (- (length cells) 2)
        for prev-cell = (aref cells idx)
        for next-cell = (aref cells (1+ idx))
        do
        (setf (car-ref prev-cell) nil
              (cdr-ref prev-cell) nil
              (next prev-cell) next-cell)
        finally
        (setf (car-ref next-cell) nil
              (cdr-ref next-cell) nil)
  (setf (next (aref cells (1- (length cells)))) nil)))

(defun init-free-atoms (atoms)
  (loop for idx from 0 upto (- (length atoms) 2)
        for prev-atom = (aref atoms idx)
        for next-atom = (aref atoms (1+ idx))
        do
        (setf (val prev-atom) nil
              (next prev-atom) next-atom)
  (setf (next (aref atoms (1- (length atoms)))) nil)))


(defun create-vm ()
  (let ((vm (make-instance '<lisp-vm>)))
    (init-vm vm)
    vm))

(defmethod alloc-cons ((vm <lisp-vm>))
  ;;まずフリーリストの先頭を取得
  (let ((new-cons (free-cell-top vm)))
    (if (not (null new-cons))
      (progn
        ;;フリーリストを一つすすめる
        (setf (free-cell-top vm)
            (next (free-cell-top vm)))
        (setf (car-ref new-cons) nil
              (cdr-ref new-cons) nil)
        new-cons)
      (progn
        (run-gc vm)
        (if (not (null (free-cell-top vm)))
          (alloc-cons vm)
          (error "Can't allocate new cons.."))))))

(defmethod alloc-atom ((vm <lisp-vm>))
  (let ((new-atom (free-atom-top vm)))
    (if (not (null new-atom))
      (progn
        (setf (free-atom-top vm)
            (next (free-atom-top vm)))
        (setf (val new-atom) nil)
        new-atom)
      (progn
        (run-gc vm)
        (if (not (null (free-atom-top vm)))
          (alloc-atom vm)
          (error "Can't allocate new atom.."))))))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  Mark
;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod recur-mark ((obj <lobject>))
  (error "Unknown object"))

(defmethod recur-mark ((cell <cons>))
  (setf (mark cell) t)
  (when (car-ref cell)
    (recur-mark (car-ref cell)))
  (when (cdr-ref cell)
    (recur-mark (cdr-ref cell))))

(defmethod recur-mark ((atom <atom>))
  (setf (mark atom) t))

(defmethod mark-phrase ((vm <lisp-vm>))
  (loop for bind in (bind-table (bind-table vm))
        do
        (recur-mark (cdr bind))))

(defmethod display-vm-state ((vm <lisp-vm>))
  (princ "Cells: ")
  (loop for cell across (free-cells vm)
        do
        (cond ((mark cell) (format t "*"))
              ((equal (free-cell-top vm) cell)
               (format t "F"))
              ((equal (free-cell-bottom vm) cell)
               (format t "B"))
              (t (format t "-")))
        finally (format t "~%"))
  (princ "Atoms: ")
  (loop for atom across (free-atoms vm)
        do
        (cond ((mark atom) (format t "*"))
              ((equal (free-atom-top vm) atom)
               (format t "F"))
              ((equal (free-atom-bottom vm) atom)
               (format t "B"))
              (t (format t "-")))
        finally (format t "~%"))
  (format t "Bind: ~%~{|~A|~%~}~%" (bind-table (bind-table vm))))

(defmethod bind-val ((vm <lisp-vm>) (sym Symbol) (lobj <lobject>))
  (bind (bind-table vm) sym lobj))

;;;;;;;;;;;;;;;;;
;;
;; Sweep
;;
;;;;;;;;;;;;;;;;;
(defmethod sweep-cells ((vm <lisp-vm>))
  ;;setup free list
  (loop for cell across (free-cells vm)
        until (not (mark cell))
        finally (setf (free-cell-top vm) cell
                      (free-cell-bottom vm) cell))
  (loop for cell across (free-cells vm)
        if (not (mark cell))
        do
        (setf (next (free-cell-bottom vm)) cell)
        (setf (free-cell-bottom vm) cell)
        else
        do
        (setf (mark cell) nil)
        finally (setf (next (free-cell-bottom vm)) nil)))

(defmethod sweep-atoms ((vm <lisp-vm>))
  ;;setup free list
  (loop for atom across (free-atoms vm)
        until (not (mark atom))
        finally (setf (free-atom-top vm) atom
                      (free-atom-bottom vm) atom))
  (loop for atom across (free-atoms vm)
        if (not (mark atom))
        do
        (setf (next (free-atom-bottom vm)) atom)
        (setf (free-atom-bottom vm) atom)
        else
        do
        (setf (mark atom) nil)
        finally (setf (next (free-atom-bottom vm)) nil)))

(defmethod sweep-phrase ((vm <lisp-vm>))
    (sweep-atoms vm)
    (sweep-cells vm))

;;;;
;;; GC
;;; 
(defmethod run-gc ((vm <lisp-vm>))
  (format t "[Garbage collection]~%");
  (mark-phrase vm)
  (sweep-phrase vm))

(defmethod gc-test ((vm <lisp-vm>))
  (loop repeat (+ *max-cons* 5)
        do
        (display-vm-state vm)
        (if (zerop (random 5))
          (bind-val vm (gensym) (alloc-cons vm))
          (alloc-cons vm))))

(defmethod primitive-p (val)
  (or (numberp val)
      (stringp val)))

(defmacro aif (test then else)
  `(let ((it ,test))
     (if it
       ,then
       ,else)))

(defmethod leval ((vm <lisp-vm>) expr)
  (cond ((null expr)
         nil)
         ((primitive-p expr)
         (let ((new-atom (alloc-atom vm)))
           (setf (val new-atom) expr)
           new-atom))
        ((symbolp expr)
         (aif (lookup (bind-table vm) expr)
           (cdr it)
           (error "Undefined variable")))
        ((and (eql 'bind (car expr))
              (symbolp (cadr expr)))
         (bind-val vm (cadr expr) 
                   (leval vm (caddr expr))))
        ((and (eql 'cons (car expr))
              (= 2 (length (cdr expr))))
         (let ((new-cons (alloc-cons vm)))
           (setf (car-ref new-cons) (leval vm (cadr expr))
                 (cdr-ref new-cons) (leval vm (caddr expr)))
           new-cons))
        ((eql 'car (car expr))
         (car-ref (leval vm (cadr expr))))
        ((eql 'cdr (car expr))
         (cdr-ref (leval vm (cadr expr))))
        (t (error "Unrecognizable expr"))))

(defmethod print-object ((obj <cons>) stream)
  (princ "(" stream)
  (if (null (car-ref obj))
    (princ "NIL" stream)
    (print-object (car-ref obj) stream))
  (when (not (null (cdr-ref obj)))
    (princ " . " stream)
    (print-object (cdr-ref obj) stream))
  (princ ")" stream))

(defmethod print-object ((obj <atom>) stream)
  (princ (val obj) stream))
(defmethod lisp-vm-repl ((vm <lisp-vm>))
  (loop 
    do
    (display-vm-state vm)
    (format t "~A~%" (leval vm (read)))))

