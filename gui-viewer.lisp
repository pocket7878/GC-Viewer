(load "mark-sweep.lisp")

(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-gfx)

(defparameter *cell-size* 20)

(defmethod draw-vm-state ((vm <lisp-vm>))
  (with-slots (bind-table 
                free-cell-top free-cell-bottom
                free-atom-top free-atom-bottom
                free-cells free-atoms) vm
    (loop for cell across free-cells
          for idx from 1 upto (length free-cells)
          do
          (cond 
            ((equal cell free-cell-top)
             (sdl-gfx:draw-box-* (* *cell-size* idx) *cell-size* *cell-size* *cell-size* :color sdl:*red*))
            ((equal cell free-cell-bottom)
             (sdl-gfx:draw-box-* (* *cell-size* idx) *cell-size* *cell-size* *cell-size* :color sdl:*blue*))
            (t
             (sdl-gfx:draw-rectangle-* (* *cell-size* idx) *cell-size* *cell-size* *cell-size* :color sdl:*black*))))
    (loop for atom across free-atoms
          for idx from 1 upto (length free-atoms)
          do
          (cond 
            ((equal atom free-atom-top)
             (sdl-gfx:draw-box-* (* *cell-size* idx) (* 3 *cell-size*) *cell-size* *cell-size* :color sdl:*red*))
            ((equal atom free-atom-bottom)
             (sdl-gfx:draw-box-* (* *cell-size* idx) (* 3 *cell-size*) *cell-size* *cell-size* :color sdl:*blue*))
            (t
             (sdl-gfx:draw-rectangle-* (* *cell-size* idx) (* 3 *cell-size*) *cell-size* *cell-size* :color sdl:*black*))))))

(defun vm-simulator (prog-list)
  (let ((vm (create-vm))
        (width (* *cell-size*
                  (+ 2 (max *max-atom* *max-cons*)))))
    (sdl:with-init ()
        (sdl:window width (* *cell-size* 5) :title-caption "Lisp VM Simulator")
        (sdl:clear-display sdl:*white*)
        (setf (sdl:frame-rate) 2)
        (sdl:with-events ()
          (:quit-event () t)
          (:idle
            (when (not (null prog-list))
              (leval vm (pop prog-list)))
            (sdl:clear-display sdl:*white*)
            (draw-vm-state vm)
            (sdl:update-display))))))

(defun vm-simulator-demo ()
  (let ((vm (create-vm))
        (width (* *cell-size*
                  (+ 2 (max *max-atom* *max-cons*)))))
    (sdl:with-init ()
        (sdl:window width (* *cell-size* 5) :title-caption "Lisp VM Simulator")
        (sdl:clear-display sdl:*white*)
        (setf (sdl:frame-rate) 2)
        (sdl:with-events ()
          (:quit-event () t)
          (:idle
            (if (zerop (random 5))
              (if (zerop (random 2))
                (leval vm `(bind ,(gensym) (cons 10 10)))
                (leval vm `(bind ,(gensym) 10)))
              (if (zerop (random 2))
                (leval vm `(cons 10 10))
                (leval vm 10)))
            (sdl:clear-display sdl:*white*)
            (draw-vm-state vm)
            (sdl:update-display))))))
