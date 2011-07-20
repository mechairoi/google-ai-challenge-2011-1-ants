;;;; ants-bot.lisp

(in-package :ants-bot)


;;; Functions

;; This is the actual 'AI'.  Very simple currently: loops through each of your
;; ants and issues an order to go either north, east, south or west if the tile
;; in the direction is not a water tile.
(defun do-turn ()
  (logmsg "[do-turn] " (length (my-ants *state*)) " ants~%")
  (calc-distance-to-food)
  (loop for ant in (my-ants *state*)
        for row = (row ant)
        for col = (col ant)
        do (let ((after (greedy-tile ant)))
             (if after (issue-order-from-tile ant after)
                 (setf (next-flag ant) t)))))


;;; Queue
(defstruct queue (front nil) (rear nil))

(defun enqueue (queue item)
  (let ((new-cell (list item)))
    (if (queue-front queue)
      (setf (cdr (queue-rear queue)) new-cell)
      (setf (queue-front queue) new-cell))
    (setf (queue-rear queue) new-cell)))

(defun dequeue (queue)
  (if (queue-front queue)
      (prog1
        (pop (queue-front queue))
        (unless (queue-front queue)
          (setf (queue-rear queue) nil)))))

(defun empty-queue? (queue)
  (not (queue-front queue)))

;;; Anaphoric
(defmacro anaphoric (op test &body body)
  `(let ((it ,test))
     (,op it ,@body)))

(defmacro aif (test then &optional else)
  `(anaphoric if ,test ,then ,else))

(defmacro aand (first &rest rest)
  `(anaphoric and ,first ,@rest))

;;; Utils

(defun greedy-tile (tile)
  (first (sort
          (remove-if #'(lambda (ti) (or (waterp ti)  (nextp ti)))
                     (neighbor-tiles tile))
          #'(lambda (a b) (< (or (distance-to-food a) 10000000)
                             (or (distance-to-food b) 10000000))))))

(defun issue-order-from-tile (before after)
  (setf (next-flag after) t)
  (issue-order (row before) (col before) (dir-from-tile before after)))

(defun dir-from-tile (before after)
  (let ((tiles (neighbor-tiles before)))
    (cond
      ((eq (first tiles) after) :north)
      ((eq (second tiles) after) :east)
      ((eq (third tiles) after) :south)
      (t :west))))
      ;; ((eq (fourth tiles) after) :west)
      ;; (t (error "not neighbor")))))

(defun calc-distance-to-food ()
  (let ((queue (make-queue)))
    (mapc #'(lambda (food) (enqueue queue (vector food 0)))
          (food *state*))
    (loop until (empty-queue? queue)
       do
         (let* ((v (dequeue queue))
                (tile (elt v 0))
                (dist (+ (elt v 1) 1)))
           (when (not (distance-to-food tile))
             (if (waterp tile)
                 (setf (distance-to-food tile) most-positive-fixnum)
                 (progn
                   (setf (distance-to-food tile) dist)
                   (mapc #'(lambda (tile)
                             (enqueue queue (vector tile (1+ dist))))
                         (neighbor-tiles tile)))))))))

;; (defun calc-distance-to-food ()
;;   (let ((queue (make-queue)))
;;     (mapc #'(lambda (enemy) (enqueue queue (vector enemy 0)))
;;           (enemy-ant *state*))
;;     (loop until (empty-queue? queue)
;;        do
;;          (let* ((v (dequeue queue))
;;                 (tile (elt v 0))
;;                 (dist (+ (elt v 1) 1)))
;;            (when (not (distance-to-enemy tile))
;;              (if (waterp tile)
;;                  (setf (distance-to-enemy tile) most-positive-fixnum)
;;                  (progn
;;                    (setf (distance-to-enemy tile) dist)
;;                    (mapc #'(lambda (tile)
;;                              (enqueue queue (vector tile (1+ dist))))
;;                          (neighbor-tiles tile)))))))))

(defun neighbor-tiles (tile)
  (let ((row (row tile))
        (col (col tile)))
    (mapcar #'(lambda (dir)
                (let* ((nlv (new-location row col dir))
                       (nlrow (elt nlv 0))
                       (nlcol (elt nlv 1)))
                  (tile-at nlrow nlcol)))
                '(:north :east :south :west))))

;; (defun nearest-tile (row col max-depth pred-target pred-walk)
;;   (let ((map (make-array 2 (rows *state*) (cols *state*) nil))
;;         (queue (make-empty-queue)))
;;     (enqueue-at-end queue (vector row col 0))
;;     (while (empty-queue? queue)
;;       (let* ((v (remove-front queue))
;;              (row (aref v 0))
;;              (col (aref v 1))
;;              (depth (aref v 2))
;;              (v (wrapped-row-col row col))
;;              (row (aref v 0))
;;              (col (aref v 1)))
;;         (unless (aref map row cols)
;;           (setq (aref map row cols) t)
;;           (cond
;;             ((> depth max-depth) nil)
;;             ((funcall pred-target (tile-at row col)) (return-form nearest-tile depth))
;;             ((not (funcall pred-walk (tile-at row col))) nil)
;;             (else
;;              (enqueue-at-end queue (vector (1+ row) col (1+ depth)))
;;              (enqueue-at-end queue (vector (1- row) col (1+ depth)))
;;              (enqueue-at-end queue (vector row (1+ col) (1+ depth)))
;;              (enqueue-at-end queue (vector row (1- col) (1+ depth)))))))))
;;   most-positive-fixnum)

;;;

;;; Main Program

;; This MAIN is used on the competition server.
(defun main (&key (log nil) (state (make-instance 'ants-bot-state))
                  (verbose nil))
  "Main game loop: parses the (initial) game state and calls DO-TURN and
  FINISH-TURN."
  (let ((*state* state)
        (*verbose* verbose))
    (cond ((and log *verbose*)
           (setf (slot-value *state* 'log-stream)
                 (open log :direction :output :if-exists :append
                           :if-does-not-exist :create)))
          (*verbose*
           (setf (slot-value *state* 'log-stream) *debug-io*)))
    (logmsg "~&=== New Match: " (current-date-time-string) " ===~%")
    (handler-bind ((sb-sys:interactive-interrupt #'user-interrupt))
      (loop while (handler-case (peek-char nil (input *state*) nil)
                    (sb-int:simple-stream-error nil))
            for end-of-game-p = (parse-game-state)
            when end-of-game-p do (loop-finish)
            do (logmsg "--- turn: " (turn *state*) " ---~%")
               (logmsg "~&[start] " (current-date-time-string) "~%")
               (do-turn)
               (finish-turn)
               (logmsg "~&[end] move took " (turn-time-used) " seconds ("
                       (turn-time-remaining) " left).~%")))))


;; This MAIN is called when you use the Makefile locally.
(defun main-for-local (&key (log "ants-bot.log") (verbose t))
  (main :log log :verbose verbose))


;; This MAIN is for the Slime REPL with bin/play-proxy-game.sh.
(defun main-for-proxybot (&key (log "ants-bot-proxied.log") (verbose t)
                          (host #-allegro #(127 0 0 1) #+allegro "localhost")
                          (port 41807))
  (let (client socket stream)
    (unwind-protect
         (handler-bind (#+sbcl (sb-bsd-sockets:socket-error
                                #'socket-error-handler)
                        (address-in-use-error #'address-in-use))
           (setf socket (socket-listen host port :reuse-address t))
           (format *debug-io* "Waiting for connection on ~A:~D...~%"
                   (host2str host)  port)
           (force-output)
           (setf client (socket-accept socket)
                 stream (socket-stream client))
           (format *debug-io* "Connected. Playing game...~%")
           (force-output)
           (main :state (make-instance 'ants-bot-state :input stream
                                       :output stream)
                 :log log :verbose verbose))
      (ignore-errors (socket-close client)
                     (socket-close socket))))
  (format *debug-io* "Game finished. Connection closed...~%"))
