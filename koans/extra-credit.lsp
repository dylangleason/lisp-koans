;; EXTRA CREDIT:
;;
;; Create a program that will play the Greed Game.
;; Rules for the game are in GREED_RULES.TXT.
;;
;; You already have a DiceSet class and score function you can use.
;; Write a player class and a Game class to complete the project.  This
;; is a free form assignment, so approach it however you desire.

;; Constants

(defconstant +sides-per-die+ 6)
(defconstant +nums-in-a-set+ 3)
(defconstant +points-per-set-of-ones+ 1000)
(defconstant +points-per-set-multiplier+ 100)
(defconstant +points-per-one+ 100)
(defconstant +points-per-five+ 50)
(defconstant +points-per-other+ 0)

;; Game objects

(defclass player ()
  ((name :initarg :name :reader player-name)
   (points :initform 0 :accessor player-points)))

(defun make-players (num-players)
  (loop for i from 1 to num-players
     collect (make-instance 'player :name (format nil "Player ~d" i))))

(defclass dice ()
  ((values :reader dice-values :initform nil)))

(defmethod roll (how-many (object dice))
  (setf (slot-value object 'values)
        (loop repeat how-many
           collect (+ 1 (random +sides-per-die+)))))

(defclass game ()
  ((num-players :initarg :num-players)
   (dice :initform (make-instance 'dice) :reader game-dice)
   (players :accessor game-players)
   (current-player :accessor game-current-player)))

(defmethod initialize-instance :after ((g game) &key num-players)
  (with-accessors ((players game-players)) g
    (setf players (make-players num-players))
    (setf (game-current-player g) (first players))))

(defmethod next-player ((g game))
  "Get the next player turn, round-robin style"
  ;; (with-accessors ((players game-players)
  ;;                  (current-player game-current-player)) g
  ;;   (setf current-player
  ;;         (nth (mod (+ current-player 1) (length players))
  ;;              players)))
  )

;; Scoring routines

(defun count-sides (dice)
  (let ((nums-hash (make-hash-table :test #'eql)))
    (dolist (num dice)
      (let ((count (gethash num nums-hash 0)))
        (setf (gethash num nums-hash) (incf count))))
    nums-hash))

(defun score-num (num count points-per-num)
  (let ((calc-points (lambda (new-count) (* new-count points-per-num)))
        (points-per-set (if (= num 1)
                            +points-per-set-of-ones+
                            (* num +points-per-set-multiplier+))))
    (cond ((= count +nums-in-a-set+) points-per-set)
          ((> count +nums-in-a-set+)
           (+ points-per-set (funcall calc-points (- count 3))))
          (t (funcall calc-points count)))))

(defun score (dice)
  (let ((total 0) (points 0) (counts (count-sides dice)))
    (loop for num being the hash-keys of counts
       using (hash-value count)
       do (cond ((= num 1) (setf points +points-per-one+))
                ((= num 5) (setf points +points-per-five+))
                (t (setf points +points-per-other+)))
         (setf total (+ total (score-num num count points))))
    total))

;; Game routines

(defun display-current-player (game)
  "Displays the current player."
  (let ((player (game-current-player game)))
    (format t "The current player is: ~a~%" (player-name player))))

(defun display-score (game)
  "Displays the current score for each player in a tabular format."
  (dolist (p (game-players game))
    (format t "~a ~4d~%" (player-name p) (player-points p))))

(defun roll-dice (game)
  "Roll the game dice for the current player and add points"
  )

(defun tally-points (game)
  "Tally points for the current player based on the current round's
  score."
  (print "TODO: tabulate points"))
