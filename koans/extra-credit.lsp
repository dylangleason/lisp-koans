;; EXTRA CREDIT:
;;
;; Create a program that will play the Greed Game.
;; Rules for the game are in GREED_RULES.TXT.
;;
;; You already have a DiceSet class and score function you can use.
;; Write a player class and a Game class to complete the project.  This
;; is a free form assignment, so approach it however you desire.

;;; Constants

(defconstant +sides-per-die+ 6)
(defconstant +nums-in-a-set+ 3)
(defconstant +points-per-set-of-ones+ 1000)
(defconstant +points-per-set-multiplier+ 100)
(defconstant +points-per-one+ 100)
(defconstant +points-per-five+ 50)
(defconstant +points-per-other+ 0)

;;; Game objects

(defclass player ()
  ((name
    :initarg :name
    :reader name
    :documentation "Name of player")
   (points
    :initform 0
    :accessor points
    :documentation "Number of points assigned to player")))

(defun make-players (num-players)
  (loop for i from 1 to num-players
     collect (make-instance 'player :name (format nil "Player ~d" i))))

(defclass dice ()
  ((numbers
    :reader numbers
    :initform nil
    :documentation "A list of numbers between 1-6")))

(defmethod roll (how-many (object dice))
  (setf (slot-value object 'numbers)
        (loop repeat how-many
           collect (+ 1 (random +sides-per-die+)))))

(defclass game ()
  ((num-players
    :initarg :num-players
    :documentation "Number of players in the game")
   (dice
    :initform (make-instance 'dice)
    :reader dice
    :documentation "Dice for playing the game")
   (players
    :reader players
    :documentation "A list of game players")
   (current-player
    :initform 0
    :documentation "Index of current player in players list")))

(defmethod initialize-instance :after ((game game) &key num-players)
  (setf (slot-value game 'players)
        (make-players num-players)))

(defmethod current-player ((game game))
  "Get the current player for the game"
  (nth (slot-value game 'current-player) (players game)))

(defmethod next-player ((game game))
  "Get the next player for the game"
  (with-slots ((players players)
               (current-player current-player)) game
    (let ((index (mod (+ current-player 1) (length players))))
      (setf current-player index)
      (nth index players))))

;;; Scoring procedures

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

;; Game procedures

(defun display-current-player (game)
  "Displays the current player."
  (let ((player (current-player game)))
    (format t "The current player is: ~a~%" (name player))))

(defun display-scores (game)
  "Displays the current score for each player in a tabular format."
  (dolist (player (players game))
    (format t "~a ~4d~%" (name player) (points player))))

(defun roll-dice (game)
  "Roll the game dice for the current player and add points"
  (let ((player (current-player game))
        (result (roll 5 (dice game))))
    (format t "~a rolled ~a~%" (name player) result)))

(defun tally-points (game)
  "Tally points for the current player based on the current round's
  score."
  (print "TODO: tabulate points"))
