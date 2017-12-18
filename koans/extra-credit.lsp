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
(defconstant +minimum-initial-points+ 300)
(defconstant +points-to-final-round+ 3000)

;;; Scoring procedures & helper functions

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

(defun make-players (num-players)
  (loop for i from 1 to num-players
     collect (make-instance 'player :name (format nil "Player ~d" i))))

(defun roll-dice (how-many)
  (loop repeat how-many
     collect (+ 1 (random +sides-per-die+))))

;;; Player class holds player data for the GREED game.

(defclass player ()
  ((name
    :initarg :name
    :reader name
    :documentation "Name of player")
   (points
    :initform 0
    :accessor points
    :documentation "Number of points assigned to player")))

;;; Game keeps track of the GREED game state and provides operations
;;; for playing GREED.

(defclass game ()
  ((num-players
    :initarg :num-players)
   (current-player
    :initform 0)
   (final-round-p
    :initform nil)
   (final-round-player
    :initform nil)
   (players
    :reader players
    :documentation "A list of game players")))

(defmethod initialize-instance :after ((game game) &key num-players)
  (setf (slot-value game 'players)
        (make-players num-players)))

(defmethod current-player ((game game))
  "Get the current player for the game"
  (nth (slot-value game 'current-player) (players game)))

(defmethod set-next-player ((game game))
  "Get the next player for the game"
  (with-slots ((players players)
               (current-player current-player)) game
    (let ((index (mod (+ current-player 1) (length players))))
      (setf current-player index)
      (nth index players))))

(defmethod display-current-player ((game game))
  "Displays the current player."
  (format t "The current player is: ~a~%" (name (current-player game))))

(defmethod display-scores ((game game))
  "Displays the current score for each player in a tabular format."
  (dolist (player (players game))
    (format t "~a ~5d~%" (name player) (points player))))

(defmethod play ((game game))
  "Play a game round, which roll the dice for the player and calculate
points. The current player may optionally end this round, assuming
it's their turn."
  (let ((player (current-player game)))
    (with-slots (final-round-p final-round-player) game
      (if (and final-round-p
               (eq (current-player game) final-round-player))
          (format t "The game has ended.")
          (let* ((player-name (name player))
                 (dice-roll (roll-dice 5))
                 (new-points (score dice-roll)))
            (format t "~a rolled: ~{~d~^, ~}.~%" player-name dice-roll)
            (with-accessors ((points points)) player
              (unless (and (= points 0)
                           (< new-points +minimum-initial-points+))
                (format t "~a got ~d points.~%" player-name new-points)
                (setf points (+ points new-points))
                (when (and (not final-round-p)
                           (>= points +points-to-final-round+))
                  (setf final-round-p t)
                  (setf final-round-player player))))
            (set-next-player game))))))

(defmacro defgame (name &body body)
  `(defvar ,name (make-instance 'game :num-players ,@body)))
