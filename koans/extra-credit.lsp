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

;;; Player class holds player data for the GREED game.

(defclass player ()
  ((name
    :initarg :name
    :reader name
    :documentation "Name of player")
   (points
    :initform 0
    :accessor points
    :documentation "Number of points assigned to player")
   (turn-points
    :initform 0
    :accessor turn-points
    :documentation "Number of points for the current turn")))

;;; Game keeps track of the GREED game state and provides operations
;;; for playing GREED.

(defclass game ()
  ((num-players
    :initarg :num-players)
   (current-player
    :initform 0)
   (final-round-player
    :initform nil)
   (reroll-dice
    :initform 0)
   (players
    :reader players
    :documentation "A list of game players")))

;;; Scoring procedures & helper functions

(defun score (dice)
  "Score the dice roll and return both the accumulated score for the
roll and the number of non-scoring dice, if any."
  (flet ((counts-hash ()
             (let ((nums-hash (make-hash-table :test #'eql)))
               (dolist (num dice)
                 (let ((count (gethash num nums-hash 0)))
                   (setf (gethash num nums-hash) (incf count))))
               nums-hash))
         (score-num (num count points-per-num)
           (let ((calc-points (lambda (new-count) (* new-count points-per-num)))
                 (points-per-set (if (= num 1)
                                     +points-per-set-of-ones+
                                     (* num +points-per-set-multiplier+))))
             (cond ((= count +nums-in-a-set+) points-per-set)
                   ((> count +nums-in-a-set+)
                    (+ points-per-set (funcall calc-points (- count 3))))
                   (t (funcall calc-points count))))))
    (let* ((total 0)
           (counts (counts-hash))
           (non-scoring
            (loop for num being the hash-keys of counts
               using (hash-value count)
               do (let ((points 0))
                    (cond ((= num 1) (setf points +points-per-one+))
                          ((= num 5) (setf points +points-per-five+))
                          (t (setf points +points-per-other+)))
                    (setf total (+ total (score-num num count points))))
               when (and (not (or (= num 5) (= num 1))) (< count 3))
               sum count)))
      (values total non-scoring))))

(defun make-players (num-players)
  "Make n number of players and return a list."
  (loop for i from 1 to num-players
     collect (make-instance 'player :name (format nil "Player ~d" i))))

(defun roll-dice (how-many)
  "Roll n number of dice and return the results in a list."
  (loop repeat how-many
     collect (+ 1 (random +sides-per-die+))))

(defun get-high-scorer (players)
  "Given a list of players, return the highest scoring player"
  (labels ((hs (players curr-player)
             (cond ((null players) curr-player)
                   ((> (points (car players)) (points curr-player))
                    (hs (cdr players) (car players)))
                   (t (hs (cdr players) curr-player)))))
    (hs (cdr players) (car players))))

;;; Player methods

(defmethod add-turn-points ((player player) new-points)
  "Add points to the player's score for the current turn, as long as
they have reached the minimum to get in the game. If the player gets
zero points for the current roll, they lose all points for the turn"
  (let ((new-turn-p t))
    (with-slots (name points turn-points) player
      (if (> new-points 0)
          (progn
            (format t "~a got ~d points.~%" name new-points)
            (setf turn-points (+ turn-points new-points))
            (setf new-turn-p nil))
          (progn
            (when (> turn-points 0)
              (format t "~a lost all points for this turn!~%" name))
            (setf turn-points 0))))
    new-turn-p))

;;; Game Methods

(defmethod initialize-instance :after ((game game) &key num-players)
  (setf (slot-value game 'players)
        (make-players num-players)))

(defmethod current-player ((game game))
  "Get the current player for the game"
  (nth (slot-value game 'current-player) (players game)))

(defmethod display-current-player ((game game))
  "Displays the current player."
  (format t "The current player is: ~a~%" (name (current-player game))))

(defmethod display-scores ((game game))
  "Displays the current score for each player in a tabular format."
  (dolist (player (players game))
    (format t "~a ~5d~%" (name player) (points player))))

(defmethod next-turn ((game game))
  "Calculate the accumulated points for this player's turn and advance
to the next player's turn. Additionally, determine whether the current
player has started the final round before advancing to the next turn."
  (let ((player (current-player game)))
    (with-slots (current-player final-round-player reroll-dice) game
      (when (and (null final-round-player)
                 (>= (points player) +points-to-final-round+))
        (setf final-round-player player))
      (with-accessors ((points points)
                       (turn-points turn-points)) player
        (unless (and (= points 0)
                     (< turn-points +minimum-initial-points+))
          (setf points (+ points turn-points)))
        (setf turn-points 0)
        (setf reroll-dice 0)
        (setf current-player (mod (+ current-player 1) (length (players game))))))
    (format t "It is now ~a's turn.~%" (name (current-player game)))))

(defmethod play ((game game))
  "Play a game round, which rolls the dice for the player and
calculate points. The current player may optionally end this round,
assuming it's their turn."
  (let ((player (current-player game)))
    (with-slots (final-round-player reroll-dice) game
      (if (eq player final-round-player)
          (format t "~a is the winner!" (name (get-high-scorer (players game))))
          (let ((dice-roll (roll-dice (if (> reroll-dice 0) reroll-dice 5)))
                (player-name (name player)))
            (format t "~a rolled: ~{~d~^, ~}.~%" player-name dice-roll)
            (multiple-value-bind (new-points remaining-dice) (score dice-roll)
              (if (add-turn-points player new-points)
                  (progn
                    (format t "~a's turn has ended.~%" player-name)
                    (next-turn game))
                  (progn
                    (format t "It is still ~a's turn.~%" player-name)
                    (setf reroll-dice remaining-dice)))))))
    nil))

(defmacro defgame (name &body body)
  `(defvar ,name (make-instance 'game :num-players ,@body)))
