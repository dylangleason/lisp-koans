;; EXTRA CREDIT:
;;
;; Create a program that will play the Greed Game.
;; Rules for the game are in GREED_RULES.TXT.
;;
;; You already have a DiceSet class and score function you can use.
;; Write a player class and a Game class to complete the project.  This
;; is a free form assignment, so approach it however you desire.

;;; Constants

(defconstant +sides-per-die+ 6
  "The number of sides per die")

(defconstant +nums-in-a-set+ 3
  "The number of dice rolled of the same number, which constitutes a
  set")

(defconstant +points-per-set-of-ones+ 1000
  "The number of points allocated for three rolled 1's")

(defconstant +points-per-set-multiplier+ 100
  "Scaling factor for a number that constitutes a set")

(defconstant +points-per-one+ 100
  "The number of points, not part of a set, given for a rolled 1")

(defconstant +points-per-five+ 50
  "The number of points, not part of a set, given for a rolled 5")

(defconstant +points-per-other+ 0
  "The number of points, not in a set, given for a any other side")

(defconstant +minimum-initial-points+ 300
  "The minimum number of points to get into a game")

(defconstant +points-to-final-round+ 3000
  "The number of points to reach before the game enters it's final
  round.")

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
   (players ; TODO: use an array instead of a list
    :reader players
    :documentation "A list of game players")))

(define-condition invalid-players-arg-error (error)
  ((message
    :initarg :message
    :initform "must specify number of players as an integer"
    :reader message)
   (value
    :initarg :value
    :initform nil
    :reader value)))

(defmacro defgame (name &body body)
  (let ((num-players (gensym)))
    `(let ((,num-players ,@body))
       (when (not (typep ,num-players 'integer))
         (error 'invalid-players-arg-error :value ,num-players))
       (defvar ,name (make-instance 'game :num-players ,num-players)))))

;;; Scoring procedures & helper functions

(defun make-counts (dice)
  (let ((nums-hash (make-hash-table :test #'eql)))
    (dolist (num dice)
      (let ((count (gethash num nums-hash 0)))
        (setf (gethash num nums-hash) (1+ count))))
    nums-hash))

(defun score-num (num count)
  (flet ((points-per-set ()
           (if (= num 1)
               +points-per-set-of-ones+
               (* num +points-per-set-multiplier+)))
         (calc-points (new-count)
           (* new-count
              (case num
                (1 +points-per-one+)
                (5 +points-per-five+)
                (otherwise +points-per-other+)))))
    (cond ((= count +nums-in-a-set+) (points-per-set))
          ((> count +nums-in-a-set+)
           (+ (points-per-set) (calc-points (- count 3))))
          (t (calc-points count)))))

(defun score (dice)
  "Score the dice roll and return both the accumulated score for the
roll and the number of non-scoring dice, if any."
  (let ((counts (make-counts dice)))
    (loop for num being the hash-keys of counts
       using (hash-value count)
       summing (score-num num count) into total-points
       when (and (not (or (= num 5) (= num 1))) (< count 3))
       summing count into non-scoring-count
       finally (return (values total-points non-scoring-count)))))

(defun make-players (num-players)
  "Make n number of players and return a list."
  (loop for i from 1 to num-players
     collect (make-instance 'player :name (format nil "Player ~d" i))))

(defun roll-dice (how-many)
  "Roll n number of dice and return the results in a list."
  (loop repeat how-many
     collect (1+ (random +sides-per-die+))))

(defun get-high-scorer (players)
  "Given a list of players, return the highest scoring player"
  (labels ((hs (players curr-player)
             (cond ((null players) curr-player)
                   ((> (points (first players)) (points curr-player))
                    (hs (rest players) (first players)))
                   (t (hs (rest players) curr-player)))))
    (hs (rest players) (first players))))


;; This gets the highest score, but not the player associated with
;; that score...
;; (defun get-high-scorer (players)
;;   (reduce #'max players :key #'points))

;;; Player methods

(defmethod add-turn-points ((player player) new-points)
  "Add points to the player's score for the current turn, as long as
they have reached the minimum to get in the game. If the player gets
zero points for the current roll, they lose all points for the turn"
  (with-slots (name points turn-points) player
    (cond ((> new-points 0)
           (format t "~a got ~d points.~%" name new-points)
           (incf turn-points new-points)
           nil)
          (t (when (> turn-points 0)
               (format t "~a lost all points for this turn!~%" name))
             (setf turn-points 0)
             t))))

;;; Game Methods

(defmethod initialize-instance :after ((game game) &key num-players)
  (setf (slot-value game 'players)
        (make-players num-players)))

(defmethod current-player ((game game))
  "Get the current player for the game"
  ;; TODO: refactor so this uses an array, not a list
  (nth (slot-value game 'current-player) (players game)))

(defmethod display-current-player ((game game))
  "Displays the current player."
  (format t "The current player is: ~a~%" (name (current-player game))))

(defmethod display-scores ((game game))
  "Displays the current score for each player in a tabular format."
  (dolist (player (players game))
    (format t "~a ~5d~%" (name player) (points player))))

(defmethod display-winner ((game game))
  (with-slots (final-round-player) game
    (when (eq (curent-player game) final-round-player)
      (format t "~a is the winner!" (name (get-high-scorer (players game)))))))

(defmethod next-turn ((game game))
  "Calculate the accumulated points for this player's turn and advance
to the next player's turn. Additionally, determine whether the current
player has started the final round before advancing to the next turn."
  (let ((player (current-player game)))
    (with-slots (current-player final-round-player reroll-dice) game
      (with-accessors ((points points)
                       (turn-points turn-points)) player
        (unless (and (= points 0)
                     (< turn-points +minimum-initial-points+))
          (incf points turn-points))
        (setf turn-points 0)
        (setf reroll-dice 0)
        (setf current-player (mod (1+ current-player) (length (players game)))))
      (when (and (null final-round-player)
                 (>= (points player) +points-to-final-round+))
        (format t "FINAL ROUND!~%")
        (setf final-round-player player))
      (if (eql player final-round-player)
          (display-winner game)
          (format t "It is now ~a's turn.~%" (name (current-player game)))))))

(defmethod play ((game game))
  "Play a game round, which rolls the dice for the player and
calculate points. The current player may optionally end this round,
assuming it's their turn."
  (let ((player (current-player game)))
    (with-slots (final-round-player reroll-dice) game
      (if (eql player final-round-player)
          (display-winner game)
          (let ((player-name (name player))
                (dice-roll (roll-dice
                            (if (> reroll-dice 0) reroll-dice 5))))
            (format t "~a rolled: ~{~d~^, ~}.~%" player-name dice-roll)
            (multiple-value-bind (new-points remaining-dice) (score dice-roll)
              (cond ((add-turn-points player new-points)
                     (format t "~a's turn has ended.~%" player-name)
                     (next-turn game))
                    (t (setf reroll-dice remaining-dice)
                       (format t "It is still ~a's turn.~%" player-name)))))))))
