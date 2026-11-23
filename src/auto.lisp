;;;; auto.lisp -- a plain computer player for the defenders.
;;;;
;;;; Used for autoplay in the simulator and for balance runs on the host.
;;;; It plays the strategy that won most often in balance runs: kill the main
;;;; cannon first, since a stopped Behemoth with its cannon still in range of
;;;; the bunker is not beaten; then put every shot into the drive; once the
;;;; drive is wrecked, the weapons.  A defender moves up only when it can
;;;; fire that turn, rather than walking into the Behemoth's guns.

(in-package #:behemoth)

(defun threat (sys)
  (* (system-property (sys-kind sys) :attack) (system-property (sys-kind sys) :damage)))

(defun choose-target (game unit)
  (let* ((b (game-behemoth game))
         (weapons (live-systems b))
         (cannon (find :cannon weapons :key #'sys-kind)))
    (cond (cannon (sys-id cannon))
          ((plusp (behemoth-drive b)) :drive)
          (weapons
           (sys-id (first (sort (copy-list weapons) #'>
                                :key (lambda (s)
                                       (* (threat s)
                                          (target-chance game unit (sys-id s)))))))))))

(defun best-firing-hex (game unit)
  "Where UNIT should move: in range of the Behemoth, as far from it as its
range allows.  NIL if no hex it can reach this turn is in range."
  (let* ((there (behemoth-hex (game-behemoth game)))
         (range (unit-range unit))
         (hexes (remove-if-not (lambda (h) (<= (hex-distance h there) range))
                               (cons (unit-hex unit) (reachable-hexes game unit)))))
    (first (sort hexes #'> :key (lambda (h) (hex-distance h there))))))

(defun auto-step (game)
  "Make one defender action.  Returns its events, or NIL when every
defender is done for the turn."
  (when (eq (game-phase game) :player)
    (dolist (unit (game-units game))
      (when (and (combat-unit-p unit) (not (unit-fired unit)))
        (cond ((in-range-p game unit)
               (let ((target (choose-target game unit)))
                 (when target
                   (return (fire-at game (unit-id unit) target)))))
              ((not (unit-moved unit))
               (let ((hex (best-firing-hex game unit)))
                 (setf (unit-moved unit) t)
                 (when (and hex (not (equal hex (unit-hex unit))))
                   (return (list (move-unit game (unit-id unit) hex)))))))))))

(defun auto-play-turn (game)
  "Play the defenders' whole turn and end it.  Returns all its events."
  (let ((events '()))
    (loop for step = (auto-step game)
          while step do (setf events (append events step)))
    (append events (end-turn game))))
