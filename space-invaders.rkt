;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define FONT-SIZE 32)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define TRANSPARENT-GRAY (color 128 128 128 52))
(define OVERLAY-BACKGROUND (empty-scene WIDTH HEIGHT TRANSPARENT-GRAY))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body
(define MISSILE (ellipse 5 15 "solid" "red"))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank

;; Game examples defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1, doesn't move if dir 0

(define T0 (make-tank (/ WIDTH 2) 0))                  ;center not moving
(define T1 (make-tank 50 1))                           ;going right
(define T2 (make-tank 50 -1))                          ;going left
(define T3 (make-tank (- WIDTH TANK-WIDTH/2) 1))       ;reaches right edge
(define T4 (make-tank (+ 0 TANK-WIDTH/2) -1))          ;reaches left edge

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))                                         ;not landed, moving right
(define I2 (make-invader 100 80 -2))                                          ;not landed, moving left
(define I3 (make-invader (- WIDTH INVADER-WIDTH/2) 200 10))                   ;reaches right edge, moving right
(define I4 (make-invader (+ 0 INVADER-WIDTH/2) 150 -10))                      ;reaches left edge, moving left
(define I5 (make-invader (+ (- WIDTH INVADER-WIDTH/2) 10) 80 10))             ;tries to pass right edge, moving right
(define I6 (make-invader (- (+ 0 INVADER-WIDTH/2) 2) 100 -2))                 ;tries to pass left edge, moving left
(define I7 (make-invader 150 HEIGHT -10))                                     ;exactly landed, moving left
(define I8 (make-invader 150 (+ HEIGHT 10) 10))                               ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates
  
(define M1-1 (make-missile 150 300))                                                          ;not hit I1
(define M1-2 (make-missile (invader-x I1) (+ (invader-y I1) HIT-RANGE)))                      ;exactly hit I1
(define M1-3 (make-missile (invader-x I1) (+ (invader-y I1) (/ HIT-RANGE 2))))                ;> hit I1
(define M2-1 (make-missile (invader-x I2) (+ (+ (invader-y I2) HIT-RANGE) 1)))                ;approaches I2
(define M2-2 (make-missile (+ (invader-x I2) (- HIT-RANGE 1)) (+ (invader-y I2) HIT-RANGE)))  ;> hit I2
(define M10  (make-missile (/ WIDTH 2) (+ HEIGHT TANK-HEIGHT/2)))                             ;just fired from T0
(define M11  (make-missile (/ WIDTH 2) (- 0 10)))                                             ;offscreen
(define M12  (make-missile (/ WIDTH 2) 0))                                                    ;reaches offscreen

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1-1) T1))
(define G3 (make-game (list I1 I2) (list M1-1 M1-2) T1))

;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; no tests for main function
(define (main ws)
  (big-bang ws                     ; Game
    (on-tick    play-game)         ; Game -> Game
    (to-draw    render-game)       ; Game -> Image
    (stop-when  game-over?         ; Game -> Boolean
                display-game-over) ; Game -> Image
    (on-key     handle-key)))      ; Game KeyEvent -> Game

;; Game -> Game
;; advance invaders, missiles and tank, remove any colliding invaders and missiles
(check-random (play-game (make-game empty empty T0))                                         ; initial game state, an invader is added with a 2 percent probability
              (make-game (if (< (random INVADE-RATE) 2)
                             (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) empty)
                             empty)
                         empty
                         T0))

(check-random (play-game (make-game (list I3 I2)                                             ; one invader bounces off edge, another collides with a missile,
                                    (list M1-1 M2-1)                                         ; tank is advanced to the right,
                                    T1))                                                     ; a new invader is added with a 2 percent probability
              (make-game (if (< (random INVADE-RATE) 2)
                             (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)
                                   (make-invader (- WIDTH INVADER-WIDTH/2) (invader-y I3) (- (invader-dx I3))))
                             (list (make-invader (- WIDTH INVADER-WIDTH/2) (invader-y I3) (- (invader-dx I3)))))
                         (list (make-missile (missile-x M1-1) (- (missile-y M1-1) MISSILE-SPEED)))
                         (make-tank (+ (tank-x T1) TANK-SPEED) 1)))
                  
;(define (play-game g) g) ;stub

(define (play-game g)
  (handle-collisions (make-game (next-invaders (game-invaders g))
                                (next-missiles (game-missiles g))
                                (next-tank (game-tank g)))))


;; Game -> Game
;; remove all colliding invaders and missiles from given game g
(check-expect (handle-collisions (make-game (list I1 I3)                                     ; A game with no colliding invaders and missiles 
                                            (list M1-1)
                                            T1))
              (make-game (list I1 I3)
                         (list M1-1)
                         T1))

(check-expect (handle-collisions (make-game (list I1 I3)                                     ; A game with one colliding invader and missile
                                            (list M1-1 M1-2) 
                                            T2))
              (make-game (list I3)
                         (list M1-1)
                         T2))

(check-expect (handle-collisions (make-game (list I1 I2 I3)                                  ; A game with more than one collision
                                            (list M1-1 M1-2 M2-2)
                                            T1))
              (make-game (list I3)
                         (list M1-1)
                         T1))


;(define (handle-collisions g) g) ;stub

(define (handle-collisions g)
  (make-game (remove-colliding-invaders (game-invaders g) (game-missiles g))
             (remove-colliding-missiles (game-missiles g) (game-invaders g))
             (game-tank g)))

;; ListOfInvader ListOfMissile -> ListOfInvader
;; remove any invader i that collides with a missile m in lom
(check-expect (remove-colliding-invaders empty (list M1-1)) empty)                           ; base case

(check-expect (remove-colliding-invaders (list I1 I2)                                        ; no invaders collide with a missile                                                     
                                         (list M1-1 M2-1))
              (list I1 I2))
                   
(check-expect (remove-colliding-invaders (list I1 I2)                                        ; one invader collides with a missile
                                         (list M1-2 M2-1))
              (list I2))

(check-expect (remove-colliding-invaders (list I1 I2 I3)                                     ; multiple invaders collide with multiple missiles                                
                                         (list M2-2 M1-2))
              (list I3))

;(define (remove-colliding-invaders loi lom) loi) ;stub

(define (remove-colliding-invaders loi lom)
  (cond [(empty? loi) empty]
        [else (if (invader-collides? (first loi) lom)
                  (remove-colliding-invaders (rest loi) lom)
                  (cons (first loi) (remove-colliding-invaders (rest loi) lom)))]))

;; Invader ListOfMissile -> Boolean
;; return true if given invader i collidies with any missile in lom
(check-expect (invader-collides? I1 empty) false)                                            ; base case
(check-expect (invader-collides? I1 (list M1-1 M2-1)) false)                                 ; invader doesn't collide with any missile
(check-expect (invader-collides? I1 (list M1-1 M1-2)) true)                                  ; invader collides with a missile further down lom 
               
;(define (invader-collides? i lom) false) ;stub
;<template on ListOfMissile>

(define (invader-collides? i lom)
  (cond [(empty? lom) false]
        [else (if (collides? i (first lom))
                  true
                  (invader-collides? i (rest lom)))]))

;; Invader Missile -> Boolean
;; produce true if given invader i collidies with given missile m
(check-expect (collides? I1 M1-1) false)                                                     
(check-expect (collides? I1 M1-2) true)                                     ; invader collides with a missile, with same x and within HIT-RANGE on the y
(check-expect (collides? I2 M2-1) false)
(check-expect (collides? I2 M2-2) true)                                     ; invader collides with a missile within HIT-RANGE on the x and on the y

;(define (collides? i m) false) ;stub

(define (collides? i m)
  (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))

;; ListOfMissile ListOfInvader -> ListOfMissile
;; remove any missile m in lom that collides with any invader i in loi
(check-expect (remove-colliding-missiles empty (list I1)) empty)                          ; base case
              
(check-expect (remove-colliding-missiles (list M1-1 M2-1)                                 ; no invaders collide with a missile                                                     
                                         (list I1 I2))
              (list M1-1 M2-1))
                   
(check-expect (remove-colliding-missiles (list M1-2 M2-1)                                 ; one invader collides with a missile
                                         (list I1 I2))
              (list M2-1))

(check-expect (remove-colliding-missiles (list M2-2 M1-2 M10)                             ; multiple invaders collide with multiple missiles                                
                                         (list I1 I2))
              (list M10))

;(define (remove-colliding-missiles lom loi) lom) ;stub

(define (remove-colliding-missiles lom loi)
  (cond [(empty? lom) empty]
        [else (if (missile-collides? (first lom) loi)
                  (remove-colliding-missiles (rest lom) loi)
                  (cons (first lom) (remove-colliding-missiles (rest lom) loi)))]))

;; Missile ListOfInvader -> Boolean
;; produce true if missile m collides with any invader i in loi
(check-expect (missile-collides? M1-1 empty) false)                                       ; base case
(check-expect (missile-collides? M1-1 (list I1 I2 I3)) false)                             ; missile doesn't collide with any invader
(check-expect (missile-collides? M1-2 (list I3 I2 I1)) true)                              ; missile collides with an invader further down loi  

;(define (missile-collides? m loi) false) ;stub

(define (missile-collides? m loi) 
  (cond [(empty? loi) false]
        [else (if (collides? (first loi) m)
                  true
                  (missile-collides? m (rest loi)))]))

;; ListOfInvader -> ListOfInvader
;; increase each (invader-x i) by (invader-dx i) and (invader-y i) by INVADER-Y-SPEED
;; inverse (invader-dx i) if invader tries to pass an edge; add a new invader with 2% probability 
(check-random (next-invaders (list I1 I2 I3))
              (if (< (random INVADE-RATE) 2)
                  (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)
                        (make-invader (+ (invader-x I1) (invader-dx I1)) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1))
                        (make-invader (+ (invader-x I2) (invader-dx I2)) (+ (invader-y I2) INVADER-Y-SPEED) (invader-dx I2))
                        (make-invader (- WIDTH INVADER-WIDTH/2) (invader-y I3) (- (invader-dx I3))))
                  (list (make-invader (+ (invader-x I1) (invader-dx I1)) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1))
                        (make-invader (+ (invader-x I2) (invader-dx I2)) (+ (invader-y I2) INVADER-Y-SPEED) (invader-dx I2))
                        (make-invader (- WIDTH INVADER-WIDTH/2) (invader-y I3) (- (invader-dx I3))))))
                             
;(define (next-invaders loi) loi) ;stub

(define (next-invaders loi)
  (add-invader (advance-invaders loi)))

;; ListOfInvader -> ListOfInvader
;; increase each (invader-x i) by (invader-dx i) and (invader-y i) by INVADER-Y-SPEED
;; inverse (invader-dx i) if invader tries to pass an edge
(check-expect (advance-invaders empty) empty)

(check-random (advance-invaders (list I1 I2 I3))        
              (list (make-invader (+ (invader-x I1) (invader-dx I1)) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1))
                    (make-invader (+ (invader-x I2) (invader-dx I2)) (+ (invader-y I2) INVADER-Y-SPEED) (invader-dx I2))
                    (make-invader (- WIDTH INVADER-WIDTH/2) (invader-y I3) (- (invader-dx I3)))))
                  

;(define (advance-invaders loi) loi) ;stub

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader (first loi))
               (advance-invaders (rest loi)))]))

;; Invader -> Invader
;; increase (invader-x i) by (invader-dx i) and (invader-y) i by INVADER-Y-SPEED; bounce off edges
(check-expect (advance-invader I1)                                                                   ; increase invader-x and invader-y
              (make-invader (+ (invader-x I1) (invader-dx I1))
                            (+ (invader-y I1) INVADER-Y-SPEED)
                            (invader-dx I1)))

(check-expect (advance-invader I3)                                                                   ; bounce off right edge
              (make-invader (- WIDTH INVADER-WIDTH/2)
                            (invader-y I3)
                            (- (invader-dx I3))))

(check-expect (advance-invader I4)                                                                   ; bounce off left edge
              (make-invader (+ 0 INVADER-WIDTH/2)
                            (invader-y I4)
                            (- (invader-dx I4))))

;(define (advance-invader i) i) ;stub

(define (advance-invader i)
  (cond [(> (+ (invader-x i) (invader-dx i)) (- WIDTH INVADER-WIDTH/2))
         (make-invader (- WIDTH INVADER-WIDTH/2) (invader-y i) (- (invader-dx i)))]
        [(< (+ (invader-x i) (invader-dx i)) (+ 0 INVADER-WIDTH/2))
         (make-invader (+ 0 INVADER-WIDTH/2) (invader-y i) (- (invader-dx i)))]
        [else (make-invader (+ (invader-x i) (invader-dx i))
                            (+ (invader-y i) INVADER-Y-SPEED)
                            (invader-dx i))]))

;; ListOfInvader -> ListOfInvader
;; add a new invader with a 2% probability
(check-random (add-invader empty)
              (if (< (random INVADE-RATE) 2)
                  (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) empty)
                  empty))
(check-random (add-invader (list I1))
              (if (< (random INVADE-RATE) 2)
                  (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED)
                        (cons I1 empty))
                  (list I1)))

;(define (add-invader loi) loi) ;stub

(define (add-invader loi)
  (if (< (random INVADE-RATE) 2)
      (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)
      loi))

;; ListOfMissile -> ListOfMissile
;; decrease each (missile-y m) by MISSILE-SPEED; remove all off screen missiles  
(check-expect (next-missiles (list M10 M12))
              (list (make-missile (missile-x M10) (- (missile-y M10) MISSILE-SPEED))))

;(define (next-missiles lom) lom) ;stub

(define (next-missiles lom)
  (remove-off-screen (advance-missiles lom)))

;; ListOfMissile -> ListOfMissile
;; decrease each (missile-y m) by MISSILE-SPEED
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list M10 M11 M12))
              (list (make-missile (missile-x M10) (- (missile-y M10) MISSILE-SPEED))
                    (make-missile (missile-x M11) (- (missile-y M11) MISSILE-SPEED))
                    (make-missile (missile-x M12) (- (missile-y M12) MISSILE-SPEED))))
                    
;(define (advance-missiles lom) lom) ;stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (advance-missile (first lom))
                    (advance-missiles (rest lom)))]))

;; Missile -> Missile
;; decrease (missile-y m) by MISSILE-SPEED
(check-expect (advance-missile M10)
              (make-missile (missile-x M10) (- (missile-y M10) MISSILE-SPEED)))
(check-expect (advance-missile M12)
              (make-missile (missile-x M12) (- (missile-y M12) MISSILE-SPEED)))

;(define (advance-missile m) m) ;stub

(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissile -> ListOfMissile
;; remove missiles with (missile-y) less than or equal to 0
(check-expect (remove-off-screen empty) empty)
(check-expect (remove-off-screen (list M10 M11 M12))                                       ; M11 is offscreen, M12 just reached screen upper edge              
              (list M10 M12))

;(define (remove-off-screen lom) lom) ;stub

(define (remove-off-screen lom)
  (cond [(empty? lom) empty]
        [else
         (if (off-screen? (first lom))
             (remove-off-screen (rest lom))
             (cons (first lom) (remove-off-screen (rest lom))))]))

;; Missile -> Boolean
;; return true if (missile-y m) is less than 0
(check-expect (off-screen? M10) false)
(check-expect (off-screen? M11) true)
(check-expect (off-screen? M12) false)

;(define (off-screen? m) false) ;stub

(define (off-screen? m)
  (< (missile-y m) 0))

;; Tank -> Tank
;; increase (tank-x t) by TANK-SPEED; change (tank-dir t) if tries to pass an edge
(check-expect (next-tank T0)                                                              ; tank is not moving
              T0)
(check-expect (next-tank T1)                                                              ; tank is moving left -> right
              (make-tank (+ (tank-x T1) TANK-SPEED) (tank-dir T1)))
(check-expect (next-tank T2)                                                              ; tank is moving right -> left
              (make-tank (- (tank-x T2) TANK-SPEED) (tank-dir T2)))
(check-expect (next-tank T3)                                                              ; tank is moving left -> right and tries to pass right edge
              (make-tank (- WIDTH TANK-WIDTH/2) -1))
(check-expect (next-tank T4)                                                              ; tank is moving right -> left and tries to pass left edge
              (make-tank (+ 0 TANK-WIDTH/2) 1))

;(define (next-tank t) t) ;stub

(define (next-tank t)
  (cond [(= (tank-dir t) 0) t]
        [(= (tank-dir t) 1)
         (if (> (+ (tank-x t) TANK-SPEED) (- WIDTH TANK-WIDTH/2))
             (make-tank (- WIDTH TANK-WIDTH/2) -1)
             (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t)))]
        [else
         (if (< (- (tank-x t) TANK-SPEED) (+ 0 TANK-WIDTH/2))
             (make-tank (+ 0 TANK-WIDTH/2) 1)
             (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)))]))

;; Game -> Image
;; render game elements on BACKGROUND
(check-expect (render-game (make-game empty empty T0))                                    ; rendering of initial game state
              (place-image TANK
                           (tank-x T0) (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))

(check-expect (render-game (make-game (list I1 I3)                                        ; rendering of a game with two invaders, one missile, and a moving tank    
                                      (list M1-1)
                                      T2))
              (place-image INVADER
                           (invader-x I1) (invader-y I1)
                           (place-image INVADER
                                        (invader-x I3) (invader-y I3) 
                                        (place-image MISSILE
                                                     (missile-x M1-1) (missile-y M1-1)
                                                     (place-image TANK
                                                                  (tank-x T2) (- HEIGHT TANK-HEIGHT/2)
                                                                  BACKGROUND)))))
                                                          
                                      
                                     
;(define (render-game g) BACKGROUND) ;stub

(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g)
                                                 BACKGROUND))))

;; ListOfInvader Image -> Image
;; render each invader at its (invader-x), (invader-y) on img
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list I1 I7)
                               BACKGROUND)
              (place-image INVADER
                           (invader-x I1) (invader-y I1)
                           (place-image INVADER
                                        (invader-x I7) (invader-y I7)
                                        BACKGROUND)))

;(define (render-invaders loi img) img) ;stub

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else (render-invader (first loi)
                              (render-invaders (rest loi) img))]))

;; Invader Image -> Image
;; render invader i at (invader-x i), (invader-y i) on img
(check-expect (render-invader I1 BACKGROUND)
              (place-image INVADER
                           (invader-x I1) (invader-y I1)
                           BACKGROUND))

;(define (render-invader i img) img) ;stub

(define (render-invader i img)
  (place-image INVADER
               (invader-x i) (invader-y i)
               img))

;; ListOfMissile Image -> Image
;; render each missile at its (missile-x), (missile-y) on img
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list M1-1 M1-2)
                               BACKGROUND)
              (place-image MISSILE
                           (missile-x M1-1) (missile-y M1-1)
                           (place-image MISSILE
                                        (missile-x M1-2) (missile-y M1-2)
                                        BACKGROUND)))

;(define (render-missiles lom img) img) ;stub

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else (render-missile (first lom)
                              (render-missiles (rest lom) img))]))

;; Missile Image -> Image
;; render missile m at (missile-x m), (missile-y m) on img
(check-expect (render-missile M1-1 BACKGROUND)
              (place-image MISSILE
                           (missile-x M1-1) (missile-y M1-1)
                           BACKGROUND))

;(define (render-missile m img) img) ;stub

(define (render-missile m img)
  (place-image MISSILE
               (missile-x m) (missile-y m)
               img))

;; Tank Image -> Image
;; render tank t at (tank-x t) and bottom of img
(check-expect (render-tank T0 BACKGROUND)
              (place-image TANK
                           (tank-x T0) (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))

(check-expect (render-tank T4 BACKGROUND)
              (place-image TANK
                           (tank-x T4) (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))

;(define (render-tank t img) img) ;stub

(define (render-tank t img)
  (place-image TANK
               (tank-x t) (- HEIGHT TANK-HEIGHT/2)
               img))


;; Game -> Boolean
;; return true if game g is over; any invader in (game-invaders g) landed, i.e. reached bottom of screen
(check-expect (game-over? (make-game
                           empty
                           empty
                           T0))
              false)

(check-expect (game-over? (make-game
                           (list I1 I2 I3)
                           empty
                           T2))
              false)

(check-expect (game-over? (make-game
                           (list I1 I2 I7)
                           empty
                           T3))
              true)

;(define (game-over? g) false) ;stub

(define (game-over? g)
  (cond [(empty? (game-invaders g)) false]
        [else (or (invader-landed? (first (game-invaders g)))
                  (game-over? (make-game (rest (game-invaders g))
                                         (game-missiles g)
                                         (game-tank g))))]))


;; Invader -> Boolean
;; return true if given invader i reached bottom of screen
(check-expect (invader-landed? I1) false)
(check-expect (invader-landed? I7) true)

;(define (invader-landed? i) false) ;stub

(define (invader-landed? i)
  (>= (invader-y i) HEIGHT))

;; Game -> Image
;; render an image of "GAME OVER" text overlayed over the rendering of the current game
(check-expect (display-game-over (make-game (list I1 I3 I7)                                  ; a rendering of a finished game (an invader landed)
                                            (list M1-1)
                                            T1))
              (overlay
               (text "GAME OVER!" FONT-SIZE "red")
               OVERLAY-BACKGROUND
               (render-game (make-game (list I1 I3 I7)
                                       (list M1-1)
                                       T1))))

;(define (display-game-over g) empty-image) ;stub

(define (display-game-over g)
  (overlay
   (text "GAME OVER!" FONT-SIZE "red")
   OVERLAY-BACKGROUND
   (render-game g)))

;; Game KeyEvent -> Game
;; move TANK left or right if left or right arrows are pressed respectively;
;; fire a new missile if the space bar is pressed
(check-expect (handle-key (make-game (list I1 I2)                                            ; right arrow is pressed, change tank direction to left -> right    
                                     empty
                                     T0)
                          "right")
              (make-game (list I1 I2)   
                         empty
                         (make-tank (tank-x T0) 1)))

(check-expect (handle-key (make-game (list I1 I2)                                            ; left arrow is pressed, change tank direction to right -> left
                                     empty
                                     T0)
                          "left")
              (make-game (list I1 I2)
                         empty
                         (make-tank (tank-x T0) -1)))

(check-expect (handle-key (make-game (list I1 I2)                                            ; spacebar is pressed, cons a new missile at (tank-x, tank-y)   
                                     empty
                                     T1)
                          " ")
              (make-game (list I1 I2)
                         (cons (make-missile (tank-x T1) (- HEIGHT TANK-HEIGHT/2)) empty)
                         T1))

(check-expect (handle-key (make-game (list I1 I2 I3)                                         ; nothing on other key events   
                                     (list M1-1)
                                     T2)
                          "x")
              (make-game (list I1 I2 I3) 
                         (list M1-1)
                         T2))

;(define (handle-key g ke) g) ;stub

(define (handle-key g ke)
  (cond [(key=? ke "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) 1))]
        [(key=? ke "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) -1))] 
        [(key=? ke " ")
         (make-game (game-invaders g)
                    (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2))
                          (game-missiles g))
                    (game-tank g))]
        [else g]))

