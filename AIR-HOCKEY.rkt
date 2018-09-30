#lang racket


(require 2htdp/image)
(require 2htdp/universe)

;define constants here (e puck-radius stiker-radius left right)

(define e 0.8)
(define puck-radius 20)
(define striker-radius 30)
(define left 300)
(define right 700)
(define top 50)
(define bottom 650)
(define vmax (* 100 (sqrt 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; AI Part



(define (distance x1 y1 x2 y2)                                  
  (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))))

(define (line-converter x y slope)                             
    (cond [(equal? slope "infinite") (cons slope x)]
          [else (cons  slope (- y (* slope x)))]))

(define (line-converter-2p x1 y1 x2 y2)                           
  (let* ([slope (if (not (equal? x1 x2)) (/ (- y2 y1) (- x2 x1)) "infinite")])
    (if (equal? slope "infinite") (cons slope x1) (cons slope (- y1 (* slope x1))))))

(define (x-val  line y) (if (equal? (car line) "infinite") (cdr line)   
                            (if (equal? (car line) 0) 100000000 (/ (- y (cdr line)) (car line)))))

(define (y-val line x) (if (equal? (car line) "infinite") 10000000 (+ (* (car line) x) (cdr line))))  

(define (after-reflect-v vline line)
  (line-converter vline (y-val line vline) (* -1 (car line))))

(define (after-reflect-h hline line)
  (line-converter (x-val line hline) hline (* -1 (car line))))

(define (will-goal? line vx vy)   ;(Checks whether the puck will hit the goal or not)
  (cond  [(equal? (car line) "infinite") (if (and (> (cdr line) 400) (< (cdr line) 600)) #t #f)]
         [(equal? (car line) 0) #f]
     [else (let* (  
         [x-cut (if (> vy 0) (x-val line (* -1 top)) (x-val line (* -1 bottom)))]
         [y-cut (if (> vx 0) (* -1 (y-val line right)) (* -1 (y-val line left)))])

    (cond 
          [(and (> x-cut 400) (< x-cut 600)) #t]
          [(and (> y-cut 50) (< y-cut 650))
           (if (> vx 0) (will-goal? (after-reflect-v  right line) (* -1 vx) vy) (will-goal? (after-reflect-v left line) (* -1 vx) vy))]
          [else #f]))]))

(define (will-goal-s? x1 y1 x y x2 y2 vx vy)  ;(Checks whether the puck will go into goal after being hit by the striker)
  (let* ([vel (distance vx vy 0 0)]
         [vel-req (velocity-required x1 y1 x y x2 y2 vel)]
         [pair (give-velocity x1 y1 x2 y2 vel-req)]
         [vx1 (car pair)]
         [vy1 (cdr pair)]
         [col-list (collision x1 y1 x y vx1 vy1 vx vy)]
         [vel-x-puck (car col-list)]
         [vel-y-puck (cadr col-list)]
         [x-puck (caddr col-list)]
         [y-puck (cadddr col-list)]
         [slope (if (= vel-x-puck 0) "infinite" (/ vel-y-puck vel-x-puck))]
         [line (line-converter x-puck (* -1 y-puck) slope)])
    (will-goal? line vel-x-puck vel-y-puck)))

(define (vel-gt-max-bot x1 y1 x y x2 y2 vx vy)   ;(Checks whether the velocity with which it will reach is greater than 100)
  (let* ([vel (distance vx vy 0 0)]
         [vel-req (velocity-required x1 y1 x y x2 y2 vel)]
         )
    (if (> vel-req 100) #t #f)))     ;Change done here

(define (true x1 y1 x y x2 y2 vx vy) #t)

(define (give-velocity x1 y1 x2 y2 v1)
  (let* ([xdif (- x1 x2)]
         [ydif (- y1  y2)]
         [sqr (sqrt (+ (* xdif xdif) (* ydif ydif)))]
         [vx (* v1 (/ (* -1 xdif) sqr))]
         [vy (* v1 (/ ydif sqr))])
    (if (and (= x1 x2) (= y1 y2)) (cons 0 0) (cons vx vy))))

(define (left-cut line x1 y1)
                         (cond [(equal? (car line) "infinite") (cdr line)]
                         [(= 0 (car line)) (if (and (> (* -1 (cdr line)) y1) ( < (* -1 (cdr line)) 350)) left 1000000)]
                         [(< 0 (car line)) (if (or (< (* -1 (y-val line left)) y1) (> (* -1 (y-val line right)) 350)) 100000 (max left (x-val line -350)))  ]
                         [else (if (or (< (* -1 (y-val line right)) y1) (> (* -1 (y-val line left)) 350)) 10000 (max left (x-val line (* -1 y1))))]))

(define (right-cut line x1 y1)
                          (cond [(equal? (car line) "infinite") (cdr line)]
                          [(= 0 (car line)) (if (and (> (* -1 (cdr line) y1)) ( < (* -1 (cdr line)) 350)) right 0) ]
                          [(< 0 (car line)) (if (or (< (* -1 (y-val line left)) y1) (> (* -1 (y-val line right)) 350)) 0  (min (x-val line (* -1 y1)) right)) ]
                          [else (if (or (< (* -1 (y-val line right)) y1) (> (* -1 (y-val line left)) 350)) 0 (min right (x-val line -350)) )]))

(define (left-cut-back line x1 y1)
  (cond [(equal? (car line) "infinite") (cdr line)]
        [(= 0 (car line)) (if (> y1 (* -1 (cdr line))) left 10000)]
        [(> (car line) 0) (if (< y1 (* -1 (y-val line right))) 10000 (max left (x-val line (* -1 y1))))]
        [else (if (< y1 (* -1 (y-val line left))) 10000 (max left (x-val line (* -1 top))))]))

(define (right-cut-back line x1 y1)
  (cond [(equal? (car line) "infinite") (cdr line)]
        [(= 0 (car line)) (if (> y1 (* -1 (cdr line))) right 0)]
        [(> (car line) 0) (if (< y1 (* -1 (y-val line right))) 0 (min right (x-val line (* -1 top))))]
        [else (if (< y1 (* -1 (y-val line left))) 0 (min right (x-val line (* -1 y1))))]))

(define (no-goal-coordinates x1 y1 x y vx vy)  
  (define l '())
  (define (help x line right)
    (let* ([cord (cons x (* -1 (y-val line x)))])
      (if (> x right) l (begin (if (will-goal-s? x1 y1 x y (car cord) (cdr cord) vx vy) (void) (set! l (cons cord l))) (help (+ x 10) line right)))))
  (let* ([slope (if (= vx 0) "infinte" (/ vy vx))]
         [line (line-converter x (* -1 y) slope)]
         [left (left-cut-back line x1 y1)]
         [right (right-cut-back line x1 y1)])
    (help left line right)))


(define (list-forward f x1 y1 x y vx vy)    ;Higher Order Function
  (define l '())
  (define (help x line right)
    (let* ([cord (cons x (* -1 (y-val line x )))])
    (if (> x right) l (begin (if (f x1 y1 x y (car cord) (cdr cord) vx vy) (set! l (cons cord l)) (void)) (help (+ x 10) line right)))))
  (let* ([slope (if (= vx 0) "infinite" (/ vy vx))]
         [line (line-converter x (* -1 y) slope)]
         [left (left-cut line x1 y1)]
         [right (right-cut line x1 y1)])
  (help left line right)))

(define (shortest-distance-coordinates x1 y1 list)
  (define (help list res)
   (cond [(null? list) res]
         [else (if (< (distance x1 y1 (caar list) (cdar list)) (distance x1 y1 (car res) (cdr res))) (help (cdr list) (car list))
                   (help (cdr list) res))]))
  (help list (cons 100000 100000)))

(define (velocity-required x1 y1 x y x2 y2 v)
 (if (= v 0) vmax
  (let* ([distance-striker  (distance x1 y1 x2 y2) ]
         [distance-puck (distance x y x2 y2)]
         [time-req (/ distance-puck v)]
         )
    (/ distance-striker time-req))))

(define (velocity-required-f f x1 y1 x y vx vy )    ;Higher Order Function
  (let* ([list-f (list-forward f x1 y1 x y vx vy)]
         [short-cord  (shortest-distance-coordinates x1 y1 list-f)]
         [x2 (car short-cord)]
         [y2 (cdr short-cord)]
         [vel (distance vx vy 0 0)]
         [vel-req (velocity-required x1 y1 x y x2 y2 vel)])
    (give-velocity x1 y1 x2 y2 vel-req)))

(define (shortest-velocity x1 y1 x y vx vy) (velocity-required-f true x1 y1 x y vx vy))    ;Various strategies for the bot

(define (goal-velocity x1 y1 x y vx vy) (velocity-required-f will-goal-s? x1 y1 x y vx vy))

(define (big-velocity x1 y1 x y vx vy) (velocity-required-f vel-gt-max-bot x1 y1 x y vx vy))

(define (follow x1 y1 x y vx vy) (give-velocity x1 y1 x y (distance vx vy 0 0) ))

(define (followx vx vy) (cons vx 0))

(define (no-goal-velocity x1 y1 x y vx vy)
  (let* ([list-b (no-goal-coordinates x1 y1 x y vx vy)]
         [short-cord  (shortest-distance-coordinates x1 y1 list-b)]
         [x2 (car short-cord)]
         [y2 (cdr short-cord)]
         [vel (distance vx vy 0 0)]
         [vel-req (velocity-required x1 y1 x y x2 y2 vel)])
    (give-velocity x1 y1 x2 y2 vel-req)))



(define (new-move x1 y1 x y vx vy)
  (cond ;[(> y (+ 350 puck-radius)) (go_near_goal x x1 y1)]
        
        ;[(and (< y 350) (< vy 0)) (go_near_goal x x1 y1) ]
       ; [(and (< y y1) (> vy 0)) (no-goal-velocity x1 y1 x y vx vy)]
        ;[(and (< y y1) (< vy 0)) (if (< vx 0) (cons 20 0) (if (> vx 0) (cons -20 0) (cons 10 10)))]
        [(= vy 0) (give-velocity x1 y1  x y 100)]
        [(and (< y 350) (> y y1) (> vy 0) (> (distance vx vy 0 0) 50))  (shortest-velocity x1 y1 x y vx vy)]
        [(and (< y 350) (> y y1) (> vy 0) (< (distance vx vy 0 0) 50) (> (distance vx vy 0 0) 25))  (goal-velocity x1 y1 x y vx vy)]
        [(and (< y 350) (> y y1) (> vy 0) (< (distance vx vy 0 0) 25)) (big-velocity x1 y1 x y vx vy)]
        [(and (> y y1) (< vy 0)) (give-velocity x1 y1 500 100 50)]
        ;[(and (< y y1) (> vy 0) (> (distance vx vy 0 0) 50)) (follow x1 y1 x y vx vy)]
        [(and (< y y1) (> vy 0)) (no-goal-velocity x1 y1 x y vx vy)]
        [(and (< y y1) (< vy 0))  (cons (* -1 vx) 0)] 
        [else (followx vx vy)]))

(define (cosa x)
  [cond [(= x (/ pi 2)) 0]
        [else (cos x)]])

(define (go_near_goal x  x1 y1)
  (give-velocity x1 y1 x 100 (/ y1 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Graphics Part


(define state-list (list 'x-player 'y-player 'prev-x-player 'prev-y-player 'x-puck 'y-puck
                         'vel-x-puck 'vel-y-puck 'x-bot 'y-bot 'vel-x-bot 'vel-y-bot 'new-game 'timer 'player-score 'bot-score))

(define l (map (λ (x) (cons x 0)) state-list))



(define state (make-hash l))
(hash-set! state 'x-player 500)
(hash-set! state 'y-player 620)
(hash-set! state 'x-puck 500)
(hash-set! state 'y-puck 350)
(hash-set! state 'x-bot 500)
(hash-set! state 'y-bot 80)
 

(define (collision x-striker y-striker x-puck y-puck vel-x-striker vel-y-striker vel-x-puck vel-y-puck )   ;;Collision between the two particles
  (let* ([xdiff (- x-puck x-striker)]
         [ydiff (- y-striker y-puck)]
         [tangent (if (= xdiff 0 ) (/ pi 2) (/ ydiff xdiff))]
         [angle (if (= xdiff 0) (/ pi 2 ) (atan (abs tangent))) ]
         [sine (sin angle)]
         [cosine (cosa angle)]
         [vlpuck  (cond
                        [(> tangent 0) (+ (* vel-x-puck cosine) (* vel-y-puck sine))]
                        [else (- (* vel-y-puck sine) (* vel-x-puck cosine))])]
         [vlstriker (cond 
                          [ (> tangent 0) (+ (* vel-x-striker cosine) (* vel-y-striker sine))]
                          [else (- (* vel-y-striker sine) (* vel-x-striker cosine))])]
         [vflpuck (+ (* e (- vlstriker vlpuck)) vlstriker)]
         [vxpf (cond 
                     [ (> tangent 0) (- (+ (* sine sine vel-x-puck) (* cosine vflpuck)) (* vel-y-puck sine cosine))]
                     [else (- (+ (* sine sine vel-x-puck) (* vel-y-puck sine cosine)) (* cosine vflpuck))])]
         [vypf (cond 
                     [(> tangent 0) (- (+ (* vel-y-puck cosine cosine) (* sine vflpuck)) (* vel-x-puck sine cosine))]
                     [else (+ (* vel-y-puck cosine cosine) (* sine vflpuck) (* vel-x-puck sine cosine))])]
         [dist (sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))]
         [distinside (- (+ puck-radius striker-radius) dist)]
         [dcos (* distinside cosine)]
         [dsin (* distinside sine)] 
         [xfinal (if (>= xdiff 0) (+ x-puck  (* (+ 2 e) dcos)) (- x-puck  (* (+ 1 e) dcos)))]
         [yfinal (if (>= ydiff 0) (- y-puck  (* (+ 2 e) dsin)) (+ y-puck  (* (+ 1 e) dsin)))])
    (list vxpf vypf xfinal yfinal)))


(define (mouse2x x)
  (let* ([start (+ left striker-radius)]
         [end (- right striker-radius)]
         [trav (- end start)]
         [mtrav (- 900 100)])
  (cond [(<= x 100) start]
        [(and (> x 100) (< x 900)) (+ start (* (/ trav mtrav) (- x 100))) ]
        [(>= x 900) end])))
  
(define (mouse2y y)
  (let* ([length (- bottom top)]
         [start (+ top (/ length 2) striker-radius)]
         [end (- bottom striker-radius)]
         [trav (- end start)]
         [mtrav (- 650 50)])
  (cond [(<= y 50) start]
        [(and (> y 50) (< y 650)) (+ start  (* (/ trav mtrav) (- y 50)))]
        [(>= y 650) end])))


(define (collide? xs ys xp yp d)
  (let* ([ydif (- yp ys)]
         [xdif (- xp xs)]
         [d1 (sqrt (+ (* ydif ydif) (* xdif xdif)))])
    (if (< d1 d)  #t #f)))


(define (mousehandler st x y me)
  (cond [(= 0 (hash-ref st 'new-game))
         (if (and (mouse=? me "button-down" ) (and (< x 320) (> y 600))) (hash-set! st 'new-game 1) void )]
         
   [else (hash-set! st 'x-player (mouse2x x))
   (hash-set! st 'y-player (mouse2y y))])
   st)


(define (velocity-checker v)
  (cond [(>= v 0 ) ( min v 150)]
        [(< v 0)  (max v -150)]))

(define (update st)

  
 
  (let* ((x-player (hash-ref st 'x-player))
         (y-player (hash-ref st 'y-player))
         (x-bot (hash-ref st 'x-bot))
         (y-bot (hash-ref st 'y-bot))
         (x-puck (hash-ref st 'x-puck))
         (y-puck (hash-ref st 'y-puck))
         (vel-x-puck (hash-ref st 'vel-x-puck))
         (vel-y-puck (hash-ref st 'vel-y-puck))
         (vel-x-bot (hash-ref st 'vel-x-bot))
         (vel-y-bot (hash-ref st 'vel-y-bot))
         (vel-x-player (* (- (hash-ref st 'x-player) (hash-ref st 'prev-x-player)) 28))
         (vel-y-player (* -1 (* (- (hash-ref st 'y-player) (hash-ref st 'prev-y-player)) 28)))
         (length (- bottom top)))

    (define (init-botx)
     (if (= (hash-ref st 'timer) 0) (hash-set! st 'vel-x-bot
               (velocity-checker (car (new-move x-bot y-bot (hash-ref st 'x-puck)
                                            (hash-ref st 'y-puck) (hash-ref st 'vel-x-puck) (hash-ref st 'vel-y-puck))))) (hash-set! st 'vel-x-bot 0) ))

  (define (init-boty)
    (if (= (hash-ref st 'timer) 0) (hash-set! st 'vel-y-bot
               (velocity-checker (cdr (new-move x-bot y-bot (hash-ref st 'x-puck)
                                            (hash-ref st 'y-puck) (hash-ref st 'vel-x-puck) (hash-ref st 'vel-y-puck))))) (hash-set! st 'vel-y-bot 0)))
    
 (begin (hash-set! st 'prev-x-player (hash-ref st 'x-player))
        (hash-set! st 'prev-y-player (hash-ref st 'y-player))
        (if (> (hash-ref st 'timer) 0) (hash-set! st 'timer (- (hash-ref st 'timer) 1 )) void)
        
        
        (hash-set! st 'x-bot (cond [(and (<= x-bot (- right striker-radius)) (>= x-bot (+ left striker-radius))) (+ x-bot (/ vel-x-bot 28))]
                                   [(> x-bot (- right striker-radius)) (- right striker-radius)]
                                   [else (+ left striker-radius)]))                                  
       
        (hash-set! st 'y-bot (cond [(and (<= y-bot (- (+ top (/ length 2))   striker-radius)) (>= y-bot (+ top striker-radius)))
                                                                                            (- y-bot (/ vel-y-bot 28))]
                                   [(> y-bot (- (+ top (/ length 2))   striker-radius)) (- (+ top (/ length 2))   striker-radius)]
                                   [else (+ top striker-radius)]))
         

        
        
        (cond

            [  (and (> x-puck 400) (< x-puck 600) (< y-puck (+ top puck-radius)))
               (hash-set! st 'player-score (+ (hash-ref  st 'player-score) 1))
               (hash-set! st 'x-player 500)
               (hash-set! st 'y-player 620)
               (hash-set! st 'x-puck 500)
               (hash-set! st 'y-puck 300)
               (hash-set! st 'x-bot 500)
               (hash-set! st 'y-bot 80)
               (hash-set! st 'vel-x-bot 0)
               (hash-set! st 'vel-y-bot 0)
               (hash-set! st 'vel-x-puck 0)
               (hash-set! st 'vel-y-puck 0)
               ]


             [(and (> x-puck 400) (< x-puck 600) (> y-puck (- bottom puck-radius)))
               (hash-set! st 'bot-score (+ (hash-ref  st 'bot-score) 1))
               (hash-set! st 'x-player 500)
               (hash-set! st 'y-player 620)
               (hash-set! st 'x-puck 500)
               (hash-set! st 'y-puck 400)
               (hash-set! st 'x-bot 500)
               (hash-set! st 'y-bot 80)
               (hash-set! st 'vel-x-bot 0)
               (hash-set! st 'vel-y-bot 0)
               (hash-set! st 'vel-x-puck 0)
               (hash-set! st 'vel-y-puck 0)]


            [(collide? x-player y-player x-puck y-puck (+ puck-radius striker-radius))
             (let* ((col-list (collision x-player y-player x-puck y-puck vel-x-player vel-y-player vel-x-puck vel-y-puck )))
               (begin  (hash-set! st 'vel-x-puck (if (> (car col-list) 0) (min 100 (car col-list)) (max -100 (car col-list))))
                       (hash-set! st 'vel-y-puck (if (> (cadr col-list) 0) (min 100 (cadr col-list)) (max -100 (cadr col-list))))
                       (hash-set! st 'x-puck (caddr col-list))
                       (hash-set! st 'y-puck (cadddr col-list))
                       (init-botx) 
                       (init-boty)
                       ))]
              [(collide? x-bot y-bot x-puck y-puck (+ puck-radius striker-radius))
               (let* ((col-list (collision x-bot y-bot x-puck y-puck vel-x-bot vel-y-bot vel-x-puck vel-y-puck )))
                (begin (hash-set! st 'vel-x-puck  (min 100 (car col-list)))
                       (hash-set! st 'vel-y-puck  (min 100 (cadr col-list)))
                       (hash-set! st 'x-puck (caddr col-list))
                       (hash-set! st 'y-puck (cadddr col-list))
                       (hash-set! st 'vel-x-bot 0)
                       (hash-set! st 'vel-y-bot 0)
                       (hash-set! st 'timer 10)))]
              
              [(<= x-puck (+ left puck-radius))
                   (begin (hash-set! st 'x-puck (+ x-puck (* 2 (- (+ left puck-radius) x-puck))))
                   (hash-set! st 'vel-x-puck (* -1 (hash-ref st 'vel-x-puck)))
                   (init-botx) 
                   (init-boty))]
              
              [(>= x-puck (- right puck-radius))
                   (begin (hash-set! st 'x-puck (+ x-puck (* 2 (- (- right puck-radius) x-puck))))
                   (hash-set! st 'vel-x-puck (* -1 (hash-ref st 'vel-x-puck)))
                   (init-botx) 
                   (init-boty))]
              
              [(<= y-puck (+ top puck-radius))
                   (begin (hash-set! st 'y-puck (+ y-puck (* 2 (- (+ top puck-radius) y-puck))))
                   (hash-set! st 'vel-y-puck (* -1 (hash-ref st 'vel-y-puck)))
                   (init-botx) 
                   (init-boty))]
              
              [(>= y-puck (- bottom puck-radius))
                   (begin (hash-set! st 'y-puck (+ y-puck (* 2 (- (- bottom puck-radius) y-puck))))
                   (hash-set! st 'vel-y-puck (* -1 (hash-ref st 'vel-y-puck)))
                   (init-botx) 
                   (init-boty))]
              
              [(and (> vel-y-puck 0) (> y-puck (+ top (/ (- bottom top) 2))))
              
                   (begin (hash-set! st 'x-puck (+ x-puck (/ vel-x-puck 28)))
                           (hash-set! st 'y-puck (- y-puck (/ vel-y-puck 28)))
                   (init-botx)
                   (init-boty)
                           )]

              [(and (= vel-x-puck 0) (= vel-y-puck 0) (= y-puck 300))
               (begin (hash-set! st 'x-puck (+ x-puck (/ vel-x-puck 28)))
                           (hash-set! st 'y-puck (- y-puck (/ vel-y-puck 28)))
                   (init-botx)
                   (init-boty))]

              [ (< y-puck y-bot)
                (begin (hash-set! st 'x-puck (+ x-puck (/ vel-x-puck 28)))
                           (hash-set! st 'y-puck (- y-puck (/ vel-y-puck 28)))
                   (init-botx)
                   (init-boty))]
                
                
                    
              [else (begin (hash-set! st 'x-puck (+ x-puck (/ vel-x-puck 28)))
                           (hash-set! st 'y-puck (- y-puck (/ vel-y-puck 28)))
                           ;(init-botx)
                           ;(init-boty)
                           )])
    st)))

(define (scoreboard n1 n2)
  (place-image (text "BOT" 40 "black") 50 90 (place-image (text "PLAYER" 40 "black") 220 90 (place-image (text (number->string n1) 60 "black") 50 150
               (place-image (text (number->string n2) 60 "black") 230 150
                            (place-image (text "SCOREBOARD" 40 "black") 150 30 (rectangle 300 250 "solid" "white")))))))



(define (my-board st)
  (cond [(= 0 (hash-ref st 'new-game)) (place-image (bitmap/file "openingwindow.jpg" ) 500 350 (rectangle 1000 700 "outline" "black"))]
        [else
  (let* ((x-player (hash-ref st 'x-player))
         (y-player (hash-ref st 'y-player))
         (x-bot (hash-ref st 'x-bot))
         (y-bot (hash-ref st 'y-bot))
         (x-puck (hash-ref st 'x-puck))
         (y-puck (hash-ref st 'y-puck)))
               (place-image (scoreboard (hash-ref st 'bot-score) (hash-ref st 'player-score)) 851 350
               (place-image (rectangle 1000 700 "outline" "black") 500 350
               (place-image (rectangle 400 600 "outline" "red" ) 500 350              
               (place-image (circle striker-radius "solid" "blue") x-player y-player
               (place-image (circle striker-radius "solid" "indigo") x-bot y-bot
               (place-image (circle puck-radius "solid" "red") x-puck y-puck             
               (place-image (rectangle 200 20 "solid" "yellow" ) 500 40
               (place-image (rectangle 200 20 "solid" "yellow" ) 500 660
               (overlay (circle 25 "solid" (color 211 211 211 200))
                        (overlay (circle 50 "outline" "blue" ) (add-line (rectangle 1000 700 "outline" "white") 300 350 700 350 "marron"))))))))))))]))

(big-bang state
          [on-mouse mousehandler]
          [on-tick update 1/100 ]
          [to-draw my-board])

