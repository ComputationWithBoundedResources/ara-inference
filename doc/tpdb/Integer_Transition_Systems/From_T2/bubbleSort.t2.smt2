(declare-sort Loc 0)
(declare-const l0 Loc)
(declare-const l1 Loc)
(declare-const l2 Loc)
(declare-const l3 Loc)
(declare-const l4 Loc)
(declare-const l5 Loc)
(declare-const l6 Loc)
(declare-const l7 Loc)
(declare-const l8 Loc)
(declare-const l9 Loc)
(declare-const l10 Loc)
(assert (distinct l0 l1 l2 l3 l4 l5 l6 l7 l8 l9 l10))

(define-fun cfg_init ( (pc Loc) (src Loc) (rel Bool) ) Bool
  (and (= pc src) rel))

(define-fun cfg_trans2 ( (pc Loc) (src Loc)
                         (pc1 Loc) (dst Loc)
                         (rel Bool) ) Bool
  (and (= pc src) (= pc1 dst) rel))

(define-fun cfg_trans3 ( (pc Loc) (exit Loc)
                         (pc1 Loc) (call Loc)
                         (pc2 Loc) (return Loc)
                         (rel Bool) ) Bool
  (and (= pc exit) (= pc1 call) (= pc2 return) rel))

(define-fun init_main ( (pc^0 Loc) (i^0 Int) (j^0 Int) (tmp5^0 Int) (x3^0 Int) (y4^0 Int) ) Bool
  (cfg_init pc^0 l10 true))

(define-fun next_main (
                 (pc^0 Loc) (i^0 Int) (j^0 Int) (tmp5^0 Int) (x3^0 Int) (y4^0 Int)
                 (pc^post Loc) (i^post Int) (j^post Int) (tmp5^post Int) (x3^post Int) (y4^post Int)
             ) Bool
  (or
    (cfg_trans2 pc^0 l0 pc^post l1 (and (and (and (and (= i^0 i^post) (= j^0 j^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l2 pc^post l3 (and (and (and (and (= i^0 i^post) (= j^0 j^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l4 pc^post l2 (and (and (and (and (= i^0 i^post) (= j^0 j^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l4 pc^post l2 (and (and (and (and (= i^0 i^post) (= j^0 j^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l5 pc^post l6 (and (and (and (and (= j^post (+ 1 j^0)) (= i^0 i^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l6 pc^post l7 (and (and (and (and (= i^0 i^post) (= j^0 j^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l8 pc^post l5 (and (and (and (and (= x3^post (+ 0 j^0)) (= y4^post (+ 1 j^0))) (= tmp5^post tmp5^post)) (= i^0 i^post)) (= j^0 j^post)))
    (cfg_trans2 pc^0 l8 pc^post l5 (and (and (and (and (= i^0 i^post) (= j^0 j^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l7 pc^post l0 (and (and (and (and (and (<= (+ 0 i^0) (+ 0 j^0)) (= i^post (+ -1 i^0))) (= j^0 j^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l7 pc^post l8 (and (and (and (and (and (<= (+ 1 j^0) (+ 0 i^0)) (= i^0 i^post)) (= j^0 j^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l1 pc^post l4 (and (and (and (and (and (<= (+ 1 i^0) 0) (= i^0 i^post)) (= j^0 j^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l1 pc^post l6 (and (and (and (and (and (<= 0 (+ 0 i^0)) (= i^0 i^post)) (= j^0 j^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l9 pc^post l0 (and (and (and (and (= j^post 0) (= i^post 4)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
    (cfg_trans2 pc^0 l10 pc^post l9 (and (and (and (and (= i^0 i^post) (= j^0 j^post)) (= tmp5^0 tmp5^post)) (= x3^0 x3^post)) (= y4^0 y4^post)))
  )
)
