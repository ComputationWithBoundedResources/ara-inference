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
(declare-const l11 Loc)
(declare-const l12 Loc)
(declare-const l13 Loc)
(declare-const l14 Loc)
(declare-const l15 Loc)
(declare-const l16 Loc)
(declare-const l17 Loc)
(declare-const l18 Loc)
(declare-const l19 Loc)
(declare-const l20 Loc)
(declare-const l21 Loc)
(declare-const l22 Loc)
(declare-const l23 Loc)
(declare-const l24 Loc)
(assert (distinct l0 l1 l2 l3 l4 l5 l6 l7 l8 l9 l10 l11 l12 l13 l14 l15 l16 l17 l18 l19 l20 l21 l22 l23 l24))

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

(define-fun init_main ( (pc^0 Loc) (N^0 Int) (b_arp^0 Int) (b_configured^0 Int) (b_ip^0 Int) (b_probe^0 Int) (i^0 Int) (k^0 Int) (pos^0 Int) (seq^0 Int) (z^0 Int) ) Bool
  (cfg_init pc^0 l24 true))

(define-fun next_main (
                 (pc^0 Loc) (N^0 Int) (b_arp^0 Int) (b_configured^0 Int) (b_ip^0 Int) (b_probe^0 Int) (i^0 Int) (k^0 Int) (pos^0 Int) (seq^0 Int) (z^0 Int)
                 (pc^post Loc) (N^post Int) (b_arp^post Int) (b_configured^post Int) (b_ip^post Int) (b_probe^post Int) (i^post Int) (k^post Int) (pos^post Int) (seq^post Int) (z^post Int)
             ) Bool
  (or
    (cfg_trans2 pc^0 l0 pc^post l1 (and (and (and (and (and (and (and (and (and (and (<= (+ 0 seq^0) (+ 1 N^0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l2 pc^post l0 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 b_arp^0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l2 pc^post l0 (and (and (and (and (and (and (and (and (and (and (<= (+ 0 b_arp^0) 0) (= b_configured^post 1)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l3 pc^post l2 (and (and (and (and (and (and (and (and (and (= N^0 N^post) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l4 pc^post l5 (and (and (and (and (and (and (and (and (and (and (= k^post (+ 1 k^0)) (<= (+ 0 seq^0) (+ 1 N^0))) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l6 pc^post l4 (and (and (and (and (and (and (and (and (and (and (<= (+ 0 seq^0) (+ 1 N^0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l7 pc^post l6 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 pos^0)) (= i^post (+ -1 i^0))) (= pos^post 0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= k^0 k^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l7 pc^post l6 (and (and (and (and (and (and (and (and (and (and (and (<= (+ 0 pos^0) 0) (<= (+ 0 b_arp^0) 0)) (= pos^post (+ 1 pos^0))) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l8 pc^post l6 (and (and (and (and (and (and (and (and (and (and (and (<= (+ 0 i^0) 0) (= seq^post (+ 1 seq^0))) (= i^post (+ 3 seq^post))) (= z^post z^post)) (<= 0 (+ 0 z^post))) (= pos^post 0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= k^0 k^post)))
    (cfg_trans2 pc^0 l8 pc^post l7 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 i^0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l9 pc^post l6 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 z^0)) (= z^post (+ -1 z^0))) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)))
    (cfg_trans2 pc^0 l9 pc^post l8 (and (and (and (and (and (and (and (and (and (and (<= (+ 0 z^0) 0) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l10 pc^post l4 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 b_ip^0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l10 pc^post l9 (and (and (and (and (and (and (and (and (and (and (and (and (<= (+ 0 b_ip^0) 0) (= b_arp^post b_arp^post)) (<= 0 (+ 0 b_arp^post))) (<= (+ 0 b_arp^post) 1)) (= N^0 N^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l11 pc^post l4 (and (and (and (and (and (and (and (and (and (and (<= (+ 1 b_probe^0) 1) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l11 pc^post l10 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 b_probe^0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l12 pc^post l11 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 pos^0)) (= i^post (+ -1 i^0))) (= pos^post 0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= k^0 k^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l12 pc^post l11 (and (and (and (and (and (and (and (and (and (and (and (<= (+ 0 pos^0) 0) (<= (+ 0 b_probe^0) 0)) (= pos^post (+ 1 pos^0))) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l13 pc^post l11 (and (and (and (and (and (and (and (and (and (and (and (<= (+ 0 i^0) 0) (= seq^post (+ 1 seq^0))) (= i^post (+ 3 seq^post))) (= z^post z^post)) (<= 0 (+ 0 z^post))) (= pos^post 0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= k^0 k^post)))
    (cfg_trans2 pc^0 l13 pc^post l12 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 i^0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l14 pc^post l11 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 z^0)) (= z^post (+ -1 z^0))) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)))
    (cfg_trans2 pc^0 l14 pc^post l13 (and (and (and (and (and (and (and (and (and (and (<= (+ 0 z^0) 0) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l15 pc^post l3 (and (and (and (and (and (and (and (and (and (and (<= (+ 1 N^0) (+ 0 k^0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l15 pc^post l14 (and (and (and (and (and (and (and (and (and (and (and (and (<= (+ 0 k^0) (+ 0 N^0)) (= b_probe^post b_probe^post)) (<= 0 (+ 0 b_probe^post))) (<= (+ 0 b_probe^post) 1)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l16 pc^post l3 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 b_arp^0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l16 pc^post l15 (and (and (and (and (and (and (and (and (and (and (<= (+ 0 b_arp^0) 0) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l5 pc^post l16 (and (and (and (and (and (and (and (and (and (= N^0 N^post) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l17 pc^post l5 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 pos^0)) (= i^post (+ -1 i^0))) (= pos^post 0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= k^0 k^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l17 pc^post l5 (and (and (and (and (and (and (and (and (and (and (and (<= (+ 0 pos^0) 0) (<= (+ 0 b_ip^0) 0)) (= pos^post (+ 1 pos^0))) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l18 pc^post l5 (and (and (and (and (and (and (and (and (and (and (and (<= (+ 0 i^0) 0) (= seq^post (+ 1 seq^0))) (= i^post (+ 3 seq^post))) (= z^post z^post)) (<= 0 (+ 0 z^post))) (= pos^post 0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= k^0 k^post)))
    (cfg_trans2 pc^0 l18 pc^post l17 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 i^0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l19 pc^post l5 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 z^0)) (= z^post (+ -1 z^0))) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)))
    (cfg_trans2 pc^0 l19 pc^post l18 (and (and (and (and (and (and (and (and (and (and (<= (+ 0 z^0) 0) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l20 pc^post l1 (and (and (and (and (and (and (and (and (and (and (and (= seq^post 1) (= i^post (+ 3 seq^post))) (= z^post z^post)) (<= 0 (+ 0 z^post))) (= pos^post 0)) (= N^post N^post)) (<= 0 (+ 0 N^post))) (= b_ip^post 0)) (= b_probe^post 0)) (= b_arp^post 0)) (= b_configured^post 0)) (= k^post 0)))
    (cfg_trans2 pc^0 l21 pc^post l22 (and (and (and (and (and (and (and (and (and (= N^0 N^post) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l23 pc^post l21 (and (and (and (and (and (and (and (and (and (and (<= 1 (+ 0 b_configured^0)) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l23 pc^post l21 (and (and (and (and (and (and (and (and (and (and (<= (+ 1 b_configured^0) 0) (= N^0 N^post)) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l23 pc^post l19 (and (and (and (and (and (and (and (and (and (and (and (and (and (<= (+ 0 b_configured^0) 0) (<= 0 (+ 0 b_configured^0))) (= b_arp^post 0)) (= b_probe^post 0)) (= k^post 0)) (= b_ip^post b_ip^post)) (<= 0 (+ 0 b_ip^post))) (<= (+ 0 b_ip^post) 1)) (= N^0 N^post)) (= b_configured^0 b_configured^post)) (= i^0 i^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l1 pc^post l23 (and (and (and (and (and (and (and (and (and (= N^0 N^post) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
    (cfg_trans2 pc^0 l24 pc^post l20 (and (and (and (and (and (and (and (and (and (= N^0 N^post) (= b_arp^0 b_arp^post)) (= b_configured^0 b_configured^post)) (= b_ip^0 b_ip^post)) (= b_probe^0 b_probe^post)) (= i^0 i^post)) (= k^0 k^post)) (= pos^0 pos^post)) (= seq^0 seq^post)) (= z^0 z^post)))
  )
)
