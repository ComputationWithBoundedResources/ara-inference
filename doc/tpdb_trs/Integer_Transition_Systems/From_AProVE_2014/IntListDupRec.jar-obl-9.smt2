(declare-sort Loc 0)
(declare-const f1_0_main_Load Loc)
(declare-const f117_0_createList_GE Loc)
(declare-const f232_0_main_InvokeMethod Loc)
(declare-const f389_0_dupList_NONNULL Loc)
(declare-const __init Loc)
(assert (distinct f1_0_main_Load f117_0_createList_GE f232_0_main_InvokeMethod f389_0_dupList_NONNULL __init ))

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

(define-fun init_main ( (pc Loc) (arg1 Int) (arg2 Int) (arg3 Int) ) Bool
  (cfg_init pc __init true))

(define-fun next_main (
                 (pc Loc) (arg1 Int) (arg2 Int) (arg3 Int)
                 (pc1 Loc) (arg1P Int) (arg2P Int) (arg3P Int)
             ) Bool
  (or
    (cfg_trans2 pc f1_0_main_Load pc1 f117_0_createList_GE (and (and (> arg1P (- 1)) (> arg2 0)) (> arg1 0)))
    (cfg_trans2 pc f1_0_main_Load pc1 f232_0_main_InvokeMethod (exists ((x7 Int)) (and (and (and (and (and (and (> x7 (- 1)) (> arg2 0)) (<= arg1P arg1)) (> arg1 0)) (> arg1P 0)) (> arg2P 3)) (= 0 arg3P))))
    (cfg_trans2 pc f1_0_main_Load pc1 f232_0_main_InvokeMethod (exists ((x12 Int)) (and (and (and (and (and (and (and (> x12 (- 1)) (> arg2 0)) (>= arg1 arg1P)) (>= arg1 (- arg2P 1))) (> arg1 0)) (> arg1P 0)) (> arg2P 1)) (= 0 arg3P))))
    (cfg_trans2 pc f117_0_createList_GE pc1 f117_0_createList_GE (and (and (> arg1 (- 1)) (< (- arg1 1) arg1)) (= (- arg1 1) arg1P)))
    (cfg_trans2 pc f232_0_main_InvokeMethod pc1 f389_0_dupList_NONNULL (exists ((x15 Int)) (and (and (and (and (and (and (and (and (<= arg1P arg2) (> x15 0)) (<= (+ arg2P 2) arg2)) (> arg1 0)) (> arg2 1)) (> arg1P 1)) (> arg2P (- 1))) (= 0 arg3)) (= 0 arg3P))))
    (cfg_trans2 pc f389_0_dupList_NONNULL pc1 f389_0_dupList_NONNULL (and (and (and (and (and (and (and (and (and (and (and (<= (+ arg1P 2) arg1) (< arg3 0)) (<= arg1P arg2)) (<= (+ arg2P 3) arg1)) (<= (+ arg2P 1) arg2)) (> arg1 2)) (> arg2 0)) (> arg1P 0)) (> arg2P (- 1))) (<= (+ arg3P 4) arg1)) (<= (+ arg3 2) arg1)) (<= (+ arg3P 2) arg2)))
    (cfg_trans2 pc f389_0_dupList_NONNULL pc1 f389_0_dupList_NONNULL (and (and (and (and (and (and (and (and (and (and (and (<= (+ arg1P 2) arg1) (> arg3 0)) (<= arg1P arg2)) (<= (+ arg2P 3) arg1)) (<= (+ arg2P 1) arg2)) (> arg1 2)) (> arg2 0)) (> arg1P 0)) (> arg2P (- 1))) (<= (+ arg3P 4) arg1)) (<= (+ arg3 2) arg1)) (<= (+ arg3P 2) arg2)))
    (cfg_trans2 pc f389_0_dupList_NONNULL pc1 f389_0_dupList_NONNULL (and (and (and (and (and (and (and (and (and (<= arg1P arg1) (<= (- arg1P 2) arg2)) (<= (+ arg2P 2) arg1)) (<= arg2P arg2)) (> arg1 2)) (> arg2 0)) (> arg1P 2)) (> arg2P 0)) (= 0 arg3)) (= 1 arg3P)))
    (cfg_trans2 pc __init pc1 f1_0_main_Load true)
  )
)