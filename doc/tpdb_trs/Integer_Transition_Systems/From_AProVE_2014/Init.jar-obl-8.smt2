(declare-sort Loc 0)
(declare-const f38_0__clinit__Load Loc)
(declare-const f1_0_main_New Loc)
(declare-const __init Loc)
(assert (distinct f38_0__clinit__Load f1_0_main_New __init ))

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

(define-fun init_main ( (pc Loc)  ) Bool
  (cfg_init pc __init true))

(define-fun next_main (
                 (pc Loc) 
                 (pc1 Loc) 
             ) Bool
  (or
    (cfg_trans2 pc f38_0__clinit__Load pc1 f38_0__clinit__Load true)
    (cfg_trans2 pc f1_0_main_New pc1 f38_0__clinit__Load true)
    (cfg_trans2 pc __init pc1 f1_0_main_New true)
  )
)
