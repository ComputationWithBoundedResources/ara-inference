(* breadth-first search *)

(* appendreverse : (L(T(int)),L(T(int))) -> L(T(int)) *)
appendreverse (toreverse,sofar) = match toreverse with
    | nil -> sofar
    | (a::as) -> appendreverse(as,a::sofar);

(* reverse: L(T(int)) -> L(T(int)) *)
reverse xs = appendreverse(xs,[]);

(* bfs : (L(T(int)),L(T(int)),int) -> T(int) *)
bfs(queue,futurequeue,x) = match queue with
  | nil -> match futurequeue with
             | nil -> leaf
             | (t::ts) -> bfs(reverse(t::ts),[],x)
  | (t::ts) -> match t with
                | leaf -> bfs(ts,futurequeue,x)
                | node(y,t1,t2) -> if x==y then node(y,t1,t2) else bfs(ts,t2::t1::futurequeue,x);

(* dobfs : (T(int),int) -> T(int) *)
dobfs(t,x) = bfs([t],[],x);

(* bfs2 : (T(int),int) -> T(int) *)
bfs2(t,x) = let t' = dobfs(t,x) in dobfs(t',x);

main = dodfs(node(+1,node(+2,leaf,leaf),node(+3,leaf,leaf)),+4)
