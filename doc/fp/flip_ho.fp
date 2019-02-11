
type u = E | Z of u | O of u;;

let rec flip x =
  match x with
  | E -> E
  | Z(x') -> O(flip x')
  | O(x') -> Z(flip x')
;;

let main w = flip w;;


