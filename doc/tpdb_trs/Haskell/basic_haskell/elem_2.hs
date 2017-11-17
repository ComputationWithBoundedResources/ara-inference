{-# htermination (elemRatio :: Ratio MyInt  ->  (List (Ratio MyInt))  ->  MyBool) #-} 
import qualified Prelude 
data MyBool = MyTrue | MyFalse 
data List a = Cons a (List a) | Nil 
data MyInt = Pos Nat  | Neg Nat ;

data Nat = Succ Nat  | Zero ;

data Ratio a = CnPc a a;

map :: (a  ->  b)  ->  (List a)  ->  (List b);
map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

foldr :: (b  ->  a  ->  a)  ->  a  ->  (List b)  ->  a;
foldr f z Nil = z;
foldr f z (Cons x xs) = f x (foldr f z xs);

pePe :: MyBool  ->  MyBool  ->  MyBool;
pePe MyFalse x = x;
pePe MyTrue x = MyTrue;

or :: (List MyBool)  ->  MyBool;
or = foldr pePe MyFalse;

pt :: (a  ->  b)  ->  (c  ->  a)  ->  c  ->  b;
pt f g x = f (g x);

any :: (a  ->  MyBool)  ->  (List a)  ->  MyBool;
any p = pt or (map p);

asAs :: MyBool  ->  MyBool  ->  MyBool;
asAs MyFalse x = MyFalse;
asAs MyTrue x = x;

primEqNat :: Nat  ->  Nat  ->  MyBool;
primEqNat Zero Zero = MyTrue;
primEqNat Zero (Succ y) = MyFalse;
primEqNat (Succ x) Zero = MyFalse;
primEqNat (Succ x) (Succ y) = primEqNat x y;

primEqInt :: MyInt  ->  MyInt  ->  MyBool;
primEqInt (Pos (Succ x)) (Pos (Succ y)) = primEqNat x y;
primEqInt (Neg (Succ x)) (Neg (Succ y)) = primEqNat x y;
primEqInt (Pos Zero) (Neg Zero) = MyTrue;
primEqInt (Neg Zero) (Pos Zero) = MyTrue;
primEqInt (Neg Zero) (Neg Zero) = MyTrue;
primEqInt (Pos Zero) (Pos Zero) = MyTrue;
primEqInt vv vw = MyFalse;

esEsMyInt :: MyInt  ->  MyInt  ->  MyBool
esEsMyInt = primEqInt;

esEsRatio :: Ratio MyInt  ->  Ratio MyInt  ->  MyBool
esEsRatio (CnPc x0 x1) (CnPc y0 y1) = asAs (esEsMyInt x0 y0) (esEsMyInt x1 y1);

elemRatio :: Ratio MyInt  ->  (List (Ratio MyInt))  ->  MyBool
elemRatio = pt any esEsRatio;

