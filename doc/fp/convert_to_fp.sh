
FILES=`find . -name "btree_lookup.raml"`

dtUnit="type Unit = Unit

"

dtNat="type nat = 0 | S of nat

"

dtList="type 'a list = Nil | Cons of 'a * 'a list

"

dtPair="type ('a,'b) pair = Pair of 'a * 'b

"

dtSome="type 'a option = None | Some of 'a

"

dtBool="type bool = True | False

"

fstsnd="
let fst x =
  match x with
  | Pair(a,b) -> a

let snd x =
  match x with
  | Pair(a,b) -> b

"

geqleqeq="
let rec leqNat x y =
  match x with
  | 0 -> True
  | S(x') -> (match y with
            | S(y') -> leqNat x' y'
            | 0 -> False)

let rec eqNat x y =
  match y with
  | 0 -> (match x with
      | 0 -> True
      | S(x') -> False)
  | S(y') -> (match x with
            | S(x') -> eqNat x' y'
            | 0 -> False)

let rec geqNat x y =
  match x with
  | 0 -> False
  | S(x') -> (match y with
             | 0 -> True
             | S(x') -> geqNat x' y')

let rec ltNat x y =
  match x with
   | 0 -> True
   | S(x') -> (match y with
        | 0 -> False
        | S(y') -> ltNat x' y')

let rec gtNat x y =
  match x with
   | 0 -> False
   | S(x') -> (match y with
             | 0 -> True
             | S(y') -> gtNat x' y')


"

intro="
let ifz n th el = match n with
   | 0 -> th 0
   | S(x) -> el x

let ite b th el = match b with
   | True -> th
   | False -> el

let minus n m =
  let rec minus' m n = match m with
        | 0 -> 0
        | S(x) -> (match n with
          | 0 -> m
          | S(y) -> minus' x y)
  in Pair(minus' n m,m)

let rec plus n m = match m with
  | 0 -> n
  | S(x) -> S(plus n x)

type ('a,'b,'c) triple = Triple of 'a * 'b * 'c

let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> (match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> (match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)))

let rec mult n m = match n with
   | 0 -> 0
   | S(x) -> S(plus (mult x m) m)

"

addDt() {
    file="$1"
    str="$3"
    count=`grep "$2" -c $file`
    if [ $count -eq 0 ]; then
        printf "$str" | cat - $file > /tmp/tempfile && mv /tmp/tempfile $file;
    fi
}


# file=calculator.raml

for fn in $FILES; do
    # add datatypes
    rm -f /tmp/tempfile
    file=$fn.ocaml
    cp $fn $file

    # add datatypes

    addDt "$file" "Pair" "$dtPair"
    addDt "$file" "Unit" "$dtUnit"
    addDt "$file" "nat"  "$dtNat"
    addDt "$file" "Cons" "$dtList"
    addDt "$file" "Some" "$dtSome"
    addDt "$file" "bool" "$dtBool"
    printf "$intro" | cat - $file >> /tmp/tempfile && mv /tmp/tempfile $file;
    printf "$geqleqeq" | cat - $file >> /tmp/tempfile && mv /tmp/tempfile $file;


    # printf "$intro" | cat - $file > /tmp/tempfile && mv /tmp/tempfile $file;

    # replace trailing whitespaces
    sed -i 's/[ \t]*$//' $file

    # function --> match xyz with
    sed -i "s/= function/xyz = match xyz with/g" $file


    # Replace lists
    sed -i "s/\([a-zA-Z0-9']*\)[ ]*::[ ]*\(\(([^)]*)\|[a-zA-Z0-9'][a-zA-Z0-9']*\)\)/Cons(\1,\2)/g" $file
    sed -i "s/\[\]/Nil/g" $file


    # Replace pairs
    # sed -i "/^type/! s/\([^a-zA-Z0-9']*\)(\([^,)(]*\),\([^(,)]*\))/\1Pair(\2,\3)/g" $file # no constructors
    # sed -i "/^type/! s/\([^a-zA-Z0-9']*\)(\([^,)(]*\),\([a-zA-Z0-9']*([^)]*)\))/\1Pair(\2,\3)/g" $file # 1 constructors in rhs
    # sed -i "/^type/! s/\([^a-zA-Z0-9']*\)(\([a-zA-Z0-9']*([^)]*)\),\([^,)(]*\))/\1Pair(\2,\3)/g" $file # 1 constructors in lhs
    # sed -i "/^type/! s/^\(.*\) let (\([^,)(]*\),\([^,()]*\),\([^,()]*\))/\1let Triple(\2,\3,\4)/g" $file # no constructors
    # # sed -i "/^type/! s/\([^a-zA-Z0-9']*\)(\([^,)(]*\),\([a-zA-Z0-9']*([^)]*)\))/\1Pair(\2,\3)/g" $file # 1 constructors in rhs
    # # sed -i "/^type/! s/\([^a-zA-Z0-9']*\)(\([a-zA-Z0-9']*([^)]*)\),\([^,)(]*\))/\1Pair(\2,\3)/g" $file # 1 constructors in lhs

    # sed -i "/^type/! s/let (\([^,]*\),\([^,]*\)) = \(.*\) in/match (\3) with\n        | Pair(\1,\2) -> /g" $file
    # sed -i "/^type/! s/let (\([^,]*\),\([^,]*\),\([^,]*\)) = \(.*\) in/match (\4) with\n        | Triple(\1,\2,\3) -> /g" $file
    # # sed -i "/^type/! s/ (\(.*\),\(.*\))/ Pair(\1,\2)/g" $file
    # sed -i "/^type/! s/ (Nil,Nil)/ Pair(Nil,Nil)/g" $file


    # Replace exceptions by types
    sed -i "s/exception Not_found of int \* int/type ('a,'b) exception = Not_found of 'a * 'b;;/g" $file
    # sed -i "s/raise[ ]*(Not_found[ ]*\(([^)]*)\))/error Not_found\1/g" $file
    sed -i "s/raise/error/g" $file

    # replace = function with match ... with
    sed -i "s/ \([a-zA-Z']*\) = function/\1 = match \1 with/g" $file

    # Check if there is a pattern match on Pair within a function using let
    fstsndadded=0
    count=`grep -e "let Pair([a-zA-Z'0-9]*,[a-zA-Z'0-9]*) =" -c $file`
    if [ $count -gt 0 ]; then
        # add fst and second functions
        fstsndadded=1
        printf "$fstsnd" | cat - $file > /tmp/tempfile && mv /tmp/tempfile $file;

        # sed -i "s/\(^[ ]*\)let Pair(\([a-zA-Z'0-9]*\),\([a-zA-Z'0-9]*\)) = \([ a-zA-Z0-9'()]*\) in/\1let \2 = fst (\4) in\n\1let \3 = snd (\4) in/g" $file

        sed -i "s/let Pair(\([^,]*\),\([^,]*\)) = \(.*\) in/match (\3) with\n        | Pair(\1,\2) -> /g" $file
        sed -i "s/let Triple(\([^,]*\),\([^,]*\),\([^,]*\)) = \(.*\) in/match (\4) with\n        | Triple(\1,\2,\3) -> /g" $file
        sed -i "s/let \([a-z0-9]*\),[ ]*\([a-z0-9]*\) = \(.*\) in/match (\3) with\n        | Pair(\1,\2) -> /g" $file
        # sed -i "/^type/! s/s/ (\(.*\),\(.*\))/ Pair(\1,\2)/g" $file

    fi

    # # check for
    # count=`grep -e "let [a-zA-Z'0-9]*, [a-zA-Z'0-9]* =" -c $file`
    # if [ $count -gt 0 ]; then
    #     # add fst and second functions
    #     if [ $fstsndadded -eq 0 ]; then
    #         printf "$fstsnd" | cat - $file > /tmp/tempfile && mv /tmp/tempfile $file;
    #     fi
    #     sed -i "s/\(^[ ]*\)let \([a-zA-Z'0-9]*\), \([a-zA-Z'0-9]*\) = \([ a-zA-Z0-9'()]*\) in/\1let \2 = fst (\4) in\n\1let \3 = snd (\4) in/g" $file
    # fi

    # check for Bool
    countleq=`grep -e "<=" -c $file`
    countgeq=`grep -e ">=" -c $file`
    if [ $countleq -gt 0 ] || [ $countgeq -gt 0 ]; then
        # add fst and second functions
        sed -i "s/(\(.*\)) <=/leqNat (\1)/g" $file
        sed -i "s/(\(.*\)) >=/geqNat (\1)/g" $file
        sed -i "s/(\(.*\)) </ltNat (\1)/g" $file
        sed -i "s/(\(.*\)) >/gtNat (\1)/g" $file

        sed -i "s/\([a-z]\) <=/leqNat \1/g" $file
        sed -i "s/\([a-z]\) >=/geqNat \1/g" $file
        sed -i "s/\([a-z]\) </ltNat \1/g" $file
        sed -i "s/\([a-z]\) >/gtNat \1/g" $file
    fi

    # handle ifs
    # sed -i "s/if \(.*\) then \(.*\) else \(.*\)/ite (\1) (\2) (\3)/g" $file
    sed -i "s/if \(.*\)/ite (\1)/g" $file
    sed -i "s/then \(.*\)/(\1)/g" $file
    sed -i "s/else \(.*\)/(\1)/g" $file


    # add parenthesis for argument in matching constructors
    sed -i "s/\([A-Z][a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) ->/\1(\2) ->/g" $file
    sed -i "s/\([A-Z][a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) ->/\1(\2,\3) ->/g" $file
    sed -i "s/\([A-Z][a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) ->/\1(\2,\3,\4) ->/g" $file
    sed -i "s/\([A-Z][a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) ->/\1(\2,\3,\4,\5) ->/g" $file
    sed -i "s/\([A-Z][a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) ->/\1(\2,\3,\4,\5,\6) ->/g" $file

    # remove weird Pairs
    # sed -i "s/\([a-zA-Z0-9']\+\) Pair(\([a-zA-Z 0-9']\+\),\([a-z A-Z0-9']\+\))/\1(\2,\3)/g" $file
    # sed -i "s/\([a-zA-Z0-9']\+\) Pair(\([a-zA-Z0-9']\+\),\([a-zA-Z0-9']\+\))/\1(\2,\3)/g" $file


    # replace unused variable _ with a name
    sed -i 's/ _ / unused /g' $file

    # remove keywords
    # sed -i 's/begin//g' $file
    # sed -i 's/end//g' $file

    # remove ticks
    sed -i 's/Raml.tick([0-9\.]*)[;]*//g' $file
    sed -i 's/Raml.tick [0-9\.]*[;]*//g' $file
    sed -i 's/:int//g' $file

    # remove first | from type when there
    sed -i ":begin;$!N;$!N;s/type \([a-zA-Z0-9']*\) =[\n ]*|/type \1 =/;tbegin;P;D" $file

    # Add semicolons
    sed -i 's/;;//g' $file        # remove all semicolons
    cat $file > /tmp/tempfile
    printf ";;\n" >> /tmp/tempfile && mv /tmp/tempfile $file

    sed -i ':begin;$!N;$!N;s/\n\nlet/\n;;\nlet/;tbegin;P;D'   $file
    sed -i ':begin;$!N;$!N;s/\n\ntype/\n;;\ntype/;tbegin;P;D' $file
    sed -i ':begin;$!N;$!N;s/;;\n;;/;;\n\n/;tbegin;P;D' $file
    # sed -i ':begin;$!N;$!N;s/ *)\n;;let/;;\n\n/;tbegin;P;D' $file

    # remove type casts
    sed -i "s/(\([a-zA-Z0-9']*\)[ ]*:[ ]*[^),]*)\([^=]*\)=$/\1\2=/g" $file
    sed -i "s/(\([a-zA-Z0-9']*\)[ ]*:[ ]*[^),]*)\([^=]*\)=$/\1\2=/g" $file
    sed -i "s/(\([a-zA-Z0-9']*\)[ ]*:[ ]*[^),]*)\([^=]*\)=$/\1\2=/g" $file

    # lambda functions
    sed -i "s/fun(\([^)]*\))/fun \1/g" $file

    # constructors
    sed -i "s/^\([ \t]*\)| \([A-Z][^ (]*\) \([a-z]*\)/\1| \2(\3)/g" $file

    # replace main
    sed -i "s/^let unused =/let main =/g" $file


    for i in `seq 1 3`; do
        sed -i "s/Cons(\(.*\),\(.*\))Cons(,\(.*\))/(Cons(\1,Cons(\2,\3)))/g" $file
        sed -i "s/\[\([^;]*\)\]/(Cons(\1,Nil))/g" $file                     # [x]
        sed -i "s/\[\([^;[]*\);\([^;[]*\)\]/(Cons(\1,Cons(\2,Nil)))/g" $file # [x,y]
        sed -i "s/\[\([^;[]*\);\([^;[]*\);\([^;[]*\)\]/(Cons(\1,Cons(\2,Cons(\3,Nil))))/g" $file # [x,y,z]
        sed -i "s/\[\([^;[]*\);\([^;[]*\);\([^;[]*\);\([^;[]*\)\]/(Cons(\1,Cons(\2,Cons(\3,Cons(\4,Nil)))))/g" $file # [x,y,z]
        sed -i "s/\[\([^;[]*\);\([^;[]*\);\([^;[]*\);\([^;[]*\);\([^;[]*\)\]/(Cons(\1,Cons(\2,Cons(\3,Cons(\4,Cons(\5,Nil))))))/g" $file # [x,y,z]
        sed -i "s/\[\([^;[]*\);\([^;[]*\);\([^;[]*\);\([^;[]*\);\([^;[]*\);\([^;[]*\)\]/(Cons(\1,Cons(\2,Cons(\3,Cons(\4,Cons(\5,Cons(\6,Nil)))))))/g" $file # [x,y,z]
    done

    # convert nats
    sed -i "s/1\.0/1/g" $file
    sed -i "s/2\.0/2/g" $file
    sed -i "s/3\.0/3/g" $file
    sed -i "s/4\.0/4/g" $file
    sed -i "s/5\.0/5/g" $file
    sed -i "s/6\.0/6/g" $file
    sed -i "s/ 1/ S(0)/g" $file
    sed -i "s/ 2/ S(S(0))/g" $file
    sed -i "s/ 3/ S(S(S(0)))/g" $file
    sed -i "s/ 4/ S(S(S(S(0))))/g" $file
    sed -i "s/ 5/ S(S(S(S(S(0)))))/g" $file
    sed -i "s/ 6/ S(S(S(S(S(S(0))))))/g" $file

    sed -i "s/(1/(S(0)/g" $file
    sed -i "s/(2/(S(S(0))/g" $file
    sed -i "s/(3/(S(S(S(0)))/g" $file
    sed -i "s/(4/(S(S(S(S(0))))/g" $file
    sed -i "s/(5/(S(S(S(S(S(0)))))/g" $file
    sed -i "s/(6/(S(S(S(S(S(S(0))))))/g" $file

    # we ignore negative numbers
    sed -i "s/-1/S(0)/g" $file
    sed -i "s/-2/S(S(0))/g" $file
    sed -i "s/-3/S(S(S(0)))/g" $file
    sed -i "s/-4/S(S(S(S(0))))/g" $file
    sed -i "s/-5/S(S(S(S(S(0)))))/g" $file
    sed -i "s/-6/S(S(S(S(S(S(0))))))/g" $file


    # translate
    # rm -f ${file}.trs
    pcf2trs translate $file
    outcome=$?

    if [ $outcome -eq 0 ]; then
        echo "SUCCESS: $file"
    fi

done
