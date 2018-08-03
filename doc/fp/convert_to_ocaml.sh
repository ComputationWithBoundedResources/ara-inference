
# function for adding datatypes
addDt() {
    file="$1"
    str="$3"
    count=`grep "$2" -c $file`
    if [ $count -eq 0 ]; then
        printf "$str" | cat - $file > /tmp/tempfile && mv /tmp/tempfile $file;
    fi
}


intro="
type nat = 0 | S of nat
;;
type Unit = Unit
;;
type ('a,'b) pair = Pair of 'a * 'b
;;
type ('a,'b,'c) triple = Triple of 'a * 'b * 'c
;;
let ifz n th el = match n with
   | 0 -> th 0
   | S(x) -> el x
;;
let minus n m =
  let rec minus' m n = match m with
        | 0 -> 0
        | S(x) -> match n with
          | 0 -> m
          | S(y) -> minus' x y
  in Pair(minus' n m,m)
;;
let rec plus n m = match m with
  | 0 -> n
  | S(x) -> S(plus n x)
;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)

;;

let rec linear n =
  ifz n
    (fun x -> x)
    (fun n' ->

       linear n'
    )

"

FILES=`find . -name '*.raml'`

for fn in $FILES; do
    f=${fn}.fp
    t=${fn}.tmp
    echo "FILE: $f"
    echo "----------------------------------------"
    rm -f $f                    # delete old files
    rm -f $t
    cp $fn $t                   # copy to tmp

    sed -i "s/Rnat.//g" $t
    sed -i "s/Raml.//g" $t
    sed -i "s/tick [0-9].[0-9] in//g" $t
    sed -i "s/^let/;;\n\nlet/g" $t
    sed -i "s/^type/;;\n\ntype/g" $t
    sed -i "s/fun () -> ()/fun x -> x/g" $t
    sed -i "s/let ()/let unused/g" $t
    sed -i "s/^[ ]*let unused =[ ]*$//g" $t
    sed -i "s/let () =//g" $t   # unused function result
    sed -i "s/_,/unused,/g" $t
    sed -i "s/,_/,unused/g" $t
    sed -i "s/let (\([^,]*\),\([^,]*\)) = \(.*\) in/match (\3) with\n        | Pair(\1,\2) -> /g" $t
    sed -i "s/let (\([^,]*\),\([^,]*\),\([^,]*\)) = \(.*\) in/match (\4) with\n        | Triple(\1,\2,\3) -> /g" $t
    sed -i "s/fun () ->/fun unused ->/g" $t
    sed -i "s/^[ ]*let unused = ; in ()/\n;;\n/g" $t
    sed -i "s/zero/0/g" $t
    sed -i "s/raise [a-zA-Z_]*/error/g" $t
    sed -i "s/succ[ ]*(\(.*\))/S(\1)/g" $t
    sed -i "s/^\( [ ]*\)\(.*\);/\1let unused = \2 in/g" $t
    sed -i "s/[ ]*let unused = ; in ()/;;/g" $t

    printf "$intro" | cat  > $f
    cat $t >> $f
    rm $t

    pcf2trs translate $f


    # mv $f $f.fp
done
