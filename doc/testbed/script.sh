
FILES=`find . -name "*.raml"`

intro="type Unit = Unit
;;

type nat = 0 | S of nat
;;

type 'a list = Nil | Cons of 'a * 'a list
;;

type ('a,'b) pair = Pair of 'a * 'b
;;

type 'a option = None | Some of 'a
;;
"


for file in $FILES; do
  # add datatypes
  rm -f /tmp/tempfile
  printf "$intro" | cat - $file > /tmp/tempfile && mv /tmp/tempfile $file;

  # Replace lists
  sed -i "s/\([a-zA-Z0-9']*\)[ ]*::[ ]*\(\(([^)]*)\|[a-zA-Z0-9'][a-zA-Z0-9']*\)\)/Cons(\1,\2)/g" $file
  sed -i "s/\[\]/Nil/g" $file

  # Replace pairs
  sed -i "/^type/! s/\([^a-zA-Z0-9']\)((\[^,)(]*\),\([^(,)]*\))/\1Pair(\2,\3)/g" $file # no constructors

  sed -i "/^type/! s/\([^a-zA-Z0-9']\)((\[^,)(]*\),\([a-zA-Z0-9']*([^)]*)\))/\1Pair(\2,\3)/g" $file # 1 constructors in rhs
  sed -i "/^type/! s/\([^a-zA-Z0-9']\)(\([a-zA-Z0-9']*([^)]*)\),\([^,)(]*\))/\1Pair(\2,\3)/g" $file # 1 constructors in lhs

  # Replace exceptions by types
  sed -i "s/exception Not_found of int \* int/type ('a,'b) exception = Not_found of 'a * 'b;;/g" $file
  sed -i "s/raise[ ]*(Not_found[ ]*\(([^)]*)\))/Not_found\1/g" $file

# sed -e "s/^let [a-zA-Z _=\n .()0-9]*/ASDF/g" avanzini.raml
# sed ':begin;$!N;$!N;s/\n\n/\n;;\n/;tbegin;P;D' avanzini.raml
# sed ':begin;$!N;$!N;s/\n\n/\n;;\n \n/;tbegin;P;D' avanzini.raml
done
