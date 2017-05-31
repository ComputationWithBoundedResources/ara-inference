
FILES=`find . -name "*.raml"`

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

fstsnd="
let fst x =
  match x with
  | Pair(a,b) -> a

let snd x =
  match x with
  | Pair(a,b) -> b

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

for file in $FILES; do
  # add datatypes
  rm -f /tmp/tempfile

  # add datatypes
  addDt "$file" "Unit" "$dtUnit"
  addDt "$file" "nat"  "$dtNat"
  addDt "$file" "Cons" "$dtList"
  addDt "$file" "Pair" "$dtPair"
  addDt "$file" "Some" "$dtSome"
  # printf "$intro" | cat - $file > /tmp/tempfile && mv /tmp/tempfile $file;

  # replace trailing whitespaces
  sed -i 's/[ \t]*$//' $file


  # Replace lists
  sed -i "s/\([a-zA-Z0-9']*\)[ ]*::[ ]*\(\(([^)]*)\|[a-zA-Z0-9'][a-zA-Z0-9']*\)\)/Cons(\1,\2)/g" $file
  sed -i "s/\[\]/Nil/g" $file

  # Replace pairs
  sed -i "/^type/! s/\([^a-zA-Z0-9']\)(\([^,)(]*\),\([^(,)]*\))/\1Pair(\2,\3)/g" $file # no constructors

  sed -i "/^type/! s/\([^a-zA-Z0-9']\)(\([^,)(]*\),\([a-zA-Z0-9']*([^)]*)\))/\1Pair(\2,\3)/g" $file # 1 constructors in rhs
  sed -i "/^type/! s/\([^a-zA-Z0-9']\)(\([a-zA-Z0-9']*([^)]*)\),\([^,)(]*\))/\1Pair(\2,\3)/g" $file # 1 constructors in lhs

  # Replace exceptions by types
  sed -i "s/exception Not_found of int \* int/type ('a,'b) exception = Not_found of 'a * 'b;;/g" $file
  sed -i "s/raise[ ]*(Not_found[ ]*\(([^)]*)\))/Not_found\1/g" $file

  # replace = function with match ... with
  sed -i "s/ \([a-zA-Z']*\) = function/\1 = match \1 with/g" $file

  # Check if there is a pattern match on Pair within a function using let
  fstsndadded=0
  count=`grep -e "let Pair([a-zA-Z'0-9]*,[a-zA-Z'0-9]*) =" -c $file`
  if [ $count -gt 0 ]; then
      # add fst and second functions
      fstsndadded=1
      printf "$fstsnd" | cat - $file > /tmp/tempfile && mv /tmp/tempfile $file;

      sed -i "s/\(^[ ]*\)let Pair(\([a-zA-Z'0-9]*\),\([a-zA-Z'0-9]*\)) = \([ a-zA-Z0-9'()]*\) in/\1let \2 = fst (\4) in\n\1let \3 = snd (\4) in/g" $file

  fi

  # check for
  count=`grep -e "let [a-zA-Z'0-9]*, [a-zA-Z'0-9]* =" -c $file`
  if [ $count -gt 0 ]; then
      # add fst and second functions
      if [ $fstsndadded -eq 0 ]; then
          printf "$fstsnd" | cat - $file > /tmp/tempfile && mv /tmp/tempfile $file;
      fi
      sed -i "s/\(^[ ]*\)let \([a-zA-Z'0-9]*\), \([a-zA-Z'0-9]*\) = \([ a-zA-Z0-9'()]*\) in/\1let \2 = fst (\4) in\n\1let \3 = snd (\4) in/g" $file
  fi

  # add parenthesis for argument in matching constructors
  sed -i "s/\([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) ->/\1(\2) ->/g" $file
  sed -i "s/\([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) ->/\1(\2,\3) ->/g" $file
  sed -i "s/\([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) ->/\1(\2,\3,\4) ->/g" $file
  sed -i "s/\([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) ->/\1(\2,\3,\4,\5) ->/g" $file
  sed -i "s/\([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) \([a-zA-Z0-9']\+\) ->/\1(\2,\3,\4,\5,\6) ->/g" $file

  # remove weird Pairs
  sed -i "s/\([a-zA-Z0-9']\+\) Pair(\([a-zA-Z 0-9']\+\),\([a-z A-Z0-9']\+\))/\1(\2,\3)/g" $file
  # sed -i "s/\([a-zA-Z0-9']\+\) Pair(\([a-zA-Z0-9']\+\),\([a-zA-Z0-9']\+\))/\1(\2,\3)/g" $file


  # replace unused variable _ with a name
  sed -i 's/ _ / unused /g' $file

  # remove keywords
  sed -i 's/begin//g' $file
  sed -i 's/end//g' $file

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

  # translate
  rm ${file}.trs 2>/dev/null
  pcf2trs translate $file > "${file}.trs"
  outcome=$?

  if [ $outcome -eq 0 ]; then
      echo "SUCCESS: $file"
  fi

done
