

FILES=`find . -type f -name "take_lazy.fp.raml"`

for f in $FILES; do

    echo $f
    sed -i "s/NilL/[]/g" $f
    sed -i "s/ConsL(\([^,]*\),\([^)]*\))/\1::\2/g" $f
    sed -i "s/NilF/[]/g" $f
    sed -i "s/ConsF(\([^,]*\),\([^)]*\))/\1::\2/g" $f
    sed -i "s/Nil/[]/g" $f
    sed -i "s/Cons(\([^,]*\),\([^)]*\))/\1::\2/g" $f


    sed -i "s/ 0/ Zero/g" $f
    sed -i "s/S(0)/S(Zero)/g" $f

    sed -i "s/True/true/g" $f
    sed -i "s/False/false/g" $f

    sed -i "s/Pair//g" $f
    sed -i "s/Triple//g" $f


done

exit

type nat = Zero | S of nat;;
type unit = Unit ;;
