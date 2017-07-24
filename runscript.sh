#!/bin/bash

# COMMAND="./dist/build/inference-exe/inference-exe $@ "
PROG=".stack-work/dist/x86_64-linux-tinfo6/Cabal-1.24.2.0/build/ara-inference-exe/ara-inference-exe"
COMMAND="$PROG $@ "
TIMES=1
# FOLDERS=" ./doc/examples/"
FOLDERS=" ./doc/testbed/"
# FOLDERS=" ./doc/examples/linear "
# FOLDERS=" ./doc/tpdb_full/"
# FOLDERS=" ./doc/tpdb_full/raML/"
# FOLDERS=" ./doc/tpdb_constrtrs/"
# FOLDERS="./doc/examples/linear/ ./doc/examples/quadratic/ "
# FOLDERS="./doc/examples/linear/ ./doc/examples/quadratic/ ./doc/examples/infeasible/"
# FOLDERS="./doc/examples/tpdb/Frederiksen_Glenstrup/ ./doc/examples/tpdb/Frederiksen_Others/ ./doc/examples/tpdb/hoca/ ./doc/examples/tpdb/raML/ ./doc/examples/tpdb/TCT_12/"

# tct-trs -s "matrices :to 3" --complexity rci doc/examples/quadratic/pairsp.trs
# poly strategie

COLWIDTH=10
TIMEOUT=60

function displayUsage {
    echo -e -n "Usage:\n$0 [arguments to analyse program]"
    echo -e -n "\n\nTo edit the COMMAND, COLUMN WIDTH, FOLDERS, etc. edit the file directly.\n\n"
}

function setColWidth {

    len=$(($COLWIDTH-`echo $new | wc -c`))
    # bef=$(($len/2))
    # aft=$(($len-$(($len/2))))
    aft=1
    if [ $COLWIDTH -lt $len ]
    then bef=0;
    else bef=$(($len-1));
    fi
}

function print {
    new=`echo "$*" | tr ' ' '_'`
    setColWidth $new
    printf "%*s" $bef
    printf "%s" "$*"
    printf "%*s|" $aft
}


function runOnFile {
    time=0.00
    i=0

    while [ $i -lt $TIMES ]
    do
        if [ $i -gt 0 ] && [ $outcome -eq 124 ];
        then print "-";
        else ts=$(date +%s%N);
             timeout $TIMEOUT $COMMAND $1 1>/tmp/runfile 2>/dev/null
             outcome=$?              # save exit code for later usage
             t=$((($(date +%s%N) - $ts)/1000000)) ;

             time=`echo "scale=2; $time+$t" | bc -l`

             print `echo $t | bc`
        fi;
        i=$((i+1));
    done

    # print time
    if [ $outcome -eq 124 ];
    then avg=`echo "scale=1; $time" | bc -l`;
    else avg=`echo "scale=1; $time/$TIMES" | bc -l`
    fi;
    if [ `echo "$avg < 1" | bc` -eq 1 ]; then
        avg=0$avg;
    fi;

    print `echo $avg | bc`

    # print outcome
    if [ $outcome -eq 124 ]; then
        print "Timeout";
    else

        if [ $outcome -eq 0 ]; then
            print "Success";
        else
            print "Failure";
        fi;
    fi;

    print `head -n 1 /tmp/runfile | cut -d' ' -f1`;

}


function header {

    printf "| Example%*s |" $(($1-8))

    for i in $(seq 1 $TIMES); do
        print "$i [ms]"
    done;

    print "Avg"
    print "RetVal"
    print "     Result     "
    echo                        # new line

    echo `printf "|%*s|" $(($1+40+$(($TIMES*$(($COLWIDTH+1)))))) | tr ' ' '-'`

}


# check whether user had supplied -h or --help . If yes display usage
for p in $@; do
    if [[ ( $p == "--help") ||  $p == "-h" ]]
    then
        displayUsage
        exit 0
    fi;
done;


if [ ! -e $PROG ]; then
    echo "Fatal Error: File $PROG not found! Did you compile the program yet?";
    exit 1;
fi;


for fld in $FOLDERS; do
    # FILES=`find $fld -maxdepth 1 -iname "*.trs" | sort`
    FILES=`find $fld -iname "*.trs" | sort`
    maxlen=0

    # get longest basename
    for f in $FILES; do
        bn=`basename "$f"`
        if [ `echo $bn | wc -c` -gt $maxlen ]; then
            maxlen=`echo $bn | wc -c`;
        fi;
    done;

    printf "%s\n------------------------------\n\n" "Folder: $fld"

    header $maxlen

    for f in $FILES; do
        # find out length of space to be added to filename
        bn=`basename "$f"`
        len=`echo $(basename "$f") | wc -c`
        len=$(($maxlen-len))

        # generate string to add to filename
        add=`printf "%*s" $len`

        # run command, print table
        printf "%s" "| $bn$add |";
        runOnFile $f;
        echo
    done;
    printf "\n\n"
done;
