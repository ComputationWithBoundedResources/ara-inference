#!/bin/bash

FOLDERS="./Runtime_Complexity_Innermost_Rewriting/Various_04/"
# FOLDERS="./Runtime_Complexity_Innermost_Rewriting/"
XSL=./xml/xtc2tpdb.xsl

# ./Runtime_Complexity_Innermost_Rewriting/AG01/
# ./Runtime_Complexity_Innermost_Rewriting/AProVE_04/
# ./Runtime_Complexity_Innermost_Rewriting/AProVE_06/
# ./Runtime_Complexity_Innermost_Rewriting/AProVE_07/
# ./Runtime_Complexity_Innermost_Rewriting/AProVE_08/
# ./Runtime_Complexity_Innermost_Rewriting/AProVE_09_Inductive/
# ./Runtime_Complexity_Innermost_Rewriting/Beerendonk_07/
# ./Runtime_Complexity_Innermost_Rewriting/CiME_04/
# ./Runtime_Complexity_Innermost_Rewriting/Der95/
# ./Runtime_Complexity_Innermost_Rewriting/Endrullis_06/
# ./Runtime_Complexity_Innermost_Rewriting/Frederiksen_Glenstrup/
# ./Runtime_Complexity_Innermost_Rewriting/Frederiksen_Others/
# ./Runtime_Complexity_Innermost_Rewriting/GTSSK07/
# ./Runtime_Complexity_Innermost_Rewriting/HirokawaMiddeldorp_04/
# ./Runtime_Complexity_Innermost_Rewriting/hoca/
# ./Runtime_Complexity_Innermost_Rewriting/Mixed_TRS/
# ./Runtime_Complexity_Innermost_Rewriting/raML/
# ./Runtime_Complexity_Innermost_Rewriting/Rubio_04/
# ./Runtime_Complexity_Innermost_Rewriting/Secret_05_TRS/
# ./Runtime_Complexity_Innermost_Rewriting/Secret_06_TRS/
# ./Runtime_Complexity_Innermost_Rewriting/Secret_07_TRS/
# ./Runtime_Complexity_Innermost_Rewriting/SK90/
# ./Runtime_Complexity_Innermost_Rewriting/Strategy_removed_AG01/
# ./Runtime_Complexity_Innermost_Rewriting/Strategy_removed_CSR_05/
# ./Runtime_Complexity_Innermost_Rewriting/Strategy_removed_mixed_05/
# ./Runtime_Complexity_Innermost_Rewriting/TCT_12/
# ./Runtime_Complexity_Innermost_Rewriting/Transformed_CSR_04/
# ./Runtime_Complexity_Innermost_Rewriting/Various_04/
# ./Runtime_Complexity_Innermost_Rewriting/Waldmann_06/
# ./Runtime_Complexity_Innermost_Rewriting/Zantema_05/


for fld in $FOLDERS; do
    FILES=`find $fld -iname "*.xml" | sort`
    for f in $FILES; do
        xsltproc $XSL $f > ${f%.xml}.trs;
    done;
done;
