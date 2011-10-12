#!/bin/bash -

if [ -z $1 ]; then
    echo "Usage: run-vampire.sh <FILENAME>";
    exit 2;
fi

theory=$1;

if [ ! -e $theory ]; then
    echo "Theory file ($theory) doesn't exist";
    exit 1;
fi

if [ ! -r $theory ]; then
    echo "Theory file ($theory) is unreadable";
    exit 1;
fi

# Go to the directory where $theory is found
dir=`dirname $theory`;
cd $dir; # we could do some sanity checking before doing this

theory_text=`cat -n $theory`;
now=`date`;
me=`whoami`;

echo "% Calling vampire at";
echo "% ";
echo "% ";
echo "%   $now"; 
echo "% ";
echo "% ";
echo "% as";
echo "% ";
echo "% ";
echo "%   $me";
echo "% ";
echo "% ";
echo "% on";
echo "% ";
echo "% ";
echo "%   $theory,";
echo "% ";
echo "% ";
echo "% which is:";
echo "% ";
echo "% ";
echo "% ========================================================================================================================";
echo "$theory_text" | sed -e 's/^/% /';
echo "% ========================================================================================================================";
echo "% Results:";

proof=`vampire -t 300 < $theory 2>&1 | cat -n`;

if [ ! "$?" -eq '0' ]; then
    echo "% Vampire did not terminate cleanly; its gave this as output:";
    echo "% ======================================================================";
    echo "$proof" | sed -e 's/^/% /';
    exit 1;
fi

echo "$proof" | sed -e 's/^/% /';

exit 0;
