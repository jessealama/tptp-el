#!/bin/bash -

theory=$1;
timeout=${2:"30"};

if [ -z $theory ]; then
    echo "Usage: `basename $0` THEORY [TIMEOUT]";
    exit 1;
fi

ulimit -t $timeout;

eprove $theory | epclextract;
