#!/bin/bash -

theory=$1;
timeout=$2;

ulimit -t $2;

eprove $1 | epclextract;
