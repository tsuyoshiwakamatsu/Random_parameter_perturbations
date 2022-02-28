#!/bin/bash
#
# Note: Parameters_bio_mem##.txt files should be in the same directory
#

nens=10     # number of ensemble members
perr=10     # size of perturbation relative to prescribed range of value [%]
styp="NORM" # type of distribution [only Gaussian supported for now]

exec=./ptrb_param_bio

# rewrite parameter files

echo "./ptrb_params_bio $nens $perr $styp"
$exec $nens $perr $styp
