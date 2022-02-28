#!/bin/bash

nens=10     # number of ensemble menbers
perr=30     # size of perturbation relative to default value [%]
styp="NORM" # type of distribution [only Gaussian supported for now]

#--- BGC parameters

exec=./init_param_bio

# generate new parameter files

echo "./init_params_bio $nens $perr $styp"
$exec $nens $perr $styp
