# Random_parameter_perturbations

Generate ensemble parameter files from a set of prescribed default parameter values and re-perturb existing ensemble parameter files based on a set of prescribed range of parameter values.

# Installation

At command line:

```
make
```

and you see two fortran executables:

```
init_param_bio
ptrb_param_bio
```

# Usage

To generate ensemble parameter files for the first time:

```
bash init_param_bio.sh
```

To re-perturb ensemble parameter files:

```
bash ptrb_param_bio.sh
```

For further information, read comments in:

```
init_param_bio.sh
init_param_bio.F90
ptrb_param_bio.sh
ptrb_param_bio.F90
```
