# makefile includes for hexagon, gnu compilers
# Standard compilers and linkers
CF90= f95
CC= cc
LD= f95

# CPP, used internally by compilers - note that some compilers
# need special options for this, consult compiler manual if in trouble
CPP= /usr/bin/cpp -P -traditional

# Flags for fixed/free format
F90FLG= -ffree-form 

# Compiler flags, real8 version
FFLAGS= -O2 -fconvert=big-endian
CFLAGS= -O2

# Linker flags
LINKFLAGS= $(FFLAGS)  

#Netcdf, FFTW and lapack Libraries
INCLUDE=
LIBS =

# Some fortran compilers have iargc as built in, 
# others as library routine
CPPFLAGS=
