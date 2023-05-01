#key parameters:
OPENMP=n
PETSC=n
HDF5=n
COMPILER=default
GPU=n
CMD := MPI_Test
ALL:${CMD}
LIB :=
LD_LIB :=

##### inquire the hostname to judge the system name, to determine what target we're compiling on
ifneq (,${HOST})
   SYSTEMS := ${HOST}
  else
   SYSTEMS := $(shell hostname)
endif

##### target machine determines the default compiler, and paths to libraries #####
# edison
ifneq (,$(findstring edison,$(SYSTEMS)))
   ifeq ($(COMPILER),default)
      COMPILER := intel
   endif
   CMP := ftn
endif

# cori
ifneq (,$(findstring cori,$(SYSTEMS)))
   ifeq ($(COMPILER),default)
      COMPILER := intel
   endif
   CMP := ftn
endif

#titan
ifneq (,$(findstring titan,$(SYSTEMS)))
   ifeq ($(COMPILER),default)
     ifeq ($(GPU),y)
        COMPILER := portland
     else
        COMPILER := intel
     endif
   endif
   CMP := ftn
endif   

# sahasrat
ifneq (,$(findstring login,$(SYSTEMS)))
   ifeq ($(COMPILER),default)
      COMPILER := intel
   endif
   CMP := ftn

   export CRAY_CPU_TARGET=x86-64
 
endif

ifeq ($(OPENMP),y)
#   OPT += -qopenmp
endif

ifeq ($(HDF5),y)
#   HDF5_HOME=/home/application/HDF5_P_Intel/1.10.5
   HDF5_HOME=/exports/apps/tarfile/spack/opt/spack/linux-centos7-x86_64_v3/gcc-4.8.5/hdf5-1.12.1-4ckbo7zoe3qkmt5ezre3nwa5ab7bu7aw
#   hdf5-1.12.1-4ckbo7zoe3qkmt5ezre3nwa5ab7bu7aw                      petsc-3.17.0-nltigbp32w7n4exvjcmtezb2jjymunm6
#   hdf5-1.12.1-xzphglbdkpftv3vo72i6tqx446ijcv65
   LIB += -I$(HDF5_HOME)/include -L$(HDF5_HOME)/lib
   OPT += -lhdf5_fortran -lhdf5 -lz -lm
endif

ifeq ($(PETSC),y)
#   PETSC_HOME=/home/application/petsc/3.8.4
   PETSC_HOME=/exports/apps/tarfile/spack/opt/spack/linux-centos7-x86_64_v3/gcc-4.8.5/petsc-3.17.0-nltigbp32w7n4exvjcmtezb2jjymunm6
   LIB += -I$(PETSC_HOME)/include -L$(PETSC_HOME)/lib #-I/home/application/hdf5/1.10.5/include 
   OPT += -lpetsc
endif

CMP := mpifort

# OPT += -fcheck=all
# OPT += -no-wrap-margin

#### compilation ####
OBJ := main.o

$(CMD): $(OBJ)
		$(CMP) -o $(CMD) $(OBJ) $(LIB) $(OPT)
main.o: main.F90
		$(CMP) -c main.F90 $(LIB) $(OPT)

clean:
	rm -f *.o* *.mod MPI_Test *.out *.e*


