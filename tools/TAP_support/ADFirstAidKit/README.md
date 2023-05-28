Contents of the ADFirstAidKit
=============================

## adStack.c && adStack.h
  Mechanism for the main stack used by codes produced by Tapenade in reverse mode.
  Use these files regardless of the language of the reverse-differentiated code.
  For OpenMP compatibility, compile with option -fopenmp.

## adBinomial.c && adBinomial.h
  Primitives for Tapenade adjoint with binomial checkpoiting (cf Griewank-Walther
  "revolve" method). Triggered by Tapenade directive $AD BINOMIAL-CKP.

## (adContext.c || adContextMPI.c) && adContext.h
  Primitives used by Tapenade-differentiated code with automated validation.
  Triggered by tapenade command-line option -context.
  Use adContextMPI.c instead of adContext.c on MPI code.

## adContextCPX.c && adContextCPX.h
  Same as adContext, but to validate code differentiated with the command-line
  option -complexstep, which is an attempt to automate differentiation
  via the complex-step method.

## (adDebug.c || adDebugMPI.c) && adDebug.h
  Primitives used by Tapenade-differentiated code with automated debugging.
  Triggered by tapenade command-line option -context combined either with
  -debugTGT or with -debugADJ.
  Use adDebugMPI.c instead of adDebug.c on MPI code.

## adComplex.h
  Utility for adStack, adContext, adDebug

## admm.c && admm.h (might be integrated later into adBuffer)
  The "ADMM" primitives, which manage push/pop of pointers when the pointer's
  destination lies in memory chunks that have been deallocated and reallocated.

## admm_tapenade_interface.f90
  A Fortran90 module that must be USE'd by Tapenade-generated adjoint code,
  if these codes need push/pop of pointers or use ADMM primitives.

## adProfile.c && adProfile.h (Currently broken, sorry)
  Primitives used by Tapenade adjoint to measure the cost/benefit of checkpointing,
  to find parts of the code that should or shouldn't be checkpointed.

## fortranSupport.F && ampiSupport.c
  Files needed by AMPI, for Tapenade adjoint code of MPI programs.

## ampi
  Directory containing the include files needed by code using the "adjoinable mpi" library.
  This is primarily intended for some nonRegression tests. This is not the ampi install.

## PUSHPOPGeneralLib && PUSHPOPDiff.f
  Useful for repeated differentiation forward-on-reverse. Specification of the
  activity behavior of push/pop primitives + implementation of the forward
  derivatives of these primitives.

## testMemSizef.f && testMemSizec.c
  Short standalone code that tries to measure the size of primitive types in bytes.

## testpushpop.sh && testpushpop.f90
  Short standalone code to test push/pop primitives of adStack.c

## validityTest.f (Poorly tested, sorry)
  Primitives used by Tapenade tangent code to estimate how close the execution
  comes to non-differentiable behavior (e.g. tests).
  Triggered by tapenade command-line option -directValid.

## OLDadBuffer.c && OLDadBuffer.h && OLDadBuffer.f && OLDadStack.c && OLDadStack.h
  Old version of the stack mechanism used by reverse AD codes. The new adStack.c && adStack.h do not need adBuffer any more.
  With gfortran 10.2.1, use -fallow-argument-mismatch option to compile OLDadBuffer.f.

Notice that, in order to run some nonRegression tests, you may have to place manually into this ADFirstAidKit directory two symbolic links: one named "mpich" pointing to the include directory of your mpich insall, and another one named "openmpi" pointing to the include directory of your openmpi install.
