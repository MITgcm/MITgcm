Contents of the ADFirstAidKit
=============================

## adStack.c && adStack.h
  Mechanism for the main stack used by codes produced by Tapenade in reverse mode.
  Use these files regardless of the language of the reverse-differentiated code.

## adBuffer.c && adBuffer.h
  Mechanism for the stack buffers (one per data type) used by reverse AD codes.
  Use these files when the reverse-differentiated code is in C.

## adBuffer.f
  Mechanism for the stack buffers (one per data type) used by reverse AD codes.
  Use these files when the reverse-differentiated code is in Fortran.

## adBinomial.c && adBinomial.h
  Primitives for Tapenade adjoint with binomial checkpoiting (cf Griewank-Walther
  "revolve" method). Triggered by Tapenade directive $AD BINOMIAL-CKP.

## (adContext.c || adContextMPI.c) && adContext.h
  Primitives used by Tapenade-differentiated code with automated validation.
  Triggered by tapenade command-line option -context.
  Use adContextMPI.c instead of adContext.c on MPI code.

## (adDebug.c || adDebugMPI.c) && adDebug.h
  Primitives used by Tapenade-differentiated code with automated debugging.
  Triggered by tapenade command-line option -context combined either with
  -debugTGT or with -debugADJ.
  Use adDebugMPI.c instead of adDebug.c on MPI code.

## admm.c && admm.h (might be integrated later into adBuffer)
  The "ADMM" primitives, which manage push/pop of pointers when the pointer's
  destination lies in memory chunks that have been deallocated and reallocated.

## adProfile.c && adProfile.h (Currently broken, sorry)
  Primitives used by Tapenade adjoint to measure the cost/benefit of checkpointing,
  to find parts of the code that should or shouldn't be checkpointed.

## admm_tapenade_interface.f90
  A Fortran90 module that must be USE'd by Tapenade-generated adjoint code,
  if these codes need push/pop of pointers or use ADMM primitives.

## fortranSupport.F && ampiSupport.c
  Files needed by AMPI, for Tapenade adjoint code of MPI programs.

## PUSHPOPGeneralLib && PUSHPOPDiff.f
  Useful for repeated differentiation forward-on-reverse. Specification of the
  activity behavior of push/pop primitives + implementation of the forward
  derivatives of these primitives.

## testMemSizef.f && testMemSizec.c
  Short standalone code that tries to measure the size of primitive types in bytes.

## validityTest.f (Poorly tested, sorry)
  Primitives used by Tapenade tangent code to estimate how close the execution
  comes to non-differentiable behavior (e.g. tests).
  Triggered by tapenade command-line option -directValid.
