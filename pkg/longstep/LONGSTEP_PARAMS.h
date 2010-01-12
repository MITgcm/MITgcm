C $Header: /u/gcmpack/MITgcm/pkg/longstep/LONGSTEP_PARAMS.h,v 1.2 2010/01/12 23:55:48 jahn Exp $
C $Name:  $

#ifdef ALLOW_LONGSTEP

CBOP
C    !ROUTINE: LONGSTEP_PARAMS.h
C    !INTERFACE:
C #include LONGSTEP_PARAMS.h

C    !DESCRIPTION:
C Contains parameters for long tracer time step.

CEOP

C     COMMON /LONGSTEP_PARAMS/  LONGSTEP parameters:
C     LS_nIter        :: number of dynamics time steps between ptracer steps
C     LS_whenToSample :: when to sample dynamical fields for the longstep average
C                        0 - at beginning of timestep (reproduces offline results)
C                        1 - after first THERMODYNAMICS but before DYNAMICS
C                            (use use old U,V,W for advection, but new T,S for GCHEM if
C                            staggerTimeStep=.FALSE.; reproduces online with
C                            staggerTimeStep=.FALSE. for LS_nIter=1)
C                        2 - after DYNAMICS and second THERMODYNAMICS
C                            (use new U,V,W and T,S; reproduces online with
C                            staggerTimeStep=.TRUE. for LS_nIter=1)

      INTEGER LS_nIter, LS_whenToSample
      LOGICAL LS_usePmEpR
      COMMON /LONGSTEP_PARAMS/ LS_nIter, LS_whenToSample, LS_usePmEpR

#endif /* ALLOW_LONGSTEP */

