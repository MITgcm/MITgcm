C $Header: /u/gcmpack/MITgcm/pkg/longstep/LONGSTEP_PARAMS.h,v 1.1 2009/06/26 23:10:09 jahn Exp $
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
C     LS_nIter    :: number of dynamics time steps between ptracer steps
C     LS_afterTS         :: use T,S at end of time step (after thermodynamics)
C     LS_staggerTimeStep :: use U,V,W,T,S at end of time step
C     (default: U,V,W,T,S at beginning of time step, i.e. like offline)

      INTEGER LS_nIter
      LOGICAL LS_staggerTimeStep
      LOGICAL LS_afterTS
      COMMON /LONGSTEP_PARAMS/ LS_nIter, LS_staggerTimeStep,
     &                         LS_afterTS

#endif /* ALLOW_LONGSTEP */

