C $Header: /u/gcmpack/MITgcm/pkg/ptracers/PTRACERS_START.h,v 1.1 2012/03/08 17:05:44 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: PTRACERS_START.h
C     !INTERFACE:
C     include "PTRACERS_START.h"
C
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | PTRACERS_START.h
C     | o Holds passive-tracer internal parameters related
C     |   to start and restart process
C     *==========================================================*
C     | Passive-tracer internal parameters/variables related to
C     | a) which tracer is stepped forward
C     | b) writing or reading pickup for a restart.
C     | Note:
C     |  external parameters (read from parameter file "data.ptracers")
C     |  stay in PTRACERS_PARAMS.h and should not appear here ;
C     |  therefore, this header file should not be included
C     |  in S/R PTRACERS_READPARMS.
C     *==========================================================*
C     \ev
CEOP

C--   COMMON / PTRACERS_START_I / Integer valued parameters used for (re)start
C     PTRACERS_StartAB  :: number of gPtr previous time levels that are
C                      available to start (or restart) Adams-Bashforth
      COMMON / PTRACERS_START_I /
     &  PTRACERS_StartAB
      INTEGER PTRACERS_StartAB(PTRACERS_num)

C--   COMMON / PTRACERS_START_L / Logical valued parameters used for (re)start
C     PTRACERS_StepFwd  :: switch on/off this tracer time-stepping
      COMMON / PTRACERS_START_L /
     &  PTRACERS_StepFwd
      LOGICAL PTRACERS_StepFwd(PTRACERS_num)

