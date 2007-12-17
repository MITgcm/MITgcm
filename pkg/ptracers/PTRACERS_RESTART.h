C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/PTRACERS_RESTART.h,v 1.1 2007/12/17 22:03:15 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: PTRACERS_RESTART.h
C     !INTERFACE:
C     include "PTRACERS_RESTART.h"
C
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | PTRACERS_RESTART.h
C     | o Holds passive-tracer internal parameters related
C     |   to restart process
C     *==========================================================*
C     | Passive-tracer internal parameters/variables related to
C     |  writing or reading pickup for a restart.
C     | Note:
C     |  external parameters (read from parameter file "data.ptracers")
C     |  stay in PTRACERS_PARAMS.h and should not appear here ;
C     |  therefore, this header file should not be included
C     |  in S/R PTRACERS_READPARMS.
C     *==========================================================*
C     \ev
CEOP

C--   COMMON / PTRACERS_RESTART_I / Integer valued parameters used for restart
C     PTRACERS_StartAB  :: number of gPtr previous time levels that are
C                      available to start (or restart) Adams-Bashforth
      COMMON / PTRACERS_RESTART_I /
     &  PTRACERS_StartAB
      INTEGER PTRACERS_StartAB(PTRACERS_num)

