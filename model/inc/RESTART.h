C $Header: /u/gcmpack/MITgcm/model/inc/RESTART.h,v 1.1 2007/10/19 14:34:13 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: RESTART.h
C     !INTERFACE:
C     include "RESTART.h"
C
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | RESTART.h
C     | o Holds internal parameters related to restart process
C     *==========================================================*
C     | Model internal parameters/variables related to writing
C     |  or reading pickup for a restart.
C     | Note: external parameters (read from parameter file "data")
C     |  stay in PARAMS.h and should not appear in RESTART.h ;
C     |  therefore, this header file should not be included
C     |  in S/R INI_PARMS.
C     *==========================================================*
C     \ev
CEOP

C     Alternating pickup
      INTEGER maxNoChkptLev
      PARAMETER ( maxNoChkptLev = 2 )

C--   COMMON / RESTART_I / Integer   valued parameters used for restart
C     nCheckLev     :: Holds current checkpoint level (alternating pickup)
C     tempStartAB   :: number of previous time level gT/Temp that are
C                      available to start (or restart) Adams-Bashforth
C     saltStartAB   :: number of previous time level gS/Salt that are
C                      available to start (or restart) Adams-Bashforth
C     mom_StartAB   :: number of previous time level gU,gV   that are
C                      available to start (or restart) Adams-Bashforth
C     nHydStartAB   :: number of previous time level gW      that are
C                      available to start (or restart) Adams-Bashforth
      COMMON / RESTART_I /
     &  nCheckLev,
     &  tempStartAB, saltStartAB,
     &  mom_StartAB, nHydStartAB
      INTEGER nCheckLev
      INTEGER tempStartAB
      INTEGER saltStartAB
      INTEGER mom_StartAB
      INTEGER nHydStartAB

C--   COMMON / RESTART_C / Character valued parameters used for restart
C     checkPtSuff   :: List of checkpoint file suffices
      COMMON / RESTART_C /
     &  checkPtSuff
      CHARACTER*(5) checkPtSuff(maxNoChkptLev)
