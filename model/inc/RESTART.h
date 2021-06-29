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
C     tempStartAB   :: number of previous gT/Temp time levels which are
C                      available to start (or restart) Adams-Bashforth.
C     saltStartAB   :: number of previous gS/Salt time levels which are
C                      available to start (or restart) Adams-Bashforth.
C     mom_StartAB   :: number of previous gU,gV   time levels which are
C                      available to start (or restart) Adams-Bashforth.
C     nHydStartAB   :: number of previous gW      time levels which are
C                      available to start (or restart) Adams-Bashforth.
C     qHydStartAB   :: number of previous QH accel. time levels which are
C                      available to start (or restart) Adams-Bashforth.
C     dPhiNHstatus  :: status of field dPhiNH: 1= loaded from pickup
C                      0= not available in pickup
      COMMON / RESTART_I /
     &  nCheckLev,
     &  tempStartAB, saltStartAB,
     &  mom_StartAB, nHydStartAB, qHydStartAB,
     &  dPhiNHstatus
      INTEGER nCheckLev
      INTEGER tempStartAB
      INTEGER saltStartAB
      INTEGER mom_StartAB
      INTEGER nHydStartAB
      INTEGER qHydStartAB
      INTEGER dPhiNHstatus

C--   COMMON / RESTART_C / Character valued parameters used for restart
C     checkPtSuff   :: List of checkpoint file suffices
      COMMON / RESTART_C /
     &  checkPtSuff
      CHARACTER*(5) checkPtSuff(maxNoChkptLev)
