!BOP
! !ROUTINE: RESTART.h
! !INTERFACE:
! include "RESTART.h"
!
! !DESCRIPTION:
! \bv
! *==========================================================*
! | RESTART.h
! | o Holds internal parameters related to restart process
! *==========================================================*
! | Model internal parameters/variables related to writing
! |  or reading pickup for a restart.
! | Note: external parameters (read from parameter file "data")
! |  stay in PARAMS.h and should not appear in RESTART.h ;
! |  therefore, this header file should not be included
! |  in S/R INI_PARMS.
! *==========================================================*
! \ev
!EOP

! Alternating pickup
      INTEGER :: maxNoChkptLev
      PARAMETER ( maxNoChkptLev = 2 )

!--   COMMON / RESTART_I / Integer   valued parameters used for restart
! nCheckLev     :: Holds current checkpoint level (alternating pickup)
! tempStartAB   :: number of previous gT/Temp time levels which are
!                  available to start (or restart) Adams-Bashforth.
! saltStartAB   :: number of previous gS/Salt time levels which are
!                  available to start (or restart) Adams-Bashforth.
! mom_StartAB   :: number of previous gU,gV   time levels which are
!                  available to start (or restart) Adams-Bashforth.
! nHydStartAB   :: number of previous gW      time levels which are
!                  available to start (or restart) Adams-Bashforth.
! qHydStartAB   :: number of previous QH accel. time levels which are
!                  available to start (or restart) Adams-Bashforth.
! dPhiNHstatus  :: status of field dPhiNH: 1= loaded from pickup
!                  0= not available in pickup
      COMMON / RESTART_I /                                                        &
     &      nCheckLev,                                                            &
     &      tempStartAB, saltStartAB,                                             &
     &      mom_StartAB, nHydStartAB, qHydStartAB,                                &
     &      dPhiNHstatus
      INTEGER :: nCheckLev
      INTEGER :: tempStartAB
      INTEGER :: saltStartAB
      INTEGER :: mom_StartAB
      INTEGER :: nHydStartAB
      INTEGER :: qHydStartAB
      INTEGER :: dPhiNHstatus

!--   COMMON / RESTART_C / Character valued parameters used for restart
! checkPtSuff   :: List of checkpoint file suffices
      COMMON / RESTART_C /                                                        &
     &      checkPtSuff
      CHARACTER(len=5) :: checkPtSuff(maxNoChkptLev)
