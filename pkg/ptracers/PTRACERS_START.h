!BOP
! !ROUTINE: PTRACERS_START.h
! !INTERFACE:
! include "PTRACERS_START.h"
!
! !DESCRIPTION:
! \bv
! *==========================================================*
! | PTRACERS_START.h
! | o Holds passive-tracer internal parameters related
! |   to start and restart process
! *==========================================================*
! | Passive-tracer internal parameters/variables related to
! | a) which tracer is stepped forward
! | b) writing or reading pickup for a restart.
! | Note:
! |  external parameters (read from parameter file "data.ptracers")
! |  stay in PTRACERS_PARAMS.h and should not appear here ;
! |  therefore, this header file should not be included
! |  in S/R PTRACERS_READPARMS.
! *==========================================================*
! \ev
!EOP

!--   COMMON / PTRACERS_START_I / Integer valued parameters used for (re)start
! PTRACERS_StartAB  :: number of gPtr previous time levels that are
!                  available to start (or restart) Adams-Bashforth
      COMMON / PTRACERS_START_I /                                                 &
     &      PTRACERS_StartAB
      INTEGER :: PTRACERS_StartAB(PTRACERS_num)

!--   COMMON / PTRACERS_START_L / Logical valued parameters used for (re)start
! PTRACERS_StepFwd  :: switch on/off this tracer time-stepping
      COMMON / PTRACERS_START_L /                                                 &
     &      PTRACERS_StepFwd
      LOGICAL :: PTRACERS_StepFwd(PTRACERS_num)

