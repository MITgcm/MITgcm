C $Header: /u/gcmpack/MITgcm/pkg/runclock/RUNCLOCK.h,v 1.1 2005/05/31 18:24:30 adcroft Exp $
C $Name:  $

#ifdef ALLOW_RUNCLOCK

C     Package flag
      logical RUNCLOCKisON
      COMMON /RC_PACKAGE/ RUNCLOCKisON

C     RUNCLOCK parameters
      INTEGER RC_maxtime_hr, RC_maxtime_mi, RC_maxtime_sc
      COMMON /RC_PARAMS/
     &    RC_maxtime_hr, RC_maxtime_mi, RC_maxtime_sc

C     RUNCLOCK internal state
      Real*8  RC_start_tins,RC_prev_tins
      INTEGER RC_start_yr, RC_start_mo, RC_start_dy
      INTEGER RC_start_hr, RC_start_mi, RC_start_sc
      COMMON /RC_INTERNAL/
     &    RC_start_tins,RC_prev_tins,
     &    RC_start_yr, RC_start_mo, RC_start_dy,
     &    RC_start_hr, RC_start_mi, RC_start_sc

#endif /* ALLOW_RUNCLCOK */
