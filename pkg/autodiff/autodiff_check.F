C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_check.F,v 1.7 2007/10/19 23:52:43 heimbach Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "AUTODIFF_OPTIONS.h"

CBOP
C     !ROUTINE: AUTODIFF_CHECK
C     !INTERFACE:
      SUBROUTINE AUTODIFF_CHECK( myThid )

C     !DESCRIPTION: \bv
C     \ev

      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF
#include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid -  Number of this instances
      INTEGER myThid
CEOP

#ifdef ALLOW_AUTODIFF_TAMC

#if (defined (AUTODIFF_2_LEVEL_CHECKPOINT))
      if (nchklev_1*nchklev_2 .lt. nTimeSteps) then
        print*, ' the_main_loop: TAMC checkpointing parameters'
        print*, '     nchklev_1*nchklev_2 = ',
     &       nchklev_1*nchklev_2
        print*, '            are not consistent with nTimeSteps = ',
     &       nTimeSteps
        stop    ' ... stopped in autodiff_check'
      endif
#elif (defined (AUTODIFF_4_LEVEL_CHECKPOINT))
      if (nchklev_1*nchklev_2*nchklev_3*nchklev_4 .lt. nTimeSteps) then
        print*, ' the_main_loop: TAMC checkpointing parameters'
        print*, '     nchklev_1*nchklev_2*nchklev_3*nchklev_4 = ',
     &       nchklev_1*nchklev_2*nchklev_3*nchklev_4
        print*, '            are not consistent with nTimeSteps = ',
     &       nTimeSteps
        stop    ' ... stopped in autodiff_check'
      endif
#else
c--   Check the choice of the checkpointing parameters in relation
c--   to nTimeSteps: (nchklev_1*nchklev_2*nchklev_3 .ge. nTimeSteps)
      if (nchklev_1*nchklev_2*nchklev_3 .lt. nTimeSteps) then
        print*, ' the_main_loop: TAMC checkpointing parameters'
        print*, '     nchklev_1*nchklev_2*nchklev_3 = ',
     &       nchklev_1*nchklev_2*nchklev_3
        print*, '            are not consistent with nTimeSteps = ',
     &       nTimeSteps
        stop    ' ... stopped in autodiff_check'
      endif
#endif

#ifndef ALLOW_AUTODIFF_MONITOR
       IF ( adjDumpFreq .NE. 0. ) THEN
        STOP 'adjDumpFreq <> 0, but undef ALLOW_AUTODIFF_MONITOR'
       ENDIF
       IF ( adjMonitorFreq .NE. 0. ) THEN
        STOP 'adjMonitorFreq <> 0, but undef ALLOW_AUTODIFF_MONITOR'
       ENDIF
#endif

#ifdef ALLOW_GRDCHK
       IF ( useSingleCpuIO .AND. useGrdchk ) THEN
          STOP 'Need to have useSingleCpuIO=.FALSE. with useGrdchk'
       ENDIF
#endif

#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
