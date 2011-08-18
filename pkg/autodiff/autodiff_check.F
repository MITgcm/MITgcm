C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_check.F,v 1.11 2011/08/18 09:42:17 heimbach Exp $
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
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

#ifdef ALLOW_AUTODIFF_TAMC

#if (defined (AUTODIFF_2_LEVEL_CHECKPOINT))
      if (nchklev_1*nchklev_2 .lt. nTimeSteps) then
        WRITE(msgBuf,'(A)') 
     &       'THE_MAIN_LOOP: TAMC checkpointing parameters'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(A,I10)') 
     &       'nchklev_1*nchklev_2 = ',
     &       nchklev_1*nchklev_2
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(A,I10)') 
     &       'are not consistent with nTimeSteps = ',
     &       nTimeSteps
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R AUTODIFF_CHECK'
      endif
#elif (defined (AUTODIFF_4_LEVEL_CHECKPOINT))
      if (nchklev_1*nchklev_2*nchklev_3*nchklev_4 .lt. nTimeSteps) then
        WRITE(msgBuf,'(A)') 
     &       'THE_MAIN_LOOP: TAMC checkpointing parameters'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(A,I10)') 
     &       'nchklev_1*nchklev_2*nchklev_3*nchklev_4 = ',
     &       nchklev_1*nchklev_2*nchklev_3*nchklev_4
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(A,I10)') 
     &       'are not consistent with nTimeSteps = ',
     &       nTimeSteps
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R AUTODIFF_CHECK'
      endif
#else
c--   Check the choice of the checkpointing parameters in relation
c--   to nTimeSteps: (nchklev_1*nchklev_2*nchklev_3 .ge. nTimeSteps)
      if (nchklev_1*nchklev_2*nchklev_3 .lt. nTimeSteps) then
        WRITE(msgBuf,'(A)') 
     &       'THE_MAIN_LOOP: TAMC checkpointing parameters'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(A,I10)') 
     &       'nchklev_1*nchklev_2*nchklev_3 = ',
     &       nchklev_1*nchklev_2*nchklev_3
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(A,I10)') 
     &       'are not consistent with nTimeSteps = ',
     &       nTimeSteps
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R AUTODIFF_CHECK'
      endif
#endif

#ifndef ALLOW_AUTODIFF_MONITOR
       IF ( adjDumpFreq .NE. 0. ) THEN
        WRITE(msgBuf,'(A)') 
     &         'adjDumpFreq <> 0, but undef ALLOW_AUTODIFF_MONITOR'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R AUTODIFF_CHECK'
       ENDIF
       IF ( adjMonitorFreq .NE. 0. ) THEN
        WRITE(msgBuf,'(A)') 
     &         'adjMonitorFreq <> 0, but undef ALLOW_AUTODIFF_MONITOR'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R AUTODIFF_CHECK'
       ENDIF
#endif

#ifdef ALLOW_GRDCHK
       IF ( useSingleCpuIO .AND. useGrdchk ) THEN
        WRITE(msgBuf,'(A)') 
     &         'Need to have useSingleCpuIO=.FALSE. with useGrdchk'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R AUTODIFF_CHECK'
       ENDIF
#endif

#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
