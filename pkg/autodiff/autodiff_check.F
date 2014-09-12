C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_check.F,v 1.18 2014/09/12 20:49:42 gforget Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#include "AUTODIFF_OPTIONS.h"
#include "AD_CONFIG.h"

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

#if (defined ALLOW_AUTODIFF_TAMC )

#ifdef ALLOW_TAMC_CHECKPOINTING
# if (defined (AUTODIFF_2_LEVEL_CHECKPOINT))
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
# elif (defined (AUTODIFF_4_LEVEL_CHECKPOINT))
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
# else
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
# endif
#else /* undef ALLOW_TAMC_CHECKPOINTING */
      if (nchklev_1 .lt. nTimeSteps) then
        WRITE(msgBuf,'(A)')
     &       'THE_MAIN_LOOP: TAMC checkpointing parameters'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(A,I10)')
     &       'nchklev_1 = ', nchklev_1
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(A,I10)')
     &       'are not consistent with nTimeSteps = ', nTimeSteps
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R AUTODIFF_CHECK'
      endif
#endif /* ALLOW_TAMC_CHECKPOINTING */

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

c#ifndef ALLOW_AUTODIFF_WHTAPEIO
c      IF ( useSingleCpuIO ) THEN
c       WRITE(msgBuf,'(3A)') '** WARNING ** AUTODIFF_CHECK: ',
c    &        'relying on mdsio_read/writevector.F to write ',
c    &        'tapes is unsafe when useSingleCpuIO is true.'
c       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
c    &  SQUEEZE_RIGHT, myThid )
c      ENDIF
c#endif

#endif /* ALLOW_AUTODIFF_TAMC & ( ALLOW_ADJOINT_RUN | ALLOW_ADMTLM )*/

#ifdef ALLOW_OPENAD
# if (defined (ALLOW_ECCO) || defined (ALLOW_PROFILES))
       WRITE(msgBuf,'(2A)')
     &  'AUTODIFF_CHECK: pkg ECCO or PROFILES not yet implemented ',
     &  ' for ALLOW_AUTODIFF_OPENAD'
       CALL PRINT_ERROR( msgBuf , myThid )
       STOP 'ABNORMAL END: S/R AUTODIFF_CHECK'
# endif
#endif /* ALLOW_OPENAD */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
