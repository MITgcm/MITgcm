#include "LONGSTEP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: LONGSTEP_CHECK

C     !INTERFACE:
      SUBROUTINE LONGSTEP_CHECK( myThid )

C     !DESCRIPTION:
C     Print summary of longstep parameters

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LONGSTEP_PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_LONGSTEP

C     !LOCAL VARIABLES:
C     msgBuf     :: message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'LONGSTEP_CHECK: #define ALLOW_LONGSTEP'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// longstep parameters '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// ==================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL WRITE_0D_I( LS_nIter, INDEX_NONE,
     &   'LS_nIter =',
     &   ' /* number of dynamics steps between ptracer steps */')
      CALL WRITE_0D_I( LS_whenToSample, INDEX_NONE,
     &   'LS_whenToSample =',
     &   ' /* 0: before; 1: after TD, before DYN; 2: after */')

#ifdef EXACT_CONSERV
      IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &     .AND. useRealFreshWaterFlux .AND. staggerTimeStep
     &     .AND. LS_whenToSample.LT.2 ) THEN
        WRITE(msgBuf,'(2A)')
     &     ' LONGSTEP: staggerTimeStep with EXACT_CONSERV,',
     &     ' useRealFreshWaterFlux'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)')
     &     ' LONGSTEP: and nonlinFreeSurf or PCoords',
     &     ' requires LS_whenToSample=2'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R LONGSTEP_INIT_FIXED'
      ENDIF
#endif /* EXACT_CONSERV */

      _END_MASTER(myThid)
      _BARRIER

#endif /* ALLOW_LONGSTEP */

      RETURN
      END
