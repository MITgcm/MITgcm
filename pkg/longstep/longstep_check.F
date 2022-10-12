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
#include "LONGSTEP_PARAMS.h"
#include "PARAMS.h"

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

      _END_MASTER(myThid)
      _BARRIER

#endif /* ALLOW_LONGSTEP */

      RETURN
      END
