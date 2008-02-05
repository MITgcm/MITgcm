C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_close_io.F,v 1.2 2008/02/05 15:31:19 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_CLOSE_IO
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_CLOSE_IO( myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | S/R DIAGSTATS_CLOSE_IO
C     | o Close I/O unit of ASCII output file
C     *==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER  n, iL, nUnit
      CHARACTER*(MAX_LEN_FNAM) dataFName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

      _BEGIN_MASTER( myThid)

#ifdef ALLOW_USE_MPI
      IF ( diagSt_Ascii .AND. mpiMyId.EQ.0 ) THEN
#else
      IF ( diagSt_Ascii ) THEN
#endif

        DO n=1,diagSt_nbLists

          nUnit = diagSt_ioUnit(n)

C-      write a conclusion & close the file:
          WRITE(nUnit,'(A)') '# records End here.'
          CLOSE(nUnit)

          iL = ILNBLNK(diagSt_Fname(n))
          WRITE(dataFName,'(2A,I10.10,A)')
     &          diagSt_Fname(n)(1:iL), '.', nIter0, '.txt'
          WRITE(msgBuf,'(4A,I6)') 'DIAGSTATS_CLOSE_IO: ',
     &         'close file: ',dataFName(1:iL+15), ' , unit=', nUnit
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)

        ENDDO

      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
