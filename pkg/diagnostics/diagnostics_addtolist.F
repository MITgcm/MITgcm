C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_addtolist.F,v 1.4 2013/08/14 00:54:06 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_ADDTOLIST

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_ADDTOLIST (
     O     diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate,
     I     myThid )

C     !DESCRIPTION:
C     routine to add 1 diagnostics to the list of available diagnostics:
C     set the attributes:
C     name (=cdiag), parsing code (=gdiag), units (=udiag), title (=tdiag)
C     and diagnostic mate number (=hdiag) of the new diagnostic and
C     update the total number of available diagnostics
C     Note: needs to be called after DIAGNOSTICS_INIT_EARLY
C                         and before DIAGNOSTICS_INIT_FIXED

C     !USES:
       IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     diagName  :: diagnostic name to declare
C     diagCode  :: parser code for this diagnostic
C     diagUnits :: field units for this diagnostic
C     diagTitle :: field description for this diagnostic
C     diagMate  :: diagnostic mate number
C     myThid    :: my Thread Id number
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(*) diagTitle
      INTEGER diagMate
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     numDiag :: diagnostic number in the list of available diagnostics
      INTEGER diagNum
CEOP

C     !LOCAL VARIABLES:
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER n

C--   Initialise
      diagNum = 0

      _BEGIN_MASTER( myThid)

C--   Check if this S/R is called from the right place ;
C     needs to be after DIAGNOSTICS_INIT_EARLY and before DIAGNOSTICS_INIT_FIXED
      IF ( diag_pkgStatus.NE.ready2setDiags ) THEN
        CALL DIAGNOSTICS_STATUS_ERROR( 'DIAGNOSTICS_ADDTOLIST',
     &                   ' ', diagName, ready2setDiags, myThid )
      ENDIF

C--   Search for "diagName" in the list of available diagnostics:
      DO n=1,ndiagt
       IF ( cdiag(n).EQ.diagName ) THEN
         diagNum = n
         IF ( gdiag(n).EQ.diagCode .AND. hdiag(n).EQ.diagMate ) THEN
C-    diagnostics is already defined and has the same characteristics
          WRITE(msgBuf,'(3A,I6,A)') 'DIAGNOSTICS_ADDTOLIST: diag=',
     &     diagName,' is already defined (# ',n,' )'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
          WRITE(msgBuf,'(2A)') 'DIAGNOSTICS_ADDTOLIST:',
     &     ' with same parser => update Title & Units '
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
          udiag(diagNum) = diagUnits
          tdiag(diagNum) = diagTitle
         ELSE
C-    diagnostics is already defined but with different characteristics
          WRITE(msgBuf,'(3A,I6,A)') 'DIAGNOSTICS_ADDTOLIST: diag=',
     &     diagName,' is already defined (# ',n,' )'
          CALL PRINT_ERROR( msgBuf , myThid)
          WRITE(msgBuf,'(4A,I6)')   'DIAGNOSTICS_ADDTOLIST: cannot ',
     &     'change parser="',gdiag(n),'" & mate=',hdiag(n)
          CALL PRINT_ERROR( msgBuf , myThid)
          WRITE(msgBuf,'(4A,I6,A)') 'DIAGNOSTICS_ADDTOLIST:',
     &     '  to : "',diagCode,'" and mate=',diagMate,' ; => STOP'
          CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL END: S/R DIAGNOSTICS_ADDTOLIST'
         ENDIF
       ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( diagNum.EQ.0 ) THEN
C--   Add one diagnostic to the list of available diagnostics:
       diagNum = ndiagt + 1

       IF ( diagNum .LE. ndiagMax ) THEN
        cdiag(diagNum) = diagName
        gdiag(diagNum) = diagCode
        hdiag(diagNum) = diagMate
        udiag(diagNum) = diagUnits
        tdiag(diagNum) = diagTitle
        ndiagt = diagNum
       ELSE
        WRITE(msgBuf,'(2A,I6)') 'DIAGNOSTICS_ADDTOLIST:',
     &   ' Exceed Max.Number of diagnostics ndiagMax=', ndiagMax
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A)')
     &   'DIAGNOSTICS_ADDTOLIST: when setting diagnostic: ',diagName
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R DIAGNOSTICS_ADDTOLIST'
       ENDIF

      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
