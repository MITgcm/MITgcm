C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/Attic/diagnostics_add2list.F,v 1.2 2004/12/15 00:19:22 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_ADD2LIST

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_ADD2LIST (
     I     diagName, diagCode, diagUnits, diagTitle,
     I     myThid )

C     !DESCRIPTION:
C     routine to add 1 diagnostics to the list of available diagnostics:
C     set the attributes:
C     name (=cdiag), parsing code (=gdiag), units (=udiag), and title (=tdiag)
C     of the new diagnostics and update the total number of available diagnostic
C     Note: needs to be done very early, before reading "data.diagnostics"
      
C     !USES:
       IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(*) diagTitle
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER n

C-    Add one diagnostic to the list of available diagnostics:
      n = ndiagt + 1

      _BEGIN_MASTER( myThid)

      IF ( n .LE. ndiagMax ) THEN
        cdiag(n) = diagName
        gdiag(n) = diagCode
        udiag(n) = diagUnits
        tdiag(n) = diagTitle
        ndiagt = n
      ELSE
        WRITE(msgBuf,'(2A,I4)') 'DIAGNOSTICS_ADD2LIST:',
     &   ' Exceed Max.Number of diagnostics ndiagMax=', ndiagMax 
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A)')
     &   'DIAGNOSTICS_ADD2LIST: when setting diagnostic: ',diagName
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R DIAGNOSTICS_ADD2LIST'
      ENDIF
      
      _END_MASTER( myThid )

      RETURN 
      END
