C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_write_pickup.F,v 1.1 2005/02/20 04:31:54 edhill Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DIAGNOSTICS_WRITE_PICKUP 
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_WRITE_PICKUP(
     I     suff, 
     I     myTime,
     I     myIter,
     I     myThid )

C     !DESCRIPTION:
C     Writes current state of the diagnostics package.

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: current time
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      CHARACTER*(*) suff
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIAGNOSTICS

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
C     lgf     :: flag to write "global" files
c     INTEGER prec, iChar, lChar, k
      INTEGER prec, lChar, k, i
      CHARACTER*(MAX_LEN_FNAM) fn
      LOGICAL lgf

      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (diag_pickup_write) THEN

        DO i = 1,MAX_LEN_FNAM
          fn(i:i) = ' '
        ENDDO
        lChar = ILNBLNK(suff)
        WRITE(fn,'(A,A)') 'pickup_diagnostics.',suff(1:lChar)
        prec = precFloat64
        lgf = globalFiles
        
#ifdef ALLOW_MNC
C       IF (useMNC .AND. diag_pickup_write_mnc) THEN
C       ENDIF
#endif
        
        IF (diag_pickup_write_mdsio) THEN
          CALL MDSWRITEFIELD(fn,prec,lgf,'RL',numdiags,qdiag,
     &         1,myIter,myThid)
        ENDIF

      ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
