#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: GRDCHK_CTRL_FNAME

C !INTERFACE:
      SUBROUTINE GRDCHK_CTRL_FNAME(
     U                              fName,
     U                              grdchk_index,
     I                              myThid )

C     !DESCRIPTION:
C     associate control file-name with the grdchk_index,
C     depending on which on is set

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "CTRL_SIZE.h"
#include "CTRL.h"
#include "CTRL_GENARR.h"
#ifdef ALLOW_OBCS_CONTROL
# include "CTRL_OBCS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     fName        :: name of the CTRL var/file matching grdchk_index
C     grdchk_index :: grdchk index of CTRL variable to perturb
C     myThid       :: my Thread Id number
      CHARACTER*(MAX_LEN_FNAM) fName
      INTEGER grdchk_index
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER il, ivar
CEOP

      IF ( grdchk_index .EQ. UNSET_I ) THEN
C--   If not set, determine grdchk_index from fName (= grdchkvarname)
       IF ( fName .NE. ' ' ) THEN
        il = ILNBLNK(fName)
        DO ivar = 1, maxcvars
         IF ( ncvarfname(ivar)(1:il).EQ.fName(1:il) ) THEN
          grdchk_index = ivar
          GOTO 99
         ENDIF
        ENDDO
       ENDIF
 99    CONTINUE

       IF ( grdchk_index .EQ. UNSET_I ) THEN
        WRITE(msgBuf,'(3A)') 'S/R GRDCHK_CTRL_FNAME: ',
     &       'grdchkvarindex is not set and could not be determined ',
     &       'from grdchkvarname'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GRDCHK_CTRL_FNAME'
       ENDIF

      ELSE
C--   IF both grdchk_index and fName are set, check for consistency
       il = ILNBLNK(ncvarfname(grdchk_index))
       IF ( fName(1:il) .NE. ' '
     &      .AND. fName(1:il) .NE. ncvarfname(grdchk_index)(1:il) ) THEN
        WRITE(msgBuf,'(5A)') 'S/R GRDCHK_CTRL_FNAME: ',
     &       'fName(grdchkvarindex) = ', fName(1:il),
     &       ' .NE. ncvarfname = ', ncvarfname(grdchk_index)(1:il)
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GRDCHK_CTRL_FNAME'
       ENDIF

      ENDIF

      IF ( grdchk_index.LE.0 .OR. grdchk_index.GT.maxcvars ) THEN
        WRITE(msgBuf,'(2A,I8)') 'GRDCHK_CTRL_FNAME: ',
     &        'Invalid grdchk_index=', grdchk_index
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GRDCHK_CTRL_FNAME'
      ELSE
        fName = ncvarfname( grdchk_index )
      ENDIF

      RETURN
      END
