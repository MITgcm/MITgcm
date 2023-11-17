#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: GRDCHK_CTRL_FNAME

C !INTERFACE:
      SUBROUTINE GRDCHK_CTRL_FNAME(
     O                              fName,
     I                              grdchk_index, myThid )

C     !DESCRIPTION:
C     Return the Control file-name corresponding to grdchk-index

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

#ifdef ALLOW_GRDCHK
C     !FUNCTIONS:
c     INTEGER  ILNBLNK
c     EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iarr
CEOP

      IF ( grdchk_index.LE.0 .OR. grdchk_index.GT.maxcvars ) THEN
        WRITE(msgBuf,'(2A,I8)') 'GRDCHK_CTRL_FNAME: ',
     &        'Invalid grdchk_index=', grdchk_index
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GRDCHK_CTRL_FNAME'
      ELSE
        fname = ncvarfname( grdchk_index )
      ENDIF

#endif /* ALLOW_GRDCHK */

      RETURN
      END
