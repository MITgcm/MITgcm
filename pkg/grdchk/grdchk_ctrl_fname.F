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

#ifdef ALLOW_OBCSN_CONTROL
      ELSEIF ( grdchk_index .EQ. 11 ) THEN
        fName = xx_obcsn_file
#endif /* ALLOW_OBCSN_CONTROL */

#ifdef ALLOW_OBCSS_CONTROL
      ELSEIF ( grdchk_index .EQ. 12 ) THEN
        fName = xx_obcss_file
#endif /* ALLOW_OBCSS_CONTROL */

#ifdef ALLOW_OBCSW_CONTROL
      ELSEIF ( grdchk_index .EQ. 13 ) THEN
        fName = xx_obcsw_file
#endif /* ALLOW_OBCSW_CONTROL */

#ifdef ALLOW_OBCSE_CONTROL
      ELSEIF ( grdchk_index .EQ. 14 ) THEN
        fName = xx_obcse_file
#endif /* ALLOW_OBCSE_CONTROL */

#ifdef ALLOW_GENARR2D_CONTROL
      ELSEIF ( grdchk_index .GE. 101 .AND.
     &         grdchk_index .LE. 100+maxCtrlArr2D ) THEN
        iarr = grdchk_index - 100
        fName = xx_genarr2d_file(iarr)
#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
      ELSEIF ( grdchk_index .GE. 201 .AND.
     &         grdchk_index .LE. 200+maxCtrlArr3D ) THEN
        iarr = grdchk_index - 200
        fName = xx_genarr3d_file(iarr)
#endif /* ALLOW_GENARR3D_CONTROL */

#ifdef ALLOW_GENTIM2D_CONTROL
      ELSEIF ( grdchk_index .GE. 301 .AND.
     &         grdchk_index .LE. 300+maxCtrlTim2D ) THEN
        iarr = grdchk_index - 300
        fName = xx_gentim2d_file(iarr)
#endif /* ALLOW_GENTIM2D_CONTROL */

      ELSE
        WRITE(fName,'(A)') 'undefined'
      ENDIF

#endif /* ALLOW_GRDCHK */

      RETURN
      END
