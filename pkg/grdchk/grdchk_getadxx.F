#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE grdchk_getadxx(
     I                       icvrec,
     I                       iGrdC, jGrdC, layer,
     I                       bi_gc, bj_gc, procId_gc,
     I                       varIndex_gc,
     O                       xx_comp,
     I                       myThid )

C     ==================================================================
C     SUBROUTINE grdchk_getadxx
C     ==================================================================
C
C     o Set component a component of the control vector; xx(loc)
C
C     started: Christian Eckert eckert@mit.edu 08-Mar-2000
C     continued: heimbach@mit.edu: 13-Jun-2001
C
C     ==================================================================
C     SUBROUTINE grdchk_getadxx
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "CTRL_SIZE.h"
#include "CTRL.h"
#include "optim.h"

C     == routine arguments ==
      INTEGER icvrec
      INTEGER iGrdC, jGrdC, layer
      INTEGER bi_gc, bj_gc
      INTEGER procId_gc
      INTEGER varIndex_gc
      _RL     xx_comp
      INTEGER myThid

#ifdef ALLOW_GRDCHK
C--   == external ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == local variables ==
      INTEGER il, ilDir
      _RL     dummy
      LOGICAL doglobalread
      LOGICAL ladinit
#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      _RL tmpfldxz (1-OLx:sNx+OLx,Nr,nSx,nSy)
#endif
#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      _RL tmpfldyz (1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
      _RL loctmp2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL loctmp3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) fName
      CHARACTER*(MAX_LEN_FNAM) ctrl_name
C--   == end of interface ==

      doglobalread = .false.
      ladinit      = .false.
C     Find ctrlDir (w/o trailing blanks) length
      ilDir = ILNBLNK(ctrlDir)

C-    get the name of varIndex_gc corresponding CTRL:
      CALL GRDCHK_CTRL_FNAME( ctrl_name, varIndex_gc, myThid )
      iL = ILNBLNK( ctrl_name )

      WRITE(fName,'(3A,I10.10)') ctrlDir(1:ilDir)//yadmark,
     &           ctrl_name(1:iL), '.', optimcycle

      IF ( varIndex_gc.EQ.1 .OR. varIndex_gc.EQ.2 .OR.
     &     varIndex_gc.EQ.27 .OR. varIndex_gc.EQ.28 .OR.
     &     varIndex_gc.EQ.15 .OR. varIndex_gc.EQ.16 .OR.
     &     varIndex_gc.EQ.17 .OR. varIndex_gc.EQ.21 .OR.
     &     varIndex_gc.EQ.22 .OR. varIndex_gc.EQ.31 .OR.
     &     varIndex_gc.EQ.44
#ifdef ALLOW_GENARR3D_CONTROL
     &     .OR. ( varIndex_gc .GE. 201 .AND.
     &     varIndex_gc .LE. 200+maxCtrlArr3D )
#endif
     &                                            ) THEN

         CALL active_read_xyz( fName, loctmp3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         myThid, dummy )
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp = loctmp3d( iGrdC,jGrdC,layer,bi_gc,bj_gc )

#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      ELSEIF ( varIndex_gc.EQ.11 .OR. varIndex_gc.EQ.12) THEN
         CALL active_read_xz( fName, tmpfldxz, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        myThid, dummy)
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp = tmpfldxz( iGrdC,layer,bi_gc,bj_gc )
#endif

#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      ELSEIF ( varIndex_gc.EQ.13 .OR. varIndex_gc.EQ.14) THEN
         CALL active_read_yz( fName, tmpfldyz, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        myThid, dummy )
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp = tmpfldyz( jGrdC,layer,bi_gc,bj_gc )
#endif

      ELSE

         CALL active_read_xy( fName, loctmp2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        myThid, dummy )
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp = loctmp2d( iGrdC,jGrdC,bi_gc,bj_gc )

      ENDIF

#endif /* ALLOW_GRDCHK */

      RETURN
      END
