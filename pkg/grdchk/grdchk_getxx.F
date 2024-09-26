#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE GRDCHK_GETXX(
     I                       icvrec,
     I                       theSimulationMode,
     I                       iGrdC, jGrdC, layer,
     I                       bi_gc, bj_gc, procId_gc,
     I                       varIndex_gc,
     O                       xx_comp_ref,
     O                       xx_comp_pert,
     I                       localEps,
     I                       myThid )

C     ==================================================================
C     SUBROUTINE grdchk_getxx
C     ==================================================================
C
C     o Set component a component of the control vector; xx(loc)
C
C     started: Christian Eckert eckert@mit.edu 08-Mar-2000
C     continued: heimbach@mit.edu: 13-Jun-2001
C
C     ==================================================================
C     SUBROUTINE grdchk_getxx
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "CTRL_SIZE.h"
#include "CTRL.h"
#include "OPTIMCYCLE.h"

C     == routine arguments ==
      INTEGER icvrec
      INTEGER theSimulationMode
      INTEGER iGrdC, jGrdC, layer
      INTEGER bi_gc, bj_gc
      INTEGER procId_gc
      INTEGER varIndex_gc
      _RL     xx_comp_ref
      _RL     xx_comp_pert
      _RL     localEps
      INTEGER myThid

#ifdef ALLOW_GRDCHK
C--   == external ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == local variables ==
      INTEGER iL, ilDir
      _RL     dummy
      LOGICAL doglobalread
      LOGICAL ladinit
      CHARACTER*(MAX_LEN_FNAM) ctrl_name
      CHARACTER*(MAX_LEN_FNAM) fName
      _RL loctmp2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_GENARR3D_CONTROL
      _RL loctmp3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      _RL tmpfldxz (1-OLx:sNx+OLx,Nr,nSx,nSy)
#endif
#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      _RL tmpfldyz (1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
C--   == end of interface ==

      xx_comp_ref  = 0. _d 0
      xx_comp_pert = 0. _d 0

      doglobalread = .FALSE.
      ladinit      = .FALSE.
C     Find ctrlDir (w/o trailing blanks) length
      ilDir = ILNBLNK(ctrlDir)

      ctrl_name = ncvarfname(varIndex_gc)
      iL = ILNBLNK( ctrl_name )

      IF ( theSimulationMode .EQ. TANGENT_SIMULATION ) THEN
        WRITE(fName,'(3A,I10.10)') ctrlDir(1:ilDir)//yadmark,
     &           ctrl_name(1:iL), '.', optimcycle
      ELSEIF ( theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
        WRITE(fName,'(3A,I10.10)') ctrlDir(1:ilDir),
     &           ctrl_name(1:iL), '.', optimcycle
      ELSE
        WRITE(fName,'(A)') ' '
      ENDIF

#ifdef ALLOW_GENARR3D_CONTROL
      IF ( ncvartype(varIndex_gc) .EQ. 'Arr3D' ) THEN
         CALL active_read_xyz( fName, loctmp3d, 1,
     &                         doglobalread, ladinit, optimcycle,
     &                         myThid, dummy )
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp_ref = loctmp3d( iGrdC,jGrdC,layer,bi_gc,bj_gc )
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp_pert = xx_comp_ref + localEps
         IF ( myProcId .EQ. procId_gc )
     &   loctmp3d( iGrdC,jGrdC,layer,bi_gc,bj_gc ) = xx_comp_pert
         CALL active_write_xyz( fName, loctmp3d, 1,
     &                          optimcycle,
     &                          myThid, dummy )
#else
      IF ( .FALSE. ) THEN
#endif

#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      ELSEIF ( ncvartype(varIndex_gc) .EQ. 'SecXZ' ) THEN
         CALL active_read_xz( fName, tmpfldxz, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        myThid, dummy )
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp_ref = tmpfldxz( iGrdC,layer,bi_gc,bj_gc )
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp_pert = xx_comp_ref + localEps
         IF ( myProcId .EQ. procId_gc )
     &   tmpfldxz( iGrdC,layer,bi_gc,bj_gc ) = xx_comp_pert
         CALL active_write_xz( fName, tmpfldxz, icvrec,
     &                         optimcycle,
     &                         myThid, dummy )
#endif

#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      ELSEIF ( ncvartype(varIndex_gc) .EQ. 'SecYZ' ) THEN
         CALL active_read_yz( fName, tmpfldyz, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        myThid, dummy )
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp_ref = tmpfldyz( jGrdC,layer,bi_gc,bj_gc )
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp_pert = xx_comp_ref + localEps
         IF ( myProcId .EQ. procId_gc )
     &   tmpfldyz( jGrdC,layer,bi_gc,bj_gc ) = xx_comp_pert
         CALL active_write_yz( fName, tmpfldyz, icvrec,
     &                         optimcycle,
     &                         myThid, dummy )
#endif

      ELSE
         CALL active_read_xy( fName, loctmp2d, icvrec,
     &                        doglobalread, ladinit, optimcycle,
     &                        myThid, dummy )
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp_ref = loctmp2d( iGrdC,jGrdC,bi_gc,bj_gc )
         IF ( myProcId .EQ. procId_gc )
     &   xx_comp_pert = xx_comp_ref + localEps
         IF ( myProcId .EQ. procId_gc )
     &   loctmp2d( iGrdC,jGrdC,bi_gc,bj_gc ) = xx_comp_pert
         CALL active_write_xy( fName, loctmp2d, icvrec,
     &                         optimcycle,
     &                         myThid, dummy )

      ENDIF

#endif /* ALLOW_GRDCHK */

      RETURN
      END
