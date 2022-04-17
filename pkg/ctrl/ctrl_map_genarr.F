#include "CTRL_OPTIONS.h"

C--   File ctrl_map_genarr.F: Generic arrays control vector
C--    Contents:
C--    o CTRL_MAP_GENARR2D
C--    o CTRL_MAP_GENARR3D

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: CTRL_MAP_GENARR2D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_GENARR2D( fld, iarr, myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_MAP_GENARR2D
C     | Add the generic 2D-arrays of the
C     | control vector to the model state and update the tile halos.
C     | The control vector is defined in the header file "ctrl.h".
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "optim.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL     fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iarr
      INTEGER myThid

#ifdef ALLOW_GENARR2D_CONTROL
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j
      INTEGER numsmo, k2
      LOGICAL dowc01
      LOGICAL dosmooth
      LOGICAL doscaling
      LOGICAL dolog10ctrl
      _RL log10initval
      _RL ln10
      _RL xx_gen  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(80) fnamegenIn
      CHARACTER*(80) fnamegenOut
      CHARACTER*(MAX_LEN_FNAM) fnamebase
      INTEGER ilgen
      LOGICAL doglobalread
      LOGICAL ladinit
      _RS mask2D (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEOP

C--   Now, read the control vector.
      doglobalread = .FALSE.
      ladinit      = .FALSE.

      CALL CTRL_ASSIGN(xx_gen, 1, zeroRL, myThid)

      dosmooth=.FALSE.
      dowc01  = .FALSE.
      doscaling=.TRUE.
      dolog10ctrl=.FALSE.
      log10initval=0. _d 0
      ln10 = LOG(10. _d 0)

      numsmo = 1
      DO k2 = 1, maxCtrlProc
        IF ( xx_genarr2d_preproc(k2,iarr).EQ.'WC01' ) THEN
          dowc01 = .TRUE.
          IF (xx_genarr2d_preproc_i(k2,iarr).NE.0)
     &      numsmo = xx_genarr2d_preproc_i(k2,iarr)
        ENDIF
        IF ( (.NOT.dowc01).AND.
     &       (xx_genarr2d_preproc(k2,iarr).EQ.'smooth') ) THEN
          dosmooth = .TRUE.
          IF (xx_genarr2d_preproc_i(k2,iarr).NE.0)
     &      numsmo = xx_genarr2d_preproc_i(k2,iarr)
        ENDIF
        IF ( xx_genarr2d_preproc(k2,iarr).EQ.'noscaling' ) THEN
          doscaling = .FALSE.
        ENDIF
        IF (xx_genarr2d_preproc_c(k2,iarr).EQ.'log10ctrl') THEN
C             fld := log10(xx_gen)
C               with initial guess for xx_gen set to log10initval
C               passed to data.ctrl, (default = 0.0)
          dolog10ctrl=.TRUE.
          log10initval = xx_genarr2d_preproc_r(k2,iarr)
        ENDIF
      ENDDO

      fnamebase = xx_genarr2d_file(iarr)
      ilgen = ILNBLNK( fnamebase )
      WRITE(fnamegenIn(1:80),'(2A,I10.10)')
     &       fnamebase(1:ilgen),'.',optimcycle
      WRITE(fnamegenOut(1:80),'(2A,I10.10)')
     &       fnamebase(1:ilgen),'.effective.',optimcycle

      CALL READ_REC_3D_RL( xx_genarr2d_weight(iarr), ctrlprec, 1,
     &          wgenarr2d(1-OLx,1-OLy,1,1,iarr), 1, 1, myThid )

#ifdef ALLOW_AUTODIFF
      CALL ACTIVE_READ_XY( fnamegenIn, xx_gen, 1, doglobalread,
     &     ladinit, optimcycle, myThid, xx_genarr2d_dummy(iarr) )
#else
      CALL READ_REC_XY_RL( fnamegenIn, xx_gen, 1, 1, myThid)
#endif

C --- Get appropriate mask for ctrl variable
      CALL CTRL_GET_MASK2D(xx_genarr2d_file(iarr), mask2D, myThid)

C --- Do any smoothing
#ifdef ALLOW_SMOOTH
      IF (useSMOOTH) THEN
       IF (dowc01) CALL SMOOTH_CORREL2D(xx_gen,mask2D,numsmo,myThid)
       IF (dosmooth) CALL SMOOTH2D(xx_gen,mask2D,numsmo,myThid)
      ENDIF
#endif

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        IF (doscaling) THEN
         DO j = 1,sNy
          DO i = 1,sNx
C scale param adjustment
           IF ( wgenarr2d(i,j,bi,bj,iarr).GT.0. ) THEN
            xx_gen(i,j,bi,bj) = xx_gen(i,j,bi,bj)
     &               / SQRT( wgenarr2d(i,j,bi,bj,iarr) )
            IF (dolog10ctrl) THEN
             xx_gen(i,j,bi,bj) = xx_gen(i,j,bi,bj) + log10initval
c            xx_gen(i,j,bi,bj) = 10.0 ** xx_gen(i,j,bi,bj)
C     this is faster, especially if log(10) has been evaluated before
c            xx_gen(i,j,bi,bj) = EXP(LOG(10.0) * xx_gen(i,j,bi,bj))
             xx_gen(i,j,bi,bj) = EXP(ln10 * xx_gen(i,j,bi,bj))
            ENDIF ! dolog10ctrls
           ELSE
            xx_gen(i,j,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDIF ! doscaling
C add to model parameter
C or in case of log10ctrl, fld = 10^(xx_gen)
        IF (dolog10ctrl) THEN
         DO j = 1,sNy
          DO i = 1,sNx
           fld(i,j,bi,bj) = xx_gen(i,j,bi,bj)*mask2D(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO j = 1,sNy
          DO i = 1,sNx
           fld(i,j,bi,bj) = fld(i,j,bi,bj)
     &           + xx_gen(i,j,bi,bj)*mask2D(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF ! dolog10ctrls
       ENDDO
      ENDDO

C avoid param out of [boundsVec(1) boundsVec(4)]
      CALL CTRL_BOUND_2D(fld,mask2D,xx_genarr2d_bounds(1,iarr),myThid)

      CALL EXCH_XY_RL( fld, myThid )

      CALL WRITE_REC_3D_RL( fnamegenOut, ctrlprec, 1,
     &                      fld, 1, optimcycle, myThid )

#endif /* ALLOW_GENARR2D_CONTROL */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: CTRL_MAP_GENARR3D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_GENARR3D( fld, iarr, myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_MAP_GENARR3D
C     | Add the generic 3D-arrays of the
C     | control vector to the model state and update the tile halos.
C     | The control vector is defined in the header file "ctrl.h".
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "optim.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL     fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iarr
      INTEGER myThid

#ifdef ALLOW_OPENAD
      STOP 'ABNORMAL END: CTRL_MAP_GENARR3D is empty'
#else /* ALLOW_OPENAD */
#ifdef ALLOW_GENARR3D_CONTROL
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER i,j,k
      INTEGER numsmo,k2
      LOGICAL dowc01
      LOGICAL dosmooth
      LOGICAL doscaling
      LOGICAL dolog10ctrl
      _RL log10initval
      _RL ln10
      _RL xx_gen(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(80) fnamegenIn
      CHARACTER*(80) fnamegenOut
      CHARACTER*(MAX_LEN_FNAM) fnamebase
      INTEGER ilgen
      LOGICAL doglobalread
      LOGICAL ladinit
      _RS mask3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CEOP

C--   Now, read the control vector.
      doglobalread = .FALSE.
      ladinit      = .FALSE.

      CALL CTRL_ASSIGN(xx_gen, Nr, zeroRL, myThid)

      dosmooth=.FALSE.
      dowc01  = .FALSE.
      doscaling=.TRUE.
      dolog10ctrl=.FALSE.
      log10initval=0. _d 0
      ln10 = LOG(10. _d 0)

      numsmo = 1
      DO k2 = 1, maxCtrlProc
        IF ( xx_genarr3d_preproc(k2,iarr).EQ.'WC01' ) THEN
          dowc01 = .TRUE.
          IF (xx_genarr3d_preproc_i(k2,iarr).NE.0)
     &      numsmo = xx_genarr3d_preproc_i(k2,iarr)
        ENDIF
        IF ( (.NOT.dowc01).AND.
     &       (xx_genarr3d_preproc(k2,iarr).EQ.'smooth') ) THEN
          dosmooth = .TRUE.
          IF (xx_genarr3d_preproc_i(k2,iarr).NE.0)
     &      numsmo = xx_genarr3d_preproc_i(k2,iarr)
        ENDIF
        IF (xx_genarr3d_preproc(k2,iarr).EQ.'noscaling') THEN
          doscaling = .FALSE.
        ENDIF
        IF (xx_genarr3d_preproc_c(k2,iarr).EQ.'log10ctrl') THEN
C             fld := log10(xx_gen)
C               with initial guess for xx_gen set to log10initval
C               passed to data.ctrl, (default = 0.0)
          dolog10ctrl=.TRUE.
          log10initval = xx_genarr3d_preproc_r(k2,iarr)
        ENDIF
      ENDDO

      fnamebase = xx_genarr3d_file(iarr)
      ilgen = ILNBLNK( fnamebase )
      WRITE(fnamegenIn(1:80),'(2a,i10.10)')
     &       fnamebase(1:ilgen),'.',optimcycle
      WRITE(fnamegenOut(1:80),'(2a,i10.10)')
     &       fnamebase(1:ilgen),'.effective.',optimcycle

      CALL READ_REC_3D_RL( xx_genarr3d_weight(iarr), ctrlprec, Nr,
     &          wgenarr3d(1-OLx,1-OLy,1,1,1,iarr), 1, 1, myThid )

#ifdef ALLOW_AUTODIFF
      CALL ACTIVE_READ_XYZ( fnamegenIn, xx_gen, 1, doglobalread,
     & ladinit, optimcycle, myThid, xx_genarr3d_dummy(iarr) )
#else
      CALL READ_REC_XYZ_RL( fnamegenIn, xx_gen, 1, 1, myThid)
#endif

C --- Get appropriate mask for ctrl variable
      CALL CTRL_GET_MASK3D(xx_genarr3d_file(iarr), mask3D, myThid)

C --- Do any smoothing
#ifdef ALLOW_SMOOTH
      IF (useSMOOTH) THEN
       IF (dowc01) CALL SMOOTH_CORREL3D( xx_gen, numsmo, myThid )
       IF (dosmooth) CALL SMOOTH3D( xx_gen, numsmo, myThid )
      ENDIF
#endif

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        IF (doscaling) THEN
C     scale param adjustment
         DO k = 1,Nr
          DO j = 1,sNy
           DO i = 1,sNx
            IF ( wgenarr3d(i,j,k,bi,bj,iarr).GT.0. ) THEN
             xx_gen(i,j,k,bi,bj) = xx_gen(i,j,k,bi,bj)
     &                / SQRT( wgenarr3d(i,j,k,bi,bj,iarr) )
             IF (dolog10ctrl) THEN
              xx_gen(i,j,k,bi,bj) = xx_gen(i,j,k,bi,bj) + log10initval
c             xx_gen(i,j,k,bi,bj) = 10.0 ** xx_gen(i,j,k,bi,bj)
C     this is faster, especially if log(10) has been evaluated before
c             xx_gen(i,j,k,bi,bj) = EXP(LOG(10.0) * xx_gen(i,j,k,bi,bj))
              xx_gen(i,j,k,bi,bj) = EXP(ln10 * xx_gen(i,j,k,bi,bj))
             ENDIF
            ELSE
             xx_gen(i,j,k,bi,bj) = 0. _d 0
            ENDIF
           ENDDO
          ENDDO
         ENDDO
C     doscaling
        ENDIF
C     add to model parameter
C     or in case of log10ctrl, fld = 10^(xx_gen)
        IF ( dolog10ctrl ) THEN
         DO k = 1,Nr
          DO j = 1,sNy
           DO i = 1,sNx
            fld(i,j,k,bi,bj) = xx_gen(i,j,k,bi,bj)*mask3D(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ELSE
         DO k = 1,Nr
          DO j = 1,sNy
           DO i = 1,sNx
            fld(i,j,k,bi,bj) = fld(i,j,k,bi,bj)
     &           + xx_gen(i,j,k,bi,bj)*mask3D(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDIF
C     end bi/bj-loops
       ENDDO
      ENDDO

C avoid param out of [boundsVec(1) boundsVec(4)]
      CALL CTRL_BOUND_3D(fld,mask3D,xx_genarr3d_bounds(1,iarr),myThid)

C The tile exchange for xx_uvel and xx_vvel will be
C  done in CTRL_MAP_INI_GENARR.F when both
C  xx_uvel and xx_vvel are read in.
      IF (xx_genarr3d_file(iarr)(1:7).NE.'xx_uvel'.AND.
     &    xx_genarr3d_file(iarr)(1:7).NE.'xx_vvel')
     &    CALL EXCH_XYZ_RL( fld, myThid )

      CALL WRITE_REC_3D_RL( fnamegenOut, ctrlprec, Nr,
     &                      fld, 1, optimcycle, myThid )

#endif /* ALLOW_GENARR3D_CONTROL */
#endif /* ALLOW_OPENAD */

      RETURN
      END
