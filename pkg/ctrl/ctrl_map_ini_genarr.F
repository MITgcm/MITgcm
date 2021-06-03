#include "CTRL_OPTIONS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE_OPTIONS.h"
#endif

C--   File ctrl_map_ini_genarr.F: Generic arrays control vector
C--    Contents:
C--    o CTRL_MAP_INI_GENARR
C--    o CTRL_MAP_GENARR2D
C--    o CTRL_MAP_GENARR3D

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: CTRL_MAP_INI_GENARR
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_INI_GENARR( myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_MAP_INI_GENARR
C     | Add the generic arrays of the
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
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "optim.h"
#include "ctrl_dummy.h"
#include "CTRL_FIELDS.h"
#include "CTRL_GENARR.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
# include "PTRACERS_FIELDS.h"
#endif
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

#if (defined (ALLOW_GENARR3D_CONTROL) && defined(ALLOW_PTRACERS))
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif

C     !LOCAL VARIABLES:
#if (defined (ALLOW_GENARR2D_CONTROL) || defined(ALLOW_GENARR3D_CONTROL))
      INTEGER iarr
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      INTEGER igen_etan,igen_bdrag,igen_geoth
# ifdef ALLOW_SHELFICE
      INTEGER igen_shiCoeffT, igen_shiCoeffS, igen_shiCDrag
      INTEGER i, j, bi, bj, k2
      LOGICAL dragThermoEqualMom
# endif
#endif /* ALLOW_GENARR2D_CONTROL */
#ifdef ALLOW_GENARR3D_CONTROL
      INTEGER igen_theta0, igen_salt0
      INTEGER igen_kapgm, igen_kapredi, igen_diffkr
# if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      INTEGER igen_uvel0, igen_vvel0
# endif
# ifdef ALLOW_PTRACERS
      INTEGER iPtr, iLen
      INTEGER igen_ptr(PTRACERS_num)
# endif
#endif /* ALLOW_GENARR3D_CONTROL */
CEOP

#ifdef ALLOW_GENARR2D_CONTROL
C--   generic 2D control variables

      igen_etan=0
      igen_bdrag=0
      igen_geoth=0
#ifdef ALLOW_SHELFICE
      igen_shiCoeffT=0
      igen_shiCoeffS=0
      igen_shiCDrag=0
#endif
      DO iarr = 1, maxCtrlArr2D
       IF (xx_genarr2d_weight(iarr).NE.' ') THEN
        IF (xx_genarr2d_file(iarr)(1:7).EQ.'xx_etan')
     &     igen_etan=iarr
        IF (xx_genarr2d_file(iarr)(1:13).EQ.'xx_bottomdrag')
     &     igen_bdrag=iarr
        IF (xx_genarr2d_file(iarr)(1:13).EQ.'xx_geothermal')
     &     igen_geoth=iarr
#ifdef ALLOW_SHELFICE
# ifndef SHI_ALLOW_GAMMAFRICT
        IF (xx_genarr2d_file(iarr)(1:12).EQ.'xx_shicoefft')
     &     igen_shiCoeffT=iarr
        IF (xx_genarr2d_file(iarr)(1:12).EQ.'xx_shicoeffs')
     &     igen_shiCoeffS=iarr
# else
        IF (xx_genarr2d_file(iarr)(1:11).EQ.'xx_shicdrag')
     &     igen_shiCDrag=iarr
# endif
#endif
       ENDIF
      ENDDO

      IF (igen_etan.GT.0) THEN
        CALL CTRL_MAP_GENARR2D( etaN, igen_etan, myThid )
      ENDIF
#ifdef ALLOW_BOTTOMDRAG_CONTROL
      IF (igen_bdrag.GT.0)
     &  CALL CTRL_MAP_GENARR2D( bottomDragFld, igen_bdrag, myThid )
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX
      IF (igen_geoth.GT.0)
     &  CALL CTRL_MAP_GENARR2D( geothermalFlux, igen_geoth, myThid )
#endif
#ifdef ALLOW_SHELFICE
# ifndef SHI_ALLOW_GAMMAFRICT
      IF (igen_shiCoeffT.GT.0)
     &  call ctrl_map_genarr2d(shiTransCoeffT,igen_shiCoeffT,myThid)

      IF (igen_shiCoeffS.GT.0)
     &  call ctrl_map_genarr2d(shiTransCoeffS,igen_shiCoeffS,myThid)

C--   xx_shiCoeffS not used, but shiCoeffT is adjusted by xx_shicoefft
      IF ((igen_shiCoeffS.EQ.0).AND.(igen_shiCoeffT.GT.0)) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           shiTransCoeffS(i,j,bi,bj) =
     &       SHELFICEsaltToHeatRatio*shiTransCoeffT(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C--   xx_shiCoeffT not used, but shiCoeffS is adjusted by xx_shicoeffs
      ELSEIF ((igen_shiCoeffT.EQ.0).AND.(igen_shiCoeffS.GT.0)) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           shiTransCoeffT(i,j,bi,bj) =
     &       shiTransCoeffS(i,j,bi,bj)/SHELFICEsaltToHeatRatio
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
# else
      dragThermoEqualMom = .FALSE.
      IF (igen_shiCDrag.GT.0)
     &  call ctrl_map_genarr2d(shiCDragFld,igen_shiCDrag,myThid)
C--     Set drag coefficient used in momentum equal to thermodynamic,
C--      u* drag coefficient
      DO k2 = 1, maxCtrlProc

       IF (xx_genarr2d_preproc_c(k2,igen_shiCDrag)(1:3).EQ.'mom')
     &    dragThermoEqualMom = .TRUE.
      ENDDO
      IF (dragThermoEqualMom) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           shiDragQuadFld(i,j,bi,bj) = shiCDragFld(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
# endif /* SHI_ALLOW_GAMMAFRICT */
#endif /* ALLOW_SHELFICE */

#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
C--   generic 3D control variables

      igen_theta0=0
      igen_salt0=0
      igen_kapgm=0
      igen_kapredi=0
      igen_diffkr=0
# if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      igen_uvel0=0
      igen_vvel0=0
# endif
# ifdef ALLOW_PTRACERS
      DO iPtr = 1, PTRACERS_num
        igen_ptr(iPtr) = 0
      ENDDO
# endif /* ALLOW_PTRACERS */
      DO iarr = 1, maxCtrlArr3D
       IF (xx_genarr3d_weight(iarr).NE.' ') THEN
        IF (xx_genarr3d_file(iarr)(1:8).EQ.'xx_theta')
     &     igen_theta0=iarr
        IF (xx_genarr3d_file(iarr)(1:7).EQ.'xx_salt')
     &     igen_salt0=iarr
        IF (xx_genarr3d_file(iarr)(1:8).EQ.'xx_kapgm')
     &     igen_kapgm=iarr
        IF (xx_genarr3d_file(iarr)(1:10).EQ.'xx_kapredi')
     &     igen_kapredi=iarr
        IF (xx_genarr3d_file(iarr)(1:9).EQ.'xx_diffkr')
     &     igen_diffkr=iarr
# if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
        IF (xx_genarr3d_file(iarr)(1:7).EQ.'xx_uvel')
     &     igen_uvel0=iarr
        IF (xx_genarr3d_file(iarr)(1:7).EQ.'xx_vvel')
     &     igen_vvel0=iarr
# endif
# ifdef ALLOW_PTRACERS
        IF ( usePTRACERS ) THEN
          iLen = ILNBLNK(xx_genarr3d_file(iarr))
          IF ( iLen.EQ.7 .AND.
     &         xx_genarr3d_file(iarr)(1:6).EQ.'xx_ptr' ) THEN
            READ(xx_genarr3d_file(iarr)(7:7),*) iPtr
            IF ( iPtr.GE.1 .AND. iPtr.LE.PTRACERS_numInUse )
     &                  igen_ptr(iPtr) = iarr
          ENDIF
        ENDIF
# endif /* ALLOW_PTRACERS */
       ENDIF
      ENDDO

      IF (igen_theta0.GT.0)
     &  CALL CTRL_MAP_GENARR3D( theta, igen_theta0, myThid )
      IF (igen_salt0.GT.0)
     &  CALL CTRL_MAP_GENARR3D( salt, igen_salt0, myThid )
# ifdef ALLOW_KAPGM_CONTROL
      IF (igen_kapgm.GT.0)
     &  CALL CTRL_MAP_GENARR3D( kapGM, igen_kapgm, myThid )
# endif
# ifdef ALLOW_KAPREDI_CONTROL
      IF (igen_kapredi.GT.0)
     &  CALL CTRL_MAP_GENARR3D( kapRedi, igen_kapredi, myThid )
# endif
# if ( defined ALLOW_DIFFKR_CONTROL && defined ALLOW_3D_DIFFKR )
      IF (igen_diffkr.GT.0)
     &  CALL CTRL_MAP_GENARR3D( diffKr, igen_diffkr, myThid )
# endif

# ifdef ALLOW_PTRACERS
CADJ loop = parallel
      DO iPtr = 1, PTRACERS_num
        IF ( igen_ptr(iPtr).GT.0 ) THEN
          CALL CTRL_MAP_GENARR3D( pTracer(1-OLx,1-OLy,1,1,1,iPtr),
     &                            igen_ptr(iPtr), myThid )
        ENDIF
      ENDDO
# endif /* ALLOW_PTRACERS */

# if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      IF (igen_uvel0.GT.0 .AND. igen_vvel0.GT.0) THEN
        CALL CTRL_MAP_GENARR3D( uVel, igen_uvel0, myThid )
        CALL CTRL_MAP_GENARR3D( vVel, igen_vvel0, myThid )
        CALL EXCH_UV_XYZ_RL( uVel, vVel, .TRUE., myThid )
      ENDIF
# endif

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: CTRL_MAP_GENARR2D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_GENARR2D( fld, iarr, myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_MAP_GENARR2D
C     | Add the generic arrays of the
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
      _RL xx_gen  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dummyRS(1)
      CHARACTER*(80) fnamegenIn
      CHARACTER*(80) fnamegenOut
      CHARACTER*(80) fnamebase
      INTEGER ilgen
      LOGICAL doglobalread
      LOGICAL ladinit
      _RS mask2D (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEOP

c--   Now, read the control vector.
      doglobalread = .FALSE.
      ladinit      = .FALSE.

      CALL CTRL_ASSIGN(xx_gen, 1, zeroRL, myThid)

      dosmooth=.FALSE.
      dowc01  = .FALSE.
      doscaling=.TRUE.
      dolog10ctrl=.FALSE.
      log10initval=0. _d 0

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
c             fld := log10(xx_gen)
c               with initial guess for xx_gen set to log10initval
c               passed to data.ctrl, (default = 0.0)
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

      CALL MDS_READ_FIELD(xx_genarr2d_weight(iarr),ctrlprec,.FALSE.,
     &     'RL',1,1,1,wgenarr2d(1-OLx,1-OLy,1,1,iarr),dummyRS,1,myThid)

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
        DO j = 1,sNy
         DO i = 1,sNx
c scale param adjustment
            IF ( (mask2D(i,j,bi,bj).NE.0.).AND.
     &           (wgenarr2d(i,j,bi,bj,iarr).GT.0.) ) THEN
              IF (doscaling) THEN
                  xx_gen(i,j,bi,bj) = xx_gen(i,j,bi,bj)
     &                     /SQRT( wgenarr2d(i,j,bi,bj,iarr) )
                IF (dolog10ctrl) THEN
                  xx_gen(i,j,bi,bj) = xx_gen(i,j,bi,bj) + log10initval
                  xx_gen(i,j,bi,bj) = 10.0 ** xx_gen(i,j,bi,bj)
                ENDIF ! log10ctrls
              ENDIF ! doscaling
            ELSE
              xx_gen(i,j,bi,bj) = 0.
            ENDIF
c add to model parameter
c or in case of log10ctrl, fld = 10^(xx_gen)
            IF ( .NOT. dolog10ctrl ) THEN
              fld(i,j,bi,bj) = fld(i,j,bi,bj)+xx_gen(i,j,bi,bj)
            ELSE
              fld(i,j,bi,bj) = xx_gen(i,j,bi,bj)
            ENDIF
          ENDDO
         ENDDO
       ENDDO
      ENDDO

c avoid param out of [boundsVec(1) boundsVec(4)]
      CALL CTRL_BOUND_2D(fld,mask2D,xx_genarr2d_bounds(1,iarr),myThid)

      CALL EXCH_XY_RL( fld, myThid )

      CALL MDS_WRITE_FIELD( fnamegenOut, ctrlprec, .FALSE., .FALSE.,
     &         'RL', 1, 1, 1, fld, dummyRS, 1, optimcycle, myThid )

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
C     | Add the generic arrays of the
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
      _RL xx_gen(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS dummyRS(1)
      CHARACTER*(80) fnamegenIn
      CHARACTER*(80) fnamegenOut
      CHARACTER*(80) fnamebase
      INTEGER ilgen
      LOGICAL doglobalread
      LOGICAL ladinit
      _RS mask3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nr,nSx,nSy)
CEOP

c--   Now, read the control vector.
      doglobalread = .FALSE.
      ladinit      = .FALSE.

      CALL CTRL_ASSIGN(xx_gen, Nr, zeroRL, myThid)

      dosmooth=.FALSE.
      dowc01  = .FALSE.
      doscaling=.TRUE.
      dolog10ctrl=.FALSE.
      log10initval=0. _d 0

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
c             fld := log10(xx_gen)
c               with initial guess for xx_gen set to log10initval
c               passed to data.ctrl, (default = 0.0)
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

      CALL MDS_READ_FIELD(xx_genarr3d_weight(iarr),ctrlprec,.FALSE.,
     & 'RL',nR,1,nR,wgenarr3d(1-OLx,1-OLy,1,1,1,iarr),dummyRS,1,myThid)

#ifdef ALLOW_AUTODIFF
      CALL ACTIVE_READ_XYZ( fnamegenIn, xx_gen, 1, doglobalread,
     & ladinit, optimcycle, myThid, xx_genarr3d_dummy(iarr) )
#else
      CALL READ_REC_XYZ_RL( fnamegenIn, xx_gen, 1, 1, myThid)
#endif

#ifdef ALLOW_SMOOTH
      IF (useSMOOTH) THEN
       IF (dowc01) CALL SMOOTH_CORREL3D( xx_gen, numsmo, myThid )
       IF (dosmooth) CALL SMOOTH3D( xx_gen, numsmo, myThid )
      ENDIF
#endif

C --- Get appropriate mask
      CALL CTRL_GET_MASK3D(xx_genarr3d_file(iarr), mask3D, myThid)

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k = 1,nr
         DO j = 1,sNy
          DO i = 1,sNx
c scale param adjustment
            IF ( (mask3D(i,j,k,bi,bj).NE.0.).AND.
     &           (wgenarr3d(i,j,k,bi,bj,iarr).GT.0.) ) THEN
             IF (doscaling) THEN
              xx_gen(i,j,k,bi,bj)=xx_gen(i,j,k,bi,bj)
     &                 /SQRT( wgenarr3d(i,j,k,bi,bj,iarr) )
              IF (dolog10ctrl) THEN
               xx_gen(i,j,k,bi,bj) =
     &              xx_gen(i,j,k,bi,bj) + log10initval
               xx_gen(i,j,k,bi,bj) = 10.0 ** xx_gen(i,j,k,bi,bj)
              ENDIF ! log10ctrls
             ENDIF ! doscaling
            ELSE
              xx_gen(i,j,k,bi,bj)=0.
            ENDIF
c add to model parameter
c or in case of log10ctrl, fld = 10^(xx_gen)
            IF ( .NOT. dolog10ctrl ) THEN
             fld(i,j,k,bi,bj)=fld(i,j,k,bi,bj)+xx_gen(i,j,k,bi,bj)
            ELSE
             fld(i,j,k,bi,bj)=xx_gen(i,j,k,bi,bj)
            ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

c avoid param out of [boundsVec(1) boundsVec(4)]
      CALL CTRL_BOUND_3D(fld,mask3D,xx_genarr3d_bounds(1,iarr),myThid)

C The tile exchange for xx_uvel and xx_vvel will be
C  done in CTRL_MAP_INI_GENARR.F when both
C  xx_uvel and xx_vvel are read in.
      IF (xx_genarr3d_file(iarr)(1:7).NE.'xx_uvel'.AND.
     &    xx_genarr3d_file(iarr)(1:7).NE.'xx_vvel')
     &    CALL EXCH_XYZ_RL( fld, myThid )

      CALL MDS_WRITE_FIELD( fnamegenOut, ctrlprec, .FALSE., .FALSE.,
     &         'RL', Nr, 1, Nr, fld, dummyRS, 1, optimcycle, myThid )

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END
