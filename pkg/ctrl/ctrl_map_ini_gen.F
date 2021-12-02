#include "CTRL_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C--  File ctrl_map_ini_gen.F:
C--   Contents
C--   o CTRL_MAP_INI_GEN3D
C--   o CTRL_MAP_INI_GEN2D

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: CTRL_MAP_INI_GEN3D

C !INTERFACE:
      SUBROUTINE CTRL_MAP_INI_GEN3D( xxFileCur, wFileCur, xxDummyCur,
     &        boundsVec, paramFld3d, maskFld3d, paramSmooth, myThid )

C     !DESCRIPTION:
C     ==================================================================
C     Generetic routine for an individual 3D control term
C      (to be called from ctrl_map_ini in a loop e.g.)
C     ==================================================================
C     started: Gael Forget gforget@mit.edu 8-Feb-2008
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "optim.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) xxFileCur, wFileCur
      _RL xxDummyCur, boundsVec(5)
      _RL paramFld3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS maskFld3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER paramSmooth
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER bi, bj
      INTEGER i, j, k, il
      LOGICAL doglobalread, ladinit
      _RL tmpMax
      _RL wFld3d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL xxFld3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*( 80) fnamegeneric
#ifdef ALLOW_OPENAD
      _RS dummyRS(1)
#endif
C--   only do interior, and exchange outside
      INTEGER iMin, iMax, jMin, jMax
      PARAMETER( iMin = 1, iMax = sNx, jMin = 1, jMax = sNy )
CEOP

      doglobalread = .FALSE.
      ladinit      = .FALSE.

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO k = 1,Nr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           xxFld3d(i,j,k,bi,bj) = 0. _d 0
           wFld3d(i,j,k,bi,bj)  = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_OPENAD
      CALL MDS_READ_FIELD( wFileCur, ctrlprec, .FALSE., 'RL',
     &                     Nr, 1, Nr, wFld3d, dummyRS, 1, myThid )
#else
      CALL READ_REC_3D_RL( wFileCur,ctrlprec,Nr, wFld3d, 1,1,myThid )
#endif
      _EXCH_XYZ_RL( wFld3d, myThid )

      il = ILNBLNK( xxFileCur )
      WRITE(fnamegeneric(1:80),'(2A,I10.10)')
     &      xxFileCur(1:il), '.', optimcycle
#ifdef ALLOW_AUTODIFF
      CALL ACTIVE_READ_XYZ( fnamegeneric, xxFld3d, 1, doglobalread,
     &                      ladinit, optimcycle, myThid, xxDummyCur )
#else
      CALL READ_REC_XYZ_RL( fnamegeneric, xxFld3d, 1, 1, myThid )
#endif

      IF ( ctrlSmoothCorrel3D ) THEN

#ifdef ALLOW_SMOOTH
C-    apply Weaver And Courtier correlation operator
        IF ( paramSmooth.NE.0 .AND. useSMOOTH ) THEN
         CALL SMOOTH_CORREL3D( xxFld3d, paramSmooth, myThid )
        ENDIF
#endif

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1,Nr
           DO j = jMin,jMax
            DO i = iMin,iMax
C-    scale param adjustment
             IF ( ( maskFld3d(i,j,k,bi,bj).NE.zeroRS )
     &          .AND. ( wFld3d(i,j,k,bi,bj).GT.zeroRL ) ) THEN
              xxFld3d(i,j,k,bi,bj) = xxFld3d(i,j,k,bi,bj)
     &                             / SQRT( wFld3d(i,j,k,bi,bj) )
             ELSE
              xxFld3d(i,j,k,bi,bj) = 0. _d 0
             ENDIF
             paramFld3d(i,j,k,bi,bj) = paramFld3d(i,j,k,bi,bj)
     &                               + xxFld3d(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      ELSE

C-    avoid xx larger than boundsVec(5) X uncertainty
        IF ( boundsVec(5).GT.0. ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k = 1,Nr
            DO j = jMin,jMax
             DO i = iMin,iMax
              IF ( ( maskFld3d(i,j,k,bi,bj).NE.zeroRS )
     &           .AND. ( wFld3d(i,j,k,bi,bj).GT.zeroRL ) ) THEN
               tmpMax = boundsVec(5)/SQRT(wFld3d(i,j,k,bi,bj))
               IF ( ABS(xxFld3d(i,j,k,bi,bj)).GT.tmpMax ) THEN
                xxFld3d(i,j,k,bi,bj) = SIGN(tmpMax,xxFld3d(i,j,k,bi,bj))
               ELSE
                xxFld3d(i,j,k,bi,bj) = xxFld3d(i,j,k,bi,bj)
               ENDIF
              ENDIF
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF

#ifdef ALLOW_SMOOTH
# ifdef ALLOW_SMOOTH_CTRL3D
        IF (useSMOOTH) CALL SMOOTH3D( xxFld3d, paramSmooth, myThid )
        WRITE(fnamegeneric(1:80),'(2A,I10.10)')
     &        xxFileCur(1:il), '.smooth.', optimcycle
        CALL WRITE_REC_3D_RL( fnamegeneric, ctrlprec, Nr,
     &                        xxFld3d, 1, optimcycle, myThid )
# endif
#endif

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1,Nr
           DO j = jMin,jMax
            DO i = iMin,iMax
              paramFld3d(i,j,k,bi,bj) = paramFld3d(i,j,k,bi,bj)
     &                                + xxFld3d(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C-    end if ctrlSmoothCorrel3D / else
      ENDIF

C-    avoid param out of [boundsVec(1) boundsVec(4)]
      CALL CTRL_BOUND_3D( paramFld3d, maskFld3d, boundsVec, myThid )

      IF ( ctrlSmoothCorrel3D ) THEN
        WRITE(fnamegeneric(1:80),'(2A,I10.10)')
     &        xxFileCur(1:il), '.effective.', optimcycle
        CALL WRITE_REC_3D_RL( fnamegeneric, ctrlprec, Nr,
     &                        paramFld3d, 1, optimcycle, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: CTRL_MAP_INI_GEN2D

C !INTERFACE:
      SUBROUTINE CTRL_MAP_INI_GEN2D(xxFileCur, wFileCur, xxDummyCur,
     &        boundsVec, paramFld2d, maskFld2d, paramSmooth, myThid )

C     !DESCRIPTION:
C     ==================================================================
C     Generetic routine for an individual 2D control term
C      (to be called from ctrl_map_ini in a loop e.g.)
C     ==================================================================
C     started: Gael Forget gforget@mit.edu 8-Feb-2008
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "optim.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) xxFileCur, wFileCur
      _RL xxDummyCur, boundsVec(5)
      _RL paramFld2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS maskFld2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER paramSmooth
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER bi, bj
      INTEGER i, j, il
      LOGICAL doglobalread, ladinit
      _RL tmpMax
      _RL wFld2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL xxFld2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*( 80) fnamegeneric
#ifdef ALLOW_OPENAD
      _RS dummyRS(1)
#endif
C--   only do interior, and exchange outside
      INTEGER iMin, iMax, jMin, jMax
      PARAMETER( iMin = 1, iMax = sNx, jMin = 1, jMax = sNy )
CEOP

      doglobalread = .FALSE.
      ladinit      = .FALSE.

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
           xxFld2d(i,j,bi,bj) = 0. _d 0
           wFld2d(i,j,bi,bj)  = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_OPENAD
      CALL MDS_READ_FIELD( wFileCur, ctrlprec, .FALSE., 'RL',
     &                     1, 1, 1, wFld2d, dummyRS, 1, myThid )
#else
      CALL READ_REC_3D_RL( wFileCur, ctrlprec, 1, wFld2d, 1,1,myThid )
#endif
      _EXCH_XY_RL( wFld2d, myThid )

      il = ILNBLNK( xxFileCur )
      WRITE(fnamegeneric(1:80),'(2A,I10.10)')
     &      xxFileCur(1:il), '.', optimcycle
#ifdef ALLOW_AUTODIFF
      CALL ACTIVE_READ_XY( fnamegeneric, xxFld2d, 1, doglobalread,
     &                     ladinit, optimcycle, myThid, xxDummyCur )
#else
      CALL READ_REC_XY_RL( fnamegeneric, xxFld2d, 1, 1, myThid )
#endif

      IF ( ctrlSmoothCorrel2D ) THEN

#ifdef ALLOW_SMOOTH
C-    apply Weaver And Courtier correlation operator
        IF ( paramSmooth.NE.0 .AND. useSMOOTH ) THEN
         CALL SMOOTH_CORREL2D( xxFld2d, maskFld2d,
     &                         paramSmooth, myThid )
        ENDIF
#endif

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = jMin,jMax
            DO i = iMin,iMax
C-    scale param adjustment
             IF ( ( maskFld2d(i,j,bi,bj).NE.zeroRS )
     &          .AND. ( wFld2d(i,j,bi,bj).GT.zeroRL ) ) THEN
              xxFld2d(i,j,bi,bj) = xxFld2d(i,j,bi,bj)
     &                           / SQRT( wFld2d(i,j,bi,bj) )
             ELSE
              xxFld2d(i,j,bi,bj) = 0. _d 0
             ENDIF
             paramFld2d(i,j,bi,bj) = paramFld2d(i,j,bi,bj)
     &                             + xxFld2d(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDDO
        ENDDO

      ELSE

C-    avoid xx larger than boundsVec(5) X uncertainty
        IF ( boundsVec(5).GT.0.) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j = jMin,jMax
             DO i = iMin,iMax
              IF ( ( maskFld2d(i,j,bi,bj).NE.zeroRS )
     &           .AND. ( wFld2d(i,j,bi,bj).GT.zeroRL ) ) THEN
               tmpMax = boundsVec(5)/SQRT(wFld2d(i,j,bi,bj))
               IF ( ABS(xxFld2d(i,j,bi,bj)).GT.tmpMax ) THEN
                xxFld2d(i,j,bi,bj) = SIGN(tmpMax,xxFld2d(i,j,bi,bj))
               ELSE
                xxFld2d(i,j,bi,bj) = xxFld2d(i,j,bi,bj)
               ENDIF
              ENDIF
             ENDDO
            ENDDO
          ENDDO
         ENDDO
        ENDIF

#ifdef ALLOW_SMOOTH
# ifdef ALLOW_SMOOTH_CTRL2D
        IF (useSMOOTH) CALL SMOOTH2D( xxFld2d, maskFld2d,
     &                                paramSmooth, myThid )
        WRITE(fnamegeneric(1:80),'(2A,I10.10)')
     &        xxFileCur(1:il), '.smooth.', optimcycle
        CALL WRITE_REC_3D_RL( fnamegeneric, ctrlprec, 1,
     &                        xxFld2d, 1, optimcycle, myThid )
# endif
#endif

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = jMin,jMax
            DO i = iMin,iMax
             paramFld2d(i,j,bi,bj) = paramFld2d(i,j,bi,bj)
     &                             + xxFld2d(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDDO
        ENDDO

C-    end if ctrlSmoothCorrel2D / else
      ENDIF

      CALL CTRL_BOUND_2D( paramFld2d, maskFld2d, boundsVec, myThid )

      IF ( ctrlSmoothCorrel2D ) THEN
        WRITE(fnamegeneric(1:80),'(2A,I10.10)')
     &        xxFileCur(1:il), '.effective.', optimcycle
        CALL WRITE_REC_3D_RL( fnamegeneric, ctrlprec, 1,
     &                        paramFld2d, 1, optimcycle, myThid )
      ENDIF

      RETURN
      END
