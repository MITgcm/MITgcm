#include "THSICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_DO_ADVECT
C     !INTERFACE:
      SUBROUTINE THSICE_DO_ADVECT(
     I                  biArg, bjArg, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE THSICE_DO_ADVECT
C     | o wraper for pkg/thSIce advection-diffusion calls
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     biArg     :: Tile 1rst index argument
C     bjArg     :: Tile 2nd  index argument
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id. number
      INTEGER biArg, bjArg
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     bi, bj    :: Tile indices
C     uIce/vIce :: ice velocity on C-grid [m/s]
      INTEGER bi, bj
      INTEGER i, j
      INTEGER iMin, iMax, jMin, jMax
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ticekey
#endif
      _RL  uIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  vIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      IF ( thSIceAdvScheme.GT.0 .AND. biArg.EQ.0 .AND. bjArg.EQ.0 ) THEN
c      iMin = 1
c      iMax = sNx
c      jMin = 1
c      jMax = sNy
       iMin = 1-OLx
       iMax = sNx+OLx-1
       jMin = 1-OLy
       jMax = sNy+OLy-1
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_AUTODIFF_TAMC
         act1 = bi - myBxLo(myThid)
         max1 = myBxHi(myThid) - myBxLo(myThid) + 1
         act2 = bj - myByLo(myThid)
         max2 = myByHi(myThid) - myByLo(myThid) + 1
         act3 = myThid - 1
         max3 = nTx*nTy
         act4 = ikey_dynamics - 1
         ticekey = (act1 + 1) + act2*max1
     &                        + act3*max1*max2
     &                        + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

         CALL THSICE_GET_VELOCITY(
     O                        uIce, vIce,
     I                        bi,bj, myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE icemask(:,:,bi,bj) =
CADJ &     comlev1_bibj, key=ticekey, byte=isbyte
CADJ STORE qice1(:,:,bi,bj) =
CADJ &     comlev1_bibj, key=ticekey, byte=isbyte
CADJ STORE hOceMxL(:,:,bi,bj) =
CADJ &     comlev1_bibj, key=ticekey, byte=isbyte
#endif
         CALL THSICE_ADVDIFF(
     U                        uIce, vIce,
     I                        bi,bj, myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hOceMxL(:,:,bi,bj) =
CADJ &     comlev1_bibj, key=ticekey, byte=isbyte
CADJ STORE snowHeight(:,:,bi,bj) =
CADJ &     comlev1_bibj, key=ticekey, byte=isbyte
CADJ STORE iceHeight(:,:,bi,bj) =
CADJ &     comlev1_bibj, key=ticekey, byte=isbyte
CADJ STORE iceMask(:,:,bi,bj) =
CADJ &     comlev1_bibj, key=ticekey, byte=isbyte
#endif
         DO j = jMin, jMax
          DO i = iMin, iMax
           IF ( hOceMxL(i,j,bi,bj).GT.0. _d 0 ) THEN
            Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj) - oceQnet(i,j,bi,bj)
            EmPmR(i,j,bi,bj)= EmPmR(i,j,bi,bj)- oceFWfx(i,j,bi,bj)
            saltFlux(i,j,bi,bj)=saltFlux(i,j,bi,bj) - oceSflx(i,j,bi,bj)
           ENDIF
C--     Compute Sea-Ice Loading (= mass of sea-ice + snow / area unit)
           sIceLoad(i,j,bi,bj) = ( snowHeight(i,j,bi,bj)*rhos
     &                           + iceHeight(i,j,bi,bj)*rhoi
     &                           )*iceMask(i,j,bi,bj)
          ENDDO
         ENDDO

C--     cumulate time-averaged fields and also fill-up flux diagnostics
         CALL THSICE_AVE(
     I                     bi,bj, myTime, myIter, myThid )

        ENDDO
       ENDDO

       IF ( stressReduction.GT. 0. _d 0 )
     &   _EXCH_XY_RL( iceMask, myThid )
       IF ( useRealFreshWaterFlux )
     &  _EXCH_XY_RS( sIceLoad, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      ENDIF

      RETURN
      END
