C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_common_factors.F,v 1.12 2000/06/09 14:26:30 heimbach Exp $

#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CALC_COMMON_FACTORS( 
     I        bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     O        xA,yA,uTrans,vTrans,rTrans,rVel,maskC,maskUp,
     I        myThid)

C     /==========================================================\
C     | SUBROUTINE CALC_COMMON_FACTORS                           |
C     | o Calculate common data (such as volume flux) for use    |
C     |   by "Right hand side" subroutines.                      |
C     |==========================================================|
C     | Here, we calculate terms or spatially varying factors    |
C     | that are used at various points in the "RHS" subroutines.|
C     | This reduces the amount of total work, total memory      |
C     | and therefore execution time and is generally a good     |
C     | idea.                                                    |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "GW.h"
#endif

C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     xA      - Tracer cell face area normal to X
C     yA      - Tracer cell face area normal to X
C     uTrans  - Zonal volume transport through cell face
C     vTrans  - Meridional volume transport through cell face
C     rTrans  - R-direction volume transport through cell face
C     rVel    - R-direction velocity at cell upper and lower faces
C     maskC   - land/water mask for tracer points
C     maskUp  - land/water mask for Wvel points (above tracer level)
C     myThid - Instance number for this innvocation of CALC_COMMON_FACTORS
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS maskC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J, K - Loop counters
C     kUp, kDown, kM1 - Index for layer above and below. K_UP and K_DOWN
C                         are switched with layer to be the appropriate index 
C                         into fluxUD.
      INTEGER i,j
      LOGICAL TOP_LAYER

#ifdef ALLOW_AUTODIFF_TAMC
C--   rvel(:,:kDown) is still required
      rVel(1,1,kDown) = rVel(1,1,kDown)
#endif

      TOP_LAYER = K .EQ. 1

C--   Calculate mask for tracer cells  (0 => land, 1 => water)
      DO j=jMin,jMax
       DO i=iMin,iMax
        maskC(i,j) = 1.
        IF (_hFacC(i,j,k,bi,bj).eq.0.) maskC(i,j)=0.
        maskUp(i,j) = 1.
        IF (_hFacC(i,j,k,bi,bj).eq.0. .OR. TOP_LAYER ) 
     &    maskUp(i,j)=0.
       ENDDO
      ENDDO

C--   Calculate tracer cell face open areas
      DO j=jMin,jMax
       DO i=iMin,iMax
        xA(i,j) = _dyG(i,j,bi,bj)
     &   *drF(k)*_hFacW(i,j,k,bi,bj)
        yA(i,j) = _dxG(i,j,bi,bj)
     &   *drF(k)*_hFacS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C--   Calculate velocity field "volume transports" through
C--   tracer cell faces.
      DO j=jMin,jMax
       DO i=iMin,iMax
        uTrans(i,j) = uVel(i,j,k,bi,bj)*xA(i,j)
        vTrans(i,j) = vVel(i,j,k,bi,bj)*yA(i,j)
       ENDDO
      ENDDO

C--   Calculate vertical "volume transport" through
C--   tracer cell face *above* this level.
      IF (TOP_LAYER .AND. rigidLid) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         rTrans(i,j) = 0.
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         rTrans(i,j) = 
     &    uTrans(i,j)*recip_rkFac-uTrans(i+1,j)*recip_rkFac
     &   +vTrans(i,j)*recip_rkFac-vTrans(i,j+1)*recip_rkFac
     &   +rTrans(i,j)
        ENDDO
       ENDDO
      ENDIF

C--   Vertical velocity at upper face
      DO j=jMin,jMax
       DO i=iMin,iMax
        rVel(i,j,kUp) = rTrans(i,j)/_rA(i,j,bi,bj)
       ENDDO
      ENDDO

#ifdef ALLOW_NONHYDROSTATIC
C--   Vertical velocity at upper face
C     IF ( nonHydrostatic ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          wVel(i,j,k,bi,bj)=rVel(i,j,kUp)
         ENDDO
        ENDDO
C     ENDIF
#endif

      RETURN
      END
