C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_common_factors.F,v 1.3 1998/05/27 05:18:39 cnh Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterFace
      SUBROUTINE CALC_COMMON_FACTORS( 
     I        bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     O        xA,yA,uTrans,vTrans,wTrans,maskC,maskUp,
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
C     | We also think lower taxes are a good idea but we doubt   |
C     | whether we'll ever get them.                             |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     xA      - Tracer cell face area normal to X
C     yA      - Tracer cell face area normal to X
C     uTrans  - Zonal volume transport through cell face
C     vTrans  - Meridional volume transport through cell face
C     wTrans  - Vertical volume transport through cell face
C     maskC   - land/water mask for tracer points
C     maskUp  - land/water mask for Wvel points (above tracer level)
C     myThid - Instance number for this innvocation of CALC_COMMON_FACTORS
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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

C--    Calculate tracer cell face open areas
      DO j=jMin,jMax
       DO i=iMin,iMax
        xA(i,j) = dyG(i,j,bi,bj)*dzF(k)*hFacW(i,j,k,bi,bj)
        yA(i,j) = _dxG(i,j,bi,bj)*dzF(k)*hFacS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C--    Calculate velocity field "volume transports" through
C--    tracer cell faces.
      DO j=jMin,jMax
       DO i=iMin,iMax
        uTrans(i,j) = uVel(i,j,k,bi,bj)*xA(i,j)
        vTrans(i,j) = vVel(i,j,k,bi,bj)*yA(i,j)
       ENDDO
      ENDDO

C--    Calculate vertical "volume transport" through
C--    tracer cell face *above* this level.
      DO j=jMin,jMax
       DO i=iMin,iMax
        wTrans(i,j) = uTrans(i,j)-uTrans(i+1,j)
     &               +vTrans(i,j)-vTrans(i,j+1)
     &               +wTrans(i,j)
       ENDDO
      ENDDO

C--    Calculate mask for tracer cells  (0 => land, 1 => water)
      DO j=jMin,jMax
       DO i=iMin,iMax
        maskC(i,j) = 1.
        if (hFacC(i,j,k,bi,bj).eq.0.) maskC(i,j)=0.
        maskUp(i,j) = 1.
        if (hFacC(i,j,kM1,bi,bj).eq.0.) maskUp(i,j)=0.
       ENDDO
      ENDDO

      RETURN
      END
