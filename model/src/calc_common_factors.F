C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_common_factors.F,v 1.18 2001/09/26 18:09:13 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_COMMON_FACTORS
C     !INTERFACE: 
      SUBROUTINE CALC_COMMON_FACTORS( 
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     O        xA,yA,uTrans,vTrans,rTrans,maskUp,
     I        myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_COMMON_FACTORS                            
C     | o Calculate common data (such as volume flux) for use     
C     |   by "Right hand side" subroutines.                       
C     *==========================================================*
C     | Here, we calculate terms or spatially varying factors     
C     | that are used at various points in the "RHS" subroutines. 
C     | This reduces the amount of total work, total memory       
C     | and therefore execution time and is generally a good      
C     | idea.                                                     
C     *==========================================================*
C     \ev

C     !USES:
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax :: Range of points for which calculation
C                                      results will be set.
C     xA      :: Tracer cell face area normal to X
C     yA      :: Tracer cell face area normal to X
C     uTrans  :: Zonal volume transport through cell face
C     vTrans  :: Meridional volume transport through cell face
C     rTrans  :: R-direction volume transport through cell face
C     maskUp  :: land/water mask for Wvel points (above tracer level)
C     myThid  ::Instance number for this innvocation of CALC_COMMON_FACTORS
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I, J :: Loop counters
      INTEGER i,j
CEOP

C--   Initialisation
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        xA(i,j)      = 0. _d 0
        yA(i,j)      = 0. _d 0
        uTrans(i,j)  = 0. _d 0
        vTrans(i,j)  = 0. _d 0
        rTrans(i,j)  = 0. _d 0
       ENDDO
      ENDDO      

C--   Calculate mask for tracer cells  (0 => land, 1 => water)
      IF (K .EQ. 1) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          maskUp(i,j) = 0.
         ENDDO
        ENDDO
      ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          maskUp(i,j) = maskC(i,j,k-1,bi,bj)*maskC(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

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
      DO j=jMin,jMax
        DO i=iMin,iMax
          rTrans(i,j) = wVel(i,j,k,bi,bj)*rA(i,j,bi,bj)
        ENDDO
      ENDDO

      RETURN
      END
