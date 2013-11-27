C $Header: /u/gcmpack/MITgcm/model/src/calc_adv_flow.F,v 1.2 2013/11/27 23:58:24 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_ADV_FLOW
C     !INTERFACE:
      SUBROUTINE CALC_ADV_FLOW(
     I                uFld, vFld, wFld,
     U                rTrans,
     O                uTrans, vTrans, rTransKp,
     O                maskUp, xA, yA,
     I                k, bi, bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_ADV_FLOW
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
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     uFld     :: 3-D local copy of horizontal velocity, zonal  component
C     vFld     :: 3-D local copy of horizontal velocity, merid. component
C     wFld     :: 3-D local copy of vertical velocity
C     rTrans   :: Vertical volume transport through interface k
C     uTrans   :: Zonal volume transport through cell face
C     vTrans   :: Meridional volume transport through cell face
C     rTransKp :: Vertical volume transport through interface k+1
C     maskUp   :: Land/water mask for Wvel points (interface k)
C     xA       :: Tracer cell face area normal to X
C     yA       :: Tracer cell face area normal to X
C     k,bi,bj  :: vertical & tile indices for this calculation
C     myThid   :: my Thread Id. number

      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k,bi,bj
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j :: Loop counters
      INTEGER i,j
CEOP

C--   Calculate tracer cell face open areas
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         xA(i,j) = _dyG(i,j,bi,bj)*deepFacC(k)
     &           *drF(k)*_hFacW(i,j,k,bi,bj)
         yA(i,j) = _dxG(i,j,bi,bj)*deepFacC(k)
     &           *drF(k)*_hFacS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C--   copy previous rTrans (input) to output array rTransKp
      IF ( k.EQ.Nr ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          rTransKp(i,j) = 0. _d 0
         ENDDO
        ENDDO
      ELSE
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifdef ALLOW_AUTODIFF
C-    Re-compute vertical transport: this changes "rTrans" to be
C     an output only argument and therefore simplifies dependencies
          rTransKp(i,j) = wFld(i,j,k+1)*rA(i,j,bi,bj)
     &                  * maskC(i,j,k,bi,bj)*maskC(i,j,k+1,bi,bj)
     &                  * deepFac2F(k+1)*rhoFacF(k+1)
#else /* ALLOW_AUTODIFF */
C-    Copy rTrans value from previous call (i.e., k+1):
          rTransKp(i,j) = rTrans(i,j)
#endif /* ALLOW_AUTODIFF */
         ENDDO
        ENDDO
      ENDIF

C--   Calculate "volume transports" through tracer cell faces.
C     anelastic: scaled by rhoFacC (~ mass transport)
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         uTrans(i,j) = uFld(i,j,k)*xA(i,j)*rhoFacC(k)
         vTrans(i,j) = vFld(i,j,k)*yA(i,j)*rhoFacC(k)
       ENDDO
      ENDDO

C--   Calculate vertical "volume transport" through tracer cell face
      IF (k.EQ.1) THEN
C-      Surface interface :
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           maskUp(i,j) = 0. _d 0
           rTrans(i,j) = 0. _d 0
         ENDDO
        ENDDO
      ELSE
C-      Interior interface :
C       anelastic: rTrans is scaled by rhoFacF (~ mass transport)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           maskUp(i,j) = maskC(i,j,k-1,bi,bj)*maskC(i,j,k,bi,bj)
           rTrans(i,j) = wFld(i,j,k)*rA(i,j,bi,bj)*maskUp(i,j)
     &                              *deepFac2F(k)*rhoFacF(k)
         ENDDO
        ENDDO
      ENDIF

      RETURN
      END
