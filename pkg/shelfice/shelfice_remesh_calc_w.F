#include "SHELFICE_OPTIONS.h"

CBOP
C     !ROUTINE: SHELFICE_REMESH_CALC_W
C     !INTERFACE:
      SUBROUTINE SHELFICE_REMESH_CALC_W(
     I                             uFld, vFld,
     O                             wFld,
     I                             myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_REMESH_CALC_W
C     | o Integrate the continuity Eq : compute vertical velocity
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "DYNVARS.h"
#include "GRID.h"
c#include "SURFACE.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     uFld     :: Zonal velocity ( m/s )
C     vFld     :: Meridional velocity ( m/s )
C     wFld     :: Vertical velocity ( m/s )
C     myIter   :: Current iteration number
C     myThid   :: my Thread Id. number
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SHELFICE_REMESHING

C     !LOCAL VARIABLES:
C     bi,bj    :: tile index
C     i,j,k    :: Loop counters
C     hConv3d  :: horizontal transport convergence [wVel units, m/s]
      _RL uTrans(1:sNx+1,1:sNy+1)
      _RL vTrans(1:sNx+1,1:sNy+1)
      _RL hConv3d(1:sNx,1:sNy,Nr)
      _RL locFac
      INTEGER bi, bj
      INTEGER i, j, k
#if (defined NONLIN_FRSURF) && !(defined DISABLE_RSTAR_CODE)
      _RL rStarDhDt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#else
      _RL rStarDhDt(1)
#endif
CEOP

C--   Start bi,bj loops
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Compute the Convergence of horizontal Flow :
      DO k=1,Nr

C-    Calculate velocity field "volume transports" through tracer cell faces
C     anelastic: uTrans,vTrans are scaled by rhoFacC (~ mass transport).
        locFac = drF(k)*deepFacC(k)*rhoFacC(k)
        DO j=1,sNy+1
         DO i=1,sNx+1
          uTrans(i,j) = uFld(i,j,k,bi,bj)*_dyG(i,j,bi,bj)
     &                 *locFac*_hFacW(i,j,k,bi,bj)
          vTrans(i,j) = vFld(i,j,k,bi,bj)*_dxG(i,j,bi,bj)
     &                 *locFac*_hFacS(i,j,k,bi,bj)
         ENDDO
        ENDDO

C-    horizontal transport convergence [in wVel units]
        DO j=1,sNy
         DO i=1,sNx
          hConv3d(i,j,k) = -( uTrans(i+1,j) - uTrans(i,j)
     &                      + vTrans(i,j+1) - vTrans(i,j) )
         ENDDO
        ENDDO
        IF ( selectAddFluid.GE.1 .AND. myIter.NE.0 ) THEN
#ifdef ALLOW_ADDFLUID
         DO j=1,sNy
          DO i=1,sNx
           hConv3d(i,j,k) = ( hConv3d(i,j,k)
     &                      + addMass(i,j,k,bi,bj)*mass2rUnit
     &                      )*recip_rA(i,j,bi,bj)
          ENDDO
         ENDDO
#endif /* ALLOW_ADDFLUID */
        ELSE
         DO j=1,sNy
          DO i=1,sNx
           hConv3d(i,j,k) = hConv3d(i,j,k)*recip_rA(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF

C-    End DO k=1,Nr
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Integrate continuity vertically for vertical velocity

      CALL INTEGRATE_FOR_W(
     I                      bi, bj,
     I                      hConv3d, rStarDhDt,
     O                      wFld,
     I                      myIter, myThid )

#ifdef ALLOW_OBCS
      IF ( useOBCS ) THEN
C--   reset W to zero outside the OB interior region
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
            wFld(i,j,k,bi,bj) = wFld(i,j,k,bi,bj)*maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_OBCS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   End bi,bj loops
       ENDDO
      ENDDO

      _EXCH_XYZ_RL( wFld, myThid )

#endif /* ALLOW_SHELFICE_REMESHING */

      RETURN
      END
