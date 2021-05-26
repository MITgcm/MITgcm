#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_QUASIHYDROSTATIC

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_QUASIHYDROSTATIC(
     I                bi, bj, k,
     I                uFld, vFld,
     U                effectiveBuoy,
     I                myTime, myIter, myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | o SUBROUTINE MOM_QUASIHYDROSTATIC
C     |   Add Quasi-Hydrostatic Terms to buoyancy
C     *==========================================================*

C !USES: ===============================================================
      IMPLICIT NONE

C--   == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_QHYD_STAGGER_TS
# include "RESTART.h"
# include "NH_VARS.h"
#endif

C--   == Routine arguments ==
C !INPUT VARIABLES: ====================================================
C  bi, bj        :: tile indices
C  k             :: vertical level
C  uFld          :: zonal flow
C  vFld          :: meridional flow
C  myTime        :: current time in simulation
C  myIter        :: current iteration number
C  myThid        :: my Thread Id number
      INTEGER bi, bj, k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT/MODIFIED VARIABLES: ==========================================
C  effectiveBuoy :: Density (z-coord) / specific volume (p-coord) anomaly
      _RL effectiveBuoy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i, j          :: loop indices
C  gWinBuoy      :: vertical acceleraton term to add to buoyancy
C  gw_AB         :: tendency increment from Adams-Bashforth
C  scalingFactor :: scaling factor (from acceleration to density)
      INTEGER iMin,iMax,jMin,jMax
      PARAMETER( iMin = 0 , iMax = sNx+1 )
      PARAMETER( jMin = 0 , jMax = sNy+1 )
      INTEGER i, j
      _RL gWinBuoy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_QHYD_STAGGER_TS
      _RL gw_AB   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
      _RL scalingFactor
CEOP

      IF ( use3dCoriolis .OR. useNHMTerms ) THEN

        IF ( usingZCoords ) THEN
C--   Z-coordinate case: Input is density anomaly

          scalingFactor = rhoConst*gravitySign
     &                            *recip_gravity*recip_gravFacC(k)
C-    to reproduce old (wrong) results:
c         scalingFactor=gravitySign*recip_gravity

        ELSEIF ( fluidIsWater ) THEN
C--   P-coordinate, oceanic case: Input is specific-volume anomaly

          scalingFactor = recip_rhoConst*recip_gravity
c         scalingFactor = rVel2wUnit(k) <-- @ interface = wrong location
C-    should use expression below (consistent with omega <-> w-velocity
C      conversion) but rhoRef(k) = rho(tRef,sRef,p) is computed
C     in S/R SET_REF_STATE but is not stored:
c         scalingFactor = ( oneRL / rhoRef(k) )*recip_gravity

        ELSE
C--   P-coord., Ideal-Gas case: Input is virtual potential temp. anomaly
C     (see White & Bromley, QJRMS 1995)
          scalingFactor = tRef(k)*recip_gravity

        ENDIF

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gWinBuoy(i,j) = 0. _d 0
         ENDDO
        ENDDO

        IF ( use3dCoriolis ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gWinBuoy(i,j) = fCoriCos(i,j,bi,bj)*
     &       ( angleCosC(i,j,bi,bj)*halfRL
     &             *( uFld(i,j,k,bi,bj) + uFld(i+1,j,k,bi,bj) )
     &        -angleSinC(i,j,bi,bj)*halfRL
     &             *( vFld(i,j,k,bi,bj) + vFld(i,j+1,k,bi,bj) )
     &       )
          ENDDO
         ENDDO
        ENDIF

        IF ( useNHMTerms ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gWinBuoy(i,j) = gWinBuoy(i,j)
     &       + (  ( uFld( i ,j,k,bi,bj)*uFld( i ,j,k,bi,bj)
     &            + uFld(i+1,j,k,bi,bj)*uFld(i+1,j,k,bi,bj) )
     &          + ( vFld(i, j ,k,bi,bj)*vFld(i, j ,k,bi,bj)
     &            + vFld(i,j+1,k,bi,bj)*vFld(i,j+1,k,bi,bj) )
     &         )* halfRL*recip_rSphere*recip_deepFacC(k)
          ENDDO
         ENDDO
        ENDIF

#ifdef ALLOW_QHYD_STAGGER_TS
        IF ( staggerTimeStep ) THEN
# ifdef ALLOW_ADAMSBASHFORTH_3
          CALL ADAMS_BASHFORTH3( bi, bj, k, 1,
     U                           gWinBuoy,
     U                           QHydGwNm,
     O                           gw_AB,
     I                           qHydStartAB, myIter, myThid )
# else /* ALLOW_ADAMSBASHFORTH_3 */
          CALL ADAMS_BASHFORTH2( bi, bj, 1, 1,
     U                           gWinBuoy,
     U                           QHydGwNm(1-OLx,1-OLy,k,bi,bj),
     O                           gw_AB,
     I                           qHydStartAB, myIter, myThid )
# endif /* ALLOW_ADAMSBASHFORTH_3 */
        ENDIF
#endif /* ALLOW_QHYD_STAGGER_TS */

        DO j=jMin,jMax
         DO i=iMin,iMax
           effectiveBuoy(i,j) = effectiveBuoy(i,j)
     &                        + scalingFactor*gWinBuoy(i,j)
         ENDDO
        ENDDO

      ENDIF

      RETURN
      END
