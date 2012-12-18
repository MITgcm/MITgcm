C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_quasihydrostatic.F,v 1.5 2012/12/18 01:18:01 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_QUASIHYDROSTATIC

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_QUASIHYDROSTATIC(
     I                bi,bj,k,
     I                uFld, vFld,
     O                effectiveBuoy,
     I                myThid )

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

C--   == Routine arguments ==
C !INPUT VARIABLES: ====================================================
C  bi,bj         :: tile indices
C  k             :: vertical level
C  uFld          :: zonal flow
C  vFld          :: meridional flow
C  myThid        :: my Thread Id number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C !OUTPUT/MODIFIED VARIABLES: ==========================================
C  effectiveBuoy :: Density (z-coord) / specific volume (p-coord) anomaly
      _RL effectiveBuoy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j           :: loop indices
C  scalingFactor :: scaling factor (from acceleration to density)
      INTEGER iMin,iMax,jMin,jMax
      PARAMETER( iMin = 0 , iMax = sNx+1 )
      PARAMETER( jMin = 0 , jMax = sNy+1 )
      INTEGER i,j
      _RL scalingFactor
CEOP

      IF ( usingZCoords ) THEN
C--   Z-coordinate case: Input is density anomaly

        scalingFactor=rhoConst*gravitySign*recip_gravity
C-    to reproduce old (wrong) results:
c       scalingFactor=gravitySign*recip_gravity

      ELSEIF ( fluidIsWater ) THEN
C--   P-coordinate, oceanic case: Input is specific-volume anomaly

        scalingFactor = recip_rhoConst*recip_gravity
c       scalingFactor = rVel2wUnit(k) <-- @ interface = wrong location
C-    should use expression below (consistent with omega <-> w-velocity
C      conversion) but rhoRef(k) = rho(tRef,sRef,p) is computed
C     in S/R SET_REF_STATE but is not stored:
c       scalingFactor = ( oneRL / rhoRef(k) )*recip_gravity

      ELSE
C--   P-coord., Ideal-Gas case: Input is virtual potential temp. anomaly
C     (see White & Bromley, QJRMS 1995)
        scalingFactor = tRef(k)*recip_gravity

      ENDIF

      IF ( use3dCoriolis ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         effectiveBuoy(i,j)=effectiveBuoy(i,j)
     &    +scalingFactor*
     &     fCoriCos(i,j,bi,bj)*
     &       ( angleCosC(i,j,bi,bj)*0.5 _d 0 *
     &                (uFld(i,j,k,bi,bj)+uFld(i+1,j,k,bi,bj))
     &        -angleSinC(i,j,bi,bj)*0.5 _d 0 *
     &                (vFld(i,j,k,bi,bj)+vFld(i,j+1,k,bi,bj))
     &       )
        ENDDO
       ENDDO
      ENDIF

      IF ( useNHMTerms ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         effectiveBuoy(i,j)=effectiveBuoy(i,j)
     &    +scalingFactor*
     &     (   (uFld( i ,j,k,bi,bj)*uFld( i ,j,k,bi,bj)
     &         +uFld(i+1,j,k,bi,bj)*uFld(i+1,j,k,bi,bj))
     &       + (vFld(i, j ,k,bi,bj)*vFld(i, j ,k,bi,bj)
     &         +vFld(i,j+1,k,bi,bj)*vFld(i,j+1,k,bi,bj))
     &     )* 0.5 _d 0 * recip_rSphere*recip_deepFacC(k)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
