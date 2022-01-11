#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_LINEAR_PHISURF
C     !INTERFACE:
      SUBROUTINE INI_LINEAR_PHISURF( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_LINEAR_PHISURF
C     | o Initialise the Linear Relation Phi_surf(eta)
C     *==========================================================*
C     | Initialise -Buoyancy at surface level (Bo_surf)
C     |  to setup the Linear relation: Phi_surf(eta)=Bo_surf*eta
C     | Initialise phi0surf = starting point for integrating
C     |                       phiHyd (= phiHyd at r=RoSurf)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid

C     == Local variables in common ==
C     topoHloc had to be in common for multi threading but no longer
C     needed since MDSIO now allows (2009/06/07) to write local arrays

C     !LOCAL VARIABLES:
C     topoHloc  :: Temporary array used to write surface topography
C     bi,bj  :: tile indices
C     i,j,k  :: Loop counters
#ifndef ALLOW_AUTODIFF
      _RS topoHloc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
      INTEGER bi, bj
      INTEGER i, j, k
      _RL     pLoc, rhoLoc
      _RL     dPIdp
CEOP

C--   Initialisation
#ifdef ALLOW_AUTODIFF
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           Bo_surf(i,j,bi,bj)  = 0. _d 0
           recip_Bo(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_AUTODIFF */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Initialise -Buoyancy at surface level : Bo_surf
C   Bo_surf is defined as d/dr(Phi_surf) and set to g/z2rUnit with
C     z2rUnit = conversion factor from z-unit to r-unit: [z] * z2rUnit = [r]
C   an accurate formulation includes P_surf and T,S_ref effects on rho_surf:
C    (setting uniformLin_PhiSurf=.FALSE.):
C    z-coord (z2rUnit=1): Bo_surf = - Buoyancy
C                                 = g * rho_surf(Tref,Sref,pSurf_0)/rho_0
C    p-coord (z2rUnit=rho*g): Bo_surf = 1/rho(Tref(ksurf),pSurf_0)
C   Note: on Phi_surf splitting : Non-linear Time-dependent effects on B_surf
C   [through eta & (T-tRef)_surf] are included in PhiHyd rather than in Bo_surf
C--
      IF ( usingZCoords ) THEN
C-  gBaro = gravity (except for External mode test with reduced gravity)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             Bo_surf(i,j,bi,bj) = gBaro
             recip_Bo(i,j,bi,bj) = 1. _d 0 / gBaro
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ELSEIF ( uniformLin_PhiSurf ) THEN
C-  use a linear (in ps) uniform relation : Phi'_surf = 1/rhoConst * ps'_surf
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
c            Bo_surf(i,j,bi,bj)  = rVel2wUnit(1)*gravity
c            recip_Bo(i,j,bi,bj) = wUnit2rVel(1)*recip_gravity
             Bo_surf(i,j,bi,bj)  = recip_rhoConst
             recip_Bo(i,j,bi,bj) = rhoConst
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ELSEIF ( fluidIsWater ) THEN
C--   More precise than uniformLin_PhiSurf case but inconsistent
C     with nonlinFreeSurf=4 in CALC_PHI_HYD (eta contribution to phiHyd)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF ( Ro_surf(i,j,bi,bj).GT.0. _d 0
     &          .AND. kSurfC(i,j,bi,bj).LE.Nr ) THEN
             pLoc = Ro_surf(i,j,bi,bj)
#ifdef ALLOW_OPENAD
             CALL FIND_RHO_SCALAR(
     I            tRef(kSurfC(i,j,bi,bj)),
     I            sRef(kSurfC(i,j,bi,bj)),
     I            pLoc,
     O            rhoLoc, myThid )
#else /* ALLOW_OPENAD */
             k = kSurfC(i,j,bi,bj)
             CALL FIND_RHO_SCALAR(
     I            tRef(k), sRef(k), pLoc,
     O            rhoLoc, myThid )
#endif /* ALLOW_OPENAD */
             IF ( rhoLoc .EQ. 0. _d 0 ) THEN
              Bo_surf(i,j,bi,bj) = 0. _d 0
             ELSE
              Bo_surf(i,j,bi,bj) = 1. _d 0/rhoLoc
             ENDIF
             recip_Bo(i,j,bi,bj) =  rhoLoc
            ELSE
              Bo_surf(i,j,bi,bj)  = 0. _d 0
              recip_Bo(i,j,bi,bj) = 0. _d 0
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ELSEIF ( fluidIsAir ) THEN
C-  use a linearized (in ps) Non-uniform relation : Bo_surf(Po_surf,tRef_surf)
C    Bo = d/d_p(Phi_surf) = tRef_surf*d/d_p(PI) ; PI = Cp*(p/Po)^kappa
C   and atm_Cp*atm_kappa = atm_Rd
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          IF ( select_rStar.GE.1 .OR. selectSigmaCoord.GE.1 ) THEN
C-      isothermal (theta=const) reference state
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF ( Ro_surf(i,j,bi,bj).GT.0. _d 0
     &          .AND. kSurfC(i,j,bi,bj).LE.Nr ) THEN
              dPIdp = (atm_Rd/atm_Po)*
     &         (Ro_surf(i,j,bi,bj)/atm_Po)**(atm_kappa-1. _d 0)
              Bo_surf(i,j,bi,bj) = dPIdp*thetaConst
              recip_Bo(i,j,bi,bj) = 1. _d 0 / Bo_surf(i,j,bi,bj)
             ELSE
              Bo_surf(i,j,bi,bj) = 0.
              recip_Bo(i,j,bi,bj) = 0.
             ENDIF
            ENDDO
           ENDDO
          ELSE
C-      horizontally uniform (tRef) reference state
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF ( Ro_surf(i,j,bi,bj).GT.0. _d 0
     &          .AND. kSurfC(i,j,bi,bj).LE.Nr ) THEN
              dPIdp = (atm_Rd/atm_Po)*
     &         (Ro_surf(i,j,bi,bj)/atm_Po)**(atm_kappa-1. _d 0)
              Bo_surf(i,j,bi,bj) = dPIdp*tRef(kSurfC(i,j,bi,bj))
              recip_Bo(i,j,bi,bj) = 1. _d 0 / Bo_surf(i,j,bi,bj)
             ELSE
              Bo_surf(i,j,bi,bj) = 0.
              recip_Bo(i,j,bi,bj) = 0.
             ENDIF
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
      ELSE
        STOP 'INI_LINEAR_PHISURF: We should never reach this point!'
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Update overlap regions (jmc: is it really needed ?)
c     _EXCH_XY_RL(Bo_surf, myThid)
c     _EXCH_XY_RL(recip_Bo, myThid)

      IF ( usingPCoords .AND. .NOT.uniformLin_PhiSurf ) THEN
        CALL WRITE_FLD_XY_RL( 'Bo_surf',' ',Bo_surf,0,myThid)
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Initialise phi0surf: used for atmos. surf. P-loading (ocean, z-coord)
C                               or topographic geopotential anom. (p-coord)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          phi0surf(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( geoPotAnomFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( geoPotAnomFile, ' ', phi0Surf, 0, myThid )
      ENDIF
      CALL EXCH_XY_RS( phi0Surf  , myThid )

      IF ( fluidIsAir .AND. topoFile.NE.' ' ) THEN

#ifdef ALLOW_AUTODIFF
         STOP 'CANNOT PRESENTLY USE THIS OPTION WITH ADJOINT'
#else

C--   Compute topoH = PhiRef(Po_surf)/g ; is different from original
C      topoZ(read from file) because of truncation of Po_surf.
C     NOTE: not clear for now which topoZ needs to be saved in common block
C--   AND set phi0surf = starting point for integrating Geopotential;

        CALL INI_P_GROUND( -2,
     O                     topoHloc,
     I                     Ro_surf, myThid )

       IF (selectFindRoSurf.NE.0) THEN
        _EXCH_XY_RS(phi0surf, myThid)
        CALL WRITE_FLD_XY_RS( 'phi0surf',' ',phi0surf,0,myThid)
       ENDIF

        CALL WRITE_FLD_XY_RS( 'topo_H',' ',topoHloc,0,myThid)

#endif /* ALLOW_AUTODIFF */

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      RETURN
      END
