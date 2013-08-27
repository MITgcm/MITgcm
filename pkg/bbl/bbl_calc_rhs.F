C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_calc_rhs.F,v 1.8 2013/08/27 21:34:21 dimitri Exp $
C $Name:  $

#include "BBL_OPTIONS.h"

CBOP
C     !ROUTINE: BBL_CALC_RHS

C     !INTERFACE:
      SUBROUTINE BBL_CALC_RHS(
     I        myTime, myIter, myThid )

C     !DESCRIPTION:
C     Calculate tendency of tracers due to bottom boundary layer.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "BBL.h"

C     !INPUT PARAMETERS:
C     myTime    :: Current time in simulation
C     myIter    :: Current time-step number
C     myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter, myThid

C     !OUTPUT PARAMETERS:

C     !LOCAL VARIABLES:
C     bi,bj     :: Tile indices
C     i,j       :: Loop indices
C     d,r       :: Donnor/Receiver indices
C     kBot      :: k index of bottommost wet grid box
C     kLowC1    :: k index of bottommost (i,j) cell
C     kLowC2    :: k index of bottommost (i+1,j) or (i,j+1) cell
C     kl        :: k index at which to compare 2 cells
C     thk_d     :: thickness of donnor bottommost wet grid cell
C     thk_r     :: thickness of receiver bottommost wet grid cell
C     bblEta_d  :: bbl_eta of donnor cell
C     bblEta_r  :: bbl_eta of receiver cell
C     resThk_r  :: thk_r - bblEta_r
C     Theta_r   :: Theta of receiver cell
C     bblTheta_d:: Theta of donnor bbl
C     bblTheta_r:: Theta of receiver bbl
C     resTheta_r:: Theta of resThk_r
C     Salt_r    :: Salt of receiver cell
C     bblSalt_d :: Salt of donnor bbl
C     bblSalt_r :: Salt of receiver bbl
C     resSalt_r :: Salt of resThk_r
C     deltaRho  :: density change
C     deltaDpt  :: depth change
C     dVol      :: horizontal volume transport
C     bbl_tend  :: temporary variable for tendency terms
C     sloc      :: salinity of bottommost wet grid box
C     tloc      :: temperature of bottommost wet grid box
C     rholoc    :: in situ density of bottommost wet grid box
C     rhoBBL    :: in situ density of bottom boundary layer
C     bbl_rho1  :: local (i,j) density
C     bbl_rho2  :: local (i+1, j) or (i,j+1) density
      INTEGER bi, bj
      INTEGER i, j, d, r, kBot, kLowC1, kLowC2, kl
      _RL     thk_d, thk_r, bblEta_d, bblEta_r, resThk_r
      _RL     Theta_r, bblTheta_d, bblTheta_r, resTheta_r
      _RL     Salt_r,  bblSalt_d,  bblSalt_r,  resSalt_r
      _RL     deltaRho, deltaDpt, dVol, bbl_tend
      _RL     sloc   ( 0:sNx+1, 0:sNy+1 )
      _RL     tloc   ( 0:sNx+1, 0:sNy+1 )
      _RL     rholoc ( 0:sNx+1, 0:sNy+1 )
      _RL     rhoBBL ( 0:sNx+1, 0:sNy+1 )
      _RL     bbl_rho1, bbl_rho2
CEOP

C--   Loops on tile indices bi,bj
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C     Initialize tendency terms, make local copy of
C     bottomost temperature, salinity, in-situ density
C     and in-situ BBL density.
        DO j=0,sNy+1
         DO i=0,sNx+1
          bbl_TendTheta(i,j,bi,bj) = 0. _d 0
          bbl_TendSalt (i,j,bi,bj) = 0. _d 0
          kBot        = max(1,kLowC(i,j,bi,bj))
          tLoc(i,j)   = theta(i,j,kBot,bi,bj)
          sLoc(i,j)   = salt (i,j,kBot,bi,bj)
          rholoc(i,j) = rhoInSitu(i,j,kBot,bi,bj)
          IF ( kBot .EQ. Nr ) THEN
           rhoBBL(i,j) = bbl_rho_nr(i,j,bi,bj)
          ELSE
           rhoBBL(i,j) = rhoInSitu(i,j,kBot+1,bi,bj)
          ENDIF
         ENDDO
        ENDDO
 
C==== Compute and apply vertical exchange between BBL and
C     residual volume of botommost wet grid box.
C     This operation does not change total tracer quantity
C     in botommost wet grid box.

        DO j=0,sNy+1
         DO i=0,sNx+1
c        DO j=-oly,sNy+oly
c         DO i=-olx,sNx+olx
          kBot = kLowC(i,j,bi,bj)
          IF ( kBot .GT. 0 ) THEN
C     If model density is lower than BBL, slowly diffuse upward.
           IF ( rhoLoc(i,j) .LT. rhoBBL(i,j) )
     &            bbl_eta(i,j,bi,bj) = max ( 0. _d 0 ,
     &            bbl_eta(i,j,bi,bj) - bbl_wvel * dTtracerLev(kBot) )
C     If model density is higher than BBL then mix instantly.
           IF ( rhoLoc(i,j) .GE. rhoBBL(i,j) .OR.
     &          bbl_eta(i,j,bi,bj) .EQ. 0. _d 0 ) THEN
            bbl_theta(i,j,bi,bj) = tLoc(i,j)
            bbl_salt (i,j,bi,bj) = sLoc(i,j)
            bbl_eta  (i,j,bi,bj) = 0. _d 0
           ENDIF
          ENDIF
         ENDDO
        ENDDO

C==== Compute meridional bbl exchange at northern edge.
        j=sNy
        DO i=0,sNx+1
         kLowC1 = kLowC(i,j  ,bi,bj)
         kLowC2 = kLowC(i,j+1,bi,bj)
         IF ((kLowC1.GT.0).AND.(kLowC2.GT.0)) THEN

C     Compare the bbl densities at the higher pressure
C     (highest possible density of given t,s)
C     bbl in situ density is stored in k > kLowC indices
          kl = MAX(kLowC1,kLowC2) + 1
          deltaDpt = R_low(i,j,bi,bj)   + bbl_eta(i,j,bi,bj) -
     &               R_low(i,j+1,bi,bj) - bbl_eta(i,j+1,bi,bj)
          IF ( deltaDpt .GT. 0. ) THEN
           IF ( kl .GT. Nr ) THEN
            bbl_rho1 = bbl_rho_nr(i,j,bi,bj)
           ELSE
            bbl_rho1 = rhoInSitu(i,j,kl,bi,bj)
           ENDIF
           bbl_rho2 = rhoInSitu(i,j+1,kLowC2,bi,bj)
          ELSE
           bbl_rho1 = rhoInSitu(i,j,kLowC1,bi,bj)
           IF ( kl .GT. Nr ) THEN
            bbl_rho2 = bbl_rho_nr(i,j+1,bi,bj)
           ELSE
            bbl_rho2 = rhoInSitu(i,j+1,kl,bi,bj)
           ENDIF
          ENDIF
          deltaRho = bbl_rho2 - bbl_rho1
          IF ( (deltaRho*deltaDpt) .LT. 0. ) THEN
C     If heavy BBL water is higher than light BBL water,
C     exchange properties laterally.

C     Determine donnor and receiver cells.
           IF ( bbl_rho1 .GT. bbl_rho2 ) THEN
            d = j
            r = j + 1
           ELSE
            d = j + 1
            r = j
           ENDIF

C     Replenish thickness of donor cell, if needed.
           thk_d = drF(kLowC(i,d,bi,bj)) *
     &          hFacC(i,d,kLowC(i,d,bi,bj),bi,bj)
           IF ( ( bbl_theta(i,d,bi,bj) .EQ. tloc(i,d) ) .AND.
     &          ( bbl_salt (i,d,bi,bj) .EQ. sloc(i,d) ) .AND.
     &          ( bbl_eta  (i,d,bi,bj) .LT. bbl_initEta ) )
     &          bbl_eta(i,d,bi,bj) = min ( bbl_initEta, thk_d )

C     Compute some donnor and receiver cell properties.
           thk_r      = drF(kLowC(i,r,bi,bj)) *
     &                  hFacC(i,r,kLowC(i,r,bi,bj),bi,bj)
           Theta_r    = tLoc(i,r)
           Salt_r     = sLoc(i,r)
           bblTheta_d = bbl_theta(i,d,bi,bj)
           bblTheta_r = bbl_theta(i,r,bi,bj)
           bblSalt_d  = bbl_salt (i,d,bi,bj)
           bblSalt_r  = bbl_salt (i,r,bi,bj)
           bblEta_d   = bbl_eta  (i,d,bi,bj)
           bblEta_r   = bbl_eta  (i,r,bi,bj)
           resThk_r   = thk_r - bblEta_r
           resTheta_r = (Theta_r*thk_r-bblTheta_r*bblEta_r)/resThk_r
           resSalt_r  = (Salt_r *thk_r-bblSalt_r *bblEta_r)/resThk_r

C     Compute volume transport from donnor to receiver.
           dVol = min ( bblEta_d * rA(i,d,bi,bj) / 2. _d 0,
     &          resThk_r * rA(i,r,bi,bj) / 2. _d 0,
     &          dxG(i,j+1,bi,bj) * bblEta_d * bbl_hvel * deltaT )

C     Compute temperature tracer tendencies for donor and receiver cell.
           bbl_tend = dVol * (bblTheta_d - resTheta_r) / deltaT
           bbl_TendTheta(i,d,bi,bj) = bbl_TendTheta(i,d,bi,bj) -
     &                                bbl_tend / rA(i,d,bi,bj) / thk_d
           bbl_TendTheta(i,r,bi,bj) = bbl_TendTheta(i,r,bi,bj) +
     &                                bbl_tend / rA(i,r,bi,bj) / thk_r

C     Compute salinity tracer tendencies for donor and receiver cell.
           bbl_tend = dVol * (bblSalt_d - resSalt_r) / deltaT
           bbl_TendSalt(i,d,bi,bj) = bbl_TendSalt(i,d,bi,bj) -
     &                               bbl_tend / rA(i,d,bi,bj) / thk_d
           bbl_TendSalt(i,r,bi,bj) = bbl_TendSalt(i,r,bi,bj) +
     &                               bbl_tend / rA(i,r,bi,bj) / thk_r

C     Adjust pbl thickness and tracer properties.
           bbl_eta(i,d,bi,bj) = bblEta_d - dVol / rA(i,d,bi,bj)
           IF ( bbl_eta(i,d,bi,bj) .LT. 0.0001 ) THEN
            bbl_eta(i,d,bi,bj) = 0. _d 0
            bbl_theta(i,d,bi,bj) = tLoc(i,d)
            bbl_salt (i,d,bi,bj) = sLoc(i,d)
           ENDIF
           bbl_eta(i,r,bi,bj) = bblEta_r + dVol / rA(i,r,bi,bj)
           bbl_theta(i,r,bi,bj) = ( dVol * bblTheta_d +
     &          bblEta_r * rA(i,r,bi,bj) * bblTheta_r ) /
     &          ( bbl_eta(i,r,bi,bj) * rA(i,r,bi,bj) )
           bbl_salt(i,r,bi,bj)  = ( dVol * bblSalt_d  +
     &          bblEta_r * rA(i,r,bi,bj) * bblSalt_r  ) /
     &          ( bbl_eta(i,r,bi,bj) * rA(i,r,bi,bj) )
          ENDIF
         ENDIF
        ENDDO

C==== Compute meridional bbl exchange inside tile.
        DO j=0,sNy-1
         DO i=0,sNx+1
          kLowC1 = kLowC(i,j  ,bi,bj)
          kLowC2 = kLowC(i,j+1,bi,bj)
          IF ((kLowC1.GT.0).AND.(kLowC2.GT.0)) THEN

C     Compare the bbl densities at the higher pressure
C     (highest possible density of given t,s)
C     bbl in situ density is stored in k > kLowC indices
           kl = MAX(kLowC1,kLowC2) + 1
           deltaDpt = R_low(i,j,bi,bj)   + bbl_eta(i,j,bi,bj) -
     &                R_low(i,j+1,bi,bj) - bbl_eta(i,j+1,bi,bj)
           IF ( deltaDpt .GT. 0. ) THEN
            IF ( kl .GT. Nr ) THEN
             bbl_rho1 = bbl_rho_nr(i,j,bi,bj)
            ELSE
             bbl_rho1 = rhoInSitu(i,j,kl,bi,bj)
            ENDIF
            bbl_rho2 = rhoInSitu(i,j+1,kLowC2,bi,bj)
           ELSE
            bbl_rho1 = rhoInSitu(i,j,kLowC1,bi,bj)
            IF ( kl .GT. Nr ) THEN
             bbl_rho2 = bbl_rho_nr(i,j+1,bi,bj)
            ELSE
             bbl_rho2 = rhoInSitu(i,j+1,kl,bi,bj)
            ENDIF
           ENDIF
           deltaRho = bbl_rho2 - bbl_rho1
           IF ( (deltaRho*deltaDpt) .LT. 0. ) THEN
C     If heavy BBL water is higher than light BBL water,
C     exchange properties laterally.

C     Determine donnor and receiver cells.
            IF ( bbl_rho1 .GT. bbl_rho2 ) THEN
             d = j
             r = j + 1
            ELSE
             d = j + 1
             r = j
            ENDIF

C     Replenish thickness of donor cell, if needed.
            thk_d = drF(kLowC(i,d,bi,bj)) *
     &              hFacC(i,d,kLowC(i,d,bi,bj),bi,bj)
            IF ( ( bbl_theta(i,d,bi,bj) .EQ. tloc(i,d) ) .AND.
     &           ( bbl_salt (i,d,bi,bj) .EQ. sloc(i,d) ) .AND.
     &           ( bbl_eta  (i,d,bi,bj) .LT. bbl_initEta ) )
     &           bbl_eta(i,d,bi,bj) = min ( bbl_initEta, thk_d )

C     Compute some donnor and receiver cell properties.
            thk_r      = drF(kLowC(i,r,bi,bj)) *
     &                   hFacC(i,r,kLowC(i,r,bi,bj),bi,bj)
            Theta_r    = tLoc(i,r)
            Salt_r     = sLoc(i,r)
            bblTheta_d = bbl_theta(i,d,bi,bj)
            bblTheta_r = bbl_theta(i,r,bi,bj)
            bblSalt_d  = bbl_salt (i,d,bi,bj)
            bblSalt_r  = bbl_salt (i,r,bi,bj)
            bblEta_d   = bbl_eta  (i,d,bi,bj)
            bblEta_r   = bbl_eta  (i,r,bi,bj)
            resThk_r   = thk_r - bblEta_r
            resTheta_r = (Theta_r*thk_r-bblTheta_r*bblEta_r)/resThk_r
            resSalt_r  = (Salt_r *thk_r-bblSalt_r *bblEta_r)/resThk_r

C     Compute volume transport from donnor to receiver.
            dVol = min ( bblEta_d * rA(i,d,bi,bj) / 2. _d 0,
     &           resThk_r * rA(i,r,bi,bj) / 2. _d 0,
     &           dxG(i,j+1,bi,bj) * bblEta_d * bbl_hvel * deltaT )

C     Compute temperature tracer tendencies for donor and receiver cell.
            bbl_tend = dVol * (bblTheta_d - resTheta_r) / deltaT
            bbl_TendTheta(i,d,bi,bj) = bbl_TendTheta(i,d,bi,bj) -
     &                                 bbl_tend / rA(i,d,bi,bj) / thk_d
            bbl_TendTheta(i,r,bi,bj) = bbl_TendTheta(i,r,bi,bj) +
     &                                 bbl_tend / rA(i,r,bi,bj) / thk_r

C     Compute salinity tracer tendencies for donor and receiver cell.
            bbl_tend = dVol * (bblSalt_d - resSalt_r) / deltaT
            bbl_TendSalt(i,d,bi,bj) = bbl_TendSalt(i,d,bi,bj) -
     &                                bbl_tend / rA(i,d,bi,bj) / thk_d
            bbl_TendSalt(i,r,bi,bj) = bbl_TendSalt(i,r,bi,bj) +
     &                                bbl_tend / rA(i,r,bi,bj) / thk_r

C     Adjust pbl thickness and tracer properties.
            bbl_eta(i,d,bi,bj) = bblEta_d - dVol / rA(i,d,bi,bj)
            IF ( bbl_eta(i,d,bi,bj) .LT. 0.0001 ) THEN
             bbl_eta(i,d,bi,bj) = 0. _d 0
             bbl_theta(i,d,bi,bj) = tLoc(i,d)
             bbl_salt (i,d,bi,bj) = sLoc(i,d)
            ENDIF
            bbl_eta(i,r,bi,bj) = bblEta_r + dVol / rA(i,r,bi,bj)
            bbl_theta(i,r,bi,bj) = ( dVol * bblTheta_d +
     &           bblEta_r * rA(i,r,bi,bj) * bblTheta_r ) /
     &           ( bbl_eta(i,r,bi,bj) * rA(i,r,bi,bj) )
            bbl_salt(i,r,bi,bj)  = ( dVol * bblSalt_d  +
     &           bblEta_r * rA(i,r,bi,bj) * bblSalt_r  ) /
     &           ( bbl_eta(i,r,bi,bj) * rA(i,r,bi,bj) )
           ENDIF
          ENDIF
         ENDDO
        ENDDO

C==== Compute zonal bbl exchange at Eastern edge.
        i=sNx
        DO j=1,sNy
         kLowC1 = kLowC(i  ,j,bi,bj)
         kLowC2 = kLowC(i+1,j,bi,bj)
         IF ((kLowC1.GT.0).AND.(kLowC2.GT.0)) THEN

C     Compare the bbl densities at the higher pressure
C     (highest possible density of given t,s)
C     bbl in situ density is stored in k > kLowC indices
          kl = MAX(kLowC1,kLowC2) + 1
          deltaDpt = R_low(i,j,bi,bj)   + bbl_eta(i,j,bi,bj) -
     &               R_low(i+1,j,bi,bj) - bbl_eta(i+1,j,bi,bj)
          IF ( deltaDpt .GT. 0. ) THEN
           IF ( kl .GT. Nr ) THEN
            bbl_rho1 = bbl_rho_nr(i,j,bi,bj)
           ELSE
            bbl_rho1 = rhoInSitu(i,j,kl,bi,bj)
           ENDIF
           bbl_rho2 = rhoInSitu(i+1,j,kLowC2,bi,bj)
          ELSE
           bbl_rho1 = rhoInSitu(i,j,kLowC1,bi,bj)
           IF ( kl .GT. Nr ) THEN
            bbl_rho2 = bbl_rho_nr(i+1,j,bi,bj)
           ELSE
            bbl_rho2 = rhoInSitu(i+1,j,kl,bi,bj)
           ENDIF
          ENDIF
          deltaRho = bbl_rho2 - bbl_rho1
          IF ( (deltaRho*deltaDpt) .LT. 0. ) THEN
C     If heavy BBL water is higher than light BBL water,
C     exchange properties laterally.

C     Determine donnor and receiver cells.
           IF ( bbl_rho1 .GT. bbl_rho2 ) THEN
            d = i
            r = i + 1
           ELSE
            d = i + 1
            r = i
           ENDIF

C     Replenish thickness of donor cell, if needed.
           thk_d = drF(kLowC(d,j,bi,bj)) *
     &             hFacC(d,j,kLowC(d,j,bi,bj),bi,bj)
           IF ( ( bbl_theta(d,j,bi,bj) .EQ. tloc(d,j) ) .AND.
     &          ( bbl_salt (d,j,bi,bj) .EQ. sloc(d,j) ) .AND.
     &          ( bbl_eta  (d,j,bi,bj) .LT. bbl_initEta ) )
     &          bbl_eta(d,j,bi,bj) = min ( bbl_initEta, thk_d )

C     Compute some donnor and receiver cell properties.
           thk_r      = drF(kLowC(r,j,bi,bj)) *
     &                  hFacC(r,j,kLowC(r,j,bi,bj),bi,bj)
           Theta_r    = tLoc(r,j)
           Salt_r     = sLoc(r,j)
           bblTheta_d = bbl_theta(d,j,bi,bj)
           bblTheta_r = bbl_theta(r,j,bi,bj)
           bblSalt_d  = bbl_salt (d,j,bi,bj)
           bblSalt_r  = bbl_salt (r,j,bi,bj)
           bblEta_d   = bbl_eta  (d,j,bi,bj)
           bblEta_r   = bbl_eta  (r,j,bi,bj)
           resThk_r   = thk_r - bblEta_r
           resTheta_r = (Theta_r*thk_r-bblTheta_r*bblEta_r)/resThk_r
           resSalt_r  = (Salt_r *thk_r-bblSalt_r *bblEta_r)/resThk_r

C     Compute volume transport from donnor to receiver.
           dVol = min ( bblEta_d * rA(d,j,bi,bj) / 2. _d 0,
     &          resThk_r * rA(r,j,bi,bj) / 2. _d 0,
     &          dxG(i+1,j,bi,bj) * bblEta_d * bbl_hvel * deltaT )

C     Compute temperature tracer tendencies for donor and receiver cell.
           bbl_tend = dVol * (bblTheta_d - resTheta_r) / deltaT
           bbl_TendTheta(d,j,bi,bj) = bbl_TendTheta(d,j,bi,bj) -
     &                                bbl_tend / rA(d,j,bi,bj) / thk_d
           bbl_TendTheta(r,j,bi,bj) = bbl_TendTheta(r,j,bi,bj) +
     &                                bbl_tend / rA(r,j,bi,bj) / thk_r

C     Compute salinity tracer tendencies for donor and receiver cell.
           bbl_tend = dVol * (bblSalt_d - resSalt_r) / deltaT
           bbl_TendSalt(d,j,bi,bj) = bbl_TendSalt(d,j,bi,bj) -
     &                               bbl_tend / rA(d,j,bi,bj) / thk_d
           bbl_TendSalt(r,j,bi,bj) = bbl_TendSalt(r,j,bi,bj) +
     &                               bbl_tend / rA(r,j,bi,bj) / thk_r

C     Adjust pbl thickness and tracer properties.
           bbl_eta(d,j,bi,bj) = bblEta_d - dVol / rA(d,j,bi,bj)
           IF ( bbl_eta(d,j,bi,bj) .LT. 0.0001 ) THEN
            bbl_eta(d,j,bi,bj) = 0. _d 0
            bbl_theta(d,j,bi,bj) = tLoc(d,j)
            bbl_salt (d,j,bi,bj) = sLoc(d,j)
           ENDIF
           bbl_eta(r,j,bi,bj) = bblEta_r + dVol / rA(r,j,bi,bj)
           bbl_theta(r,j,bi,bj) = ( dVol * bblTheta_d +
     &          bblEta_r * rA(r,j,bi,bj) * bblTheta_r ) /
     &          ( bbl_eta(r,j,bi,bj) * rA(r,j,bi,bj) )
           bbl_salt(r,j,bi,bj)  = ( dVol * bblSalt_d  +
     &          bblEta_r * rA(r,j,bi,bj) * bblSalt_r  ) /
     &          ( bbl_eta(r,j,bi,bj) * rA(r,j,bi,bj) )
          ENDIF
         ENDIF
        ENDDO

C==== Compute zonal bbl exchange inside tile.
        DO j=1,sNy
         DO i=0,sNx-1
          kLowC1 = kLowC(i  ,j,bi,bj)
          kLowC2 = kLowC(i+1,j,bi,bj)
          IF ((kLowC1.GT.0).AND.(kLowC2.GT.0)) THEN

C     Compare the bbl densities at the higher pressure
C     (highest possible density of given t,s)
C     bbl in situ density is stored in k > kLowC indices
           kl = MAX(kLowC1,kLowC2) + 1
           deltaDpt = R_low(i,j,bi,bj)   + bbl_eta(i,j,bi,bj) -
     &                R_low(i+1,j,bi,bj) - bbl_eta(i+1,j,bi,bj)
           IF ( deltaDpt .GT. 0. ) THEN
            IF ( kl .GT. Nr ) THEN
             bbl_rho1 = bbl_rho_nr(i,j,bi,bj)
            ELSE
             bbl_rho1 = rhoInSitu(i,j,kl,bi,bj)
            ENDIF
            bbl_rho2 = rhoInSitu(i+1,j,kLowC2,bi,bj)
           ELSE
            bbl_rho1 = rhoInSitu(i,j,kLowC1,bi,bj)
            IF ( kl .GT. Nr ) THEN
             bbl_rho2 = bbl_rho_nr(i+1,j,bi,bj)
            ELSE
             bbl_rho2 = rhoInSitu(i+1,j,kl,bi,bj)
            ENDIF
           ENDIF
           deltaRho = bbl_rho2 - bbl_rho1
           IF ( (deltaRho*deltaDpt) .LT. 0. ) THEN
C     If heavy BBL water is higher than light BBL water,
C     exchange properties laterally.

C     Determine donnor and receiver cells.
            IF ( bbl_rho1 .GT. bbl_rho2 ) THEN
             d = i
             r = i + 1
            ELSE
             d = i + 1
             r = i
            ENDIF

C     Replenish thickness of donor cell, if needed.
            thk_d = drF(kLowC(d,j,bi,bj)) *
     &              hFacC(d,j,kLowC(d,j,bi,bj),bi,bj)
            IF ( ( bbl_theta(d,j,bi,bj) .EQ. tloc(d,j) ) .AND.
     &           ( bbl_salt (d,j,bi,bj) .EQ. sloc(d,j) ) .AND.
     &           ( bbl_eta  (d,j,bi,bj) .LT. bbl_initEta ) )
     &           bbl_eta(d,j,bi,bj) = min ( bbl_initEta, thk_d )

C     Compute some donnor and receiver cell properties.
            thk_r      = drF(kLowC(r,j,bi,bj)) *
     &                   hFacC(r,j,kLowC(r,j,bi,bj),bi,bj)
            Theta_r    = tLoc(r,j)
            Salt_r     = sLoc(r,j)
            bblTheta_d = bbl_theta(d,j,bi,bj)
            bblTheta_r = bbl_theta(r,j,bi,bj)
            bblSalt_d  = bbl_salt (d,j,bi,bj)
            bblSalt_r  = bbl_salt (r,j,bi,bj)
            bblEta_d   = bbl_eta  (d,j,bi,bj)
            bblEta_r   = bbl_eta  (r,j,bi,bj)
            resThk_r   = thk_r - bblEta_r
            resTheta_r = (Theta_r*thk_r-bblTheta_r*bblEta_r)/resThk_r
            resSalt_r  = (Salt_r *thk_r-bblSalt_r *bblEta_r)/resThk_r

C     Compute volume transport from donnor to receiver.
            dVol = min ( bblEta_d * rA(d,j,bi,bj) / 2. _d 0,
     &           resThk_r * rA(r,j,bi,bj) / 2. _d 0,
     &           dxG(i+1,j,bi,bj) * bblEta_d * bbl_hvel * deltaT )

C     Compute temperature tracer tendencies for donor and receiver cell.
            bbl_tend = dVol * (bblTheta_d - resTheta_r) / deltaT
            bbl_TendTheta(d,j,bi,bj) = bbl_TendTheta(d,j,bi,bj) -
     &                                 bbl_tend / rA(d,j,bi,bj) / thk_d
            bbl_TendTheta(r,j,bi,bj) = bbl_TendTheta(r,j,bi,bj) +
     &                                 bbl_tend / rA(r,j,bi,bj) / thk_r

C     Compute salinity tracer tendencies for donor and receiver cell.
            bbl_tend = dVol * (bblSalt_d - resSalt_r) / deltaT
            bbl_TendSalt(d,j,bi,bj) = bbl_TendSalt(d,j,bi,bj) -
     &                                bbl_tend / rA(d,j,bi,bj) / thk_d
            bbl_TendSalt(r,j,bi,bj) = bbl_TendSalt(r,j,bi,bj) +
     &                                bbl_tend / rA(r,j,bi,bj) / thk_r

C     Adjust pbl thickness and tracer properties.
            bbl_eta(d,j,bi,bj) = bblEta_d - dVol / rA(d,j,bi,bj)
            IF ( bbl_eta(d,j,bi,bj) .LT. 0.0001 ) THEN
             bbl_eta(d,j,bi,bj) = 0. _d 0
             bbl_theta(d,j,bi,bj) = tLoc(d,j)
             bbl_salt (d,j,bi,bj) = sLoc(d,j)
            ENDIF
            bbl_eta(r,j,bi,bj) = bblEta_r + dVol / rA(r,j,bi,bj)
            bbl_theta(r,j,bi,bj) = ( dVol * bblTheta_d +
     &           bblEta_r * rA(r,j,bi,bj) * bblTheta_r ) /
     &           ( bbl_eta(r,j,bi,bj) * rA(r,j,bi,bj) )
            bbl_salt(r,j,bi,bj)  = ( dVol * bblSalt_d  +
     &           bblEta_r * rA(r,j,bi,bj) * bblSalt_r  ) /
     &           ( bbl_eta(r,j,bi,bj) * rA(r,j,bi,bj) )
           ENDIF
          ENDIF
         ENDDO
        ENDDO

C--   end bi,bj loops.
       ENDDO
      ENDDO

      CALL EXCH_XY_RL( bbl_eta      , myThid )
      CALL EXCH_XY_RL( bbl_theta    , myThid )
      CALL EXCH_XY_RL( bbl_salt     , myThid )
      CALL EXCH_XY_RL( bbl_TendTheta, myThid )
      CALL EXCH_XY_RL( bbl_TendSalt , myThid )

      RETURN
      END
