C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_calc_rhs.F,v 1.4 2012/04/02 04:34:07 dimitri Exp $
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
C     kBot      :: k index of bottommost wet grid box
C     kLowC1    :: k index of bottommost (i,j) cell
C     kLowC2    :: k index of bottommost (i+1,j) or (i,j+1) cell
C     kl        :: k index at which to compare 2 cells
C     resThk    :: thickness of bottommost wet grid box minus bbl_eta
C     resTheta  :: temperature of this residual volume
C     resSalt   :: salinity of this residual volume
C     deltaRho  :: density change
C     deltaDpt  :: depth change
C     bbl_tend  :: temporary variable for tendency terms
C     sloc      :: salinity of bottommost wet grid box
C     tloc      :: temperature of bottommost wet grid box
C     rholoc    :: in situ density of bottommost wet grid box
C     rhoBBL    :: in situ density of bottom boundary layer
C     fZon      :: Zonal flux
C     fMer      :: Meridional flux
C     bbl_rho1  :: local (i,j) density
C     bbl_rho2  :: local (i+1, j) or (i,j+1) density 
      INTEGER bi, bj
      INTEGER i, j, kBot, kLowC1, kLowC2, kl
      _RL     resThk, resTheta, resSalt
      _RL     deltaRho, deltaDpt, bbl_tend
      _RL     bbl_rho1, bbl_rho2
      _RL     sloc    ( 1-Olx:sNx+Olx, 1-Oly:sNy+Oly )
      _RL     tloc    ( 1-Olx:sNx+Olx, 1-Oly:sNy+Oly )
      _RL     rholoc  ( 1-Olx:sNx+Olx, 1-Oly:sNy+Oly )
      _RL     rhoBBL  ( 1-Olx:sNx+Olx, 1-Oly:sNy+Oly )
      _RL     fZon    ( 1-Olx:sNx+Olx, 1-Oly:sNy+Oly )
      _RL     fMer    ( 1-Olx:sNx+Olx, 1-Oly:sNy+Oly )
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C--   Loops on tile indices bi,bj
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C     Initialize tendency terms, make local copy of
C     bottomost temperature, salinity, in-situ density
C     and in-situ BBL density, and ensure that bbl_eta
C     is not thicker than bottom level thickness.
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          bbl_TendTheta(i,j,bi,bj) = 0. _d 0
          bbl_TendSalt (i,j,bi,bj) = 0. _d 0
          kBot        = max(1,kLowC(i,j,bi,bj))
          tLoc(i,j)   = theta(i,j,kBot,bi,bj)
          sLoc(i,j)   = salt (i,j,kBot,bi,bj)
          rholoc(i,j) = rhoInSitu(i,j,kBot,bi,bj)
          IF ( kBot .EQ. Nr ) THEN
           rhoBBL(i,j) = bbl_rho_nr(i,j,bi,bj)
          ELSE
           rhoBBL(i,j) = rhoInSitu(i,j,kBot+1, bi,bj)
          ENDIF
          bbl_eta(i,j,bi,bj) = MIN (bbl_eta(i,j,bi,bj),
     &         drF(kBot)*hFacC(i,j,kBot,bi,bj))
         ENDDO
        ENDDO

C==== Compute and apply vertical exchange between BBL and
C     residual volume of botommost wet grid box.
C     This operation does not change total tracer quantity
C     in botommost wet grid box.

        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          kBot = kLowC(i,j,bi,bj)
          IF ( kBot .GT. 0 ) THEN
           resThk = hFacC(i,j,kBot,bi,bj)*drF(kBot) - bbl_eta(i,j,bi,bj)

C     If bbl occupies most of the bottom model grid box or
C     if model density is higher than BBL then mix instantly.
           IF ( (resThk.LE.1.) .OR. (rhoLoc(i,j).GE.rhoBBL(i,j)) ) THEN
            bbl_theta(i,j,bi,bj) = tLoc(i,j)
            bbl_salt (i,j,bi,bj) = sLoc(i,j)

C     If model density is lower than BBL, slowly diffuse upward.
           ELSE
            resTheta = ( tLoc(i,j) * (resThk+bbl_eta(i,j,bi,bj)) -
     &           (bbl_theta(i,j,bi,bj)*bbl_eta(i,j,bi,bj)) ) / resThk
            resSalt  = ( sLoc(i,j) * (resThk+bbl_eta(i,j,bi,bj)) -
     &           (bbl_salt (i,j,bi,bj)*bbl_eta(i,j,bi,bj)) ) / resThk
            bbl_theta(i,j,bi,bj) = bbl_theta(i,j,bi,bj) +
     &           dTtracerLev(kBot) * (resTheta-bbl_theta(i,j,bi,bj))
     &           / bbl_RelaxR
            bbl_salt (i,j,bi,bj) = bbl_salt (i,j,bi,bj) +
     &           dTtracerLev(kBot) * (resSalt -bbl_salt (i,j,bi,bj))
     &           / bbl_RelaxR
           ENDIF
          ENDIF
         ENDDO
        ENDDO

C==== Compute zonal bbl exchange.
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx-1
          kLowC1 = kLowC(i,j,bi,bj)
          kLowC2 = kLowC(i+1,j,bi,bj)
          IF ((kLowC1.GT.0).AND.(kLowC2.GT.0)) THEN
C     Compare the bbl densities at the higher pressure
C     (highest possible density of given t,s)
C     bbl in situ density is stored in kLowC + 1 index  
           kl = MAX(kLowC1, kLowC2) + 1
           IF ( kl .GT. Nr ) THEN
            bbl_rho1 = bbl_rho_nr(i,j,bi,bj)
            bbl_rho2 = bbl_rho_nr(i+1,j,bi,bj)
           else
            bbl_rho1 = rhoInSitu(i,j,kl,bi,bj)
            bbl_rho2 = rhoInSitu(i+1,j,kl,bi,bj)
           endif
           deltaRho = bbl_rho2 - bbl_rho1
           deltaDpt = R_low(i  ,j,bi,bj) + bbl_eta(i  ,j,bi,bj) -
     &          R_low(i+1,j,bi,bj) - bbl_eta(i+1,j,bi,bj)

C     If heavy BBL water is higher than light BBL water,
C     exchange properties laterally.
           IF ( (deltaRho*deltaDpt) .LE. 0. ) THEN
            bbl_TendTheta(i,j,bi,bj) = bbl_TendTheta(i,j,bi,bj) +
     &           ( bbl_theta(i+1,j,bi,bj) - bbl_theta(i,j,bi,bj) ) /
     &           bbl_RelaxH
            bbl_TendTheta(i+1,j,bi,bj) = bbl_TendTheta(i+1,j,bi,bj) -
     &           ( bbl_theta(i+1,j,bi,bj) - bbl_theta(i,j,bi,bj) ) *
     &           ( rA(i  ,j,bi,bj) * bbl_eta(i  ,j,bi,bj) ) /
     &           ( rA(i+1,j,bi,bj) * bbl_eta(i+1,j,bi,bj) * bbl_RelaxH )
            bbl_TendSalt(i,j,bi,bj) = bbl_TendSalt(i,j,bi,bj) +
     &           ( bbl_salt(i+1,j,bi,bj) - bbl_salt(i,j,bi,bj) ) /
     &           bbl_RelaxH
            bbl_TendSalt(i+1,j,bi,bj) = bbl_TendSalt(i+1,j,bi,bj) -
     &           ( bbl_salt(i+1,j,bi,bj) - bbl_salt(i,j,bi,bj) ) *
     &           ( rA(i  ,j,bi,bj) * bbl_eta(i  ,j,bi,bj) ) /
     &           ( rA(i+1,j,bi,bj) * bbl_eta(i+1,j,bi,bj) * bbl_RelaxH )
           ENDIF
          ENDIF
         ENDDO
        ENDDO

C==== Compute meridional bbl exchange.
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx
          kLowC1 = kLowC(i,j,bi,bj)
          kLowC2 = kLowC(i,j+1, bi,bj)
          IF ((kLowC1.GT.0).AND.(kLowC2.GT.0)) THEN
C     compare the bbl densities at the higher pressure
C     (highest possible density of given t,s)
C     bbl in situ density is stored in kLowC + 1 index  
           kl = MAX(kLowC1, kLowC2) + 1
           IF ( kl .GT. Nr ) THEN
            bbl_rho1 = bbl_rho_nr(i,j,bi,bj)
            bbl_rho2 = bbl_rho_nr(i,j+1,bi,bj)
           else
            bbl_rho1 = rhoInSitu(i,j,kl,bi,bj)
            bbl_rho2 = rhoInSitu(i,j+1,kl,bi,bj)
           endif
           deltaRho = bbl_rho2 - bbl_rho1
           deltaDpt = R_low(i,j  ,bi,bj) + bbl_eta(i,j  ,bi,bj) -
     &          R_low(i,j+1,bi,bj) - bbl_eta(i,j+1,bi,bj)

C     If heavy BBL water is higher than light BBL water,
C     exchange properties laterally.
           IF ( (deltaRho*deltaDpt) .LE. 0. ) THEN
            bbl_TendTheta(i,j,bi,bj) = bbl_TendTheta(i,j,bi,bj) +
     &           ( bbl_theta(i,j+1,bi,bj) - bbl_theta(i,j,bi,bj) ) /
     &           bbl_RelaxH
            bbl_TendTheta(i,j+1,bi,bj) = bbl_TendTheta(i,j+1,bi,bj) -
     &           ( bbl_theta(i,j+1,bi,bj) - bbl_theta(i,j,bi,bj) ) *
     &           ( rA(i  ,j,bi,bj) * bbl_eta(i  ,j,bi,bj) ) /
     &           ( rA(i,j+1,bi,bj) * bbl_eta(i,j+1,bi,bj) ) /
     &           bbl_RelaxH
            bbl_TendSalt(i,j,bi,bj) = bbl_TendSalt(i,j,bi,bj) +
     &           ( bbl_salt(i,j+1,bi,bj) - bbl_salt(i,j,bi,bj) ) /
     &           bbl_RelaxH
            bbl_TendSalt(i,j+1,bi,bj) = bbl_TendSalt(i,j+1,bi,bj) -
     &           ( bbl_salt(i,j+1,bi,bj)-bbl_salt(i,j,bi,bj)) *
     &           ( rA(i  ,j,bi,bj) * bbl_eta(i  ,j,bi,bj) ) /
     &           ( rA(i,j+1,bi,bj) * bbl_eta(i,j+1,bi,bj) * bbl_RelaxH )
           ENDIF
          ENDIF
         ENDDO
        ENDDO

C==== Apply lateral BBL exchange then scale tendency term
C     for botommost wet grid box.
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
          kBot = kLowC(i,j,bi,bj)
          IF ( kBot .GT. 0 ) THEN
           bbl_theta(i,j,bi,bj) = bbl_theta(i,j,bi,bj) +
     &          dTtracerLev(kBot) * bbl_TendTheta(i,j,bi,bj)
           bbl_salt (i,j,bi,bj) = bbl_salt (i,j,bi,bj) +
     &          dTtracerLev(kBot) * bbl_TendSalt (i,j,bi,bj)
           bbl_TendTheta(i,j,bi,bj) = bbl_TendTheta(i,j,bi,bj) *
     &          bbl_eta(i,j,bi,bj) / (hFacC(i,j,kBot,bi,bj)*drF(kBot))
           bbl_TendSalt (i,j,bi,bj) = bbl_TendSalt (i,j,bi,bj) *
     &          bbl_eta(i,j,bi,bj) / (hFacC(i,j,kBot,bi,bj)*drF(kBot))
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB ) THEN
C     Check salinity conservation
         bbl_tend=0
         DO j=1,sNy
          DO i=1,sNx
           kBot = kLowC(i,j,bi,bj)
           IF ( kBot .GT. 0 ) THEN
            bbl_tend = bbl_tend + bbl_TendSalt(i,j,bi,bj) *
     &           hFacC(i,j,kBot,bi,bj) * drF(kBot) *rA(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
         _GLOBAL_SUM_RL( bbl_tend, myThid )
         WRITE(msgBuf,'(A,E10.2)') 'total salt tendency = ', bbl_tend
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT, myThid )
        ENDIF
#endif /* ALLOW_DEBUG */

C--   end bi,bj loops.
       ENDDO
      ENDDO

      CALL EXCH_XY_RL( bbl_theta, myThid )
      CALL EXCH_XY_RL( bbl_salt , myThid )

      RETURN
      END
