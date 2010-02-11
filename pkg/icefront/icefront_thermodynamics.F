C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_thermodynamics.F,v 1.5 2010/02/11 23:14:31 dimitri Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"

CBOP
C     !ROUTINE: ICEFRONT_THERMODYNAMICS
C     !INTERFACE:
      SUBROUTINE ICEFRONT_THERMODYNAMICS(
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  ICEFRONT_THERMODYNAMICS
C     | o shelf-ice main routine.
C     |   compute temperature and (virtual) salt flux at the
C     |   shelf-ice ocean interface
C     |
C     | stresses at the ice/water interface are computed in separate
C     | routines that are called from mom_fluxform/mom_vecinv
C     *=============================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "ICEFRONT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_ICEFRONT
C     !LOCAL VARIABLES :
C     === Local variables ===
C     I,J,K,bi,bj  :: loop counters
C     tLoc, sLoc, pLoc :: local in-situ temperature, salinity, pressure
C     pICE, thetaICE   :: averaged depth() and temperature of glacier interior
C     theta/saltFreeze :: temperature and salinity of water at the
C                         ice-ocean interface (at the freezing point)
C     freshWaterFlux   :: local variable for fresh water melt flux due to
C                         melting in kg/m^2/s (negative density x melt rate)
C     convertFW2SaltLoc:: local copy of convertFW2Salt
C     cFac             :: 1 for conservative form, 0, otherwise
C     auxiliary variables and abbreviations:
C     a0, a1, a2, b, c0
C     eps1, eps2, eps3, eps4, eps5, eps6, eps7
C     aqe, bqe, cqe, discrim, recip_aqe
      INTEGER I,J,K
      INTEGER bi,bj
      _RL tLoc, sLoc, pLoc
      _RL pICE, thetaICE
      _RL thetaFreeze, saltFreeze
      _RL freshWaterFlux, convertFW2SaltLoc
      _RL a0, b, c0
      _RL eps1, eps2, eps3, eps4, eps5, eps6, eps7
      _RL cFac, rFac
      _RL aqe, bqe, cqe, discrim, recip_aqe
      _RL tmpFac

      _RS icefrontHeatFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS icefrontFreshWaterFlux
     &                     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      _RL SW_TEMP
      EXTERNAL SW_TEMP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     are we doing the conservative form of Jenkins et al. (2001)?
      cFac = 0. _d 0
      IF ( ICEFRONTconserve ) cFac = 1. _d 0
C     with "real fresh water flux" (affecting ETAN), there is more to modify
      rFac = 1. _d 0
      IF ( ICEFRONTconserve .AND. useRealFreshWaterFlux ) rFac = 0. _d 0
C     linear dependence of freezing point on salinity
      a0 = -0.0575   _d  0
      c0 =  0.0901   _d  0
      b  =  -7.61    _d -4

C     a few abbreviations
      eps1 = rUnit2mass*HeatCapacity_Cp*ICEFRONTheatTransCoeff
      eps2 = rUnit2mass*ICEFRONTlatentHeat*ICEFRONTsaltTransCoeff
      eps3 = rUnit2mass*ICEFRONTheatCapacity_Cp*ICEFRONTsaltTransCoeff
      eps5 = mass2rUnit/HeatCapacity_Cp

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-Oly,sNy+Oly
         DO I = 1-Olx,sNx+Olx
          DO K = 1, Nr
           icefrontHeatFlux      (I,J,K,bi,bj) = 0. _d 0
           icefrontFreshWaterFlux(I,J,K,bi,bj) = 0. _d 0
           icefront_TendT        (I,J,K,bi,bj) = 0. _d 0
           icefront_TendS        (I,J,K,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
        DO J = 1, sNy
         DO I = 1, sNx
          IF (ICEFRONTlength(I,J,bi,bj) .GT. 0.D0 ) THEN
           DO K = 1, K_icefront(I,J,bi,bj)

C******  make local copies of temperature, salinity and depth (pressure)
            pLoc = ABS(rC(k))
            tLoc = theta(I,J,K,bi,bj)
            sLoc = MAX(salt(I,J,K,bi,bj), 0. _d 0)

C     turn potential temperature into in-situ temperature
            tLoc = SW_TEMP(sLoc,tLoc,pLoc,0.D0)

C      get ice temperature. Assuming linear ice temperature change from 
C      the surface (ICEFRONTthetaSurface) to the base(0 degree C) 
            IF ( K .EQ. K_icefront(I,J,bi,bj)) THEN
              pLoc = 0.5*(ABS(R_icefront(I,J,bi,bj))+ABS(rF(K)))
            ENDIF
            thetaICE = ICEFRONTthetaSurface*
     &           (R_icefront(I,J,bi,bj)-pLoc) 
     &           / R_icefront(I,J,bi,bj)

C     a few abbreviations
            eps4 = b*pLoc + c0
            eps6 = eps4 - tLoc
            eps7 = eps4 - thetaIce

C     solve quadratic equation to get salinity at icefront-ocean interface
            aqe = a0  *(-eps1+eps3)
            recip_aqe = 0. _d 0
            IF ( aqe .NE. 0. _d 0 ) recip_aqe = 0.5 _d 0/aqe
            bqe = - eps1*eps6 -sLoc*a0*eps3 + eps3*eps7 + eps2
            cqe = -(eps2+eps3*eps7)*sLoc
            discrim = bqe*bqe - 4. _d 0*aqe*cqe

            saltFreeze = (- bqe - SQRT(discrim))*recip_aqe
            IF ( saltFreeze .LT. 0. _d 0 )
     &           saltFreeze = (- bqe + SQRT(discrim))*recip_aqe
            thetaFreeze = a0*saltFreeze + eps4

C     outward fresh water flux due to freezing (in kg/m^2/s)
            freshWaterFlux = eps1 * ( thetaFreeze - tLoc ) /
     &           (ICEFRONTlatentHeat + ICEFRONTheatCapacity_cp*
     &           (thetaFreeze - thetaIce))

C--   Calculate the upward heat and fresh water fluxes;
C--   MITgcm sign conventions: downward (negative) fresh water flux
C--   implies melting and due to upward (positive) heat flux
            icefrontHeatFlux(I,J,K,bi,bj) =
     &           HeatCapacity_Cp *
     &           ( -rUnit2mass*ICEFRONTheatTransCoeff + freshWaterFlux)
     &           * ( thetaFreeze - tLoc )

            icefrontFreshWaterFlux(I,J,K,bi,bj) = freshWaterFlux

C--   compute tendencies
            icefront_TendT(i,j,K,bi,bj) =
     &           - icefrontHeatFlux(I,J,K,bi,bj)* eps5
            icefront_TendS(i,j,K,bi,bj) =
     &           freshWaterFlux * mass2rUnit* sLoc

C--   scale by icefrontlength, which is the ratio of the horizontal length
C     of the ice front in each model grid cell divided by the grid cell area
            IF (k .LT. k_icefront(i,j,bi,bj)) THEN  
             icefront_TendT(i,j,K,bi,bj) = icefront_TendT(i,j,K,bi,bj)
     &            * ICEFRONTlength(i,j,bi,bj)
             icefront_TendS(i,j,K,bi,bj) = icefront_TendS(i,j,K,bi,bj)
     &            * ICEFRONTlength(i,j,bi,bj)
            ELSEIF (k .EQ. k_icefront(i,j,bi,bj)) THEN
C--   at the bottom of the ice shelf there is additional scaling due
C     to partial depth of ice front
             icefront_TendT(i,j,K,bi,bj) = icefront_TendT(i,j,K,bi,bj)
     &            * ICEFRONTlength(i,j,bi,bj)
     &            * (ABS(R_icefront(I,J,bi,bj))-ABS(rF(K)))
     &            * recip_drF(K)
             icefront_TendS(i,j,K,bi,bj) = icefront_TendS(i,j,K,bi,bj)
     &            * ICEFRONTlength(i,j,bi,bj)
     &            * (ABS(R_icefront(I,J,bi,bj))-ABS(rF(K)))
     &            * recip_drF(K)
         ENDIF

           ENDDO
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL_RS(icefrontFreshWaterFlux,'ICFfwFlx',
     &      0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL_RS(icefrontHeatFlux,      'ICFhtFlx',
     &      0,Nr,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_ICEFRONT */
      RETURN
      END
