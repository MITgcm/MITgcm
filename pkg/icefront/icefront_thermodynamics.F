C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_thermodynamics.F,v 1.2 2010/01/22 00:51:54 dimitri Exp $
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
C     I,J,K,Kp1,bi,bj  :: loop counters
C     tLoc, sLoc, pLoc :: local in-situ temperature, salinity, pressure
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
C     drKp1, recip_drLoc
      INTEGER I,J,K,Kp1
      INTEGER bi,bj
      _RL tLoc(1:sNx,1:sNy)
      _RL sLoc(1:sNx,1:sNy)
      _RL pLoc(1:sNx,1:sNy)
      _RL thetaFreeze, saltFreeze
      _RL freshWaterFlux, convertFW2SaltLoc
      _RL a0, a1, a2, b, c0
      _RL eps1, eps2, eps3, eps4, eps5, eps6, eps7
      _RL cFac, rFac
      _RL aqe, bqe, cqe, discrim, recip_aqe
      _RL drKp1, recip_drLoc
      _RL tmpFac

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
      a1 =  0.0      _d -0
      a2 =  0.0      _d -0
      c0 =  0.0901   _d  0
      b  =  -7.61    _d -4
#ifdef ALLOW_ISOMIP_TD
      IF ( useISOMIPTD ) THEN
C     non-linear dependence of freezing point on salinity
       a0 = -0.0575   _d  0
       a1 = 1.710523  _d -3
       a2 = -2.154996 _d -4
       b  = -7.53     _d -4
       c0 = 0. _d 0
      ENDIF
      convertFW2SaltLoc = convertFW2Salt
C     hardcoding this value here is OK because it only applies to ISOMIP
C     where this value is part of the protocol
      IF ( convertFW2SaltLoc .EQ. -1. ) convertFW2SaltLoc = 33.4 _d 0
#endif /* ALLOW_ISOMIP_TD */

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-Oly,sNy+Oly
         DO I = 1-Olx,sNx+Olx
          icefrontHeatFlux      (I,J,bi,bj) = 0. _d 0
          icefrontFreshWaterFlux(I,J,bi,bj) = 0. _d 0
          icefrontForcingT      (I,J,bi,bj) = 0. _d 0
          icefrontForcingS      (I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
        DO J = 1, sNy
         DO I = 1, sNx
C--   make local copies of temperature, salinity and depth (pressure)
C--   underneath the ice
          K         = 1
          pLoc(I,J) = ABS(R_icefront(I,J,bi,bj))
          tLoc(I,J) = theta(I,J,K,bi,bj)
          sLoc(I,J) = MAX(salt(I,J,K,bi,bj), 0. _d 0)
         ENDDO
        ENDDO

C--   turn potential temperature into in-situ temperature relative
C--   to the surface
        DO J = 1, sNy
         DO I = 1, sNx
          tLoc(I,J) = SW_TEMP(sLoc(I,J),tLoc(I,J),pLoc(I,J),0.D0)
         ENDDO
        ENDDO

#ifdef ALLOW_ISOMIP_TD
        IF ( useISOMIPTD ) THEN
         DO J = 1, sNy
          DO I = 1, sNx
           K = 1
           IF ( K .NE. 0 .AND. pLoc(I,J) .GT. 0. _d 0 ) THEN
C--   Calculate freezing temperature as a function of salinity and pressure
            thetaFreeze =
     &           sLoc(I,J) * ( a0 + a1*sqrt(sLoc(I,J)) + a2*sLoc(I,J) )
     &           + b*pLoc(I,J) + c0
C--   Calculate the upward heat and  fresh water fluxes
            icefrontHeatFlux(I,J,bi,bj) = maskC(I,J,K,bi,bj) *
     &           ICEFRONTheatTransCoeff * ( tLoc(I,J) - thetaFreeze )
     &           *HeatCapacity_Cp*rUnit2mass
C     upward heat flux into the shelf-ice implies basal melting,
C     thus a downward (negative upward) fresh water flux (as a mass flux),
C     and vice versa
            icefrontFreshWaterFlux(I,J,bi,bj) =
     &           - icefrontHeatFlux(I,J,bi,bj)
     &           *recip_ICEFRONTlatentHeat
C--   compute surface tendencies
            icefrontForcingT(i,j,bi,bj) =
     &           - icefrontHeatFlux(I,J,bi,bj)
     &           *recip_Cp*mass2rUnit
     &           - cFac * icefrontFreshWaterFlux(I,J,bi,bj)*mass2rUnit
     &           * ( thetaFreeze - tLoc(I,J) )
            icefrontForcingS(i,j,bi,bj) =
     &           icefrontFreshWaterFlux(I,J,bi,bj) * mass2rUnit
     &           * ( cFac*sLoc(I,J) + (1. _d 0-cFac)*convertFW2SaltLoc )
C--   stress at the ice/water interface is computed in separate
C     routines that are called from mom_fluxform/mom_vecinv
           ELSE
            icefrontHeatFlux      (I,J,bi,bj) = 0. _d 0
            icefrontFreshWaterFlux(I,J,bi,bj) = 0. _d 0
            icefrontForcingT      (I,J,bi,bj) = 0. _d 0
            icefrontForcingS      (I,J,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ELSE
#else
        IF ( .TRUE. ) THEN
#endif /* ALLOW_ISOMIP_TD */
C     use BRIOS thermodynamics, following Hellmers PhD thesis:
C     Hellmer, H., 1989, A two-dimensional model for the thermohaline
C     circulation under an ice shelf, Reports on Polar Research, No. 60
C     (in German).

C     a few abbreviations
         eps1 = rUnit2mass*HeatCapacity_Cp*ICEFRONTheatTransCoeff
         eps2 = rUnit2mass*ICEFRONTlatentHeat*ICEFRONTsaltTransCoeff
         eps5 = rUnit2mass*HeatCapacity_Cp*ICEFRONTsaltTransCoeff

         DO J = 1, sNy
          DO I = 1, sNx
           K    = 1
           IF ( K .NE. 0 .AND. pLoc(I,J) .GT. 0. _d 0 ) THEN
C     solve quadratic equation to get salinity at icefront-ocean interface
C     note: this part of the code is not very intuitive as it involves
C     many arbitrary abbreviations that were introduced to derive the
C     correct form of the quadratic equation for salinity. The abbreviations
C     only make sense in connection with my notes on this (M.Losch)
            eps3 = rhoIcefront*ICEFRONTheatCapacity_Cp
     &           * ICEFRONTkappa/pLoc(I,J)
            eps4 = b*pLoc(I,J) + c0
            eps6 = eps4 - tLoc(I,J)
            eps7 = eps4 - ICEFRONTthetaSurface
            aqe = a0  *(eps1+eps3)
            recip_aqe = 0. _d 0
            IF ( aqe .NE. 0. _d 0 ) recip_aqe = 0.5 _d 0/aqe
            bqe = eps1*eps6 + eps3*eps7 - eps2
            cqe = eps2*sLoc(I,J)
            discrim = bqe*bqe - 4. _d 0*aqe*cqe
#undef ALLOW_ICEFRONT_DEBUG
#ifdef ALLOW_ICEFRONT_DEBUG
            IF ( discrim .LT. 0. _d 0 ) THEN
             print *, 'ml-icefront: discrim = ', discrim,aqe,bqe,cqe
             print *, 'ml-icefront: pLoc    = ', pLoc(I,J)
             print *, 'ml-icefront: tLoc    = ', tLoc(I,J)
             print *, 'ml-icefront: sLoc    = ', sLoc(I,J)
             print *, 'ml-icefront: tsurface= ',
     &            ICEFRONTthetaSurface
             print *, 'ml-icefront: eps1    = ', eps1
             print *, 'ml-icefront: eps2    = ', eps2
             print *, 'ml-icefront: eps3    = ', eps3
             print *, 'ml-icefront: eps4    = ', eps4
             print *, 'ml-icefront: eps5    = ', eps5
             print *, 'ml-icefront: eps6    = ', eps6
             print *, 'ml-icefront: eps7    = ', eps7
             print *, 'ml-icefront: rU2mass = ', rUnit2mass
             print *, 'ml-icefront: rhoIce  = ', rhoIcefront
             print *, 'ml-icefront: cFac    = ', cFac
             print *, 'ml-icefront: Cp_W    = ', HeatCapacity_Cp
             print *, 'ml-icefront: Cp_I    = ',
     &            ICEFRONTHeatCapacity_Cp
             print *, 'ml-icefront: gammaT  = ',
     &            ICEFRONTheatTransCoeff
             print *, 'ml-icefront: gammaS  = ',
     &            ICEFRONTsaltTransCoeff
             print *, 'ml-icefront: lat.heat= ',
     &            ICEFRONTlatentHeat
             STOP 'ABNORMAL END in S/R ICEFRONT_THERMODYNAMICS'
            ENDIF
#endif /* ALLOW_ICEFRONT_DEBUG */
            saltFreeze = (- bqe - SQRT(discrim))*recip_aqe
            IF ( saltFreeze .LT. 0. _d 0 )
     &           saltFreeze = (- bqe + SQRT(discrim))*recip_aqe
            thetaFreeze = a0*saltFreeze + eps4
C--   upward fresh water flux due to melting (in kg/m^2/s)
            freshWaterFlux = rUnit2mass*ICEFRONTsaltTransCoeff
     &           * ( saltFreeze - sLoc(I,J) ) / saltFreeze
C--   Calculate the upward heat and fresh water fluxes;
C--   MITgcm sign conventions: downward (negative) fresh water flux
C--   implies melting and due to upward (positive) heat flux
            icefrontHeatFlux(I,J,bi,bj) =
     &           ( eps3*( thetaFreeze - ICEFRONTthetaSurface )
     &           -  cFac*freshWaterFlux*( ICEFRONTlatentHeat
     &             - HeatCapacity_Cp*( thetaFreeze - rFac*tLoc(I,J) ) )
     &           )
            icefrontFreshWaterFlux(I,J,bi,bj) = freshWaterFlux
C--   compute surface tendencies
            icefrontForcingT(i,j,bi,bj) =
     &           ( ICEFRONTheatTransCoeff
     &           - cFac*icefrontFreshWaterFlux(I,J,bi,bj)*mass2rUnit )
     &           * ( thetaFreeze - tLoc(I,J) )
            icefrontForcingS(i,j,bi,bj) =
     &           ( ICEFRONTsaltTransCoeff
     &           - cFac*icefrontFreshWaterFlux(I,J,bi,bj)*mass2rUnit )
     &           * ( saltFreeze - sLoc(I,J) )
           ELSE
            icefrontHeatFlux      (I,J,bi,bj) = 0. _d 0
            icefrontFreshWaterFlux(I,J,bi,bj) = 0. _d 0
            icefrontForcingT      (I,J,bi,bj) = 0. _d 0
            icefrontForcingS      (I,J,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDIF
C     endif (not) useISOMIPTD
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL_RS(icefrontFreshWaterFlux,'SHIfwFlx',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL_RS(icefrontHeatFlux,      'SHIhtFlx',
     &      0,1,0,1,1,myThid)
C     SHIForcT (Ice shelf forcing for theta [W/m2], >0 increases theta)
       tmpFac = HeatCapacity_Cp*rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL(icefrontForcingT,tmpFac,1,
     &      'SHIForcT',0, 1,0,1,1,myThid)
C     SHIForcS (Ice shelf forcing for salt [g/m2/s], >0 increases salt)
       tmpFac = rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL(icefrontForcingS,tmpFac,1,
     &      'SHIForcS',0, 1,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_ICEFRONT */
      RETURN
      END
