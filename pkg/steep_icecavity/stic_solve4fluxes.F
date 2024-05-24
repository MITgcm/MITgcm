#include "STIC_OPTIONS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: STIC_SOLVE4FLUXES
C     !INTERFACE:
      SUBROUTINE STIC_SOLVE4FLUXES(
     I   tLoc, sLoc, pLoc,
     I   gammaT, gammaS,
     I   iceConductionDistance, thetaIceConduction,
     O   heatFlux, fwFlux,
     O   forcingT, forcingS,
     O   insitutLoc,
     I   bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SOLVE4FLUXES
C     | o Calculate
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif

C     !INPUT PARAMETERS:
C     tLoc         ::
C     sLoc         ::
C     pLoc         ::
C     gammaT ::
C     gammaS ::
C     bi,bj      :: tile indices
C     myTime     :: current time in simulation
C     myIter     :: iteration number in simulation
C     myThid     :: my Thread Id number

C     !OUTPUT PARAMETERS:
C     heatFlux       ::
C     fwFlux ::
C     forcingT       ::
C     forcingS       ::
C     insitutLoc   ::
C----------

      _RL tLoc, sLoc, pLoc
      _RL gammaT, gammaS
      _RL iceConductionDistance, thetaIceConduction
      _RL heatFlux, fwFlux, forcingT, forcingS, insitutLoc
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifndef ALLOW_OPENAD
      _RL SW_TEMP
      EXTERNAL SW_TEMP
#endif

C     !LOCAL VARIABLES:
C     === Local variables ===
      _RL thetaFreeze, saltFreeze
      _RL eps1, eps2, eps3, eps4, eps6, eps7, eps8
      _RL aqe, bqe, cqe, discrim, recip_aqe
      _RL a0, b0, c0
      _RL w_B
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     === Useful Units ===
C--   gammaT, m s^-1
C--   gammaS, m s^-1
C--   rUnit2mass (rhoConst), kg m^-3
C--   mass2rUnit (recip_rhoConst), m^3 kg^-1
C--   eps3, W K^-1 m^-2
C--   fwFlux, kg m^-2 s^-1
C--   heatFlux, kg m^-2 s^-1
C--   forcing T, K m/s
C--   forcing S, psu m/s
C--   SHELFICEkappa, m^2/s
C--   w_B,  m/s

C--   eps1, W K^-1 m^-2 : kg/m^3 * J/kg/K * m/s
C--   eps2, W m^-2 : kg/m^3 * J/kg * m/s
C--   eps3, W K^-1 m^-2 : kg m^-3 * J kg^-1 K^-1 * m^2 s^-1 * m^-1

C--   fwFlux : fresh water flux due to melting (kg m^-2 s^-1)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     linear dependence of freezing point on salinity
      a0 = -0.0575   _d  0
      c0 =  0.0901   _d  0
      b0 =  -7.61    _d -4

C--   convert potential T into in-situ T relative to surface
      insitutLoc = SW_TEMP(sLoc,tLoc,pLoc, zeroRL)

C--   DEFINE SOME CONSTANTS
      eps1 = rUnit2mass*HeatCapacity_Cp * gammaT
      eps2 = rUnit2mass*SHELFICElatentHeat * gammaS
      eps3 = rhoSHELFICE*SHELFICEheatCapacity_Cp * SHELFICEkappa
     &       /iceConductionDistance;
      eps4 = b0*pLoc + c0
      eps6 = eps4 - insitutLoc
      eps7 = eps4 - thetaIceConduction

      IF ( debugLevel.GE.debLevE ) THEN
         WRITE(msgBuf,'(A,9E16.8)')
     &   'r2mass, Cp, gmaT,SIlh,gmaS, rhoSI, SI_Cp, Kap, ICondDis ',
     &   rUnit2mass,HeatCapacity_Cp,gammaT, SHELFICElatentHeat,gammaS,
     &   rhoSHELFICE, SHELFICEheatCapacity_Cp, SHELFICEkappa,
     &   iceConductionDistance

         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT , 1)
      ENDIF

C--   Default thermodynamics specify a linear T gradient
C--   through the ice (Holland and Jenkins, 1999, Section 2).
      IF (SHELFICEadvDiffHeatFlux .EQV. .FALSE.) THEN
          aqe = a0*(eps1 + eps3)
          bqe = eps1*eps6 + eps3*eps7 - eps2 - SHELFICEsalinity*aqe
          cqe = eps2 * sLoc - SHELFICEsalinity*(eps1*eps6 + eps3*eps7)

C--   Alterantively, we can have a nonlinear  T gradient
C--   through the ice (Holland and Jenkins, 1999, Section 3).
C--   This demands a different set of constants
      ELSE
          eps8 = rUnit2mass * gammaS * SHELFICEheatCapacity_Cp
          aqe = a0 *(eps1 - eps8)
          bqe = eps1*eps6 + sLoc*eps8*a0 - eps8*eps7 - eps2 -
     &        SHELFICEsalinity*eps1*a0
          cqe = sLoc*(eps8*eps7 + eps2) - SHELFICEsalinity*eps1
      ENDIF

C     solve quadratic equation for salinity at shelfice-ocean interface
      recip_aqe = 0. _d 0
      IF ( aqe .NE. 0. _d 0 ) recip_aqe = 0.5 _d 0/aqe

C--   Sb = \frac{-bqe \pm \sqrt(bqe^2 - 4 aqc cqe)}{2 aqe}
      discrim = bqe*bqe - 4. _d 0*aqe*cqe

C--   Try the negative root (- SQRT(discrim))  of the quadratic eq.
      saltFreeze = (- bqe - SQRT(discrim))*recip_aqe

C---  If the negative root yields a negative salinity, then use the
C--   positive root (+ SQRT(discrim))
      IF ( saltFreeze .LT. 0. _d 0 ) THEN
          saltFreeze = (- bqe + SQRT(discrim))*recip_aqe
      ENDIF

C--   in situ seawater freezing point using linearization
      thetaFreeze = a0*saltFreeze + eps4

C--   Calculate the upward heat and fresh water fluxes;
C--   MITgcm sign conventions: downward (negative) fresh water flux
C--   implies melting and due to upward (positive) heat flux

C--   Default thermodynamics specify a linear T gradient
C--   through the ice (Holland and Jenkins, 1999, Section 2).
      IF (SHELFICEadvDiffHeatFlux .EQV. .FALSE.) THEN
C--   This formulation of fwflux, derived from the heat balance equation
C--   instead of the salt balance equation, can handle the case when the
C--   salinity of the ocean, boundary layer, and ice are identical.
          fwFlux = 1/SHELFICElatentHeat*(
     &        eps3*(thetaFreeze - thetaIceConduction) -
     &        eps1*(insitutLoc - thetaFreeze) )
C--   Alterantively, we can have a nonlinear  T gradient
C--   through the ice (Holland and Jenkins, 1999, Section 3).
C--   This is only for melting case (Eq. 31 of Holland and Jenkins, 1999)
      ELSE
          fwFlux =
     &        eps1 * ( thetaFreeze - insitutLoc ) /
     &        (SHELFICElatentHeat + SHELFICEheatCapacity_Cp*
     &        (thetaFreeze - thetaIceConduction))
      ENDIF

C--   If a nonlinear local ice T gradient near the ice-ocean interface
C--   is allowed and fwflux is positive (ice growth) then
C--   we must solve the quadratic equation using a different set of
C--   coeffs (Holland Jenkins, 1999).
C--   Since we first need to know fwFlux, the solving of the
C--   quadratic equation for this case cannot be combined
C--   with the other two cases (linear T, nonlinear T and melting).
      IF ((SHELFICEadvDiffHeatFlux .EQV. .TRUE.) .AND.
     &    (fwFlux .GT. zeroRL)) THEN
          aqe = a0 *(eps1)
          bqe = eps1*eps6 - eps2 - SHELFICEsalinity*eps1*a0
          cqe = sLoc*(eps2) - SHELFICEsalinity*eps1

          recip_aqe = 0. _d 0
          IF ( aqe .NE. 0. _d 0 ) recip_aqe = 0.5 _d 0/aqe

          discrim = bqe*bqe - 4. _d 0*aqe*cqe
          saltFreeze = (- bqe - SQRT(discrim))*recip_aqe
          IF ( saltFreeze .LT. 0. _d 0 ) THEN
              saltFreeze = (- bqe + SQRT(discrim))*recip_aqe
          ENDIF

          thetaFreeze = a0*saltFreeze + eps4

          fwFlux =
     &        eps1 * ( thetaFreeze - insitutLoc ) /
     &        SHELFICElatentHeat
      ENDIF

C     velocity of meltwater flux at ice-ocean interface (m/s)
C     * negative corresponds to downward flux of meltwater (melting)
      w_B = fwFlux * mass2rUnit

C--   Calculate the upward heat fluxes:
C--   melting requires upward (positive) heat flux from ocean to ice.

C--   The heatFlux variable corresponds with the change of energy in the
C--   ocean grid cell volume.  In the conservative case (J2001),
C--   advective heat fluxes change the energy of the volume whereas in
C--   the non-conservative case there are no advective heat fluxes
C--   melting or freezing have no associated advective heat fluxes.
      IF (SHELFICEconserve) THEN

C--   In the conservative case (J2001) there are two cases, fixed and
C--   non-fixed ocean volume.

          IF (useRealFreshWaterFlux ) THEN

C--   If the ocean volume can change (realFWFlux=true) then advection of
C--   meltwater does not displace water at T=insitutLoc in the cell and the
C--   heat flux correpsonding to the total energy flux of the volume
C--   consists of only two terms: turbulent fluxes (positive out)
C--   and advective meltwater fluxes (negative in).
             heatFlux = rUnit2mass*HeatCapacity_Cp * (
     &           gammaT * (insitutLoc - thetaFreeze)
     &           + w_B * (thetaFreeze - insitutLoc + tLoc) )
          ELSE

C--   If the volume is fixed (realFWFlux=false) then the advection of
C--   meltwater does displace ambient water at T=insitutLoc in the cell.
C--   Displacement reduction volume energy by w_B * insitutLoc (positive)
             heatFlux = rUnit2mass*HeatCapacity_Cp * (
     &           gammaT * (insitutLoc - thetaFreeze)
     &           + w_B * (thetaFreeze - insitutLoc)        )
          ENDIF

      ELSE

C--   In the non-conservative form, only fluxes are turbulent fluxes
          heatFlux = rUnit2mass*HeatCapacity_Cp *
     &           gammaT * (insitutLoc - thetaFreeze)
      ENDIF

C--   Calculate the T and S tendency terms.  T tendency term is
C--   not necessarily proportional to the heat flux term above because
C--   the heat flux term corresponds to total energy change in the grid
C--   cell and not the change of energy per unit volume.

      IF (SHELFICEconserve) THEN
C--   In the conservative case, meltwater advection contributes (J2001)
C--   to T and S tendencies
C--   * forcing T (K m/s)
          forcingT =
     &      (gammaT - w_B)*(thetaFreeze - insitutLoc)

C--   * forcing S (psu m/s)
          forcingS =
     &      (gammaS - w_B)*(saltFreeze  - sLoc)
      ELSE
C--   Otherwise, the only fluxes out of the ocean that change T and S
C--   are the turbulent fluxes.
          forcingT = gammaT * ( thetaFreeze - insitutLoc )
          forcingS = gammaS * ( saltFreeze  - sLoc )
      ENDIF

      IF ( debugLevel.GE.debLevE ) THEN
         WRITE(msgBuf,'(A,7E16.8)')
     &   'aqe, bqe, ceq ',
     &   aqe,bqe,cqe

         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

         WRITE(msgBuf,'(A,7E16.8)')
     &   'T,S,P,t,TFrz,SFrz,w_B ',
     &   tLoc,sLoc,pLoc, insitutLoc, thetaFreeze, saltFreeze, w_B

         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT , 1)
      ENDIF

      RETURN
      END
