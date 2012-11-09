C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_budget_ocean.F,v 1.19 2012/11/09 22:15:18 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_BUDGET_OCEAN(
     I     UG,
     I     TSURF,
     O     netHeatFlux, SWHeatFlux,
     I     bi, bj, myTime, myIter, myThid )
C     *================================================================*
C     | SUBROUTINE seaice_budget_ocean
C     | o Calculate surface heat fluxes over open ocean
C     |   see Hibler, MWR, 108, 1943-1973, 1980
C     |   If SEAICE_EXTERNAL_FLUXES is defined this routine simply
C     |   copies the global fields to the seaice-local fields.
C     *================================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
#ifndef SEAICE_EXTERNAL_FLUXES
# include "PARAMS.h"
# include "GRID.h"
# include "SEAICE_SIZE.h"
# include "SEAICE_PARAMS.h"
# ifdef ALLOW_EXF
#  include "EXF_OPTIONS.h"
#  include "EXF_FIELDS.h"
# endif
#endif

C     === Routine arguments ===
C     INPUT:
C     UG      :: thermal wind of atmosphere
C     TSURF   :: ocean surface temperature in Kelvin
C     bi,bj   :: loop indices
C     myTime  :: Simulation time
C     myIter  :: Simulation timestep number
C     myThid  :: Thread no. that called this routine.
C     OUTPUT:
C     netHeatFlux :: net surface heat flux over open water or under ice
C     SWHeatFlux  :: short wave heat flux over open water or under ice
      _RL UG         (1:sNx,1:sNy)
      _RL TSURF      (1:sNx,1:sNy)
      _RL netHeatFlux(1:sNx,1:sNy)
      _RL SWHeatFlux (1:sNx,1:sNy)
      _RL myTime
      INTEGER bi, bj, myIter, myThid
CEndOfInterface

C     === Local variables ===
C     i,j - Loop counters
      INTEGER i, j
#ifndef SEAICE_EXTERNAL_FLUXES
      _RL  QS1, D1, D1W, D3, TMELT

C     local copies of global variables
      _RL tsurfLoc   (1:sNx,1:sNy)
      _RL atempLoc   (1:sNx,1:sNy)
      _RL lwdownLoc  (1:sNx,1:sNy)

C     auxiliary variable
      _RL ssq, sstdegC
      _RL recip_rhoConstFresh, recip_lhEvap

C NOW DEFINE ASSORTED CONSTANTS
C SATURATION VAPOR PRESSURE CONSTANT
      QS1=0.622 _d 0/1013.0 _d 0
C SENSIBLE HEAT CONSTANT
      D1=SEAICE_dalton*SEAICE_cpAir*SEAICE_rhoAir
C WATER LATENT HEAT CONSTANT
      D1W=SEAICE_dalton*SEAICE_lhEvap*SEAICE_rhoAir
C STEFAN BOLTZMAN CONSTANT TIMES EMISSIVITY
      D3=SEAICE_emissivity*SEAICE_boltzmann
C MELTING TEMPERATURE OF ICE
      TMELT = celsius2K
C
      recip_lhEvap = 1./SEAICE_lhEvap
      recip_rhoConstFresh = 1./rhoConstFresh

      DO J=1,sNy
       DO I=1,sNx
        netHeatFlux(I,J) = 0. _d 0
        SWHeatFlux (I,J) = 0. _d 0
C
C     MAX_TICE does not exist anly longer, lets see if it works without
C       tsurfLoc (I,J) = MIN(celsius2K+MAX_TICE,TSURF(I,J))
        tsurfLoc (I,J) = TSURF(I,J)
# ifdef ALLOW_ATM_TEMP
C     Is this necessary?
        atempLoc (I,J) = MAX(celsius2K+MIN_ATEMP,ATEMP(I,J,bi,bj))
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
        lwdownLoc(I,J) = MAX(MIN_LWDOWN,LWDOWN(I,J,bi,bj))
# endif
       ENDDO
      ENDDO
#endif /* SEAICE_EXTERNAL_FLUXES */

C NOW DETERMINE OPEN WATER HEAT BUD. ASSUMING TSURF=WATER TEMP.
C WATER ALBEDO IS ASSUMED TO BE THE CONSTANT SEAICE_waterAlbedo
      DO J=1,sNy
       DO I=1,sNx
#ifdef SEAICE_EXTERNAL_FLUXES
        netHeatFlux(I,J) = Qnet(I,J,bi,bj)
        SWHeatFlux (I,J) =  Qsw(I,J,bi,bj)
#else /* SEAICE_EXTERNAL_FLUXES undefined */
C     This is an example of how one could implement surface fluxes
C     over the ocean (if one dislikes the fluxes computed in pkg/exf).
C     In this example, the exf-fields are re-used so that they no longer
C     have the same values as at the time when they are saved for
C     diagnostics (e.g., hl, hs, lwflux, sflux). To properly
C     diagnose them, one has to save them again as different (SI-)fields.
# ifdef ALLOW_DOWNWARD_RADIATION
C     net upward short wave heat flux
        SWHeatFlux(I,J) = (SEAICE_waterAlbedo - 1. _d 0)
     &       *swdown(I,J,bi,bj)
C     lwup = emissivity*stefanBoltzmann*Tsrf^4 + (1-emissivity)*lwdown
C     the second terms is the reflected incoming long wave radiation
C     so that the net upward long wave heat flux is:
        lwflux(I,J,bi,bj) = - lwdownLoc(I,J)*SEAICE_emissivity
     &       + D3*tsurfLoc(I,J)**4
        sstdegC = tsurfLoc(I,J) - TMELT
C     downward sensible heat
        hs(I,J,bi,bj) = D1*UG(I,J)*(atempLoc(I,J)-tsurfLoc(I,J))
C     saturation humidity
        ssq = QS1*6.11 _d 0 *EXP( 17.2694 _d 0
     &                           *sstdegC/(sstdegC+237.3 _d 0) )
C     downward latent heat
        hl(I,J,bi,bj) = D1W*UG(I,J)*(AQH(I,J,bi,bj)-ssq)
C     net heat is positive upward
        netHeatFlux(I,J)=SWHeatFlux(I,J)
     &       + lwflux(I,J,bi,bj)
     &       - hs(I,J,bi,bj) - hl(I,J,bi,bj)
C     compute evaporation here again because latent heat is different
C     from its previous value
        evap(i,j,bi,bj) = -hl(I,J,bi,bj)
     &       *recip_lhEvap*recip_rhoConstFresh
C     Salt flux from Precipitation and Evaporation.
        sflux(i,j,bi,bj) = evap(i,j,bi,bj) - precip(i,j,bi,bj)
#  ifdef ALLOW_RUNOFF
        sflux(i,j,bi,bj) = sflux(i,j,bi,bj) - runoff(i,j,bi,bj)
#  endif
        sflux(i,j,bi,bj) = sflux(i,j,bi,bj)*maskC(i,j,1,bi,bj)
        empmr(i,j,bi,bj) = sflux(i,j,bi,bj)*rhoConstFresh
# endif /* ALLOW_DOWNWARD_RADIATION */
#endif /* SEAICE_EXTERNAL_FLUXES */
       ENDDO
      ENDDO

      RETURN
      END
