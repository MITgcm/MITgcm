C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_budget_ocean.F,v 1.10 2010/01/20 00:44:08 dimitri Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_BUDGET_OCEAN(
     I     UG,
     U     TSURF,
     O     netHeatFlux, SWHeatFlux,
     I     bi, bj, myTime, myIter, myThid )
C     /================================================================\
C     | SUBROUTINE seaice_budget_ocean                                 |
C     | o Calculate surface heat fluxes over open ocean                |
C     |   see Hibler, MWR, 108, 1943-1973, 1980                        |
C     |   If SEAICE_EXTERNAL_FLUXES is defined this routine simply     |
C     |   simply copies the global fields to the seaice-local fields.  |
C     |================================================================|
C     \================================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
#include "SEAICE_PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
# include "EXF_FIELDS.h"
#endif

C     === Routine arguments ===
C     INPUT:
C     UG      :: thermal wind of atmosphere
C     TSURF   :: surface temperature of ocean in Kelvin
C     bi,bj   :: loop indices
C     myTime  :: Simulation time
C     myIter  :: Simulation timestep number
C     myThid  :: Thread no. that called this routine.
C     OUTPUT:
C     netHeatFlux :: net surface heat flux over open water or under ice
C     SWHeatFlux  :: short wave heat flux over open water or under ice
      _RL UG         (1:sNx,1:sNy)
      _RL TSURF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
      _RL ALB        (1:sNx,1:sNy)
C     coefficients of Hibler (1980), appendix B
      _RL A1         (1:sNx,1:sNy)
      _RL A2         (1:sNx,1:sNy)
C     auxiliary variable
      _RL B          (1:sNx,1:sNy)

C NOW DEFINE ASSORTED CONSTANTS
C SATURATION VAPOR PRESSURE CONSTANT
      QS1=0.622 _d +00/1013.0 _d +00
C SENSIBLE HEAT CONSTANT
      D1=SEAICE_sensHeat
C WATER LATENT HEAT CONSTANT
      D1W=SEAICE_latentWater
C STEFAN BOLTZMAN CONSTANT TIMES 0.97 EMISSIVITY
      D3=SEAICE_emissivity
C MELTING TEMPERATURE OF ICE
      TMELT=273.16 _d +00

      DO J=1,sNy
       DO I=1,sNx
        netHeatFlux(I,J) = 0. _d 0
        SWHeatFlux (I,J) = 0. _d 0
C     
        tsurfLoc (I,J) = MIN(273.16 _d 0+MAX_TICE,TSURF(I,J,bi,bj))
# ifdef ALLOW_ATM_TEMP
C     Is this necessary?
        atempLoc (I,J) = MAX(273.16 _d 0+MIN_ATEMP,ATEMP(I,J,bi,bj))
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
        ALB(I,J)=SEAICE_waterAlbedo                                
# ifdef ALLOW_DOWNWARD_RADIATION
        A1(I,J)=(ONE-ALB(I,J))*SWDOWN(I,J,bi,bj)
     &       +lwdownLoc(I,J)*0.97 _d 0
     &       +D1*UG(I,J)*atempLoc(I,J)+D1W*UG(I,J)*AQH(I,J,bi,bj)
        B(I,J)=QS1*6.11 _d +00*EXP(17.2694 _d +00
     &       *(tsurfLoc(I,J)-TMELT)
     &       /(tsurfLoc(I,J)-TMELT+237.3 _d +00))
        A2(I,J)=-D1*UG(I,J)*tsurfLoc(I,J)-D1W*UG(I,J)*B(I,J)
     &       -D3*(tsurfLoc(I,J)**4)
        netHeatFlux(I,J)=-A1(I,J)-A2(I,J)    
        SWHeatFlux (I,J)=-(ONE-ALB(I,J))*SWDOWN(I,J,bi,bj)
# endif /* ALLOW_DOWNWARD_RADIATION */
#endif /* SEAICE_EXTERNAL_FLUXES */
       ENDDO
      ENDDO

      RETURN
      END
