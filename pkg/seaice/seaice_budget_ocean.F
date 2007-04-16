C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_budget_ocean.F,v 1.3 2007/04/16 22:43:01 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_BUDGET_OCEAN(
     I     UG,
     U     TSURF,
     O     netHeatFlux, SWHeatFlux,
     I     bi, bj, myThid )
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
#include "SEAICE_FFIELDS.h"
#ifdef SEAICE_VARIABLE_FREEZING_POINT
#include "DYNVARS.h"
#endif /* SEAICE_VARIABLE_FREEZING_POINT */

C     === Routine arguments ===
C     INPUT:
C     UG      :: thermal wind of atmosphere
C     TSURF   :: surface temperature of ocean in Kelvin
C     bi,bj   :: loop indices
C     myThid  :: Thread no. that called this routine.
C     OUTPUT:
C     netHeatFlux :: net surface heat flux over open water or under ice
C     SWHeatFlux  :: short wave heat flux over open water or under ice
      _RL UG         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL TSURF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL netHeatFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SWHeatFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi, bj, myThid
CEndOfInterface

C     === Local variables ===
C     i,j - Loop counters
      INTEGER i, j
#ifndef SEAICE_EXTERNAL_FLUXES
      INTEGER ITER
      _RL  QS1, TB, D1, D1W, D3, TMELT
C     effective conductivity of combined ice and snow
      _RL  effConduct
C     specific humidity at ice surface
      _RL  qhIce
C     powers of temperature
      _RL  t1, t2, t3, t4

C     local copies of global variables
      _RL tsurfLoc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL atempLoc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL lwdownLoc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ALB        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     coefficients of Hibler (1980), appendix B
      _RL A1         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL A2         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL A3         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     auxiliary variable
      _RL B          (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C NOW DEFINE ASSORTED CONSTANTS
C SATURATION VAPOR PRESSURE CONSTANT
      QS1=0.622 _d +00/1013.0 _d +00
C FREEZING TEMPERATURE OF SEAWATER
      TB=271.2 _d +00
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
C     Is this necessary?
        atempLoc (I,J) = MAX(273.16 _d 0+MIN_ATEMP,ATEMP(I,J,bi,bj))
        lwdownLoc(I,J) = MAX(MIN_LWDOWN,LWDOWN(I,J,bi,bj))
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
#endif /* SEAICE_EXTERNAL_FLUXES */
       ENDDO
      ENDDO

      RETURN
      END
