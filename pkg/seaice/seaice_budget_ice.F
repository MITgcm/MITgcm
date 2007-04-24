C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/seaice_budget_ice.F,v 1.5 2007/04/24 21:17:47 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_BUDGET_ICE(
     I     UG, hIce, hSnwLoc,
     U     TSURF,
     O     netHeatFlux, SWHeatFlux,
     I     bi, bj, myThid )
C     /================================================================\
C     | SUBROUTINE seaice_budget_ice                                   |
C     | o Calculate ice growth rate, surface fluxes and temperature of |
C     |   ice surface.                                                 |
C     |   see Hibler, MWR, 108, 1943-1973, 1980                        |
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
C     TSURF   :: surface temperature of ice in Kelvin, updated
C     hIce    :: (actual) ice thickness with upper and lower limit
C     hSnwLoc :: actual snow thickness
C     bi,bj   :: loop indices
C     myThid  :: Thread no. that called this routine.
C     OUTPUT:
C     netHeatFlux :: net heat flux under ice = growth rate
C     SWHeatFlux  :: short wave heat flux under ice
      _RL UG         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL TSURF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL hIce       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hSnwloc    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL netHeatFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SWHeatFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi, bj, myTHid
      INTEGER KOPEN
CEndOfInterface

C     === Local variables ===
C     i,j - Loop counters
      INTEGER i, j
      INTEGER ITER
      _RL  QS1, C1, C2, C3, C4, C5, TB, D1, D1I, D3
      _RL  TMELT, TMELTP, XKI, XKS, HCUT, ASNOW, XIO
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
C MAYKUTS CONSTANTS FOR SAT. VAP. PRESSURE TEMP. POLYNOMIAL
      C1=2.7798202 _d -06
      C2=-2.6913393 _d -03
      C3=0.97920849 _d +00
      C4=-158.63779 _d +00
      C5=9653.1925 _d +00
C FREEZING TEMPERATURE OF SEAWATER
      TB=271.2 _d +00
C SENSIBLE HEAT CONSTANT
      D1=SEAICE_sensHeat
C ICE LATENT HEAT CONSTANT
      D1I=SEAICE_latentIce
C STEFAN BOLTZMAN CONSTANT TIMES 0.97 EMISSIVITY
      D3=SEAICE_emissivity
C MELTING TEMPERATURE OF ICE
      TMELT=273.16 _d +00
      TMELTP=273.159 _d +00
C ICE CONDUCTIVITY
      XKI=SEAICE_iceConduct
C SNOW CONDUCTIVITY
      XKS=SEAICE_snowConduct
C CUTOFF SNOW THICKNESS
      HCUT=SEAICE_snowThick
C PENETRATION SHORTWAVE RADIATION FACTOR
      XIO=SEAICE_shortwave

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

C NOW DECIDE ON ALBEDO                       
      DO J=1,sNy
       DO I=1,sNx
        ALB(I,J)=SEAICE_dryIceAlb
        IF(tsurfLoc(I,J).GT.TMELTP) ALB(I,J)=SEAICE_wetIceAlb
        ASNOW=SEAICE_drySnowAlb
        IF(tsurfLoc(I,J).GT.TMELTP) ASNOW=SEAICE_wetSnowAlb
C     For albedo computation, actual rather than effective snow thickness
c     must be used. 
        IF(hSnwLoc(I,J).GT.HCUT) THEN
         ALB(I,J)=ASNOW
        ELSE
         ALB(I,J)=ALB(I,J) + (hSnwLoc(I,J)/HCUT)*(ASNOW-ALB(I,J))
         IF(ALB(I,J).GT.ASNOW) ALB(I,J)=ASNOW
        END IF
       ENDDO
      ENDDO
C NOW DETERMINE FIXED FORCING TERM IN HEAT BUDGET
      DO J=1,sNy
       DO I=1,sNx
        IF(hSnwLoc(I,J).GT.0.0) THEN
C NO SW PENETRATION WITH SNOW
         A1(I,J)=(ONE-ALB(I,J))*SWDOWN(I,J,bi,bj)
     &        +lwdownLoc(I,J)*0.97 _d 0
     &        +D1*UG(I,J)*atempLoc(I,J)+D1I*UG(I,J)*AQH(I,J,bi,bj)
        ELSE
C SW PENETRATION UNDER ICE
         A1(I,J)=(ONE-ALB(I,J))*SWDOWN(I,J,bi,bj)
     &        *(ONE-XIO*EXP(-1.5 _d 0*hice(I,J)))
     &        +lwdownLoc(I,J)*0.97 _d 0
     &        +D1*UG(I,J)*atempLoc(I,J)+D1I*UG(I,J)*AQH(I,J,bi,bj)
        ENDIF
       ENDDO
      ENDDO
C NOW COMPUTE OTHER TERMS IN HEAT BUDGET                        
C COME HERE AT START OF ITERATION

crg check wether a2 is needed in the list of variables
cdm Ralf, the line below causes following error message
cdm INTERNAL ERROR: cannot find var clone to ada2
cdm c$taf loop = iteration TSURF,A2
cdm iterative solver for ice growth rate
cdm inputs:  TSURF    ice temperature
cdm          UG       forcing
cdm          hSnwLoc  snow thickness
cdm          hice  ice thickness
cdm outputs: A2 is needed for netHeatFlux, which is ice growth rate
cdm          tsurfLoc
      DO ITER=1,IMAX_TICE
        
       DO J=1,sNy
        DO I=1,sNx
         t1 = tsurfLoc(I,J)
         t2 = t1*t1
         t3 = t2*t1
         t4 = t2*t2
         qhIce=QS1*(C1*t4+C2*t3 +C3*t2+C4*t1+C5)
         A2(I,J)=-D1*UG(I,J)*t1-D1I*UG(I,J)*qhIce-D3*t4
         effConduct=
     &        XKS/(hSnwLoc(I,J)/hice(I,J)+XKS/XKI)/hice(I,J)
C     These two formulations give different results than
C     original formulation. Bummer!
C         effConduct= 1. _d 0 / ( hSnwLoc(I,J)/XKS + hice(I,J)/XKI )
C         effConduct= XKS*XKI / ( hSnwLoc(I,J)*XKI + hice(I,J)*XKS )
         A3(I,J)=4.0 _d +00*D3*t3 + effConduct + D1*UG(I,J)
#ifdef SEAICE_VARIABLE_FREEZING_POINT
         TB = -0.0575 _d 0*salt(I,J,1,bi,bj) + 0.0901 _d 0 
     &        + 273.15 _d 0
#endif /* SEAICE_VARIABLE_FREEZING_POINT */
         B(I,J)=effConduct*(TB-tsurfLoc(I,J))
cdm
cdm   if(tsurfLoc(I,J).le.206.)
cdm  &           print '(A,3i4,f12.2)','### ITER,I,J,TSURF',
cdm  &           ITER,I,J,tsurfLoc(I,J)
cdm
        ENDDO
       ENDDO
C NOW DECIDE IF IT IS TIME TO ESTIMATE GROWTH RATES
C NOW DETERMINE NEW ICE TEMPERATURE              
       DO J=1,sNy
        DO I=1,sNx
         tsurfLoc(I,J)=tsurfLoc(I,J)
     &        +(A1(I,J)+A2(I,J)+B(I,J))/A3(I,J) 
         tsurfLoc(I,J)=MAX(273.16 _d 0+MIN_TICE,tsurfLoc(I,J))
        ENDDO
       ENDDO
C NOW SET ICE TEMP TO MIN OF TMELT/ITERATION RESULT  
       DO J=1,sNy
        DO I=1,sNx
         tsurfLoc(I,J)=MIN(tsurfLoc(I,J),TMELT)
        ENDDO
       ENDDO

C END OF ITERATION
      ENDDO

      DO J=1,sNy
       DO I=1,sNx
        netHeatFlux(I,J)=-A1(I,J)-A2(I,J)    
        IF(hSnwLoc(I,J).GT.0.0) THEN
C NO SW PENETRATION WITH SNOW
         SWHeatFlux(I,J)=ZERO
        ELSE
C SW PENETRATION UNDER ICE
         SWHeatFlux(I,J)=-(ONE-ALB(I,J))*SWDOWN(I,J,bi,bj)
     &        *XIO*EXP(-1.5 _d 0*hice(I,J))
        ENDIF
       ENDDO
      ENDDO
C     pass back the surface temperature of the ice
      DO J=1,sNy
       DO I=1,sNx
        TSURF(I,J,bi,bj)=MIN(tsurfLoc(I,J),TMELT)
       ENDDO
      ENDDO

      RETURN
      END
