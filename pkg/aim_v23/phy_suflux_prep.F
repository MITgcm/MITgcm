C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_suflux_prep.F,v 1.3 2004/06/25 18:24:17 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: SUFLUX_PREP
C     !INTERFACE:
      SUBROUTINE SUFLUX_PREP(
     I                   PSA,TA,QA,RH,ThA,Vsurf2,WVS,CLAT,FOROG,
     I                   FMASK,TLAND,TSEA,TSICE,SSR,
     O                   SPEED0,DRAG,DENVV,dTskin,T1,T0,Q0,
     I                   kGrd,bi,bj,myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SUFLUX_PREP
C     | o prepare surface flux calculation
C     *==========================================================*
C     | o contain 1rst part of original S/R SUFLUX (Speedy code)
C     *==========================================================*
C--
C--   SUBROUTINE SUFLUX (PSA,UA,VA,TA,QA,RH,PHI,
C--  &                   PHI0,FMASK,TLAND,TSEA,SWAV,SSR,SLRD,
C--  &                   USTR,VSTR,SHF,EVAP,SLRU,
C--  &                   TSFC,TSKIN,U0,V0,T0,Q0)
C--
C--   Purpose: Compute surface fluxes of momentum, energy and moisture,
C--            and define surface skin temperature from energy balance
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"

C     Physical constants + functions of sigma and latitude
#include "com_physcon.h"

C     Surface flux constants
#include "com_sflcon.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C--   Input:   
C    PSA    :: norm. surface pressure [p/p0]   (2-dim)
C    TA     :: temperature                     (3-dim)
C    QA     :: specific humidity [g/kg]        (3-dim)
C    RH     :: relative humidity [0-1]         (3-dim)
C    ThA    :: Pot.temperature    [K]          (3-dim)
C    Vsurf2 :: square of surface wind speed (2-dim,input)
C               ==> UA,VA are no longer used
C    WVS    :: weights for near surf interp    (2-dim)
C    CLAT   :: cos(lat)                        (2-dim)
C    FOROG  :: orographic factor (surf. drag)  (2-dim)
C    FMASK  :: fraction land - sea - sea-ice (2.5-dim)
C    TLAND  :: land-surface temperature        (2-dim)
C    TSEA   ::  sea-surface temperature        (2-dim)
C    TSICE  ::  sea-ice surface temperature    (2-dim)
C    SSR    :: sfc sw radiation (net flux)     (2-dim)
C--   Output:  
C    SPEED0 :: effective surface wind speed    (2-dim)
C    DRAG   :: surface Drag term (= Cd*Rho*|V|)(2-dim)
C                         ==> USTR,VSTR are no longer used
C    DENVV  :: surface flux (sens,lat.) coeff. (=Rho*|V|) [kg/m2/s]
C    dTskin :: temp. correction for daily-cycle heating [K]
C    T1     :: near-surface air temperature (from Pot.Temp)
C    T0     :: near-surface air temperature    (2-dim)
C    Q0     :: near-surface sp. humidity [g/kg](2-dim)
C--   Input:
C    kGrd   :: Ground level index              (2-dim)
C    bi,bj  :: tile index
C    myThid :: Thread number for this instance of the routine
C--
      _RL  PSA(NGP), TA(NGP,NLEV), QA(NGP,NLEV), RH(NGP,NLEV)
      _RL  ThA(NGP,NLEV)
      _RL  Vsurf2(NGP), WVS(NGP), CLAT(NGP), FOROG(NGP)
      _RL  FMASK(NGP,3), TLAND(NGP), TSEA(NGP), TSICE(NGP)
      _RL  SSR(NGP)

      _RL  SPEED0(NGP), DRAG(NGP,0:3), T1(NGP), DENVV(NGP)
      _RL  dTskin(NGP), T0(NGP), Q0(NGP)

      INTEGER kGrd(NGP)
      INTEGER bi,bj,myThid
CEOP

#ifdef ALLOW_AIM

C-- Local variables:
       _RL  QSAT0(NGP,2)

      INTEGER J, Ktmp, NL1
      _RL tmpRH(NGP)
      _RL factWind2, kappa

C- jmc: declare all local variables:
      _RL GTEMP0, GHUM0, RCP, PRD, VG2
c     _RL RDTH, FSLAND, FSSEA, FSSICE
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   1. Extrapolation of wind, temp, hum. and density to the surface

C     1.1 Wind components

c     DO J=1,NGP
c       U0(J) = 0.0
c       V0(J) = 0.0
c       Ktmp = kGrd(J)
c      IF ( Ktmp.GT.0 ) THEN
c       U0(J) = FWIND0*UA(J,Ktmp)
c       V0(J) = FWIND0*VA(J,Ktmp)
c      ENDIF
c     ENDDO

C     1.2 Temperature

      GTEMP0 = 1.-FTEMP0
      RCP = 1. _d 0 /CP
      kappa = RD/CP
C
      DO J=1,NGP
        Ktmp = kGrd(J)
        NL1 = Ktmp-1
       IF ( Ktmp.GT.1 ) THEN
c_FM    T0(J) = TA(J,NLEV)+WVI(NLEV,2)*(TA(J,NLEV)-TA(J,NL1))
c_FM    T1(J) = TA(J,NLEV)+RCP*(PHI(J,NLEV)-PHI0(J))
        T0(J) = TA(J,Ktmp) +    WVS(J)*(TA(J,Ktmp)-TA(J,NL1))
Cjmc: used previously but not valid with partial cell !
c       T1(J) = TA(J,Ktmp)*(SIGH(Ktmp)/SIG(Ktmp))**kappa
        T1(J) = ThA(J,Ktmp)*(PSA(J)**kappa)
        tmpRH(J)=RH(J,Ktmp)
       ELSE
        T0(J) = 273.16 _d 0
        T1(J) = 273.16 _d 0
        tmpRH(J)= 0.
       ENDIF
      ENDDO

      DO J=1,NGP
c       T0(J) = FTEMP0*T0(J)+GTEMP0*T1(J)
        T0(J) = FTEMP0*MIN(T0(J),T1(J))+GTEMP0*T1(J)
      ENDDO

C     1.3 Spec. humidity

      GHUM0 = 1.-FHUM0

      CALL SHTORH (-1,NGP,T0, PSA, 1. _d 0, Q0, tmpRH, QSAT0, myThid)

      DO J=1,NGP
       IF ( kGrd(J) .GT. 0 ) THEN
        Q0(J)=FHUM0*Q0(J)+GHUM0*QA(J,kGrd(J))
       ENDIF
      ENDDO

C     1.4 Density * wind speed (including gustiness factor)

      PRD = P0/RD
      VG2 = VGUST*VGUST
      factWind2 = FWIND0*FWIND0

      DO J=1,NGP
c_FM    DENVV(J)=(PRD*PSA(J)/T0(J))*
c_FM &           SQRT(U0(J)*U0(J)+V0(J)*V0(J)+VG2)
        SPEED0(J)=SQRT(factWind2*Vsurf2(J)+VG2)
        DENVV(J)=(PRD*PSA(J)/T0(J))*SPEED0(J)
      ENDDO

C     1.5 Define effective skin temperature to compensate for
C         non-linearity of heat/moisture fluxes during the daily cycle
C         Tskin = Tland + dTskin

      DO J=1,NGP
        dTskin(J)=CTDAY*CLAT(J)*SSR(J)*PSA(J)
      ENDDO


C--   2. Computation of fluxes over land and sea

C     2.1 Wind stress

C     Orographic correction

      DO J=1,NGP
c       CDENVV(J,1)=CDL*DENVV(J)*FOROG(J)
c       CDENVV(J,2)=CDS*DENVV(J)
        DRAG(J,1) = CDL*DENVV(J)*FOROG(J)
        DRAG(J,2) = CDS*DENVV(J)
        DRAG(J,3) = CDS*DENVV(J)
      ENDDO

C - Notes:
C   Because of a different mapping between the Drag and the Wind (A/C-grid)
C   the surface stress is computed later, in "External Forcing",
C   Here compute only surface drag term (= C_drag*Rho*|V| ) 

c     DO J=1,NGP
c       USTR(J,1) = -CDENVV(J,1)*UA(J,NLEV)
c       VSTR(J,1) = -CDENVV(J,1)*VA(J,NLEV)
c       USTR(J,2) = -CDENVV(J,2)*UA(J,NLEV)
c       VSTR(J,2) = -CDENVV(J,2)*VA(J,NLEV)
c     ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */

      RETURN
      END

      SUBROUTINE SFLSET (PHI0, FOROG, bi,bj,myThid)
C--
C--   SUBROUTINE SFLSET (PHI0)
C--
C--   Purpose: compute orographic factor for land surface drag
C--   Input:   PHI0   = surface geopotential            (2-dim)
C     Output:  FOROG  = orographic factor (surf. drag)  (2-dim)
C--            (originally in common blocks: SFLFIX)

      IMPLICIT NONE

C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"

C     Physical constants + functions of sigma and latitude
#include "com_physcon.h"

C     Surface flux constants
#include "com_sflcon.h"

C-- Routine arguments:
      INTEGER bi,bj,myThid
      _RL  PHI0(NGP)
      _RL  FOROG(NGP)

#ifdef ALLOW_AIM

C-- Local variables:
      INTEGER J
      _RL  RHDRAG

      RHDRAG = 1./(GG*HDRAG)

      DO J=1,NGP
        FOROG(J) = 1. _d 0
     &   + FHDRAG*(1. _d 0 - EXP(-MAX(PHI0(J),0. _d 0)*RHDRAG) )
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AIM */

      RETURN
      END
