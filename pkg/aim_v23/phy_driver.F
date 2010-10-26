C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_driver.F,v 1.9 2010/10/26 20:59:53 dfer Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: PHY_DRIVER
C     !INTERFACE:
      SUBROUTINE PHY_DRIVER( tYear, usePkgDiag,
     I                       bi, bj, myTime, myIter, myThid )


C     !DESCRIPTION: \bv
C------------------------
C--   SUBROUTINE PHYDRIVER (tYear, myTime, bi, bj, myThid )
C--   Purpose: stand-alone driver for physical parametrization routines
C--   Input  :  TYEAR  : fraction of year (0 = 1jan.00, 1 = 31dec.24)
C--             grid-point model fields in common block: PHYGR1
C--             forcing fields in common blocks : LSMASK, FORFIX, FORCIN
C--   Output :  Diagnosed upper-air variables in common block: PHYGR2
C--             Diagnosed surface variables in common block: PHYGR3
C--             Physical param. tendencies in common block: PHYTEN
C--             Surface and upper boundary fluxes in common block: FLUXES
C-------
C     Note: tendencies are not /dpFac here but later in AIM_AIM2DYN
C-------
C from SPEDDY code: (part of original code left with c_FM)
C * S/R PHYPAR : except interp. dynamical Var. from Spectral of grid point
C                here dynamical var. are loaded within S/R AIM_DYN2AIM.
C * S/R FORDATE: only the CALL SOL_OZ (done once / day in SPEEDY)
C------------------------
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"
#include "EEPARAMS.h"

C-- Physics package
#include "AIM_PARAMS.h"
#include "AIM_GRID.h"

C     Constants + functions of sigma and latitude
#include "com_physcon.h"

C     Model variables, tendencies and fluxes on gaussian grid
#include "com_physvar.h"

C     Surface forcing fields (time-inv. or functions of seasonal cycle)
#include "com_forcing.h"

C     Constants for forcing fields:
#include "com_forcon.h"

C     Radiation scheme variables
#include "com_radvar.h"

C     Radiation constants
#include "com_radcon.h"

C     Logical flags
c_FM  include "com_lflags.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     tYear      :: Fraction into year
C     usePkgDiag :: logical flag, true if using Diagnostics PKG
C     bi, bj     :: Tile index
C     myTime     :: Current time of simulation ( s )
C     myIter     :: Current iteration number in simulation
C     myThid     :: Number of this instance of the routine
      _RL     tYear
      LOGICAL usePkgDiag
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_AIM
C     !FUNCTIONS:
C     !LOCAL VARIABLES:
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Local Variables originally (Speedy) in common bloc (com_physvar.h):
C      TG1     :: absolute temperature
C      QG1     :: specific humidity (g/kg)
C      VsurfSq :: Square of surface wind speed (grid position = as T,Q)
C      SE      :: dry static energy <- replaced by Pot.Temp.
C      QSAT    :: saturation specific humidity (g/kg)
C      PSG     :: surface pressure (normalized)
      _RL TG1    (NGP,NLEV)
      _RL QG1    (NGP,NLEV)
      _RL VsurfSq(NGP)
      _RL SE   (NGP,NLEV)
      _RL QSAT (NGP,NLEV)
      _RL PSG   (NGP)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Local variables:
C    kGrd   :: Ground level index              (2-dim)
C    dpFac  :: cell delta_P fraction           (3-dim)
C    dTskin :: temp. correction for daily-cycle heating [K]
C    T1s    :: near-surface air temperature (from Pot.Temp)
C    DENVV  :: surface flux (sens,lat.) coeff. (=Rho*|V|) [kg/m2/s]
C    Shf0   :: sensible heat flux over freezing surf.
C    dShf   :: sensible heat flux derivative relative to surf. temp
C    Evp0   :: evaporation computed over freezing surface (Ts=0.oC)
C    dEvp   :: evaporation derivative relative to surf. temp
C    Slr0   :: upward long wave radiation over freezing surf.
C    dSlr   :: upward long wave rad. derivative relative to surf. temp
C    sFlx   :: net surface flux (+=down) function of surf. temp Ts:
C              0: Flux(Ts=0.oC) ; 1: Flux(Ts^n) ; 2: d.Flux/d.Ts(Ts^n)
      LOGICAL LRADSW
      INTEGER ICLTOP(NGP)
      INTEGER kGround(NGP)
      _RL dpFac(NGP,NLEV)
c_FM  REAL    RPS(NGP), ST4S(NGP)
      _RL ST4S(NGP)
      _RL PSG_1(NGP), RPS_1
      _RL dTskin(NGP), T1s(NGP), DENVV(NGP)
      _RL Shf0(NGP), dShf(NGP), Evp0(NGP), dEvp(NGP)
      _RL Slr0(NGP), dSlr(NGP), sFlx(NGP,0:2)
      _RL UPSWG(NGP)

      INTEGER J, K

#ifdef ALLOW_CLR_SKY_DIAG
      _RL dummyR(NGP)
      INTEGER dummyI(NGP)
#endif
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   1. Compute grid-point fields

C-    1.1 Convert model spectral variables to grid-point variables

      CALL AIM_DYN2AIM(
     O                 TG1, QG1, SE, VsurfSq, PSG, dpFac, kGround,
     I                 bi, bj, myTime, myIter, myThid )

C-    1.2 Compute thermodynamic variables

C-    1.2.a Surface pressure (ps), 1/ps and surface temperature
      RPS_1 = 1. _d 0
      DO J=1,NGP
       PSG_1(J)=1. _d 0
c_FM   PSG(J)=EXP(PSLG1(J))
c_FM   RPS(J)=1./PSG(J)
      ENDDO

C     1.2.b Dry static energy
C      <= replaced by Pot.Temp in aim_dyn2aim
c     DO K=1,NLEV
c      DO J=1,NGP
c_FM    SE(J,K)=CP*TG1(J,K)+PHIG1(J,K)
c      ENDDO
c     ENDDO

C     1.2.c Relative humidity and saturation spec. humidity

      DO K=1,NLEV
c_FM   CALL SHTORH (1,NGP,TG1(1,K),PSG,SIG(K),QG1(1,K),
c_FM &              RH(1,K),QSAT(1,K))
       CALL SHTORH (1,NGP,TG1(1,K),PSG_1,SIG(K),QG1(1,K),
     O              RH(1,K,myThid),QSAT(1,K),
     I              myThid)
      ENDDO

C--   2. Precipitation

C     2.1 Deep convection

c_FM  CALL CONVMF (PSG,SE,QG1,QSAT,
c_FM &             ICLTOP,CBMF,PRECNV,TT_CNV,QT_CNV)
      CALL CONVMF (PSG,dpFac,SE,QG1,QSAT,
     O             ICLTOP,CBMF(1,myThid),PRECNV(1,myThid),
     O             TT_CNV(1,1,myThid),QT_CNV(1,1,myThid),
     I             kGround,bi,bj,myThid)

      DO K=2,NLEV
       DO J=1,NGP
        TT_CNV(J,K,myThid)=TT_CNV(J,K,myThid)*RPS_1*GRDSCP(K)
        QT_CNV(J,K,myThid)=QT_CNV(J,K,myThid)*RPS_1*GRDSIG(K)
       ENDDO
      ENDDO

C     2.2 Large-scale condensation

c_FM  CALL LSCOND (PSG,QG1,QSAT,
c_FM &             PRECLS,TT_LSC,QT_LSC)
      CALL LSCOND (PSG,dpFac,QG1,QSAT,
     O             PRECLS(1,myThid),TT_LSC(1,1,myThid),
     O             QT_LSC(1,1,myThid),
     I             kGround,bi,bj,myThid)

      IF ( aim_energPrecip ) THEN
C     2.3 Snow Precipitation (update TT_CNV & TT_LSC)
        CALL SNOW_PRECIP (
     I             PSG, dpFac, SE, ICLTOP,
     I             PRECNV(1,myThid), QT_CNV(1,1,myThid),
     I             PRECLS(1,myThid), QT_LSC(1,1,myThid),
     U             TT_CNV(1,1,myThid), TT_LSC(1,1,myThid),
     O             EnPrec(1,myThid),
     I             kGround,bi,bj,myThid)
      ELSE
        DO J=1,NGP
          EnPrec(J,myThid) = 0. _d 0
        ENDDO
      ENDIF

C--   3. Radiation (shortwave and longwave) and surface fluxes

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C --> from FORDATE (in SPEEDY) :

C     3.0 Compute Incomming shortwave rad. (from FORDATE in SPEEDY)

c_FM  CALL SOL_OZ (SOLC,TYEAR)
      CALL SOL_OZ (SOLC,tYear, snLat(1,myThid), csLat(1,myThid),
     O             FSOL, OZONE, OZUPP, ZENIT, STRATZ,
     I             bi,bj,myThid)

C <-- from FORDATE (in SPEEDY).
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     3.1 Compute shortwave tendencies and initialize lw transmissivity

C     The sw radiation may be called at selected time steps
      LRADSW = .TRUE.

      IF (LRADSW) THEN

c_FM    CALL RADSW (PSG,QG1,RH,ALB1,
c_FM &              ICLTOP,CLOUDC,TSR,SSR,TT_RSW)
       ICLTOP(1) = 1
       CALL RADSW (PSG,dpFac,QG1,RH(1,1,myThid),ALB1(1,0,myThid),
     I             FSOL, OZONE, OZUPP, ZENIT, STRATZ,
     O             TAU2, STRATC,
     O             ICLTOP,CLOUDC(1,myThid),
     O             TSR(1,myThid),SSR(1,0,myThid),
     O             UPSWG,TT_RSW(1,1,myThid),
     I             kGround,bi,bj,myThid)

        DO J=1,NGP
          CLTOP(J,myThid)=SIGH(ICLTOP(J)-1)*PSG_1(J)
        ENDDO

        DO K=1,NLEV
         DO J=1,NGP
          TT_RSW(J,K,myThid)=TT_RSW(J,K,myThid)*RPS_1*GRDSCP(K)
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( usePkgDiag ) THEN
        CALL DIAGNOSTICS_FILL( UPSWG,
     &                        'UPSWG   ', 1, 1 , 3,bi,bj, myThid )
      ENDIF
#endif

      ENDIF

C     3.2 Compute downward longwave fluxes

c_FM  CALL RADLW (-1,TG1,TS,ST4S,
c_FM &            OLR,SLR,TT_RLW)
      CALL RADLW (-1,TG1,TS(1,myThid),ST4S,
     &            OZUPP, STRATC, TAU2, FLUX, ST4A,
     O            OLR(1,myThid),SLR(1,0,myThid),TT_RLW(1,1,myThid),
     I            kGround,bi,bj,myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     3.3. Compute surface fluxes and land skin temperature

c_FM  CALL SUFLUX (PSG,UG1,VG1,TG1,QG1,RH,PHIG1,
c_FM &             PHIS0,FMASK1,STL1,SST1,SOILW1,SSR,SLR,
c_FM &             USTR,VSTR,SHF,EVAP,ST4S,
c_FM &             TS,TSKIN,U0,V0,T0,Q0)
      CALL SUFLUX_PREP(
     I             PSG, TG1, QG1, RH(1,1,myThid), SE, VsurfSq,
     I             WVSurf(1,myThid),csLat(1,myThid),fOrogr(1,myThid),
     I             FMASK1(1,1,myThid),STL1(1,myThid),SST1(1,myThid),
     I             sti1(1,myThid), SSR(1,0,myThid),
     O             SPEED0(1,myThid),DRAG(1,0,myThid),DENVV,
     O             dTskin,T1s,T0(1,myThid),Q0(1,myThid),
     I             kGround,bi,bj,myThid)

      CALL SUFLUX_LAND (
     I             PSG, FMASK1(1,1,myThid), EMISFC,
     I             STL1(1,myThid), dTskin,
     I             SOILW1(1,myThid), SSR(1,1,myThid), SLR(1,0,myThid),
     I             T1s, T0(1,myThid), Q0(1,myThid), DENVV,
     O             SHF(1,1,myThid), EVAP(1,1,myThid), SLR(1,1,myThid),
     O             Shf0, dShf, Evp0, dEvp, Slr0, dSlr, sFlx,
     O             TS(1,myThid), TSKIN(1,myThid),
     I             bi,bj,myThid)
#ifdef ALLOW_LAND
      CALL AIM_LAND_IMPL(
     I             FMASK1(1,1,myThid), dTskin,
     I             Shf0, dShf, Evp0, dEvp, Slr0, dSlr,
     U             sFlx, STL1(1,myThid),
     U             SHF(1,1,myThid), EVAP(1,1,myThid), SLR(1,1,myThid),
     O             dTsurf(1,1,myThid),
     I             bi, bj, myTime, myIter, myThid)
#endif /* ALLOW_LAND */

      CALL SUFLUX_OCEAN(
     I             PSG, FMASK1(1,2,myThid),
     I             SST1(1,myThid),
     I             SSR(1,2,myThid), SLR(1,0,myThid),
     O             T1s, T0(1,myThid), Q0(1,myThid), DENVV,
     O             SHF(1,2,myThid), EVAP(1,2,myThid), SLR(1,2,myThid),
     I             bi,bj,myThid)

      IF ( aim_splitSIOsFx ) THEN
        CALL SUFLUX_SICE (
     I             PSG, FMASK1(1,3,myThid), EMISFC,
     I             STI1(1,myThid), dTskin,
     I             SSR(1,3,myThid), SLR(1,0,myThid),
     I             T1s, T0(1,myThid), Q0(1,myThid), DENVV,
     O             SHF(1,3,myThid), EVAP(1,3,myThid), SLR(1,3,myThid),
     O             Shf0, dShf, Evp0, dEvp, Slr0, dSlr, sFlx,
     O             TS(1,myThid), TSKIN(1,myThid),
     I             bi,bj,myThid)
#ifdef ALLOW_THSICE
        CALL AIM_SICE_IMPL(
     I             FMASK1(1,3,myThid), SSR(1,3,myThid), sFlx,
     I             Shf0, dShf, Evp0, dEvp, Slr0, dSlr,
     U             STI1(1,myThid),
     U             SHF(1,3,myThid), EVAP(1,3,myThid), SLR(1,3,myThid),
     O             dTsurf(1,3,myThid),
     I             bi, bj, myTime, myIter, myThid)
#endif /* ALLOW_THSICE */
      ELSE
        DO J=1,NGP
          SHF (J,3,myThid) = 0. _d 0
          EVAP(J,3,myThid) = 0. _d 0
          SLR (J,3,myThid) = 0. _d 0
        ENDDO
      ENDIF

      CALL SUFLUX_POST(
     I             FMASK1(1,1,myThid), EMISFC,
     I             STL1(1,myThid), SST1(1,myThid), sti1(1,myThid),
     I             dTskin, SLR(1,0,myThid),
     I             T0(1,myThid), Q0(1,myThid), DENVV,
     U             DRAG(1,0,myThid), SHF(1,0,myThid),
     U             EVAP(1,0,myThid), SLR(1,1,myThid),
     O             ST4S, TS(1,myThid), TSKIN(1,myThid),
     I             bi,bj,myThid)

#ifdef ALLOW_DIAGNOSTICS
      IF ( usePkgDiag ) THEN
        CALL DIAGNOSTICS_FILL( SLR(1,0,myThid),
     &                        'DWNLWG  ', 1, 1 , 3,bi,bj, myThid )
      ENDIF
#endif
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     3.4 Compute upward longwave fluxes, convert them to tendencies
C         and add shortwave tendencies

c_FM  CALL RADLW (1,TG1,TS,ST4S,
c_FM &            OLR,SLR,TT_RLW)
      CALL RADLW (1,TG1,TS(1,myThid),ST4S,
     &            OZUPP, STRATC, TAU2, FLUX, ST4A,
     O            OLR(1,myThid),SLR(1,0,myThid),TT_RLW(1,1,myThid),
     I            kGround,bi,bj,myThid)

      DO K=1,NLEV
       DO J=1,NGP
        TT_RLW(J,K,myThid)=TT_RLW(J,K,myThid)*RPS_1*GRDSCP(K)
c_FM    TTEND (J,K)=TTEND(J,K)+TT_RSW(J,K)+TT_RLW(J,K)
       ENDDO
      ENDDO

#ifdef ALLOW_CLR_SKY_DIAG
C     3.5 Compute clear-sky radiation (for diagnostics only)
      IF ( aim_clrSkyDiag ) THEN

C      3.5.1 Compute shortwave tendencies
       dummyI(1) = -1
       CALL RADSW (PSG,dpFac,QG1,RH(1,1,myThid),ALB1(1,0,myThid),
     I             FSOL, OZONE, OZUPP, ZENIT, STRATZ,
     O             TAU2, STRATC,
     O             dummyI, dummyR,
     O  TSWclr(1,myThid), SSWclr(1,myThid), UPSWG, TT_SWclr(1,1,myThid),
     I             kGround,bi,bj,myThid)

#ifdef ALLOW_DIAGNOSTICS
      IF ( usePkgDiag ) THEN
        CALL DIAGNOSTICS_FILL( UPSWG,
     &                        'UPSWGclr', 1, 1 , 3,bi,bj, myThid )
      ENDIF
#endif

C      3.5.2 Compute downward longwave fluxes

       CALL RADLW (-1,TG1,TS(1,myThid),ST4S,
     &             OZUPP, STRATC, TAU2, FLUX, ST4A,
     O      OLWclr(1,myThid), SLWclr(1,myThid), TT_LWclr(1,1,myThid),
     I             kGround,bi,bj,myThid)

C      3.5.3 Compute upward longwave fluxes, convert them to tendencies

       CALL RADLW (1,TG1,TS(1,myThid),ST4S,
     &            OZUPP, STRATC, TAU2, FLUX, ST4A,
     O      OLWclr(1,myThid), SLWclr(1,myThid), TT_LWclr(1,1,myThid),
     I            kGround,bi,bj,myThid)

       DO K=1,NLEV
        DO J=1,NGP
          TT_SWclr(J,K,myThid)=TT_SWclr(J,K,myThid)*RPS_1*GRDSCP(K)
          TT_LWclr(J,K,myThid)=TT_LWclr(J,K,myThid)*RPS_1*GRDSCP(K)
        ENDDO
       ENDDO

      ENDIF
#endif /* ALLOW_CLR_SKY_DIAG */

C--   4. PBL interactions with lower troposphere

C     4.1 Vertical diffusion and shallow convection

c_FM  CALL VDIFSC (UG1,VG1,SE,RH,QG1,QSAT,PHIG1,
c_FM &             UT_PBL,VT_PBL,TT_PBL,QT_PBL)
      CALL VDIFSC (dpFac, SE, RH(1,1,myThid), QG1, QSAT,
     O             TT_PBL(1,1,myThid),QT_PBL(1,1,myThid),
     I             kGround,bi,bj,myThid)

C     4.2 Add tendencies due to surface fluxes

      DO J=1,NGP
c_FM   UT_PBL(J,NLEV)=UT_PBL(J,NLEV)+USTR(J,3)*RPS(J)*GRDSIG(NLEV)
c_FM   VT_PBL(J,NLEV)=VT_PBL(J,NLEV)+VSTR(J,3)*RPS(J)*GRDSIG(NLEV)
c_FM   TT_PBL(J,NLEV)=TT_PBL(J,NLEV)+ SHF(J,3)*RPS(J)*GRDSCP(NLEV)
c_FM   QT_PBL(J,NLEV)=QT_PBL(J,NLEV)+EVAP(J,3)*RPS(J)*GRDSIG(NLEV)
       K = kGround(J)
       IF ( K.GT.0 ) THEN
        TT_PBL(J,K,myThid) = TT_PBL(J,K,myThid)
     &                     + SHF(J,0,myThid) *RPS_1*GRDSCP(K)
        QT_PBL(J,K,myThid) = QT_PBL(J,K,myThid)
     &                     + EVAP(J,0,myThid)*RPS_1*GRDSIG(K)
       ENDIF
      ENDDO

c_FM  DO K=1,NLEV
c_FM   DO J=1,NGP
c_FM    UTEND(J,K)=UTEND(J,K)+UT_PBL(J,K)
c_FM    VTEND(J,K)=VTEND(J,K)+VT_PBL(J,K)
c_FM    TTEND(J,K)=TTEND(J,K)+TT_PBL(J,K)
c_FM    QTEND(J,K)=QTEND(J,K)+QT_PBL(J,K)
c_FM   ENDDO
c_FM  ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
