C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_driver.F,v 1.1 2002/11/22 17:17:03 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE PHY_DRIVER (tYear, myTime, myIter, bi, bj, myThid )   

C------------------------
C from SPEDDY code: (part of original code left with c_FM)
C * S/R PHYPAR : except interp. dynamical Var. from Spectral of grid point
C                here dynamical var. are loaded within S/R AIM_DYN2AIM.
C * S/R FORDATE: only the CALL SOL_OZ (done once / day in SPEEDY)
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
C  Note: tendencies are not /dpFac here but later in AIM_AIM2DYN
C-------

      IMPLICIT NONE

C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 
#include "EEPARAMS.h"
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

c #include "com_sflcon.h"

C     Logical flags
c_FM  include "com_lflags.h"

C-- Routine arguments:
      _RL tYear, myTime
      INTEGER myIter, bi,bj, myThid

#ifdef ALLOW_AIM

C-- Local variables:
C    kGrd   = Ground level index              (2-dim)  
C    dpFac  = cell delta_P fraction           (3-dim)
      LOGICAL LRADSW
      INTEGER ICLTOP(NGP)
      INTEGER kGround(NGP)
      _RL dpFac(NGP,NLEV)
c_FM  REAL    RPS(NGP), ST4S(NGP)
      _RL ST4S(NGP)
      _RL PSG_1(NGP), RPS_1

      INTEGER J, K

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
       CALL RADSW (PSG,dpFac,QG1,RH(1,1,myThid),ALB1(1,myThid),
     I             FSOL, OZONE, OZUPP, ZENIT, STRATZ,
     O             TAU2, STRATC,
     O             ICLTOP,CLOUDC(1,myThid),
     O             TSR(1,myThid),SSR(1,myThid),TT_RSW(1,1,myThid),
     I             kGround,bi,bj,myThid)
 
        DO J=1,NGP
          CLTOP(J,myThid)=SIGH(ICLTOP(J)-1)*PSG_1(J)
        ENDDO
 
        DO K=1,NLEV
         DO J=1,NGP
          TT_RSW(J,K,myThid)=TT_RSW(J,K,myThid)*RPS_1*GRDSCP(K)
         ENDDO
        ENDDO
 
      ENDIF

C     3.2 Compute downward longwave fluxes
 
c_FM  CALL RADLW (-1,TG1,TS,ST4S,
c_FM &            OLR,SLR,TT_RLW)
      CALL RADLW (-1,TG1,TS(1,myThid),ST4S,
     &            OZUPP, STRATC, TAU2, FLUX, ST4A,
     O            OLR(1,myThid),SLR(1,myThid),TT_RLW(1,1,myThid),
     I            kGround,bi,bj,myThid)

C     3.3. Compute surface fluxes and land skin temperature
 
c_FM  CALL SUFLUX (PSG,UG1,VG1,TG1,QG1,RH,PHIG1,
c_FM &             PHIS0,FMASK1,STL1,SST1,SOILW1,SSR,SLR,
c_FM &             USTR,VSTR,SHF,EVAP,ST4S,
c_FM &             TS,TSKIN,U0,V0,T0,Q0) 
      CALL SUFLUX (PSG, TG1, QG1, RH(1,1,myThid), VsurfSq,
     I             WVSurf(1,myThid),csLat(1,myThid),fOrogr(1,myThid),
     I             FMASK1(1,myThid),STL1(1,myThid),SST1(1,myThid),
     I             SOILW1(1,myThid), SSR(1,myThid),SLR(1,myThid),
     O             SPEED0(1,myThid),DRAG(1,1,myThid),
     O             SHF(1,1,myThid), EVAP(1,1,myThid),
     O             ST4S,TS(1,myThid),TSKIN(1,myThid),
     O             T0(1,myThid),Q0(1,myThid),
     I             kGround,bi,bj,myThid)

C     3.4 Compute upward longwave fluxes, convert them to tendencies
C         and add shortwave tendencies

c_FM  CALL RADLW (1,TG1,TS,ST4S,
c_FM &            OLR,SLR,TT_RLW)
      CALL RADLW (1,TG1,TS(1,myThid),ST4S,
     &            OZUPP, STRATC, TAU2, FLUX, ST4A,
     O            OLR(1,myThid),SLR(1,myThid),TT_RLW(1,1,myThid),
     I            kGround,bi,bj,myThid)
 
      DO K=1,NLEV
       DO J=1,NGP
        TT_RLW(J,K,myThid)=TT_RLW(J,K,myThid)*RPS_1*GRDSCP(K)
c_FM    TTEND (J,K)=TTEND(J,K)+TT_RSW(J,K)+TT_RLW(J,K)
       ENDDO
      ENDDO 

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
     &                     + SHF(J,3,myThid) *RPS_1*GRDSCP(K)
        QT_PBL(J,K,myThid) = QT_PBL(J,K,myThid)
     &                     + EVAP(J,3,myThid)*RPS_1*GRDSIG(K)
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
