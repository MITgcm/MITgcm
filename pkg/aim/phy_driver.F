C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/phy_driver.F,v 1.3 2001/05/29 19:28:53 cnh Exp $
C $Name:  $

      SUBROUTINE PDRIVER (TYEAR, myThid)
C--
C--   SUBROUTINE PDRIVER (TYEAR)
C--
C--   Purpose: stand-alone driver for physical parametrization routines
C--   Input  :  TYEAR  : fraction of year (0 = 1jan.00, 1 = 31dec.24)
C--             grid-point model fields in common block: PHYGR1
C--             forcing fields in common blocks : LSMASK, FORFIX, FORCIN
C--   Output :  Diagnosed upper-air variables in common block: PHYGR2
C--             Diagnosed surface variables in common block: PHYGR3
C--             Physical param. tendencies in common block: PHYTEN
C--             Surface and upper boundary fluxes in common block: FLUXES
C--


      IMPLICIT rEAL*8 ( A-H,O-Z)

#include "EEPARAMS.h"

C     Resolution parameters
C
#include "atparam.h"
#include "atparam1.h"
C
      INTEGER NLON, NLAT, NLEV, NGP
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )
C
C     Constants + functions of sigma and latitude
C
#include "Lev_def.h"
#include "com_physcon.h"
C
C     Model variables, tendencies and fluxes on gaussian grid
C
#include "com_physvar.h"
C
C     Surface forcing fields (time-inv. or functions of seasonal cycle)
C
#include "com_forcing1.h"
#include "com_forcon.h"
#include "com_sflcon.h"

      REAL TYEAR
      INTEGER myThid

      INTEGER IDEPTH(NGP)
      REAL RPS(NGP), ALB1(NGP), FSOL1(NGP), OZONE1(NGP)

      REAL TAURAD(NGP,NLEV), ST4ARAD(NGP,NLEV,2)
CcnhDebugStarts
      REAL    AUX(NGP)
      REAL    Phymask(NGP,NLEV)
      real xminim
      REAL UT_VDI(NGP,NLEV), VT_VDI(NGP,NLEV), TT_VDI(NGP,NLEV)
      REAL QT_VDI(NGP,NLEV)
CcnhDebugEnds
      INTEGER J, K


C--   1. Compute surface variables

C     1.1 Surface pressure (ps), 1/ps and surface temperature 
C
      DO J=1,NGP
       PSG(J,myThid)=EXP(PSLG1(J,myThid))
       RPS(J)=1./PSG(J,myThid)
       TS(J,myThid) =SST1(J,myThid)+
     &  FMASK1(J,myThid)*(STL1(J,myThid)-SST1(J,myThid))
      ENDDO

C     1.2 Surface albedo:
C         defined as a weighed average of land and ocean albedos, where
C         land albedo depends linearly on snow depth (up to the SDALB
C         threshold) and sea albedo depends linearly on sea-ice fraction. 
C
      DALB=ALBICE-ALBSEA
      RSD=1./SDALB
C
CmoltBegin
      DO J=1,NGP
        ALB1(J)=ALB0(J,myThid)
      ENDDO
CmoltEnd

C--   2. Compute thermodynamic variables

C     2.1 Dry static energy

      DO K=1,NLEV
       DO J=1,NGP
        SE(J,K,myThid)=CP*TG1(J,K,myThid)+PHIG1(J,K,myThid)
       ENDDO
      ENDDO
C
C     2.2 Relative humidity and saturation spec. humidity
C
      DO K=1,NLEV
       CALL SHTORH (1,NGP,TG1(1,K,myThid),PSG(1,myThid),
     &              SIG(K),QG1(1,K,myThid),
     *              RH(1,K,myThid),QSAT(1,K,myThid),
     I              myThid)
      ENDDO
C
      DO K=1,NLEV
       DO J=1,NGP
        phymask(J,K)=0.
        IF (Tg1(J,K,myThid).ne.0.) THEN
         phymask(J,K)=1.
        ENDIF
        QSAT(J,K,myThid)=QSAT(J,K,myThid)*Phymask(J,K)
        QG1(J,K,myThid)=QG1(J,K,myThid)*Phymask(J,K)
        RH(J,K,myThid)=RH(J,K,myThid)*Phymask(J,K)
       ENDDO
      ENDDO
cdbgch
C
C--   3. Precipitation

C     3.1 Deep convection
C
cch      CALL CONVMF (PSG,SE,QG1,QSAT,
      CALL CONVMF (PSG(1,myThid),TG1(1,1,myThid),
     &             QG1(1,1,myThid),QSAT(1,1,myThid),
     *             IDEPTH,CBMF(1,myThid),PRECNV(1,myThid),
     &             TT_CNV(1,1,myThid),QT_CNV(1,1,myThid),
     I             myThid)

C
      DO K=2,NLEV
       DO J=1,NGP
        TT_CNV(J,K,myThid)=TT_CNV(J,K,myThid)*RPS(J)*GRDSCP(K)
        QT_CNV(J,K,myThid)=QT_CNV(J,K,myThid)*RPS(J)*GRDSIG(K)
       ENDDO
      ENDDO

C     3.2 Large-scale condensation

      CALL LSCOND (PSG(1,myThid),QG1(1,1,myThid),QSAT(1,1,myThid),
     *             PRECLS(1,myThid),TT_LSC(1,1,myThid),
     &             QT_LSC(1,1,myThid),
     I             myThid)

C
C--   4. Radiation (shortwave and longwave) 

C     4.1 Compute climatological forcing

      CALL SOL_OZ (SOLC,TYEAR,FSOL1,OZONE1,
     I             myThid)

C     4.2 Compute shortwave tendencies and initialize lw transmissivity
C     (The sw radiation may be called at selected time steps)

      CALL RADSW (PSG(1,myThid),QG1(1,1,myThid),RH(1,1,myThid),
     *            FSOL1,OZONE1,ALB1,TAURAD,
     *            CLOUDC(1,myThid),TSR(1,myThid),SSR(1,myThid),
     &            TT_RSW(1,1,myThid),
     I            myThid)

C     4.3 Compute longwave fluxes 

      CALL RADLW (1,TG1(1,1,myThid),TS(1,myThid),ST4S(1,myThid),
     &            TAURAD, ST4ARAD,
     *            OLR(1,myThid),SLR(1,myThid),TT_RLW(1,1,myThid),
     &            SLR_DOWN(1,myThid),
     I            myThid)

      DO K=1,NLEV
       DO J=1,NGP
        TT_RSW(J,K,myThid)=TT_RSW(J,K,myThid)*RPS(J)*GRDSCP(K)
        TT_RLW(J,K,myThid)=TT_RLW(J,K,myThid)*RPS(J)*GRDSCP(K)
       ENDDO
      ENDDO

C
C--   5. PBL interactions with lower troposphere and surface

C     5.1. Surface fluxes (from climatological surface temperature)

cch Attention the pressure used is a the last T level and
Cch  not at the last W level
C --------------------------------
      CALL SUFLUX (PNLEVW(1,myThid),
     &             UG1(1,1,myThid),VG1(1,1,myThid),
     &             TG1(1,1,myThid),QG1(1,1,myThid),
     &             RH(1,1,myThid),QSAT(1,1,myThid),
     &             VsurfSq(1,myThid),PHIG1(1,1,myThid),
     &             PHI0(1,myThid),FMASK1(1,myThid),
     &             STL1(1,myThid),SST1(1,myThid),SOILQ1(1,myThid),
     &             SSR(1,myThid),SLR(1,myThid),
     &             DRAG(1,myThid),
     &             USTR(1,1,myThid),VSTR(1,1,myThid),SHF(1,1,myThid),
     &             EVAP(1,1,myThid),T0(1,1,myThid),Q0(1,myThid),
     &             QSAT0(1,1,myThid),SPEED0(1,myThid),
     I             myThid)

C
C     remove when vdifsc is implemented
      DO K=1,NLEV
       DO J=1,NGP
        UT_PBL(J,K,myThid)=0.
        VT_PBL(J,K,myThid)=0.
        TT_PBL(J,K,myThid)=0.
        QT_PBL(J,K,myThid)=0.
       ENDDO
      ENDDO
c
C
c
C     5.3 Add surface fluxes and convert fluxes to tendencies

      DO J=1,NGP
       IF ( NLEVxy(J,myThid) .GT. 0 ) THEN
        UT_PBL(J,NLEVxy(J,myThid),myThid)=UT_PBL(J,NLEVxy(J,myThid),myThid)+ USTR(J,3,myThid)
        VT_PBL(J,NLEVxy(J,myThid),myThid)=VT_PBL(J,NLEVxy(J,myThid),myThid)+ VSTR(J,3,myThid)
        TT_PBL(J,NLEVxy(J,myThid),myThid)=TT_PBL(J,NLEVxy(J,myThid),myThid)+ SHF(J,3,myThid)
        QT_PBL(J,NLEVxy(J,myThid),myThid)=QT_PBL(J,NLEVxy(J,myThid),myThid)+ EVAP(J,3,myThid)
       ENDIF
      ENDDO
C
Cdbgch
      DO J=1,NGP
       IF ( NLEVxy(J,myThid) .GT. 0 ) THEN
        DO K=NLEVxy(J,myThid)-1,NLEVxy(J,myThid)
         UT_PBL(J,K,myThid)=UT_PBL(J,K,myThid)*GRDSIG(K)
         VT_PBL(J,K,myThid)=VT_PBL(J,K,myThid)*GRDSIG(K)
         TT_PBL(J,K,myThid)=TT_PBL(J,K,myThid)*GRDSCP(K)
         QT_PBL(J,K,myThid)=QT_PBL(J,K,myThid)*GRDSIG(K)
        ENDDO
       ENDIF
      ENDDO
C
C     5.2 Vertical diffusion and shallow convection (not yet implemented)
C
      CALL VDIFSC (UG1(1,1,myThid),VG1(1,1,myThid),
     &             TG1(1,1,myThid),RH(1,1,myThid), 
     &             QG1(1,1,myThid), QSAT(1,1,myThid),
     *             UT_VDI,VT_VDI,TT_VDI,QT_VDI,
     I             myThid)
C
      DO K=1,NLEV
       DO J=1,NGP
        UT_PBL(J,K,myThid)=UT_PBL(J,K,myThid)+ UT_VDI(J,K)
        VT_PBL(J,K,myThid)=VT_PBL(J,K,myThid)+ VT_VDI(J,K)
        TT_PBL(J,K,myThid)=TT_PBL(J,K,myThid)+ TT_VDI(J,K)
        QT_PBL(J,K,myThid)=QT_PBL(J,K,myThid)+ QT_VDI(J,K)
       ENDDO
      ENDDO
C

CdbgC--
      RETURN
      END
