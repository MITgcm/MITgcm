C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/phy_driver.F,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Name:  $

      SUBROUTINE PDRIVER (TYEAR)
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


C     Resolution parameters
C
#include "atparam.h"
#include "atparam1.h"
C
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

      INTEGER IDEPTH(NGP)
      REAL RPS(NGP), ALB1(NGP), FSOL1(NGP), OZONE1(NGP)
CcnhDebugStarts
      LOGICAL CALL1
      DATA    CALL1 /.TRUE./
      SAVE    CALL1
      REAL    AUX(NGP)
      REAL    Phymask(NGP,NLEV)
      real xminim
      REAL UT_VDI(NGP,NLEV), VT_VDI(NGP,NLEV), TT_VDI(NGP,NLEV)
      REAL QT_VDI(NGP,NLEV)
CcnhDebugEnds


C--   1. Compute surface variables

C     1.1 Surface pressure (ps), 1/ps and surface temperature 
C
      DO J=1,NGP
       PSG(J)=EXP(PSLG1(J))
       RPS(J)=1./PSG(J)
       TS(J) =SST1(J)+FMASK1(J)*(STL1(J)-SST1(J))
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
        ALB1(J)=ALB0(J)
      ENDDO
CmoltEnd

C--   2. Compute thermodynamic variables

C     2.1 Dry static energy

      DO K=1,NLEV
       DO J=1,NGP
        SE(J,K)=CP*TG1(J,K)+PHIG1(J,K)
       ENDDO
      ENDDO
C
C     2.2 Relative humidity and saturation spec. humidity
C
      DO K=1,NLEV
       CALL SHTORH (1,NGP,TG1(1,K),PSG,SIG(K),QG1(1,K),
     *              RH(1,K),QSAT(1,K))
      ENDDO
C
      DO K=1,NLEV
       DO J=1,NGP
        phymask(J,K)=0.
        IF (Tg1(J,K).ne.0.) THEN
         phymask(J,K)=1.
        ENDIF
        QSAT(J,K)=QSAT(J,K)*Phymask(J,K)
        QG1(J,K)=QG1(J,K)*Phymask(J,K)
        RH(J,K)=RH(J,K)*Phymask(J,K)
       ENDDO
      ENDDO
cdbgch
C
C--   3. Precipitation

C     3.1 Deep convection
C
cch      CALL CONVMF (PSG,SE,QG1,QSAT,
      CALL CONVMF (PSG,TG1,QG1,QSAT,
     *             IDEPTH,CBMF,PRECNV,TT_CNV,QT_CNV)
C
      DO K=2,NLEV
       DO J=1,NGP
        TT_CNV(J,K)=TT_CNV(J,K)*RPS(J)*GRDSCP(K)
        QT_CNV(J,K)=QT_CNV(J,K)*RPS(J)*GRDSIG(K)
       ENDDO
      ENDDO

C     3.2 Large-scale condensation

      CALL LSCOND (PSG,QG1,QSAT,
     *             PRECLS,TT_LSC,QT_LSC)

C
C--   4. Radiation (shortwave and longwave) 

C     4.1 Compute climatological forcing

      CALL SOL_OZ (SOLC,TYEAR,FSOL1,OZONE1)

C     4.2 Compute shortwave tendencies and initialize lw transmissivity
C     (The sw radiation may be called at selected time steps)

      CALL RADSW (PSG,QG1,RH,
     *            FSOL1,OZONE1,ALB1,
     *            CLOUDC,TSR,SSR,TT_RSW)

C     4.3 Compute longwave fluxes 

      CALL RADLW (1,TG1,TS,ST4S,
     *            OLR,SLR,TT_RLW,SLR_DOWN)

      DO K=1,NLEV
       DO J=1,NGP
        TT_RSW(J,K)=TT_RSW(J,K)*RPS(J)*GRDSCP(K)
        TT_RLW(J,K)=TT_RLW(J,K)*RPS(J)*GRDSCP(K)
       ENDDO
      ENDDO
C
C--   5. PBL interactions with lower troposphere and surface

C     5.1. Surface fluxes (from climatological surface temperature)

cch Attention the pressure used is a the last T level and
Cch  not at the last W level
C --------------------------------
      CALL SUFLUX (PNLEVW,UG1,VG1,TG1,QG1,RH,QSAT,PHIG1,
     *             PHI0,FMASK1,STL1,SST1,SOILQ1,SSR,SLR,
     *             USTR,VSTR,SHF,EVAP,T0,Q0,QSAT0,SPEED0)

C
C     remove when vdifsc is implemented
      DO K=1,NLEV
       DO J=1,NGP
        UT_PBL(J,K)=0.
        VT_PBL(J,K)=0.
        TT_PBL(J,K)=0.
        QT_PBL(J,K)=0.
       ENDDO
      ENDDO
c
C
c
C     5.3 Add surface fluxes and convert fluxes to tendencies

      DO J=1,NGP
       IF ( NLEVxy(J) .GT. 0 ) THEN
        UT_PBL(J,NLEVxy(J))=UT_PBL(J,NLEVxy(J))+ USTR(J,3)
        VT_PBL(J,NLEVxy(J))=VT_PBL(J,NLEVxy(J))+ VSTR(J,3)
        TT_PBL(J,NLEVxy(J))=TT_PBL(J,NLEVxy(J))+ SHF(J,3)
        QT_PBL(J,NLEVxy(J))=QT_PBL(J,NLEVxy(J))+ EVAP(J,3)
       ENDIF
      ENDDO
C
Cdbgch
      DO J=1,NGP
       IF ( NLEVxy(J) .GT. 0 ) THEN
        DO K=NLEVxy(J)-1,NLEVxy(J)
         UT_PBL(J,K)=UT_PBL(J,K)*GRDSIG(K)
         VT_PBL(J,K)=VT_PBL(J,K)*GRDSIG(K)
         TT_PBL(J,K)=TT_PBL(J,K)*GRDSCP(K)
         QT_PBL(J,K)=QT_PBL(J,K)*GRDSIG(K)
        ENDDO
       ENDIF
      ENDDO
C
C     5.2 Vertical diffusion and shallow convection (not yet implemented)
C
      CALL VDIFSC (UG1,VG1,TG1,RH, QG1, QSAT,
     *             UT_VDI,VT_VDI,TT_VDI,QT_VDI)
C
      DO K=1,NLEV
       DO J=1,NGP
        UT_PBL(J,K)=UT_PBL(J,K)+ UT_VDI(J,K)
        VT_PBL(J,K)=VT_PBL(J,K)+ VT_VDI(J,K)
        TT_PBL(J,K)=TT_PBL(J,K)+ TT_VDI(J,K)
        QT_PBL(J,K)=QT_PBL(J,K)+ QT_VDI(J,K)
       ENDDO
      ENDDO
C

CdbgC--
      RETURN
      END
