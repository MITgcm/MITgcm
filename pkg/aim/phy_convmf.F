C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/phy_convmf.F,v 1.4 2001/09/25 19:50:28 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

cmolt      SUBROUTINE CONVMF (PSA,SE,QA,QSAT,
      SUBROUTINE CONVMF (PSA,TA,QA,QSAT,
     *                   IDEPTH,CBMF,PRECNV,DFSE,DFQA,
     I                   myThid)
C--
C--   SUBROUTINE CONVMF (PSA,SE,QA,QSAT,
C--  *                   IDEPTH,CBMF,PRECNV,DFSE,DFQA)
C--
C--   Purpose: Compute convective fluxes of dry static energy and moisture
C--            using a simplified mass-flux scheme
C--   Input:   PSA    = norm. surface pressure [p/p0]            (2-dim)
C--            SE     = dry static energy                        (3-dim)
C--            QA     = specific humidity [g/kg]                 (3-dim)
C--            QSAT   = saturation spec. hum. [g/kg]             (3-dim)
C--   Output:  IDEPTH = convection depth in layers               (2-dim)
C--            CBMF   = cloud-base mass flux                     (2-dim)
C--            PRECNV = convective precipitation [g/(m^2 s)]     (2-dim)
C--            DFSE   = net flux of d.s.en. into each atm. layer (3-dim)
C--            DFQA   = net flux of sp.hum. into each atm. layer (3-dim)
C--

      IMPLICIT rEAL*8 ( A-H,O-Z)
      INTEGER  myThid

C     Resolution parameters
C
#include "EEPARAMS.h"

#include "atparam.h"
#include "atparam1.h"
#include "Lev_def.h"
C
      INTEGER NLON, NLAT, NLEV, NGP
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )
C
C     Physical constants + functions of sigma and latitude
C
#include "com_physcon.h"
C
C     Convection constants
C
#include "com_cnvcon.h"
C
      REAL PSA(NGP), SE(NGP,NLEV), QA(NGP,NLEV), QSAT(NGP,NLEV)
C
      INTEGER IDEPTH(NGP)
      REAL CBMF(NGP), PRECNV(NGP), DFSE(NGP,NLEV), DFQA(NGP,NLEV)
C
      INTEGER ITOP(NGP)
      REAL SM(NGP,NLEV), ENTR(NGP,2:NLEV-1)
      REAL FM0(NGP), DENTR(NGP)
C
      REAL Th(NGP,NLEV), Ta(NGP,NLEV)
      REAL dThdp(NGP,NLEV), dThdpHat(NGP,NLEV)
      REAL stab(NGP,NLEV)
      REAL Prefw(NLEV), Prefs(NLEV)
      DATA Prefs / 75., 250., 500., 775., 950./
      DATA Prefw / 0., 150., 350., 650., 900./
      REAL Pground
      DATA pground /1000./
      REAL FDMUS

      INTEGER J, K, K1
C
C     1. Initialization of output and workspace arrays
C
      DO J=1,NGP
       FM0(J)=0.
       IF ( NLEVxy(J,myThid) .NE. 0 ) THEN
        FM0(J)=P0*DSIG(NLEVxy(J,myThid))/(GG*TRCNV*3600)
       ENDIF
       DENTR(J)=ENTMAX/(SIG(NLEV-1)-0.5)
      ENDDO
C   
      DO K=1,NLEV
        DO J=1,NGP
          DFSE(J,K)=0.0
          DFQA(J,K)=0.0
        ENDDO
      ENDDO
C
C
      DO J=1,NGP
        ITOP(J)  =NLEVxy(J,myThid)
        CBMF(J)  =0.0
        PRECNV(J)=0.0
      ENDDO
C
C     Saturation moist static energy
cmolt      DO J=1,NGP
cmolt        DO K=1,NLEVxy(J,myThid)
cmolt          SM(J,K)=SE(J,K)+ALHC*QSAT(J,K)
cmolt        ENDDO
cmolt      ENDDO
C
C     Entrainment profile (up to sigma = 0.5)
      DO J=1,NGP
        DO K=2,NLEVxy(J,myThid)-1
          ENTR(J,K)=MAX(0.,SIG(K)-0.5)*DENTR(J)
        ENDDO
      ENDDO
C
C--   2. Check of conditions for convection
C
C     2.1 Conditional instability
C
cmolt      DO J=1,NGP
cmolt        DO K=NLEVxy(J,myThid)-2,2,-1
cmolt          SMB=SM(J,K)+WVI(K,2)*(SM(J,K+1)-SM(J,K))
cmolt          IF (SM(J,NLEVxy(J,myThid)).GT.SMB) ITOP(J)=K
cmolt        ENDDO
cmolt      ENDDO
C
C New writing of the Conditional stability
C ----------------------------------------
      DO J=1,NGP
        DO k=1,NLEVxy(J,myThid)
          Th(J,K)=Ta(J,K)*(Pground/Prefs(k))**(RD/CP)
        ENDDO
      ENDDO
C
      DO J=1,NGP
        dThdp(J,1)=0.
        IF ( NLEVxy(J,myThid) .NE. 0 ) THEN
         dThdp(J,NLEVxy(J,myThid))=0.
        ENDIF
        DO k=2,NLEVxy(J,myThid)
          dThdp(J,K-1)=(Th(J,K-1)-Th(J,K))
     &              *((Prefw(k)/Pground)**(RD/CP))*CP
        ENDDO
      ENDDO
C
      DO J=1,NGP
       IF ( NLEVxy(J,myThid) .NE. 0 ) THEN
        dThdpHat(J,NLEVxy(J,myThid))=dThdp(J,NLEVxy(J,myThid))
       ENDIF
      ENDDO
C
      DO J=1,NGP
        DO k=NLEVxy(J,myThid)-1,2,-1
          dThdpHat(J,K)=dThdpHat(J,K+1)+dThdp(J,k)
        ENDDO
      ENDDO
C
      DO J=1,NGP
        DO k=2,NLEVxy(J,myThid)-1
          stab(J,K)=dThdpHat(J,K)+ALHC*(QSAT(J,K)-QSAT(J,NLEVxy(J,myThid)))
     &        -WVI(K,2)*(dThdp(J,K) +ALHC*(QSAT(J,K) -QSAT(J,K+1)) )
        ENDDO
      ENDDO
C
      DO J=1,NGP
        DO K=NLEVxy(J,myThid)-2,2,-1
          if(stab(J,K).lt.0.) ITOP(J)=K
        ENDDO
      ENDDO
C
C     2.2 Humidity exceeding prescribed threshold
C
      DO J=1,NGP
        IF ( NLEVxy(J,myThid) .NE. 0 ) THEN
         IF (QA(J,NLEVxy(J,myThid)).LT.RHBL*QSAT(J,NLEVxy(J,myThid)))
     &          ITOP(J)=NLEVxy(J,myThid)
        ENDIF
        IDEPTH(J)=NLEVxy(J,myThid)-ITOP(J)
      ENDDO
C
C--   3. Convection over selected grid-points
C
      DO 300 J=1,NGP
      IF (ITOP(J).EQ.NLEVxy(J,myThid)) GO TO 300
C
C       3.1 Boundary layer (cloud base)
C
        K =NLEVxy(J,myThid)
        K1=K-1
C
C       Dry static energy and moisture at upper boundary
cch        SB=SE(J,K1)+WVI(K1,2)*(SE(J,K)-SE(J,K1))
        QB=QA(J,K1)+WVI(K1,2)*(QA(J,K)-QA(J,K1))
cch        QB=QA(J,K1)
C
C       Cloud-base mass flux
        DQSAT=MAX(QSAT(J,K)-QB,0.05*QSAT(J,K))
        FMASS=FM0(J)*PSA(J)*(QA(J,K)-RHBL*QSAT(J,K))/DQSAT
        CBMF(J)=FMASS
C
C       Upward fluxes at upper boundary
cch        FUS=FMASS*SE(J,K)
#ifdef OLD_AIM_INTERFACE 
        FUQ=FMASS*QSAT(J,K)
#else 
        FUQ=FMASS*MAX( QSAT(J,K), MIN(QB,QA(J,K)) )
#endif
C
C       Downward fluxes at upper boundary
cch        FDS=FMASS*SB
        FDQ=FMASS*QB
C
C       Net flux of dry static energy and moisture
cch        DFSE(J,K)=FDS-FUS
        DFSE(J,K)=FMASS*dThdp(J,K1)*(1-WVI(K1,2))
        FDMUS=FMASS*dThdp(J,K1)*(1-WVI(K1,2))
        DFQA(J,K)=FDQ-FUQ
C
C       3.2 Intermediate layers (entrainment)
C
        DO K=NLEVxy(J,myThid)-1,ITOP(J)+1,-1
        K1=K-1
C
C         Fluxes at lower boundary
cch          DFSE(J,K)=FUS-FDS
          DFQA(J,K)=FUQ-FDQ
C
C         Mass entrainment
          ENMASS=ENTR(J,K)*PSA(J)*FMASS
          FMASS=FMASS+ENMASS
C
C         Upward fluxes at upper boundary
cch          FUS=FUS+ENMASS*SE(J,K)
          FUQ=FUQ+ENMASS*QA(J,K)
C
C         Downward fluxes at upper boundary
cch          SB=SE(J,K1)+WVI(K1,2)*(SE(J,K)-SE(J,K1))
          QB=QA(J,K1)+WVI(K1,2)*(QA(J,K)-QA(J,K1))
cch          QB=QA(J,K1)
cch          FDS=FMASS*SB
          FDQ=FMASS*QB
C
C         Net flux of dry static energy and moisture
cch          DFSE(J,K)=DFSE(J,K)+FDS-FUS
          DFSE(J,K)=FMASS*(1-WVI(K1,2))*dThdp(J,K1)+
     &              (FMASS-ENMASS)*WVI(K,2)*dThdp(J,K)
          FDMUS=FDMUS+ FMASS*(1-WVI(K1,2))*dThdp(J,K1)+
     &              (FMASS-ENMASS)*WVI(K,2)*dThdp(J,K)
          DFQA(J,K)=DFQA(J,K)+FDQ-FUQ
C
        ENDDO
c
C       3.3 Top layer (condensation and detrainment)
C
        K=ITOP(J)
C
C       Flux of convective precipitation
        QSATB=QSAT(J,K)+WVI(K,2)*(QSAT(J,K+1)-QSAT(J,K))
        PRECNV(J)=MAX(FUQ-FMASS*QSATB,0.0)
C
C       Net flux of dry static energy and moisture
cch        DFSE(J,K)=FUS-FDS+ALHC*PRECNV(J)
        DFSE(J,K)=-FDMUS+ALHC*PRECNV(J)
        DFQA(J,K)=FUQ-FDQ-PRECNV(J)
C       
 300  CONTINUE
C
      RETURN
      END
