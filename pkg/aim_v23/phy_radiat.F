C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_radiat.F,v 1.7 2013/08/11 05:03:39 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

C--  File phy_radiat.F:
C--   Contents
C--   o SOL_OZ
C--   o RADSW
C--   o RADLW
C--   o RADSET

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: SOL_OZ (SOLC,TYEAR)
C !INTERFACE:
      SUBROUTINE SOL_OZ (SOLC, TYEAR, SLAT, CLAT,
     O                   FSOL, OZONE, OZUPP, ZENIT, STRATZ,
     I                   bi, bj, myThid)
C !DESCRIPTION: \bv
C   Purpose: Compute the flux of incoming solar radiation
C            and a climatological ozone profile for SW absorption
C   Input:   SOLC   = solar constant (area averaged)
C            TYEAR  = time as fraction of year (0-1, 0 = 1jan.h00)
C            SLAT   = sin(lat)
C            CLAT   = cos(lat)
C   Output:  FSOL   = flux of incoming solar radiation
C            OZONE  = flux absorbed by ozone (lower stratos.)
C            OZUPP  = flux absorbed by ozone (upper stratos.)
C            ZENIT  = function of solar zenith angle
C            STRATZ = ?
C   Updated common blocks: RADZON
C *==========================================================*
C \ev

C !USES:
      IMPLICIT NONE

C     Resolution parameters
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AIM_PARAMS.h"

C     Constants + functions of sigma and latitude
#include "com_physcon.h"
C     Radiation constants
#include "com_radcon.h"

C !INPUT/OUTPUT PARAMETERS:
      INTEGER  bi, bj, myThid
      _RL SOLC, TYEAR
      _RL SLAT(NGP), CLAT(NGP)
      _RL FSOL(NGP), OZONE(NGP), OZUPP(NGP), ZENIT(NGP), STRATZ(NGP)
CEOP

#ifdef ALLOW_AIM
C !LOCAL VARIABLES:
      INTEGER J, NZEN
      _RL ALPHA, CSR1, CSR2, COZ1, COZ2
      _RL AZEN, RZEN, CZEN, SZEN, AST, FS0, FLAT2
#ifdef ALLOW_INSOLATION
      _RL TanDelcl, cosH, HourAng, TanLat
      _RL largeTan
      largeTan = 1. _d 16
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     ALPHA = year phase ( 0 - 2pi, 0 = winter solstice = 22dec.h00 )
      ALPHA = 4. _d 0*ASIN(1. _d 0)*(TYEAR+10. _d 0/365. _d 0)

      CSR1=-0.796 _d 0*COS(ALPHA)
      CSR2= 0.147 _d 0*COS(2. _d 0*ALPHA)-0.477 _d 0
      COZ1= 1.0 _d 0 * COS(ALPHA)
      COZ2= 1.8 _d 0
C
      AZEN=1.0
      NZEN=2

#ifdef ALLOW_INSOLATION
      SZEN = - SIN(  OBLIQ * PI/180. _d 0) * COS(ALPHA)
      RZEN = ASIN( SZEN )
      CZEN =  COS( RZEN )
      IF ( SZEN .EQ. 1. _d 0 ) THEN
         TanDelcl = largeTan
      ELSEIF ( SZEN .EQ. -1. _d 0 ) THEN
         TanDelcl =-largeTan
      ELSE
         TanDelcl = SZEN / CZEN
      ENDIF
#else
      RZEN=-COS(ALPHA)*23.45 _d 0*ASIN(1. _d 0)/90. _d 0
      CZEN=COS(RZEN)
      SZEN=SIN(RZEN)
#endif

      AST=0.025 _d 0
      FS0=10. _d 0
C     FS0=16.-8.*COS(ALPHA)

      DO J=1,NGP

        FLAT2 = 1.5 _d 0*SLAT(J)**2 - 0.5 _d 0

#ifndef ALLOW_INSOLATION
C       solar radiation at the top
        FSOL(J) = SOLC*
     &     MAX( 0. _d 0, 1. _d 0+CSR1*SLAT(J)+CSR2*FLAT2 )
#else
        IF ( CLAT(J) .EQ. 0. _d 0 ) THEN
           TanLat = SIGN(1. _d 0, SLAT(J) ) * largeTan
        ELSE
           TanLat = SLAT(J)/CLAT(J)
        ENDIF
        cosH     = - TanLat * TanDelcl
        cosH     = MAX(MIN(cosH,1. _d 0), -1. _d 0)
        HourAng  =  ACOS( cosH )
        FSOL(J)  = 4. _d 0 / PI * SOLC *
     &   (SLAT(J)*SZEN*HourAng+CLAT(J)*CZEN*SIN(HourAng))
#endif

C       ozone depth in upper and lower stratosphere
        OZUPP(J) = EPSSW*(1.-FLAT2)
        OZONE(J) = EPSSW*(1.+COZ1*SLAT(J)+COZ2*FLAT2)

C       zenith angle correction to (downward) absorptivity
        ZENIT(J) = 1. + AZEN*
     &    (1. _d 0-(CLAT(J)*CZEN+SLAT(J)*SZEN))**NZEN

C       ozone absorption in upper and lower stratosphere
        OZUPP(J)=FSOL(J)*OZUPP(J)*ZENIT(J)
        OZONE(J)=FSOL(J)*OZONE(J)*ZENIT(J)
        STRATZ(J)=AST*FSOL(J)*CLAT(J)**3
     &           +MAX( FS0-FSOL(J), 0. _d 0 )

      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( FSOL,
     &                        'FSOL    ', 1, 1, 3,bi,bj, myThid )
      ENDIF
#endif

#endif /* ALLOW_AIM */
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: RADSW (PSA,QA,RH,ALB,
C    &             ICLTOP,CLOUDC,FTOP,FSFC,DFABS)
C !INTERFACE:
      SUBROUTINE RADSW (PSA,dpFac,QA,RH,ALB,
     I                  FSOL, OZONE, OZUPP, ZENIT, STRATZ,
     O                  TAU2, STRATC,
     O                  ICLTOP,CLOUDC,FTOP,FSFC,UPSWG,DFABS,
     I                  kGrd,bi,bj,myThid)
C !DESCRIPTION: \bv
C   Purpose: Compute the absorption of shortwave radiation and
C            initialize arrays for longwave-radiation routines
C   Input:   PSA    = norm. surface pressure [p/p0]           (2-dim)
C            dpFac  = cell delta_P fraction                   (3-dim)
C            QA     = specific humidity [g/kg]                (3-dim)
C            RH     = relative humidity                       (3-dim)
C            ALB    = surface albedo                          (2-dim)
C   Output:  ICLTOP = cloud top level                             (2-dim)
C            CLOUDC = total cloud cover                           (2-dim)
C            FTOP   = net downw. flux of sw rad. at the atm. top  (2-dim)
C            FSFC   = net downw. flux of sw rad. at the surface   (2-dim)
C            UPSWG  = upward flux of sw rad. at the surface       (2-dim)
C            DFABS  = flux of sw rad. absorbed by each atm. layer (3-dim)
C  Input:    kGrd   = Ground level index                      (2-dim)
C            bi,bj  = tile index
C            myThid = Thread number for this instance of the routine

C !USES:
      IMPLICIT NONE

C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"

C     Constants + functions of sigma and latitude
#include "com_physcon.h"
C     Radiation parameters
#include "com_radcon.h"

C !INPUT/OUTPUT PARAMETERS:
      _RL PSA(NGP),dpFac(NGP,NLEV),QA(NGP,NLEV),RH(NGP,NLEV)
      _RL ALB(NGP,0:3)
      INTEGER  ICLTOP(NGP)
      _RL CLOUDC(NGP), FTOP(NGP), FSFC(NGP,0:3), DFABS(NGP,NLEV)
      _RL UPSWG(NGP)

      _RL FSOL(NGP), OZONE(NGP), OZUPP(NGP), ZENIT(NGP), STRATZ(NGP)
      _RL TAU2(NGP,NLEV,NBAND),STRATC(NGP)
c     _RL FLUX(NGP,4)

      INTEGER  kGrd(NGP)
      INTEGER  bi, bj, myThid
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AIM
C !LOCAL VARIABLES:
c     _RL  QCLOUD(NGP), ACLOUD(NGP), PSAZ(NGP),
      _RL  QCLOUD(NGP), ACLOUD(NGP),
     &     ALBTOP(NGP,NLEV), FREFL(NGP,NLEV), FLUX(NGP,2)
#ifdef ALLOW_CLOUD_3D
C_DE       CLDCLW = Local cloud cover                           (3-dim)
      _RL CLDCLW(NGP,NLEV), ACLDLW(NGP,NLEV)
#endif

C-  jmc: define "FLUX" as a local variable & remove Equivalences:
c     EQUIVALENCE (ALBTOP(1,1),TAU2(1,1,3))
c     EQUIVALENCE ( FREFL(1,1),TAU2(1,1,4))
      INTEGER NL1(NGP)
      INTEGER K, J
      LOGICAL makeClouds
      _RL FBAND1, FBAND2, RRCL, RQCL, DQACL, QACL3
      _RL ABS1, DELTAP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      FBAND2=0.05 _d 0
      FBAND1=1.-FBAND2

      DO J=1,NGP
        NL1(J)=kGrd(J)-1
      ENDDO

C--   1.  Cloud cover:
C         defined as a linear fun. of the maximum relative humidity
C         in all tropospheric layers above PBL:
C         CLOUDC =  0 for RHmax < RHCL1, = 1 for RHmax > RHCL2.
C         This value is reduced by a factor (Qbase/QACL) if the
C         cloud-base absolute humidity Qbase < QACL.
C
      makeClouds = ICLTOP(1) .GE. 0
      RRCL=1./(RHCL2-RHCL1)
      RQCL=1./QACL2
C
      DO J=1,NGP
        CLOUDC(J)=0.
        QCLOUD(J)=0.
        ICLTOP(J)=NLEV+1
        FREFL(J,1)=0.
      ENDDO

      DO K=1,NLEV
       DO J=1,NGP
         ALBTOP(J,K)=0.
#ifdef ALLOW_CLOUD_3D
         CLDCLW(J,K)=0.
#endif
       ENDDO
      ENDDO
C
      IF ( makeClouds ) THEN
C-    skipp this part for clear-sky diagnostics

       DQACL=(QACL2-QACL1)/(0.5 _d 0 - SIG(2))
       DO J=1,NGP
        ICLTOP(J)= kGrd(J)
        DO K=NL1(J),2,-1
         QACL3=MIN(QACL2,QACL1+DQACL*(SIG(K)-SIG(2)))
         IF (RH(J,K).GT.RHCL1.AND.QA(J,K).GT.QACL1) THEN
            CLOUDC(J)=MAX(CLOUDC(J),RH(J,K)-RHCL1)
            IF (QA(J,K).GT.QACL3) ICLTOP(J)=K
#ifdef ALLOW_CLOUD_3D
            CLDCLW(J,K)=MAX(0. _d 0,RH(J,K)-RHCL1)
            CLDCLW(J,K)=MIN(1. _d 0,CLDCLW(J,K)*RRCL)
#endif
         ENDIF
        ENDDO
       ENDDO

       DO J=1,NGP
        IF (kGrd(J).NE.0)
     &  QCLOUD(J)= MAX( QA(J,kGrd(J)), QA(J,NL1(J)) )
        CLOUDC(J)=MIN(1. _d 0,CLOUDC(J)*RRCL)
        IF (CLOUDC(J).GT.0.0) THEN
          CLOUDC(J)=CLOUDC(J)*MIN(1. _d 0,QCLOUD(J)*RQCL)
#ifdef ALLOW_CLOUD_3D
          DO K=NL1(J),2,-1
             CLDCLW(J,K)=CLDCLW(J,K)*MIN(1. _d 0,QCLOUD(J)*RQCL)
          ENDDO
#endif
          ALBTOP(J,ICLTOP(J))=ALBCL*CLOUDC(J)
        ELSE
          ICLTOP(J)=NLEV+1
        ENDIF
       ENDDO

C-    end if makeClouds
      ENDIF

C
C--   2. Shortwave transmissivity:
C        function of layer mass, ozone (in the statosphere),
C        abs. humidity and cloud cover (in the troposphere)

      DO J=1,NGP
c_FM    PSAZ(J)=PSA(J)*ZENIT(J)
        ACLOUD(J)=CLOUDC(J)*(ABSCL1+ABSCL2*QCLOUD(J))
      ENDDO

      DO J=1,NGP
c_FM    DELTAP=PSAZ(J)*DSIG(1)
        DELTAP=ZENIT(J)*DSIG(1)*dpFac(J,1)
        TAU2(J,1,1)=EXP(-DELTAP*ABSDRY)
      ENDDO
C
      DO J=1,NGP
       DO K=2,NL1(J)
c_FM     ABS1=ABSDRY+ABSAER*SIG(K)**2
c_FM     DELTAP=PSAZ(J)*DSIG(K)
         ABS1=ABSDRY+ABSAER*(SIG(K)/PSA(J))**2
         DELTAP=ZENIT(J)*DSIG(K)*dpFac(J,K)
         IF (K.EQ.ICLTOP(J)) THEN
           TAU2(J,K,1)=EXP(-DELTAP*
     &                 (ABS1+ABSWV1*QA(J,K)+2.*ACLOUD(J)))
         ELSE IF (K.GT.ICLTOP(J)) THEN
           TAU2(J,K,1)=EXP(-DELTAP*
     &                 (ABS1+ABSWV1*QA(J,K)+ACLOUD(J)))
         ELSE
           TAU2(J,K,1)=EXP(-DELTAP*(ABS1+ABSWV1*QA(J,K)))
         ENDIF
       ENDDO
      ENDDO

c_FM  ABS1=ABSDRY+ABSAER*SIG(NLEV)**2
      DO J=1,NGP
       K = kGrd(J)
       ABS1=ABSDRY+ABSAER*(SIG(K)/PSA(J))**2
c_FM    DELTAP=PSAZ(J)*DSIG(NLEV)
        DELTAP=ZENIT(J)*DSIG(K)*dpFac(J,K)
        TAU2(J,K,1)=EXP(-DELTAP*(ABS1+ABSWV1*QA(J,K)))
      ENDDO

      DO J=1,NGP
       DO K=2,kGrd(J)
         DELTAP=ZENIT(J)*DSIG(K)*dpFac(J,K)
         TAU2(J,K,2)=EXP(-DELTAP*ABSWV2*QA(J,K))
       ENDDO
      ENDDO
C
C---  3. Shortwave downward flux
C
C     3.1  Absorption in the stratosphere

C     3.1.1 Initialization of fluxes (subtracting
C           ozone absorption in the upper stratosphere)

      DO J=1,NGP
        FTOP(J)  =FSOL(J)
        FLUX(J,1)=FSOL(J)*FBAND1-OZUPP(J)
        FLUX(J,2)=FSOL(J)*FBAND2
        STRATC(J)=STRATZ(J)*PSA(J)
      ENDDO

C     3.1.2 Ozone and dry-air absorption
C           in the lower (modelled) stratosphere

      DO J=1,NGP
        DFABS(J,1)=FLUX(J,1)
        FLUX (J,1)=TAU2(J,1,1)*(FLUX(J,1)-OZONE(J)*PSA(J))
        DFABS(J,1)=DFABS(J,1)-FLUX(J,1)
      ENDDO

C     3.3  Absorption and reflection in the troposphere
C
      DO J=1,NGP
       DO K=2,kGrd(J)
         FREFL(J,K)=FLUX(J,1)*ALBTOP(J,K)
         FLUX (J,1)=FLUX(J,1)-FREFL(J,K)
         DFABS(J,K)=FLUX(J,1)
         FLUX (J,1)=TAU2(J,K,1)*FLUX(J,1)
         DFABS(J,K)=DFABS(J,K)-FLUX(J,1)
       ENDDO
      ENDDO

      DO J=1,NGP
       DO K=2,kGrd(J)
         DFABS(J,K)=DFABS(J,K)+FLUX(J,2)
         FLUX (J,2)=TAU2(J,K,2)*FLUX(J,2)
         DFABS(J,K)=DFABS(J,K)-FLUX(J,2)
       ENDDO
      ENDDO

C
C---  4. Shortwave upward flux
C
C     4.1  Absorption and reflection at the surface
C
      DO J=1,NGP
C      for each surface type:
        FSFC(J,1)=FLUX(J,1)*(1.-ALB(J,1))+FLUX(J,2)
        FSFC(J,2)=FLUX(J,1)*(1.-ALB(J,2))+FLUX(J,2)
        FSFC(J,3)=FLUX(J,1)*(1.-ALB(J,3))+FLUX(J,2)
C      weighted average according to land/sea/sea-ice fraction:
        FSFC(J,0)=FLUX(J,1)+FLUX(J,2)
        FLUX(J,1)=FLUX(J,1)*ALB(J,0)
        FSFC(J,0)=FSFC(J,0)-FLUX(J,1)
      ENDDO

C--   Store upward shortwave flux at the surface for diagnostics purpose
      DO J=1,NGP
        UPSWG(J)=FLUX(J,1)
      ENDDO
C
C     4.2  Absorption of upward flux
C
      DO K=NLEV,1,-1
       DO J=1,NGP
        IF ( K .LE. kGrd(J) ) THEN
         DFABS(J,K)=DFABS(J,K)+FLUX(J,1)
         FLUX (J,1)=TAU2(J,K,1)*FLUX(J,1)
         DFABS(J,K)=DFABS(J,K)-FLUX(J,1)
         FLUX (J,1)=FLUX(J,1)+FREFL(J,K)
        ELSE
         DFABS(J,K)= 0. _d 0
        ENDIF
       ENDDO
      ENDDO
C
C     4.3  Net solar radiation = incoming - outgoing
C
      DO J=1,NGP
        FTOP(J)=FTOP(J)-FLUX(J,1)
      ENDDO

C
C---  5.  Initialization of longwave radiation model
C
C     5.1  Longwave transmissivity:
C          function of layer mass, abs. humidity and cloud cover.

#ifdef ALLOW_CLOUD_3D
      DO K=2,NLEV
        DO J=1,NGP
          ACLDLW(J,K)=CLDCLW(J,K)*(ABLCL1+ABLCL2*QCLOUD(J))
        ENDDO
      ENDDO
#else
      DO J=1,NGP
        ACLOUD(J)=CLOUDC(J)*(ABLCL1+ABLCL2*QCLOUD(J))
      ENDDO
#endif

      DO J=1,NGP
c_FM    DELTAP=PSA(J)*DSIG(1)
        DELTAP=DSIG(1)*dpFac(J,1)
        TAU2(J,1,1)=EXP(-DELTAP*ABLWIN)
        TAU2(J,1,2)=EXP(-DELTAP*ABLCO2)
        TAU2(J,1,3)=1.
        TAU2(J,1,4)=1.
      ENDDO

      DO K=2,NLEV
       DO J=1,NGP
c_FM     DELTAP=PSA(J)*DSIG(K)
         DELTAP=DSIG(K)*dpFac(J,K)
         IF ( K.GE.ICLTOP(J).AND.K.NE.kGrd(J) ) THEN
#ifdef ALLOW_CLOUD_3D
           TAU2(J,K,1)=EXP(-DELTAP*(ABLWIN+ACLDLW(J,K)))
#else
           TAU2(J,K,1)=EXP(-DELTAP*(ABLWIN+ACLOUD(J)))
#endif
         ELSE
           TAU2(J,K,1)=EXP(-DELTAP*ABLWIN)
         ENDIF
         TAU2(J,K,2)=EXP(-DELTAP*ABLCO2)
         TAU2(J,K,3)=EXP(-DELTAP*ABLWV1*QA(J,K))
         TAU2(J,K,4)=EXP(-DELTAP*ABLWV2*QA(J,K))
       ENDDO
      ENDDO

#if (defined ALLOW_CLOUD_3D && defined ALLOW_DIAGNOSTICS)
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( CLDCLW,
     &                        'CLDCLW  ',-1, Nr, 3,bi,bj, myThid )
      ENDIF
#endif

#endif /* ALLOW_AIM */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: RADLW (IMODE,TA,TS,ST4S,
C    &             FTOP,FSFC,DFABS)
C !INTERFACE:
      SUBROUTINE RADLW (IMODE,TA,TS,ST4S,
     I                  OZUPP, STRATC, TAU2,
     &                  FLUX, ST4A,
     &                  FTOP,FSFC,DFABS,
     I                  kGrd,bi,bj,myThid)

C !DESCRIPTION: \bv
C   Purpose: Compute the absorption of longwave radiation
C   Input:   IMODE  = index for operation mode
C                     -1 : downward flux only
C                      0 : downward + upward flux
C                     +1 : upward flux only
C            TA     = absolute temperature (3-dim)
C            TS     = surface temperature                  [if IMODE=0,1]
C            ST4S   = surface blackbody emission             [if IMODE=1]
C            FSFC   = FSFC  output from RADLW(-1,... )       [if IMODE=1]
C            DFABS  = DFABS output from RADLW(-1,... )       [if IMODE=1]
C   Output:  ST4S   = surface blackbody emission             [if IMODE=0]
C            FTOP   = outgoing flux of lw rad. at the top  [if IMODE=0,1]
C            FSFC   = downward flux of lw rad. at the sfc. [if IMODE= -1]
C                     net upw. flux of lw rad. at the sfc. [if IMODE=0,1]
C            DFABS  = flux of lw rad. absorbed by each atm. layer (3-dim)
C  Input:    kGrd   = Ground level index                      (2-dim)
C            bi,bj  = tile index
C            myThid = Thread number for this instance of the routine

C !USES:
      IMPLICIT NONE

C     Resolution parameters
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"
#include "EEPARAMS.h"

C     Number of radiation bands with tau < 1
c     INTEGER NBAND
c     PARAMETER ( NBAND=4 )
C     Constants + functions of sigma and latitude
#include "com_physcon.h"
C     Radiation parameters
#include "com_radcon.h"

C !INPUT/OUTPUT PARAMETERS:
      INTEGER IMODE
      _RL TA(NGP,NLEV), TS(NGP), ST4S(NGP)
      _RL FTOP(NGP), FSFC(NGP), DFABS(NGP,NLEV)
      _RL OZUPP(NGP), STRATC(NGP)
      _RL TAU2(NGP,NLEV,NBAND), FLUX(NGP,NBAND), ST4A(NGP,NLEV,2)

      INTEGER kGrd(NGP)
      INTEGER bi,bj,myThid
CEOP

#ifdef ALLOW_AIM
C !LOCAL VARIABLES:
      INTEGER K, J, JB
c     INTEGER J0, Jl, I2
      INTEGER NL1(NGP)
      _RL REFSFC, BRAD, EMIS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO J=1,NGP
        NL1(J)=kGrd(J)-1
      ENDDO

      REFSFC=1.-EMISFC

      IF (IMODE.EQ.1) GO TO 410

C---  1. Blackbody emission from atmospheric full and half levels.
C        Temperature is interpolated as a linear function of ln sigma.
C        At the lower boundary, the emission is linearly extrapolated;
C        at the upper boundary, the atmosphere is assumed isothermal.

      DO K=1,NLEV
       DO J=1,NGP
         ST4A(J,K,1)=TA(J,K)*TA(J,K)
         ST4A(J,K,1)=SBC*ST4A(J,K,1)*ST4A(J,K,1)
       ENDDO
      ENDDO
C
      DO K=1,NLEV-1
       DO J=1,NGP
         ST4A(J,K,2)=TA(J,K)+WVI(K,2)*(TA(J,K+1)-TA(J,K))
         ST4A(J,K,2)=ST4A(J,K,2)*ST4A(J,K,2)
         ST4A(J,K,2)=SBC*ST4A(J,K,2)*ST4A(J,K,2)
       ENDDO
      ENDDO
C
      DO J=1,NGP
c       ST4A(J,NLEV,2)=ST4A(J,NLEV,1)
        K=kGrd(J)
        ST4A(J,K,2)=2.*ST4A(J,K,1)-ST4A(J,NL1(J),2)
      ENDDO

C---  2. Initialization
C---     (including the stratospheric correction term)

      DO J=1,NGP
        FTOP(J)   = 0.
        FSFC(J)   = STRATC(J)
        DFABS(J,1)=-STRATC(J)
      ENDDO

      DO K=2,NLEV
       DO J=1,NGP
         DFABS(J,K)=0.
       ENDDO
      ENDDO

C---  3. Emission ad absorption of longwave downward flux.
C        Downward emission is an average of the emission from the full level
C        and the half-level below, weighted according to the transmissivity
C        of the layer.

C     3.1  Stratosphere

      K=1
      DO JB=1,2
       DO J=1,NGP
         BRAD=ST4A(J,K,2)+TAU2(J,K,JB)*(ST4A(J,K,1)-ST4A(J,K,2))
         EMIS=FBAND(NINT(TA(J,K)),JB)*(1.-TAU2(J,K,JB))
         FLUX(J,JB)=EMIS*BRAD
         DFABS(J,K)=DFABS(J,K)-FLUX(J,JB)
       ENDDO
      ENDDO

      DO JB=3,NBAND
       DO J=1,NGP
         FLUX(J,JB)=0.
       ENDDO
      ENDDO

C     3.2  Troposphere

      DO JB=1,NBAND
       DO J=1,NGP
        DO K=2,kGrd(J)
          BRAD=ST4A(J,K,2)+TAU2(J,K,JB)*(ST4A(J,K,1)-ST4A(J,K,2))
          EMIS=FBAND(NINT(TA(J,K)),JB)*(1.-TAU2(J,K,JB))
          DFABS(J,K)=DFABS(J,K)+FLUX(J,JB)
          FLUX(J,JB)=TAU2(J,K,JB)*FLUX(J,JB)+EMIS*BRAD
          DFABS(J,K)=DFABS(J,K)-FLUX(J,JB)
        ENDDO
       ENDDO
      ENDDO

      DO JB=1,NBAND
       DO J=1,NGP
         FSFC(J)=FSFC(J)+EMISFC*FLUX(J,JB)
       ENDDO
      ENDDO

      IF (IMODE.EQ.-1) RETURN

C---  4. Emission ad absorption of longwave upward flux
C        Upward emission is an average of the emission from the full level
C        and the half-level above, weighted according to the transmissivity
C        of the layer (for the top layer, full-level emission is used).
C        Surface lw emission in "band 0" goes directly into FTOP.

C     4.1  Surface

      DO J=1,NGP
        ST4S(J)=TS(J)*TS(J)
        ST4S(J)=SBC*ST4S(J)*ST4S(J)
        ST4S(J)=EMISFC*ST4S(J)
      ENDDO

C     Entry point for upward-only mode (IMODE=1)
 410  CONTINUE

      DO J=1,NGP
        FSFC(J)=ST4S(J)-FSFC(J)
        FTOP(J)=FTOP(J)+FBAND(NINT(TS(J)),0)*ST4S(J)
      ENDDO

      DO JB=1,NBAND
       DO J=1,NGP
         FLUX(J,JB)=FBAND(NINT(TS(J)),JB)*ST4S(J)
     &              +REFSFC*FLUX(J,JB)
       ENDDO
      ENDDO

C     4.2  Troposphere

      DO JB=1,NBAND
       DO J=1,NGP
        DO K=kGrd(J),2,-1
          BRAD=ST4A(J,K-1,2)+TAU2(J,K,JB)*(ST4A(J,K,1)-ST4A(J,K-1,2))
          EMIS=FBAND(NINT(TA(J,K)),JB)*(1.-TAU2(J,K,JB))
          DFABS(J,K)=DFABS(J,K)+FLUX(J,JB)
          FLUX(J,JB)=TAU2(J,K,JB)*FLUX(J,JB)+EMIS*BRAD
          DFABS(J,K)=DFABS(J,K)-FLUX(J,JB)
        ENDDO
       ENDDO
      ENDDO

C     4.3  Stratosphere

      K=1
      DO JB=1,2
       DO J=1,NGP
         EMIS=FBAND(NINT(TA(J,K)),JB)*(1.-TAU2(J,K,JB))
         DFABS(J,K)=DFABS(J,K)+FLUX(J,JB)
         FLUX(J,JB)=TAU2(J,K,JB)*FLUX(J,JB)+EMIS*ST4A(J,K,1)
         DFABS(J,K)=DFABS(J,K)-FLUX(J,JB)
       ENDDO
      ENDDO

C     4.4  Outgoing longwave radiation

      DO JB=1,NBAND
       DO J=1,NGP
         FTOP(J)=FTOP(J)+FLUX(J,JB)
       ENDDO
      ENDDO

      DO J=1,NGP
        FTOP(J)=FTOP(J)+OZUPP(J)
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: RADSET
C !INTERFACE:
      SUBROUTINE RADSET( myThid )

C !DESCRIPTION: \bv
C   Purpose: compute energy fractions in LW bands
C            as a function of temperature
C   Initialized common blocks: RADFIX

C !USES:
      IMPLICIT NONE

C     Resolution parameters
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"
#include "EEPARAMS.h"

C     Radiation constants
#include "com_radcon.h"

C !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_AIM
C !LOCAL VARIABLES:
      INTEGER JTEMP, JB
      _RL EPS3

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      EPS3=0.95 _d 0

      DO JTEMP=200,320
        FBAND(JTEMP,0)= EPSLW
        FBAND(JTEMP,2)= 0.148 _d 0 - 3.0 _d -6 *(JTEMP-247)**2
        FBAND(JTEMP,3)=(0.375 _d 0 - 5.5 _d -6 *(JTEMP-282)**2)*EPS3
        FBAND(JTEMP,4)= 0.314 _d 0 + 1.0 _d -5 *(JTEMP-315)**2
        FBAND(JTEMP,1)= 1. _d 0 -(FBAND(JTEMP,0)+FBAND(JTEMP,2)
     &                           +FBAND(JTEMP,3)+FBAND(JTEMP,4))
      ENDDO

      DO JB=0,NBAND
        DO JTEMP=lwTemp1,199
          FBAND(JTEMP,JB)=FBAND(200,JB)
        ENDDO
        DO JTEMP=321,lwTemp2
          FBAND(JTEMP,JB)=FBAND(320,JB)
        ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
