C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_convmf.F,v 1.1 2002/11/22 17:17:03 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE CONVMF (PSA,dpFac,SE,QA,QSAT,
     O                   IDEPTH,CBMF,PRECNV,DFSE,DFQA,
     I                   kGrd,bi,bj,myThid)
C--
C--   SUBROUTINE CONVMF (PSA,SE,QA,QSAT,
C--  *                   IDEPTH,CBMF,PRECNV,DFSE,DFQA)
C--
C--   Purpose: Compute convective fluxes of dry static energy and moisture
C--            using a simplified mass-flux scheme
C--   Input:   PSA    = norm. surface pressure [p/p0]            (2-dim)
C              dpFac  = cell delta_P fraction                    (3-dim)
C--            SE     = dry static energy                        (3-dim)
C--            QA     = specific humidity [g/kg]                 (3-dim)
C--            QSAT   = saturation spec. hum. [g/kg]             (3-dim)
C--   Output:  IDEPTH = convection depth in layers               (2-dim)
C--            CBMF   = cloud-base mass flux                     (2-dim)
C--            PRECNV = convective precipitation [g/(m^2 s)]     (2-dim)
C--            DFSE   = net flux of d.s.en. into each atm. layer (3-dim)
C--            DFQA   = net flux of sp.hum. into each atm. layer (3-dim)
C    Input:    kGrd   = Ground level index                       (2-dim)
C              bi,bj  = tile index
C              myThid = Thread number for this instance of the routine
C-------
C  Note: dry static energy has been replaced by Pot.Temp.
C-------

      IMPLICIT NONE

C     Resolution parameters

C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 

#include "EEPARAMS.h"

C     Physical constants + functions of sigma and latitude

#include "com_physcon.h"

C     Convection constants

#include "com_cnvcon.h"

C-- Routine arguments:
      _RL PSA(NGP), SE(NGP,NLEV), QA(NGP,NLEV), QSAT(NGP,NLEV)
      _RL dpFac(NGP,NLEV)
      INTEGER IDEPTH(NGP)
      _RL CBMF(NGP), PRECNV(NGP), DFSE(NGP,NLEV), DFQA(NGP,NLEV)
      INTEGER  kGrd(NGP)
      INTEGER  bi,bj,myThid

#ifdef ALLOW_AIM

C-- Local variables:
      INTEGER ITOP(NGP)
c_FM  REAL SM(NGP,NLEV), QATHR(NGP), ENTR(2:NLEV-1)
      _RL  QATHR(NGP), ENTR(2:NLEV-1) 
      _RL  ENTR_PS(NGP,2:NLEV-1), FM0(NGP) 

      INTEGER J, K, K1, Ktmp
      _RL  dSEdp(NGP,NLEV), factP, PSA_1
      _RL  dSEdpTot, stab_crit, FDMUS
C- jmc: declare all local variables:
      _RL  QMAX, DELQ, QB, QSATB, FMASS, ENMASS, SENTR 
      _RL  FPSA, FQMAX, RDPS, FUQ, FDQ, FSQ
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   1. Initialization of output and workspace arrays

      PSA_1 = 1.
      FQMAX=  5.

      RDPS = 2. _d 0 /(1. _d 0 - PSMIN)
c_FM  FM0=P0*DSIG(NLEV)/(GG*TRCNV*3600)
c_FM    FPSA=PSA(J)*MIN(1.,(PSA(J)-PSMIN)*RDPS)
C-    compute FM0(J) = FM0*FPSA
      DO J=1,NGP
       FM0(J)=0.
       Ktmp = kGrd(J)
       IF ( Ktmp .NE. 0 ) THEN
        FPSA = MIN(1. _d 0 ,(PSA(J)-PSMIN)*RDPS)
        FM0(J)=P0*DSIG(Ktmp)*dpFac(J,Ktmp)/(GG*TRCNV*3600. _d 0)
       ENDIF
      ENDDO

      DO K=1,NLEV
        DO J=1,NGP
          DFSE(J,K)=0.0
          DFQA(J,K)=0.0
        ENDDO
      ENDDO
      DO K=2,NLEV-1
        DO J=1,NGP
          ENTR_PS(J,K)=0.
        ENDDO
      ENDDO

      DO J=1,NGP
        ITOP(J)  =kGrd(J)
        CBMF(J)  =0.0
        PRECNV(J)=0.0
      ENDDO

C     Saturation moist static energy
c_FM  DO K=1,NLEV
c_FM    DO J=1,NGP
c_FM      SM(J,K)=SE(J,K)+ALHC*QSAT(J,K)
c_FM    ENDDO
c_FM  ENDDO

C ---------------------------------------------
C    Write Conditional stability based on Pot.Temp :
C    dSEdp(K) = Delta[Static-Energy] between 2 Plevels(k,k+1)
C    and corresponds to SE(K+1)-SE(K) in the original code
C -------
      DO K=1,NLEV-1
       factP = CP*SIGH(K)**(RD/CP)
       DO J=1,NGP
         dSEdp(J,K)=(SE(J,K+1)-SE(J,K))*factP
       ENDDO
      ENDDO

C     Entrainment profile (up to sigma = 0.5)

c_FM  SENTR=0.
c_FM  DO K=2,NLEV-1
c_FM    ENTR(K)=( MAX( 0. _d 0, SIG(K)-0.5 _d 0) )**2
c_FM    SENTR=SENTR+ENTR(K)
c_FM  ENDDO

c_FM  SENTR=ENTMAX/SENTR
c_FM  DO K=2,NLEV-1
c_FM    ENTR(K)=ENTR(K)*SENTR
c_FM  ENDDO 

      DO J=1,NGP
       DO K=2,NLEV-1
        ENTR_PS(J,K)=0.
       ENDDO
       Ktmp = kGrd(J)
       IF (Ktmp.GT.2) THEN
         SENTR=0.
         DO K=2,Ktmp-1
           ENTR(K)= ( MAX( 0. _d 0, SIG(K)/PSA(J) - 0.5 _d 0) )**2
           SENTR=SENTR+ENTR(K)
         ENDDO
         IF (SENTR.GT.0.) THEN
          SENTR=ENTMAX/SENTR
          DO K=2,Ktmp-1
           ENTR_PS(J,K) = ENTR(K)*SENTR*PSA(J)
          ENDDO
         ENDIF
       ENDIF
      ENDDO

C--   2. Check of conditions for convection

C     2.1 Conditional instability

c_FM  DO K=NLEV-2,2,-1
c_FM    DO J=1,NGP
c_FM      SMB=SM(J,K)+WVI(K,2)*(SM(J,K+1)-SM(J,K))
c_FM      IF (SM(J,NLEV).GT.SMB) ITOP(J)=K
c_FM    ENDDO
c_FM  ENDDO   

      DO J=1,NGP
       Ktmp = kGrd(J)
       IF ( Ktmp .GE. 2 ) THEN
        dSEdpTot = dSEdp(J,Ktmp-1)
        DO k=Ktmp-2,2,-1
          dSEdpTot = dSEdpTot + dSEdp(J,K)
          stab_crit = dSEdpTot + ALHC*(QSAT(J,Ktmp)-QSAT(J,K))
     &     -WVI(K,2)*(dSEdp(J,K) + ALHC*(QSAT(J,K+1)-QSAT(J,K)) )
          IF (stab_crit.GT.0.) ITOP(J) = K
        ENDDO
       ENDIF
      ENDDO


C     2.2 Humidity exceeding prescribed threshold

      DO J=1,NGP
       Ktmp = kGrd(J)
       IF ( Ktmp .NE. 0 ) THEN
        QATHR(J)=MIN(QBL,RHBL*QSAT(J,Ktmp))
        IF (QA(J,Ktmp).LT.QATHR(J).OR.PSA(J).LT.PSMIN)
     &      ITOP(J)=Ktmp
       ENDIF
        IDEPTH(J)=Ktmp-ITOP(J)
      ENDDO 

C--   3. Convection over selected grid-points

      DO 300 J=1,NGP
       Ktmp = kGrd(J)
      IF (ITOP(J).EQ.Ktmp) GO TO 300

C-      3.1 Boundary layer (cloud base)

        K = Ktmp
        K1=K-1

C       Maximum specific humidity in the PBL
        QMAX=MAX(1.01 _d 0 *QA(J,K),QSAT(J,K))

C       Dry static energy and moisture at upper boundary
c_FM    SB=SE(J,K1)+WVI(K1,2)*(SE(J,K)-SE(J,K1))
        QB=QA(J,K1)+WVI(K1,2)*(QA(J,K)-QA(J,K1))
        QB=MIN(QB,QA(J,K))

C       Cloud-base mass flux, computed to satisfy:
C       fmass*(qmax-qb)*(g/dp)=(q-qthr)/trcnv

c_FM    FPSA=PSA(J)*MIN(1.,(PSA(J)-PSMIN)*RDPS)
c_FM    FMASS=FM0*FPSA*MIN(FQMAX,(QA(J,K)-QATHR(J))/(QMAX-QB))
        FMASS = FM0(J)*MIN(FQMAX,(QA(J,K)-QATHR(J))/(QMAX-QB))
        CBMF(J)=FMASS

C       Upward fluxes at upper boundary
c_FM    FUS=FMASS*SE(J,K)
        FUQ=FMASS*QMAX

C       Downward fluxes at upper boundary
c_FM    FDS=FMASS*SB
        FDQ=FMASS*QB

C       Net flux of dry static energy and moisture
        FDMUS = FMASS*dSEdp(J,K1)*(WVI(K1,2)-1.)
        DFSE(J,K)=FDMUS
c_FM    DFSE(J,K)=FDS-FUS
        DFQA(J,K)=FDQ-FUQ

C-      3.2 Intermediate layers (entrainment)

        DO K=Ktmp-1,ITOP(J)+1,-1
        K1=K-1

C         Fluxes at lower boundary
c_FM      DFSE(J,K)=FUS-FDS
          DFQA(J,K)=FUQ-FDQ

C         Mass entrainment
c_FM      ENMASS=ENTR(K)*PSA(J)*CBMF(J)
          ENMASS=ENTR_PS(J,K) * CBMF(J)
          FMASS=FMASS+ENMASS

C         Upward fluxes at upper boundary
c_FM      FUS=FUS+ENMASS*SE(J,K)
          FUQ=FUQ+ENMASS*QA(J,K)

C         Downward fluxes at upper boundary
c_FM      SB=SE(J,K1)+WVI(K1,2)*(SE(J,K)-SE(J,K1))
          QB=QA(J,K1)+WVI(K1,2)*(QA(J,K)-QA(J,K1))
c_FM      FDS=FMASS*SB
          FDQ=FMASS*QB

C         Net flux of dry static energy and moisture
          DFSE(J,K) = FMASS*(WVI(K1,2)-1.)*dSEdp(J,K1)
     &             -(FMASS-ENMASS)*WVI(K,2)*dSEdp(J,K)
          FDMUS = FDMUS + DFSE(J,K)
c_FM      DFSE(J,K)=DFSE(J,K)+FDS-FUS
          DFQA(J,K)=DFQA(J,K)+FDQ-FUQ

C         Secondary moisture flux
          DELQ=RHIL*QSAT(J,K)-QA(J,K)
          IF (DELQ.GT.0.0) THEN
            FSQ=SMF*CBMF(J)*DELQ
            DFQA(J,K)   =DFQA(J,K)   +FSQ
            DFQA(J,Ktmp)=DFQA(J,Ktmp)-FSQ
          ENDIF

        ENDDO

C-      3.3 Top layer (condensation and detrainment)

        K=ITOP(J)

C       Flux of convective precipitation
        QSATB=QSAT(J,K)+WVI(K,2)*(QSAT(J,K+1)-QSAT(J,K))
        PRECNV(J)=MAX(FUQ-FMASS*QSATB, 0. _d 0)

C       Net flux of dry static energy and moisture
        DFSE(J,K)= -FDMUS+ALHC*PRECNV(J)
c_FM    DFSE(J,K)=FUS-FDS+ALHC*PRECNV(J)
        DFQA(J,K)=FUQ-FDQ-PRECNV(J)

 300  CONTINUE

#endif /* ALLOW_AIM */ 

      RETURN
      END
