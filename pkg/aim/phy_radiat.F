C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/phy_radiat.F,v 1.5 2001/09/06 13:19:54 adcroft Exp $
C $Name:  $

      SUBROUTINE SOL_OZ (SOLC,TYEAR,FSOL,OZONE,myThid)

C--
C--   SUBROUTINE SOL_OZ (SOLC,TYEAR,FSOL,OZONE)
C--
C--   Purpose: Compute the flux of incoming solar radiation
C--            and a climatological ozone profile for SW absorption
C--   Input:   SOLC   = solar constant (area averaged)
C--            TYEAR  = time as fraction of year (0-1, 0 = 1jan.h00)
C--   Output:  FSOL   = flux of incoming solar radiation        (2-dim)
C--            OZONE  = strat. ozone as fraction of global mean (2-dim)
C--


      IMPLICIT rEAL*8 (A-H,O-Z)
      INTEGER  myThid

#include "EEPARAMS.h"

C     Resolution parameters
C
#include "atparam.h"
#include "atparam1.h"
C
      INTEGER NLON, NLAT, NLEV, NGP
      INTEGER I, J, I2
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )
C
C     Constants + functions of sigma and latitude
C
#include "com_physcon.h"

      REAL FSOL(NLON,NLAT), OZONE(NLON,NLAT)
C
C     ALPHA = year phase ( 0 - 2pi, 0 = winter solstice = 22dec.h00 )
      ALPHA=4.*ASIN(1.)*(TYEAR+10./365.)

      CSR1=-0.796*COS(ALPHA)
      CSR2= 0.147*COS(2*ALPHA)-0.477
      COZ1= 0.0
C     COZ1= 0.2*SIN(ALPHA)
      COZ2= 0.3

C
      DO J=1,NLAT
       DO I=1,NLON
        I2=J
        I2=NLON*(J-1)+I
        FSOL(I,J)=
     &   SOLC*MAX(0.,1.0+CSR1*FMU(I2,1,myThid)+CSR2*FMU(I2,2,myThid))
        OZONE(I,J)=1.0+COZ1*FMU(I2,1,myThid)+COZ2*FMU(I2,2,myThid)
       ENDDO
      ENDDO
C     DO J=1,NLAT
C       FSOL(1,J)=
C    &   SOLC*MAX(0.,1.0+CSR1*FMU(J,1,myThid)+CSR2*FMU(J,2,myThid))
C       OZONE(1,J)=1.0+COZ1*FMU(J,1,myThid)+COZ2*FMU(J,2,myThid)
C       DO I=2,NLON
C         FSOL(I,J)=FSOL(1,J)
C         OZONE(I,J)=OZONE(1,J)
C       ENDDO
C     ENDDO
C
      RETURN
      END


      SUBROUTINE RADSW (PSA,QA,RH,
     *                  FSOL,OZONE,ALB,TAU,
     *                  CLOUDC,FTOP,FSFC,DFABS,
     I                  myThid)
C--
C--  SUBROUTINE RADSW (PSA,QA,RH,
C-- *                  FSOL,OZONE,ALB,
C-- *                  CLOUDC,FTOP,FSFC,DFABS)
C--
C--   Purpose: Compute the absorption of shortwave radiation and
C--            initialize arrays for longwave-radiation routines
C--   Input:   PSA    = norm. surface pressure [p/p0]           (2-dim)
C--            QA     = specific humidity [g/kg]                (3-dim)
C--            RH     = relative humidity                       (3-dim)
C--            FSOL   = flux of incoming solar radiation        (2-dim)
C--            OZONE  = strat. ozone as fraction of global mean (2-dim)
C--            ALB    = surface albedo                          (2-dim)
C--   Output:  CLOUDC = total cloud cover                           (2-dim)
C--            FTOP   = net downw. flux of sw rad. at the atm. top  (2-dim)
C--            FSFC   = net downw. flux of sw rad. at the surface   (2-dim)
C--            DFABS  = flux of sw rad. absorbed by each atm. layer (3-dim)
C--


      IMPLICIT rEAL*8 (A-H,O-Z)
      INTEGER  myThid


C     Resolution parameters
C
#include "atparam.h"
#include "atparam1.h"
#include "EEPARAMS.h"
#include "Lev_def.h"
C
      INTEGER NLON, NLAT, NLEV, NGP
      INTEGER K, J
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )
C
C     Constants + functions of sigma and latitude
C
#include "com_physcon.h"
C
C     Radiation parameters
C
#include "com_radcon.h"
C
      REAL PSA(NGP), QA(NGP,NLEV), RH(NGP,NLEV),
     *     FSOL(NGP), OZONE(NGP), ALB(NGP), TAU(NGP,NLEV)

      REAL CLOUDC(NGP), FTOP(NGP), FSFC(NGP), DFABS(NGP,NLEV)

      REAL FLUX(NGP), FREFL(NGP), TAUOZ(NGP)
      INTEGER NL1(NGP)
Cchdbg
      INTEGER Npas
      SAVE npas
      LOGICAL Ifirst
      SAVE Ifirst
      DATA Ifirst /.TRUE./
      REAL clsum(NGP)
      SAVE clsum
      REAL ABWLW1
cchdbg
C
      DO J=1,NGP
        NL1(J)=NLEVxy(J,myThid)-1
      ENDDO
C
C--   1.  Cloud cover:
C         defined as a linear fun. of the maximum relative humidity
C         in all tropospheric layers above PBL:
C         CLOUDC =  0 for RHmax < RHCL1, = 1 for RHmax > RHCL2.
C         This value is reduced by a factor (Qbase/QACL) if the 
C         cloud-base absolute humidity Qbase < QACL.
C
      DRHCL=RHCL2-RHCL1
      RCL=1./(DRHCL*QACL)
C
      DO 122 J=1,NGP
        CLOUDC(J)=0.
  122 CONTINUE
C
      DO 123 K=1,NLEV
      DO 123 J=1,NGP
        DFABS(J,K)=0.
  123 CONTINUE 

C
      DO 124 J=1,NGP
      DO 124 K=2,NL1(J)
        CLOUDC(J)=MAX(CLOUDC(J),(RH(J,K)-RHCL1))
  124 CONTINUE
C
      DO 126 J=1,NGP
        IF ( NL1(J) .GT. 0 ) THEN
         CLOUDC(J)=MIN(CLOUDC(J),DRHCL)*MIN(QA(J,NL1(J)),QACL)*RCL
        ENDIF
cchdbg *******************************************
cchdbg        CLOUDC(J)=MIN(CLOUDC(J),DRHCL)/DRHCL
cchdbg *******************************************
clear sky experiment
C       cloudc(j) = 0.
  126 CONTINUE
C
C
C--   2. Shortwave transmissivity:
C        function of layer mass, ozone (in the statosphere),
C        abs. humidity and cloud cover (in the troposphere)
C
      DO 202 J=1,NGP
        TAU(J,1)=EXP(-ABSSW*PSA(J)*DSIG(1))
        TAUOZ(J)=EXP(-EPSSW*OZONE(J)*PSA(J))
  202 CONTINUE
C
chhh      WRITE(0,*) ' Hello from RADSW'
      DO 204 J=1,NGP
      DO 204 K=2,NL1(J)
        TAU(J,K)=EXP(-(ABSSW+ABWSW*QA(J,K)
     *           +ABCSW*CLOUDC(J)*QA(J,NL1(J)))*PSA(J)*DSIG(K))
  204 CONTINUE

      DO 206 J=1,NGP
       IF ( NLEVxy(J,myThid) .GT. 0 ) THEN
        TAU(J,NLEVxy(J,myThid))=EXP(-(ABSSW+ABWSW*QA(J,NLEVxy(J,myThid)))
     *                              *PSA(J)*DSIG(NLEVxy(J,myThid)))
       ENDIF
  206 CONTINUE
C
C---  3. Shortwave downward flux 
C	
C     3.1  Absorption in the stratosphere
C
      DO 312 J=1,NGP
        FLUX(J)=TAU(J,1)*TAUOZ(J)*FSOL(J)
        DFABS(J,1)=FSOL(J)-FLUX(J)
  312 CONTINUE

C     RETURN

C	
C     3.2  Reflection at the top of the troposphere 
C          (proportional to cloud cover).
C
      DO 322 J=1,NGP
        FREFL(J)=ALBCL*CLOUDC(J)*FLUX(J)
        FTOP(J) =FSOL(J)-FREFL(J)
        FLUX(J) =FLUX(J)-FREFL(J)
  322 CONTINUE	
C	
C     3.3  Absorption in the troposphere
C
      DO 332 J=1,NGP
      DO 332 K=2,NLEVxy(J,myThid)
        DFABS(J,K)=FLUX(J)
        FLUX(J)=TAU(J,K)*FLUX(J)
        DFABS(J,K)=DFABS(J,K)-FLUX(J)
  332 CONTINUE

Cxx   RETURN

C
C---  4. Shortwave upward flux 
C	
C     4.1  Absorption and reflection at the surface
C
      DO 412 J=1,NGP
        FREFL(J)=ALB(J)*FLUX(J)
        FSFC(J) =FLUX(J)-FREFL(J)
        FLUX(J) =FREFL(J)
  412 CONTINUE
C	
C     4.2  Absorption in the atmosphere
C
      DO 422 J=1,NGP
      DO 422 K=NLEVxy(J,myThid),1,-1
        DFABS(J,K)=DFABS(J,K)+FLUX(J)
        FLUX(J)=TAU(J,K)*FLUX(J)
        DFABS(J,K)=DFABS(J,K)-FLUX(J)
  422 CONTINUE

Cxx   RETURN

C
C     4.3  Absorbed solar radiation = incoming - outgoing
C
      DO 432 J=1,NGP
        FTOP(J)=FTOP(J)-FLUX(J)
  432 CONTINUE

C     RETURN
cdj
c     write(0,*)'position j=20'
c     j=20
c     write(0,*)'ftop fsfc ftop-fsfc'
c     write(0,*)ftop(j),fsfc(j),ftop(j)-fsfc(j)
c     write(0,*)
c     write(0,*)'k dfabs'
c     do k = 1, nlevxy(j)
c       write(0,*)k,dfabs(j,k)
c     enddo
c     write(0,*)'sum dfabs'
c     write(0,*)sum(dfabs(j,:))
cdj
C
C---  5.  Initialization of longwave radiation model
C
C     5.1  Longwave transmissivity:
C          function of layer mass, abs. humidity and cloud cover.
C
      DO 512 J=1,NGP
        TAU(J,1)=EXP(-ABSLW*PSA(J)*DSIG(1))
  512 CONTINUE

C
      DO 514 J=1,NGP
      DO 514 K=2,NL1(J)
        TAU(J,K)=EXP(-(ABSLW+ABWLW*QA(J,K)
     *           +ABCLW*CLOUDC(J)*QA(J,NL1(J)))*PSA(J)*DSIG(K))
  514 CONTINUE

C     RETURN
C
cchdbg ***************************************************
c      ABCLW1=0.15
c      DO 514 J=1,NGP
c      DO 514 K=2,NL1(J)-1
c        TAU(J,K)=EXP(-(ABSLW+ABWLW*QA(J,K)
c     *           +ABCLW1*CLOUDC(J)*QA(J,NL1(J)))*PSA(J)*DSIG(K))
c  514 CONTINUE
C
c      DO 515 J=1,NGP
c      DO 515 K=NL1(J),NL1(J)
c        TAU(J,K)=EXP(-(ABSLW+ABWLW*QA(J,K)
c     *           +ABCLW*CLOUDC(J))*PSA(J)*DSIG(K))
c  515 CONTINUE
cchdbg ************************************************************
C
C *********************************************************************
C *********************************************************************
C *****************************************************************
cchdbg
c      if(Ifirst) then
c        npas=0
c        do J=1,NGP
c          clsum(J)=0.
c        enddo
c        ifirst=.FALSE.
c      ENDIF
C
c      npas=npas+1
c      DO J=1,NGP
c        clsum(J)=clsum(J)+ABCLW*CLOUDC(J)*QA(J,NL1(J))/5760.
c      ENDDO      
C
c      IF(npas.eq.5760) then
c        open(73,file='transmoy',form='unformatted')
c        write(73) clsum
c        close(73)
c      ENDIF
Cchdbg
C      
C *********************************************************************
C *********************************************************************

C     RETURN

      ABWLW1=0.7
      DO 516 J=1,NGP
       IF ( NLEVxy(J,myThid) .GT. 0 ) THEN
        TAU(J,NLEVxy(J,myThid))=EXP(-(ABSLW+ABWLW*QA(J,NLEVxy(J,myThid)))*PSA(J)
cchdbg        TAU(J,NLEVxy(J,myThid))=EXP(-(ABSLW+ABWLW1*QA(J,NLEVxy(J,myThid)))*PSA(J)
     *                     *DSIG(NLEVxy(J,myThid)))
       ENDIF
  516 CONTINUE

C
C---
      RETURN
      END


      SUBROUTINE RADLW (IMODE,TA,TS,ST4S,
     &                  TAU,ST4A,
     *                  FTOP,FSFC,DFABS,FDOWN, 
     I                  myThid)
C--
C--   SUBROUTINE RADLW (IMODE,TA,TS,ST4S,
C--  *                  FTOP,FSFC,DFABS)
C--
C--   Purpose: Compute the absorption of longwave radiation
C--   Input:   IMODE  = index for operation mode (see below)
C--            TA     = absolute temperature (3-dim)
C--            TS     = surface temperature (2-dim)        [if IMODE=1]
C--            ST4S   = surface blackbody emission (2-dim) [if IMODE=2]
C--   Output:  ST4S   = surface blackbody emission (2-dim) [if IMODE=1]
C--            FTOP   = outgoing flux of lw rad. at the atm. top    (2-dim)
C--            FSFC   = net upw. flux of lw rad. at the surface     (2-dim)
C--            DFABS  = flux of lw rad. absorbed by each atm. layer (3-dim)
C--            FDOWN  = downward flux of lw rad. at the surface     (2-dim)
C--


      IMPLICIT rEAL*8 (A-H,O-Z)
      INTEGER  myThid

C     Resolution parameters
C
#include "atparam.h"
#include "atparam1.h"
#include "EEPARAMS.h"
#include "Lev_def.h"
C
      INTEGER NLON, NLAT, NLEV, NGP
      INTEGER K, J
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )
C
C     Constants + functions of sigma and latitude
C
#include "com_physcon.h"
C
C     Radiation parameters
C
#include "com_radcon.h"
C
      REAL TA(NGP,NLEV), TS(NGP), ST4S(NGP), 
     &     TAU(NGP,NLEV), ST4A(NGP,NLEV,2)

      REAL FTOP(NGP), FSFC(NGP), DFABS(NGP,NLEV)
      REAL FDOWN(NGP)

      REAL FLUX(NGP), BRAD(NGP), STCOR(NGP)
      INTEGER NL1(NGP)
      INTEGER IMODE, J0, Jl, I2
C
Cchdbg
      INteger npas
      SAVE npas
      LOGICAL Ifirst
      SAVE IFIRST
      DATA Ifirst/.TRUE./
      REAL FluxMoy(NGP)
      REAL ST4SMoy(NGP)
      SAVE FluxMoy, ST4SMoy
Cchdbg

      DO J=1,NGP
        NL1(J)=NLEVxy(J,myThid)-1
      ENDDO

C
      DO K=1,NLEV
        DO J=1,NGP
          DFABS(J,K)=0.
        ENDDO
      ENDDO

C
C---  1. Blackbody emission from atmospheric full and half levels.
C        Temperature is interpolated as a linear function of ln sigma.
C        At the lower boundary, the emission is linearly extrapolated;
C        at the upper boundary, the atmosphere is assumed isothermal.
C
      DO 102 J=1,NGP
      DO 102 K=1,NLEVxy(J,myThid)
        ST4A(J,K,1)=TA(J,K)*TA(J,K)
        ST4A(J,K,1)=SBC*ST4A(J,K,1)*ST4A(J,K,1)
  102 CONTINUE
C
      DO 104 J=1,NGP
      DO 104 K=1,NL1(J)
        ST4A(J,K,2)=TA(J,K)+WVI(K,2)*(TA(J,K+1)-TA(J,K))
        ST4A(J,K,2)=ST4A(J,K,2)*ST4A(J,K,2)
        ST4A(J,K,2)=SBC*ST4A(J,K,2)*ST4A(J,K,2)
  104 CONTINUE

C
      DO 106 J=1,NGP
       IF ( NLEVxy(J,myThid) .GT. 0 ) THEN
        ST4A(J,NLEVxy(J,myThid),2)=2.*ST4A(J,NLEVxy(J,myThid),1)-ST4A(J,NL1(J),2)
       ENDIF
  106 CONTINUE
C
C---  2. Empirical stratospheric correction
C
      COR0= -13.
      COR1=   0.
      COR2=  24.
C
      J0=0
      DO JL=1,nlat
C       CORR=COR0+COR1*FMU(JL,1,myThid)+COR2*FMU(JL,2,myThid)
        DO J=J0+1,J0+NLON
          I2=JL
          I2=J
          STCOR(J)=COR0+COR1*FMU(I2,1,myThid)+COR2*FMU(I2,2,myThid)
C         STCOR(J)=CORR
        ENDDO
        J0=J0+NLON
      ENDDO
C
C---  3. Emission ad absorption of longwave downward flux.
C        Downward emission is an average of the emission from the full level
C        and the half-level below, weighted according to the transmissivity
C        of the layer.
C	
C     3.1  Stratosphere
C
      DO 312 J=1,NGP
        BRAD(J)=ST4A(J,1,2)+TAU(J,1)*(ST4A(J,1,1)-ST4A(J,1,2))
        FLUX(J)=(1.-TAU(J,1))*BRAD(J)
        DFABS(J,1)=STCOR(J)-FLUX(J)
  312 CONTINUE
C	
C     3.2  Troposphere
C
      DO 322 J=1,NGP
      DO 322 K=2,NLEVxy(J,myThid)
        DFABS(J,K)=FLUX(J)
        BRAD(J)=ST4A(J,K,2)+TAU(J,K)*(ST4A(J,K,1)-ST4A(J,K,2))
        FLUX(J)=TAU(J,K)*(FLUX(J)-BRAD(J))+BRAD(J)
        DFABS(J,K)=DFABS(J,K)-FLUX(J)
  322 CONTINUE
C
C---  4. Emission ad absorption of longwave upward flux 
C        Upward emission is an average of the emission from the full level
C        and the half-level above, weighted according to the transmissivity
C        of the layer (for the top layer, full-level emission is used).
C        Surface lw emission in the IR window goes directly into FTOP.
C	
C     4.1  Surface
C
      IF (IMODE.LE.1) THEN
        DO 412 J=1,NGP
          ST4S(J)=TS(J)*TS(J)
          ST4S(J)=SBC*ST4S(J)*ST4S(J)
  412   CONTINUE
      ENDIF
C
C **************************************************************
Cchdbg
      if(ifirst) then
        DO J=1,NGP
          ST4SMoy(J)=0.
          FluxMoy(J)=0.
        ENDDO 
        npas=0.
        ifirst=.FALSE.
      endif
C
      npas=npas+1
      DO 413 J=1,NGP
        ST4SMoy(J)=ST4SMoy(J)+ ST4S(J)
        FluxMoy(J)=FluxMoy(J)+ Flux(J)
  413 CONTINUE
C
      if(npas.eq.5760) then
        DO J=1,NGP
          ST4SMoy(J)=ST4SMoy(J)/float(npas)
          FluxMoy(J)=FluxMoy(J)/float(npas)
        ENDDO
        open(73,file='ST4Smoy',form='unformatted')
        write(73) ST4SMoy
        close(73)
        open(74,file='FluxMoy',form='unformatted')
        write(74) FluxMoy
        close(74)
      ENDIF
Cchdbg
C ****************************************************************
C
C
      DO 414 J=1,NGP
        FSFC(J)=ST4S(J)-FLUX(J)
        FDOWN(J)=FLUX(J)
        FTOP(J)=EPSLW*ST4S(J)
        FLUX(J)=ST4S(J)-FTOP(J)
  414 CONTINUE
C	
C     4.2  Troposphere
C
      DO 422 J=1,NGP
      DO 422 K=NLEVxy(J,myThid),2,-1
        DFABS(J,K)=DFABS(J,K)+FLUX(J)
        BRAD(J)=ST4A(J,K-1,2)+TAU(J,K)*(ST4A(J,K,1)-ST4A(J,K-1,2))
        FLUX(J)=TAU(J,K)*(FLUX(J)-BRAD(J))+BRAD(J)
        DFABS(J,K)=DFABS(J,K)-FLUX(J)
  422 CONTINUE
C	
C     4.3  Stratosphere
C
      DO 432 J=1,NGP
        DFABS(J,1)=DFABS(J,1)+FLUX(J)
        FLUX(J)=TAU(J,1)*(FLUX(J)-ST4A(J,1,1))+ST4A(J,1,1)
        DFABS(J,1)=DFABS(J,1)-FLUX(J)
  432 CONTINUE
C
C     4.4  Outgoing longwave radiation 
C
      DO 442 J=1,NGP
cdj     FTOP(J)=FTOP(J)+FLUX(J)
        FTOP(J)=FTOP(J)+FLUX(J)-STCOR(J)
  442 CONTINUE
cdj
c     write(0,*)'position j=20'
c     j=20
c     write(0,*)'ftop fsfc ftop-fsfc'
c     write(0,*)ftop(j),fsfc(j),ftop(j)-fsfc(j)
c     write(0,*)
c     write(0,*)'k dfabs'
c     do k = 1, nlevxy(j)
c       write(0,*)k,dfabs(j,k)
c     enddo
c     write(0,*)'sum dfabs'
c     write(0,*)sum(dfabs(j,:))
c     open(74,file='ftop0',form='unformatted',status='unknown')
c     write(74) ftop
c     open(75,file='stcor',form='unformatted',status='unknown')
c     write(75) stcor
c     stop
cdj
C
C---						
      RETURN
      END
