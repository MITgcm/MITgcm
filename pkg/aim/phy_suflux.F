C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/phy_suflux.F,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Name:  $

      SUBROUTINE SUFLUX (PSA,UA,VA,TA,QA,RH,QSAT,PHI,
     *                   PHI0,FMASK,TLAND,TSEA,SWAV,SSR,SLR,
     *                   USTR,VSTR,SHF,EVAP,T0,Q0,QSAT0,SPEED0)
C--
C--   SUBROUTINE SUFLUX (PSA,UA,VA,TA,QA,RH,PHI,
C--  *                   PHI0,FMASK,TLAND,TSEA,SWAV,
C--  *                   USTR,VSTR,SHF,EVAP)
C--
C--   Purpose: Compute surface fluxes of momentum, energy and moisture
C--   Input:   PSA    = norm. surface pressure [p/p0] (2-dim)
C--            UA     = u-wind                        (3-dim)
C--            VA     = v-wind                        (3-dim)
C--            TA     = temperature                   (3-dim)
C--            QA     = specific humidity [g/kg]      (3-dim)
C--            RH     = relative humidity             (3-dim)
C--            PHI    = geopotential                  (3-dim)
C--            PHI0   = surface geopotential          (2-dim)
C--            FMASK  = fractional land-sea mask         (2-dim)
C--            TLAND  = land-surface temperature         (2-dim)
C--            TSEA   =  sea-surface temperature         (2-dim)
C--            SWET   = soil wetness availability [0-1]  (2-dim)
C--   Output:  USTR   = u stress                         (2-dim)
C--            VSTR   = v stress                         (2-dim)
C--            SHF    = sensible heat flux               (2-dim)
C--            EVAP   = evaporation [g/(m^2 s)]          (2-dim)
C--


      IMPLICIT rEAL*8 (A-H,O-Z)


C     Resolution parameters
#include "atparam.h"
#include "atparam1.h"
#include "Lev_def.h"
C
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )

C     Physical constants + functions of sigma and latitude

#include "com_physcon.h"

C     Surface flux constants

#include "com_sflcon.h"
C
      REAL PSA(NGP), UA(NGP,NLEV), VA(NGP,NLEV), TA(NGP,NLEV),
     *     QA(NGP,NLEV), RH(NGP,NLEV), QSAT(NGP,NLEV), PHI(NGP,NLEV),
     *     PHI0(NGP), FMASK(NGP), TLAND(NGP), TSEA(NGP), SWAV(NGP),
     *     SSR(NGP), SLR(NGP)
C
      REAL USTR(NGP,3), VSTR(NGP,3), SHF(NGP,3), EVAP(NGP,3)
C									
      REAL U0(NGP),V0(NGP),T0(NGP,2),Q0(NGP),QSAT0(NGP,2),DENVV(NGP,3)
      REAL SPEED0(NGP)
      REAL WORK(NGP)
      INTEGER NL1(NGP)
      REAL AUX(NGP)
C
      REAL    pSurfs(NLEV)
      DATA    pSurfs /75 _d 2,250 _d 2,500 _d 2,775 _d 2,950 _d 2 /
C
      REAL    pSurfw(NLEV)
      DATA    pSurfw /150 _d 2,350 _d 2,650 _d 2,900 _d 2,1000 _d 2/
Cchdbg
      INTEGER NPAS
      SAVE NPAS
      LOGICAL Ifirst
      SAVE Ifirst
      DATA Ifirst /.TRUE./
      REAL T0moy1(NGP)
      REAL T0moy2(NGP)
      SAVE T0moy2, T0moy1
      REAL denmoy1(NGP)
      REAL denmoy2(NGP)
      SAVE denmoy1, denmoy2
      REAL Q0moy(NGP)
      SAVE Q0moy
C
C--   1. Extrapolation of wind, temp, hum. and density to the surface
C
C     1.1 Wind components
C   
      DO J=1,NGP
        IF ( NLEVxyU(J) .GT. 0 ) THEN
         U0(J) = FWIND0*UA(J,NLEVxyU(J))
        ENDIF
        IF ( NLEVxyV(J) .GT. 0 ) THEN
         V0(J) = FWIND0*VA(J,NLEVxyV(J))
        ENDIF
C       U0(J) = UA(J,5)
C       V0(J) = VA(J,5)
      ENDDO
C
C     1.2 Temperature
C
      GTEMP0 = 1.D0-FTEMP0
      RCP = 1.D0/CP
      DO J=1,NGP
        NL1(J)=NLEVxy(J)-1
      ENDDO
C
      DO J=1,NGP
       IF ( NLEVxy(J) .GT. 0 ) THEN
        T0(J,1) = TA(J,NLEVxy(J))+WVI(NLEVxy(J),2)*
     &                          (TA(J,NLEVxy(J))-TA(J,NL1(J)))
ccccc        T0(J,2) = TA(J,NLEVxy(J))+RCP*(PHI(J,NLEVxy(J))-PHI0(J))
        T0(J,2) = TA(J,NLEVxy(J))*
     &         ((Psurfw(NLEVxy(J))/Psurfs(Nlevxy(J)))**(RD/CP))
       ELSE
        T0(J,1) = 273.16 _d 0
        T0(J,2) = 273.16 _d 0
       ENDIF
      ENDDO
C
      DO J=1,NGP
        T0(J,1) = FTEMP0*T0(J,1)+GTEMP0*T0(J,2)
      ENDDO
C
C     1.3 Spec. humidity
C
      GHUM0 = 1. _d 0-FHUM0
C
      DO J=1,NGP
       IF ( NLEVxy(J) .GT. 0 ) THEN
        WORK(J)=RH(J,Nlevxy(J))
cchdbg
c        WORK(J) = RH(J,NLEVxy(J))+WVI(NLEVxy(J),2)*
c     &                          (RH(J,NLEVxy(J))-RH(J,NL1(J)))
cchdbg
       ENDIF
      ENDDO
C
      CALL SHTORH (-1,NGP,T0,PSA,1. _d 0,Q0,WORK,QSAT0)
C
      DO J=1,NGP
       IF ( NLEVxy(J) .GT. 0 ) THEN
        Q0(J)=FHUM0*Q0(J)+GHUM0*QA(J,NLEVxy(J))
       ENDIF
      ENDDO

C     1.4 Density * wind speed (including gustiness factor)

      PRD = P0/RD
      VG2 = VGUST*VGUST
C
      DO J=1,NGP
        DENVV(J,3)=(PRD*PSA(J)/T0(J,1))*
     *           SQRT(U0(J)*U0(J)+V0(J)*V0(J)+VG2)
cchdbg        DENVV(J,3)=0.7*(PRD*PSA(J)/T0(J,1))*
cchdbg     *           SQRT(U0(J)*U0(J)+V0(J)*V0(J)+VG2)
        SPEED0(J)=SQRT(U0(J)*U0(J)+V0(J)*V0(J)+VG2)
      ENDDO
C
C     1.5 Stability correction for heat/moisture fluxes
C
      DTHETAF = 3. _d 0
      FSTAB = 0.67 _d 0
      RDTH = FSTAB/DTHETAF
C
      DO J=1,NGP
        FSLAND=1.+MIN(DTHETAF,MAX(-DTHETAF,TLAND(J)-T0(J,2)))*RDTH
        FSSEA =1.+MIN(DTHETAF,MAX(-DTHETAF, TSEA(J)-T0(J,2)))*RDTH
        aux(J)=FSLAND
        DENVV(J,1)=DENVV(J,3)*FSLAND
        DENVV(J,2)=DENVV(J,3)*FSSEA
cchdbg        DENVV(J,1)=DENVV(J,3)
cchdbg        DENVV(J,2)=DENVV(J,3)
      ENDDO
C
C--   2. Computation of fluxes over land and sea
C
C     2.1 Wind stress
C
      DO J=1,NGP
       IF ( NLEVxyu(J) .GT. 0  ) THEN
        USTR(J,1) = -CDL*DENVV(J,3)*UA(J,NLEVxyu(J))
        USTR(J,2) = -CDS*DENVV(J,3)*UA(J,NLEVxyu(J))
       ENDIF
       IF ( NLEVxyv(J) .GT. 0  ) THEN
        VSTR(J,1) = -CDL*DENVV(J,3)*VA(J,NLEVxyv(J))
        VSTR(J,2) = -CDS*DENVV(J,3)*VA(J,NLEVxyv(j))
       ENDIF
      ENDDO
C
C     2.2 Sensible heat flux (from clim. TS over land)
C
      CHLCP = CHL*CP
      CHSCP = CHS*CP
C
      DO J=1,NGP
        SHF(J,1) = CHLCP*DENVV(J,1)*(TLAND(J)-T0(J,1))
        SHF(J,2) = CHSCP*DENVV(J,2)*(TSEA(J) -T0(J,1))
      ENDDO
C
C ****************************************************
cchdbg
      IF (Ifirst) then 
        npas=0.
        do J=1,NGP
          T0moy1(J)=0. 
          T0moy2(J)=0. 
          denmoy1(J)=0. 
          denmoy2(J)=0. 
          Q0moy(J)=0.
        enddo
        ifirst=.false.
      ENDIF
C
      npas=npas+1
      DO J=1,NGP
        T0moy1(J)=T0moy1(J) + T0(J,1)
        T0moy2(J)=T0moy2(J) + T0(J,2)
        denmoy1(J)=denmoy1(J) + DENVV(J,1)
        denmoy2(J)=denmoy2(J) + DENVV(J,2)
        Q0moy(J)=Q0moy(J) + Q0(J)
      ENDDO
C
      if(npas.eq.5760) then
        DO J=1,NGP
          T0moy1(J)=T0moy1(J)/float(npas)
          T0moy2(J)=T0moy2(J)/float(npas)
          denmoy1(J)=denmoy1(J)/float(npas)
          denmoy2(J)=denmoy2(J)/float(npas)
          Q0moy(J)=Q0moy(J)/float(npas)
        ENDDO
C
        open(73,file='Tmoy1',form='unformatted')
        write(73) T0moy1
        close(73)
        open(74,file='Tmoy2',form='unformatted')
        write(74) T0moy2
        close(74)
        open(73,file='denmoy1',form='unformatted')
        write(73) denmoy1
        close(73)
        open(74,file='denmoy2',form='unformatted')
        write(74) denmoy2
        close(74)
        open(74,file='Q0moy',form='unformatted')
        write(74) Q0moy
        close(74)
      ENDIF
C
cchdbg
C ****************************************************
C
c        CALL DUMP_WRITE2D ( NGP,1,'T0.', nIter,T0 , iErr)
c        CALL DUMP_WRITE2D ( NGP,3,'DENVV.', nIter,DENVV , iErr)
c        CALL DUMP_WRITE2D ( NGP,1,'TSEA.', nIter,TSEA , iErr)
c        CALL DUMP_WRITE2D ( NGP,1,'TLAND.', nIter,TLAND , iErr)
c        CALL DUMP_WRITE2D ( NGP,1,'AUX.', nIter,aux , iErr)
C
C     2.3 Evaporation
C
      CALL SHTORH (0,NGP,TLAND,PSA,1. _d 0,QDUMMY,RDUMMY,QSAT0(1,1))
      CALL SHTORH (0,NGP,TSEA ,PSA,1. _d 0,QDUMMY,RDUMMY,QSAT0(1,2))

      DO J=1,NGP
Cdj        EVAP(J,1) = CHL*DENVV(J,1)*MAX(0. _d 0,SWAV(J)*QSAT0(J,1)-Q0(J))
        EVAP(J,1)=CHL*DENVV(J,1)*MIN(1. _d 0,SWAV(J))*(QSAT0(J,1)-Q0(J))
cdj try new scheme : assume that the net heat flux on land is zero
cdj                  at all time and at all points (this is equivalent
cdj                  to say that the land has a zero heat capacity)
cdj        EVAP(J,1) = SSR(J)-SLR(J)-SHF(J,1)
        EVAP(J,2) = CHS*DENVV(J,2)*(QSAT0(J,2)-Q0(J))
      ENDDO


C--   3. Weighted average of fluxes according to land-sea mask

      DO J=1,NGP
        USTR(J,3) = USTR(J,2)+FMASK(J)*(USTR(J,1)-USTR(J,2))
        VSTR(J,3) = VSTR(J,2)+FMASK(J)*(VSTR(J,1)-VSTR(J,2))
         SHF(J,3) =  SHF(J,2)+FMASK(J)*( SHF(J,1)- SHF(J,2))
        EVAP(J,3) = EVAP(J,2)+FMASK(J)*(EVAP(J,1)-EVAP(J,2))
cdj
        QSAT0(J,1) = QSAT0(J,2)+FMASK(J)*(QSAT0(J,1)-QSAT0(J,2))
cdj
      ENDDO

C--
      RETURN
      END
