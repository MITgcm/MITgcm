C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_coare3_flux.F,v 1.11 2012/12/28 23:25:50 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

C--   File cheapaml_coare3_flux.F:
C--    Contents:
C--    o CHEAPAML_COARE3_FLUX
C--    o PSIU (Function)
C--    o PSIT (Function)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: CHEAPAML_COARE3_FLUX
C     !INTERFACE:
      SUBROUTINE CHEAPAML_COARE3_FLUX(
     I                    i,j,bi,bj,
     O                    hf, ef, evap, Rnl, ssqt, q100, cdq )

C     !DESCRIPTION:

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CHEAPAML.h"
#include "DYNVARS.h"

C     !INPUT PARAMETERS:
      INTEGER i,j,bi,bj
C     !OUTPUT PARAMETERS:
      _RL hf, ef, evap, Rnl, ssqt, q100, cdq
CEOP

C     !LOCAL VARIABLES:
      INTEGER iter,nits
      _RL tau,L,psu,pst,Bf
      _RL CD,usr,tsr,qsr,ttas,essqt
      _RL zo,zot,zoq,RR,zL,pt,ttt,tta,ttt2
      _RL es,twoPI,cwave,lwave

C various constants
      _RL u,ts,q,zi,qs,tsw
      _RL psiu,psit,zot10,Ct10,CC,Ribu
      _RL Du,Wg,Dt,Dq,u10,zo10,Cd10,Ch10
      _RL xBeta,visa,Ribcu,QaR
      _RL Ct,zetu,L10,Tas,ta,charn

C Constants and coefficients (Stull 1988 p640).
      xBeta = 1.2 _d 0    !Given as 1.25 in Fairall et al.(1996)
      twoPI = 2. _d 0*PI
      visa = 1.326 _d -5
C default relative humidity
      QaR = 0.8 _d 0

C sea surface temperature without skin correction
      ts=theta(i,j,1,bi,bj)
      tsw=ts
      Tas=Tair(i,j,bi,bj)

C net upward long wave
      Rnl = 0.96 _d 0*(stefan*(tsw+celsius2K)**4) !Net longwave (up = +).

C Teten''s return s air svp es in mb
      es = (1.0007 _d 0 + 3.46 _d -6*p0)*6.1121 _d 0
     &     *EXP( 17.502 _d 0*tsw/(240.97 _d 0+tsw) )
      es = es*0.98 _d 0             !reduced for salinity Kraus 1972 p. 46
C     convert from mb to spec. humidity  kg/kg
      qs = 0.62197 _d 0*es/(p0 -0.378 _d 0*es)
      tta = Tas+celsius2K
      ttas=tta+gamma_blk*zt
      ttt=tta-(cheaphgrid(i,j,bi,bj) - zt)*gamma_blk
      ttt2=tta-(cheaphgrid(i,j,bi,bj) - zt)*gamma_blk-celsius2K
      pt = p0*(1.-gamma_blk*cheaphgrid(i,j,bi,bj)/ttas)
     &     **(gravity/gamma_blk/gasR)
      essqt = (1.0007 _d 0 + 3.46 _d -6*pt)*6.1121 _d 0
     &        *EXP( 17.502 _d 0*ttt2/(240.97 _d 0+ttt2) )
C     convert from mb to spec. humidity  kg/kg
      ssqt = 0.62197 _d 0*essqt/(pt -0.378 _d 0*essqt)
C      LANL formulation
C     saturation no more at the top:
      ssqt=ssq0*EXP( lath*(ssq1-ssq2/tta) ) / p0

      IF (useFreshWaterFlux) THEN
        q=qair(i,j,bi,bj)
      ELSE
        q=QaR*ssqt
      ENDIF

C Wave parameters
      cwave=gravity*wavesp(i,j,bi,bj)/twoPI
      lwave=cwave*wavesp(i,j,bi,bj)

C Initial guesses
      zo = 0.0001 _d 0
      Wg = 0.5 _d 0                 !Gustiness factor initial guess

C Air-sea differences - includes warm layer in Dt and Dq
      u = (uwind(i,j,bi,bj)-uVel(i,j,1,bi,bj))**2
     &  + (vwind(i,j,bi,bj)-vVel(i,j,1,bi,bj))**2
      u = SQRT(u)
      Du= (u**2 + Wg**2 )**.5  !include gustiness in wind spd. difference
      Dt=tsw-Tas-gamma_blk*zt  !potential temperature difference.
      Dq=qs-q

C **************** neutral coefficients ******************

      u10 = Du*LOG(10. _d 0/zo)/LOG(zu/zo)
      usr = 0.035 _d 0*u10
      zo10= 0.011 _d 0*usr*usr/gravity+0.11 _d 0*visa/usr
      Cd10= (xkar/LOG(10. _d 0/zo10))**2
      Ch10= 0.00115 _d 0
      Ct10= Ch10/SQRT(Cd10)
      zot10=10. _d 0/EXP(xkar/Ct10)
      Cd = (xkar/LOG(zu/zo10))**2

C standard coare3 boundary layer height
      zi=600. _d 0

C ************* Grachev and Fairall (JAM, 1997) **********

      ta=Tas+celsius2K
      Ct=xkar/LOG(zt/zot10)   ! Temperature transfer coefficient
      CC=xkar*Ct/Cd            ! z/L vs Rib linear coefficient
      Ribcu=-zu/(zi*0.004 _d 0*xBeta**3)  ! Saturation or plateau Rib
      Ribu=-gravity*zu*(Dt+0.61 _d 0*ta*Dq)/(ta*Du**2)
      IF (Ribu.LT.0. _d 0) THEN
          zetu=CC*Ribu/(1. _d 0+Ribu/Ribcu)   ! Unstable G and F
      ELSE
          zetu=CC*Ribu*(1. _d 0 +27. _d 0/9. _d 0*Ribu/CC) ! Stable
      ENDIF
      L10=zu/zetu                       ! MO length
      IF (zetu.GT.50. _d 0) THEN
        nits=1
      ELSE
        nits=3   ! number of iterations
      ENDIF

C First guess M-O stability dependent
C scaling params.(u*,t*,q*) to estimate zo and z/L

      usr= Du*xkar/(LOG(zu/zo10)-psiu(zu/L10))
      tsr=-(Dt)*xkar/(LOG(zt/zot10)-psit(zt/L10))
      qsr=-(Dq)*xkar/(LOG(zq/zot10)-psit(zq/L10))

      charn=0.011 _d 0     !then modify Charnock for high wind speeds Chris data
      IF (Du.GT.10. _d 0) charn=0.011 _d 0
     &                    + (0.018 _d 0-0.011 _d 0)*(Du-10.)/(18.-10.)
      IF (Du.GT.18. _d 0) charn=0.018 _d 0

C **** Iterate across u*(t*,q*),zo(zot,zoq) and z/L including cool skin ****

      DO iter=1,nits
       IF (WAVEMODEL.EQ.'Smith') THEN
        zo=charn*usr*usr/gravity + 0.11 _d 0*visa/usr    !after Smith 1988
       ELSEIF (WAVEMODEL.EQ.'Oost') THEN
        zo=(50./twoPI)*lwave*(usr/cwave)**4.5 _d 0
     &    + 0.11 _d 0*visa/usr !Oost et al.
       ELSEIF (WAVEMODEL.EQ.'TayYel') THEN
        zo=1200. _d 0*wavesh(i,j,bi,bj)*(wavesh(i,j,bi,bj)/lwave)**4.5
     &    + 0.11 _d 0*visa/usr !Taylor and Yelland
       ENDIF
       rr=zo*usr/visa

C *** zoq and zot fitted to results from several ETL cruises ************

       zoq = MIN( 1.15 _d -4, 5.5 _d -5/rr**0.6 _d 0 )
       zot = zoq

       zL=xkar*gravity*zu*( tsr*(1.+0.61 _d 0*q)+0.61 _d 0*ta*qsr )
     &                   /( ta*usr*usr*(1. _d 0+0.61 _d 0*q) )
       L=zu/zL
       psu=psiu(zu/L)
       pst=psit(zt/L)
       usr=Du*xkar/(LOG(zu/zo)-psiu(zu/L))
       tsr=-(Dt)*xkar/(LOG(zt/zot)-psit(zt/L))
       qsr=-(Dq)*xkar/(LOG(zq/zoq)-psit(zq/L))
       Bf=-gravity/ta*usr*(tsr+0.61 _d 0*ta*qsr)
       IF (Bf.GT.0. _d 0) THEN
          Wg=xBeta*(Bf*zi)**.333 _d 0
       ELSE
          Wg=0.2 _d 0
       ENDIF
       Du=SQRT(u**2 + Wg**2)        !include gustiness in wind spd.
      ENDDO

C compute surface fluxes and other parameters
      tau=rhoa*usr*usr*u/Du          !stress N/m2
      hf=-cpair*rhoa*usr*tsr           !sensible W/m2
      ef=-lath*rhoa*usr*qsr           !latent W/m2
      evap=-rhoa*usr*qsr
      cdq = evap/Dq
      IF (.NOT.useStressOption) THEN
       ustress(i,j,bi,bj)=tau*(uwind(i,j,bi,bj)-uVel(i,j,1,bi,bj))/u
       vstress(i,j,bi,bj)=tau*(vwind(i,j,bi,bj)-vVel(i,j,1,bi,bj))/u
      ENDIF
      q100=qs+qsr*(LOG(100. _d 0/zoq)-psit(100. _d 0/L))

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: PSIU

C     !INTERFACE:
      _RL FUNCTION psiu(zL)

C     !DESCRIPTION:
C psiu and psit evaluate stability function for wind speed and scalars
C matching Kansas and free convection forms with weighting f
C convective form follows Fairall et al (1996) with profile constants
C from Grachev et al (2000) BLM
C stable form from Beljaars and Holtslag (1991)

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      _RL zL
C     !LOCAL VARIABLES:
      _RL x,y,psik,psic,f,c
CEOP

      IF (zL.LT.0.0) THEN
       x = (1. - 15.*zL)**.25                   !Kansas unstable
       psik=2.*LOG((1.+x)/2.)+LOG((1.+x*x)/2.)-2.*ATAN(x)+2.*ATAN(oneRL)
       y = (1. - 10.15 _d 0*zL)**.3333 _d 0     !Convective
       psic = 1.5*LOG((1.+y+y*y)/3.)
     &      - SQRT(3. _d 0)*ATAN( (1.+2.*y)/SQRT(3. _d 0) )
     &      + 4.*ATAN(oneRL)/SQRT(3. _d 0)
       f = zL*zL/(1.+zL*zL)
       psiu = (1.-f)*psik+f*psic
      ELSE
       c = MIN( 50. _d 0, 0.35 _d 0*zL )        !Stable
c      psiu=-((1.+1.*zL)**1.+.6667*(zL-14.28)/EXP(c)+8.525)
       psiu = -( (1.+zL) + 0.6667 _d 0*(zL-14.28 _d 0)/EXP(c)
     &          + 8.525 _d 0 )
      ENDIF
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: PSIT

C     !INTERFACE:
      _RL FUNCTION psit(zL)

C     !DESCRIPTION:

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      _RL zL
C     !LOCAL VARIABLES:
      _RL x,y,psik,psic,f,c
CEOP

      IF (zL.LT.0.0) THEN
       x = (1. - 15.*zL)**.5                    !Kansas unstable
       psik = 2.*LOG((1.+x)/2.)
       y = (1. - 34.15 _d 0*zL)**.3333 _d 0     !Convective
       psic = 1.5*LOG((1.+y+y*y)/3.)
     &      - SQRT(3. _d 0)*ATAN( (1.+2.*y)/SQRT(3. _d 0) )
     &      + 4.*ATAN(oneRL)/SQRT(3. _d 0)
       f = zL*zL/(1.+zL*zL)
       psit = (1.-f)*psik+f*psic
      ELSE
       c = MIN( 50. _d 0, 0.35 _d 0*zL )        !Stable
       psit = -( (1.+2.*zL/3.)**1.5
     &          + 0.6667 _d 0*(zL-14.28 _d 0)/EXP(c)
     &          + 8.525 _d 0 )
      ENDIF

      RETURN
      END
