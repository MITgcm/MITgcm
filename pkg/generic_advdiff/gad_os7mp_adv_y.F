C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_os7mp_adv_y.F,v 1.8 2008/06/16 13:40:25 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_OS7MP_ADV_Y(
     I           bi,bj,k, calcCFL, deltaTloc,
     I           vTrans, vFld,
     I           maskLocS, Q,
     O           vT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_OS7MP_ADV_Y                               |
C     | o Compute Meridional advective Flux of tracer Q using    |
C     |   7th Order DST Sceheme with monotone preserving limiter |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"
#include "GAD.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      LOGICAL calcCFL
      _RL deltaTloc
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Q     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL cfl,Psi
      _RL vLoc,Fac,DelIp,DelI,Phi,Eps,rp1h,rp1h_cfl
      _RL recip_DelIp, recip_DelI
      _RL Qippp,Qipp,Qip,Qi,Qim,Qimm,Qimmm
      _RL MskIpp,MskIp,MskI,MskIm,MskImm,MskImmm
      _RL d2,d2p1,d2m1,A,B,C,D
      _RL dp1h,dm1h, PhiMD,PhiLC,PhiMin,PhiMax
      _RL DelM,DelP,DelMM,DelPP,DelMMM,DelPPP
      _RL Del2MM,Del2M,Del2,Del2P,Del2PP
      _RL Del3MM,Del3M,Del3P,Del3PP
      _RL Del4M,Del4,Del4P
      _RL Del5M,Del5P
      _RL Del6

      Eps = 1. _d -20

      DO i=1-Olx,sNx+Olx
       vT(i,1-Oly)=0. _d 0
       vT(i,2-Oly)=0. _d 0
       vT(i,3-Oly)=0. _d 0
       vT(i,4-Oly)=0. _d 0
       vT(i,sNy+Oly-2)=0. _d 0
       vT(i,sNy+Oly-1)=0. _d 0
       vT(i,sNy+Oly)=0. _d 0
      ENDDO
      DO j=1-Oly+4,sNy+Oly-3
       DO i=1-Olx,sNx+Olx

        vLoc = vFld(i,j)
        cfl = vLoc
        IF ( calcCFL ) cfl = abs(vLoc*deltaTloc*recip_dyC(i,j,bi,bj))

        IF (vTrans(i,j).GT.0. _d 0) THEN
         Qippp = Q(i,j+2)
         Qipp  = Q(i,j+1)
         Qip   = Q(i,j)
         Qi    = Q(i,j-1)
         Qim   = Q(i,j-2)
         Qimm  = Q(i,j-3)
         Qimmm = Q(i,j-4)

         MskIpp  = maskLocS(i,j+2)
         MskIp   = maskLocS(i,j+1)
         MskI    = maskLocS(i,j)
         MskIm   = maskLocS(i,j-1)
         MskImm  = maskLocS(i,j-2)
         MskImmm = maskLocS(i,j-3)
        ELSEIF (vTrans(i,j).LT.0. _d 0) THEN
         Qippp = Q(i,j-3)
         Qipp  = Q(i,j-2)
         Qip   = Q(i,j-1)
         Qi    = Q(i,j)
         Qim   = Q(i,j+1)
         Qimm  = Q(i,j+2)
         Qimmm = Q(i,j+3)

         MskIpp  = maskLocS(i,j-2)
         MskIp   = maskLocS(i,j-1)
         MskI    = maskLocS(i,j)
         MskIm   = maskLocS(i,j+1)
         MskImm  = maskLocS(i,j+2)
         MskImmm = maskLocS(i,j+3)
        ELSE
         Qippp = 0. _d 0
         Qipp  = 0. _d 0
         Qip   = 0. _d 0
         Qi    = 0. _d 0
         Qim   = 0. _d 0
         Qimm  = 0. _d 0
         Qimmm = 0. _d 0

         MskIpp  = 0. _d 0
         MskIp   = 0. _d 0
         MskI    = 0. _d 0
         MskIm   = 0. _d 0
         MskImm  = 0. _d 0
         MskImmm = 0. _d 0
        ENDIF

        IF (vTrans(i,j).NE.0. _d 0) THEN
C        2nd order correction [i i-1]
         Fac = 1. _d 0
         DelP = (Qip-Qi)*MskI
         Phi = Fac * DelP
C        3rd order correction [i i-1 i-2]
         Fac = Fac * ( cfl + 1. _d 0 )/3. _d 0
         DelM = (Qi-Qim)*MskIm
         Del2 = DelP - DelM
         Phi = Phi - Fac * Del2
C        4th order correction [i+1 i i-1 i-2]
         Fac = Fac * ( cfl - 2. _d 0 )/4. _d 0
         DelPP = (Qipp-Qip)*MskIp*MskI
         Del2P = DelPP - DelP
         Del3P = Del2P - Del2
         Phi = Phi + Fac * Del3p
C        5th order correction [i+1 i i-1 i-2 i-3]
         Fac = Fac * ( cfl - 3. _d 0 )/5. _d 0
         DelMM = (Qim-Qimm)*MskImm*MskIm
         Del2M = DelM - DelMM
         Del3M = Del2 - Del2M
         Del4 = Del3P - Del3M
         Phi = Phi + Fac * Del4
C        6th order correction [i+2 i+1 i i-1 i-2 i-3]
         Fac = Fac * ( cfl + 2. _d 0 )/6. _d 0
         DelPPP = (Qippp-Qipp)*MskIpp*MskIp*MskI
         Del2PP = DelPP - DelP
         Del3PP = Del2PP - Del2P
         Del4P = Del3PP - Del3P
         Del5P = Del4P - Del4
         Phi = Phi + Fac * Del5P
C        7th order correction [i+2 i+1 i i-1 i-2 i-3 i-4]
         Fac = Fac * ( cfl + 2. _d 0 )/7. _d 0
         DelMMM = (Qimm-Qimmm)*MskImmm*MskImm*MskIm
         Del2MM = DelMM - DelMMM
         Del3MM = Del2M - Del2MM
         Del4M = Del3M - Del3MM
         Del5M = Del4 - Del4M
         Del6 = Del5P - Del5M
         Phi = Phi - Fac * Del6

         DelIp = ( Qip - Qi ) * MskI
c        Phi = sign(1. _d 0,Phi)*sign(1. _d 0,DelIp)
c    &        *abs(Phi+Eps)/abs(DelIp+Eps)
C--   simplify and avoid division by zero
         recip_DelIp = sign(1. _d 0,DelIp)/max(abs(DelIp),Eps)
         Phi = Phi*recip_DelIp

         DelI = ( Qi - Qim ) * MskIm
c        rp1h =sign(1. _d 0,DelI)*sign(1. _d 0,DelIp)
c    &        *abs(DelI+Eps)/abs(DelIp+Eps)
C--   simplify and avoid division by zero
         recip_DelI = sign(1. _d 0,DelI)/max(abs(DelI),Eps)
         rp1h = DelI*recip_DelIp
         rp1h_cfl = rp1h/(cfl+Eps)

C        TVD limiter
c        Phi = max(0. _d 0, min( 2./(1-cfl), Phi, 2.*rp1h_cfl ) )

C        MP limiter
         d2   = Del2 !( ( Qip + Qim ) - 2.*Qi  ) * MskI * MskIm
         d2p1 = Del2P !( ( Qipp + Qi ) - 2.*Qip ) * MskIp * MskI
         d2m1 = Del2M !( ( Qi + Qimm ) - 2.*Qim ) * MskIm * MskImm
         A = 4. _d 0*d2 - d2p1
         B = 4. _d 0*d2p1 - d2
         C = d2
         D = d2p1
         dp1h = max(min(A,B,C,D),0. _d 0)+min(max(A,B,C,D),0. _d 0)
         A = 4. _d 0*d2m1 - d2
         B = 4. _d 0*d2 - d2m1
         C = d2m1
         D = d2
         dm1h = max(min(A,B,C,D),0. _d 0)+min(max(A,B,C,D),0. _d 0)
c        qMD = 0.5*( ( Qi + Qip ) - dp1h )
c        qMD = 0.5 _d 0*( ( 2. _d 0*Qi + DelIp ) - dp1h )
c        qUL = Qi + (1. _d 0-cfl)/(cfl+Eps)*DelI
c        qLC = Qi + 0.5 _d 0*( 1. _d 0+dm1h/(DelI+Eps) )*(qUL-Qi)
c        PhiMD = 2. _d 0/(1. _d 0-cfl)*(qMD-Qi+Eps)/(DelIp+Eps)
c        PhiLC = 2. _d 0*rp1h_cfl*(qLC-Qi+Eps)/(qUL-Qi+Eps)
C--   simplify and avoid division by zero
         PhiMD = 1. _d 0/(1. _d 0-cfl)*(DelIp-dp1h)*recip_DelIp
         PhiLC = rp1h_cfl*( 1. _d 0+dm1h*recip_DelI )
C--
         PhiMin = max( min(0. _d 0,PhiMD),
     &                 min(0. _d 0,2. _d 0*rp1h_cfl,PhiLC) )
         PhiMax = min( max(2. _d 0/(1. _d 0-cfl),PhiMD),
     &                 max(0. _d 0,2. _d 0*rp1h_cfl,PhiLC) )
         Phi = max(PhiMin,min(Phi,PhiMax))

         Psi = Phi * 0.5 _d 0 * (1. _d 0 - cfl)
         vT(i,j) = vTrans(i,j)*( Qi + Psi*DelIp )
        ELSE
         vT(i,j) = 0. _d 0
        ENDIF

       ENDDO
      ENDDO

      RETURN
      END
