C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_os7mp_adv_r.F,v 1.4 2007/05/11 18:24:31 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_OS7MP_ADV_R(
     I           bi,bj,k,deltaTloc,
     I           wTrans, wFld,
     I           Q,
     O           wT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_OS7MP_ADV_R                               |
C     | o Compute Vertical advective Flux of tracer Q using      |
C     |   7th Order DST Sceheme with monotone preserving limiter |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"
#include "GAD.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL deltaTloc
      _RL wTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Q     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j,kp3,kp2,kp1,km1,km2,km3,km4
      _RL cfl,Psi
      _RL wLoc,Fac,DelIp,DelI,Phi,Eps,rp1h,rp1h_cfl
      _RL Qippp,Qipp,Qip,Qi,Qim,Qimm,Qimmm
      _RL MskIpp,MskIp,MskI,MskIm,MskImm,MskImmm
      _RL d2,d2p1,d2m1,A,B,C,D
      _RL dp1h,dm1h,qMD,qUL,qLC,PhiMD,PhiLC,PhiMin,PhiMax
      _RL DelM,DelP,DelMM,DelPP,DelMMM,DelPPP
      _RL Del2MM,Del2M,Del2,Del2P,Del2PP
      _RL Del3MM,Del3M,Del3P,Del3PP
      _RL Del4M,Del4,Del4P
      _RL Del5M,Del5P
      _RL Del6

      Eps = 1. _d -20

      km4=MAX(1,k-4)
      km3=MAX(1,k-3)
      km2=MAX(1,k-2)
      km1=MAX(1,k-1)
      kp1=MIN(Nr,k+1)
      kp2=MIN(Nr,k+2)
      kp3=MIN(Nr,k+3)

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx

        wLoc = wFld(i,j)
        cfl = abs(wLoc*deltaTloc*recip_drC(k))

        IF (wTrans(i,j).lt.0.) THEN
         Qippp = Q(i,j,kp2)
         Qipp  = Q(i,j,kp1)
         Qip   = Q(i,j,k)
         Qi    = Q(i,j,km1)
         Qim   = Q(i,j,km2)
         Qimm  = Q(i,j,km3)
         Qimmm = Q(i,j,km4)

         MskIpp  = maskC(i,j,kp2,bi,bj) * float(kp2-kp1)
         MskIp   = maskC(i,j,kp1,bi,bj) * float(kp1-k)
         MskI    = maskC(i,j,k,bi,bj)   * float(k-km1)
         MskIm   = maskC(i,j,km1,bi,bj) * float(km1-km2)
         MskImm  = maskC(i,j,km2,bi,bj) * float(km2-km3)
         MskImmm = maskC(i,j,km3,bi,bj) * float(km3-km4)
        ELSEIF (wTrans(i,j).gt.0.) THEN
         Qippp = Q(i,j,km3)
         Qipp  = Q(i,j,km2)
         Qip   = Q(i,j,km1)
         Qi    = Q(i,j,k)
         Qim   = Q(i,j,kp1)
         Qimm  = Q(i,j,kp2)
         Qimmm = Q(i,j,kp3)

         MskIpp  = maskC(i,j,km2,bi,bj) * float(km2-km3)
         MskIp   = maskC(i,j,km1,bi,bj) * float(km1-km2)
         MskI    = maskC(i,j,k,bi,bj)   * float(k-km1)
         MskIm   = maskC(i,j,kp1,bi,bj) * float(kp1-k)
         MskImm  = maskC(i,j,kp2,bi,bj) * float(kp2-kp1)
         MskImmm = maskC(i,j,kp3,bi,bj) * float(kp3-kp2)
        ENDIF

        IF (wTrans(i,j).ne.0.) THEN
C        2nd order correction [i i-1]
         Fac = 1.
         DelP = (Qip-Qi)*MskI
         Phi = Fac * DelP
C        3rd order correction [i i-1 i-2]
         Fac = Fac * ( cfl + 1. )/3.
         DelM = (Qi-Qim)*MskIm
         Del2 = DelP - DelM
         Phi = Phi - Fac * Del2
C        4th order correction [i+1 i i-1 i-2]
         Fac = Fac * ( cfl - 2. )/4.
         DelPP = (Qipp-Qip)*MskIp*MskI
         Del2P = DelPP - DelP
         Del3P = Del2P - Del2
         Phi = Phi + Fac * Del3p
C        5th order correction [i+1 i i-1 i-2 i-3]
         Fac = Fac * ( cfl - 3. )/5.
         DelMM = (Qim-Qimm)*MskImm*MskIm
         Del2M = DelM - DelMM
         Del3M = Del2 - Del2M
         Del4 = Del3P - Del3M
         Phi = Phi + Fac * Del4
C        6th order correction [i+2 i+1 i i-1 i-2 i-3]
         Fac = Fac * ( cfl + 2. )/6.
         DelPPP = (Qippp-Qipp)*MskIpp*MskIp*MskI
         Del2PP = DelPP - DelP
         Del3PP = Del2PP - Del2P
         Del4P = Del3PP - Del3P
         Del5P = Del4P - Del4
         Phi = Phi + Fac * Del5P
C        7th order correction [i+2 i+1 i i-1 i-2 i-3 i-4]
         Fac = Fac * ( cfl + 2. )/7.
         DelMMM = (Qimm-Qimmm)*MskImmm*MskImm*MskIm
         Del2MM = DelMM - DelMMM
         Del3MM = Del2M - Del2MM
         Del4M = Del3M - Del3MM
         Del5M = Del4 - Del4M
         Del6 = Del5P - Del5M
         Phi = Phi - Fac * Del6

         DelIp = ( Qip - Qi ) * MskI
         Phi = sign(1. _d 0,Phi)*sign(1. _d 0,DelIp)
     &        *abs(Phi+Eps)/abs(DelIp+Eps)

         DelI = ( Qi - Qim ) * MskIm
         rp1h =sign(1. _d 0,DelI)*sign(1. _d 0,DelIp)
     &        *abs(DelI+Eps)/abs(DelIp+Eps)
         rp1h_cfl = rp1h/(cfl+Eps)

C        TVD limiter
!        Phi = max(0. _d 0, min( 2./(1-cfl), Phi, 2.*rp1h_cfl ) )

C        MP limiter
         d2   = Del2 !( ( Qip + Qim ) - 2.*Qi  ) * MskI * MskIm
         d2p1 = Del2P !( ( Qipp + Qi ) - 2.*Qip ) * MskIp * MskI
         d2m1 = Del2M !( ( Qi + Qimm ) - 2.*Qim ) * MskIm * MskImm
         A = 4.*d2 - d2p1
         B = 4.*d2p1 - d2
         C = d2
         D = d2p1;
         dp1h = max(min(A,B,C,D),0. _d 0)+min(max(A,B,C,D),0. _d 0)
         A = 4.*d2m1 - d2
         B = 4.*d2 - d2m1
         C = d2m1
         D = d2;
         dm1h = max(min(A,B,C,D),0. _d 0)+min(max(A,B,C,D),0. _d 0)
        !qMD = 0.5*( ( Qi + Qip ) - dp1h )
         qMD = 0.5*( ( 2.*Qi + DelIp ) - dp1h )
         qUL = Qi + (1.-cfl)/(cfl+Eps)*DelI
         qLC = Qi + 0.5*( 1.+dm1h/(DelI+Eps) )*(qUL-Qi)
         PhiMD = 2./(1.-cfl)*(qMD-Qi+Eps)/(DelIp+Eps)
         PhiLC = 2.*rp1h_cfl*(qLC-Qi+Eps)/(qUL-Qi+Eps)
         PhiMin = max(min(0. _d 0,PhiMD),
     &        min(0. _d 0,2.*rp1h_cfl,PhiLC))
         PhiMax = min(max(2. _d 0/(1.-cfl),PhiMD),
     &        max(0. _d 0,2.*rp1h_cfl,PhiLC))
         Phi = max(PhiMin,min(Phi,PhiMax))

         Psi = Phi * 0.5 * (1. - cfl)
         wT(i,j) = wTrans(i,j)*( Qi + Psi*DelIp )
        ELSE
         wT(i,j) = 0.
        ENDIF

       ENDDO
      ENDDO

      RETURN
      END
