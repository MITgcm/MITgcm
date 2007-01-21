C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_os7mp_adv_x.F,v 1.2 2007/01/21 17:25:31 mlosch Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_OS7MP_ADV_X(
     I           bi,bj,k,deltaTloc,
     I           uTrans, uFld,
     I           maskLocW, Q,
     O           uT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_OS7MP_ADV_X                               |
C     | o Compute Zonal advective Flux of tracer Q using         |
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
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Q     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL cfl,Psi
      _RL uLoc,Fac,Del,DelIp,DelI,Phi,Eps,rp1h,Msk
      _RL Qippp,Qipp,Qip,Qi,Qim,Qimm,Qimmm
      _RL MskIpp,MskIp,MskI,MskIm,MskImm,MskImmm
      _RL d2,d2p1,d2m1,A,B,C,D
      _RL dp1h,dm1h,qMD,qUL,qLC,PhiMD,PhiLC,PhiMin,PhiMax

      Eps = 1. _d -20

      DO j=1-Oly,sNy+Oly
       uT(1-Olx,j)=0. _d 0
       uT(2-Olx,j)=0. _d 0
       uT(3-Olx,j)=0. _d 0
       uT(4-Olx,j)=0. _d 0
       uT(sNx+Olx-2,j)=0. _d 0
       uT(sNx+Olx-1,j)=0. _d 0
       uT(sNx+Olx,j)=0. _d 0
       DO i=1-Olx+4,sNx+Olx-3

        uLoc = uFld(i,j)
        cfl = abs(uLoc*deltaTloc*recip_dxC(i,j,bi,bj))

        IF (uLoc.gt.0.) THEN
         Qippp = Q(i+2,j)
         Qipp  = Q(i+1,j)
         Qip   = Q(i,j)
         Qi    = Q(i-1,j)
         Qim   = Q(i-2,j)
         Qimm  = Q(i-3,j)
         Qimmm = Q(i-4,j)

         MskIpp  = maskLocW(i+2,j)
         MskIp   = maskLocW(i+1,j)
         MskI    = maskLocW(i,j)
         MskIm   = maskLocW(i-1,j)
         MskImm  = maskLocW(i-2,j)
         MskImmm = maskLocW(i-3,j)
        ELSEIF (uLoc.lt.0.) THEN
         Qippp = Q(i-3,j)
         Qipp  = Q(i-2,j)
         Qip   = Q(i-1,j)
         Qi    = Q(i,j)
         Qim   = Q(i+1,j)
         Qimm  = Q(i+2,j)
         Qimmm = Q(i+3,j)

         MskIpp  = maskLocW(i-2,j)
         MskIp   = maskLocW(i-1,j)
         MskI    = maskLocW(i,j)
         MskIm   = maskLocW(i+1,j)
         MskImm  = maskLocW(i+2,j)
         MskImmm = maskLocW(i+3,j)
        ENDIF

        IF (uLoc.ne.0.) THEN
C        2nd order correction [i i-1]
         Fac = 1.
         Del = Qip-Qi
         Msk = MskI
         Phi = Msk * Fac * Del
C        3rd order correction [i i-1 i-2]
         Fac = Fac * ( cfl + 1. )/3.
         Del = Del - ( Qi-Qim )
         Msk = Msk * MskIm
         Phi = Phi - Msk * Fac * Del
C        4th order correction [i+1 i i-1 i-2]
         Fac = Fac * ( cfl - 2. )/4.
         Del = ( Qipp-2.*Qip+Qi ) - Del
         Msk = Msk * MskIp
         Phi = Phi + Msk * Fac * Del
C        5th order correction [i+1 i i-1 i-2 i-3]
         Fac = Fac * ( cfl - 3. )/5.
         Del = Del - ( Qip-3.*Qi+3.*Qim-Qimm )
         Msk = Msk * MskImm
         Phi = Phi + Msk * Fac * Del
C        6th order correction [i+2 i+1 i i-1 i-2 i-3]
         Fac = Fac * ( cfl + 2. )/6.
         Del = ( Qippp-4.*Qipp+6.*Qip-4.*Qi+Qim ) - Del
         Msk = Msk * MskIpp
         Phi = Phi + Msk * Fac * Del
C        7th order correction [i+2 i+1 i i-1 i-2 i-3 i-4]
         Fac = Fac * ( cfl + 2. )/7.
         Del = Del - ( Qipp-5.*Qip+10.*Qi-10.*Qim+5.*Qimm-Qimmm ) 
         Msk = Msk * MskImmm
         Phi = Phi - Msk * Fac * Del

         DelIp = ( Qip - Qi ) * MskI
         Phi = sign(1. _d 0,Phi)*sign(1. _d 0,DelIp)
     &        *abs(Phi+Eps)/abs(DelIp+Eps)

         DelI = ( Qi - Qim ) * MskIm
         rp1h =sign(1. _d 0,DelI)*sign(1. _d 0,DelIp)
     &        *abs(DelI+Eps)/abs(DelIp+Eps)

C        TVD limiter
!        Phi = max(0. _d 0, min( 2./(1-cfl), Phi, 2.*rp1h/cfl ) )

C        MP limiter
         d2   = ( ( Qip + Qim ) - 2.*Qi  ) * MskI * MskIm
         d2p1 = ( ( Qipp + Qi ) - 2.*Qip ) * MskIp * MskI
         d2m1 = ( ( Qi + Qimm ) - 2.*Qim ) * MskIm * MskImm
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
         qMD = 0.5*( ( Qi + Qip ) - dp1h )
         qUL = Qi + (1.-cfl)/cfl*( Qi-Qim )
         qLC = Qi + 0.5*( 1.+dm1h/(Qi-Qim+Eps) )*(qUL-Qi)
         PhiMD = 2./(1.-cfl)*(qMD-Qi+Eps)/(Qip-Qi+Eps)
         PhiLC = 2.*rp1h/cfl*(qLC-Qi+Eps)/(qUL-Qi+Eps)
         PhiMin = max(min(0. _d 0,PhiMD),
     &        min(0. _d 0,2.*rp1h/cfl,PhiLC))
         PhiMax = min(max(2. _d 0/(1.-cfl),PhiMD),
     &        max(0. _d 0,2.*rp1h/cfl,PhiLC))
         Phi = max(PhiMin,min(Phi,PhiMax))

         Psi = Phi * 0.5 * (1. - cfl)
         uT(i,j) = uTrans(i,j)*( Qi + Psi*DelIp )
        ELSE
         uT(i,j) = 0.
        ENDIF

       ENDDO
      ENDDO

      RETURN
      END
