C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_u_vertshear.F,v 1.4 2001/09/10 14:14:36 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_U_VERTSHEAR(
     I        bi,bj,K,
     I        uFld,wFld,
     U        uShearTerm,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_U_VERTSHEAR                                      |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,K
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL uShearTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J,Kp1,Km1
      _RL  mask_Kp1,mask_Km1,wBarXm,wBarXp
      _RL  uZm,uZp,umask_Kp1,umask_K,umask_Km1
      LOGICAL freeslipK,noslipK
      PARAMETER(freeslipK=.TRUE.)
      PARAMETER(noslipK=.NOT.freeslipK)
      LOGICAL freeslip1,noslip1
      PARAMETER(freeslip1=.TRUE.)
      PARAMETER(noslip1=.NOT.freeslip1)
c1    _RL  wBarXZ,uZbarZ
c1    LOGICAL upwindShear
c1    PARAMETER(upwindShear=.FALSE.)

      Kp1=min(K+1,Nr)
      mask_Kp1=1.
      IF (K.EQ.Nr) mask_Kp1=0.
      Km1=max(K-1,1)
      mask_Km1=1.
      IF (K.EQ.1) mask_Km1=0.

      DO J=1-Oly,sNy+Oly
       DO I=2-Olx,sNx+Olx

c       umask_K=_maskW(i,j,k,bi,bj)

C barZ( barX( W ) )
c       wBarXm=0.5*(wFld(I,J,K,bi,bj)+wFld(I-1,J,K,bi,bj))
c       wBarXp=0.5*(wFld(I,J,Kp1,bi,bj)+wFld(I-1,J,Kp1,bi,bj))
c    &         *mask_Kp1

C       Transport at interface k
        wBarXm=0.5*(
     &    wFld(I,J,K,bi,bj)*rA(i,j,bi,bj)*maskC(I,J,Km1,bi,bj)
     &   +wFld(I-1,J,K,bi,bj)*rA(i-1,j,bi,bj)*maskC(I-1,J,Km1,bi,bj)
     &             )*mask_Km1

C       Transport at interface k+1 (here wFld is already masked)
        wBarXp=0.5*(
     &    wFld(I,J,Kp1,bi,bj)*rA(i,j,bi,bj)
     &   +wFld(I-1,J,Kp1,bi,bj)*rA(i-1,j,bi,bj)
     &             )*mask_Kp1

C delta_Z( U )  @ interface k
c       umask_Km1=mask_Km1*maskW(i,j,Km1,bi,bj)
        uZm=(mask_Km1*uFld(I,J,Km1,bi,bj)-uFld(I,J,K,bi,bj))
c2   &      *recip_dRC(K)
c       IF (freeslip1) uZm=uZm*umask_Km1
c       IF (noslip1.AND.umask_Km1.EQ.0.) uZm=uZm*2.

C delta_Z( U )  @ interface k+1
c       umask_Kp1=mask_Kp1*maskW(i,j,Kp1,bi,bj)
        uZp=(uFld(I,J,K,bi,bj)-mask_Kp1*uFld(I,J,Kp1,bi,bj))
c2   &      *recip_dRC(Kp1)
c       IF (freeslipK) uZp=uZp*umask_Kp1
c       IF (noslipK.AND.umask_Kp1.EQ.0.) uZp=uZp*2.

c1      IF (upwindShear) THEN
c1       wBarXZ=0.5*( wBarXm + wBarXp )
c1       IF (wBarXZ.GT.0.) THEN
c1        uZbarZ=uZp
c1       ELSE
c1        uZbarZ=uZm
c1       ENDIF
c1      ELSE
c1       uZbarZ=0.5*(uZm+uZp)
c1      ENDIF
c1      uShearTerm(I,J)=-wBarXZ*uZbarZ*_maskW(I,J,K,bi,bj)

c2      uShearTerm(I,J)=-0.5*(wBarXp*uZp+wBarXm*uZm)
c2   &                  *_maskW(I,J,K,bi,bj)
        uShearTerm(I,J)=-0.5*(wBarXp*uZp+wBarXm*uZm)
     &                  *recip_raw(i,j,bi,bj)
     &                  *recip_hFacW(i,j,k,bi,bj)
     &                  *recip_dRF(K)
       ENDDO
      ENDDO

      RETURN
      END
