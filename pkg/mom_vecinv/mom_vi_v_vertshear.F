C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_v_vertshear.F,v 1.4 2001/09/10 14:14:36 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_V_VERTSHEAR( 
     I        bi,bj,K,
     I        vFld,wFld,
     U        vShearTerm,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_V_VERTSHEAR                                      |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,K
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vShearTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J,Kp1,Km1
      _RL mask_Kp1,mask_Km1,wBarYm,wBarYp
      _RL vZm,vZp,vmask_Kp1,vmask_K,vmask_Km1
      LOGICAL freeslipK,noslipK
      PARAMETER(freeslipK=.TRUE.)
      PARAMETER(noslipK=.NOT.freeslipK)
      LOGICAL freeslip1,noslip1
      PARAMETER(freeslip1=.TRUE.)
      PARAMETER(noslip1=.NOT.freeslip1)
c1    _RL wBarYZ,vZbarZ
c1    LOGICAL upwindShear
c1    PARAMETER(upwindShear=.FALSE.)

      Kp1=min(K+1,Nr)
      mask_Kp1=1.
      IF (K.EQ.Nr) mask_Kp1=0.
      Km1=max(K-1,1)
      mask_Km1=1.
      IF (K.EQ.1) mask_Km1=0.

      DO J=2-Oly,sNy+Oly
       DO I=1-Olx,sNx+Olx

c       vmask_K=_maskS(i,j,k,bi,bj)

C barZ( barY( W ) )
c       wBarYm=0.5*(wFld(I,J,K,bi,bj)+wFld(I,J-1,K,bi,bj))
c       wBarYp=0.5*(wFld(I,J,Kp1,bi,bj)+wFld(I,J-1,Kp1,bi,bj))
c    &              *mask_Kp1

C       Transport at interface k
        wBarYm=0.5*(
     &    wFld(I,J,K,bi,bj)*rA(i,j,bi,bj)*maskC(i,j,Km1,bi,bj)
     &   +wFld(I,J-1,K,bi,bj)*rA(i,j-1,bi,bj)*maskC(i,j-1,Km1,bi,bj)
     &             )*mask_Km1

C       Transport at interface k+1 (here wFld is already masked)
        wBarYp=0.5*(
     &    wFld(I,J,Kp1,bi,bj)*rA(i,j,bi,bj)
     &   +wFld(I,J-1,Kp1,bi,bj)*rA(i,j-1,bi,bj)
     &             )*mask_Kp1

C delta_Z( V )  @ interface k
c       vmask_Km1=mask_Km1*maskS(i,j,Km1,bi,bj)
        vZm=(mask_Km1*vFld(I,J,Km1,bi,bj)-vFld(I,J,K,bi,bj))
c2   &      *recip_dRC(K)
c       IF (freeslip1) vZm=vZm*vmask_Km1
c       IF (noslip1.AND.vmask_Km1.EQ.0.) vZm=vZm*2.

C delta_Z( V )  @ interface k+1
c       vmask_Kp1=mask_Kp1*maskS(i,j,Kp1,bi,bj)
        vZp=(vFld(I,J,K,bi,bj)-mask_Kp1*vFld(I,J,Kp1,bi,bj))
c2   &      *recip_dRC(Kp1)
c       IF (freeslipK) vZp=vZp*vmask_Kp1
c       IF (noslipK.AND.vmask_Kp1.EQ.0.) vZp=vZp*2.

c1      IF (upwindShear) THEN
c1       wBarYZ=0.5*( wBarXm + wBarXp )
c1       IF (wBarYZ.GT.0.) THEN
c1        vZbarZ=vZp
c1       ELSE
c1        vZbarZ=vZm
c1       ENDIF
c1      ELSE
c1       vZbarZ=0.5*(vZm+vZp)
c1      ENDIF
c1      vShearTerm(I,J)=-wBarYZ*vZbarZ*_maskS(I,J,K,bi,bj)

c2      vShearTerm(I,J)=-0.5*(wBarYp*vZp+wBarYm*vZm)
c2   &                  *_maskS(I,J,K,bi,bj)
        vShearTerm(I,J)=-0.5*(wBarYp*vZp+wBarYm*vZm)
     &                  *recip_ras(i,j,bi,bj)
     &                  *recip_hFacS(i,j,k,bi,bj)
     &                  *recip_dRF(K)
       ENDDO
      ENDDO

      RETURN
      END
