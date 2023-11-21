#include "MOM_VECINV_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_VI_V_VERTSHEAR

C     !INTERFACE:
      SUBROUTINE MOM_VI_V_VERTSHEAR(
     I        bi, bj, k, deepFacA,
     I        vFld, wFld,
     U        vShearTerm,
     I        myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R MOM_V_VERTSHEAR
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C  deepFacA             :: deep-model grid factor at level center
      INTEGER bi, bj, k
      _RL deepFacA(Nr)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vShearTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER i, j, kp1, km1
      _RL mask_Kp1, mask_Km1, wBarYm, wBarYp
      _RL vZm, vZp, recip_drDeepRho
      LOGICAL rAdvAreaWeight
c     _RL vmask_Kp1,vmask_K,vmask_Km1
c1    _RL wBarYZ,vZbarZ
CEOP

      rAdvAreaWeight =.TRUE.
C-    Area-weighted average either in KE or in vert. advection:
      IF ( selectKEscheme.EQ.1 .OR. selectKEscheme.EQ.3 )
     &  rAdvAreaWeight =.FALSE.

      kp1 = MIN(k+1,Nr)
      mask_Kp1 = oneRL
      IF (k.EQ.Nr) mask_Kp1 = zeroRL
      km1 = MAX(k-1,1)
      mask_Km1 = oneRL
      IF (k.EQ.1) mask_Km1 = zeroRL

      recip_drDeepRho = recip_drF(k)/deepFacA(k)
     &                * recip_deepFac2C(k)*recip_rhoFacC(k)

      DO j=2-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx

c       vmask_K=_maskS(i,j,k,bi,bj)

C barZ( barY( W ) )
c       wBarYm=0.5*(wFld(I,J,K,bi,bj)+wFld(I,J-1,K,bi,bj))
c       wBarYp=0.5*(wFld(I,J,Kp1,bi,bj)+wFld(I,J-1,Kp1,bi,bj))
c    &              *mask_Kp1

       IF ( rAdvAreaWeight ) THEN
C     Transport at interface k : Area weighted average
        wBarYm = halfRL*(
     &     wFld(i,j,k,bi,bj)*rA(i,j,bi,bj)*maskC(i,j,km1,bi,bj)
     &   + wFld(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj)*maskC(i,j-1,km1,bi,bj)
     &                  )*mask_Km1*deepFac2F(k)*rhoFacF(k)
     &                   *recip_rAs(i,j,bi,bj)

C     Transport at interface k+1 (here wFld is already masked)
        wBarYp = halfRL*(
     &     wFld(i,j,kp1,bi,bj)*rA(i,j,bi,bj)
     &   + wFld(i,j-1,kp1,bi,bj)*rA(i,j-1,bi,bj)
     &                  )*mask_Kp1*deepFac2F(kp1)*rhoFacF(kp1)
     &                   *recip_rAs(i,j,bi,bj)
       ELSE
C     Transport at interface k : simple average
        wBarYm = halfRL*(
     &     wFld(i,j,k,bi,bj)*maskC(i,j,km1,bi,bj)
     &   + wFld(i,j-1,k,bi,bj)*maskC(i,j-1,km1,bi,bj)
     &                  )*mask_Km1*deepFac2F(k)*rhoFacF(k)

C     Transport at interface k+1 (here wFld is already masked)
        wBarYp = halfRL*(
     &     wFld(i,j,kp1,bi,bj)
     &   + wFld(i,j-1,kp1,bi,bj)
     &                  )*mask_Kp1*deepFac2F(kp1)*rhoFacF(kp1)
       ENDIF

C-    delta_Z( V*deepFac )  @ interface k
c       vmask_Km1=mask_Km1*maskS(i,j,Km1,bi,bj)
        vZm = ( vFld(i,j, k ,bi,bj)*deepFacA( k )
     &        - vFld(i,j,km1,bi,bj)*deepFacA(km1)*mask_Km1 )*rkSign
c2   &      *recip_dRC(K)
c       IF (freeslip1) vZm=vZm*vmask_Km1
c       IF (noslip1.AND.vmask_Km1.EQ.0.) vZm=vZm*2.

C-    delta_Z( V*deepFac )  @ interface k+1
c       vmask_Kp1=mask_Kp1*maskS(i,j,Kp1,bi,bj)
        vZp = ( vFld(i,j,kp1,bi,bj)*deepFacA(kp1)*mask_Kp1
     &        - vFld(i,j, k ,bi,bj)*deepFacA( k ) )*rkSign
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
        IF (upwindShear) THEN
         vShearTerm(i,j) = -halfRL*
     &                  (    (     wBarYp *vZp +     wBarYm *vZm )
     &                     + ( ABS(wBarYp)*vZp - ABS(wBarYm)*vZm )
     &                  )*_recip_hFacS(i,j,k,bi,bj)*recip_drDeepRho
        ELSE
         vShearTerm(i,j) = -halfRL*( wBarYp*vZp + wBarYm*vZm )
     &                   *_recip_hFacS(i,j,k,bi,bj)*recip_drDeepRho
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
