#include "MOM_VECINV_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_VI_U_VERTSHEAR

C     !INTERFACE:
      SUBROUTINE MOM_VI_U_VERTSHEAR(
     I        bi, bj, k,
     I        uFld, wFld,
     U        uShearTerm,
     I        myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R MOM_U_VERTSHEAR
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER bi, bj, k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL uShearTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER i, j, kp1, km1
      _RL  mask_Kp1, mask_Km1, wBarXm, wBarXp
      _RL  uZm, uZp
      LOGICAL rAdvAreaWeight
c     _RL  umask_Kp1,umask_K,umask_Km1
c1    _RL  wBarXZ,uZbarZ
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

      DO j=1-OLy,sNy+OLy
       DO i=2-OLx,sNx+OLx

c       umask_K=_maskW(i,j,k,bi,bj)

C barZ( barX( W ) )
c       wBarXm=0.5*(wFld(I,J,K,bi,bj)+wFld(I-1,J,K,bi,bj))
c       wBarXp=0.5*(wFld(I,J,Kp1,bi,bj)+wFld(I-1,J,Kp1,bi,bj))
c    &         *mask_Kp1

       IF ( rAdvAreaWeight ) THEN
C     Transport at interface k : Area weighted average
        wBarXm = halfRL*(
     &     wFld(i,j,k,bi,bj)*rA(i,j,bi,bj)*maskC(i,j,km1,bi,bj)
     &   + wFld(i-1,j,k,bi,bj)*rA(i-1,j,bi,bj)*maskC(i-1,j,km1,bi,bj)
     &                  )*mask_Km1*deepFac2F(k)*rhoFacF(k)
     &                   *recip_rAw(i,j,bi,bj)

C     Transport at interface k+1 (here wFld is already masked)
        wBarXp = halfRL*(
     &     wFld(i,j,kp1,bi,bj)*rA(i,j,bi,bj)
     &   + wFld(i-1,j,kp1,bi,bj)*rA(i-1,j,bi,bj)
     &                  )*mask_Kp1*deepFac2F(kp1)*rhoFacF(kp1)
     &                   *recip_rAw(i,j,bi,bj)
       ELSE
C     Transport at interface k : simple average
        wBarXm = halfRL*(
     &     wFld(i,j,k,bi,bj)*maskC(i,j,km1,bi,bj)
     &   + wFld(i-1,j,k,bi,bj)*maskC(i-1,j,km1,bi,bj)
     &                  )*mask_Km1*deepFac2F(k)*rhoFacF(k)

C     Transport at interface k+1 (here wFld is already masked)
        wBarXp = halfRL*(
     &     wFld(i,j,kp1,bi,bj)
     &   + wFld(i-1,j,kp1,bi,bj)
     &                  )*mask_Kp1*deepFac2F(kp1)*rhoFacF(kp1)
       ENDIF

#ifdef MOM_VI_DEEP_VERTSHEAR
C     delta_Z( U*deepFac )  @ interface k
        uZm = ( uFld(i,j, k ,bi,bj)*deepFacC( k )
     &        - uFld(i,j,km1,bi,bj)*deepFacC(km1)*mask_Km1 )*rkSign
C     delta_Z( U*deepFac )  @ interface k+1
        uZp = ( uFld(i,j,kp1,bi,bj)*deepFacC(kp1)*mask_Kp1
     &        - uFld(i,j, k ,bi,bj)*deepFacC( k ) )*rkSign

#else /* MOM_VI_DEEP_VERTSHEAR */
C delta_Z( U )  @ interface k
c       umask_Km1=mask_Km1*maskW(i,j,Km1,bi,bj)
        uZm=(uFld(i,j,k,bi,bj)-mask_Km1*uFld(i,j,km1,bi,bj))*rkSign
c2   &      *recip_dRC(K)
c       IF (freeslip1) uZm=uZm*umask_Km1
c       IF (noslip1.AND.umask_Km1.EQ.0.) uZm=uZm*2.

C delta_Z( U )  @ interface k+1
c       umask_Kp1=mask_Kp1*maskW(i,j,Kp1,bi,bj)
        uZp=(mask_Kp1*uFld(i,j,kp1,bi,bj)-uFld(i,j,k,bi,bj))*rkSign
c2   &      *recip_dRC(Kp1)
c       IF (freeslipK) uZp=uZp*umask_Kp1
c       IF (noslipK.AND.umask_Kp1.EQ.0.) uZp=uZp*2.
#endif /* MOM_VI_DEEP_VERTSHEAR */

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
        IF (upwindShear) THEN
         uShearTerm(i,j) = -halfRL*
     &                  (    (     wBarXp *uZp +     wBarXm *uZm )
     &                     + ( ABS(wBarXp)*uZp - ABS(wBarXm)*uZm )
     &                  )*_recip_hFacW(i,j,k,bi,bj)
     &                   * recip_drF(k)
     &                   * recip_deepFac2C(k)*recip_rhoFacC(k)
#ifdef MOM_VI_DEEP_VERTSHEAR
     &                   * recip_deepFacC(k)
#endif
        ELSE
         uShearTerm(i,j) = -halfRL*( wBarXp*uZp + wBarXm*uZm )
     &                   *_recip_hFacW(i,j,k,bi,bj)
     &                   * recip_drF(k)
     &                   * recip_deepFac2C(k)*recip_rhoFacC(k)
#ifdef MOM_VI_DEEP_VERTSHEAR
     &                   * recip_deepFacC(k)
#endif
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
