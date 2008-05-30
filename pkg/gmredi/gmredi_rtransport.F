C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_rtransport.F,v 1.15 2008/05/30 02:50:16 gforget Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

      subroutine GMREDI_RTRANSPORT(
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     Tracer,tracerIdentity,
     U     df,
     I     myThid)
C     /==========================================================\
C     | o SUBROUTINE GMREDI_RTRANSPORT                           |
C     |   Add vertical transport terms from GM/Redi              |
C     |   parameterization.                                      |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     == Routine arguments ==
C     iMin,iMax,jMin,  - Range of points for which calculation
C     jMax,bi,bj,k       results will be set.
C     xA               - Area of X face
C     Tracer           - 3D Tracer field
C     df               - Diffusive flux component work array.
      INTEGER iMin,iMax,jMin,jMax,bi,bj,k
      _RL Tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER tracerIdentity
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

#ifdef ALLOW_GMREDI

C     == Local variables ==
C     I, J - Loop counters
      INTEGER I, J
      _RL dTdx  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dTdy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef GM_BOLUS_ADVEC
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
          act0 = tracerIdentity - 1
          max0 = maxpass
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          igadkey = (act0 + 1) 
     &                      + act1*max0
     &                      + act2*max0*max1
     &                      + act3*max0*max1*max2
     &                      + act4*max0*max1*max2*max3
          kkey = (igadkey-1)*Nr + k
          if (tracerIdentity.GT.maxpass) then
             print *, 'ph-pass gmredi_rtrans ', maxpass, tracerIdentity
             STOP 'maxpass seems smaller than tracerIdentity'
          endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     Surface flux is zero
      IF (useGMRedi .AND. K.GT.1) THEN

C-      Horizontal gradients interpolated to W points
      DO j=jMin,jMax
       DO i=iMin,iMax
        dTdx(i,j) = op5*(
     &   +op5*(_maskW(i+1,j,k,bi,bj)
     &         *_recip_dxC(i+1,j,bi,bj)*
     &           (Tracer(i+1,j,k,bi,bj)-Tracer(i,j,k,bi,bj))
     &        +_maskW(i,j,k,bi,bj)
     &         *_recip_dxC(i,j,bi,bj)*
     &           (Tracer(i,j,k,bi,bj)-Tracer(i-1,j,k,bi,bj)))
     &   +op5*(_maskW(i+1,j,k-1,bi,bj)
     &         *_recip_dxC(i+1,j,bi,bj)*
     &           (Tracer(i+1,j,k-1,bi,bj)-Tracer(i,j,k-1,bi,bj))
     &        +_maskW(i,j,k-1,bi,bj)
     &         *_recip_dxC(i,j,bi,bj)*
     &           (Tracer(i,j,k-1,bi,bj)-Tracer(i-1,j,k-1,bi,bj)))
     &       )

        dTdy(i,j) = op5*(
     &   +op5*(_maskS(i,j,k,bi,bj)
     &         *_recip_dyC(i,j,bi,bj)*
     &           (Tracer(i,j,k,bi,bj)-Tracer(i,j-1,k,bi,bj))
     &        +_maskS(i,j+1,k,bi,bj)
     &         *_recip_dyC(i,j+1,bi,bj)*
     &           (Tracer(i,j+1,k,bi,bj)-Tracer(i,j,k,bi,bj)))
     &   +op5*(_maskS(i,j,k-1,bi,bj)
     &         *_recip_dyC(i,j,bi,bj)*
     &           (Tracer(i,j,k-1,bi,bj)-Tracer(i,j-1,k-1,bi,bj))
     &        +_maskS(i,j+1,k-1,bi,bj)
     &         *_recip_dyC(i,j+1,bi,bj)*
     &           (Tracer(i,j+1,k-1,bi,bj)-Tracer(i,j,k-1,bi,bj)))
     &       )
       ENDDO
      ENDDO
 
#ifdef GM_AUTODIFF_EXCESSIVE_STORE
CADJ STORE dTdx(:,:) = 
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
CADJ STORE dTdy(:,:) = 
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
#endif

C-      Off-diagonal components of vertical flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = df(i,j)
     &      - _rA(i,j,bi,bj)
     &        *( Kwx(i,j,k,bi,bj)*dTdx(i,j)+Kwy(i,j,k,bi,bj)*dTdy(i,j) )

       ENDDO
      ENDDO

#ifdef GM_BOLUS_ADVEC
      IF (GM_AdvForm .AND. GM_AdvSeparate
     & .AND. .NOT.GM_InMomAsStress) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         rTrans(i,j) = 
     &      dyG(i+1,j,bi,bj)*GM_PsiX(i+1,j,k,bi,bj)
     &     -dyG( i ,j,bi,bj)*GM_PsiX( i ,j,k,bi,bj)
     &     +dxG(i,j+1,bi,bj)*GM_PsiY(i,j+1,k,bi,bj)
     &     -dxG(i, j ,bi,bj)*GM_PsiY(i, j ,k,bi,bj)
        ENDDO
       ENDDO
#ifdef GM_AUTODIFF_EXCESSIVE_STORE
CADJ STORE rtrans(:,:) = 
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
#endif
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = df(i,j)
     &    +rTrans(i,j)*op5
     &                *(Tracer(i,j,k,bi,bj)+Tracer(i,j,k-1,bi,bj))
        ENDDO
       ENDDO
      ENDIF
#endif /* GM_BOLUS_ADVEC */  

c     IF (.NOT.implicitDiffusion) THEN
c
c This vertical diffusion term is currently implemented
c by adding the VisbeckK*Kwz diffusivity to KappaRT/S
c See calc_diffusivity.F and calc_gt.F (calc_gs.F)
c
c      DO j=jMin,jMax
c       DO i=iMin,iMax
c        df(i,j) = df(i,j) - _rA(i,j,bi,bj)
c    &    *maskUp(i,j)*VisbeckK(i,j,bi,bj)*Kwz(i,j,k,bi,bj)
c    &    *recip_drC(k)*rkfac
c    &    *(Tracer(i,j,k-1,bi,bj)-Tracer(i,j,k,bi,bj))
c       ENDDO
c      ENDDO
c     ENDIF

      ENDIF
#endif /* ALLOW_GMREDI */

      RETURN
      END
