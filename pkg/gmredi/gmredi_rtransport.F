C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_rtransport.F,v 1.2 2000/11/13 16:35:28 heimbach Exp $

#include "GMREDI_OPTIONS.h"

      subroutine GMREDI_RTRANSPORT(
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     maskUp,Tracer,
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

C     == Routine arguments ==
C     iMin,iMax,jMin,  - Range of points for which calculation
C     jMax,bi,bj,k       results will be set.
C     xA               - Area of X face
C     Tracer           - 3D Tracer field
C     df               - Diffusive flux component work array.
      INTEGER iMin,iMax,jMin,jMax,bi,bj,k
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

#ifdef ALLOW_GMREDI

C     == Local variables ==
C     I, J - Loop counters
      INTEGER I, J
      _RL dTdx,dTdy

C     Surface flux is zero
      IF (useGMRedi .AND. K.GT.1) THEN

      DO j=jMin,jMax
       DO i=iMin,iMax

C-      Horizontal gradients interpolated to W points
        dTdx = 0.5*(
     &   +0.5*(_maskW(i+1,j,k,bi,bj)
     &         *_recip_dxC(i+1,j,bi,bj)*
     &           (Tracer(i+1,j,k,bi,bj)-Tracer(i,j,k,bi,bj))
     &        +_maskW(i,j,k,bi,bj)
     &         *_recip_dxC(i,j,bi,bj)*
     &           (Tracer(i,j,k,bi,bj)-Tracer(i-1,j,k,bi,bj)))
     &   +0.5*(_maskW(i+1,j,k-1,bi,bj)
     &         *_recip_dxC(i+1,j,bi,bj)*
     &           (Tracer(i+1,j,k-1,bi,bj)-Tracer(i,j,k-1,bi,bj))
     &        +_maskW(i,j,k-1,bi,bj)
     &         *_recip_dxC(i,j,bi,bj)*
     &           (Tracer(i,j,k-1,bi,bj)-Tracer(i-1,j,k-1,bi,bj)))
     &       )

        dTdy = 0.5*(
     &   +0.5*(_maskS(i,j,k,bi,bj)
     &         *_recip_dyC(i,j,bi,bj)*
     &           (Tracer(i,j,k,bi,bj)-Tracer(i,j-1,k,bi,bj))
     &        +_maskS(i,j+1,k,bi,bj)
     &         *_recip_dyC(i,j+1,bi,bj)*
     &           (Tracer(i,j+1,k,bi,bj)-Tracer(i,j,k,bi,bj)))
     &   +0.5*(_maskS(i,j,k-1,bi,bj)
     &         *_recip_dyC(i,j,bi,bj)*
     &           (Tracer(i,j,k-1,bi,bj)-Tracer(i,j-1,k-1,bi,bj))
     &        +_maskS(i,j+1,k-1,bi,bj)
     &         *_recip_dyC(i,j+1,bi,bj)*
     &           (Tracer(i,j+1,k-1,bi,bj)-Tracer(i,j,k-1,bi,bj)))
     &       )

C-      Off-diagonal components of vertical flux
        df(i,j) = df(i,j)
     &      - _rA(i,j,bi,bj)
     &      *(GM_background_K
#ifdef GM_VISBECK_VARIABLE_K
     &      +VisbeckK(i,j,myThid)
#endif
     &      )*( Kwx(i,j,k,myThid)*dTdx  +Kwy(i,j,k,myThid)*dTdy )

       ENDDO
      ENDDO

c     IF (.NOT.implicitDiffusion) THEN
c
c This vertical diffusion term is currently implemented
c by adding the VisbeckK*Kwz diffusivity to KappaRT/S
c See calc_diffusivity.F and calc_gt.F (calc_gs.F)
c
c      DO j=jMin,jMax
c       DO i=iMin,iMax
c        df(i,j) = df(i,j) - _rA(i,j,bi,bj)
c    &    *maskUp(i,j)*VisbeckK(i,j,myThid)*Kwz(i,j,k,myThid)
c    &    *recip_drC(k)*rkfac
c    &    *(Tracer(i,j,k-1,bi,bj)-Tracer(i,j,k,bi,bj))
c       ENDDO
c      ENDDO
c     ENDIF

      ENDIF
#endif /* ALLOW_GMREDI */

      RETURN
      END
