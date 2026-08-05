#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C     !ROUTINE: GMREDI_RTRANSPORT
C     !INTERFACE:
      SUBROUTINE GMREDI_RTRANSPORT(
     I     trIdentity, bi, bj, k,
     I     iMin, iMax, jMin, jMax,
     I     maskUp, Tracer,
     U     df,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE GMREDI_RTRANSPORT                           |
C     |   Add vertical transport terms from GM/Redi              |
C     |   parameterization.                                      |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"
#include "GAD.h"
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     trIdentity   :: tracer Id number
C     bi, bj       :: current tile indices
C     k            :: current level index
C     iMin,iMax    :: Range of 1rst index where results will be set
C     jMin,jMax    :: Range of 2nd  index where results will be set
C     maskUp       :: 2-D array for mask at W points
C     Tracer       :: 3D Tracer field
C     df           :: Diffusive flux component work array.
C     myThid       :: my Thread Id number
      INTEGER trIdentity
      INTEGER bi, bj, k
      INTEGER iMin, iMax, jMin, jMax
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
C     !LOCAL VARIABLES:
C     i, j    ::  Loop counters
      INTEGER i, j
#ifdef ALLOW_AUTODIFF_TAMC
C     kkey :: tape key (depends on levels, tiles, and tracers)
      INTEGER kkey
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
      _RL dTdx  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dTdy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef GM_BOLUS_ADVEC
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
      kkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
      kkey = trIdentity + (kkey-1)*maxpass
      kkey = k + (kkey-1)*Nr
      IF (trIdentity.GT.maxpass) THEN
       WRITE(msgBuf,'(A,2I5)')
     &      'GMREDI_RTRANSPORT: trIdentity > maxpass ',
     &      trIdentity, maxpass
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GMREDI_RTRANSPORT'
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

C     Surface flux is zero
      IF ( useGMRedi .AND. k.GT.1 ) THEN

C-    Horizontal gradients interpolated to W points
      DO j=jMin,jMax
       DO i=iMin,iMax
        dTdx(i,j) = op5*(
     &    op5*recip_deepFacC(k)
     &       *( _maskW(i+1,j,k,bi,bj)*_recip_dxC(i+1,j,bi,bj)
     &             *( Tracer(i+1,j,k) - Tracer( i ,j,k) )
     &        + _maskW( i ,j,k,bi,bj) *_recip_dxC(i,j,bi,bj)
     &             *( Tracer( i ,j,k) - Tracer(i-1,j,k) )
     &        )
     &   +op5*recip_deepFacC(k-1)
     &       *( _maskW(i+1,j,k-1,bi,bj)*_recip_dxC(i+1,j,bi,bj)
     &             *( Tracer(i+1,j,k-1) - Tracer( i ,j,k-1) )
     &         +_maskW( i ,j,k-1,bi,bj)*_recip_dxC( i ,j,bi,bj)
     &             *( Tracer( i ,j,k-1) - Tracer(i-1,j,k-1) )
     &        )          )

        dTdy(i,j) = op5*(
     &    op5*recip_deepFacC(k)
     &       *( _maskS(i,j+1,k,bi,bj)*_recip_dyC(i,j+1,bi,bj)
     &             *( Tracer(i,j+1,k) - Tracer(i,j,k) )
     &        + _maskS(i, j ,k,bi,bj)*_recip_dyC(i, j ,bi,bj)
     &             *( Tracer(i,j,k) - Tracer(i,j-1,k) )
     &        )
     &   +op5*recip_deepFacC(k-1)
     &       *( _maskS(i,j+1,k-1,bi,bj)*_recip_dyC(i,j+1,bi,bj)
     &             *( Tracer(i,j+1,k-1) - Tracer(i,j,k-1) )
     &        + _maskS(i, j ,k-1,bi,bj)*_recip_dyC(i, j ,bi,bj)
     &             *( Tracer(i,j,k-1) - Tracer(i,j-1,k-1) )
     &        )          )
       ENDDO
      ENDDO

#ifdef GM_AUTODIFF_EXCESSIVE_STORE
CADJ STORE dTdx(:,:) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
CADJ STORE dTdy(:,:) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
#endif

C-    Off-diagonal components of vertical flux
#ifdef ALLOW_LONGSTEP
      IF ( trIdentity .GE. GAD_TR1 ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          df(i,j) = df(i,j)
     &      - _rA(i,j,bi,bj)*deepFac2F(k)*maskInC(i,j,bi,bj)
     &       *( LS_Kwx(i,j,k,bi,bj)*dTdx(i,j)
     &        + LS_Kwy(i,j,k,bi,bj)*dTdy(i,j) )*maskUp(i,j)
         ENDDO
        ENDDO
      ELSE
#endif /* ALLOW_LONGSTEP */
        DO j=jMin,jMax
         DO i=iMin,iMax
          df(i,j) = df(i,j)
     &      - _rA(i,j,bi,bj)*deepFac2F(k)*maskInC(i,j,bi,bj)
     &       *( Kwx(i,j,k,bi,bj)*dTdx(i,j)
     &        + Kwy(i,j,k,bi,bj)*dTdy(i,j) )*maskUp(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_LONGSTEP
      ENDIF
#endif /* ALLOW_LONGSTEP */

#ifdef GM_BOLUS_ADVEC
      IF (GM_AdvForm .AND. GM_AdvSeparate
     & .AND. .NOT.GM_InMomAsStress) THEN
C      Since bolus transport is computed as curl of stream-function, needs to
C      flip sign when using Left-Handed Coordinate system such as P-coordinate
c      flipSign4LHCoord = -gravitySign
#ifdef ALLOW_LONGSTEP
       IF ( trIdentity .GE. GAD_TR1 ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          rTrans(i,j) = -gravitySign*(
     &       dyG(i+1,j,bi,bj)*LS_PsiX(i+1,j,k,bi,bj)
     &      -dyG( i ,j,bi,bj)*LS_PsiX( i ,j,k,bi,bj)
     &      +dxG(i,j+1,bi,bj)*LS_PsiY(i,j+1,k,bi,bj)
     &      -dxG(i, j ,bi,bj)*LS_PsiY(i, j ,k,bi,bj)
     &                               )*deepFacF(k)
         ENDDO
        ENDDO
       ELSE
#endif /* ALLOW_LONGSTEP */
        DO j=jMin,jMax
         DO i=iMin,iMax
          rTrans(i,j) = -gravitySign*(
     &       dyG(i+1,j,bi,bj)*GM_PsiX(i+1,j,k,bi,bj)
     &      -dyG( i ,j,bi,bj)*GM_PsiX( i ,j,k,bi,bj)
     &      +dxG(i,j+1,bi,bj)*GM_PsiY(i,j+1,k,bi,bj)
     &      -dxG(i, j ,bi,bj)*GM_PsiY(i, j ,k,bi,bj)
     &                               )*deepFacF(k)
         ENDDO
        ENDDO
#ifdef ALLOW_LONGSTEP
       ENDIF
#endif /* ALLOW_LONGSTEP */
#ifdef GM_AUTODIFF_EXCESSIVE_STORE
CADJ STORE rtrans(:,:) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
#endif
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = df(i,j)
     &    + rTrans(i,j)*op5
     &                 *( Tracer(i,j,k) + Tracer(i,j,k-1) )
     &                 *maskInC(i,j,bi,bj)*maskUp(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif /* GM_BOLUS_ADVEC */

c     IF (.NOT.implicitDiffusion) THEN
C This vertical diffusion term is currently implemented
C by adding the Kwz diffusivity to KappaRT/S
C See calc_diffusivity.F and calc_gt.F (calc_gs.F)
c     ENDIF

      ENDIF
#endif /* ALLOW_GMREDI */

      RETURN
      END
