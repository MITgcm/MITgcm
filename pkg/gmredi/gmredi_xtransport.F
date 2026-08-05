#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: GMREDI_XTRANSPORT
C     !INTERFACE:
      SUBROUTINE GMREDI_XTRANSPORT(
     I     trIdentity, bi, bj, k,
     I     iMin, iMax, jMin, jMax,
     I     xA, maskFk, Tracer,
     U     df,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE GMREDI_XTRANSPORT
C     |   Add horizontal x transport terms from GM/Redi
C     |   parameterization.
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
C     trIdentity :: tracer Id number
C     bi, bj     :: current tile indices
C     k          :: current level index
C     iMin,iMax  :: Range of 1rst index where results will be set
C     jMin,jMax  :: Range of 2nd  index where results will be set
C     xA         :: Area of X face
C     maskFk     :: 2-D mask for vertical interface k (between level k-1 & k)
C     Tracer     :: 3D Tracer field
C     df         :: Diffusive flux component work array.
C     myThid     :: my Thread Id number
      INTEGER trIdentity
      INTEGER bi, bj, k
      INTEGER iMin, iMax, jMin, jMax
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskFk(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
C     !LOCAL VARIABLES:
C     i, j       :: Loop counters
      INTEGER i, j
#if ( defined (GM_EXTRA_DIAGONAL) || defined (GM_BOLUS_ADVEC) )
      INTEGER kp1
      _RL maskp1
#endif
#ifdef GM_EXTRA_DIAGONAL
      INTEGER km1
      _RL dTdz  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
#ifdef GM_BOLUS_ADVEC
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      _RL tmp1k(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
#endif /* GM_BOLUS_ADVEC */
#ifdef ALLOW_AUTODIFF_TAMC
C     kkey :: tape key (depends on levels and tiles)
      INTEGER kkey
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif

#ifdef ALLOW_AUTODIFF_TAMC
      kkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
      kkey = trIdentity + (kkey-1)*maxpass
      kkey = k + (kkey-1)*Nr
      IF (trIdentity.GT.maxpass) THEN
       WRITE(msgBuf,'(A,2I5)')
     &      'GMREDI_XTRANSPORT: trIdentity > maxpass ',
     &      trIdentity, maxpass
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R GMREDI_XTRANSPORT'
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

      IF (useGMRedi) THEN

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE Kux(:,:,k,bi,bj) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
# ifdef GM_EXTRA_DIAGONAL
CADJ STORE Kuz(:,:,k,bi,bj) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Area integrated zonal flux
#ifdef ALLOW_LONGSTEP
      IF ( trIdentity .GE. GAD_TR1 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = df(i,j)
     &    -xA(i,j)
     &     *LS_Kux(i,j,k,bi,bj)
     &     *_recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
     &     *( Tracer(i,j,k) - Tracer(i-1,j,k) )
        ENDDO
       ENDDO
      ELSE
#endif /* ALLOW_LONGSTEP */
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = df(i,j)
     &    -xA(i,j)
     &     *Kux(i,j,k,bi,bj)
     &     *_recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
     &     *( Tracer(i,j,k) - Tracer(i-1,j,k) )
        ENDDO
       ENDDO
#ifdef ALLOW_LONGSTEP
      ENDIF
#endif /* ALLOW_LONGSTEP */

#ifdef GM_EXTRA_DIAGONAL
      IF ( GM_ExtraDiag ) THEN
       km1 = MAX(k-1,1)
       kp1 = MIN(k+1,Nr)
C-    Vertical gradients interpolated to U points
C     Note: no real needs for masking out with maskp1 when k=Nr since the Tracer
C           difference is already zero (k=kp1 in this case); but cleaner for
C           AD-code to ensure exactly zero contribution to Tracer AD var.
       maskp1 = 1. _d 0
       IF (k.GE.Nr) maskp1 = 0. _d 0
       DO j=jMin,jMax
        DO i=iMin,iMax
         dTdz(i,j) = op5*(
     &    +op5*recip_drC(k)*
     &        ( maskFk(i-1,j)*
     &            ( Tracer(i-1,j,km1)-Tracer(i-1,j,k) )
     &         +maskFk( i ,j)*
     &            ( Tracer( i ,j,km1)-Tracer( i ,j,k) )
     &        )
     &    +op5*recip_drC(kp1)*
     &        ( maskC(i-1,j,k,bi,bj)*maskC(i-1,j,kp1,bi,bj)*maskp1*
     &            ( Tracer(i-1,j,k)-Tracer(i-1,j,kp1) )
     &         +maskC( i ,j,k,bi,bj)*maskC( i ,j,kp1,bi,bj)*maskp1*
     &            ( Tracer( i ,j,k)-Tracer( i ,j,kp1) )
     &        )          )
        ENDDO
       ENDDO
#ifdef GM_AUTODIFF_EXCESSIVE_STORE
CADJ STORE dTdz(:,:) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
#endif
C-    Off-diagonal components of horizontal flux
#ifdef ALLOW_LONGSTEP
       IF ( trIdentity .GE. GAD_TR1 ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          df(i,j) = df(i,j) - xA(i,j)*LS_Kuz(i,j,k,bi,bj)*dTdz(i,j)
         ENDDO
        ENDDO
       ELSE
#endif /* ALLOW_LONGSTEP */
        DO j=jMin,jMax
         DO i=iMin,iMax
          df(i,j) = df(i,j) - xA(i,j)*Kuz(i,j,k,bi,bj)*dTdz(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_LONGSTEP
       ENDIF
#endif /* ALLOW_LONGSTEP */
      ENDIF
#endif /* GM_EXTRA_DIAGONAL */

#ifdef GM_BOLUS_ADVEC
      IF ( GM_AdvForm .AND. GM_AdvSeparate
     &                .AND. .NOT.GM_InMomAsStress ) THEN
C      Since bolus transport is computed as curl of stream-function, needs to
C      flip sign when using Left-Handed Coordinate system such as P-coordinate
c      flipSign4LHCoord = -gravitySign
       kp1 = MIN(k+1,Nr)
       maskp1 = 1. _d 0
       IF (k.GE.Nr) maskp1 = 0. _d 0
#ifdef ALLOW_LONGSTEP
       IF ( trIdentity .GE. GAD_TR1 ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          uTrans(i,j) = -gravitySign*dyG(i,j,bi,bj)
     &       *( LS_PsiX(i,j,kp1,bi,bj)*deepFacF(kp1)*maskp1
     &        - LS_PsiX(i,j, k ,bi,bj)*deepFacF(k)
     &        )*maskW(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ELSE
#endif /* ALLOW_LONGSTEP */
        DO j=jMin,jMax
         DO i=iMin,iMax
          uTrans(i,j) = -gravitySign*dyG(i,j,bi,bj)
     &       *( GM_PsiX(i,j,kp1,bi,bj)*deepFacF(kp1)*maskp1
     &        - GM_PsiX(i,j, k ,bi,bj)*deepFacF(k)
     &        )*maskW(i,j,k,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_LONGSTEP
       ENDIF
#endif /* ALLOW_LONGSTEP */
#ifdef GM_AUTODIFF_EXCESSIVE_STORE
CADJ STORE uTrans(:,:) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
#endif
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = df(i,j)
     &    + uTrans(i,j)*op5*( Tracer(i,j,k) + Tracer(i-1,j,k) )
        ENDDO
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND. trIdentity.EQ.1 .AND.
     &     DIAGNOSTICS_IS_ON( 'GM_ubT  ', myThid ) ) THEN
       kp1 = MIN(k+1,Nr)
       maskp1 = 1. _d 0
       IF (k.GE.Nr) maskp1 = 0. _d 0
       DO j=jMin,jMax
        DO i=iMin,iMax
         tmp1k(i,j) = -gravitySign*dyG(i,j,bi,bj)
     &      *( GM_PsiX(i,j,kp1,bi,bj)*deepFacF(kp1)*maskp1
     &       - GM_PsiX(i,j, k ,bi,bj)*deepFacF(k)
     &       )*maskW(i,j,k,bi,bj)
     &        *op5*( Tracer(i,j,k) + Tracer(i-1,j,k) )
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmp1k,'GM_ubT  ', k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* GM_BOLUS_ADVEC */

      ENDIF
#endif /* ALLOW_GMREDI */

      RETURN
      END
