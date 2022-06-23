#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: GMREDI_YTRANSPORT
C     !INTERFACE:
      SUBROUTINE GMREDI_YTRANSPORT(
     I     trIdentity, bi, bj, k,
     I     iMin, iMax, jMin, jMax,
     I     yA, maskFk, Tracer,
     U     df,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE GMREDI_YTRANSPORT
C     |   Add horizontal y transport terms from GM/Redi
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
C     yA         :: Area of Y face
C     maskFk     :: 2-D mask for vertical interface k (between level k-1 & k)
C     Tracer     :: 3D Tracer field
C     df         :: Diffusive flux component work array.
C     myThid     :: my Thread Id number
      INTEGER trIdentity
      INTEGER bi, bj, k
      INTEGER iMin, iMax, jMin, jMax
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
      _RL dTdz(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
#ifdef GM_BOLUS_ADVEC
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      _RL tmp1k(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
#endif /* GM_BOLUS_ADVEC */
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act0, act1, act2, act3, act4
      INTEGER max0, max1, max2, max3
      INTEGER igadkey, kkey
#endif

#ifdef ALLOW_AUTODIFF_TAMC
      act0 = trIdentity - 1
      max0 = maxpass
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      igadkey = (act0 + 1) + act1*max0
     &                     + act2*max0*max1
     &                     + act3*max0*max1*max2
     &                     + act4*max0*max1*max2*max3
      kkey = (igadkey-1)*Nr + k
      IF (trIdentity.GT.maxpass) THEN
       PRINT *, 'ph-pass gmredi_ytrans ', maxpass, trIdentity
       STOP 'maxpass seems smaller than trIdentity'
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

      IF (useGMRedi) THEN

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE Kvy(:,:,k,bi,bj) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
# ifdef GM_EXTRA_DIAGONAL
CADJ STORE Kvz(:,:,k,bi,bj) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Area integrated meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = df(i,j)
     &   -yA(i,j)
     &    *Kvy(i,j,k,bi,bj)
     &    *_recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
     &    *( Tracer(i,j,k) - Tracer(i,j-1,k) )
       ENDDO
      ENDDO

#ifdef GM_EXTRA_DIAGONAL
      IF ( GM_ExtraDiag ) THEN
       km1 = MAX(k-1,1)
       kp1 = MIN(k+1,Nr)
C-    Vertical gradients interpolated to V points
C     Note: no real needs for masking out with maskp1 when k=Nr since the Tracer
C           difference is already zero (k=kp1 in this case); but cleaner for
C           AD-code to ensure exactly zero contribution to Tracer AD var.
       maskp1 = 1. _d 0
       IF (k.GE.Nr) maskp1 = 0. _d 0
       DO j=jMin,jMax
        DO i=iMin,iMax
         dTdz(i,j) = op5*(
     &    +op5*recip_drC(k)*
     &        ( maskFk(i,j-1)*
     &            ( Tracer(i,j-1,km1)-Tracer(i,j-1,k) )
     &         +maskFk(i, j )*
     &            ( Tracer(i, j ,km1)-Tracer(i, j ,k) )
     &        )
     &    +op5*recip_drC(kp1)*
     &        ( maskC(i,j-1,k,bi,bj)*maskC(i,j-1,kp1,bi,bj)*maskp1*
     &            ( Tracer(i,j-1,k)-Tracer(i,j-1,kp1) )
     &         +maskC(i, j ,k,bi,bj)*maskC(i, j ,kp1,bi,bj)*maskp1*
     &            ( Tracer(i, j ,k)-Tracer(i, j ,kp1) )
     &        )          )
        ENDDO
       ENDDO
#ifdef GM_AUTODIFF_EXCESSIVE_STORE
CADJ STORE dTdz(:,:) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
#endif
C-    Off-diagonal components of horizontal flux
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = df(i,j) - yA(i,j)*Kvz(i,j,k,bi,bj)*dTdz(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif /* GM_EXTRA_DIAGONAL */

#ifdef GM_BOLUS_ADVEC
      IF ( GM_AdvForm .AND. GM_AdvSeparate
     &                .AND. .NOT.GM_InMomAsStress ) THEN
       kp1 = MIN(k+1,Nr)
       maskp1 = 1. _d 0
       IF (k.GE.Nr) maskp1 = 0. _d 0
       DO j=jMin,jMax
        DO i=iMin,iMax
         vTrans(i,j) = dxG(i,j,bi,bj)
     &      *( GM_PsiY(i,j,kp1,bi,bj)*deepFacF(kp1)*maskp1
     &       - GM_PsiY(i,j, k, bi,bj)*deepFacF(k)
     &       )*maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
#ifdef GM_AUTODIFF_EXCESSIVE_STORE
CADJ STORE vTrans(:,:) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
#endif
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = df(i,j)
     &    + vTrans(i,j)*op5*( Tracer(i,j,k) + Tracer(i,j-1,k) )
        ENDDO
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND. trIdentity.EQ.1 .AND.
     &     DIAGNOSTICS_IS_ON( 'GM_vbT  ', myThid ) ) THEN
       kp1 = MIN(k+1,Nr)
       maskp1 = 1. _d 0
       IF (k.GE.Nr) maskp1 = 0. _d 0
       DO j=jMin,jMax
        DO i=iMin,iMax
         tmp1k(i,j) = dxG(i,j,bi,bj)
     &      *( GM_PsiY(i,j,kp1,bi,bj)*deepFacF(kp1)*maskp1
     &       - GM_PsiY(i,j, k, bi,bj)*deepFacF(k)
     &       )*maskS(i,j,k,bi,bj)
     &        *op5*( Tracer(i,j,k) + Tracer(i,j-1,k) )
        ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmp1k,'GM_vbT  ', k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* GM_BOLUS_ADVEC */

      ENDIF
#endif /* ALLOW_GMREDI */

      RETURN
      END
