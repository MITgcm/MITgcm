C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_ytransport.F,v 1.16 2008/02/02 02:35:53 gforget Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

      subroutine GMREDI_YTRANSPORT(
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     yA,Tracer,tracerIdentity,
     U     df,
     I     myThid)
C     /==========================================================\
C     | o SUBROUTINE GMREDI_YTRANSPORT                           |
C     |   Add horizontal y transport terms from GM/Redi          |
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
      _RS yA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      integer tracerIdentity
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

#ifdef ALLOW_GMREDI

C     == Local variables ==
C     I, J - Loop counters
      INTEGER I, J
      INTEGER km1,kp1
      _RL maskp1
#ifdef GM_EXTRA_DIAGONAL
      _RL dTdz(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
#ifdef GM_BOLUS_ADVEC
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      _RL tmp1k(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
             print *, 'ph-pass gmredi_ytrans ', maxpass, tracerIdentity
             STOP 'maxpass seems smaller than tracerIdentity'
          endif
#endif /* ALLOW_AUTODIFF_TAMC */

      IF (useGMRedi) THEN
 
#ifdef ALLOW_AUTODIFF_TAMC
# ifdef GM_NON_UNITY_DIAGONAL
CADJ STORE Kvy(:,:,k,bi,bj) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
# endif
# ifdef GM_EXTRA_DIAGONAL
CADJ STORE Kvz(:,:,k,bi,bj) =
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
# endif
#endif

C--   Area integrated meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = df(i,j)
     &   -yA(i,j)
#ifdef GM_NON_UNITY_DIAGONAL
     &    *Kvy(i,j,k,bi,bj)
#else
#ifdef ALLOW_KAPREDI_CONTROL
     &    *(kapredi(i,j,k,bi,bj)
#else
     &    *(GM_isopycK
#endif
#ifdef GM_VISBECK_VARIABLE_K
     &     +op5*(VisbeckK(i,j,bi,bj)+VisbeckK(i,j-1,bi,bj))
#endif
     &     )
#endif /* GM_NON_UNITY_DIAGONAL */
     &    *_recip_dyC(i,j,bi,bj)
     &    *(Tracer(i,j,k,bi,bj)-Tracer(i,j-1,k,bi,bj))
       ENDDO
      ENDDO

#ifdef GM_EXTRA_DIAGONAL
      IF (GM_ExtraDiag) THEN
       km1 = MAX(k-1,1)
       kp1 = MIN(k+1,Nr)
 
       DO j=jMin,jMax
        DO i=iMin,iMax
 
C-      Vertical gradients interpolated to V points
        dTdz(i,j) =  op5*(
     &   +op5*recip_drC(k)*
     &       ( maskC(i,j-1,k,bi,bj)*
     &           (Tracer(i,j-1,km1,bi,bj)-Tracer(i,j-1,k,bi,bj))
     &        +maskC(i, j ,k,bi,bj)*
     &           (Tracer(i, j ,km1,bi,bj)-Tracer(i, j ,k,bi,bj))
     &       )
     &   +op5*recip_drC(kp1)*
     &       ( maskC(i,j-1,kp1,bi,bj)*
     &           (Tracer(i,j-1,k,bi,bj)-Tracer(i,j-1,kp1,bi,bj))
     &        +maskC(i, j ,kp1,bi,bj)*
     &           (Tracer(i, j ,k,bi,bj)-Tracer(i, j ,kp1,bi,bj))
     &       )      )
        ENDDO
       ENDDO
#ifdef GM_AUTODIFF_EXCESSIVE_STORE
CADJ STORE dtdz(:,:) = 
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
#endif
       DO j=jMin,jMax
        DO i=iMin,iMax 
C-      Off-diagonal components of horizontal flux
          df(i,j) = df(i,j) - yA(i,j)*Kvz(i,j,k,bi,bj)*dTdz(i,j)

        ENDDO
       ENDDO
      ENDIF
#endif /* GM_EXTRA_DIAGONAL */   

#ifdef GM_BOLUS_ADVEC
      IF (GM_AdvForm .AND. GM_AdvSeparate) THEN
       kp1 = MIN(k+1,Nr)
       maskp1 = 1.
       IF (k.GE.Nr) maskp1 = 0.
       DO j=jMin,jMax
        DO i=iMin,iMax
         vTrans(i,j) = dxG(i,j,bi,bj)*( GM_PsiY(i,j,kp1,bi,bj)*maskp1
     &                                 -GM_PsiY(i,j,k,bi,bj) )
     &                               *maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
#ifdef GM_AUTODIFF_EXCESSIVE_STORE
CADJ STORE vtrans(:,:) = 
CADJ &     comlev1_gmredi_k_gad, key=kkey, byte=isbyte
#endif
       DO j=jMin,jMax
        DO i=iMin,iMax
         df(i,j) = df(i,j)
     &    +vTrans(i,j)*op5*(Tracer(i,j,k,bi,bj)+Tracer(i,j-1,k,bi,bj))
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( useDiagnostics
     &     .AND. DIAGNOSTICS_IS_ON('GM_vbT  ', myThid )
     &     .AND. tracerIdentity .EQ. 1) THEN
       kp1 = MIN(k+1,Nr)
       maskp1 = 1.
       IF (k.GE.Nr) maskp1 = 0.
       DO j=jMin,jMax 
        DO i=iMin,iMax
         tmp1k(i,j) = dxG(i,j,bi,bj)*( GM_PsiY(i,j,kp1,bi,bj)*maskp1
     &                                -GM_PsiY(i,j,k,bi,bj) )
     &                               *maskS(i,j,k,bi,bj)
     &               *op5*(Tracer(i,j,k,bi,bj)+Tracer(i,j-1,k,bi,bj))
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
