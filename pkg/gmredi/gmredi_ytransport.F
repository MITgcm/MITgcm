C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_ytransport.F,v 1.5 2001/12/16 18:54:49 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

      subroutine GMREDI_YTRANSPORT(
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     yA,Tracer,
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

C     == Routine arguments ==
C     iMin,iMax,jMin,  - Range of points for which calculation
C     jMax,bi,bj,k       results will be set.
C     xA               - Area of X face
C     Tracer           - 3D Tracer field
C     df               - Diffusive flux component work array.
      INTEGER iMin,iMax,jMin,jMax,bi,bj,k
      _RS yA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

#ifdef ALLOW_GMREDI

C     == Local variables ==
C     I, J - Loop counters
      INTEGER I, J
      INTEGER km1,kp1 
      _RL vTrans, maskp1, dTdz

      IF (useGMRedi) THEN

C--   Area integrated meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = df(i,j)
     &   -yA(i,j)
#ifdef GM_NON_UNITY_DIAGONAL
     &    *Kvy(i,j,k,bi,bj)
#else
     &    *(GM_isopycK
#ifdef GM_VISBECK_VARIABLE_K
     &     +0.5*(VisbeckK(i,j,bi,bj)+VisbeckK(i,j-1,bi,bj))
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
        dTdz =  0.5*(
     &   +0.5*recip_drC(k)*
     &       ( maskC(i,j-1,k,bi,bj)*
     &           (Tracer(i,j-1,km1,bi,bj)-Tracer(i,j-1,k,bi,bj))
     &        +maskC(i, j ,k,bi,bj)*
     &           (Tracer(i, j ,km1,bi,bj)-Tracer(i, j ,k,bi,bj))
     &       )
     &   +0.5*recip_drC(kp1)*
     &       ( maskC(i,j-1,kp1,bi,bj)*
     &           (Tracer(i,j-1,k,bi,bj)-Tracer(i,j-1,kp1,bi,bj))
     &        +maskC(i, j ,kp1,bi,bj)*
     &           (Tracer(i, j ,k,bi,bj)-Tracer(i, j ,kp1,bi,bj))
     &       )      )
 
C-      Off-diagonal components of horizontal flux
          df(i,j) = df(i,j) - yA(i,j)*Kvz(i,j,k,bi,bj)*dTdz               

        ENDDO
       ENDDO
      ENDIF
#endif /* GM_EXTRA_DIAGONAL */   

#ifdef GM_BOLUS_ADVEC
      IF (GM_AdvForm) THEN
       kp1 = MIN(k+1,Nr)
       maskp1 = 1.
       IF (k.GE.Nr) maskp1 = 0.
       DO j=jMin,jMax
        DO i=iMin,iMax
         vTrans = dxG(i,j,bi,bj)*( GM_PsiY(i,j,kp1,bi,bj)*maskp1
     &                            -GM_PsiY(i,j,k,bi,bj) )
     &                          *maskS(i,j,k,bi,bj)
         df(i,j) = df(i,j)
     &    +vTrans*0.5*(Tracer(i,j,k,bi,bj)+Tracer(i,j-1,k,bi,bj))
        ENDDO
       ENDDO
      ENDIF
#endif /* GM_BOLUS_ADVEC */

      ENDIF
#endif /* ALLOW_GMREDI */

      RETURN
      END
