C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_calc_tensor.F,v 1.9 2001/12/16 18:54:49 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CStartOfInterface
      SUBROUTINE GMREDI_CALC_TENSOR(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_CALC_TENSOR                            |
C     | o Calculate tensor elements for GM/Redi tensor.          |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"
#include "GMREDI_DIAGS.h"

C     == Routine arguments ==
C
      _RL sigmaX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaY(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaR(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_GMREDI

C     == Local variables ==
      INTEGER i,j,k,km1,kp1
      _RL SlopeX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDrReal(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeSqr(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL taperFct(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL maskp1, Kgm_tmp

#ifdef GM_VISBECK_VARIABLE_K
      _RS deltaH,zero_rs
      PARAMETER(zero_rs=0.)
      _RL N2,SN
      _RL Ssq
#endif

      DO k=2,Nr
C-- 1rst loop on k : compute Tensor Coeff. at W points.
c      km1 = MAX(1,k-1)

#ifdef ALLOW_AUTODIFF_TAMC
!HPF$ INDEPENDENT
#endif
      DO j=1-Oly+1,sNy+Oly-1
#ifdef ALLOW_AUTODIFF_TAMC
!HPF$ INDEPENDENT
#endif
       DO i=1-Olx+1,sNx+Olx-1

C      Gradient of Sigma at rVel points
        SlopeX(i,j)=0.25*( sigmaX(i+1, j ,k-1) +sigmaX(i,j,k-1)
     &                    +sigmaX(i+1, j , k ) +sigmaX(i,j, k ) )
     &                  *maskC(i,j,k,bi,bj)
        SlopeY(i,j)=0.25*( sigmaY( i ,j+1,k-1) +sigmaY(i,j,k-1)
     &                    +sigmaY( i ,j+1, k ) +sigmaY(i,j, k ) )
     &                  *maskC(i,j,k,bi,bj)
        dSigmaDrReal(i,j)=sigmaR(i,j,k)

       ENDDO
      ENDDO

C     Calculate slopes for use in tensor, taper and/or clip
      CALL GMREDI_SLOPE_LIMIT(
     U             dSigmadRReal,
     I             rF(K),
     U             SlopeX, SlopeY,
     O             SlopeSqr, taperFct,
     I             bi, bj, myThid )

      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1

C       Mask Iso-neutral slopes
        SlopeX(i,j)=SlopeX(i,j)*maskC(i,j,k,bi,bj)
        SlopeY(i,j)=SlopeY(i,j)*maskC(i,j,k,bi,bj)
        SlopeSqr(i,j)=SlopeSqr(i,j)*maskC(i,j,k,bi,bj)
c       Ssq=SlopeX(i,j)*SlopeX(i,j)+SlopeY(i,j)*SlopeY(i,j)

C       Components of Redi/GM tensor 
        Kwx(i,j,k,bi,bj)= SlopeX(i,j)*taperFct(i,j)
        Kwy(i,j,k,bi,bj)= SlopeY(i,j)*taperFct(i,j)
        Kwz(i,j,k,bi,bj)= SlopeSqr(i,j)*taperFct(i,j)

#ifdef GM_VISBECK_VARIABLE_K

C- note (jmc) : moved here since only used in VISBECK_VARIABLE_K 
C           but don't know if *taperFct (or **2 ?) is necessary 
        Ssq=SlopeSqr(i,j)*taperFct(i,j)

C--     Depth average of M^2/N^2 * N

C       Calculate terms for mean Richardson number
C       which is used in the "variable K" parameterisaton.
C       Distance between interface above layer and the integration depth
        deltaH=abs(GM_Visbeck_depth)-abs(rF(k))
C       If positive we limit this to the layer thickness
        deltaH=min(deltaH,drF(k))
C       If negative then we are below the integration level
        deltaH=max(deltaH,zero_rs)
C       Now we convert deltaH to a non-dimensional fraction
        deltaH=deltaH/GM_Visbeck_depth

        IF (K.eq.2) VisbeckK(i,j,bi,bj)=0.
        IF (Ssq.NE.0.) THEN
         N2= -Gravity*recip_Rhonil*dSigmaDrReal(i,j)
         SN=sqrt(Ssq*N2)
         VisbeckK(i,j,bi,bj)=VisbeckK(i,j,bi,bj)+deltaH
     &      *GM_Visbeck_alpha*GM_Visbeck_length*GM_Visbeck_length*SN
        ENDIF

#endif /* GM_VISBECK_VARIABLE_K */

       ENDDO
      ENDDO

C-- end 1rst loop on vertical level index k
      ENDDO


#ifdef GM_VISBECK_VARIABLE_K
      IF ( GM_Visbeck_alpha.NE.0. ) THEN
C-     Limit range that KapGM can take
       DO j=1-Oly+1,sNy+Oly-1
        DO i=1-Olx+1,sNx+Olx-1
         VisbeckK(i,j,bi,bj)=
     &       MIN(VisbeckK(i,j,bi,bj),GM_Visbeck_maxval_K)
#ifdef ALLOW_TIMEAVE
         Visbeck_K_T(i,j,bi,bj)=Visbeck_K_T(i,j,bi,bj)
     &                         +VisbeckK(i,j,bi,bj)*deltaTclock
#endif
        ENDDO
       ENDDO
      ENDIF
#endif /* GM_VISBECK_VARIABLE_K */


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- 2nd loop on k : compute Tensor Coeff. at U,V levels.
      DO k=1,Nr
       kp1 = MIN(Nr,k+1)
       maskp1 = 1. _d 0
       IF (k.GE.Nr) maskp1 = 0. _d 0

C-    express the Tensor in term of Diffusivity (= m**2 / s )
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        Kgm_tmp = GM_isopycK + GM_skewflx*GM_background_K
#ifdef GM_VISBECK_VARIABLE_K
     &          + VisbeckK(i,j,bi,bj)*(1.+GM_skewflx)     
#endif
        Kwx(i,j,k,bi,bj)= Kgm_tmp*Kwx(i,j,k,bi,bj)
        Kwy(i,j,k,bi,bj)= Kgm_tmp*Kwy(i,j,k,bi,bj)
        Kwz(i,j,k,bi,bj)= ( GM_isopycK
#ifdef GM_VISBECK_VARIABLE_K
     &                    + VisbeckK(i,j,bi,bj)
#endif
     &                    )*Kwz(i,j,k,bi,bj)
       ENDDO
      ENDDO

#if ( defined (GM_NON_UNITY_DIAGONAL) || defined (GM_EXTRA_DIAGONAL) )

C     Gradient of Sigma at U points
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        SlopeX(i,j)=sigmaX(i,j,k)
     &          *_maskW(i,j,k,bi,bj)
        SlopeY(i,j)=0.25*( sigmaY(i-1,j+1,k) +sigmaY(i,j+1,k)
     &                    +sigmaY(i-1, j ,k) +sigmaY(i, j ,k) )
     &          *_maskW(i,j,k,bi,bj)
        dSigmaDrReal(i,j)=0.25*( sigmaR(i-1,j, k ) +sigmaR(i,j, k )
     &                  +maskp1*(sigmaR(i-1,j,kp1) +sigmaR(i,j,kp1)) )
     &          *_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Calculate slopes for use in tensor, taper and/or clip
      CALL GMREDI_SLOPE_LIMIT(
     U             dSigmadRReal,
     I             rF(K),
     U             SlopeX, SlopeY,
     O             SlopeSqr, taperFct,
     I             bi, bj, myThid )

#ifdef GM_NON_UNITY_DIAGONAL
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          Kux(i,j,k,bi,bj) =
     &     ( GM_isopycK
#ifdef GM_VISBECK_VARIABLE_K
     &     +0.5*(VisbeckK(i,j,bi,bj)+VisbeckK(i-1,j,bi,bj))
#endif
     &     )*taperFct(i,j)
          Kux(i,j,k,bi,bj) = MAX( Kux(i,j,k,bi,bj), GM_Kmin_horiz )
         ENDDO
        ENDDO
#endif /* GM_NON_UNITY_DIAGONAL */

#ifdef GM_EXTRA_DIAGONAL
      IF (GM_ExtraDiag) THEN
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          Kuz(i,j,k,bi,bj) =
     &     ( GM_isopycK - GM_skewflx*GM_background_K
#ifdef GM_VISBECK_VARIABLE_K
     &     +0.5*(VisbeckK(i,j,bi,bj)+VisbeckK(i-1,j,bi,bj))*GM_advect
#endif
     &     )*SlopeX(i,j)*taperFct(i,j)
         ENDDO
        ENDDO
      ENDIF
#endif /* GM_EXTRA_DIAGONAL */

C     Gradient of Sigma at V points
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        SlopeX(i,j)=0.25*( sigmaX(i, j ,k) +sigmaX(i+1, j ,k)
     &                    +sigmaX(i,j-1,k) +sigmaX(i+1,j-1,k) )
     &          *_maskS(i,j,k,bi,bj)
        SlopeY(i,j)=sigmaY(i,j,k)
     &          *_maskS(i,j,k,bi,bj)
        dSigmaDrReal(i,j)=0.25*( sigmaR(i,j-1, k ) +sigmaR(i,j, k )
     &                  +maskp1*(sigmaR(i,j-1,kp1) +sigmaR(i,j,kp1)) )
     &          *_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Calculate slopes for use in tensor, taper and/or clip
      CALL GMREDI_SLOPE_LIMIT(
     U             dSigmadRReal,
     I             rF(K),
     U             SlopeX, SlopeY,
     O             SlopeSqr, taperFct,
     I             bi, bj, myThid )

#ifdef GM_NON_UNITY_DIAGONAL
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          Kvy(i,j,k,bi,bj) =
     &     ( GM_isopycK
#ifdef GM_VISBECK_VARIABLE_K
     &     +0.5*(VisbeckK(i,j,bi,bj)+VisbeckK(i,j-1,bi,bj))
#endif
     &     )*taperFct(i,j)
          Kvy(i,j,k,bi,bj) = MAX( Kvy(i,j,k,bi,bj), GM_Kmin_horiz )
         ENDDO
        ENDDO
#endif /* GM_NON_UNITY_DIAGONAL */

#ifdef GM_EXTRA_DIAGONAL
      IF (GM_ExtraDiag) THEN
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          Kvz(i,j,k,bi,bj) =
     &     ( GM_isopycK - GM_skewflx*GM_background_K
#ifdef GM_VISBECK_VARIABLE_K
     &     +0.5*(VisbeckK(i,j,bi,bj)+VisbeckK(i,j-1,bi,bj))*GM_advect
#endif
     &     )*SlopeY(i,j)*taperFct(i,j)
         ENDDO
        ENDDO
      ENDIF
#endif /* GM_EXTRA_DIAGONAL */

#endif /* GM_NON_UNITY_DIAGONAL || GM_EXTRA_DIAGONAL */

#ifdef ALLOW_TIMEAVE
C--   Time-average
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        GM_Kwx_T(i,j,k,bi,bj)=GM_Kwx_T(i,j,k,bi,bj)
     &                       +Kwx(i,j,k,bi,bj)*deltaTclock
        GM_Kwy_T(i,j,k,bi,bj)=GM_Kwy_T(i,j,k,bi,bj)
     &                       +Kwy(i,j,k,bi,bj)*deltaTclock
        GM_Kwz_T(i,j,k,bi,bj)=GM_Kwz_T(i,j,k,bi,bj)
     &                       +Kwz(i,j,k,bi,bj)*deltaTclock
       ENDDO
      ENDDO
      GM_TimeAve(k,bi,bj)=GM_TimeAve(k,bi,bj)+deltaTclock
#endif /* ALLOW_TIMEAVE */

C-- end 2nd loop on vertical level index k
      ENDDO


#ifdef GM_BOLUS_ADVEC
      IF (GM_AdvForm) THEN
        CALL GMREDI_CALC_PSI_B(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             myThid ) 
      ENDIF
#endif

#endif /* ALLOW_GMREDI */

      RETURN
      END


      SUBROUTINE GMREDI_CALC_TENSOR_DUMMY(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_CALC_TENSOR                            |
C     | o Calculate tensor elements for GM/Redi tensor.          |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"

C     == Routine arguments ==
C
      _RL sigmaX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaY(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaR(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid
CEndOfInterface

      INTEGER i, j, k

#ifdef ALLOW_GMREDI

      DO k=1,Nr
       DO j=1-Oly+1,sNy+Oly-1
        DO i=1-Olx+1,sNx+Olx-1
         Kwx(i,j,k,bi,bj) = 0.0
         Kwy(i,j,k,bi,bj) = 0.0
         Kwz(i,j,k,bi,bj) = 0.0
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_GMREDI */

      RETURN
      END
