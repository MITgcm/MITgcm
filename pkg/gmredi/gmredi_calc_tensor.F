C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_calc_tensor.F,v 1.6 2001/03/06 17:58:21 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CStartOfInterface
      SUBROUTINE GMREDI_CALC_TENSOR(
     I             bi, bj, iMin, iMax, jMin, jMax, K,
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
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_GMREDI

C     == Local variables ==
      INTEGER i,j,km1,kp1
      _RL SlopeX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDrReal(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dRdSigmaLtd(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL Ssq

#ifdef GM_VISBECK_VARIABLE_K
      _RS deltaH,zero_rs
      PARAMETER(zero_rs=0.)
      _RL N2,SN
#endif


      km1=max(1,K-1)
      kp1=min(Nr,K)


#ifdef ALLOW_AUTODIFF_TAMC
!HPF$ INDEPENDENT
#endif
      DO j=1-Oly+1,sNy+Oly-1
#ifdef ALLOW_AUTODIFF_TAMC
!HPF$ INDEPENDENT
#endif
       DO i=1-Olx+1,sNx+Olx-1

C      Gradient of Sigma at rVel points
        SlopeX(i,j)=0.25*( sigmaX(i+1, j ,km1) +sigmaX(i,j,km1)
     &                    +sigmaX(i+1, j , k ) +sigmaX(i,j, k ) )
        SlopeY(i,j)=0.25*( sigmaY( i ,j+1,km1) +sigmaY(i,j,km1)
     &                    +sigmaY( i ,j+1, k ) +sigmaY(i,j, k ) )
        dSigmaDrReal(i,j)=sigmaR(i,j,k)

        if (hFacC(i,j,k,bi,bj).eq.0.) then
         SlopeX(i,j)=0.
         SlopeY(i,j)=0.
        endif

       ENDDO
      ENDDO

C     Calculate slopes for use in tensor, taper and/or clip
      CALL GMREDI_SLOPE_LIMIT(
     I             dSigmadRReal,
     I             rF(K),
     U             SlopeX, SlopeY,
     O             dRdSigmaLtd,
     I             bi, bj, myThid )

      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1

C       Mask Iso-neutral slopes
        if (hFacC(i,j,k,bi,bj).eq.0.) then
         SlopeX(i,j)=0.
         SlopeY(i,j)=0.
        endif
        Ssq=SlopeX(i,j)*SlopeX(i,j)+SlopeY(i,j)*SlopeY(i,j)

C       Components of Redi/GM tensor
        Kwx(i,j,k,bi,bj)=2.*SlopeX(i,j)
        Kwy(i,j,k,bi,bj)=2.*SlopeY(i,j)
        Kwz(i,j,k,bi,bj)=Ssq

#ifdef GM_VISBECK_VARIABLE_K
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

        if (K.eq.2) VisbeckK(i,j,bi,bj)=0.
Calt?   if (dSigmaDrReal(i,j).NE.0.) then
Calt?    N2=(-Gravity*recip_Rhonil)*dSigmaDrReal(i,j)
        if (dRdSigmaLtd(i,j).NE.0.) then
         N2=(-Gravity*recip_Rhonil)/dRdSigmaLtd(i,j)
         SN=sqrt(Ssq*N2)
         VisbeckK(i,j,bi,bj)=VisbeckK(i,j,bi,bj)+deltaH
     &      *GM_Visbeck_alpha*GM_Visbeck_length*GM_Visbeck_length*SN
        endif

C       Limit range that KapGM can take
        VisbeckK(i,j,bi,bj)=
     &     min(VisbeckK(i,j,bi,bj),GM_Visbeck_maxval_K)

#endif /* GM_VISBECK_VARIABLE_K */


#ifdef ALLOW_TIMEAVE
C--     Time-average
        GM_Kwx_T(i,j,k,bi,bj)=GM_Kwx_T(i,j,k,bi,bj)
     &                       +Kwx(i,j,k,bi,bj)*deltaTclock
        GM_Kwy_T(i,j,k,bi,bj)=GM_Kwy_T(i,j,k,bi,bj)
     &                       +Kwy(i,j,k,bi,bj)*deltaTclock
        GM_Kwz_T(i,j,k,bi,bj)=GM_Kwz_T(i,j,k,bi,bj)
     &                       +Kwz(i,j,k,bi,bj)*deltaTclock
#ifdef GM_VISBECK_VARIABLE_K
        IF (K.EQ.Nr)
     &  Visbeck_K_T(i,j,bi,bj)=Visbeck_K_T(i,j,bi,bj)
     &                       +VisbeckK(i,j,bi,bj)*deltaTclock
#endif
#endif /* ALLOW_TIMEAVE */
       ENDDO
      ENDDO

#ifdef ALLOW_TIMEAVE
      GM_TimeAve(k,bi,bj)=GM_TimeAve(k,bi,bj)+deltaTclock
#endif


#ifdef GM_NON_UNITY_DIAGONAL
C     Gradient of Sigma at U points
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        SlopeX(i,j)=sigmaX(i,j,km1)
     &          *_maskW(i,j,k,bi,bj)
        SlopeY(i,j)=0.25*( sigmaY(i-1,j+1,k) +sigmaY(i,j+1,k)
     &                    +sigmaY(i-1, j ,k) +sigmaY(i, j ,k) )
     &          *_maskW(i,j,k,bi,bj)
        dSigmaDrReal(i,j)=0.25*( sigmaR(i-1,j, k ) +sigmaR(i,j, k )
     &                          +sigmaR(i-1,j,kp1) +sigmaR(i,j,kp1) )
     &          *_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Calculate slopes for use in tensor, taper and/or clip
      CALL GMREDI_SLOPE_LIMIT(
     I             dSigmadRReal,
     I             rF(K),
     U             SlopeX, SlopeY,
     O             dRdSigmaLtd,
     I             bi, bj, myThid )

      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        Kux(i,j,k,bi,bj)=(dSigmaDrReal(i,j)*dRdSigmaLtd(i,j))**2
       ENDDO
      ENDDO

C     Gradient of Sigma at V points
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        SlopeX(i,j)=0.25*( sigmaX(i, j ,k) +sigmaX(i+1, j ,k)
     &                    +sigmaX(i,j-1,k) +sigmaX(i+1,j-1,k) )
     &          *_maskS(i,j,k,bi,bj)
        SlopeY(i,j)=sigmaY(i,j,km1)
     &          *_maskS(i,j,k,bi,bj)
        dSigmaDrReal(i,j)=0.25*( sigmaR(i,j-1, k ) +sigmaR(i,j, k )
     &                          +sigmaR(i,j-1,kp1) +sigmaR(i,j,kp1) )
     &          *_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Calculate slopes for use in tensor, taper and/or clip
      CALL GMREDI_SLOPE_LIMIT(
     I             dSigmadRReal,
     I             rF(K),
     U             SlopeX, SlopeY,
     O             dRdSigmaLtd,
     I             bi, bj, myThid )

      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        Kvy(i,j,k,bi,bj)=(dSigmaDrReal(i,j)*dRdSigmaLtd(i,j))**2
       ENDDO
      ENDDO

#endif /* GM_NON_UNITY_DIAGONAL */



#endif /* ALLOW_GMREDI */

      RETURN
      END


      SUBROUTINE GMREDI_CALC_TENSOR_DUMMY(
     I             bi, bj, iMin, iMax, jMin, jMax, K,
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
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      INTEGER myThid
CEndOfInterface

      INTEGER i, j

#ifdef ALLOW_GMREDI

      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        Kwx(i,j,k,bi,bj) = 0.0
        Kwy(i,j,k,bi,bj) = 0.0
        Kwz(i,j,k,bi,bj) = 0.0
       ENDDO
      ENDDO
#endif /* ALLOW_GMREDI */

      end
