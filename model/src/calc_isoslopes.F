C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_isoslopes.F,v 1.15 2000/06/09 02:45:04 heimbach Exp $
#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CALC_ISOSLOPES(
     I             bi, bj, iMin, iMax, jMin, jMax, K,
     I             rhoKm1, rhoKp1, rhotmp,
     O             K13, K23, K33, KapGM,
     I             myThid )
C     /==========================================================\
C     | SUBROUTINE CALC_ISOSLOPES                                |
C     | o Calculate isentropic surface slopes for GM scheme.     |
C     |==========================================================|
C     | The small-angle approixmation of the full-slope tensor   |
C     | is used here. A maximum slope is also imposed to prevent |
C     | unphysical values in homogenised regions.                |
C     | This routine works on a given XY slab of fluid.          |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C
      _RL rhoKm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoKp1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhotmp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL K13(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL K23(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL K33(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KapGM(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
      INTEGER i,j
      _RL dSigmaDx,dSigmaDy,dSigmaDr,SlopeX,SlopeY,dRdSigma,N2
      _RL gradSmod,stratLimit,rMaxSlope
      _RL Small_Number
      PARAMETER(Small_Number=1. _d -12)
      _RS deltaH,zero_rs
      PARAMETER(zero_rs=0.)

#ifdef ALLOW_AUTODIFF_TAMC
      DO i=1-OLx,sNx+OLx
        K13  (i,  1-OLy,k) = 0.0
        K23  (i,  1-OLy,k) = 0.0
        K33  (i,  1-OLy,k) = 0.0
        K13  (i,sNy+OLy,k) = 0.0
        K23  (i,sNy+OLy,k) = 0.0
        K33  (i,sNy+OLy,k) = 0.0
      ENDDO
      DO j=1-OLy,sNy+OLy
        K13  (  1-OLx,j,k) = 0.0
        K23  (  1-OLx,j,k) = 0.0
        K33  (  1-OLx,j,k) = 0.0
        K13  (sNx+OLx,j,k) = 0.0
        K23  (sNx+OLx,j,k) = 0.0
        K33  (sNx+OLx,j,k) = 0.0
      ENDDO
#endif

C     Calculate terms for mean Richardson number
C     which is used in the "variable K" parameterisaton.
C     Distance between interface above layer and the integration depth (GMdepth)
      deltaH=abs(GMdepth)-abs(rF(k))
C     If positive we limit this to the layer thickness
      deltaH=min(deltaH,drF(k))
C     If negative then we are below the integration level
      deltaH=max(deltaH,zero_rs)
C     Now we convert deltaH to a non-dimensional fraction
      deltaH=deltaH/GMdepth

      rMaxSlope=0.
      if (GMmaxSlope.ne.0.) rMaxSlope=1. _d 0 / GMmaxSlope

#ifdef ALLOW_AUTODIFF_TAMC
!HPF$ INDEPENDENT
#endif
      DO j=1-Oly+1,sNy+Oly-1
#ifdef ALLOW_AUTODIFF_TAMC
!HPF$ INDEPENDENT
#endif
       DO i=1-Olx+1,sNx+Olx-1

C      Horizontal gradient of Sigma at rVel points
        dSigmaDx=0.25*
     &   (_maskW(i+1,j, k ,bi,bj)
     &    *_recip_dxC(i+1,j,bi,bj)
     &        *(rhoKp1(i+1,j)-rhoKp1( i ,j))
     &   +_maskW( i ,j, k ,bi,bj)
     &    *_recip_dxC(i,j,bi,bj)
     &        *(rhoKp1( i ,j)-rhoKp1(i-1,j))
     &   +_maskW(i+1,j,k-1,bi,bj)
     &    *_recip_dxC(i+1,j,bi,bj)
     &        *(rhoKm1(i+1,j)-rhoKm1( i ,j))
     &   +_maskW( i ,j,k-1,bi,bj)
     &    *_recip_dxC(i,j,bi,bj)
     &        *(rhoKm1( i ,j)-rhoKm1(i-1,j)) )
        dSigmaDy=0.25*
     &   (_maskS(i,j+1, k ,bi,bj)
     &    *_recip_dyC(i,j+1,bi,bj)
     &        *(rhoKp1(i,j+1)-rhoKp1(i, j ))
     &   +_maskS(i,  j, k ,bi,bj)
     &    *_recip_dyC(i,j,bi,bj)
     &        *(rhoKp1(i, j )-rhoKp1(i,j-1))
     &   +_maskS(i,j+1,k-1,bi,bj)
     &    *_recip_dyC(i,j+1,bi,bj)
     &        *(rhoKm1(i,j+1)-rhoKm1(i, j ))
     &   +_maskS(i, j ,k-1,bi,bj)
     &    *_recip_dyC(i,j,bi,bj)
     &        *(rhoKm1(i, j )-rhoKm1(i,j-1)) )

C       Calculate magnitude of horizontal gradient
#ifdef ALLOW_AUTODIFF_TAMC
        if (dSigmaDx*dSigmaDx+dSigmaDy*dSigmaDy .eq. 0) then
          gradSmod=0.0
        else
          gradSmod=sqrt(dSigmaDx*dSigmaDx+dSigmaDy*dSigmaDy)
        endif
#else
        gradSmod=sqrt(dSigmaDx*dSigmaDx+dSigmaDy*dSigmaDy)
#endif

C       Limit Stratification
        stratLimit=-Small_Number-gradSmod*rMaxSlope
        dSigmaDr=recip_drC(k)*rkFac*(rhotmp(i,j)-rhoKp1(i,j))
C       Altern -> if (dSigmaDz.gt.stratLimit) dSigmaDz=stratLimit
        dSigmaDr=min(dSigmaDr,stratLimit)
        N2=(-Gravity*recip_Rhonil)*dSigmaDr
        dRdSigma=1. _d 0 / dSigmaDr

C       Iso-neutral slopes
        SlopeX=-dSigmaDx*dRdSigma
        SlopeY=-dSigmaDy*dRdSigma
        if (hFacC(i,j,k,bi,bj).eq.0.) then
         SlopeX=0.
         SlopeY=0.
        endif

C       Components of Redi/GM tensor
        K13(i,j,k)=2. _d 0 * SlopeX
        K23(i,j,k)=2. _d 0 * SlopeY
        K33(i,j,k)=SlopeX*SlopeX+SlopeY*SlopeY

C       Depth average of M^2/N^2 * N
        KapGM(i,j)=KapGM(i,j)+deltaH*
     &      GMalpha*GMlength*GMlength* sqrt(K33(i,j,k)*N2)

C       Limit range that KapGM can take
        KapGM(i,j)=min(KapGM(i,j),GMmaxval)

       ENDDO
      ENDDO

      RETURN
      END
