#include "CPP_EEOPTIONS.h"

! ==============================================================================
      subroutine CALC_ISOSLOPES(
     I             bi, bj, iMin, iMax, jMin, jMax, K,
     I             rhoKm1, rhoKp1, rhotmp,
     O             K13, K23, K33, KapGM,
     I             myThid )
      implicit none
! Common
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PARAMS.h"
! Arguments
      integer bi,bj,iMin,iMax,jMin,jMax,K
      _RL rhoKm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoKp1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhotmp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL K13(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nz)
      _RL K23(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nz)
      _RL K33(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nz)
      _RL KapGM(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      integer myThid
! Local
      integer i,j
      _RL dSigmaDx,dSigmaDy,dSigmaDz,SlopeX,SlopeY,dZdSigma
      _RL gradSmod,stratLimit,rMaxSlope,deltaH
      _RL Small_Number
      parameter(Small_Number=1. _d -12)
! ------------------------------------------------------------------------------

      deltaH=dzC(k)/GMdepth
      if (k.gt.11) deltaH=0.
      rMaxSlope=0.
      if (GMmaxSlope.ne.0.) rMaxSlope=1. _d 0 / GMmaxSlope

      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1

! Horizontal gradient of Sigma at W points
        dSigmaDx=0.25*_rdxF(i,j,bi,bj)*
     &   (_maskW(i+1,j, k ,bi,bj)*(rhoKp1(i+1,j)-rhoKp1( i ,j))
     &   +_maskW( i ,j, k ,bi,bj)*(rhoKp1( i ,j)-rhoKp1(i-1,j))
     &   +_maskW(i+1,j,k-1,bi,bj)*(rhoKm1(i+1,j)-rhoKm1( i ,j))
     &   +_maskW( i ,j,k-1,bi,bj)*(rhoKm1( i ,j)-rhoKm1(i-1,j)) )
        dSigmaDy=0.25*_rdyF(i,j,bi,bj)*
     &   (_maskS(i,j+1, k ,bi,bj)*(rhoKp1(i,j+1)-rhoKp1(i, j ))
     &   +_maskS(i,  j, k ,bi,bj)*(rhoKp1(i, j )-rhoKp1(i,j-1))
     &   +_maskS(i,j+1,k-1,bi,bj)*(rhoKm1(i,j+1)-rhoKm1(i, j ))
     &   +_maskS(i, j ,k-1,bi,bj)*(rhoKm1(i, j )-rhoKm1(i,j-1)) )

! Calculate magnitude of horizontal gradient
        gradSmod=sqrt(dSigmaDx*dSigmaDx+dSigmaDy*dSigmaDy)

! Limit Stratification
        stratLimit=-Small_Number-gradSmod*rMaxSlope
        dSigmaDz=rDzC(k)*(rhotmp(i,j)-rhoKp1(i,j))
!altern if (dSigmaDz.gt.stratLimit) dSigmaDz=stratLimit
        dSigmaDz=min(dSigmaDz,stratLimit)
        dZdSigma=1. _d 0 / dSigmaDz

! Iso-neutral slopes
        SlopeX=-dSigmaDx*dZdSigma
        SlopeY=-dSigmaDy*dZdSigma
        if (hFacC(i,j,k,bi,bj).eq.0.) then
         SlopeX=0.
         SlopeY=0.
        endif

! Components of Redi/GM tensor
        K13(i,j,k)=2. _d 0 * SlopeX
        K23(i,j,k)=2. _d 0 * SlopeY
        K33(i,j,k)=SlopeX*SlopeX+SlopeY*SlopeY

! Depth average of M^2/N^2 * N
        KapGM(i,j)=KapGM(i,j)+deltaH*
     &      GMalpha*GMlength*GMlength* sqrt(K33(i,j,k))
     &      + GMkbackground

       ENDDO
      ENDDO

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================
