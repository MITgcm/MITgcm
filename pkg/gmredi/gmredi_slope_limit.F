C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_slope_limit.F,v 1.3 2001/02/04 14:38:49 cnh Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CStartOfInterface
      SUBROUTINE GMREDI_SLOPE_LIMIT(
     I             dSigmadRReal,
     I             depthZ,
     U             SlopeX, SlopeY,
     O             dRdSigmaLtd,
     I             bi,bj, myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_SLOPE_LIMIT                            |
C     | o Calculate slopes for use in GM/Redi tensor             |
C     |==========================================================|
C     | On entry:                                                |
C     |            dSigmadRReal conatins the d/dz Sigma          |
C     |            SlopeX/Y     contains X/Y gradients of sigma  |
C     |            depthZ       conatins the height (m) of level |
C     | On exit:                                                 |
C     |            SlopeX/Y     contains X/Y slopes              |
C     |            dRdSigmaLtd  conatins the effective dSig/dz   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GMREDI.h"
#include "PARAMS.h"

C     == Routine arguments ==
C
      _RL SlopeX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDrReal(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dRdSigmaLtd(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL depthZ
      INTEGER bi,bj,myThid
CEndOfInterface

#ifdef ALLOW_GMREDI

C     == Local variables ==
      _RL Small_Number
      PARAMETER(Small_Number=1.E-12)
      _RL gradSmod,stratLimit,f1,Smod,f2,Rnondim,Cspd,Lrho
      _RL dSigmaDrLtd
      _RL fpi
      PARAMETER(fpi=3.141592653589793047592)
      INTEGER i,j
      _RL     sx, sy

      if (GM_taper_scheme.EQ.'orig') then

C-      Original implementation in mitgcmuv
C       (this turns out to be the same as Cox slope clipping)
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          gradSmod=SlopeX(i,j)*SlopeX(i,j)
     &            +SlopeY(i,j)*SlopeY(i,j)
          if (gradSmod .NE. 0.) gradSmod=sqrt(gradSmod)

          stratLimit=-Small_Number-gradSmod*GM_rMaxSlope
          if (dSigmaDrReal(i,j).LT.stratLimit) then
            dSigmaDrLtd=dSigmaDrReal(i,j)
          else
            dSigmaDrLtd=stratLimit
          endif
          dRdSigmaLtd(i,j)=1./dSigmaDrLtd
          SlopeX(i,j)=-SlopeX(i,j)*dRdSigmaLtd(i,j)
          SlopeY(i,j)=-SlopeY(i,j)*dRdSigmaLtd(i,j)

         ENDDO
        ENDDO

      elseif (GM_taper_scheme.EQ.'clipping') then

C-      Cox 1987 "Slope clipping"
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          gradSmod=SlopeX(i,j)*SlopeX(i,j)
     &            +SlopeY(i,j)*SlopeY(i,j)
          if (gradSmod .NE. 0.) gradSmod=sqrt(gradSmod)

          dSigmaDrLtd=-(Small_Number+gradSmod*GM_rMaxSlope)
          if (dSigmaDrReal(i,j).LT.dSigmaDrLtd)
     &            dSigmaDrLtd=dSigmaDrReal(i,j)
          dRdSigmaLtd(i,j)=1./dSigmaDrLtd
          SlopeX(i,j)=-SlopeX(i,j)*dRdSigmaLtd(i,j)
          SlopeY(i,j)=-SlopeY(i,j)*dRdSigmaLtd(i,j)

         ENDDO
        ENDDO

      elseif (GM_taper_scheme.EQ.'gkw91') then

C-      Gerdes, Koberle and Willebrand, Clim. Dyn. 1991
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          gradSmod=SlopeX(i,j)*SlopeX(i,j)
     &            +SlopeY(i,j)*SlopeY(i,j)
          if (gradSmod .NE. 0.) gradSmod=sqrt(gradSmod)

          dSigmaDrLtd=dSigmaDrReal(i,j)
          if (dSigmaDrLtd.NE.0.) then
           dRdSigmaLtd(i,j)=1./dSigmaDrLtd
          else
           dRdSigmaLtd(i,j)=0.
          endif
          if (gradSmod.LE.GM_maxSlope*abs(dSigmaDrReal(i,j))) then
C           If the slope is < Smax then calculate slopes properly
            SlopeX(i,j)=-SlopeX(i,j)*dRdSigmaLtd(i,j)
            SlopeY(i,j)=-SlopeY(i,j)*dRdSigmaLtd(i,j)
          else
C           If the slope is > Smax then taper slopes
            SlopeX(i,j)=-SlopeX(i,j)*dSigmaDrLtd
     &                   *((GM_maxSlope/gradSmod)**2)
            SlopeY(i,j)=-SlopeY(i,j)*dSigmaDrLtd
     &                   *((GM_maxSlope/gradSmod)**2)
          endif

         ENDDO
        ENDDO

      elseif (GM_taper_scheme.EQ.'dm95') then

C-      Danabasoglu and McWilliams, J. Clim. 1995
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          dSigmaDrLtd=dSigmaDrReal(i,j)
          if (dSigmaDrLtd.NE.0.) then
           dRdSigmaLtd(i,j)=1./dSigmaDrLtd
          else
           dRdSigmaLtd(i,j)=0.
          endif
          sx   = -SlopeX(i,j)*dRdSigmaLtd(i,j)
          sy   = -SlopeY(i,j)*dRdSigmaLtd(i,j)
          Smod = sx*sx + sy*sy
          if (Smod.NE.0.) Smod=sqrt(Smod)
          f1=0.5*(1.+tanh( (GM_Scrit-Smod)/GM_Sd ))
          SlopeX(i,j) = sx*f1
          SlopeY(i,j) = sy*f1

         ENDDO
        ENDDO

      elseif (GM_taper_scheme.EQ.'ldd97') then

C-      Large, Danabasoglu and Doney, JPO 1997
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          dSigmaDrLtd=dSigmaDrReal(i,j)
          if (dSigmaDrLtd.NE.0.) then
           dRdSigmaLtd(i,j)=1./dSigmaDrLtd
          else
           dRdSigmaLtd(i,j)=0.
          endif
          sx   = -SlopeX(i,j)*dRdSigmaLtd(i,j)
          sx   = -SlopeY(i,j)*dRdSigmaLtd(i,j)
          Smod = sx*sx + sy*sy
          if (Smod.NE.0.) Smod=sqrt(Smod)
          f1=0.5*(1.+tanh( (GM_Scrit-Smod)/GM_Sd ))
          Cspd=2.
          Lrho=100.e3
          if (FCori(i,j,bi,bj).NE.0.) Lrho=Cspd/abs(Fcori(i,j,bi,bj))
          Lrho=min(Lrho,100.e3)
          Lrho=max(Lrho,15.e3)
          if (Smod.NE.0.) then
            Rnondim=depthZ/(Lrho*Smod)
          else
            Rnondim=0.
          endif
          f2=0.5*(1.+sin( fpi*(Rnondim-0.5)))
          SlopeX(i,j) = sx*f1
          SlopeY(i,j) = sy*f1

         ENDDO
        ENDDO

      elseif (GM_taper_scheme.EQ.' ') then

C-      No tapering/clipping selected so ...
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          dSigmaDrLtd=dSigmaDrReal(i,j)
          if (dSigmaDrLtd.NE.0.) then
            dRdSigmaLtd(i,j)=1./dSigmaDrLtd
          else
            dRdSigmaLtd(i,j)=0.
          endif
          SlopeX(i,j)=-SlopeX(i,j)*dRdSigmaLtd(i,j)
          SlopeY(i,j)=-SlopeY(i,j)*dRdSigmaLtd(i,j)

         ENDDO
        ENDDO

      endif


#endif /* ALLOW_GMREDI */

      RETURN
      END
