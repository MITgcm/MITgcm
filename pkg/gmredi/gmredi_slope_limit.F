C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_slope_limit.F,v 1.8 2001/12/04 15:09:34 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CStartOfInterface
      SUBROUTINE GMREDI_SLOPE_LIMIT(
     I             dSigmaDrReal,
     I             depthZ,
     U             SlopeX, SlopeY,
     O             SlopeSqr, taperFct,
     I             bi,bj, myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_SLOPE_LIMIT                            |
C     | o Calculate slopes for use in GM/Redi tensor             |
C     |==========================================================|
C     | On entry:                                                |
C     |            dSigmaDrReal conatins the d/dz Sigma          |
C     |            SlopeX/Y     contains X/Y gradients of sigma  |
C     |            depthZ       conatins the height (m) of level |
C     | On exit:                                                 |
C     |            dSigmaDrReal conatins the effective dSig/dz   |
C     |            SlopeX/Y     contains X/Y slopes              |
C     |            SlopeSqr     contains Sx^2+Sy^2               |
C     |            taperFct     contains tapering funct. value ; |
C     |                         = 1 when using no tapering       |
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
      _RL SlopeSqr(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL taperFct(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL depthZ
      INTEGER bi,bj,myThid
CEndOfInterface

#ifdef ALLOW_GMREDI

C     == Local variables ==
      _RL Small_Number
      PARAMETER(Small_Number=1.D-12)
      _RL gradSmod,f1,Smod,f2,Rnondim,Cspd,Lrho
      _RL dSigmaDrLtd, dRdSigmaLtd
      _RL maxSlopeSqr
      _RL fpi
      PARAMETER(fpi=3.141592653589793047592d0)
      INTEGER i,j

      IF (GM_taper_scheme.EQ.'orig' .OR.
     &    GM_taper_scheme.EQ.'clipping') THEN

C-      Original implementation in mitgcmuv
C       (this turns out to be the same as Cox slope clipping)

C-      Cox 1987 "Slope clipping"
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          gradSmod=SlopeX(i,j)*SlopeX(i,j)
     &            +SlopeY(i,j)*SlopeY(i,j)
          if (gradSmod .NE. 0.) gradSmod=sqrt(gradSmod)

          dSigmaDrLtd = -(Small_Number+gradSmod*GM_rMaxSlope)
          IF (dSigmaDrReal(i,j).GE.dSigmaDrLtd)
     &        dSigmaDrReal(i,j) = dSigmaDrLtd
          dRdSigmaLtd = 1./dSigmaDrReal(i,j)

          SlopeX(i,j)=-SlopeX(i,j)*dRdSigmaLtd
          SlopeY(i,j)=-SlopeY(i,j)*dRdSigmaLtd
          SlopeSqr(i,j)=SlopeX(i,j)*SlopeX(i,j)
     &                 +SlopeY(i,j)*SlopeY(i,j)
          taperFct(i,j)=1. _d 0

         ENDDO
        ENDDO

      ELSE

C- Compute the slope, no clipping, but avoid reverse slope in negatively 
C                                  stratified (Sigma_Z > 0) region :
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          dSigmaDrLtd = -Small_Number
          IF (dSigmaDrReal(i,j).GE.dSigmaDrLtd)
     &        dSigmaDrReal(i,j) = dSigmaDrLtd
          dRdSigmaLtd = 1./dSigmaDrReal(i,j)

          SlopeX(i,j) = -SlopeX(i,j)*dRdSigmaLtd
          SlopeY(i,j) = -SlopeY(i,j)*dRdSigmaLtd
          SlopeSqr(i,j)=SlopeX(i,j)*SlopeX(i,j)
     &                 +SlopeY(i,j)*SlopeY(i,j)
          taperFct(i,j)=1. _d 0

         ENDDO
        ENDDO

C- Compute the tapering function for the GM+Redi tensor :

       IF (GM_taper_scheme.EQ.'linear') THEN

C-      Simplest adiabatic tapering = Smax/Slope (linear)
        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF (SlopeSqr(i,j).GT.maxSlopeSqr) 
     &      taperFct(i,j)=GM_maxSlope/sqrt(SlopeSqr(i,j))

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'gkw91') THEN

C-      Gerdes, Koberle and Willebrand, Clim. Dyn. 1991
        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF (SlopeSqr(i,j).GT.maxSlopeSqr)
     &      taperFct(i,j)=maxSlopeSqr/SlopeSqr(i,j)

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'dm95') THEN

C-      Danabasoglu and McWilliams, J. Clim. 1995
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          Smod = SlopeSqr(i,j)
          if (Smod.NE.0.) Smod=sqrt(Smod)
          taperFct(i,j)=0.5*(1.+tanh( (GM_Scrit-Smod)/GM_Sd ))

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'ldd97') THEN

C-      Large, Danabasoglu and Doney, JPO 1997
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          Smod = SlopeSqr(i,j)
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
          taperFct(i,j)=f1*f2

         ENDDO
        ENDDO

       ENDIF

      ENDIF


#endif /* ALLOW_GMREDI */

      RETURN
      END
