C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_slope_psi.F,v 1.1 2001/12/16 18:54:49 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CStartOfInterface
      SUBROUTINE GMREDI_SLOPE_PSI_B(
     I             dSigmaDrW,dSigmaDrS,
     I             depthZ,
     U             SlopeX, SlopeY,
     O             taperX, taperY,
     I             bi,bj, myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_SLOPE_PSI_B                            |
C     | o Calculate slopes for use in GM/Redi tensor             |
C     |==========================================================|
C     | On entry:                                                |
C     |            dSigmaDrW conatins the d/dz Sigma          |
C     |            SlopeX/Y     contains X/Y gradients of sigma  |
C     |            depthZ       conatins the height (m) of level |
C     | On exit:                                                 |
C     |            dSigmaDrW conatins the effective dSig/dz   |
C     |            SlopeX/Y     contains X/Y slopes              |
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
      _RL dSigmaDrW(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDrS(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL taperX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL taperY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
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

c         gradSmod=SlopeX(i,j)*SlopeX(i,j)
c    &            +SlopeY(i,j)*SlopeY(i,j)
c         if (gradSmod .NE. 0.) gradSmod=sqrt(gradSmod)
          gradSmod=abs(SlopeX(i,j))

          dSigmaDrLtd = -(Small_Number+gradSmod*GM_rMaxSlope)
          IF (dSigmaDrW(i,j).GE.dSigmaDrLtd)
     &        dSigmaDrW(i,j) = dSigmaDrLtd
          SlopeX(i,j) = -SlopeX(i,j)/dSigmaDrW(i,j)

          gradSmod=abs(SlopeY(i,j))
          dSigmaDrLtd = -(Small_Number+gradSmod*GM_rMaxSlope)
          IF (dSigmaDrS(i,j).GE.dSigmaDrLtd)
     &        dSigmaDrS(i,j) = dSigmaDrLtd
          SlopeY(i,j) = -SlopeY(i,j)/dSigmaDrS(i,j)

          taperX(i,j)=1. _d 0
          taperY(i,j)=1. _d 0

         ENDDO
        ENDDO

      ELSE

C- Compute the slope, no clipping, but avoid reverse slope in negatively 
C                                  stratified (Sigma_Z > 0) region :
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          dSigmaDrLtd = -Small_Number
          IF (dSigmaDrW(i,j).GE.dSigmaDrLtd)
     &        dSigmaDrW(i,j) = dSigmaDrLtd
          dRdSigmaLtd = 1./dSigmaDrW(i,j)
          SlopeX(i,j) = -SlopeX(i,j)/dSigmaDrW(i,j)

          dSigmaDrLtd = -Small_Number
          IF (dSigmaDrS(i,j).GE.dSigmaDrLtd)
     &        dSigmaDrS(i,j) = dSigmaDrLtd
          SlopeY(i,j) = -SlopeY(i,j)/dSigmaDrS(i,j)

c         SlopeSqr(i,j)=SlopeX(i,j)*SlopeX(i,j)
c    &                 +SlopeY(i,j)*SlopeY(i,j)

          taperX(i,j)=1. _d 0
          taperY(i,j)=1. _d 0

         ENDDO
        ENDDO

C- Compute the tapering function for the GM+Redi tensor :

       IF (GM_taper_scheme.EQ.'linear') THEN

C-      Simplest adiabatic tapering = Smax/Slope (linear)
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF (abs(SlopeX(i,j)).GT.GM_maxSlope) 
     &      taperX(i,j)=GM_maxSlope/abs(SlopeX(i,j))
          IF (abs(SlopeY(i,j)).GT.GM_maxSlope) 
     &      taperY(i,j)=GM_maxSlope/abs(SlopeY(i,j))

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'gkw91') THEN

C-      Gerdes, Koberle and Willebrand, Clim. Dyn. 1991
        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF (abs(SlopeX(i,j)).GT.GM_maxSlope) 
     &      taperX(i,j)=maxSlopeSqr/(SlopeX(i,j)*SlopeX(i,j))
          IF (abs(SlopeY(i,j)).GT.GM_maxSlope) 
     &      taperY(i,j)=maxSlopeSqr/(SlopeY(i,j)*SlopeY(i,j))

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'dm95') THEN

C-      Danabasoglu and McWilliams, J. Clim. 1995
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          Smod = abs(SlopeX(i,j))
          taperX(i,j)=0.5*(1.+tanh( (GM_Scrit-Smod)/GM_Sd ))
          Smod = abs(SlopeY(i,j))
          taperY(i,j)=0.5*(1.+tanh( (GM_Scrit-Smod)/GM_Sd ))

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'ldd97') THEN

C-      Large, Danabasoglu and Doney, JPO 1997
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          Cspd=2.
          Lrho=100.e3
          if (FCori(i,j,bi,bj).NE.0.) Lrho=Cspd/abs(Fcori(i,j,bi,bj))
          Lrho=min(Lrho,100.e3)
          Lrho=max(Lrho,15.e3)

          Smod = abs(SlopeX(i,j))
          f1=0.5*(1.+tanh( (GM_Scrit-Smod)/GM_Sd ))
          if (Smod.NE.0.) then
            Rnondim=depthZ/(Lrho*Smod)
          else
            Rnondim=0.
          endif
          f2=0.5*(1.+sin( fpi*(Rnondim-0.5)))
          taperX(i,j)=f1*f2

          Smod = abs(SlopeY(i,j))
          f1=0.5*(1.+tanh( (GM_Scrit-Smod)/GM_Sd ))
          if (Smod.NE.0.) then
            Rnondim=depthZ/(Lrho*Smod)
          else
            Rnondim=0.
          endif
          f2=0.5*(1.+sin( fpi*(Rnondim-0.5)))
          taperY(i,j)=f1*f2

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.NE.' ') THEN
        STOP 'GMREDI_SLOPE_PSI: Bad GM_taper_scheme'
       ENDIF

      ENDIF


#endif /* ALLOW_GMREDI */

      RETURN
      END
