
#include "GMREDI_OPTIONS.h"

CStartOfInterface
      SUBROUTINE GMREDI_SLOPE_LIMIT(
     I             dSigmaDrReal,
     I             depthZ,K,
     U             SlopeX, SlopeY,
     U             dSigmaDx, dSigmaDy,
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
#include "GRID.h"
#include "EEPARAMS.h"
#include "GMREDI.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     == Routine arguments ==
C
      _RL SlopeX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDx(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDy(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDrReal(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeSqr(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL taperFct(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL depthZ
      INTEGER bi,bj,K,myThid
CEndOfInterface

#ifdef ALLOW_GMREDI

C     == Local variables ==
      _RL Small_Number
      _RL Small_Taper
      PARAMETER(Small_Number=1.D-6)
      PARAMETER(Small_Taper=1.D+03)
      _RL gradSmod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDrLtd(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dRdSigmaLtd(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL f1,Smod,f2,Rnondim,Cspd,Lrho
      _RL maxSlopeSqr
      _RL fpi
      PARAMETER(fpi=3.141592653589793047592d0)
      INTEGER i,j

#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ikey = (act1 + 1) + act2*max1
     &                  + act3*max1*max2
     &                  + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        gradSmod(i,j)    = 0. _d 0
        dSigmaDrLtd(i,j) = 0. _d 0
       ENDDO
      ENDDO

      IF (GM_taper_scheme.EQ.'orig' .OR.
     &    GM_taper_scheme.EQ.'clipping') THEN

#if (defined (GM_TAPER_ORIG_CLIPPING))

C-      Original implementation in mitgcmuv
C       (this turns out to be the same as Cox slope clipping)

C-      Cox 1987 "Slope clipping"
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF ( DSigmaDX(i,j)*DSigmaDX(i,j) + 
     &        DSigmaDY(i,j)*DSigmaDY(i,j)
     &        .EQ. 0. ) THEN
           gradSmod(i,j) = 0. _d 0
          ELSE
           gradSmod(i,j) = sqrt(DSigmaDX(i,j)*DSigmaDX(i,j)
     &                         +DSigmaDY(i,j)*DSigmaDY(i,j))
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE gradSmod(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDrReal(:,:) = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF (gradSmod(i,j) .NE. 0.) THEN
           dSigmaDrLtd(i,j) = -gradSmod(i,j)*GM_rMaxSlope
           IF ( dSigmaDrReal(i,j) .GE. 
     &          dSigmaDrLtd(i,j) )
     &          dSigmaDrReal(i,j) = 
     &          dSigmaDrLtd(i,j)
cph           IF ( dSigmaDrReal(i,j) .GE. 
cph     &          GM_adjointRescale*dSigmaDrLtd(i,j) )
cph     &          dSigmaDrReal(i,j) = 
cph     &          dSigmaDrLtd(i,j)*GM_adjointRescale
ctest           dSigmaDrReal(i,j) = dSigmaDrLtd(i,j)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE slopeX(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE slopeY(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDrReal(:,:) = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF (gradSmod(i,j) .EQ. 0.) THEN
           SlopeX(i,j) = 0. _d 0
           SlopeY(i,j) = 0. _d 0
          ELSE
           dRdSigmaLtd(i,j) = 1./( dSigmaDrReal(i,j) )
           SlopeX(i,j)=-DSigmaDX(i,j)*dRdSigmaLtd(i,j)
           SlopeY(i,j)=-DSigmaDY(i,j)*dRdSigmaLtd(i,j)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE slopeX(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE slopeY(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          SlopeSqr(i,j)=SlopeX(i,j)*SlopeX(i,j)
     &                 +SlopeY(i,j)*SlopeY(i,j)
          taperFct(i,j)=1. _d 0
         ENDDO
        ENDDO

#else

        STOP 'Need to set runtime flag GM_taper_scheme correctly'

#endif /* GM_TAPER_ORIG_CLIPPING */

      ELSE IF (GM_taper_scheme.EQ.'ac02') THEN

#if (defined (GM_TAPER_AC02))

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          dRdSigmaLtd(i,j)= 
     &                      dSigmaDx(i,j)*dSigmaDx(i,j)
     &                      + dSigmaDy(i,j)*dSigmaDy(i,j)
     &                      + dSigmaDrReal(i,j)**2
ctest
          IF (dRdSigmaLtd(i,j).NE.0.) then
             dRdSigmaLtd(i,j)=1. _d 0
     &            / ( dRdSigmaLtd(i,j) )
             SlopeSqr(i,j)=(dSigmaDx(i,j)*dSigmaDx(i,j)
     &            +dSigmaDy(i,j)*dSigmaDy(i,j))*dRdSigmaLtd(i,j)
             SlopeX(i,j)=-dSigmaDx(i,j)
     &            *dRdSigmaLtd(i,j)*dSigmaDrReal(i,j)
             SlopeY(i,j)=-dSigmaDy(i,j)
     &            *dRdSigmaLtd(i,j)*dSigmaDrReal(i,j)
cph             T11(i,j)=(dSigmaDrReal(i,j)**2)*dRdSigmaLtd(i,j)
             taperFct(i,j) = 1. _d 0
cph          ELSE
cph             SlopeSqr(i,j) = 0. _d 0
cph             SlopeX(i,j)   = 0. _d 0
cph             SlopeY(i,j)   = 0. _d 0
cph             T11(i,j)      = 0. _d 0
cph             taperFct(i,j) = 0. _d 0
          ENDIF
cph          IF ( SlopeSqr(i,j) .GT. GM_maxSlope**2 ) THEN
cph           taperFct(i,j) = GM_maxSlope**2/SlopeSqr(i,j)
cph          ENDIF
         ENDDO
        ENDDO

#else
       
        STOP 'Need to set runtime flag GM_taper_scheme correctly'

#endif /* GM_TAPER_AC02 */

      ELSE

#if (defined (GM_TAPER_REST))

C----------------------------------------------------------------------

C- Compute the slope, no clipping, but avoid reverse slope in negatively
C                                  stratified (Sigma_Z > 0) region :

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE dSigmaDrReal(:,:) = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF ( dSigmaDrReal(i,j) .NE. 0. ) THEN
           IF (dSigmaDrReal(i,j).GE.(-Small_Number))
     &         dSigmaDrReal(i,j) = -Small_Number
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE dSigmaDx(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDy(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDrReal(:,:) = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF ( dSigmaDrReal(i,j) .EQ. 0. ) THEN
           IF ( dSigmaDx(i,j) .NE. 0. ) THEN
            SlopeX(i,j) = SIGN(Small_taper,dSigmaDx(i,j))
           ELSE
            SlopeX(i,j) = 0. _d 0
           ENDIF
           IF ( dSigmaDy(i,j) .NE. 0. ) THEN
            SlopeY(i,j) = SIGN(Small_taper,dSigmaDy(i,j))
           ELSE
            SlopeY(i,j) = 0. _d 0
           ENDIF
          ELSE
           dRdSigmaLtd(i,j) = 1./(dSigmaDrReal(i,j)-Small_Number)
           SlopeX(i,j) = -dSigmaDx(i,j)*dRdSigmaLtd(i,j)
           SlopeY(i,j) = -dSigmaDy(i,j)*dRdSigmaLtd(i,j)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE slopeX(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE slopeY(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          SlopeSqr(i,j) = SlopeX(i,j)*SlopeX(i,j)
     &                   +SlopeY(i,j)*SlopeY(i,j)
          taperFct(i,j) = 1. _d 0
         ENDDO
        ENDDO

C- Compute the tapering function for the GM+Redi tensor :

       IF (GM_taper_scheme.EQ.'linear') THEN

C-      Simplest adiabatic tapering = Smax/Slope (linear)
        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF ( SlopeSqr(i,j) .EQ. 0. ) THEN
           taperFct(i,j) = 1. _d 0
          ELSE IF ( SlopeSqr(i,j) .GT. maxSlopeSqr )  THEN
           taperFct(i,j) = sqrt(maxSlopeSqr / SlopeSqr(i,j))
          ENDIF

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'gkw91') THEN

C-      Gerdes, Koberle and Willebrand, Clim. Dyn. 1991
        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF ( SlopeSqr(i,j) .EQ. 0. ) THEN
           taperFct(i,j) = 1. _d 0
          ELSE IF ( SlopeSqr(i,j) .GT. maxSlopeSqr ) THEN
           taperFct(i,j) = maxSlopeSqr/SlopeSqr(i,j)
          ENDIF

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'dm95') THEN

C-      Danabasoglu and McWilliams, J. Clim. 1995
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF ( SlopeSqr(i,j) .EQ. 0. ) THEN
           taperFct(i,j) = 1. _d 0
          ELSE
           Smod=sqrt(SlopeSqr(i,j))
           taperFct(i,j)=0.5*(1.+tanh( (GM_Scrit-Smod)/GM_Sd ))
          ENDIF
         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'ldd97') THEN

C-      Large, Danabasoglu and Doney, JPO 1997
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF (SlopeSqr(i,j) .EQ. 0.) THEN
           taperFct(i,j) = 1. _d 0
          ELSE
           Smod=sqrt(SlopeSqr(i,j))
           f1=0.5*(1.+tanh( (GM_Scrit-Smod)/GM_Sd ))
           Cspd=2.
           Lrho=100. _d 3
           if (FCori(i,j,bi,bj).NE.0.) Lrho=Cspd/abs(Fcori(i,j,bi,bj))
           Lrho=min(Lrho , 100. _d 3)
           Lrho=max(Lrho , 15. _d 3)
           Rnondim=depthZ/(Lrho*Smod)
           f2=0.5*(1.+sin( fpi*(Rnondim-0.5)))
           taperFct(i,j)=f1*f2
          ENDIF

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.NE.' ') THEN
        STOP 'GMREDI_SLOPE_LIMIT: Bad GM_taper_scheme'
       ENDIF

#endif

      ENDIF


#endif /* ALLOW_GMREDI */

      RETURN
      END
