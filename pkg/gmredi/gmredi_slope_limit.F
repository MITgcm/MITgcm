C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_slope_limit.F,v 1.24 2007/06/17 22:43:13 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CStartOfInterface
      SUBROUTINE GMREDI_SLOPE_LIMIT(
     O             SlopeX, SlopeY,
     O             SlopeSqr, taperFct,
     U             dSigmaDr,
     I             dSigmaDx, dSigmaDy,
     I             Lrho, depthZ, K,
     I             bi,bj, myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_SLOPE_LIMIT                            |
C     | o Calculate slopes for use in GM/Redi tensor             |
C     |==========================================================|
C     | On entry:                                                |
C     |            dSigmaDr     contains the d/dz Sigma          |
C     |            dSigmaDx/Dy  contains X/Y gradients of sigma  |
C     |            depthZ       contains the depth (< 0 !) [m]   |
C     | On exit:                                                 |
C     |            dSigmaDr     contains the effective dSig/dz   |
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
C     !INPUT PARAMETERS:
C     !OUTPUT PARAMETERS:
      _RL SlopeX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeSqr(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL taperFct(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDr(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDx(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDy(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL Lrho(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RS depthZ
      INTEGER bi,bj,K,myThid
CEndOfInterface

#ifdef ALLOW_GMREDI

C     == Local variables ==
      _RL Small_Taper
      PARAMETER(Small_Taper=1.D+03)

      _RL gradSmod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dRdSigmaLtd(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL tmpfld(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL f1,Smod,f2,Rnondim
      _RL maxSlopeSqr
      _RL fpi
      PARAMETER( fpi = PI )
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
        tmpfld(i,j)      = 0. _d 0
       ENDDO
      ENDDO

      IF (GM_taper_scheme.EQ.'orig' .OR.
     &    GM_taper_scheme.EQ.'clipping') THEN

#ifdef GM_EXCLUDE_CLIPPING

        STOP 'Need to compile without "#define GM_EXCLUDE_CLIPPING"'

#else  /* GM_EXCLUDE_CLIPPING */

C-      Original implementation in mitgcmuv
C       (this turns out to be the same as Cox slope clipping)

C-      Cox 1987 "Slope clipping"
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          tmpfld(i,j) = dSigmaDx(i,j)*dSigmaDx(i,j) + 
     &                  dSigmaDy(i,j)*dSigmaDy(i,j)
          IF ( tmpfld(i,j) .EQ. 0. ) THEN
           gradSmod(i,j) = 0. _d 0
          ELSE
           gradSmod(i,j) = sqrt( tmpfld(i,j) )
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE gradSmod(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDr(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF (gradSmod(i,j) .NE. 0.) THEN
           tmpfld(i,j) = -gradSmod(i,j)*GM_rMaxSlope
           IF ( dSigmaDr(i,j) .GE. tmpfld(i,j) )
     &          dSigmaDr(i,j) = tmpfld(i,j)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE slopeX(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE slopeY(:,:)       = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDr(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF (gradSmod(i,j) .EQ. 0.) THEN
           SlopeX(i,j) = 0. _d 0
           SlopeY(i,j) = 0. _d 0
          ELSE
           dRdSigmaLtd(i,j) = 1./( dSigmaDr(i,j) )
           SlopeX(i,j)=-dSigmaDx(i,j)*dRdSigmaLtd(i,j)
           SlopeY(i,j)=-dSigmaDy(i,j)*dRdSigmaLtd(i,j)
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

#endif /* GM_EXCLUDE_CLIPPING */

      ELSE IF (GM_taper_scheme.EQ.'ac02') THEN

#ifdef GM_EXCLUDE_AC02_TAP

        STOP 'Need to compile without "#define GM_EXCLUDE_AC02_TAP"'

#else  /* GM_EXCLUDE_AC02_TAP */

C-      New Scheme (A. & C. 2002): relax part of the small slope approximation
C         compute the true slope (no approximation) 
C         but still neglect Kxy & Kyx (assumed to be zero)

        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          dRdSigmaLtd(i,j)= 
     &                        dSigmaDx(i,j)*dSigmaDx(i,j)
     &                      + dSigmaDy(i,j)*dSigmaDy(i,j)
     &                      + dSigmaDr(i,j)*dSigmaDr(i,j)
          taperFct(i,j) = 1. _d 0
c
          IF (dRdSigmaLtd(i,j).NE.0.) THEN
             dRdSigmaLtd(i,j)=1. _d 0
     &            / ( dRdSigmaLtd(i,j) )
             SlopeSqr(i,j)=(dSigmaDx(i,j)*dSigmaDx(i,j)
     &            +dSigmaDy(i,j)*dSigmaDy(i,j))*dRdSigmaLtd(i,j)
             SlopeX(i,j)=-dSigmaDx(i,j)
     &            *dRdSigmaLtd(i,j)*dSigmaDr(i,j)
             SlopeY(i,j)=-dSigmaDy(i,j)
     &            *dRdSigmaLtd(i,j)*dSigmaDr(i,j)
cph             T11(i,j)=(dSigmaDr(i,j)**2)*dRdSigmaLtd(i,j)
          ENDIF
#ifndef ALLOWW_AUTODIFF_TAMC
cph-- this part does not adjoint well
          IF ( SlopeSqr(i,j) .GT. maxSlopeSqr .AND.
     &         SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
           taperFct(i,j) = maxSlopeSqr/SlopeSqr(i,j)
          ELSE IF ( SlopeSqr(i,j) .GT. GM_slopeSqCutoff ) THEN
           taperFct(i,j) = 0. _d 0
          ENDIF
#endif
         ENDDO
        ENDDO

#endif /* GM_EXCLUDE_AC02_TAP */

      ELSE

#ifdef GM_EXCLUDE_TAPERING

        STOP 'Need to compile without "#define GM_EXCLUDE_TAPERING"'

#else  /* GM_EXCLUDE_TAPERING */

C----------------------------------------------------------------------

C- Compute the slope, no clipping, but avoid reverse slope in negatively
C                                  stratified (Sigma_Z > 0) region :

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE dSigmaDr(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF ( dSigmaDr(i,j) .NE. 0. ) THEN
           IF (dSigmaDr(i,j).GE.(-GM_Small_Number))
     &         dSigmaDr(i,j) = -GM_Small_Number
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cnostore CADJ STORE dSigmaDx(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDy(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
cnostore CADJ STORE dSigmaDr(:,:)     = comlev1_bibj, key=ikey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF ( dSigmaDr(i,j) .EQ. 0. ) THEN
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
           dRdSigmaLtd(i,j) = 1. _d 0 / dSigmaDr(i,j)
           SlopeX(i,j)=-dSigmaDx(i,j)*dRdSigmaLtd(i,j)
           SlopeY(i,j)=-dSigmaDy(i,j)*dRdSigmaLtd(i,j)
c          SlopeX(i,j) = -dSigmaDx(i,j)/dSigmaDr(i,j)
c          SlopeY(i,j) = -dSigmaDy(i,j)/dSigmaDr(i,j)
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
          IF ( SlopeSqr(i,j) .GT. GM_slopeSqCutoff ) THEN
             slopeSqr(i,j) = GM_slopeSqCutoff
             taperFct(i,j) = 0. _d 0
          ENDIF
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
          ELSE IF ( SlopeSqr(i,j) .GT. maxSlopeSqr .AND.
     &             SlopeSqr(i,j) .LT. GM_slopeSqCutoff )  THEN
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
          ELSE IF ( SlopeSqr(i,j) .GT. maxSlopeSqr .AND.
     &             SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
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
          ELSE IF ( SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
           Smod=sqrt(SlopeSqr(i,j))
           taperFct(i,j)=op5*( 1. _d 0 + tanh( (GM_Scrit-Smod)/GM_Sd ))
          ENDIF
         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'ldd97') THEN

C-      Large, Danabasoglu and Doney, JPO 1997
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF (SlopeSqr(i,j) .EQ. 0.) THEN
           taperFct(i,j) = 1. _d 0
          ELSEIF ( SlopeSqr(i,j) .LT. GM_slopeSqCutoff ) THEN
           Smod=sqrt(SlopeSqr(i,j))
           f1=op5*( 1. _d 0 + tanh( (GM_Scrit-Smod)/GM_Sd ))
           Rnondim= -depthZ/(Lrho(i,j)*Smod)
           IF ( Rnondim.GE.1. _d 0 ) THEN
             f2 = 1. _d 0
           ELSE
             f2 = op5*( 1. _d 0 + SIN( fpi*(Rnondim-op5) ))
           ENDIF
           taperFct(i,j)=f1*f2
          ENDIF

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.NE.' ') THEN
        STOP 'GMREDI_SLOPE_LIMIT: Bad GM_taper_scheme'
       ENDIF

#endif /* GM_EXCLUDE_TAPERING */

      ENDIF

#endif /* ALLOW_GMREDI */

      RETURN
      END
