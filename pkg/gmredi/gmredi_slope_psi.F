C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_slope_psi.F,v 1.6 2003/01/21 19:34:13 heimbach Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CStartOfInterface
      SUBROUTINE GMREDI_SLOPE_PSI(
     I             dSigmaDrW,dSigmaDrS,
     I             depthZ,K,
     U             SlopeX, SlopeY,
     O             taperX, taperY,
     I             bi,bj, myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_SLOPE_PSI                              |
C     | o Calculate slopes for use in GM/Redi tensor             |
C     |==========================================================|
C     | On entry:                                                |
C     |            dSigmaDrW conatins the d/dz Sigma             |
C     |            SlopeX/Y     contains X/Y gradients of sigma  |
C     |            depthZ       conatins the height (m) of level |
C     | On exit:                                                 |
C     |            dSigmaDrW conatins the effective dSig/dz      |
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

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     == Routine arguments ==
C
      _RL SlopeX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL SlopeY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDrW(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDrS(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL taperX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL taperY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL depthZ
      INTEGER K,bi,bj,myThid
CEndOfInterface

#ifdef ALLOW_GMREDI
#ifdef GM_BOLUS_ADVEC

C     == Local variables ==
      _RL gradSmod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dSigmaDrLtd(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL drdsigmaltd(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL  SlopeSqr(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL f1,Smod,f2,Rnondim,Cspd,Lrho
      _RL maxSlopeSqr
      _RL tmpvar
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
      igmkey = (act1 + 1) + act2*max1
     &                    + act3*max1*max2
     &                    + act4*max1*max2*max3
      kkey = (igmkey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

      IF (GM_taper_scheme.EQ.'orig' .OR.
     &    GM_taper_scheme.EQ.'clipping') THEN

#ifdef GM_EXCLUDE_CLIPPING

        STOP 'Need to compile without "#define GM_EXCLUDE_CLIPPING"'

#else  /* GM_EXCLUDE_CLIPPING */

C-      Original implementation in mitgcmuv
C       (this turns out to be the same as Cox slope clipping)

C-- X-comp

#ifdef ALLOW_AUTODIFF_TAMC
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        dSigmaDrLtd(i,j) = 0. _d 0
       ENDDO
      ENDDO
#endif /* ALLOW_AUTODIFF_TAMC */

C-      Cox 1987 "Slope clipping"
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          dsigmadrltd(i,j) = -(GM_Small_Number+
     &     abs(SlopeX(i,j))*GM_rMaxSlope)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrltd(:,:)  = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDrW(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF (dSigmaDrW(i,j).GE.dsigmadrltd(i,j))
     &        dSigmaDrW(i,j) = dsigmadrltd(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrW(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          SlopeX(i,j) = -SlopeX(i,j)/dSigmaDrW(i,j)
          taperX(i,j)=1. _d 0
         ENDDO
        ENDDO

C-- Y-comp

#ifdef ALLOW_AUTODIFF_TAMC
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        dSigmaDrLtd(i,j) = 0. _d 0
       ENDDO
      ENDDO
#endif /* ALLOW_AUTODIFF_TAMC */
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          dsigmadrltd(i,j) = -(GM_Small_Number+
     &     abs(SlopeY(i,j))*GM_rMaxSlope)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrltd(:,:)  = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDrS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF (dSigmaDrS(i,j).GE.dsigmadrltd(i,j))
     &        dSigmaDrS(i,j) = dsigmadrltd(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          SlopeY(i,j) = -SlopeY(i,j)/dSigmaDrS(i,j)
          taperY(i,j)=1. _d 0
         ENDDO
        ENDDO

#endif /* GM_EXCLUDE_CLIPPING */

      ELSE

#ifdef GM_EXCLUDE_TAPERING

        STOP 'Need to compile without "#define GM_EXCLUDE_TAPERING"'

#else  /* GM_EXCLUDE_TAPERING */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopeX(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDrW(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

        tmpvar = sqrt( GM_slopeSqCutoff )

C- Compute the slope, no clipping, but avoid reverse slope in negatively 
C                                  stratified (Sigma_Z > 0) region :
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          dsigmadrltd(i,j) = -GM_Small_Number
          IF (dSigmaDrW(i,j).GE.dsigmadrltd(i,j))
     &        dSigmaDrW(i,j) = dsigmadrltd(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dsigmadrW(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          drdsigmaltd(i,j) = 1./dSigmaDrW(i,j)
          SlopeX(i,j) = -SlopeX(i,j)/dSigmaDrW(i,j)
          taperX(i,j)= 1. _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopex(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF ( ABS(SlopeX(i,j)) .GE. tmpvar ) THEN
             SlopeX(i,j) = SIGN(tmpvar,SlopeX(i,j))
             taperX(i,j) = 0. _d 0
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopeY(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDrS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          dsigmadrltd(i,j) = -GM_Small_Number
          IF (dSigmaDrS(i,j).GE.dsigmadrltd(i,j))
     &        dSigmaDrS(i,j) = dsigmadrltd(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dsigmadrS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          SlopeY(i,j) = -SlopeY(i,j)/dSigmaDrS(i,j)
          taperY(i,j)=1. _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopey(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
          IF ( ABS(SlopeY(i,j)) .GE. tmpvar ) THEN
             SlopeY(i,j) = SIGN(tmpvar,SlopeY(i,j))
             taperY(i,j) = 0. _d 0
          ENDIF
         ENDDO
        ENDDO

C- Compute the tapering function for the GM+Redi tensor :

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopeX(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE slopeY(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

       IF (GM_taper_scheme.EQ.'linear') THEN

C-      Simplest adiabatic tapering = Smax/Slope (linear)
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          Smod = abs(SlopeX(i,j))
          IF ( Smod .GT. GM_maxSlope .AND.
     &           Smod .LT. tmpvar )
     &           taperX(i,j)=GM_maxSlope/(Smod+GM_Small_Number)
          Smod = abs(SlopeY(i,j))
          IF ( Smod .GT. GM_maxSlope .AND.
     &           Smod .LT. tmpvar ) 
     &           taperY(i,j)=GM_maxSlope/(Smod+GM_Small_Number)

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'gkw91') THEN

C-      Gerdes, Koberle and Willebrand, Clim. Dyn. 1991

        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          IF ( abs(SlopeX(i,j)) .GT. GM_maxSlope .AND.
     &           abs(SlopeX(i,j)) .LT. tmpvar ) 
     &           taperX(i,j)=maxSlopeSqr/
     &           ( SlopeX(i,j)*SlopeX(i,j) + GM_Small_Number )
          IF ( abs(SlopeY(i,j)) .GT. GM_maxSlope .AND.
     &           abs(SlopeY(i,j)) .LT. tmpvar )
     &           taperY(i,j)=maxSlopeSqr/
     &           ( SlopeY(i,j)*SlopeY(i,j) + GM_Small_Number )

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'dm95') THEN

C-      Danabasoglu and McWilliams, J. Clim. 1995
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          Smod = abs(SlopeX(i,j))
          taperX(i,j)=op5*( 1. _d 0 + tanh( (GM_Scrit-Smod)/GM_Sd ))
          Smod = abs(SlopeY(i,j))
          taperY(i,j)=op5*( 1. _d 0 + tanh( (GM_Scrit-Smod)/GM_Sd ))

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'ldd97') THEN

C-      Large, Danabasoglu and Doney, JPO 1997
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1

          Cspd=2. _d 0
          Lrho=100. _d 3
          if (fCori(i,j,bi,bj).NE.0.) Lrho=Cspd/abs(fCori(i,j,bi,bj))
          Lrho=min(Lrho , 100. _d 3)
          Lrho=max(Lrho , 15. _d 3)

          Smod = abs(SlopeX(i,j))
          if ( Smod .LT. tmpvar ) then
          f1=op5*( 1. _d 0 + tanh( (GM_Scrit-Smod)/GM_Sd ))
          if (Smod.NE.0.) then
            Rnondim=depthZ/(Lrho*Smod)
          else
            Rnondim=0.
          endif
          f2=op5*( 1. _d 0 + sin( fpi*(Rnondim-op5)))
          taperX(i,j)=f1*f2
          endif

          Smod = abs(SlopeY(i,j))
          if ( Smod .LT. tmpvar ) then
          f1=op5*( 1. _d 0 + tanh( (GM_Scrit-Smod)/GM_Sd ))
          if (Smod.NE.0.) then
            Rnondim=depthZ/(Lrho*Smod)
          else
            Rnondim=0.
          endif
          f2=op5*( 1. _d 0 + sin( fpi*(Rnondim-op5)))
          taperY(i,j)=f1*f2
          endif

         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.NE.' ') THEN
        STOP 'GMREDI_SLOPE_PSI: Bad GM_taper_scheme'
       ENDIF

#endif /* GM_EXCLUDE_TAPERING */

      ENDIF


#endif /* BOLUS_ADVEC */
#endif /* ALLOW_GMREDI */

      RETURN
      END
