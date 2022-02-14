#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CStartOfInterface
      SUBROUTINE GMREDI_SLOPE_PSI(
     O             taperX, taperY,
     U             SlopeX, SlopeY,
     U             dSigmaDrW,dSigmaDrS,
     I             LrhoW, LrhoS, depthZ, k,
     I             bi,bj, myThid )
C     *==========================================================*
C     | SUBROUTINE GMREDI_SLOPE_PSI                              |
C     | o Calculate slopes for use in GM/Redi tensor             |
C     *==========================================================*
C     | On entry:                                                |
C     |            dSigmaDrW,S  contains the d/dz Sigma          |
C     |            SlopeX/Y     contains X/Y gradients of sigma  |
C     |            depthZ       contains the depth (< 0 !) [m]   |
C     | On exit:                                                 |
C     |            dSigmaDrW,S  contains the effective dSig/dz   |
C     |            SlopeX/Y     contains X/Y slopes              |
C     |            taperFct     contains tapering funct. value ; |
C     |                         = 1 when using no tapering       |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GMREDI.h"
#include "PARAMS.h"

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     == Routine arguments ==
C
      _RL taperX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL taperY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SlopeX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SlopeY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDrW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDrS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL LrhoW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL LrhoS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS depthZ
      INTEGER k,bi,bj,myThid
CEndOfInterface

#ifdef ALLOW_GMREDI
#ifdef GM_BOLUS_ADVEC

C     == Local variables ==
      _RL dSigmaDrLtd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL f1,Smod,f2,Rnondim
      _RL maxSlopeSqr
      _RL slopeCutoff
      _RL fpi
      PARAMETER(fpi=3.141592653589793047592d0)
      INTEGER i,j
#ifdef GMREDI_WITH_STABLE_ADJOINT
      _RL slopeTmpSpec,slopeMaxSpec
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER igmkey, kkey
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      slopeCutoff = SQRT( GM_slopeSqCutoff )

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

#ifdef ALLOW_AUTODIFF
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          dSigmaDrLtd(i,j) = 0. _d 0
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF */

C-      Cox 1987 "Slope clipping"
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          dSigmaDrLtd(i,j) = -(GM_Small_Number+
     &     ABS(SlopeX(i,j))*GM_rMaxSlope)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrLtd(:,:)  = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDrW(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          IF (dSigmaDrW(i,j).GE.dSigmaDrLtd(i,j))
     &        dSigmaDrW(i,j) = dSigmaDrLtd(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrW(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          SlopeX(i,j) = -SlopeX(i,j)/dSigmaDrW(i,j)
          taperX(i,j) = 1. _d 0
         ENDDO
        ENDDO

C-- Y-comp

#ifdef ALLOW_AUTODIFF
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          dSigmaDrLtd(i,j) = 0. _d 0
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF */
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          dSigmaDrLtd(i,j) = -(GM_Small_Number+
     &     ABS(SlopeY(i,j))*GM_rMaxSlope)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrLtd(:,:)  = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDrS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF (dSigmaDrS(i,j).GE.dSigmaDrLtd(i,j))
     &        dSigmaDrS(i,j) = dSigmaDrLtd(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          SlopeY(i,j) = -SlopeY(i,j)/dSigmaDrS(i,j)
          taperY(i,j) = 1. _d 0
         ENDDO
        ENDDO

#endif /* GM_EXCLUDE_CLIPPING */

      ELSEIF (GM_taper_scheme.EQ.'fm07') THEN

        STOP 'GMREDI_SLOPE_PSI: AdvForm not yet implemented for fm07'

      ELSE

#ifdef GM_EXCLUDE_TAPERING

        STOP 'Need to compile without "#define GM_EXCLUDE_TAPERING"'

#else  /* GM_EXCLUDE_TAPERING */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopeX(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDrW(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

C- Compute the slope, no clipping, but avoid reverse slope in negatively
C                                  stratified (Sigma_Z > 0) region :
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          IF (dSigmaDrW(i,j).GE.-GM_Small_Number)
     &        dSigmaDrW(i,j) = -GM_Small_Number
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dsigmadrW(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          SlopeX(i,j) = -SlopeX(i,j)/dSigmaDrW(i,j)
          taperX(i,j) = 1. _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopex(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        IF (GM_taper_scheme.NE.'stableGmAdjTap') THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          IF ( ABS(SlopeX(i,j)) .GE. slopeCutoff ) THEN
             SlopeX(i,j) = SIGN(slopeCutoff,SlopeX(i,j))
             taperX(i,j) = 0. _d 0
          ENDIF
         ENDDO
        ENDDO
        ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopeY(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDrS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF (dSigmaDrS(i,j).GE.-GM_Small_Number)
     &        dSigmaDrS(i,j) = -GM_Small_Number
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dsigmadrS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          SlopeY(i,j) = -SlopeY(i,j)/dSigmaDrS(i,j)
          taperY(i,j) = 1. _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopey(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        IF (GM_taper_scheme.NE.'stableGmAdjTap') THEN
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( ABS(SlopeY(i,j)) .GE. slopeCutoff ) THEN
             SlopeY(i,j) = SIGN(slopeCutoff,SlopeY(i,j))
             taperY(i,j) = 0. _d 0
          ENDIF
         ENDDO
        ENDDO
        ENDIF

C- Compute the tapering function for the GM+Redi tensor :

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopeX(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE slopeY(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

       IF (GM_taper_scheme.EQ.'linear') THEN

C-      Simplest adiabatic tapering = Smax/Slope (linear)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          Smod = ABS(SlopeX(i,j))
          IF ( Smod .GT. GM_maxSlope .AND.
     &           Smod .LT. slopeCutoff )
     &           taperX(i,j)=GM_maxSlope/(Smod+GM_Small_Number)
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Smod = ABS(SlopeY(i,j))
          IF ( Smod .GT. GM_maxSlope .AND.
     &           Smod .LT. slopeCutoff )
     &           taperY(i,j)=GM_maxSlope/(Smod+GM_Small_Number)
         ENDDO
        ENDDO

       ELSEIF ( GM_taper_scheme.EQ.'gkw91' .OR.
     &          GM_taper_scheme.EQ.'ac02' ) THEN

C-      Gerdes, Koberle and Willebrand, Clim. Dyn. 1991
        maxSlopeSqr = GM_maxSlope*GM_maxSlope
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          IF ( ABS(SlopeX(i,j)) .GT. GM_maxSlope .AND.
     &           ABS(SlopeX(i,j)) .LT. slopeCutoff )
     &           taperX(i,j)=maxSlopeSqr/
     &           ( SlopeX(i,j)*SlopeX(i,j) + GM_Small_Number )
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( ABS(SlopeY(i,j)) .GT. GM_maxSlope .AND.
     &           ABS(SlopeY(i,j)) .LT. slopeCutoff )
     &           taperY(i,j)=maxSlopeSqr/
     &           ( SlopeY(i,j)*SlopeY(i,j) + GM_Small_Number )
         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'dm95') THEN

C-      Danabasoglu and McWilliams, J. Clim. 1995
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          Smod = ABS(SlopeX(i,j))
          taperX(i,j)=op5*( 1. _d 0 + TANH( (GM_Scrit-Smod)/GM_Sd ))
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Smod = ABS(SlopeY(i,j))
          taperY(i,j)=op5*( 1. _d 0 + TANH( (GM_Scrit-Smod)/GM_Sd ))
         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'ldd97') THEN

C-      Large, Danabasoglu and Doney, JPO 1997

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          Smod = ABS(SlopeX(i,j))
          IF ( Smod .LT. slopeCutoff ) THEN
            f1=op5*( 1. _d 0 + TANH( (GM_Scrit-Smod)/GM_Sd ))
            IF (Smod.NE.0.) THEN
              Rnondim = -depthZ/(LrhoW(i,j)*Smod)
            ELSE
              Rnondim = 1.
            ENDIF
            IF ( Rnondim.GE.1. _d 0 ) THEN
              f2 = 1. _d 0
            ELSE
              f2 = op5*( 1. _d 0 + SIN( fpi*(Rnondim-op5) ))
            ENDIF
            taperX(i,j)=f1*f2
          ENDIF
         ENDDO
        ENDDO

        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Smod = ABS(SlopeY(i,j))
          IF ( Smod .LT. slopeCutoff ) THEN
            f1=op5*( 1. _d 0 + TANH( (GM_Scrit-Smod)/GM_Sd ))
            IF (Smod.NE.0.) THEN
              Rnondim = -depthZ/(LrhoS(i,j)*Smod)
            ELSE
              Rnondim = 1.
            ENDIF
            IF ( Rnondim.GE.1. _d 0 ) THEN
              f2 = 1. _d 0
            ELSE
              f2 = op5*( 1. _d 0 + SIN( fpi*(Rnondim-op5) ))
            ENDIF
            taperY(i,j)=f1*f2
          ENDIF
         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'stableGmAdjTap') THEN

#ifndef GMREDI_WITH_STABLE_ADJOINT

        STOP 'Need to compile wth "#define GMREDI_WITH_STABLE_ADJOINT"'

#else  /* GMREDI_WITH_STABLE_ADJOINT */

c special choice for adjoint/optimization of parameters
c (~ strong clipping, reducing non linearity of psi=f(K))

        slopeMaxSpec=1. _d -4

CADJ STORE slopeX(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE slopeY(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
       slopeTmpSpec=ABS(SlopeX(i,j))
       IF ( slopeTmpSpec .GT. slopeMaxSpec ) then
        SlopeX(i,j)=5.*SlopeX(i,j)*slopeMaxSpec/slopeTmpSpec
       ELSE
        SlopeX(i,j)=5.*SlopeX(i,j)
       ENDIF
       taperX(i,j)=1.
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
       slopeTmpSpec=ABS(SlopeY(i,j))
       IF ( slopeTmpSpec .GT. slopeMaxSpec ) then
        SlopeY(i,j)=5.*SlopeY(i,j)*slopeMaxSpec/slopeTmpSpec
       ELSE
        SlopeY(i,j)=5.*SlopeY(i,j)
       ENDIF
       taperY(i,j)=1.
         ENDDO
        ENDDO
#endif /* GMREDI_WITH_STABLE_ADJOINT */

       ELSEIF (GM_taper_scheme.NE.' ') THEN
        STOP 'GMREDI_SLOPE_PSI: Bad GM_taper_scheme'
       ENDIF

#endif /* GM_EXCLUDE_TAPERING */

      ENDIF

#endif /* BOLUS_ADVEC */
#endif /* ALLOW_GMREDI */

      RETURN
      END
