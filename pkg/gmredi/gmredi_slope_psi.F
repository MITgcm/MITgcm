#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: GMREDI_SLOPE_PSI
C     !INTERFACE:
      SUBROUTINE GMREDI_SLOPE_PSI(
     O             taperX, taperY,
     U             SlopeX, SlopeY,
     U             dSigmaDrW, dSigmaDrS,
     I             LrhoW, LrhoS, rDepth, k,
     I             bi, bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_SLOPE_PSI                              |
C     | o Calculate slopes for use in GM advective form          |
C     *==========================================================*
C     | On entry:                                                |
C     |     dSigmaDrW,S  contains the -d/dz Sigma if Z-coords    |
C     |                           but  d/dp Sigma if P-coords    |
C     |     SlopeX/Y     contains X/Y gradients of sigma         |
C     |     rDepth       depth (> 0) in r-Unit from the surface  |
C     | On exit:                                                 |
C     |     dSigmaDrW,S  contains the effective dSig/dz          |
C     |     SlopeX/Y     contains X/Y slopes                     |
C     |     taperFct     contains tapering funct. value ;        |
C     |                  = 1 when using no tapering              |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GMREDI.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
      _RL taperX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL taperY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SlopeX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SlopeY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDrW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDrS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL LrhoW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL LrhoS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rDepth
      INTEGER k,bi,bj,myThid

#ifdef ALLOW_GMREDI
#ifdef GM_BOLUS_ADVEC

C     !LOCAL VARIABLES:
      _RL dSigmaDrLtd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL f1, Smod, f2, Rnondim
      _RL maxSlopeSqr
      _RL slopeCutoff
      _RL loc_maxSlope, loc_rMaxSlope
      _RL fpi
      PARAMETER( fpi = PI )
      INTEGER i, j
#ifdef GMREDI_WITH_STABLE_ADJOINT
      _RL slopeMaxSpec
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER igmkey, kkey
#endif
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      slopeCutoff = SQRT( GM_slopeSqCutoff )
C-    Note regarding slopeCutoff: current code by-passes the reduction of taper
C     fct when slope is larger than "slopeCutoff", and this is not very safe !
C     However, with default GM_Small_Number (=1.E-20) and default value of
C     GM_slopeSqCutoff (=1.E+48), this is unlikely to occur as it would require
C     |sigmaX/Y| > 1.e+4

      loc_maxSlope  = GM_maxSlope*wUnit2rVel(k)
      loc_rMaxSlope = GM_rMaxSlope*rVel2wUnit(k)

#ifdef ALLOW_AUTODIFF_TAMC
      igmkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
      kkey   = (igmkey-1)*Nr + k
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
          dSigmaDrLtd(i,j) = GM_Small_Number
     &     + ABS(SlopeX(i,j))*loc_rMaxSlope
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrLtd(:,:)  = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDrW(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          IF (dSigmaDrW(i,j).LE.dSigmaDrLtd(i,j))
     &        dSigmaDrW(i,j) = dSigmaDrLtd(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrW(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          SlopeX(i,j) = SlopeX(i,j)/dSigmaDrW(i,j)
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
          dSigmaDrLtd(i,j) = GM_Small_Number
     &     + ABS(SlopeY(i,j))*loc_rMaxSlope
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrLtd(:,:)  = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDrS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF (dSigmaDrS(i,j).LE.dSigmaDrLtd(i,j))
     &        dSigmaDrS(i,j) = dSigmaDrLtd(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDrS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          SlopeY(i,j) = SlopeY(i,j)/dSigmaDrS(i,j)
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
C                                  stratified (dSigmaDr < 0) region :
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          IF (dSigmaDrW(i,j).LE.GM_Small_Number)
     &        dSigmaDrW(i,j) = GM_Small_Number
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dsigmaDrW(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          SlopeX(i,j) = SlopeX(i,j)/dSigmaDrW(i,j)
          taperX(i,j) = 1. _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopeX(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
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
          IF (dSigmaDrS(i,j).LE.GM_Small_Number)
     &        dSigmaDrS(i,j) = GM_Small_Number
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dsigmaDrS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          SlopeY(i,j) = SlopeY(i,j)/dSigmaDrS(i,j)
          taperY(i,j) = 1. _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE slopeY(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
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
          IF ( Smod .GT. loc_maxSlope .AND.
     &         Smod .LT. slopeCutoff )
     &      taperX(i,j) = loc_maxSlope/(Smod+GM_Small_Number)
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Smod = ABS(SlopeY(i,j))
          IF ( Smod .GT. loc_maxSlope .AND.
     &         Smod .LT. slopeCutoff )
     &      taperY(i,j) = loc_maxSlope/(Smod+GM_Small_Number)
         ENDDO
        ENDDO

       ELSEIF ( GM_taper_scheme.EQ.'gkw91' .OR.
     &          GM_taper_scheme.EQ.'ac02' ) THEN

C-      Gerdes, Koberle and Willebrand, Clim. Dyn. 1991
        maxSlopeSqr = loc_maxSlope*loc_maxSlope
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          Smod = ABS(SlopeX(i,j))
          IF ( Smod .GT. loc_maxSlope .AND.
     &         Smod .LT. slopeCutoff )
     &      taperX(i,j) = maxSlopeSqr/
     &           ( SlopeX(i,j)*SlopeX(i,j) + GM_Small_Number )
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Smod = ABS(SlopeY(i,j))
          IF ( Smod .GT. loc_maxSlope .AND.
     &         Smod .LT. slopeCutoff )
     &      taperY(i,j) = maxSlopeSqr/
     &           ( SlopeY(i,j)*SlopeY(i,j) + GM_Small_Number )
         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'dm95') THEN

C-      Danabasoglu and McWilliams, J. Clim. 1995
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          Smod = ABS(SlopeX(i,j))*rVel2wUnit(k)
          taperX(i,j) = op5*( oneRL + TANH( (GM_Scrit-Smod)/GM_Sd ))
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Smod = ABS(SlopeY(i,j))*rVel2wUnit(k)
          taperY(i,j) = op5*( oneRL + TANH( (GM_Scrit-Smod)/GM_Sd ))
         ENDDO
        ENDDO

       ELSEIF (GM_taper_scheme.EQ.'ldd97') THEN

C-      Large, Danabasoglu and Doney, JPO 1997

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          Smod = ABS(SlopeX(i,j))
          IF ( Smod .LT. slopeCutoff ) THEN
            f1 = op5*( oneRL
     &               + TANH( (GM_Scrit-Smod*rVel2wUnit(k))/GM_Sd ) )
            IF ( Smod.NE.zeroRL ) THEN
              Rnondim = rDepth/( LrhoW(i,j)*Smod )
            ELSE
              Rnondim = 1. _d 0
            ENDIF
            IF ( Rnondim.GE.oneRL ) THEN
              f2 = 1. _d 0
            ELSE
              f2 = op5*( oneRL + SIN( fpi*(Rnondim-op5) ))
            ENDIF
            taperX(i,j) = f1*f2
          ENDIF
         ENDDO
        ENDDO

        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Smod = ABS(SlopeY(i,j))
          IF ( Smod .LT. slopeCutoff ) THEN
            f1 = op5*( oneRL
     &               + TANH( (GM_Scrit-Smod*rVel2wUnit(k))/GM_Sd ) )
            IF ( Smod.NE.zeroRL ) THEN
              Rnondim = rDepth/( LrhoS(i,j)*Smod )
            ELSE
              Rnondim = 1. _d 0
            ENDIF
            IF ( Rnondim.GE.oneRL ) THEN
              f2 = 1. _d 0
            ELSE
              f2 = op5*( oneRL + SIN( fpi*(Rnondim-op5) ))
            ENDIF
            taperY(i,j) = f1*f2
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
          Smod = ABS(SlopeX(i,j))
          IF ( Smod .GT. slopeMaxSpec ) THEN
           SlopeX(i,j) = 5.*SlopeX(i,j)*slopeMaxSpec/Smod
          ELSE
           SlopeX(i,j) = 5.*SlopeX(i,j)
          ENDIF
          taperX(i,j) = 1.
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Smod = ABS(SlopeY(i,j))
          IF ( Smod .GT. slopeMaxSpec ) THEN
           SlopeY(i,j) = 5.*SlopeY(i,j)*slopeMaxSpec/Smod
          ELSE
           SlopeY(i,j) = 5.*SlopeY(i,j)
          ENDIF
          taperY(i,j) = 1.
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
