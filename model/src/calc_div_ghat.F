#include "CPP_OPTIONS.h"
#include "BACKWARD_COMPAT_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_DIV_GHAT
C     !INTERFACE:
      SUBROUTINE CALC_DIV_GHAT(
     I                bi, bj, oldFreeSurfTerm,
     U                cg2d_b, cg3d_b,
     O                wSurfP2d,
     I                myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CALC_DIV_GHAT
C     | o Form the right hand-side of the surface pressure eqn.
C     *==========================================================*
C     | Right hand side of pressure equation is divergence
C     | of provisional velocity (GHAT) term along with a relaxation
C     | term equal to the barotropic flow field divergence.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
# include "NH_VARS.h"
#endif
C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj  :: tile indices
C     oldFreeSurfTerm :: old free-surface treatment in NH code (exactConserv=F)
C     cg2d_b  :: Conjugate Gradient 2-D solver : Right-hand side vector
C     cg3d_b  :: Conjugate Gradient 3-D solver : Right-hand side vector
C     wSurfP2d  :: surface vertical velocity after 2-D solver
C     myThid  :: my Thread Id number
      INTEGER bi, bj
      LOGICAL oldFreeSurfTerm
      _RL cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_NONHYDROSTATIC
      _RL cg3d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wSurfP2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#else
      _RL cg3d_b(1), wSurfP2d(1)
#endif
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i, j, k :: Loop counters
C     ks      :: free-surface level index
C     xTrans  :: Transport divided by deltaT, in X-dir
C     yTrans  :: Transport divided by deltaT, in Y-dir
C     locFac  :: temp/local factor from this level
C     rec_dTfs :: freeSurfFac divided by ( detalTFreeSurf x implicDiv2DFlow )
      INTEGER i, j, k, ks
      _RL xTrans(1:sNx+1,1:sNy+1)
      _RL yTrans(1:sNx+1,1:sNy+1)
      _RL tmpFac
#ifndef PRESERVE_C69C_OUTPUT
      _RL locFac, rec_dTfs
#endif
#if ( defined ALLOW_NONHYDROSTATIC && defined EXACT_CONSERV )
      _RL rec_dT2, locGamma
#endif
#ifdef ALLOW_ADDFLUID
      _RL unitsFac
#endif
CEOP

#ifdef ALLOW_ADDFLUID
      unitsFac = mass2rUnit
# ifdef PRESERVE_C69C_OUTPUT
      unitsFac = mass2rUnit/deltaTMom
      IF ( exactConserv ) unitsFac = unitsFac*implicDiv2DFlow
# endif
#endif
#ifndef PRESERVE_C69C_OUTPUT
      rec_dTfs = freeSurfFac /( implicDiv2DFlow*deltaTFreeSurf )
#endif

#ifdef ALLOW_NONHYDROSTATIC
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         wSurfP2d(i,j,bi,bj) = 0. _d 0
       ENDDO
      ENDDO
#else
      wSurfP2d(1) = 0. _d 0
#endif

#ifdef PRESERVE_C69C_OUTPUT
C--   add Fresh-water contribution to "cg2d_b"
      IF ( useRealFreshWaterFlux.AND.fluidIsWater
     &     .AND. ( selectNHfreeSurf.EQ.0 .OR. usingPCoords ) ) THEN
         tmpFac = freeSurfFac*mass2rUnit*implicDiv2DFlow
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = tmpFac*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)
     &                       / deltaTMom
          ENDDO
         ENDDO
      ENDIF

C- Note: no reason for this reverse k loop except changing it does affect
C        results at machine truncation level
      DO k=Nr,1,-1

C     Calculate vertical face areas
        DO j=1,sNy+1
         DO i=1,sNx+1
          xTrans(i,j) = _dyG(i,j,bi,bj)*deepFacC(k)
     &                 *drF(k)*_hFacW(i,j,k,bi,bj)*rhoFacC(k)
          yTrans(i,j) = _dxG(i,j,bi,bj)*deepFacC(k)
     &                 *drF(k)*_hFacS(i,j,k,bi,bj)*rhoFacC(k)
         ENDDO
        ENDDO

C     Implicit treatment of the Barotropic Flow Divergence
        DO j=1,sNy+1
         DO i=1,sNx+1
          xTrans(i,j) = implicDiv2DFlow
     &                 *xTrans(i,j)*gU(i,j,k,bi,bj) / deltaTMom
          yTrans(i,j) = implicDiv2DFlow
     &                 *yTrans(i,j)*gV(i,j,k,bi,bj) / deltaTMom
         ENDDO
        ENDDO

#else /* PRESERVE_C69C_OUTPUT */

C--   Starts loop on level index k
      DO k=1,Nr

C     Calculate horizontal transport (divided by deltaT ):
        locFac = deepFacC(k)*drF(k)*rhoFacC(k)
        DO j=1,sNy+1
         DO i=1,sNx+1
           xTrans(i,j) = _dyG(i,j,bi,bj)*_hFacW(i,j,k,bi,bj)*locFac
     &                  *gU(i,j,k,bi,bj)
           yTrans(i,j) = _dxG(i,j,bi,bj)*_hFacS(i,j,k,bi,bj)*locFac
     &                  *gV(i,j,k,bi,bj)
         ENDDO
        ENDDO

#endif /* PRESERVE_C69C_OUTPUT */

C--   2-D Pressure equation source term
C     Term is the vertical integral of the divergence of the
C     time tendency terms along with a relaxation term that
C     pulls div(U) + dh/dt back toward zero.

#ifdef PRESERVE_C69C_OUTPUT

        DO j=1,sNy
         DO i=1,sNx
          cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &                      +  xTrans(i+1,j) - xTrans(i,j)
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
          cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &                      +  yTrans(i,j+1) - yTrans(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_NONHYDROSTATIC
C--   3-D Pressure equation source term
C     Add same contribution at each level (more terms added after CG2D call)
        IF ( use3Dsolver ) THEN
         DO j=1,sNy
          DO i=1,sNx
           cg3d_b(i,j,k,bi,bj) = ( xTrans(i+1,j) - xTrans(i,j) )
          ENDDO
         ENDDO
         DO j=1,sNy
          DO i=1,sNx
           cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &                         + ( yTrans(i,j+1) - yTrans(i,j) )
          ENDDO
         ENDDO
        ENDIF
#endif

#else /* PRESERVE_C69C_OUTPUT */

        DO j=1,sNy
         DO i=1,sNx
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &                       + ( ( xTrans(i+1,j) - xTrans(i,j) )
     &                         + ( yTrans(i,j+1) - yTrans(i,j) ) )
         ENDDO
        ENDDO
#ifdef ALLOW_NONHYDROSTATIC
C--   3-D Pressure equation source term
C     Add same contribution at each level (more terms added after CG2D call)
        IF ( use3Dsolver ) THEN
         DO j=1,sNy
          DO i=1,sNx
           cg3d_b(i,j,k,bi,bj) = ( xTrans(i+1,j) - xTrans(i,j) )
     &                         + ( yTrans(i,j+1) - yTrans(i,j) )
          ENDDO
         ENDDO
        ENDIF
#endif

#endif /* PRESERVE_C69C_OUTPUT */

#ifdef ALLOW_ADDFLUID
C--   add source term from "addMass"
        IF ( selectAddFluid.GE.1 ) THEN
          DO j=1,sNy
           DO i=1,sNx
            cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &           - addMass(i,j,k,bi,bj)*unitsFac
           ENDDO
          ENDDO
#ifdef ALLOW_NONHYDROSTATIC
         IF (use3Dsolver) THEN
          DO j=1,sNy
           DO i=1,sNx
            cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &           - addMass(i,j,k,bi,bj)*unitsFac
           ENDDO
          ENDDO
         ENDIF
#endif
        ENDIF
#endif /* ALLOW_ADDFLUID */

C--   end of loop on level index k
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifndef PRESERVE_C69C_OUTPUT
C--   add Fresh-water contribution to "cg2d_b", "cg3d_b" or to wSurfP2d in case
C     use3Dsolver & exactConserv, to be added later (with scaling ?)
      IF ( useRealFreshWaterFlux.AND.fluidIsWater ) THEN
#ifdef ALLOW_NONHYDROSTATIC
       IF ( use3Dsolver .AND. exactConserv .AND. usingZCoords ) THEN
         DO j=1,sNy
          DO i=1,sNx
           wSurfP2d(i,j,bi,bj) = mass2rUnit*EmPmR(i,j,bi,bj)
          ENDDO
         ENDDO
       ELSE
#endif /* ALLOW_NONHYDROSTATIC */
        IF ( usingPCoords .OR. uniformFreeSurfLev
     &                    .OR. .NOT.deepAtmosphere ) THEN
         ks = 1
         IF ( usingPCoords ) ks = Nr+1
         tmpFac = freeSurfFac*mass2rUnit*deepFac2F(ks)
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &                       + tmpFac*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSE
         tmpFac = freeSurfFac*mass2rUnit
         DO j=1,sNy
          DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &         + deepFac2F(ks)*tmpFac*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#ifdef ALLOW_NONHYDROSTATIC
        IF ( use3Dsolver ) THEN
C-      case ( oldFreeSurfTerm.OR.usingPCoords ): add EmPmR directly to cg3d_b
         IF ( usingPCoords .OR. uniformFreeSurfLev  ) THEN
           IF ( usingZCoords ) THEN
             ks = 1
             tmpFac = freeSurfFac*mass2rUnit*deepFac2F(ks)
           ELSE
             ks = Nr
             tmpFac = freeSurfFac*mass2rUnit*deepFac2F(ks+1)
           ENDIF
           DO j=1,sNy
            DO i=1,sNx
              cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &          + tmpFac*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)
            ENDDO
           ENDDO
         ELSE
           tmpFac = freeSurfFac*mass2rUnit
           DO j=1,sNy
            DO i=1,sNx
             ks = kSurfC(i,j,bi,bj)
             IF ( ks.LE.Nr )
     &        cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
     &          + tmpFac*deepFac2F(ks)*_rA(i,j,bi,bj)*EmPmR(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
        ENDIF
C      end if/else use3Dsolver & exactConserv &. usingZCoords
       ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
      ENDIF
#endif /* ndef PRESERVE_C69C_OUTPUT */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_NONHYDROSTATIC
      IF ( oldFreeSurfTerm ) THEN
C--   Add source term arising from w=d/dt (p_s + p_nh)
        DO j=1,sNy
         DO i=1,sNx
          ks = kSurfC(i,j,bi,bj)
          IF ( ks.LE.Nr ) THEN
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
#ifdef PRESERVE_C69C_OUTPUT
     &       - freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
#else
     &       - rec_dTfs*_rA(i,j,bi,bj)*deepFac2F(ks)
#endif
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,ks,bi,bj)*recip_Bo(i,j,bi,bj) )
           cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
#ifdef PRESERVE_C69C_OUTPUT
     &       - freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
#else
     &       - rec_dTfs*_rA(i,j,bi,bj)*deepFac2F(ks)
#endif
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,ks,bi,bj)*recip_Bo(i,j,bi,bj) )
          ENDIF
         ENDDO
        ENDDO
#endif /* ALLOW_NONHYDROSTATIC */
C--   Add source term arising from w=d/dt (p_s)
#ifdef EXACT_CONSERV
# ifdef ALLOW_NONHYDROSTATIC
#ifdef PRESERVE_C69C_OUTPUT
      ELSEIF ( selectNHfreeSurf.GE.1 ) THEN
#else
      ELSEIF ( use3Dsolver ) THEN
#endif
        IF ( .NOT.solveForDeltaP ) THEN
         tmpFac = oneRL/(implicDiv2DFlow*deltaTFreeSurf)
         DO j=1,sNy
          DO i=1,sNx
           wSurfP2d(i,j,bi,bj) = wSurfP2d(i,j,bi,bj)
     &                         - tmpFac*EtaH(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSEIF ( implicDiv2DFlow.NE.oneRL ) THEN
         tmpFac = oneRL/implicDiv2DFlow - oneRL
         DO j=1,sNy
          DO i=1,sNx
           wSurfP2d(i,j,bi,bj) = wSurfP2d(i,j,bi,bj)
     &                         - tmpFac*dEtaHdt(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#ifdef PRESERVE_C69C_OUTPUT
        IF ( useRealFreshWaterFlux .AND. fluidIsWater
     &                             .AND. usingZCoords ) THEN
         DO j=1,sNy
          DO i=1,sNx
           wSurfP2d(i,j,bi,bj) = wSurfP2d(i,j,bi,bj)
     &                 + mass2rUnit*EmPmR(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif
        IF ( selectNHfreeSurf.NE.2 ) THEN
#ifdef PRESERVE_C69C_OUTPUT
         tmpFac = freeSurfFac*implicDiv2DFlow/deltaTMom
#else
         tmpFac = freeSurfFac
#endif
         DO j=1,sNy
          DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       + tmpFac*deepFac2F(ks)*_rA(i,j,bi,bj)*wSurfP2d(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
        ks = 1
        IF ( selectNHfreeSurf.GE.1 ) THEN
         rec_dT2 = oneRL / ( implicitNHPress * deltaTMom
     &                     * implicDiv2DFlow * deltaTFreeSurf )
         DO j=1,sNy
          DO i=1,sNx
           locGamma = rec_dT2 * drC(1)*recip_Bo(i,j,bi,bj)
           wSurfP2d(i,j,bi,bj) = ( wSurfP2d(i,j,bi,bj)
     &                           + locGamma*wVel(i,j,ks,bi,bj)
     &                           )/( oneRL + locGamma )
          ENDDO
         ENDDO
        ENDIF
        IF ( selectNHfreeSurf.EQ.2 ) THEN
         tmpFac = freeSurfFac*deepFac2F(ks)
         DO j=1,sNy
          DO i=1,sNx
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       + tmpFac*_rA(i,j,bi,bj)*wSurfP2d(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
      ELSEIF ( solveForDeltaP ) THEN
# else /* ALLOW_NONHYDROSTATIC */
      IF ( solveForDeltaP ) THEN
# endif /* ALLOW_NONHYDROSTATIC */
        IF ( implicDiv2DFlow.NE.oneRL ) THEN
         tmpFac = freeSurfFac*( oneRL/implicDiv2DFlow - oneRL )
         DO j=1,sNy
          DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &       - tmpFac*_rA(i,j,bi,bj)*deepFac2F(ks)*dEtaHdt(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
      ELSEIF ( exactConserv ) THEN
        DO j=1,sNy
         DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
#ifdef PRESERVE_C69C_OUTPUT
     &       - freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
     &         * etaH(i,j,bi,bj)
#else
     &       - rec_dTfs*_rA(i,j,bi,bj)*deepFac2F(ks)*etaH(i,j,bi,bj)
#endif
         ENDDO
        ENDDO
      ELSE
#else /* EXACT_CONSERV */
# ifdef ALLOW_NONHYDROSTATIC
      ELSE
# endif
#endif /* EXACT_CONSERV */
        DO j=1,sNy
         DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
#ifdef PRESERVE_C69C_OUTPUT
     &       - freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
     &         * etaN(i,j,bi,bj)
#else
     &       - rec_dTfs*_rA(i,j,bi,bj)*deepFac2F(ks)*etaN(i,j,bi,bj)
#endif
         ENDDO
        ENDDO
#if ( defined EXACT_CONSERV || defined ALLOW_NONHYDROSTATIC )
      ENDIF
#endif

      RETURN
      END
