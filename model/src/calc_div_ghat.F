#include "CPP_OPTIONS.h"
#undef PRESERVE_C69C_OUTPUT

CBOP
C     !ROUTINE: CALC_DIV_GHAT
C     !INTERFACE:
      SUBROUTINE CALC_DIV_GHAT(
     I                bi, bj, oldFreeSurfTerm,
     U                cg2d_b, cg3d_b,
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
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
# include "NH_VARS.h"
#endif
#ifdef ALLOW_ADDFLUID
# include "FFIELDS.h"
#endif
C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj  :: tile indices
C     oldFreeSurfTerm :: old free-surface treatment in NH code (exactConserv=F)
C     cg2d_b  :: Conjugate Gradient 2-D solver : Right-hand side vector
C     cg3d_b  :: Conjugate Gradient 3-D solver : Right-hand side vector
C     myThid  :: my Thread Id number
      INTEGER bi, bj
      LOGICAL oldFreeSurfTerm
      _RL cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_NONHYDROSTATIC
      _RL cg3d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#else
      _RL cg3d_b(1)
#endif
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i, j, k :: Loop counters
C     ks      :: free-surface level index
C     xTrans  :: Transport divided by deltaT, in X-dir
C     yTrans  :: Transport divided by deltaT, in Y-dir
C     locFac  :: temp/local factor from this level
      INTEGER i, j, k, ks
      _RL xTrans(1:sNx+1,1:sNy+1)
      _RL yTrans(1:sNx+1,1:sNy+1)
      _RL locFac
      _RL tmpFac
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

#ifdef PRESERVE_C69C_OUTPUT
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

        IF ( implicDiv2DFlow.EQ.oneRL ) THEN
C     Fully Implicit treatment of the Barotropic Flow Divergence
         DO j=1,sNy+1
          DO i=1,sNx+1
           xTrans(i,j) = xTrans(i,j)*gU(i,j,k,bi,bj) / deltaTMom
           yTrans(i,j) = yTrans(i,j)*gV(i,j,k,bi,bj) / deltaTMom
          ENDDO
         ENDDO
        ELSEIF ( exactConserv ) THEN
c       ELSEIF (nonlinFreeSurf.GT.0) THEN
C     Implicit treatment of the Barotropic Flow Divergence
         DO j=1,sNy+1
          DO i=1,sNx+1
           xTrans(i,j) = implicDiv2DFlow
     &                  *xTrans(i,j)*gU(i,j,k,bi,bj) / deltaTMom
           yTrans(i,j) = implicDiv2DFlow
     &                  *yTrans(i,j)*gV(i,j,k,bi,bj) / deltaTMom
          ENDDO
         ENDDO
        ELSE
C     Explicit+Implicit part of the Barotropic Flow Divergence
C      => Filtering of uVel,vVel is necessary
C-- Now the filter are applied in the_correction_step().
C   We have left this code here to indicate where the filters used to be
C   in the algorithm before JMC moved them to after the pressure solver.
c#ifdef ALLOW_ZONAL_FILT
c        IF (zonal_filt_lat.LT.90.) THEN
c          CALL ZONAL_FILTER(
c    U                     uVel( 1-OLx,1-OLy,k,bi,bj),
c    I                     hFacW(1-OLx,1-OLy,k,bi,bj),
c    I                     0, sNy+1, 1, bi, bj, 1, myThid )
c          CALL ZONAL_FILTER(
c    U                     vVel( 1-OLx,1-OLy,k,bi,bj),
c    I                     hFacS(1-OLx,1-OLy,k,bi,bj),
c    I                     0, sNy+1, 1, bi, bj, 2, myThid )
c        ENDIF
c#endif
         DO j=1,sNy+1
          DO i=1,sNx+1
           xTrans(i,j) = ( implicDiv2DFlow * gU(i,j,k,bi,bj)
     &        + (1. _d 0-implicDiv2DFlow)* uVel(i,j,k,bi,bj)
     &                   ) * xTrans(i,j) / deltaTMom
           yTrans(i,j) = ( implicDiv2DFlow * gV(i,j,k,bi,bj)
     &        + (1. _d 0-implicDiv2DFlow)* vVel(i,j,k,bi,bj)
     &                   ) * yTrans(i,j) / deltaTMom
          ENDDO
         ENDDO
        ENDIF

#else /* PRESERVE_C69C_OUTPUT */

C--   Starts loop on level index k
      DO k=1,Nr

C     Calculate horizontal transport (divided by deltaT ):
        IF ( exactConserv .OR. implicDiv2DFlow.EQ.oneRL ) THEN
         locFac = deepFacC(k)*drF(k)*rhoFacC(k)
         DO j=1,sNy+1
          DO i=1,sNx+1
           xTrans(i,j) = _dyG(i,j,bi,bj)*_hFacW(i,j,k,bi,bj)*locFac
     &                  *gU(i,j,k,bi,bj)
           yTrans(i,j) = _dxG(i,j,bi,bj)*_hFacS(i,j,k,bi,bj)*locFac
     &                  *gV(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ELSE
         locFac = deepFacC(k)*drF(k)*rhoFacC(k)
         DO j=1,sNy+1
          DO i=1,sNx+1
           xTrans(i,j) = _dyG(i,j,bi,bj)*_hFacW(i,j,k,bi,bj)*locFac
     &                 *( gU(i,j,k,bi,bj) + tmpFac*uVel(i,j,k,bi,bj) )
           yTrans(i,j) = _dxG(i,j,bi,bj)*_hFacS(i,j,k,bi,bj)*locFac
     &                 *( gV(i,j,k,bi,bj) + tmpFac*vVel(i,j,k,bi,bj) )
          ENDDO
         ENDDO
        ENDIF

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

#ifdef ALLOW_NONHYDROSTATIC
      IF ( oldFreeSurfTerm ) THEN
C--   Add source term arising from w=d/dt (p_s + p_nh)
        tmpFac = freeSurfFac/deltaTFreeSurf
        DO j=1,sNy
         DO i=1,sNx
          ks = kSurfC(i,j,bi,bj)
          IF ( ks.LE.Nr ) THEN
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
#ifdef PRESERVE_C69C_OUTPUT
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
#else
     &       -tmpFac*_rA(i,j,bi,bj)*deepFac2F(ks)
#endif
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,ks,bi,bj)*recip_Bo(i,j,bi,bj) )
           cg3d_b(i,j,ks,bi,bj) = cg3d_b(i,j,ks,bi,bj)
#ifdef PRESERVE_C69C_OUTPUT
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
#else
     &       -tmpFac*_rA(i,j,bi,bj)*deepFac2F(ks)
#endif
     &         *( etaN(i,j,bi,bj)
     &           +phi_nh(i,j,ks,bi,bj)*recip_Bo(i,j,bi,bj) )
          ENDIF
         ENDDO
        ENDDO
# ifdef EXACT_CONSERV
      ELSEIF ( solveForDeltaP ) THEN
# else
      ELSE
# endif
#endif /* ALLOW_NONHYDROSTATIC */
C--   Add source term arising from w=d/dt (p_s)
#ifdef EXACT_CONSERV
# ifndef ALLOW_NONHYDROSTATIC
      IF ( solveForDeltaP ) THEN
# endif
        IF ( implicDiv2DFlow.NE.oneRL ) THEN
         tmpFac = freeSurfFac*( oneRL/implicDiv2DFlow - oneRL )
         DO j=1,sNy
          DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
c#ifdef EXACT_CONSERV
     &       -tmpFac*_rA(i,j,bi,bj)*deepFac2F(ks)*dEtaHdt(i,j,bi,bj)
c#endif
          ENDDO
         ENDDO
        ENDIF
      ELSEIF ( exactConserv ) THEN
        tmpFac = freeSurfFac/(implicDiv2DFlow*deltaTFreeSurf)
        DO j=1,sNy
         DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
#ifdef PRESERVE_C69C_OUTPUT
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
     &         * etaH(i,j,bi,bj)
#else
     &       -tmpFac*_rA(i,j,bi,bj)*deepFac2F(ks)*etaH(i,j,bi,bj)
#endif
         ENDDO
        ENDDO
      ELSE
#endif /* EXACT_CONSERV */
        tmpFac = freeSurfFac/deltaTFreeSurf
        DO j=1,sNy
         DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
#ifdef PRESERVE_C69C_OUTPUT
     &       -freeSurfFac*_rA(i,j,bi,bj)*deepFac2F(ks)
     &         /deltaTMom/deltaTFreeSurf
     &         * etaN(i,j,bi,bj)
#else
     &       -tmpFac*_rA(i,j,bi,bj)*deepFac2F(ks)*etaN(i,j,bi,bj)
#endif
         ENDDO
        ENDDO
#if ( defined EXACT_CONSERV || defined ALLOW_NONHYDROSTATIC )
      ENDIF
#endif

      RETURN
      END
