C $Header: /u/gcmpack/MITgcm/model/src/calc_div_ghat.F,v 1.27 2012/11/09 22:37:05 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_DIV_GHAT
C     !INTERFACE:
      SUBROUTINE CALC_DIV_GHAT(
     I                bi,bj,k,
     U                cg2d_b, cg3d_b,
     I                myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CALC_DIV_GHAT
C     | o Form the right hand-side of the surface pressure eqn.
C     *==========================================================*
C     | Right hand side of pressure equation is divergence
C     | of veclocity tendency (GHAT) term along with a relaxation
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
#include "DYNVARS.h"
#ifdef ALLOW_ADDFLUID
# include "FFIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj  :: tile indices
C     k       :: Index of layer.
C     cg2d_b  :: Conjugate Gradient 2-D solver : Right-hand side vector
C     cg3d_b  :: Conjugate Gradient 3-D solver : Right-hand side vector
C     myThid  :: Instance number for this call of CALC_DIV_GHAT
      INTEGER bi,bj
      INTEGER k
      _RL cg2d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_NONHYDROSTATIC
      _RL cg3d_b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#else
      _RL cg3d_b(1)
#endif
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j    :: Loop counters
C     xA, yA :: Cell vertical face areas
C     pf     :: Intermediate array for building RHS source term.
      INTEGER i,j
      _RS xA (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pf (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C     Calculate vertical face areas
      DO j=1,sNy+1
        DO i=1,sNx+1
          xA(i,j) = _dyG(i,j,bi,bj)*deepFacC(k)
     &              *drF(k)*_hFacW(i,j,k,bi,bj)*rhoFacC(k)
          yA(i,j) = _dxG(i,j,bi,bj)*deepFacC(k)
     &              *drF(k)*_hFacS(i,j,k,bi,bj)*rhoFacC(k)
        ENDDO
      ENDDO

C--   Pressure equation source term
C     Term is the vertical integral of the divergence of the
C     time tendency terms along with a relaxation term that
C     pulls div(U) + dh/dt back toward zero.

      IF (implicDiv2Dflow.EQ.1.) THEN
C     Fully Implicit treatment of the Barotropic Flow Divergence
        DO j=1,sNy
         DO i=1,sNx+1
          pf(i,j) = xA(i,j)*gU(i,j,k,bi,bj) / deltaTMom
         ENDDO
        ENDDO
      ELSEIF (exactConserv) THEN
c     ELSEIF (nonlinFreeSurf.GT.0) THEN
C     Implicit treatment of the Barotropic Flow Divergence
        DO j=1,sNy
         DO i=1,sNx+1
          pf(i,j) = implicDiv2Dflow
     &             *xA(i,j)*gU(i,j,k,bi,bj) / deltaTMom
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
        DO j=1,sNy
         DO i=1,sNx+1
          pf(i,j) = ( implicDiv2Dflow * gU(i,j,k,bi,bj)
     &     + (1. _d 0-implicDiv2Dflow)* uVel(i,j,k,bi,bj)
     &               ) * xA(i,j) / deltaTMom
         ENDDO
        ENDDO
      ENDIF
      DO j=1,sNy
       DO i=1,sNx
        cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj) +
     &   pf(i+1,j)-pf(i,j)
       ENDDO
      ENDDO

#ifdef ALLOW_NONHYDROSTATIC
      IF (use3Dsolver) THEN
       DO j=1,sNy
        DO i=1,sNx
         cg3d_b(i,j,k,bi,bj) = ( pf(i+1,j)-pf(i,j) )
        ENDDO
       ENDDO
      ENDIF
#endif

      IF (implicDiv2Dflow.EQ.1.) THEN
C     Fully Implicit treatment of the Barotropic Flow Divergence
        DO j=1,sNy+1
         DO i=1,sNx
          pf(i,j) = yA(i,j)*gV(i,j,k,bi,bj) / deltatmom
         ENDDO
        ENDDO
      ELSEIF (exactConserv) THEN
c     ELSEIF (nonlinFreeSurf.GT.0) THEN
C     Implicit treatment of the Barotropic Flow Divergence
        DO j=1,sNy+1
         DO i=1,sNx
          pf(i,j) = implicDiv2Dflow
     &             *yA(i,j)*gV(i,j,k,bi,bj) / deltatmom
         ENDDO
        ENDDO
      ELSE
C     Explicit+Implicit part of the Barotropic Flow Divergence
        DO j=1,sNy+1
         DO i=1,sNx
          pf(i,j) = ( implicDiv2Dflow * gV(i,j,k,bi,bj)
     &     + (1. _d 0-implicDiv2Dflow)* vVel(i,j,k,bi,bj)
     &               ) * yA(i,j) / deltaTMom
         ENDDO
        ENDDO
      ENDIF
      DO j=1,sNy
       DO i=1,sNx
        cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj) +
     &   pf(i,j+1)-pf(i,j)
       ENDDO
      ENDDO

#ifdef ALLOW_NONHYDROSTATIC
      IF (use3Dsolver) THEN
       DO j=1,sNy
        DO i=1,sNx
         cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &                       + ( pf(i,j+1)-pf(i,j) )
        ENDDO
       ENDDO
      ENDIF
#endif

#ifdef ALLOW_ADDFLUID
      IF ( selectAddFluid.GE.1 ) THEN
        DO j=1,sNy
         DO i=1,sNx
          cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &         - addMass(i,j,k,bi,bj)*mass2rUnit/deltaTMom
         ENDDO
        ENDDO
#ifdef ALLOW_NONHYDROSTATIC
       IF (use3Dsolver) THEN
        DO j=1,sNy
         DO i=1,sNx
          cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &         - addMass(i,j,k,bi,bj)*mass2rUnit/deltaTMom
         ENDDO
        ENDDO
       ENDIF
#endif
      ENDIF
#endif /* ALLOW_ADDFLUID */

      RETURN
      END
