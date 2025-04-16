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
#ifdef ALLOW_ADDFLUID
      _RL unitsFac
#endif
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

C     Implicit treatment of the Barotropic Flow Divergence
      DO j=1,sNy
       DO i=1,sNx+1
        pf(i,j) = implicDiv2Dflow*xA(i,j)*gU(i,j,k,bi,bj) / deltaTMom
       ENDDO
      ENDDO
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

C     Implicit treatment of the Barotropic Flow Divergence
      DO j=1,sNy+1
       DO i=1,sNx
        pf(i,j) = implicDiv2Dflow*yA(i,j)*gV(i,j,k,bi,bj) / deltaTMom
       ENDDO
      ENDDO
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
      unitsFac = mass2rUnit/deltaTMom
      IF ( exactConserv ) unitsFac = unitsFac*implicDiv2Dflow
      IF ( selectAddFluid.GE.1 ) THEN
        DO j=1,sNy
         DO i=1,sNx
          cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj)
     &         - addMass(i,j,k,bi,bj)*unitsFac
         ENDDO
        ENDDO
#ifdef ALLOW_NONHYDROSTATIC
       IF (use3Dsolver) THEN
        DO j=1,sNy
         DO i=1,sNx
          cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj)
     &         - addMass(i,j,k,bi,bj)*unitsFac
         ENDDO
        ENDDO
       ENDIF
#endif
      ENDIF
#endif /* ALLOW_ADDFLUID */

      RETURN
      END
