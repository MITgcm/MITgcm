C $Header: /u/gcmpack/MITgcm/model/src/calc_div_ghat.F,v 1.10 2000/03/14 17:47:25 adcroft Exp $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R CALC_DIV_GHAT                                        |
C     | o Form the right hand-side of the surface pressure eqn.  |
C     |==========================================================|
C     \==========================================================/
      SUBROUTINE CALC_DIV_GHAT( 
     I        bi,bj,iMin,iMax,jMin,jMax,
     I        K,
     I        xA,yA,
     I        myThid)

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
#endif

C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     k                              - Index of layer.
C     xA, yA                         - Cell face areas
C     myThid - Instance number for this innvocation of CALC_MOM_RHS
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER K
      _RS xA (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL pf (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C--   Pressure equation source term
C     Term is the vertical integral of the divergence of the 
C     time tendency terms along with a relaxation term that
C     pulls div(U) + dh/dt back toward zero.

      IF ( k .EQ. Nr ) THEN
C      Initialise source term on first pass
       DO j=1,sNy
        DO i=1,sNx
C Note: The source term containing cg2d_x and cg3d_x (at k=1)
C       has been moved to solve_for_pressure.F for convenience.
         cg2d_b(i,j,bi,bj) =
     &     freeSurfFac*_rA(i,j,bi,bj)*horiVertRatio*( 0.
#ifdef USE_NATURAL_BCS
     &      +EmPmR(I,J,bi,bj)/deltaTMom
#endif
     &     )
        ENDDO
       ENDDO
      ENDIF

      DO j=1,sNy
       DO i=1,sNx+1
        pf(i,j) = xA(i,j)*gUNm1(i,j,k,bi,bj) / deltaTmom
       ENDDO
      ENDDO
      DO j=1,sNy
       DO i=1,sNx
        cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj) +
     &   pf(i+1,j)-pf(i,j)
       ENDDO
      ENDDO

#ifdef ALLOW_NONHYDROSTATIC
      IF (nonHydrostatic) THEN
       DO j=1,sNy
        DO i=1,sNx
         cg3d_b(i,j,k,bi,bj) = pf(i+1,j)-pf(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif

      DO j=1,sNy+1
       DO i=1,sNx
        pf(i,j) = yA(i,j)*gVNm1(i,j,k,bi,bj) / deltatmom
       ENDDO
      ENDDO

      DO j=1,sNy
       DO i=1,sNx
        cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj) +
     &   pf(i,j+1)-pf(i,j)
       ENDDO
      ENDDO

#ifdef ALLOW_NONHYDROSTATIC
      IF (nonHydrostatic) THEN
       DO j=1,sNy
        DO i=1,sNx
         cg3d_b(i,j,k,bi,bj) = cg3d_b(i,j,k,bi,bj) +
     &    pf(i,j+1)-pf(i,j)
        ENDDO
       ENDDO
      ENDIF
#endif

      RETURN
      END
