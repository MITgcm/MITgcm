C $Header: /u/gcmpack/MITgcm/model/src/Attic/div_g.F,v 1.1 1998/06/01 20:45:23 adcroft Exp $

#include "CPP_EEOPTIONS.h"

C     /==========================================================\
C     | S/R DIV_G                                                |
C     | o Form the right hand-side of the surface pressure eqn.  |
C     |==========================================================|
C     \==========================================================/
      SUBROUTINE DIV_G( 
     I        bi,bj,iMin,iMax,jMin,jMax,
     I        K,
     I        xA,yA,
     I        myThid)

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "CG2D.h"

C     == Routine arguments ==
C     pH - Hydrostatic pressure
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     kUp, kDown, kM1                - Index for upper and lower layers.
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

      IF ( k .EQ. nZ ) THEN
C      Initialise source term on first pass
       DO j=jMin,jMax
        DO i=iMin,iMax
         cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj) 
     &    -freeSurfFac*_zA(i,j,bi,bj)*
     &     cg2d_x(I  ,J  ,bi,bj)/deltaTMom/deltaTMom
        ENDDO
       ENDDO
      ENDIF

      DO j=jMin,jMax
       DO i=iMin,iMax
        pf(i,j) = xA(i,j)*gUNm1(i,j,k,bi,bj) / deltaTmom
       ENDDO
      ENDDO
      DO j=1,sNy
       DO i=1,sNx
        cg2d_b(i,j,bi,bj) =  cg2d_b(i,j,bi,bj) +
     &   pf(i+1,j)-pf(i,j)
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        pf(i,j) = yA(i,j)*gVNm1(i,j,k,bi,bj) / deltatmom
       ENDDO
      ENDDO

      DO j=1,sNy
       DO i=1,sNx
        cg2d_b(i,j,bi,bj) =  cg2d_b(i,j,bi,bj) +
     &   pf(i,j+1)-pf(i,j)
       ENDDO
      ENDDO

      RETURN
      END
