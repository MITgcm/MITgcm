C $Header: /u/gcmpack/MITgcm/model/src/calc_div_ghat.F,v 1.4 1998/09/08 01:37:49 cnh Exp $

#include "CPP_EEOPTIONS.h"

C     /==========================================================\
C     | S/R CALC_DIV_GHAT                                        |
C     | o Form the right hand-side of the surface pressure eqn.  |
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

CcnhDEbugStarts
      INTEGER i1, j1
CcnhDebugEnds

C--   Pressure equation source term
C     Term is the vertical integral of the divergence of the 
C     time tendency terms along with a relaxation term that
C     pulls div(U) + dh/dt back toward zero.

      IF ( k .EQ. Nr ) THEN
C      Initialise source term on first pass
       DO j=jMin,jMax
        DO i=iMin,iMax
C        cg2d_b(i,j,bi,bj) = cg2d_b(i,j,bi,bj) 
C    &    -freeSurfFac*_rA(i,j,bi,bj)*
C    &     cg2d_x(I  ,J  ,bi,bj)/deltaTMom/deltaTMom
         cg2d_b(i,j,bi,bj) =
     &    -freeSurfFac*_rA(i,j,bi,bj)*horiVertRatio*
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

CcnhDebugSTarts
C     IF ( K .EQ. 10 .OR. K .EQ. 1 ) THEN
C      i1 = 54
C      j1 = 26
C      WRITE(0,*) ' @ I = ', i1, ' , J = ', j1, ' K = ', K
C      WRITE(0,*) '  cg2d_b = ', cg2d_b(i1,j1,bi,bj)
C     ENDIF
CcnhDebugEnds


      RETURN
      END
