C $Header: /u/gcmpack/MITgcm/model/src/solve_for_pressure.F,v 1.5 1998/06/09 15:58:36 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SOLVE_FOR_PRESSURE( myThid )
C     /==========================================================\
C     | SUBROUTINE SOLVE_FOR_PRESSURE                            |
C     | o Controls inversion of two and/or three-dimensional     |
C     |   elliptic problems for the pressure field.              |
C     \==========================================================/

C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "CG2D.h"

C     == Routine arguments ==
C     myThid - Number of this instance of SOLVE_FOR_PRESSURE
      INTEGER myThid
CEndOfInterface

C     Local variables
      INTEGER i,j,bi,bj

#ifdef ALLOW_CD
C--   Save previous solution.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          cg2d_xNM1(i,j,bi,bj) = cg2d_x(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif

C--   Find the surface pressure using a two-dimensional conjugate
C--   gradient solver.
C     see CG2D.h for the interface to this routine.
      CALL CG2D(
     I           myThid )

C--   Solve for a three-dimensional pressure term (NH or IGW or both ).
C     see CG3D.h for the interface to this routine.
C*P*  CALL CG3D(
C*P* I           myThid )

      RETURN
      END
