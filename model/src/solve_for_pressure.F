C $Header: /u/gcmpack/MITgcm/model/src/solve_for_pressure.F,v 1.3 1998/04/24 02:10:21 cnh Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE SOLVE_FOR_PRESSURE( myThid )
C     /==========================================================\
C     | SUBROUTINE SOLVE_FOR_PRESSURE                            |
C     | o Controls inversion of two and/or three-dimensional     |
C     |   elliptic problems for the pressure field.              |
C     \==========================================================/

C     == Routine arguments ==
C     myThid - Number of this instance of SOLVE_FOR_PRESSURE
      INTEGER myThid
CEndOfInterface

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
