C $Header: /u/gcmpack/MITgcm/model/src/ini_psurf.F,v 1.1 2000/04/05 17:52:37 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_PSURF( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_PSURF                                     |
C     | o Set model initial free-surface height/pressue.         |
C     |==========================================================|
C     | There are several options for setting the initial        |
C     | zonal velocity field.                                    |
C     |  1. Inline code                                          |
C     |  2. Two-dimensional data from a file.                    |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_UVEL 
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER  I,  J

C--   Initialise velocity field to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          cg2d_x(I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     Read an initial state
      IF (pSurfInitFile .NE. ' ') THEN
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XY_RL( pSurfInitFile, ' ', cg2d_x, 0, myThid )
       _END_MASTER(myThid)
      ENDIF
C
      _EXCH_XY_R8(cg2d_x, myThid)

      RETURN
      END
