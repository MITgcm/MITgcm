C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_pnh.F,v 1.1 2001/06/29 17:14:49 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE INI_PNH( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_PNH                                       |
C     | o Set model initial non-hydrostatic pressure.            |
C     |==========================================================|
C     | There are several options for setting the initial        |
C     | surface displacement (r unit) field.                     |
C     |  1. Inline code                                          |
C     |  2. Two-dimensional data from a file.                    |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_PSURF 
      INTEGER myThid

C     == Local variables ==
      INTEGER bi,bj,I,J,K

#ifdef ALLOW_NONHYDROSTATIC

C--   Initialise surface position anomaly to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K = 1, NR
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           phi_nh(I,J,K,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     Read an initial state
c     IF (pnhInitFile .NE. ' ') THEN
c      _BEGIN_MASTER( myThid )
c      CALL READ_FLD_XY_RL( pnhInitFile, ' ', phi_nh, 0, myThid )
c      _END_MASTER(myThid)
c     ENDIF
C
      _EXCH_XYZ_R8(phi_nh, myThid)

#endif /* ALLOW_NONHYDROSTATIC */
      RETURN
      END
