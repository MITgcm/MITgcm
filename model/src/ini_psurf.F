C $Header: /u/gcmpack/MITgcm/model/src/ini_psurf.F,v 1.11 2009/06/14 21:45:12 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_PSURF
C     !INTERFACE:
      SUBROUTINE INI_PSURF( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_PSURF                                     |
C     | o Set model initial free-surface height/pressure.        |
C     *==========================================================*
C     | There are several options for setting the initial        |
C     | surface displacement (r unit) field.                     |
C     |  1. Inline code                                          |
C     |  2. Two-dimensional data from a file.                    |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SURFACE.h"
#ifdef ALLOW_CD_CODE
#include "CD_CODE_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid ::  Number of this instance of INI_PSURF
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: tiles indices
C     I,J    :: Loop counters
      INTEGER bi, bj
      INTEGER  I,  J
CEOP

C--   Initialise surface position anomaly to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          etaN(I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     Read an initial state
      IF (pSurfInitFile .NE. ' ') THEN
       CALL READ_FLD_XY_RL( pSurfInitFile, ' ', etaN, 0, myThid )
C      fill the overlap (+ BARRIER)
       _EXCH_XY_RL(etaN, myThid)
      ENDIF

#ifdef ALLOW_CD_CODE
C--   By default, initialize etaNm1 with etaN :
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          etaNm1(I,J,bi,bj) = etaN(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     _EXCH_XY_RL(etaNm1, myThid)
#endif

#ifdef EXACT_CONSERV
C--   By default, initialize etaH with etaN :
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          etaH(i,j,bi,bj) = etaN(i,j,bi,bj)
          etaHnm1(i,j,bi,bj) = etaN(i,j,bi,bj)
          dEtaHdt(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* EXACT_CONSERV */

      RETURN
      END
