C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/ocn_export_sst.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE OCN_EXPORT_SST(myThid)
C     /==========================================================\
C     | SUBROUTINE OCN_EXPORT_SST                                |
C     | o Routine for exporting ocean sea-surface temperature    |
C     |   to the coupling layer.                                 |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "OCNIDS.h"
#include "OCNCPL.h"
#ifdef ALLOW_SEAICE
#include "ICE.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I,J :: Loop variables
      INTEGER bi,bj
      INTEGER I,J

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     Send my sea-surface temperatur to coupling layer
C     o Information is sent on the ocean model grid.
      DO J=1,sNy
       DO I=1,sNx
        IF ( hFacC(I,J,1,bi,bj) .EQ. 0. ) THEN
         SSTocn2cpl(I,J,bi,bj) = theta(I,J,1,bi,bj)
        ELSE
         SSTocn2cpl(I,J,bi,bj) = theta(I,J,1,bi,bj)
        ENDIF
       ENDDO
      ENDDO
#ifdef ALLOW_SEAICE
      DO J=1,sNy
       DO I=1,sNx
        IF ( iceMask(I,J,bi,bj) .NE. 0. ) THEN
         SSTocn2cpl(I,J,bi,bj) = 0.
        ENDIF
       ENDDO
      ENDDO
#endif
       ENDDO
      ENDDO

      CALL COMPSEND_R8TILES( ocnSSTName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, SSTocn2cpl )

      RETURN
      END
