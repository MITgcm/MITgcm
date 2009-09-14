C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_export_atmconfig.F,v 1.2 2009/09/14 16:29:30 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_EXPORT_ATMCONFIG( myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_EXPORT_ATMCONFIG                          |
C     | o Routine for exporting atmos. config to coupling level. |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "ATMIDS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
      Real*8  tmpBuf8(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER i,j,bi,bj

C     Send my configuration information to the coupler
C     o Send atmos. model orography map
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          tmpBuf8(i,j,bi,bj) = Ro_surf(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL COMPSEND_R8TILES( atmDepthName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, tmpBuf8 )

      RETURN
      END
