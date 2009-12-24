C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_export_atmconfig.F,v 1.3 2009/12/24 16:49:08 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: ATM_EXPORT_ATMCONFIG

C !INTERFACE:
      SUBROUTINE ATM_EXPORT_ATMCONFIG( myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ATM_EXPORT_ATMCONFIG
C     | o Routine for exporting atmos. config to coupling level.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "ATMIDS.h"

C !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid
CEOP

C     == Local variables ==
      Real*8  tmpBuf8(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER i,j,bi,bj

C     Send my configuration information to the coupler
C     o Send atmos. model orography map
      _BARRIER
      _BEGIN_MASTER( myThid )
      DO bj = 1,nSy
       DO bi = 1,nSx
c     DO bj = myByLo(myThid), myByHi(myThid)
c      DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          tmpBuf8(i,j,bi,bj) = Ro_surf(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL COMPSEND_R8TILES(
     I              atmDepthName, sNx, OLx, sNy, OLy, 1, nSx, nSy,
     I              tmpBuf8 )
      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
