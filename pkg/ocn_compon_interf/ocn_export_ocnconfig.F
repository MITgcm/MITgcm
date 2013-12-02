C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_export_ocnconfig.F,v 1.4 2013/12/02 22:16:19 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: OCN_EXPORT_OCNCONFIG

C !INTERFACE:
      SUBROUTINE OCN_EXPORT_OCNCONFIG( myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OCN_EXPORT_OCNCONFIG
C     | o Routine for exporting ocean config to coupling level.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OCNIDS.h"
#include "OCNCPL.h"

C !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid

C !LOCAL VARIABLES:
      INTEGER i,j,bi,bj
      INTEGER tmpFld(1)
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ocMxlD2cpl(i,j,bi,bj) = hFacC(i,j,1,bi,bj)*drF(1)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     Send my configuration information to the coupler
      _BARRIER
      _BEGIN_MASTER( myThid )

C     o Send number of coupler time-steps to do for this run
      tmpFld(1) = nTimeSteps
      CALL COMPSEND_I4VEC( 'nCouplingSteps', 1, tmpFld )

C     o Send ocean model bathymetry map
      CALL COMPSEND_R8TILES(
     I              ocnMxlDName, sNx, OLx, sNy, OLy, 1, nSx, nSy,
     I              ocMxlD2cpl )

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
