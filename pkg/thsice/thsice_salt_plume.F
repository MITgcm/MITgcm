C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_salt_plume.F,v 1.2 2012/02/08 22:32:24 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_SALT_PLUME
C     !INTERFACE:
      SUBROUTINE THSICE_SALT_PLUME(
     I                  sOce,
     I                  frzSeaWat,
     I                  iMin,iMax, jMin,jMax, bi, bj,
     I                  myTime, myIter, myThid )


C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE THSICE_SALT_PLUME
C     | o Interface Routine to pkg/salt_plume:
C     |   Fill in saltPlumeFlux array (see SALT_PLUME.h)
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "THSICE_PARAMS.h"
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     sOce       :: surface level oceanic salinity [g/kg]
C     frzSeaWat  :: seawater freezing rate (expressed as mass flux) [kg/m^2/s]
C     iMin,iMax  :: computation domain: 1rst index range
C     jMin,jMax  :: computation domain: 2nd  index range
C     bi,bj      :: tile indices
C     myTime     :: current Time of simulation [s]
C     myIter     :: current Iteration number in simulation
C     myThid     :: my Thread Id number
      _RL     sOce     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     frzSeaWat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME
C     !LOCAL VARIABLES:
      INTEGER i,j

C-    Initialisation:
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          saltPlumeFlux(i,j,bi,bj) = 0. _d 0
        ENDDO
      ENDDO
C-    salt rejected when seawater freeze (frw2oc < 0, frzSeaWat > 0):
      DO j = jMin, jMax
        DO i = iMin, iMax
          saltPlumeFlux(i,j,bi,bj) =
     &        frzSeaWat(i,j)*( sOce(i,j) - saltIce )*SPsalFRAC
        ENDDO
      ENDDO

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
