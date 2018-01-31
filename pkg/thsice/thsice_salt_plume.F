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
#ifdef ALLOW_ATM_COMPON_INTERF
# include "CPL_PARAMS.h"
# include "ATMCPL.h"
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

#if defined(ALLOW_SALT_PLUME) || defined(ALLOW_ATM_COMPON_INTERF)
C     !LOCAL VARIABLES:
      LOGICAL fillCplVar
      INTEGER i,j
      _RL locVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_ATM_COMPON_INTERF
      _RL cplTimeFraction
#endif

      fillCplVar = .FALSE.
#ifdef ALLOW_ATM_COMPON_INTERF
      IF ( useCoupler ) fillCplVar = atm_cplExch_SaltPl
#endif

      IF ( useSALT_PLUME .OR. fillCplVar ) THEN
C-    Initialisation:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           locVar(i,j) = 0. _d 0
         ENDDO
        ENDDO
C-    salt rejected when seawater freeze (frw2oc < 0, frzSeaWat > 0):
        DO j = jMin, jMax
         DO i = iMin, iMax
           locVar(i,j) = frzSeaWat(i,j)*( sOce(i,j) - saltIce )
         ENDDO
        ENDDO
      ENDIF

#ifdef ALLOW_SALT_PLUME
      IF ( useSALT_PLUME ) THEN
C-    Take a fraction of it to store in saltPlumeFlux ; used for
C     vertical redistribution in water column (see pkg in Salt-Plume)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           saltPlumeFlux(i,j,bi,bj) = locVar(i,j)*SPsalFRAC
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_SALT_PLUME */

#ifdef ALLOW_ATM_COMPON_INTERF
      IF ( fillCplVar ) THEN
C-    Cumulate salt-flux for export to Coupler interface
C Note: SPsalFRAC will be applied when Ocean Compon is applying
C       CPL-imported field to saltPlumeFlux in S/R SALT_PLUME_DO_EXCH
        cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
        saltPlmFlxTime(bi,bj) = saltPlmFlxTime(bi,bj) + cplTimeFraction
        DO j=1,sNy
         DO i=1,sNx
           saltPlmFlx_cpl(i,j,bi,bj) = saltPlmFlx_cpl(i,j,bi,bj)
     &                               + locVar(i,j)*cplTimeFraction
         ENDDO
        ENDDO
      ENDIF
#endif

#endif /* ALLOW_SALT_PLUME or ALLOW_ATM_COMPON_INTERF */

      RETURN
      END
