C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_salt_plume.F,v 1.1 2012/02/04 17:21:23 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_SALT_PLUME
C     !INTERFACE:
      SUBROUTINE THSICE_SALT_PLUME(
     I                  iceFrac, sOce,
     I                  frw2oc,
     I                  iMin,iMax, jMin,jMax, bi, bj,
     I                  callSequence, myTime, myIter, myThid )


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
C     iceFrac     :: fraction of grid area covered in ice
C     sOce        :: surface level oceanic salinity [g/kg]
C     frw2oc      :: (additional) fresh water flux to ocean [kg/m2/s] (+=dwn)
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C     bi,bj       :: tile indices
C     callSequence:: indicate from where this S/R is called from
C     myTime      :: current Time of simulation [s]
C     myIter      :: current Iteration number in simulation
C     myThid      :: my Thread Id number
      _RL     iceFrac(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     sOce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     frw2oc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi,bj
      INTEGER callSequence
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME
C     !LOCAL VARIABLES:
      INTEGER i,j

C- Note: The net freezing (frw2oc < 0) considered here can be lower than
C        the true bottom/lateral freezing when surface melting occurs at 
C        the same time, thus underestimating the amount of rejected salt.
C        This case (surf. melting + bottom freezing) will not be sustained
C        and might not happen very often (in spring ?).

      IF ( callSequence.EQ.0 ) THEN
C--   called after CALC_THICKN 

C-    Initialisation:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
            saltPlumeFlux(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
C-      salt rejected when seawater freeze (frw2oc < 0),
C       freezing from CALC_THICKN needs iceFrac factor
        DO j = jMin, jMax
         DO i = iMin, iMax
          IF ( iceFrac(i,j).GT.0. _d 0 .AND. 
     &          frw2oc(i,j).LT.0. _d 0 ) THEN
            saltPlumeFlux(i,j,bi,bj) = 
     &         -frw2oc(i,j)*( sOce(i,j) - saltIce )*SPsalFRAC
     &                     **iceFrac(i,j)
          ENDIF
         ENDDO
        ENDDO

      ELSE
C--   called after THSICE_EXTEND 

C-      Add salt rejected when seawater freeze (frw2oc < 0),
C       no iceFrac factor for freezing from THSICE_EXTEND
        DO j = jMin, jMax
         DO i = iMin, iMax
          IF ( iceFrac(i,j).GT.0. _d 0 .AND. 
     &          frw2oc(i,j).LT.0. _d 0 ) THEN
            saltPlumeFlux(i,j,bi,bj) = saltPlumeFlux(i,j,bi,bj) 
     &         -frw2oc(i,j)*( sOce(i,j) - saltIce )*SPsalFRAC
          ENDIF
         ENDDO
        ENDDO

      ENDIF

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
