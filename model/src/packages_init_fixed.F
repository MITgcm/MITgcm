C $Header: /u/gcmpack/MITgcm/model/src/packages_init_fixed.F,v 1.7 2003/06/12 18:21:34 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PACKAGES_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE PACKAGES_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_INIT_FIXED                            
C     | o Does initialisation of package-related fixed fields     
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     PACKAGES_INIT_FIXED
C       |
C       |-- GAD_INIT
C       |
C       |-- OBCS_INIT_FIXED
C       |
C       |-- FLT_INIT
C       |
C       |-- ZONAL_FILT_INIT
C       |
C       |-- AIM_INITIALISE
C       |
C       |-- LAND_INITIALISE

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid
CEOP

C--   Initialize fixed params for GAD
      CALL GAD_INIT( myThid )

C--   Initialize fixed arrays for OBCS
#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
       CALL OBCS_INIT_FIXED( myThid )
      ENDIF
#endif
#ifdef ALLOW_FLT
C--   Initialise Float positions
      IF (useFLT) THEN
       CALL FLT_INIT(nIter0,startTime,myThid )
      _BARRIER
      ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
C--   Latitude circle filter initialisation
      IF (useZONAL_FILT) THEN
       CALL ZONAL_FILT_INIT(myThid)
      _BARRIER
      ENDIF
#endif

#ifdef ALLOW_AIM
C--   Initialise & Read AIM physical parameters
      IF (useAIM) CALL AIM_INITIALISE( myThid )
#endif

#ifdef ALLOW_LAND
C--   Initialise & Read Land package parameters
      IF (useLand) CALL LAND_INITIALISE( myThid )
#endif

      RETURN
      END
