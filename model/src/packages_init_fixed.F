C $Header: /u/gcmpack/MITgcm/model/src/packages_init_fixed.F,v 1.4 2001/09/26 18:09:16 cnh Exp $
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
C       |-- OBCS_INIT_FIXED
C       |
C       |-- FLT_INIT

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

C--   Initialize fixed arrays for OBCS
#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
       CALL OBCS_INIT_FIXED( myThid )
      ENDIF
#endif
#ifdef ALLOW_FLT
C--   Initialise Float positions
      IF (useFLT) then
       CALL FLT_INIT(nIter0,startTime,myThid )
      _BARRIER
      ENDIF
#endif

      RETURN
      END
