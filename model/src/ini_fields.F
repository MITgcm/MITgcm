C $Header: /u/gcmpack/MITgcm/model/src/ini_fields.F,v 1.16 2003/10/30 12:00:41 edhill Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_FIELDS
C     !INTERFACE:
      SUBROUTINE INI_FIELDS( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_FIELDS                                     
C     | o Initialise model fields                                 
C     *==========================================================*
C     | Initial conditions of the model state variables are       
C     | set here. For continuation runs a restart file will be    
C     | used. For original runs custom rules can be included      
C     | here.                                                     
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_FIELDS
      INTEGER myThid
CEOP

C--   Set model variables to initial/restart conditions
      IF ( startTime .EQ. 0. .AND.  nIter0 .EQ. 0
     &     .AND. pickupSuff .EQ. ' ' ) THEN
       CALL INI_VEL( myThid )
       CALL INI_THETA( myThid )
       CALL INI_SALT( myThid )
       CALL INI_PSURF( myThid )
       CALL INI_PRESSURE( myThid )
#ifdef INCLUDE_EP_FORCING_CODE
       CALL INI_EP( myThid )
#endif
       IF (nonHydrostatic) CALL INI_PNH( myThid )
      ELSE
       IF ( nIter0 .EQ. 0 ) THEN
        nIter0 = INT(startTime/deltaTClock )
       ENDIF
       CALL READ_CHECKPOINT( nIter0, myThid )
      ENDIF

#ifdef ALLOW_CD_CODE
      IF (useCDscheme) CALL CD_CODE_INIT_VARS( nIter0, myThid )
#endif

      RETURN
      END
