C $Header: /u/gcmpack/MITgcm/model/src/ini_fields.F,v 1.12 2001/09/26 18:09:15 cnh Exp $
C $Name:  $

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
      IF ( startTime .EQ. 0. .AND.  nIter0 .EQ. 0 ) THEN
       CALL INI_VEL( myThid )
       CALL INI_THETA( myThid )
       CALL INI_SALT( myThid )
       CALL INI_PSURF( myThid )
       IF (nonHydrostatic) CALL INI_PNH( myThid )
      ELSE
       IF ( nIter0 .EQ. 0 ) THEN
        nIter0 = INT(startTime/deltaTClock )
       ENDIF
       CALL READ_CHECKPOINT( nIter0, myThid )
      ENDIF

      RETURN
      END
