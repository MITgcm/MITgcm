C $Header: /u/gcmpack/MITgcm/model/src/ini_fields.F,v 1.8 2001/03/12 20:41:02 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_FIELDS( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_FIELDS                                    |
C     | o Initialise model fields                                |
C     |==========================================================|
C     | Initial conditions of the model state variables are      |
C     | set here. For continuation runs a restart file will be   |
C     | used. For original runs custom rules can be included     |
C     | here.                                                    |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_FIELDS
      INTEGER myThid
CEndOfInterface

C--   Set model variables to initial/restart conditions
      IF ( startTime .EQ. 0. .AND.  nIter0 .EQ. 0 ) THEN
       CALL INI_UVEL( myThid )
       CALL INI_VVEL( myThid )
       CALL INI_THETA( myThid )
       CALL INI_SALT( myThid )
       CALL INI_PSURF( myThid )
#ifdef ALLOW_NONHYDROSTATIC
       IF ( nonHydrostatic ) THEN
         CALL INI_GW( myThid )
       ENDIF
#endif
      ELSE
       IF ( nIter0 .EQ. 0 ) THEN
        nIter0 = INT(startTime/deltaTClock )
       ENDIF
       CALL READ_CHECKPOINT( nIter0, myThid )
      ENDIF

      RETURN
      END
