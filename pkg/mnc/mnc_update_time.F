C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_update_time.F,v 1.1 2011/05/23 00:27:24 jmc Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_UPDATE_TIME

C     !INTERFACE:
      SUBROUTINE MNC_UPDATE_TIME(
     I                            myTime, myIter,  myThid )

C     !DESCRIPTION:
C     Update MNC time and iteration information

C     !USES:
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MNC_PARAMS.h"

C     !INPUT PARAMETERS:
C     myTime   :: my current time in simulation
C     myIter   :: my current iteration number in simulation
C     myThid   :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_MNC
C     !FUNCTIONS:
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( useMNC ) THEN

C     Update the default next iter for MNC
         CALL MNC_CW_CITER_SETG( 1, 1, -1, myIter , myThid )

C        TODO: Logic should be added here so that users can specify, on
C        a per-citer-group basis, when it is time to update the
C        "current" (and not just the "next") iteration

C        TODO: the following is just a temporary band-aid (mostly, for
C        Baylor) until someone writes a routine that better handles time
C        boundaries such as weeks, months, years, etc.
         IF ( mnc_filefreq .GT. 0 ) THEN
           IF (DIFFERENT_MULTIPLE(mnc_filefreq,myTime,deltaTClock))
     &          THEN
             CALL MNC_CW_CITER_SETG( 1, 1, myIter, -1 , myThid )
           ENDIF
         ENDIF

c     ENDIF
#endif /* ALLOW_MNC */

      RETURN
      END
