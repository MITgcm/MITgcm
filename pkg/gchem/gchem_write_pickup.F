C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_write_pickup.F,v 1.2 2009/06/30 16:43:00 jahn Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"
#ifdef ALLOW_DARWIN
#include "DARWIN_OPTIONS.h"
#endif

CBOP
C !ROUTINE: GCHEM_WRITE_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_WRITE_PICKUP( permPickup, suff,
     I                               myTime, myIter, myThid )

C !DESCRIPTION:
C     Writes pickup files needed for a restart
C     (wrapper interface to GCHEM sub-packages)

C !USES: ===============================================================
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
#include "GCHEM.h"

C !INPUT PARAMETERS: ===================================================
C     permPickup :: write a permanent pickup
C     suff       :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime     :: Current time in simulation
C     myIter     :: Current iteration number in simulation
C     myThid     :: My Thread Id number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_GCHEM
C     !LOCAL VARIABLES:

#ifdef ALLOW_DIC
      IF ( useDIC ) THEN
        CALL DIC_WRITE_PICKUP( permPickup, suff,
     I                         myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_DIC */

#ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
        CALL DARWIN_WRITE_PICKUP( permPickup, suff,
     I                            myTime, myIter, myThid )
#ifdef ALLOW_CARBON
        CALL DIC_WRITE_PICKUP( permPickup, suff,
     I                         myTime, myIter, myThid )
#endif
      ENDIF
#endif /* ALLOW_DARWIN */

#endif /* ALLOW_GCHEM */
      RETURN
      END
