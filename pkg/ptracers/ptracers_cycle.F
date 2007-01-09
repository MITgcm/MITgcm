C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_cycle.F,v 1.4 2007/01/09 21:52:02 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_CYCLE

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_CYCLE(bi,bj,myTime,myIter,myThid)

C !DESCRIPTION:
C     Cycle time-stepping arrays

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  myTime               :: model time
C  myIter               :: time-step number
C  myThid               :: thread number
      INTEGER bi,bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer 		:: loop indices
      INTEGER iTracer
CEOP

C Loop over passive tracers
      DO iTracer=1,PTRACERS_numInUse

C Cycle time-stepping arrays
      CALL CYCLE_TRACER(
     I           bi,bj,
     U           pTracer(1-Olx,1-Oly,1,1,1,iTracer),
     U           gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I           myTime,myIter,myThid )

C End of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
