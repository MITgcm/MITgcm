C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_cycle.F,v 1.1 2002/03/04 19:01:29 adcroft Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_CYCLE

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_CYCLE(bi,bj,k,myIter,myTime,myThid)

C !DESCRIPTION:
C     Cycle time-stepping arrays

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level number
C  myIter               :: time-step number
C  myTime               :: model time
C  myThid               :: thread number
      INTEGER bi,bj,k
      INTEGER myIter
      _RL myTime
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer 		:: loop indices
C  iMin,iMax,jMin,jMax	:: Loop ranges
      INTEGER iTracer
      INTEGER iMin,iMax,jMin,jMax
CEOP

C Loop ranges
      iMin = 1-OLx+1
      iMax = sNx+OLx
      jMin = 1-OLy+1
      jMax = sNy+OLy

C Loop over passive tracers
      DO iTracer=1,PTRACERS_numInUse

C Cycle time-stepping arrays
      CALL CYCLE_TRACER(
     I           bi,bj,iMin,iMax,jMin,jMax,K,
     U           pTracer(1-Olx,1-Oly,1,1,1,iTracer),
     U           gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     U           gPtrNm1(1-Olx,1-Oly,1,1,1,iTracer),
     I           myTime,myThid )

C End of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
