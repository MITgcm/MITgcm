C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_convect.F,v 1.5 2007/11/05 18:48:04 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_CONVECT

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_CONVECT( bi,bj,k,weightA,weightB,myThid )

C !DESCRIPTION:
C     do passive tracers convection

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj,k :: tile indices
C  weightA :: weight for level K-1
C  weightB :: weight for level K
C  myThid  :: thread number
      INTEGER bi,bj,k
      _RL weightA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL weightB(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer :: tracer index
      INTEGER iTracer
CEOP

C Loop over tracers
      DO iTracer=1,PTRACERS_numInUse

       CALL CONVECTIVELY_MIXTRACER(
     I         bi,bj,k,weightA,weightB,
     U         pTracer(1-Olx,1-Oly,1,1,1,iTracer),
     I         myThid)

C End of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
