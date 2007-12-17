C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_gterm_blocking_exchanges.F,v 1.6 2007/12/17 21:20:38 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_GTERM_BLOCKING_EXCHANGES

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_GTERM_BLOCKING_EXCH( myThid )

C !DESCRIPTION:
C     Exchange data to update overlaps for passive tracer tendencies

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid         :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer        :: loop indices
      INTEGER iTracer
CEOP

C Loop over passive tracers
      DO iTracer=1,PTRACERS_numInUse

C Exchange overlaps
        CALL EXCH_3D_RL( gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     &                   Nr, myThid )

C End of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
