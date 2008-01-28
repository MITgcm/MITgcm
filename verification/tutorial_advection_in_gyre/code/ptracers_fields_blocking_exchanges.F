C $Header: /u/gcmpack/MITgcm/verification/tutorial_advection_in_gyre/code/Attic/ptracers_fields_blocking_exchanges.F,v 1.1 2008/01/28 20:09:09 jahn Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_FIELDS_BLOCKING_EXCHANGES

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_FIELDS_BLOCKING_EXCH( myThid )

C !DESCRIPTION:
C     Exchange data to update overlaps for passive tracers

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "GAD.h"
#include "PTRACERS_FIELDS_SOM.h"

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
        CALL EXCH_3D_RL( pTracer(1-Olx,1-Oly,1,1,1,iTracer),
     &                   Nr, myThid )
#ifdef GAD_ALLOW_SOM_ADVECT
	IF ( PTRACERS_SOM_Advection(iTracer) ) THEN
	  CALL GAD_EXCH_SOM( som_Ptr(1-Olx,1-Oly,1,1,1,1,iTracer),
     &                       Nr, myThid )
	ENDIF
#endif /* GAD_ALLOW_SOM_ADVECT */

C End of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
