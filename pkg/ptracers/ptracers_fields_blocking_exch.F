C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_fields_blocking_exch.F,v 1.1 2012/03/08 17:12:33 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_FIELDS_BLOCKING_EXCH

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_FIELDS_BLOCKING_EXCH( myThid )

C !DESCRIPTION:
C     Exchange data to update overlaps for passive tracers

C !USES: ===============================================================
#include "PTRACERS_MOD.h"
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_START.h"
#include "PTRACERS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid         :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer        :: loop indices
C  bi, bj         :: tile indices
      INTEGER iTracer
#ifdef ALLOW_OBCS
c     INTEGER bi, bj
#endif /* ALLOW_OBCS */
CEOP

C Loop over passive tracers
      DO iTracer=1,PTRACERS_numInUse
       IF ( PTRACERS_StepFwd(iTracer) ) THEN

C Exchange overlaps
        CALL EXCH_3D_RL( pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     &                   Nr, myThid )
#ifdef PTRACERS_ALLOW_DYN_STATE
        IF ( PTRACERS_SOM_Advection(iTracer) ) THEN
          CALL GAD_EXCH_SOM( _Ptracers_som(:,:,:,:,:,:,iTracer),
     &                       Nr, myThid )
        ENDIF
#endif /* PTRACERS_ALLOW_DYN_STATE */

C End of tracer loop
       ENDIF
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
