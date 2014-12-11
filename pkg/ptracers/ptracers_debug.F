C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_debug.F,v 1.8 2014/12/11 01:36:16 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_DEBUG

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_DEBUG(
     I                           myThid )

C !DESCRIPTION:
C     print passive tracers stats

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid      :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_DEBUG

C !LOCAL VARIABLES: ====================================================
C  iTracer     :: tracer index
      INTEGER iTracer
      CHARACTER*(12) strng
CEOP

C Loop over tracers
      DO iTracer=1,PTRACERS_num

       WRITE(strng,'(A8,I4.4)') 'PTRACER.', iTracer
       CALL DEBUG_STATS_RL( Nr, pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     &                      strng, myThid )
       WRITE(strng,'(A8,I4.4)') 'GNm1TRC.', iTracer
       CALL DEBUG_STATS_RL( Nr, gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     &                      strng, myThid )

C end of tracer loop
      ENDDO

#endif /* ALLOW_DEBUG */
#endif /* ALLOW_PTRACERS */

      RETURN
      END
