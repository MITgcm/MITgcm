C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_debug.F,v 1.6 2007/11/05 18:48:04 jmc Exp $
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
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS
#ifdef  ALLOW_DEBUG

C !LOCAL VARIABLES: ====================================================
C  i,j,k,bi,bj,iTracer  :: loop indices
      INTEGER iTracer
      CHARACTER*(9) strng
CEOP

C Loop over tracers
      DO iTracer=1,PTRACERS_num

       WRITE(strng(1:9),'(a7,i2.2)') 'PTRACER',iTracer
       CALL DEBUG_STATS_RL(Nr,
     &   pTracer(1-Olx,1-Oly,1,1,1,iTracer),strng,myThid)
       WRITE(strng(1:9),'(a7,i2.2)') 'GTRACER',iTracer
       CALL DEBUG_STATS_RL(Nr,
     &   gPtr(1-Olx,1-Oly,1,1,1,iTracer),strng,myThid)
       WRITE(strng(1:9),'(a7,i2.2)') 'GNm1TRC',iTracer
       CALL DEBUG_STATS_RL(Nr,
     &   gpTrNm1(1-Olx,1-Oly,1,1,1,iTracer),strng,myThid)

C end of tracer loop
      ENDDO

#endif
#endif /* ALLOW_PTRACERS */

      RETURN
      END
