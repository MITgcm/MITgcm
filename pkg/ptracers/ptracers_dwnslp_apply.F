C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_dwnslp_apply.F,v 1.1 2008/08/17 02:09:36 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_DWNSLP_APPLY

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_DWNSLP_APPLY(
     I                    bi, bj, myTime, myIter, myThid )

C !DESCRIPTION:
C     Apply Down-Slope Flow parametrization to passive tracers

C !USES: ===============================================================
c#include "PTRACERS_MOD.h"
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj      :: tile indices
C  myTime     :: model time
C  myIter     :: time-step number
C  myThid     :: thread number
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_DOWN_SLOPE

C !LOCAL VARIABLES: ====================================================
C  iTr        :: tracer index
      INTEGER iTr
CEOP

C Loop over tracers
      DO iTr=1,PTRACERS_numInUse

        IF ( PTRACERS_useDWNSLP(iTr) ) THEN
          IF ( usingPCoords ) THEN
            CALL DWNSLP_APPLY(
     I                  GAD_TR1+iTr-1, bi, bj, kSurfC,
     I                  recip_drF, recip_hFacC, recip_rA,
     I                  pTracer(1-Olx,1-Oly,1,1,1,iTr),
     O                  gPtr(1-Olx,1-Oly,1,1,1,iTr),
     I                  myTime, myIter, myThid )
          ELSE
            CALL DWNSLP_APPLY(
     I                  GAD_TR1+iTr-1, bi, bj, kLowC,
     I                  recip_drF, recip_hFacC, recip_rA,
     I                  pTracer(1-Olx,1-Oly,1,1,1,iTr),
     O                  gPtr(1-Olx,1-Oly,1,1,1,iTr),
     I                  myTime, myIter, myThid )
          ENDIF
        ENDIF

C end of tracer loop
      ENDDO

#endif /* ALLOW_DOWN_SLOPE */
#endif /* ALLOW_PTRACERS */

      RETURN
      END
