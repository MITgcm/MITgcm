C $Header: /u/gcmpack/MITgcm/pkg/obcs/Attic/obcs_prescribe_exchanges.F,v 1.1 2004/09/23 17:48:24 heimbach Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_PRESCRIBE_EXCHANGES( myThid )
C     *==========================================================*
C     | S/R OBCS_PRESCRIBE_EXCHANGES                             |
C     *==========================================================*
C     | Exchanges are needed in case obcs are read from file.    |
C     | EXch of xz, yz fields are avoided, so we exchange full   |
C     | fields after obcs have been added.                       |
c     | Only needed for velocity fields, tracer fields wont be   |
C     | affected before they get exchanged at the end of         |
C     | time stepping loop.                                      |
C     *==========================================================*

      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "OBCS.h"

C     == Routine Arguments ==
      INTEGER myThid

#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCS_PRESCRIBE

C     == Local variables ==

      IF (useOBCSprescribe) THEN
       CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
       CALL EXCH_UV_XYZ_RL(gU,gV,.TRUE.,myThid)
      ENDIF

#endif
#endif

      RETURN
      END
