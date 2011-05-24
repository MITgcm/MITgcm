C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_exchanges.F,v 1.2 2011/05/24 14:31:14 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_EXCHANGES( myThid )
C     *==========================================================*
C     | S/R OBCS_EXCHANGES
C     *==========================================================*
C     | Exchanges are needed in case obcs are read from file.
C     | EXch of xz, yz fields are avoided, so we exchange full
C     | fields after obcs have been added.
c     | Only needed for velocity fields, tracer fields wont be
C     | affected before they get exchanged at the end of
C     | time stepping loop.
C     | Note (jmc): EXCH of velocity fields do not seem needed,
C     |   except EXCH gU,gV with OBCSsponge (partly because how
C     |   OBCSsponge is written)
C     *==========================================================*

      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "OBCS_PARAMS.h"

C     == Routine Arguments ==
      INTEGER myThid

#ifdef ALLOW_OBCS
c#ifdef ALLOW_OBCS_PRESCRIBE
#ifdef ALLOW_OBCS_SPONGE

C     == Local variables ==

c     IF (useOBCSprescribe) THEN
c      CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
c      CALL EXCH_UV_XYZ_RL(gU,gV,.TRUE.,myThid)
c     ENDIF
      IF ( useOBCSsponge ) THEN
        CALL EXCH_UV_XYZ_RL( gU, gV, .TRUE., myThid )
      ENDIF

#endif
c#endif
#endif

      RETURN
      END
