C $Header: /u/gcmpack/MITgcm/pkg/regrid/regrid_init_fixed.F,v 1.1 2006/08/15 04:05:48 edhill Exp $
C $Name:  $

#include "REGRID_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: REGRID_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE REGRID_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize REGRID variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "REGRID_SIZE.h"
#include "REGRID.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:

#ifdef ALLOW_MNC
      IF (useMNC) THEN
        CALL REGRID_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

      RETURN
      END
