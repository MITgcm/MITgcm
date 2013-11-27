C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/cpl_init_fixed.F,v 1.2 2013/11/27 21:10:30 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CPL_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE CPL_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Define MNC variables for use by the coupler.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_MNC
C      IF (useMNC) THEN
C      ENDIF
#endif /* ALLOW_MNC */

      RETURN
      END
