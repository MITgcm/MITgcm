C$Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_init_fixed.F,v 1.1 2005/09/04 19:22:48 jmc Exp $
C$Name:  $

#include "MOM_COMMON_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MOM_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE MOM_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize fixed quantities 
C      for momentum (common to fluxform & vecinv) packages

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
C     myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL MOM_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
