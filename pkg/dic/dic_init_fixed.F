C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_init_fixed.F,v 1.2 2007/07/31 23:01:58 ce107 Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DIC_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE DIC_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize fixed quantities

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
C     myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIC
#ifdef ALLOW_MNC
      IF ( useMNC ) THEN
        CALL DIC_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */
#endif /* ALLOW_DIC */

      RETURN
      END
