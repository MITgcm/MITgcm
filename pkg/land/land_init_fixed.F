C $Header: /u/gcmpack/MITgcm/pkg/land/land_init_fixed.F,v 1.1 2014/05/28 21:59:32 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE LAND_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_INIT_FIXED
C     | o Initialize Land package
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===

C-- size for MITgcm & Land package :
#include "LAND_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_LAND

C     !LOCAL VARIABLES:
C     msgBuf      :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF ( useMNC ) THEN
        CALL LAND_MNC_INIT(sNx,sNy, OLx,OLy, nSx,nSy, nPx,nPy,
     &       land_nLev, myThid)
      ENDIF

      IF ( useDiagnostics ) CALL LAND_DIAGNOSTICS_INIT( myThid )

#endif /* ALLOW_LAND */

      RETURN
      END
