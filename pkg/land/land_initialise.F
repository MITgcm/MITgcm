C $Header: /u/gcmpack/MITgcm/pkg/land/Attic/land_initialise.F,v 1.4 2005/07/30 23:53:48 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_INITIALISE
C     !INTERFACE:
      SUBROUTINE LAND_INITIALISE( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_INITIALISE
C     | o Read Land package parameters (from file data.land)
C     | o Initialize Land package variables
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
C     == Routine Arguments ==       
C     myThid ::  Number of this instance
      INTEGER myThid
CEOP

#ifdef ALLOW_LAND

C     == Local Variables == 
C     msgBuf      :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    for now, Land package works only with AIM package:
      IF ( .NOT.useAIM ) THEN
        WRITE(msgBuf,'(A)')
     &  'LAND_INITIALISE: land pkg only implemented for AIM physics'
        CALL PRINT_ERROR( msgBuf, myThid)
        WRITE(msgBuf,'(A)')
     &  'DO NOT use Land (useLand=T) without AIM (useAIM=F, data.pkg)' 
        CALL PRINT_ERROR( msgBuf, myThid)
        STOP 'ABNORMAL END: S/R LAND_INITIALISE'
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Read LAND parameters (from file data.land):
      CALL LAND_READPARMS( myThid )

      IF ( useMNC ) THEN
        CALL LAND_MNC_INIT(sNx,sNy, OLx,OLy, nSx,nSy, nPx,nPy,
     &       land_nLev, myThid)
      ENDIF

      IF ( useDiagnostics ) CALL LAND_DIAGNOSTICS_INIT( myThid )

#endif /* ALLOW_LAND */

      RETURN
      END
