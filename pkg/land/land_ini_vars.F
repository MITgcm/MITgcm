C $Header: /u/gcmpack/MITgcm/pkg/land/land_ini_vars.F,v 1.1 2003/06/12 17:54:22 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_INI_VARS
C     !INTERFACE:
      SUBROUTINE LAND_INI_VARS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_INI_VARS
C     | o Initialize Land package variables
C     *==========================================================*
C     | for now, used only for a restart
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==       
C     myThid -  Number of this instance
      INTEGER myThid
CEOP

#ifdef ALLOW_LAND

C     == Local Variables == 
C     msgBuf      - Informational/error meesage buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( startTime.EQ.0. .AND. nIter0.EQ.0 ) THEN

C--   Define the initial state
C     for now, loaded from AIM surf. BC in S/R aim_land2aim

c     ELSE
C-    a trick to allow to start without a land pickup:
      ELSEIF ( land_calc_grT .OR. land_calc_grW ) THEN

C--   Read Land package state variables from pickup file
        CALL LAND_READ_PICKUP( nIter0, myThid )

      ENDIF

#endif /* ALLOW_LAND */

      RETURN
      END
