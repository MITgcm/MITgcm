C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_init_fixed.F,v 1.1 2011/02/21 22:26:19 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

CBOP
C     !ROUTINE: CHEAPAML_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE CHEAPAML_INIT_FIXED(
     I                                myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CHEAPAML_INIT_FIXED
C     | o Initialize CHEAPAML variables that are kept fixed
C     |   during the run.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CHEAPAML.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid  :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP

      RETURN
      END
