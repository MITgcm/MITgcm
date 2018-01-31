#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_SEND_OCN_ATMCONFIG

C !INTERFACE:
      SUBROUTINE CPL_SEND_OCN_ATMCONFIG

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_SEND_OCN_ATMCONFIG
C     | o Routine for sending atmos. config to ocean component.
C     *==========================================================*
C     | This version talks to the MITgcm general circulation
C     | model. For now just send atmosphere land/sea mask.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
c#include "CPL_PARAMS.h"
#include "ATMIDS.h"
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "OCNIDS.h"
#include "OCNSIZE.h"
#include "OCNVARS.h"

C !INPUT/OUTPUT PARAMETERS:
C !LOCAL VARIABLES:
CEOP

C-    Map atmos. land-mask onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I     Nx_atm, Ny_atm,
     I     landMask_atm,
     I     Nx_ocn, Ny_ocn,
     O     landMask_ocn)

C-    Send atmos. config to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, atmLandName, Nx_ocn, Ny_ocn,
     &                       landMask_ocn )

      RETURN
      END
