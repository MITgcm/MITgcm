C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_send_ocn_atmconfig.F,v 1.4 2013/12/02 23:21:47 jmc Exp $
C $Name:  $

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
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"

C !INPUT/OUTPUT PARAMETERS:
C !LOCAL VARIABLES:
CEOP

C     Map atmos. depth map onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I     Nx_atm, Ny_atm,
     I     landMask_atm,
     I     Nx_ocn, Ny_ocn,
     O     landMask_ocn)

C     Send atmos. config to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, atmLandName, Nx_ocn, Ny_ocn,
     &                       landMask_ocn )

      RETURN
      END
