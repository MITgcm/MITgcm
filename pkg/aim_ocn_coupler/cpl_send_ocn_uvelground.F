C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/cpl_send_ocn_uvelground.F,v 1.1 2003/12/15 02:28:01 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_SEND_OCN_UVELGROUND
C     /==========================================================\
C     | SUBROUTINE CPL_SEND_OCN_UVELGROUND                       |
C     | o Routine for sending zonal wind speed to ocean component|
C     |   component.                                             |
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model.                                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"

C     == Routine arguments ==

C     == Local variables ==

CEndOfInterface

C     Map atmos. meridional momentum flux onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I     Nx_atm, Ny_atm, 
     I     uVelGround_atm,
     I     Nx_ocn, Ny_ocn,
     O     uVelGround_ocn)

C     Send atmos. config to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnuVelGroundName, 
     I                       Nx_ocn, Ny_ocn, 
     I                       uVelGround_ocn )

      RETURN
      END
