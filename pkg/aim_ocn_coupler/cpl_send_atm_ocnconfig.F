C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/cpl_send_atm_ocnconfig.F,v 1.2 2004/05/21 19:31:50 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_SEND_ATM_OCNCONFIG
C     /==========================================================\
C     | SUBROUTINE CPL_SEND_ATM_OCNCONFIG                        |
C     | o Routine for sending atmos. config to ocean component.  |
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model. For now just send atmosphere depths.              |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"
#include "OCNSIZE.h"
#include "OCNVARS.h"

C     == Routine arguments ==
CEndOfInterface

C     Map ocean depth map onto atmos. grid
      CALL OCN_TO_ATM_MAPXYR8(
     I     Nx_ocn, Ny_ocn, 
     I     OcMxlD_ocn, 
     I     Nx_atm, Ny_atm,
     O     OcMxlD_atm)

C     Send ocean config to atmos. component
      CALL COUPSEND_R8TILES( atmCompName, atmMxlDName, Nx_atm, Ny_atm, 
     &                       OcMxlD_atm )

      RETURN
      END
