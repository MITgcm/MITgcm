C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/cpl_send_atm_sst.F,v 1.1 2003/12/15 02:28:01 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_SEND_ATM_SST
C     /==========================================================\
C     | SUBROUTINE CPL_SEND_ATM_SST                              |
C     | o Routine for sending ocean surface temperatures to      |
C     |   atmos. component.                                      |
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model.                                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "OCNSIZE.h"
#include "ATMSIZE.h"
#include "OCNVARS.h"
#include "ATMVARS.h"
#include "ATMIDS.h"

C     == Routine arguments ==

C     == Local variables ==

CEndOfInterface

C     Map ocean surface temperature onto atmos. grid
      CALL OCN_TO_ATM_MAPXYR8(
     I     Nx_ocn, Ny_ocn, 
     I     SST_ocn,    
     I     Nx_atm, Ny_atm,
     O     SST_atm   )

C     Send atmos. config to ocean component
      CALL COUPSEND_R8TILES( atmCompName, atmSSTName, Nx_atm, Ny_atm, 
     &                       SST_atm )

      RETURN
      END
