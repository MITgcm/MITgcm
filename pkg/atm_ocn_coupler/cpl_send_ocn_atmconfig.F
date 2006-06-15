C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_send_ocn_atmconfig.F,v 1.1 2006/06/15 23:05:26 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_SEND_OCN_ATMCONFIG
C     /==========================================================\
C     | SUBROUTINE CPL_SEND_OCN_ATMCONFIG                        |
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
#include "OCNIDS.h"

C     == Routine arguments ==

C     == Local variables ==

CEndOfInterface

C     Map atmos. depth map onto ocean grid
      CALL ATM_TO_OCN_MAPXYR8(
     I     Nx_atm, Ny_atm, 
     I     AtmDepths_atm, 
     I     Nx_ocn, Ny_ocn,
     O     AtmDepths_ocn)

C     Send atmos. config to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, atmDepthName, Nx_ocn, Ny_ocn, 
     &                       AtmDepths_ocn )

      RETURN
      END
