C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/cpl_send_ocn_fwflux.F,v 1.1 2003/12/15 02:28:01 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_SEND_OCN_FWFLUX
C     /==========================================================\
C     | SUBROUTINE CPL_SEND_OCN_FWFLUX                           |
C     | o Routine for sending fresh water flux to ocean component|
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

C     Map atmos. fresh water flux onto ocean grid : Evap - Precip
      CALL ATM_TO_OCN_MAPXYR8(
     I     Nx_atm, Ny_atm, 
     I     EvMPr_atm, 
     I     Nx_ocn, Ny_ocn,
     O     FWFlux_ocn)

C     Map atmos. runOff from land onto ocean grid and substract it 
C       from previous (direct) FWFlux => total FWFlux = E-P-R
      CALL ATM_TO_OCN_MAPRUNOFF(
     I     RunOff_atm, 
     U     FWFlux_ocn)

C     Send atmos. config to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnFWFluxName, Nx_ocn, Ny_ocn, 
     &                       FWFlux_ocn )

      RETURN
      END
