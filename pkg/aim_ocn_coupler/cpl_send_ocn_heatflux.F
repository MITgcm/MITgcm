C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/cpl_send_ocn_heatflux.F,v 1.1 2003/12/15 02:28:01 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_SEND_OCN_HEATFLUX 
C     /==========================================================\
C     | SUBROUTINE CPL_SEND_OCN_HEATFLUX                         |
C     | o Routine for sending surface heatflux to ocean component|
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

C     Map heatflux onto ocean grid from atmos. grid
      CALL ATM_TO_OCN_MAPXYR8(
     I     Nx_atm, Ny_atm, 
     I     HeatFlux_atm, 
     I     Nx_ocn, Ny_ocn,
     O     HeatFlux_ocn)

C     Send atmos. config to ocean component
      CALL COUPSEND_R8TILES( ocnCompName, ocnHeatFluxName, 
     I                       Nx_ocn, Ny_ocn, 
     &                       HeatFlux_ocn )

      RETURN
      END
