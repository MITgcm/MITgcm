C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_register_atm.F,v 1.3 2006/06/15 23:29:17 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_REGISTER_ATM
C     /==========================================================\
C     | SUBROUTINE CPL_REGISTER_ATM                              |
C     | o Routine for registration of atmos config. with coupler |
C     |   layer.                                                 |
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model. For now just get the models depth map.            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "ATMSIZE.h"
#include "ATMIDS.h"

C     == Routine arguments ==
CEndOfInterface

C     Call MIT Coupler registration acceptance procedure.  Make sure that nobody
C     who registers for atmCompName has a sub-domain that exceeds the bounds
C     Nx_atm, Ny_atm.
C     The first argument to MITCOUPLER_tile_register gives the name of the
C     component whose attributes will be collected by the particular call
C     to MITCOUPLER_tile_register.
C     The second and third arguments to MITCOUPLER_tile_register are used
C     for an internal error check. This check makes sure that the sub-domain
C     sizes reported by each of the component processes for a given component
C     fall within the global domain size the coupler is expecting.
      CALL MITCOUPLER_TILE_REGISTER( atmCompName, Nx_atm, Ny_atm )

      RETURN
      END
