C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/cpl_recv_atm_atmconfig.F,v 1.3 2006/06/15 23:29:17 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_RECV_ATM_ATMCONFIG
C     /==========================================================\
C     | SUBROUTINE CPL_RECV_ATM_ATMCONFIG                        |
C     | o Routine for receiving atmos config into coupling level.|
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model. For now just get the models depth map.            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"

C     == Routine arguments ==

CEndOfInterface

C     Get config from atmos component
      CALL COUPRECV_R8TILES( atmCompName, atmDepthName, Nx_atm, Ny_atm, 
     &                       AtmDepths_atm )

      RETURN
      END
