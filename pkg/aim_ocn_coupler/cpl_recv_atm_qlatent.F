C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/cpl_recv_atm_qlatent.F,v 1.1 2003/12/15 02:28:00 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_RECV_ATM_QLATENT 
C     /==========================================================\
C     | SUBROUTINE CPL_RECV_ATM_QLATENT                          |
C     | o Routine for receiving lower boundary latent heat       |
C     |   flux from atmos component.                             |
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model.                                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "ATMSIZE.h"
#include "ATMVARS.h"
#include "ATMIDS.h"

C     == Routine arguments ==

C     == Local variables ==

CEndOfInterface

C     Receive lower boundary momentum flux from atmos. component
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      CALL COUPRECV_R8TILES( atmCompName, atmQLatentName,
     I                       Nx_atm, Ny_atm, 
     &                       qLatent_atm   )

      RETURN
      END
