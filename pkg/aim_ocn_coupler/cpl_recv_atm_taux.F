C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/cpl_recv_atm_taux.F,v 1.1 2003/12/15 02:28:00 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_RECV_ATM_TAUX    
C     /==========================================================\
C     | SUBROUTINE CPL_RECV_ATM_TAUX                             |
C     | o Routine for receiving lower boundary zonal momentum    |
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
      CALL COUPRECV_R8TILES( atmCompName, atmTauXName, Nx_atm, Ny_atm, 
     &                       TauX_atm   )

      RETURN
      END
