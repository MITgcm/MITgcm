C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/cpl_recv_atm_evmpr.F,v 1.1 2003/12/15 02:28:00 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_RECV_ATM_EVMPR
C     /==========================================================\
C     | SUBROUTINE CPL_RECV_ATM_EVMPR                            |
C     | o Routine for receiving fresh water (=Evap-Precip) from  |
C     |   atmos. component                                       |
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

C     Receive atmos. fresh water flux 
      CALL COUPRECV_R8TILES( atmCompName, atmEvMPrName, 
     &                       Nx_atm, Ny_atm,
     &                       EvMPr_atm )

      RETURN
      END
