C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/cpl_recv_atm_runoff.F,v 1.1 2003/12/15 02:28:00 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_RECV_ATM_RUNOFF
C     /==========================================================\
C     | SUBROUTINE CPL_RECV_ATM_RUNOFF                           |
C     | o Routine for exporting atmos. RunOff from land bucket   |
C     |   to coupling layer.                                     |
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
      CALL COUPRECV_R8TILES( atmCompName, atmRunOffName, 
     &                       Nx_atm, Ny_atm,
     &                       RunOff_atm )

      RETURN
      END
