C $Header: /u/gcmpack/MITgcm/pkg/aim_ocn_coupler/Attic/cpl_recv_ocn_sst.F,v 1.1 2003/12/15 02:28:01 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_RECV_OCN_SST       
C     /==========================================================\
C     | SUBROUTINE CPL_RECV_OCN_SST                              |
C     | o Routine for receiving surface temperature from ocean   |
C     |   component.                                             |
C     |==========================================================|
C     | This version talks to the MITgcm general circulation     |
C     | model.                                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "OCNSIZE.h"
#include "OCNVARS.h"
#include "OCNIDS.h"

C     == Routine arguments ==

C     == Local variables ==

CEndOfInterface

C     Receive sea surface temperature   
      CALL COUPRECV_R8TILES( ocnCompName, ocnSSTName, Nx_ocn, Ny_ocn, 
     &                       SST_ocn )

      RETURN
      END
