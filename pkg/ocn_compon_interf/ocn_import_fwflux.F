C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/ocn_import_fwflux.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE OCN_IMPORT_FWFLUX( myThid )
C     /==========================================================\
C     | SUBROUTINE OCN_IMPORT_FWFLUX                             |
C     | o Routine for importing fresh water flux into ocean      |
C     |   component from coupling layer.                         |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the       |
C     | MIT Coupler "checkpoint 1" library calls.                |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "OCNIDS.h"
#include "OCNCPL.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     Receive fresh water flux from coupling layer.
C     Data is already interpolated onto ocean model grid by coupling layer
      CALL COMPRECV_R8TILES( ocnFWFluxName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O     FWFlux )

      RETURN
      END
