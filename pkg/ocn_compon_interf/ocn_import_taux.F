C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/ocn_import_taux.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE OCN_IMPORT_TAUX( myThid )
C     /==========================================================\
C     | SUBROUTINE OCN_IMPORT_TAUX                               |
C     | o Routine for importing zonal wind stress into ocean     |
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

C     Receive atmos. model zonal wind stress.
C     Already interpolated ointo ocean model grid by coupling layer.
      CALL COMPRECV_R8TILES( ocnTauxName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O     tauX )

      RETURN
      END
