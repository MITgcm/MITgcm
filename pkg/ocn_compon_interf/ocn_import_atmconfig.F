C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_import_atmconfig.F,v 1.3 2013/12/02 22:16:19 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: OCN_IMPORT_ATMCONFIG

C !INTERFACE:
      SUBROUTINE OCN_IMPORT_ATMCONFIG( myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OCN_IMPORT_ATMCONFIG
C     | o Routine for importing atmos. config into ocean
C     |   component.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the
C     | MIT Coupler "checkpoint 1" library calls.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATMIDS.h"
#include "OCNCPL.h"

C !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid
CEOP

C     Receive atmos. model configuration info.
C     o Import atmosphere model orography
      _BARRIER
      _BEGIN_MASTER( myThid )
      CALL COMPRECV_R8TILES(
     I              atmLandName, sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O              landMask )
      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
