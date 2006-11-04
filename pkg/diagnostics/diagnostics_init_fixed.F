C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_init_fixed.F,v 1.6 2006/11/04 14:28:09 heimbach Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_INIT_FIXED(myThid)

C     !DESCRIPTION:
C     Set Pointer Locations for Diagnostics in diag_list.

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
#ifdef ALLOW_FIZHI
#include "PARAMS.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:

C--   Set number of levels for all available diagnostics:
      CALL DIAGNOSTICS_SET_LEVELS( myThid )

C--   Calculate pointers for diagnostics set to non-zero frequency
      CALL DIAGNOSTICS_SET_POINTERS( myThid )

C--   Define region-mask for regional statistics diagnostics
      CALL DIAGSTATS_SET_REGIONS( myThid )

C--   Calculate pointers for statistics-diags set to non-zero frequency
      CALL DIAGSTATS_SET_POINTERS( myThid )

      CALL DIAGSTATS_INI_IO( myThid )

#ifdef ALLOW_FIZHI
      if( useFIZHI) then
      call fizhi_diagalarms(myThid)
      endif
#endif

      RETURN
      END
