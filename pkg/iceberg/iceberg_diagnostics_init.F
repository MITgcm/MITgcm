C $Header: /u/gcmpack/MITgcm/pkg/iceberg/iceberg_diagnostics_init.F,v 1.5 2010/02/17 20:31:23 dimitri Exp $
C $Name: checkpoint65m $

#include "ICEBERG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEBERG_DIAGNOSTICS_INIT

C !INTERFACE:
      SUBROUTINE ICEBERG_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#endif /* ALLOW_PTRACERS */
#include "ICEBERG_SIZE.h"
#include "ICEBERG.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     === Local variables ===
C     diagNum   :: diagnostics number in the (long) list of available diag.
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic
      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(50) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName  = 'BRGfwFlx'
      diagTitle = 'Iceberg freshwater flux'
      diagUnits = 'm^3/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'BRGhtFlx'
      diagTitle = 'Iceberg heat flux  (+ve cools ocean)              '
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
 
      diagName  = 'BRGmltRt'
      diagTitle = 'Iceberg melt rate              '
      diagUnits = 'm/d             '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'BRGarea3D'
      diagTitle = 'Iceberg surface area               '
      diagUnits = 'm^2             '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'BRG_TauX'
      diagTitle = 
     &     'Iceberg drag stress, zonal. comp., >0 ++uVel'
      diagUnits = 'N/m^2           '
      diagCode  = 'UU      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'BRG_TauY'
      diagTitle = 
     &     'Iceberg drag stress, merid. comp., >0 ++vVel'
      diagUnits = 'N/m^2           '
      diagCode  = 'VV      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'BRGhFacC'
      diagTitle = 
     &     'Ocean fraction (HfacC) in icebergs'
      diagUnits = 'none            '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#ifdef ALLOW_PER_BERG_DIAG
C     Could implement per berg diagnostics here
#endif /* ALLOW_PER_BERG_DIAG */      


#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
