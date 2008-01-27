C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_diagnostics_init.F,v 1.3 2008/01/27 20:52:11 jmc Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_DIAGNOSTICS_INIT

C !INTERFACE:
      SUBROUTINE MYPACKAGE_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
c #include "SIZE.h"
c #include "PARAMS.h"
c #include "MYPACKAGE.h"

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
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( useDiagnotics ) THEN

        diagName  = 'MYPaSta1'
        diagTitle = 'Scalar state variable 1 for package MYPACKAGE'
        diagUnits = 'unit            '
        diagCode  = 'SM      MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'MYPaSta2'
        diagTitle = 'Scalar state variable 2 for package MYPACKAGE'
        diagUnits = 'unit            '
        diagCode  = 'SM      MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'MYPaStaU'
        diagTitle = 'uVel-type state variable for package MYPACKAGE'
        diagUnits = 'unit            '
        WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'MYPaStaV'
        diagTitle = 'vVel-type state variable for package MYPACKAGE'
        diagUnits = 'unit            '
        WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum, 'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'MYPaSur1'
        diagTitle = 'Surface field 1 for package MYPACKAGE'
        diagUnits = 'unit            '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'MYPaSur2'
        diagTitle = 'Surface field 2 for package MYPACKAGE'
        diagUnits = 'unit            '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

c     ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
