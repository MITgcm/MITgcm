C $Header: /u/gcmpack/MITgcm/pkg/mypackage/Attic/mypackage_diagnostics_fill.F,v 1.1 2006/06/28 21:26:21 heimbach Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE MYPACKAGE_DIAGNOSTICS_FILL(
     I             bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE MYPACKAGE_DIAGNOSTICS_FILL                    |
C     | o Calculate tensor elements for GM/Redi tensor.          |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MYPACKAGE.h"

C     == Routine arguments ==
C
      INTEGER bi,bj
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_MYPACKAGE

C     == Local variables ==

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|


#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

         CALL DIAGNOSTICS_FILL(mypaStatScal1,'MYPASta1',
     &      0,Nr,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(mypaStatScal2,'MYPASta2',
     &      0,Nr,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(mypaStatUvel,'MYPAStaU',
     &      0,Nr,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(mypaStatVvel,'MYPAStaV',
     &      0,Nr,1,bi,bj,myThid)
C
         CALL DIAGNOSTICS_FILL(mypaSurf1 ,'MYPASur1',
     &      0,1,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(mypaSurf2 ,'MYPASur2 ',
     &       0,1,1,bi,bj,myThid)

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_MYPACKAGE */

      RETURN
      END
