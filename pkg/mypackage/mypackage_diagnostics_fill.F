C $Header: /u/gcmpack/MITgcm/pkg/mypackage/Attic/mypackage_diagnostics_fill.F,v 1.2 2006/08/07 21:13:52 edhill Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_DIAGNOSTICS_FILL

C !INTERFACE:
      SUBROUTINE MYPACKAGE_DIAGNOSTICS_FILL(
     I             bi, bj, myThid )

C     !DESCRIPTION:
C     Provide the information for the diagnostics

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      INTEGER bi,bj
      INTEGER myThid
CEOP

#ifdef ALLOW_MYPACKAGE

C     !LOCAL VARIABLES:

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
