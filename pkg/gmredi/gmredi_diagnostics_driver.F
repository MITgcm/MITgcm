C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/gmredi_diagnostics_driver.F,v 1.1 2005/10/26 20:53:14 heimbach Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CStartOfInterface
      SUBROUTINE GMREDI_DIAGNOSTICS_DRIVER(
     I             bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_DIAGNOSTICS_DRIVER                     |
C     | o Calculate tensor elements for GM/Redi tensor.          |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"
#include "GMREDI_TAVE.h"

C     == Routine arguments ==
C
      INTEGER bi,bj
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_GMREDI

C     == Local variables ==

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|


#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

#ifdef GM_VISBECK_VARIABLE_K
       IF ( GM_Visbeck_alpha.NE.0. ) THEN
         CALL DIAGNOSTICS_FILL(VisbeckK,'GM_VisbK',0,1,1,bi,bj,myThid)
       ENDIF
#endif
#ifdef GM_NON_UNITY_DIAGONAL
         CALL DIAGNOSTICS_FILL(Kux,'GM_Kux  ',0,Nr,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(Kvy,'GM_Kvy  ',0,Nr,1,bi,bj,myThid)
#endif
#ifdef GM_EXTRA_DIAGONAL
       IF ( GM_ExtraDiag ) THEN
         CALL DIAGNOSTICS_FILL(Kuz,'GM_Kuz  ',0,Nr,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(Kvz,'GM_Kvz  ',0,Nr,1,bi,bj,myThid)
       ENDIF
#endif
         CALL DIAGNOSTICS_FILL(Kwx,'GM_Kwx  ',0,Nr,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(Kwy,'GM_Kwy  ',0,Nr,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(Kwz,'GM_Kwz  ',0,Nr,1,bi,bj,myThid)
#ifdef GM_BOLUS_ADVEC
       IF ( GM_AdvForm ) THEN
         CALL DIAGNOSTICS_FILL(GM_PsiX,'GM_PsiX ',0,Nr,1,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(GM_PsiY,'GM_PsiY ',0,Nr,1,bi,bj,myThid)
       ENDIF
#endif
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_GMREDI */

      RETURN
      END
