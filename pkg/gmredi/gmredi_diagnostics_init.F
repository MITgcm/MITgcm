C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_diagnostics_init.F,v 1.4 2007/01/30 15:17:52 dfer Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE GMREDI_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for GM/Redi package
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
c #include "SIZE.h"
c #include "PARAMS.h"
c #include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
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

        diagName  = 'GM_VisbK'
        diagTitle = 
     &     'Mixing coefficient from Visbeck etal parameterization'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_Kux  '
        diagTitle = 'K_11 element (U.point, X.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        WRITE(diagCode,'(A,I3.3,A)') 'UU P ',diagNum+2,'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_Kvy  '
        diagTitle = 'K_22 element (V.point, Y.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        WRITE(diagCode,'(A,I3.3,A)') 'VV P ', diagNum ,'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_Kuz  '
        diagTitle = 'K_13 element (U.point, Z.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_Kvz  '
        diagTitle = 'K_23 element (V.point, Z.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_Kwx  '
        diagTitle = 'K_31 element (W.point, X.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'LR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_Kwy  '
        diagTitle = 'K_32 element (W.point, Y.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'LR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_Kwz  '
        diagTitle = 'K_33 element (W.point, Z.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        diagCode  = 'WM P    LR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_PsiX '
        diagTitle = 'GM Bolus transport stream-function : X component'
        diagUnits = 'm^2/s           '
        WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'LR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_PsiY '
        diagTitle = 'GM Bolus transport stream-function : Y component'
        diagUnits = 'm^2/s           '
        WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'LR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_KuzTz'
        diagTitle = 'Redi Off-diagonal Temperature flux: X component'
        diagUnits = 'degC.m^3/s      '
        WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_KvzTz'
        diagTitle = 'Redi Off-diagonal Temperature flux: Y component'
        diagUnits = 'degC.m^3/s      '
        WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_ubT  '
        diagTitle = 'Zonal Mass-Weight Bolus Transp of Pot Temp'
        diagUnits = 'degC.m^3/s      '
        WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GM_vbT  '
        diagTitle = 'Meridional Mass-Weight Bolus Transp of Pot Temp'
        diagUnits = 'degC.m^3/s      '
        WRITE(diagCode,'(A,I3.3,A)') 'VV   ',diagNum,'MR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

#ifdef ALLOW_TAU_EDDY
        diagName  = 'GMEdTauX'
        diagTitle = 'eddy-induced stress X-comp. estimated from Kwx'
        diagUnits = 'N/m^2           '
        WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'LR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'GMEdTauY'
        diagTitle = 'eddy-induced stress Y-comp. estimated from Kwy'
        diagUnits = 'N/m^2           '
        WRITE(diagCode,'(A,I3.3,A)') 'VM   ',diagNum+2,'LR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )
#endif

c     ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
