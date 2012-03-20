C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_diagnostics_init.F,v 1.10 2012/03/20 23:36:59 jmc Exp $
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
C     diagMate  :: diag. mate number in the (long) list of available diag.
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
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( useDiagnotics ) THEN

        diagName  = 'GM_VisbK'
        diagTitle =
     &     'Mixing coefficient from Visbeck etal parameterization'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GM_hTrsL'
        diagTitle = 'Base depth (>0) of the Transition Layer'
        diagUnits = 'm               '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GM_baseS'
        diagTitle = 'Slope at the base of the Transition Layer'
        diagUnits = '1               '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GM_rLamb'
        diagTitle =
     &    'Slope vertical gradient at Trans. Layer Base (=recip.Lambda)'
        diagUnits = '1/m             '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifndef GM_EXCLUDE_SUBMESO
        diagName  = 'SubMesLf'
        diagTitle = 'Sub-Meso horiz. Length Scale (Lf)'
        diagUnits = 'm               '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'SubMpsiX'
        diagTitle =
     &    'Sub-Meso transp.stream-funct. magnitude (Psi0): U component'
        diagUnits = 'm^2/s           '
        diagCode  = 'UU      M1      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'SubMpsiY'
        diagTitle =
     &    'Sub-Meso transp.stream-funct. magnitude (Psi0): V component'
        diagUnits = 'm^2/s           '
        diagCode  = 'VV      M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
#endif

        diagName  = 'GM_Kux  '
        diagTitle = 'K_11 element (U.point, X.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        diagCode  = 'UU P    MR      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Kvy  '
        diagTitle = 'K_22 element (V.point, Y.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        diagCode  = 'VV P    MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Kuz  '
        diagTitle = 'K_13 element (U.point, Z.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        diagCode  = 'UU      MR      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Kvz  '
        diagTitle = 'K_23 element (V.point, Z.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        diagCode  = 'VV      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Kwx  '
        diagTitle = 'K_31 element (W.point, X.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        diagCode  = 'UM      LR      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Kwy  '
        diagTitle = 'K_32 element (W.point, Y.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        diagCode  = 'VM      LR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Kwz  '
        diagTitle = 'K_33 element (W.point, Z.dir) of GM-Redi tensor'
        diagUnits = 'm^2/s           '
        diagCode  = 'WM P    LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GM_PsiX '
        diagTitle = 'GM Bolus transport stream-function : U component'
        diagUnits = 'm^2/s           '
        diagCode  = 'UU      LR      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_PsiY '
        diagTitle = 'GM Bolus transport stream-function : V component'
        diagUnits = 'm^2/s           '
        diagCode  = 'VV      LR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_KuzTz'
        diagTitle = 'Redi Off-diagonal Temperature flux: X component'
        diagUnits = 'degC.m^3/s      '
        diagCode  = 'UU      MR      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_KvzTz'
        diagTitle = 'Redi Off-diagonal Temperature flux: Y component'
        diagUnits = 'degC.m^3/s      '
        diagCode  = 'VV      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_KwzTz'
        diagTitle = 'Redi main-diagonal vertical Temperature flux'
        diagUnits = 'degC.m^3/s      '
        diagCode  = 'WM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GM_ubT  '
        diagTitle = 'Zonal Mass-Weight Bolus Transp of Pot Temp'
        diagUnits = 'degC.m^3/s      '
        diagCode  = 'UUr     MR      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_vbT  '
        diagTitle = 'Meridional Mass-Weight Bolus Transp of Pot Temp'
        diagUnits = 'degC.m^3/s      '
        diagCode  = 'VVr     MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

#ifdef ALLOW_EDDYPSI
        diagName  = 'GMEdTauX'
        diagTitle = 'eddy-induced stress X-comp. estimated from Kwx'
        diagUnits = 'N/m^2           '
        diagCode  = 'UM      LR      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GMEdTauY'
        diagTitle = 'eddy-induced stress Y-comp. estimated from Kwy'
        diagUnits = 'N/m^2           '
        diagCode  = 'VM      LR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
#endif

#ifdef GM_BOLUS_BVP
        diagName  = 'GM_BVPcW'
        diagTitle = 'WKB wave speed (at Western edge location)'
        diagUnits = 'm/s             '
        diagCode  = 'SU P    M1      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_BVPcS'
        diagTitle = 'WKB wave speed (at Southern edge location)'
        diagUnits = 'm/s             '
        diagCode  = 'SV P    M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
#endif

c     ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
