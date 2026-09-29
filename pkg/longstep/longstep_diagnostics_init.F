#include "LONGSTEP_OPTIONS.h"

CBOP
C     !ROUTINE: LONGSTEP_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE LONGSTEP_DIAGNOSTICS_INIT( myThid )
C     !DESCRIPTION:
C     Routine to initialize longstep diagnostics

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LONGSTEP_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instance of LONGSTEP_DIAGNOSTICS_INIT
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      - Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C--   Add diagnostics to the (long) list

      diagName  = 'LSwVel  '
      diagTitle = 'Vertical Component of Velocity (m/s)'
      diagUnits = 'm/s             '
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'LSuVel  '
      diagTitle = 'Zonal Component of Velocity (m/s)'
      diagUnits = 'm/s             '
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'LSvVel  '
      diagTitle = 'Meridional Component of Velocity (m/s)'
      diagUnits = 'm/s             '
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'LStheta '
      diagTitle = 'Potential Temperature'
      diagUnits = 'degC            '
      diagCode  = 'SMR     MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'LSsalt  '
      diagTitle = 'Salinity'
      diagUnits = 'g/kg            '
      diagCode  = 'SMR     MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      IF ( ivdc_kappa .NE. 0. _d 0 ) THEN
        diagName  = 'LScnvAdj'
        diagTitle = 'Convective Adjustment Index [0-1] '
        diagUnits = 'fraction        '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      ENDIF

#ifdef SHORTWAVE_HEATING
      diagName  = 'LSqsw   '
      diagTitle = 'net Short-Wave radiation (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

      diagName  = 'LSfwFlux'
      diagTitle = 'net surface Fresh-Water flux into the ocean'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_GMREDI
      IF ( useGMRedi ) THEN
       diagName  = 'LSkwx   '
       diagTitle = 'K_31 element (W.point, X.dir) of GM-Redi tensor'
       diagUnits = 'm^2/s           '
       diagCode  = 'UM      LR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagName  = 'LSkwy   '
       diagTitle = 'K_32 element (W.point, Y.dir) of GM-Redi tensor'
       diagUnits = 'm^2/s           '
       diagCode  = 'VM      LR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagName  = 'LSkwz   '
       diagTitle = 'K_33 element (W.point, Z.dir) of GM-Redi tensor'
       diagUnits = 'm^2/s           '
       diagCode  = 'WM P    LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'LSkux   '
       diagTitle = 'K_11 element (U.point, X.dir) of GM-Redi tensor'
       diagUnits = 'm^2/s           '
       diagCode  = 'UU P    MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'LSkvy   '
       diagTitle = 'K_22 element (V.point, Y.dir) of GM-Redi tensor'
       diagUnits = 'm^2/s           '
       diagCode  = 'VV P    MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'LSkuz   '
       diagTitle = 'K_13 element (U.point, Z.dir) of GM-Redi tensor'
       diagUnits = 'm^2/s           '
       diagCode  = 'UU      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'LSkvz   '
       diagTitle = 'K_23 element (V.point, Z.dir) of GM-Redi tensor'
       diagUnits = 'm^2/s           '
       diagCode  = 'VV      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'LS_PsiX '
       diagTitle = 'GM Bolus transport Psi in longstep : U component'
       diagUnits = 'm^2/s           '
       diagCode  = 'UU      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'LS_PsiY '
       diagTitle = 'GM Bolus transport Psi in longstep : V component'
       diagUnits = 'm^2/s           '
       diagCode  = 'VV      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      ENDIF
#endif

#ifdef ALLOW_KPP
      IF ( useKPP ) THEN
       diagName  = 'LSKPPdfS'
       diagTitle = 'Vertical diffusion coefficient for salt & tracers'
       diagUnits = 'm^2/s           '
       diagCode  = 'SM P    LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'LSKPPght'
       diagTitle = 'Nonlocal transport coefficient'
       diagUnits = 's/m^2           '
       diagCode  = 'SM P    LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      ENDIF
#endif /* ALLOW_KPP */

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
