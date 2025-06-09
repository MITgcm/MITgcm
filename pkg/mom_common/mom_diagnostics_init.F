#include "MOM_COMMON_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE MOM_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Routine to initialize Momentum diagnostics
C      (from mom_common, mom_fluxform & mom_vecinv packages)

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid :: my Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
#ifdef ALLOW_MOM_COMMON
C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---  Add diagnostics to the (long) list of available diagnostics:

C--   add diagnostics for viscosity coefficients:
      diagName  = 'VISCAHZ '
      diagTitle = 'Harmonic Visc Coefficient (m2/s) (Zeta Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VISCA4Z '
      diagTitle = 'Biharmonic Visc Coefficient (m4/s) (Zeta Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VISCAHD '
      diagTitle = 'Harmonic Viscosity Coefficient (m2/s) (Div Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VISCA4D '
      diagTitle = 'Biharmonic Viscosity Coefficient (m4/s) (Div Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VISCAHW '
      diagTitle = 'Harmonic Viscosity Coefficient (m2/s) (W Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VISCA4W '
      diagTitle = 'Biharmonic Viscosity Coefficient (m4/s) (W Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VAHZMAX '
      diagTitle = 'CFL-MAX Harm Visc Coefficient (m2/s) (Zeta Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VA4ZMAX '
      diagTitle = 'CFL-MAX Biharm Visc Coefficient (m4/s) (Zeta Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VAHDMAX '
      diagTitle = 'CFL-MAX Harm Visc Coefficient (m2/s) (Div Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VA4DMAX '
      diagTitle = 'CFL-MAX Biharm Visc Coefficient (m4/s) (Div Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VAHZMIN '
      diagTitle = 'RE-MIN Harm Visc Coefficient (m2/s) (Zeta Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VA4ZMIN '
      diagTitle = 'RE-MIN Biharm Visc Coefficient (m4/s) (Zeta Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VAHDMIN '
      diagTitle = 'RE-MIN Harm Visc Coefficient (m2/s) (Div Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VA4DMIN '
      diagTitle = 'RE-MIN Biharm Visc Coefficient (m4/s) (Div Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VAHZLTH '
      diagTitle = 'Leith Harm Visc Coefficient (m2/s) (Zeta Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VA4ZLTH '
      diagTitle = 'Leith Biharm Visc Coefficient (m4/s) (Zeta Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VAHDLTH '
      diagTitle = 'Leith Harm Visc Coefficient (m2/s) (Div Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VA4DLTH '
      diagTitle = 'Leith Biharm Visc Coefficient (m4/s) (Div Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VAHZLTHD'
      diagTitle = 'LeithD Harm Visc Coefficient (m2/s) (Zeta Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VA4ZLTHD'
      diagTitle = 'LeithD Biharm Visc Coefficient (m4/s) (Zeta Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VAHDLTHD'
      diagTitle = 'LeithD Harm Visc Coefficient (m2/s) (Div Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VA4DLTHD'
      diagTitle = 'LeithD Biharm Visc Coefficient (m4/s) (Div Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_LEITH_QG
      diagName  = 'VAHZLTHQ'
      diagTitle = 'LeithQG Harm Visc Coefficient (m2/s) (Zeta Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VAHDLTHQ'
      diagTitle = 'LeithQG Harm Visc Coefficient (m2/s) (Div Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

      diagName  = 'VAHZSMAG'
      diagTitle = 'Smagorinsky Harm Visc Coefficient (m2/s) (Zeta Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VA4ZSMAG'
      diagTitle = 'Smagorinsky Biharm Visc Coeff. (m4/s) (Zeta Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SZ      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VAHDSMAG'
      diagTitle = 'Smagorinsky Harm Visc Coefficient (m2/s) (Div Pt)'
      diagUnits = 'm^2/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VA4DSMAG'
      diagTitle = 'Smagorinsky Biharm Visc Coeff. (m4/s) (Div Pt)'
      diagUnits = 'm^4/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C--   add diagnostics of simple derivative quantities:
      diagName  = 'momKE   '
      diagTitle = 'Kinetic Energy (in momentum Eq.)'
      diagCode  = 'SMR     MR      '
      diagUnits = 'm^2/s^2         '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_MOM_VECINV
      diagName  = 'momHDiv '
      diagTitle = 'Horizontal Divergence (in momentum Eq.)'
      diagCode  = 'SMR     MR      '
      diagUnits = 's^-1            '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'momVort3'
      diagTitle = '3rd component (vertical) of Vorticity'
      diagCode  = 'SZR     MR      '
      diagUnits = 's^-1            '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'Strain  '
      diagTitle = 'Horizontal Strain of Horizontal Velocities'
      diagCode  = 'SZR     MR      '
      diagUnits = 's^-1            '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'Tension '
      diagTitle = 'Horizontal Tension of Horizontal Velocities'
      diagCode  = 'SMR     MR      '
      diagUnits = 's^-1            '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_MOM_VECINV */

      diagName  = 'Stretch '
      diagTitle = 'Vortex stretching from QG Leith dynamic viscosity'
      diagCode  = 'SM      MR      '
      diagUnits = 's^-1            '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C--   add diagnostics of tendencies from each terms
      diagUnits = 'm/s^2           '

      diagName  = 'USidDrag'
      diagTitle = 'U momentum tendency from Side Drag'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'VSidDrag'
      diagTitle = 'V momentum tendency from Side Drag'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'Um_Diss '
      diagTitle = 'U momentum tendency from Dissipation'
     &          //' (Explicit part)'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'Vm_Diss '
      diagTitle = 'V momentum tendency from Dissipation'
     &          //' (Explicit part)'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'Um_ImplD'
      diagTitle = 'U momentum tendency from Dissipation'
     &          //' (Implicit part)'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'Vm_ImplD'
      diagTitle = 'V momentum tendency from Dissipation'
     &          //' (Implicit part)'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'Um_Advec'
      diagTitle = 'U momentum tendency from Advection terms'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'Vm_Advec'
      diagTitle = 'V momentum tendency from Advection terms'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'Um_Cori '
      diagTitle = 'U momentum tendency from Coriolis term'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'Vm_Cori '
      diagTitle = 'V momentum tendency from Coriolis term'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'Um_dPhiX'
      diagTitle = 'U momentum tendency from Pressure/Potential grad'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'Vm_dPhiY'
      diagTitle = 'V momentum tendency from Pressure/Potential grad'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'Um_Ext  '
      diagTitle = 'U momentum tendency from external forcing'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'Vm_Ext  '
      diagTitle = 'V momentum tendency from external forcing'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

#ifdef ALLOW_MOM_VECINV
      diagName  = 'Um_hDis2'
      diagTitle = 'U momentum tendency from harmonic visc alone'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'Vm_hDis2'
      diagTitle = 'V momentum tendency from harmonic visc alone'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'Um_hDis4'
      diagTitle = 'U momentum tendency from biharmonic visc alone'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'Vm_hDis4'
      diagTitle = 'V momentum tendency from biharmonic visc alone'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'Um_AdvZ3'
      diagTitle = 'U momentum tendency from Vorticity Advection'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'Vm_AdvZ3'
      diagTitle = 'V momentum tendency from Vorticity Advection'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'Um_AdvRe'
      diagTitle = 'U momentum tendency from vertical Advection'
     &          //' (Explicit part)'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'Vm_AdvRe'
      diagTitle = 'V momentum tendency from vertical Advection'
     &          //' (Explicit part)'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

#endif /* ALLOW_MOM_VECINV */

C-    diagnostics of metric-terms tendencies:
      diagName  = 'Um_Metr '
      diagTitle = 'U momentum tendency from Metric terms'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'Vm_Metr '
      diagTitle = 'V momentum tendency from Metric terms'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

#ifdef ALLOW_NONHYDROSTATIC
C-    vertical momentum tendencies
      IF ( usingPCoords ) THEN
        diagUnits = 'Pa/s^2          '
      ELSE
        diagUnits = 'm/s^2           '
      ENDIF
      diagName  = 'WSidDrag'
      diagTitle = 'Vertical momentum tendency from Side Drag'
      diagCode  = 'WMr     LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      diagName  = 'Wm_Diss '
      diagTitle = 'W momentum tendency from Dissipation'
      diagCode  = 'WMr     LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'Wm_Advec'
      diagTitle = 'W momentum tendency from Advection terms'
      diagCode  = 'WMr     LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      diagName  = 'Wm_Metr '
      diagTitle = 'W momentum tendency from Metric terms'
      diagCode  = 'WMr     LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_NONHYDROSTATIC */

#ifdef ALLOW_MOM_TEND_EXTRA_DIAGS
cHP putting this back in
      diagName  = 'UBotDrag'
      diagTitle = 'U momentum tendency from Bottom Drag'
      diagCode  = 'UUR     MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'VBotDrag'
      diagTitle = 'V momentum tendency from Bottom Drag'
      diagCode  = 'VVR     MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
#endif

C--   add diagnostics of advective & viscous flux :
C-    bottom fluxes:
      diagName  = 'botTauX '
      diagTitle = 'zonal bottom stress, >0 increases uVel'
      diagUnits = 'N/m^2           '
      diagCode  = 'UU      U1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'botTauY '
      diagTitle = 'meridional bottom stress, >0 increases vVel'
      diagUnits = 'N/m^2           '
      diagCode  = 'VV      U1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      IF ( usingPCoords ) THEN
        diagUnits = 'Pa.m^3/s^2      '
      ELSE
        diagUnits = 'm^4/s^2         '
      ENDIF

C-     Advective flux of Zonal momentum:
#ifdef ALLOW_MOM_FLUXFORM
      diagName  = 'ADVx_Um '
      diagTitle = 'Zonal      Advective Flux of U momentum'
      diagCode  = 'UM      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'ADVy_Um '
      diagTitle = 'Meridional Advective Flux of U momentum'
      diagCode  = 'VZ      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'ADVrE_Um'
      diagTitle = 'Vertical   Advective Flux of U momentum'
     &          //' (Explicit part)'
      diagCode  = 'WU      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_MOM_FLUXFORM */
c     diagName  = 'ADVrI_Um'
c     diagTitle = 'Vertical   Advective Flux of U momentum'
c    &          //' (Implicit part)'
c     diagCode  = 'WU      LR      '
c     CALL DIAGNOSTICS_ADDTOLIST( diagNum,
c    I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C-    Advective flux of Meridional momentum:
#ifdef ALLOW_MOM_FLUXFORM
      diagName  = 'ADVx_Vm '
      diagTitle = 'Zonal      Advective Flux of V momentum'
      diagCode  = 'UZ      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'ADVy_Vm '
      diagTitle = 'Meridional Advective Flux of V momentum'
      diagCode  = 'VM      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'ADVrE_Vm'
      diagTitle = 'Vertical   Advective Flux of V momentum'
     &          //' (Explicit part)'
      diagCode  = 'WV      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_MOM_FLUXFORM */
c     diagName  = 'ADVrI_Vm'
c     diagTitle = 'Vertical   Advective Flux of V momentum'
c    &          //' (Implicit part)'
c     diagCode  = 'WV      LR      '
c     CALL DIAGNOSTICS_ADDTOLIST( diagNum,
c    I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C-    Viscous flux of Zonal momentum :
#ifdef ALLOW_MOM_FLUXFORM
      diagName  = 'VISCx_Um'
      diagTitle = 'Zonal      Viscous Flux of U momentum'
      diagCode  = 'UM      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'VISCy_Um'
      diagTitle = 'Meridional Viscous Flux of U momentum'
      diagCode  = 'VZ      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'VISrE_Um'
      diagTitle = 'Vertical   Viscous Flux of U momentum'
     &          //' (Explicit part)'
      diagCode  = 'WU      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_MOM_FLUXFORM */
      diagName  = 'VISrI_Um'
      diagTitle = 'Vertical   Viscous Flux of U momentum'
     &          //' (Implicit part)'
      diagCode  = 'WU      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C-    Viscous flux of Meridional momentum :
#ifdef ALLOW_MOM_FLUXFORM
      diagName  = 'VISCx_Vm'
      diagTitle = 'Zonal      Viscous Flux of V momentum'
      diagCode  = 'UZ      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'VISCy_Vm'
      diagTitle = 'Meridional Viscous Flux of V momentum'
      diagCode  = 'VM      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
      diagName  = 'VISrE_Vm'
      diagTitle = 'Vertical   Viscous Flux of V momentum'
     &          //' (Explicit part)'
      diagCode  = 'WV      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_MOM_FLUXFORM */
      diagName  = 'VISrI_Vm'
      diagTitle = 'Vertical   Viscous Flux of V momentum'
     &          //' (Implicit part)'
      diagCode  = 'WV      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_MOM_COMMON */
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
