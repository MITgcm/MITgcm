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
        diagTitle = 'eddy-induced stress X-comp.'
        diagUnits = 'N/m^2           '
        diagCode  = 'UM      LR      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GMEdTauY'
        diagTitle = 'eddy-induced stress Y-comp.'
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

#ifdef GM_BATES_K3D
        diagName  = 'GM_BaK  '
        diagTitle = '3D diffusivity'
        diagUnits = 'm**2/s          '
        diagCode  = 'SM P    MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_KPV  '
        diagTitle = '3D eddy transport diffusivity'
        diagUnits = 'm**2/s          '
        diagCode  = 'SM P    MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_BaK_T'
        diagTitle = '3D Redi diffusivity'
        diagUnits = 'm**2/s          '
        diagCode  = 'SM P    MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

#ifdef HAVE_LAPACK
        diagName  = 'GM_A3D'
        diagTitle = '3D lower diagona'
        diagUnits = '1/m**2          '
        diagCode  = 'SM      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_B3D'
        diagTitle = '3D lower diagona'
        diagUnits = '1/m**2          '
        diagCode  = 'SM      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_C3D'
        diagTitle = '3D lower diagona'
        diagUnits = '1/m**2          '
        diagCode  = 'SM      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
#endif

        diagName  = 'GM_MODEC'
        diagTitle = 'Eigen vector for the first barcolinic mode'
        diagUnits = 'dimensionless   '
        diagCode  = 'SM      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_URMS'
        diagTitle = 'rms Eddy Velocity'
        diagUnits = 'm/s             '
        diagCode  = 'SM P    MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_UMC  '
        diagTitle = 'ubar-c'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_SFLYR'
        diagTitle = 'mixed layer depth'
        diagUnits = 'm               '
        diagCode  = 'SM P    MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_USTAR'
        diagTitle = 'u^*'
        diagUnits = 'm/s         '
        diagCode  = 'UU      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_VSTAR'
        diagTitle = 'v^*'
        diagUnits = 'm/s         '
        diagCode  = 'VV      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_RDEF'
        diagTitle = 'Deformation Radius'
        diagUnits = 'm               '
        diagCode  = 'SM P    M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_RURMS'
        diagTitle =
     I      'Mixing length for calculating u_rms: MIN(Rdef,Bates_Rmax)'
        diagUnits = 'm               '
        diagCode  = 'SM P    M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_RRHNS'
        diagTitle = 'Rhines scale: sqrt(urms/beta)'
        diagUnits = 'm               '
        diagCode  = 'SM P    M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_RMIX'
        diagTitle = 'Unmodulated Mixing Length: MIN(Rdef, RRhines)'
        diagUnits = 'm               '
        diagCode  = 'SM P    M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_SUPP'
        diagTitle = 'Suppression Factor for BatesK3d'
        diagUnits = 'none            '
        diagCode  = 'SM P    MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_dqdx '
        diagTitle = 'dq/dx'
        diagUnits = '1/(m*s)         '
        diagCode  = 'UU      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_dqdy '
        diagTitle = 'dq/dy'
        diagUnits = '1/(m*s)         '
        diagCode  = 'VV      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Kdqdx'
        diagTitle = 'K*dq/dx'
        diagUnits = 'm/s*2           '
        diagCode  = 'UU      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Kdqdy'
        diagTitle = 'K*dq/dy'
        diagUnits = 'm/s**2          '
        diagCode  = 'VV      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Xix  '
        diagTitle = '-k dq/dx expansion'
        diagUnits = 'm/s**2          '
        diagCode  = 'UU      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Xiy  '
        diagTitle = '-k dq/dy expansion'
        diagUnits = 'm/s**2          '
        diagCode  = 'VV      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Sx   '
        diagTitle = 'Zonal isopycnal slope'
        diagUnits = 'none            '
        diagCode  = 'UU      LR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_Sy   '
        diagTitle = 'Meridional isopycnal slope'
        diagUnits = 'none            '
        diagCode  = 'VV      LR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_TFLXX'
        diagTitle = 'Zonal thickness flux'
        diagUnits = '1/(m*s)         '
        diagCode  = 'UU      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_TFLXY'
        diagTitle = 'meridional thickness flux'
        diagUnits = '1/(m*s)         '
        diagCode  = 'VV      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_C'
        diagTitle = 'Doppler shifted long Rossby wave speed'
        diagUnits = 'm/s             '
        diagCode  = 'SM      M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_UBARO'
        diagTitle = 'Barotropic velocity'
        diagUnits = 'm/s             '
        diagCode  = 'SM      M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_EADY '
        diagTitle = 'Eady Growth rate'
        diagUnits = '1/s             '
        diagCode  = 'SM      M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_UBAR '
        diagTitle = 'Mean zonal velocity'
        diagUnits = 'm/s             '
        diagCode  = 'SM      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_M4   '
        diagTitle = 'M**4'
        diagUnits = '1/s**4          '
        diagCode  = 'SM      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_N2   '
        diagTitle = 'N**2'
        diagUnits = '1/s**2          '
        diagCode  = 'SM      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_M4_N2'
        diagTitle = 'M**4/N**2'
        diagUnits = '1/s**2          '
        diagCode  = 'SM      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_SLOPE'
        diagTitle = 'SLOPE'
        diagUnits = 'm/m             '
        diagCode  = 'SM      MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'GM_RENRM'
        diagTitle = 'renormalisation factor'
        diagUnits = 'none            '
        diagCode  = 'SM      M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

#ifdef GM_BATES_PASSIVE
        diagName  = 'GM_PSTAR'
        diagTitle = 'Diagnosed bolus streamfunction: V component'
        diagUnits = 'm**2/s          '
        diagCode  = 'VV      LR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
#endif
#endif

#ifdef ALLOW_GM_LEITH_QG
        diagName  = 'GM_LTHQG'
        diagTitle = 'QGLeith viscosity used as kappa_GM in GMRedi'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM      MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

c     ENDIF
# ifdef ALLOW_AUTODIFF
      diagName  = 'ADJkapgm'
      diagTitle = 'dJ/dKgm: Sensitivity to GM Intensity'
      diagUnits = 'dJ/d[m^2/s]'
      diagCode  = 'SMRA    MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ADJkapre'
      diagTitle = 'dJ/dKredi: Sensitivity to Redi Coefficient'
      diagUnits = 'dJ/d[m^2/s]'
      diagCode  = 'SMRA    MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
# endif

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
