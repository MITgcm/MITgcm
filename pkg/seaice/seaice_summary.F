#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_SUMMARY
      SUBROUTINE SEAICE_SUMMARY( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE SEAICE_SUMMARY
C     | o Summarize pkg/seaice parameters.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#ifdef ALLOW_SITRACER
# include "SEAICE_TRACER.h"
#endif

C     !INPUT PARAMETERS:
C     == routine arguments ==
C     myThid     :: my Thread Id. number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == local variables ==
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER buffI(1)
      CHARACTER*10 endList
      INTEGER ioUnit
#ifdef ALLOW_SITRACER
      INTEGER iTracer
#endif

C     == end of interface ==

      _BARRIER
      _BEGIN_MASTER(myThid)

      endList = '    ;     '
      ioUnit = standardMessageUnit

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// Seaice configuration (SEAICE_PARM01) >>> START <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--  Time-stepping related param.

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   Seaice time stepping configuration   > START <  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   ----------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      CALL WRITE_0D_RL( SEAICE_deltaTtherm,INDEX_NONE,
     & 'SEAICE_deltaTtherm=', ' /* thermodynamic timestep */')
      CALL WRITE_0D_RL( SEAICE_deltaTdyn  ,INDEX_NONE,
     & 'SEAICE_deltaTdyn  =', ' /* dynamic timestep */')
#ifdef SEAICE_ALLOW_EVP
      CALL WRITE_0D_RL( SEAICE_deltaTevp  ,INDEX_NONE,
     & 'SEAICE_deltaTevp  =', ' /* EVP timestep */')
#endif
      CALL WRITE_0D_L ( SEAICEuseBDF2, INDEX_NONE,
     & 'SEAICEuseBDF2  =',' /* use backw. differencing for mom. eq. */')
      CALL WRITE_0D_L ( SEAICEupdateOceanStress, INDEX_NONE,
     & 'SEAICEupdateOceanStress=', ' /* update Ocean surf. stress */')
      CALL WRITE_0D_L ( SEAICErestoreUnderIce, INDEX_NONE,
     & 'SEAICErestoreUnderIce  =', ' /* restore T and S under ice */')

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   Seaice dynamics configuration   > START <  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   ------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--  Seaice-Dynamics parameters
      CALL WRITE_0D_L ( SEAICEuseDYNAMICS, INDEX_NONE,
     & 'SEAICEuseDYNAMICS =', ' /* use dynamics */')

      IF (.NOT.SEAICEuseDYNAMICS) THEN
      WRITE(msgBuf,'(A)') '   pkg/seaice dynamics is OFF  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      ELSE

#ifdef SEAICE_CGRID
      CALL WRITE_0D_C( 'C-GRID', -1, INDEX_NONE,
     & 'model grid type   =', ' /* type of sea ice model grid */')
#else /* not SEAICE_CGRID */
      CALL WRITE_0D_C( 'B-GRID', -1, INDEX_NONE,
     & 'model grid type   =', ' /* type of sea ice model grid */')
#endif /* SEAICE_CGRID */
      CALL WRITE_0D_L ( SEAICEuseStrImpCpl, INDEX_NONE,
     & 'SEAICEuseStrImpCpl =', ' /* use strongly implicit coupling */')
      CALL WRITE_0D_L ( SEAICEusePicardAsPrecon, INDEX_NONE,
     & 'SEAICEusePicardAsPrecon =', ' /* Picard as preconditioner */')
      CALL WRITE_0D_L ( SEAICEuseLSR, INDEX_NONE,
     & 'SEAICEuseLSR      =', ' /* use default Picard-LSR solver */')
      CALL WRITE_0D_L ( SEAICEuseKrylov, INDEX_NONE,
     & 'SEAICEuseKrylov   =', ' /* use Picard-Krylov solver */')
      CALL WRITE_0D_L ( SEAICEuseEVP, INDEX_NONE,
     & 'SEAICEuseEVP      =', ' /* use EVP solver rather than LSR */')
      CALL WRITE_0D_L ( SEAICEuseJFNK, INDEX_NONE,
     & 'SEAICEuseJFNK     =', ' /* use JFNK solver */')
#ifdef SEAICE_ALLOW_FREEDRIFT
      CALL WRITE_0D_L ( SEAICEuseFREEDRIFT, INDEX_NONE,
     & 'SEAICEuseFREEDRIFT =', ' /* use free drift solution */')
#endif /* SEAICE_ALLOW_FREEDRIFT */
      CALL WRITE_0D_RL( OCEAN_drag        ,INDEX_NONE,
     & 'OCEAN_drag        =', ' /* air-ocean drag coefficient */')
      CALL WRITE_0D_RL( SEAICE_drag       ,INDEX_NONE,
     & 'SEAICE_drag       =', ' /* air-ice drag coefficient */')
      CALL WRITE_0D_RL( SEAICE_drag_south       ,INDEX_NONE,
     & 'SEAICE_drag_south      =', ' /* Southern Ocean SEAICE_drag */')
      CALL WRITE_0D_RL( SEAICE_waterDrag  ,INDEX_NONE,
     & 'SEAICE_waterDrag  =', ' /* water-ice drag (no units) */')
      CALL WRITE_0D_RL( SEAICE_waterDrag_south  ,INDEX_NONE,
     & 'SEAICE_waterDrag_south =',
     & ' /* Southern Ocean waterDrag (no units) */')
      CALL WRITE_0D_RL( SEAICEdWatMin ,INDEX_NONE, 'SEAICEdWatMin =',
     & ' /* minimum linear water-ice drag (in m/s) */')
#ifdef SEAICE_ALLOW_BOTTOMDRAG
      IF ( SEAICEbasalDragK2 .NE. 0. _d 0 ) THEN
       CALL WRITE_0D_RL( SEAICE_cBasalStar ,INDEX_NONE,
     &      'SEAICE_cBasalStar =', ' /* Basal drag parameter */')
       CALL WRITE_0D_RL( SEAICEbasalDragU0 ,INDEX_NONE,
     &      'SEAICEbasalDragU0 =', ' /* Basal drag parameter */')
       CALL WRITE_0D_RL( SEAICEbasalDragK1 ,INDEX_NONE,
     &      'SEAICEbasalDragK1 =', ' /* Basal drag parameter */')
      ENDIF
      CALL WRITE_0D_RL( SEAICEbasalDragK2 ,INDEX_NONE,
     &     'SEAICEbasalDragK2 =', ' /* Basal drag parameter */')
#endif /* SEAICE_ALLOW_BOTTOMDRAG */
      CALL WRITE_0D_L ( SEAICEuseTilt, INDEX_NONE,
     & 'SEAICEuseTilt     =', ' /* include surface tilt in dyna. */')
      CALL WRITE_0D_L ( SEAICEuseTEM, INDEX_NONE,
     & 'SEAICEuseTEM      =', ' /* use truncated ellipse rheology */')
      CALL WRITE_0D_RL( SEAICE_strength   ,INDEX_NONE,
     & 'SEAICE_strength   =', ' /* sea-ice strength Pstar */')
      CALL WRITE_0D_RL( SEAICE_cStar   ,INDEX_NONE,
     & 'SEAICE_cStar      =', ' /* sea-ice strength parameter cStar */')
      CALL WRITE_0D_RL( SEAICEpressReplFac,INDEX_NONE,
     & 'SEAICEpressReplFac=', ' /* press. replacement method factor */')
      CALL WRITE_0D_RL( SEAICE_tensilFac   ,INDEX_NONE,
     & 'SEAICE_tensilFac  =', ' /* sea-ice tensile strength factor */')
      CALL WRITE_0D_RL( SEAICE_tensilDepth ,INDEX_NONE,
     & 'SEAICE_tensilDepth=', ' /* crit. depth for tensile strength */')
      CALL WRITE_0D_RL( SEAICEpresH0   ,INDEX_NONE,
     & 'SEAICEpresH0   =', ' /* sea-ice strength Heff threshold */')
       CALL WRITE_0D_I ( SEAICEpresPow0,    INDEX_NONE,
     & 'SEAICEpresPow0 =', ' /* exponent for Heff<SEAICEpresH0 */')
       CALL WRITE_0D_I ( SEAICEpresPow1,    INDEX_NONE,
     & 'SEAICEpresPow1 =', ' /* exponent for Heff>SEAICEpresH0 */')
       CALL WRITE_0D_I ( SEAICEetaZmethod,  INDEX_NONE,
     & 'SEAICEetaZmethod =', ' /* method computing eta at Z-point */')
      CALL WRITE_0D_RL( SEAICE_zetaMaxFac  ,INDEX_NONE,
     & 'SEAICE_zetaMaxFac =', ' /* factor for upper viscosity bound */')
      CALL WRITE_0D_RL( SEAICE_zetaMin  ,INDEX_NONE,
     & 'SEAICE_zetaMin    =', ' /* lower bound for viscosity */')
      CALL WRITE_0D_RL( SEAICE_eccen,INDEX_NONE,'SEAICE_eccen    =',
     & ' /* elliptical yield curve eccent */')
      CALL WRITE_0D_RL( SEAICEstressFactor,INDEX_NONE,
     & 'SEAICEstressFactor    =',' /* wind stress scaling factor */')
      CALL WRITE_0D_RL( SEAICE_airTurnAngle,INDEX_NONE,
     & 'SEAICE_airTurnAngle    =',' /* air-ice turning angle */')
      CALL WRITE_0D_RL( SEAICE_waterTurnAngle,INDEX_NONE,
     & 'SEAICE_waterTurnAngle  =',' /* ice-water turning angle */')
      CALL WRITE_0D_L ( SEAICEuseMetricTerms, INDEX_NONE,
     & 'SEAICEuseMetricTerms =', ' /* use metric terms */')
      CALL WRITE_0D_L ( SEAICE_no_slip, INDEX_NONE,
     & 'SEAICE_no_slip    =', ' /* no slip boundary conditions */')
      CALL WRITE_0D_L ( SEAICE_2ndOrderBC, INDEX_NONE,
     & 'SEAICE_2ndOrderBC =',
     & ' /* 2nd order no slip boundary conditions */')
      CALL WRITE_0D_L ( SEAICE_clipVelocities, INDEX_NONE,
     & 'SEAICE_clipVeloctities =', ' /* impose max. vels. */')
      CALL WRITE_0D_L ( useHB87stressCoupling, INDEX_NONE,
     & 'useHB87stressCoupling  =', ' /* altern. ice-ocean stress */')
      CALL WRITE_0D_L ( SEAICEscaleSurfStress, INDEX_NONE,
     & 'SEAICEscaleSurfStress  =',
     & ' /* scale atm. and ocean-surface stress with AREA */')
      CALL WRITE_0D_L ( SEAICE_maskRHS, INDEX_NONE,
     & 'SEAICE_maskRHS    =', ' /* mask RHS of solver */')
      CALL WRITE_0D_L ( SEAICEaddSnowMass, INDEX_NONE,
     & 'SEAICEaddSnowMass =', ' /* add snow mass to seaiceMassC/U/V */')
      IF (.NOT.SEAICEuseEVP) THEN
       CALL WRITE_0D_I ( LSR_mixIniGuess, INDEX_NONE,
     & 'LSR_mixIniGuess =',
     & ' /* mix free-drift sol. into LSR initial Guess */')
       CALL WRITE_0D_RL( SEAICE_LSRrelaxU  ,INDEX_NONE,
     & 'SEAICE_LSRrelaxU  =', ' /* LSR solver: relaxation parameter */')
       CALL WRITE_0D_RL( SEAICE_LSRrelaxV  ,INDEX_NONE,
     & 'SEAICE_LSRrelaxV  =', ' /* LSR solver: relaxation parameter */')
       CALL WRITE_0D_RL( LSR_ERROR         ,INDEX_NONE,
     & 'LSR_ERROR         =', ' /* sets accuracy of LSR solver */')
       CALL WRITE_0D_I ( SOLV_NCHECK,       INDEX_NONE,
     & 'SOLV_NCHECK       =', ' /* test interval for LSR solver */')
       CALL WRITE_0D_L ( SEAICEuseMultiTileSolver, INDEX_NONE,
     & 'SEAICEuseMultiTileSolver =',
     & ' /* use full domain tri-diag solver */')
       CALL WRITE_0D_I ( SEAICE_OLx,  INDEX_NONE,
     & 'SEAICE_OLx =', ' /* overlap for LSR/preconditioner */')
       CALL WRITE_0D_I ( SEAICE_OLy,  INDEX_NONE,
     & 'SEAICE_OLy =', ' /* overlap for LSR/preconditioner */')
       CALL WRITE_0D_I ( SEAICEnonLinIterMax,  INDEX_NONE,
     & 'SEAICEnonLinIterMax =',
     & ' /* max. number of nonlinear solver steps */')
       CALL WRITE_0D_I ( SEAICElinearIterMax,  INDEX_NONE,
     & 'SEAICElinearIterMax =',
     & ' /* max. number of linear solver steps */')
       CALL WRITE_0D_RL( SEAICEnonLinTol  ,INDEX_NONE,
     & 'SEAICEnonLinTol     =', ' /* non-linear solver tolerance */')
#if (defined SEAICE_ALLOW_JFNK) || (defined SEAICE_ALLOW_KRYLOV)
       IF ( SEAICEuseJFNK.OR.SEAICEuseKrylov ) THEN
        CALL WRITE_0D_I ( SEAICEpreconLinIter,  INDEX_NONE,
     & 'SEAICEpreconLinIter =',
     & ' /* number of linear preconditioner steps */')
        CALL WRITE_0D_I ( SEAICEpreconNL_Iter,  INDEX_NONE,
     & 'SEAICEpreconNL_Iter =',
     & ' /* number of non-linear preconditioner steps */')
        CALL WRITE_0D_L ( SEAICEuseIMEX, INDEX_NONE,
     &  'SEAICEuseIMEX  =', ' /* IMEX scheme with JFNK-solver */')
       ENDIF
       IF ( SEAICEuseJFNK ) THEN
        CALL WRITE_0D_I ( SEAICE_JFNK_lsIter,  INDEX_NONE,
     & 'SEAICE_JFNK_lsIter =', ' /* start of line search */')
        CALL WRITE_0D_I ( SEAICE_JFNK_lsLmax,  INDEX_NONE,
     & 'SEAICE_JFNK_lsLmax =', ' /* max. number line search iters */')
        CALL WRITE_0D_RL( SEAICE_JFNK_lsGamma,  INDEX_NONE,
     & 'SEAICE_JFNK_lsGamma =', ' /* line search step size param */')
        CALL WRITE_0D_I ( SEAICE_JFNK_tolIter,  INDEX_NONE,
     & 'SEAICE_JFNK_tolIter =', ' /* tol. is relaxed after this */')
        CALL WRITE_0D_RL( JFNKres_t  ,INDEX_NONE,
     & 'JFNKres_t=', ' /* JFNK parameter */')
        CALL WRITE_0D_RL( JFNKres_tFac  ,INDEX_NONE,
     & 'JFNKres_tFac=', ' /* JFNK parameter */')
        CALL WRITE_0D_RL( SEAICE_JFNKepsilon, INDEX_NONE,
     & 'SEAICE_JFNKepsilon=', ' /* JFNK: FD-gradient step size */')
        CALL WRITE_0D_RL( SEAICE_JFNKphi, INDEX_NONE,
     & 'SEAICE_JFNKphi =', ' /* JFNK: inexact Newtow parameter */')
        CALL WRITE_0D_RL( SEAICE_JFNKalpha, INDEX_NONE,
     &  'SEAICE_JFNKalpha =', ' /* JFNK: inexact Newtow parameter */')
       ENDIF
#endif /* SEAICE_ALLOW_JFNK or SEAICE_ALLOW_KRYLOV */
#ifdef SEAICE_ALLOW_EVP
      ELSE
       CALL WRITE_0D_RL( SEAICE_elasticParm  ,INDEX_NONE,
     & 'SEAICE_elasticParm=', ' /* EVP elastic parameter */')
       CALL WRITE_0D_RL( SEAICE_evpTauRelax  ,INDEX_NONE,
     & 'SEAICE_evpTauRelax=', ' /* EVP relaxation timescale */')
       CALL WRITE_0D_RL( SEAICE_evpDampC  ,INDEX_NONE,
     & 'SEAICE_evpDampC   =', ' /* EVP damping parameter */')
       CALL WRITE_0D_L ( SEAICEuseEVPstar, INDEX_NONE,
     & 'SEAICEuseEVPstar  =', ' /* use EVP* solver */')
       CALL WRITE_0D_L ( SEAICEuseEVPrev, INDEX_NONE,
     & 'SEAICEuseEVPrev   =', ' /* use "revisited EVP" solver */')
       CALL WRITE_0D_RL( SEAICE_evpAlpha  ,INDEX_NONE,
     & 'SEAICE_evpAlpha   =', ' /* EVP* parameter*/')
       CALL WRITE_0D_RL( SEAICE_evpBeta   ,INDEX_NONE,
     & 'SEAICE_evpBeta    =', ' /* EVP*  parameter */')
       CALL WRITE_0D_RL( SEAICEaEVPcoeff ,INDEX_NONE,
     & 'SEAICEaEVPcoeff   =', ' /* adaptive EVP parameter*/')
       CALL WRITE_0D_RL( SEAICEaEVPcStar ,INDEX_NONE,
     & 'SEAICEaEVPcStar   =', ' /* adaptive EVP parameter*/')
       CALL WRITE_0D_RL( SEAICEaEVPalphaMin ,INDEX_NONE,
     & 'SEAICEaEVPalphaMin=', ' /* adaptive EVP parameter*/')
       CALL WRITE_0D_I ( SEAICEnEVPstarSteps,  INDEX_NONE,
     & 'SEAICEnEVPstarSteps =', ' /* num. of EVP* steps */')
       CALL WRITE_0D_L ( SEAICEuseEVPpickup, INDEX_NONE,
     & 'SEAICEuseEVPpickup=', ' /* start EVP solver with EVP pickup*/')
#endif /* SEAICE_ALLOW_EVP */
      ENDIF

C     end if SEAICEuseDYNAMICS bloc
      ENDIF

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   Seaice advection diffusion config,   > START <  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   -----------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      IF ( useThSIce ) THEN
      WRITE(msgBuf,'(A)')
     &'   ==> advection diffusion done in pkg ThSIce'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      ELSE

      CALL WRITE_0D_L ( SEAICEmomAdvection, INDEX_NONE,
     & 'SEAICEmomAdvection =', ' /* advect sea ice momentum */')
#ifdef SEAICE_ALLOW_MOM_ADVECTION
      IF ( SEAICEmomAdvection ) THEN
      CALL WRITE_0D_I( SEAICEselectKEscheme, INDEX_NONE,
     & 'SEAICEselectKEscheme=',
     &              ' /* V.I Kinetic Energy scheme selector */')
      CALL WRITE_0D_I( SEAICEselectKEscheme, INDEX_NONE,
     & 'SEAICEselectVortscheme=',
     &              ' /* V.I Vorticity scheme selector */')
      CALL WRITE_0D_L ( SEAICEhighOrderVorticity, INDEX_NONE,
     & 'SEAICEhighOrderVorticity =',
     &              ' /* V.I Scheme selector for Vorticity-Term */')
      CALL WRITE_0D_L ( SEAICEupwindVorticity, INDEX_NONE,
     & 'SEAICEupwindVorticity =',
     &             ' /* V.I High order vort. advect. flag */')
      CALL WRITE_0D_L ( SEAICEuseAbsVorticity, INDEX_NONE,
     & 'SEAICEuseAbsVorticity =',
     &              ' /* V.I Works with f+zeta in Coriolis */')
      CALL WRITE_0D_L ( SEAICEuseJamartMomAdv, INDEX_NONE,
     & 'SEAICEuseJamartMomAdv =',
     &              ' /* V.I Non-linear terms Jamart flag */')
      ENDIF
#endif
      CALL WRITE_0D_L ( SEAICEadvHeff, INDEX_NONE,
     & 'SEAICEadvHeff =', ' /* advect effective ice thickness */')
      CALL WRITE_0D_L ( SEAICEadvArea, INDEX_NONE,
     & 'SEAICEadvArea =', ' /* advect fractional ice area */')
      CALL WRITE_0D_L ( SEAICEadvSnow, INDEX_NONE,
     & 'SEAICEadvSnow =', ' /* advect snow layer together with ice */')
#ifdef SEAICE_VARIABLE_SALINITY
      CALL WRITE_0D_L ( SEAICEadvSalt, INDEX_NONE,
     & 'SEAICEadvSalt =', ' /* advect salinity together with ice */')
#endif
      CALL WRITE_0D_I ( SEAICEadvScheme,   INDEX_NONE,
     & 'SEAICEadvScheme   =', ' /* advection scheme for ice */')
      IF ( SEAICEadvScheme .EQ. 2 )
     & CALL WRITE_0D_L ( SEAICEuseFluxForm,   INDEX_NONE,
     & 'SEAICEuseFluxForm =', ' /* advection in FV flux form */')
      IF ( SEAICEadvArea )
     &CALL WRITE_0D_I ( SEAICEadvSchArea,   INDEX_NONE,
     & 'SEAICEadvSchArea   =', ' /* advection scheme for area */')
      IF ( SEAICEadvHeff )
     &CALL WRITE_0D_I ( SEAICEadvSchHeff,   INDEX_NONE,
     & 'SEAICEadvSchHeff   =', ' /* advection scheme for thickness */')
      IF ( SEAICEadvSnow )
     &CALL WRITE_0D_I ( SEAICEadvSchSnow,   INDEX_NONE,
     & 'SEAICEadvSchSnow   =', ' /* advection scheme for snow */')
#ifdef SEAICE_VARIABLE_SALINITY
      IF ( SEAICEadvSalt )
     &CALL WRITE_0D_I ( SEAICEadvSchSalt,   INDEX_NONE,
     & 'SEAICEadvSchSalt   =', ' /* advection scheme for salt */')
#endif
      CALL WRITE_0D_RL( SEAICEdiffKhArea,   INDEX_NONE,
     & 'SEAICEdiffKhArea   =', ' /* diffusivity (m^2/s) for area */')
      CALL WRITE_0D_RL( SEAICEdiffKhHeff,   INDEX_NONE,
     & 'SEAICEdiffKhHeff   =', ' /* diffusivity (m^2/s) for heff */')
      CALL WRITE_0D_RL( SEAICEdiffKhSnow,   INDEX_NONE,
     & 'SEAICEdiffKhSnow   =', ' /* diffusivity (m^2/s) for snow */')
      IF ( SEAICEadvSalt )
     &CALL WRITE_0D_RL( SEAICEdiffKhSalt,   INDEX_NONE,
     & 'SEAICEdiffKhSalt   =', ' /* diffusivity (m^2/s) for salt */')
      CALL WRITE_0D_RL( DIFF1, INDEX_NONE, 'DIFF1             =',
     &                  ' /* parameter used in advect.F [m/s] */')

C     end if useThSIce bloc
      ENDIF

#ifdef SEAICE_ITD
C--   ITD parameters
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   Seaice ice thickness distribution configuration   > START <  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   -----------------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL WRITE_0D_I (SEAICE_multDim,INDEX_NONE,'nITD              ='
     & , ' /* number of ice thickness categories */')
      CALL WRITE_1D_RL( Hlimit(1), nITD, INDEX_K, 'Hlimit =',
     & ' /* seaice thickness category bin limits ( m ), Hlimit(0)=0 */')
      CALL WRITE_0D_L ( SEAICEuseLinRemapITD, INDEX_NONE,
     & 'SEAICEuseLinRemapITD  =',
     & ' /* select linear remapping scheme for ITD */')
      CALL WRITE_0D_L ( useHibler79IceStrength, INDEX_NONE,
     & 'useHibler79IceStrength  =',
     & ' /* select ice strength parameterizationd */')
      CALL WRITE_0D_L ( SEAICEsimpleRidging, INDEX_NONE,
     & 'SEAICEsimpleRidging  =',
     & ' /* select ridging scheme */')
      CALL WRITE_0D_I (SEAICEpartFunc,INDEX_NONE,
     &     'SEAICEpartFunc   ='
     & , ' /* select ridging participation function */')
      CALL WRITE_0D_I (SEAICEredistFunc,INDEX_NONE,
     &     'SEAICEredistFunc ='
     & , ' /* select ridging redistribution function */')
      CALL WRITE_0D_RL( SEAICE_cf  ,INDEX_NONE,
     & 'SEAICE_cf  =', ' /* ice strength parameter */')
      CALL WRITE_0D_RL( SEAICEshearParm  ,INDEX_NONE,
     & 'SEAICEshearParm  =', ' /* amount of energy lost to shear */')
      CALL WRITE_0D_RL( SEAICEgStar  ,INDEX_NONE,
     & 'SEAICEgStar  =', ' /* ridging parameter */')
      CALL WRITE_0D_RL( SEAICEhStar  ,INDEX_NONE,
     & 'SEAICEhStar  =', ' /* ridging parameter */')
      CALL WRITE_0D_RL( SEAICEaStar  ,INDEX_NONE,
     & 'SEAICEaStar  =', ' /* ridging parameter */')
      CALL WRITE_0D_RL( SEAICEmuRidging  ,INDEX_NONE,
     & 'SEAICEmuRidging  =', ' /* ridging parameter */')
      CALL WRITE_0D_RL( SEAICEmaxRaft  ,INDEX_NONE,
     & 'SEAICEmaxRaft  =', ' /* ridging parameter */')
      CALL WRITE_0D_RL( SEAICEsnowFracRidge ,INDEX_NONE,
     & 'SEAICEsnowFracRidge  =',
     &     ' /* fraction of snow remaining on ridges */')
#endif

C--   Thermodynamics parameters
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   Seaice thermodynamics configuration   > START <  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   -----------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C- note: air and seaice (+ snow ?) density are used in both dynamics & thermo
      CALL WRITE_0D_RL( SEAICE_rhoIce     ,INDEX_NONE,
     & 'SEAICE_rhoIce     =', ' /* density of sea ice (kg/m3) */')
      CALL WRITE_0D_RL( SEAICE_rhoSnow     ,INDEX_NONE,
     & 'SEAICE_rhoSnow    =', ' /* density of snow (kg/m3) */')
      CALL WRITE_0D_RL( SEAICE_rhoAir     ,INDEX_NONE,
     & 'SEAICE_rhoAir     =', ' /* density of air (kg/m3) */')

      CALL WRITE_0D_L ( usePW79thermodynamics, INDEX_NONE,
     & 'usePW79thermodynamics  =', ' /* default 0-layer TD */')
      IF (.NOT.usePW79thermodynamics) THEN
      WRITE(msgBuf,'(A)') '   pkg/seaice thermodynamics is OFF  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      ELSE
#ifdef SEAICE_USE_GROWTH_ADX
      WRITE(msgBuf,'(A)')
     &      'SEAICE_USE_GROWTH_ADX defined, '//
     &      'using seaice_growth_adx code'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
#endif /* SEAICE_USE_GROWTH_ADX */

      CALL WRITE_0D_RL( SEAICE_lhEvap     ,INDEX_NONE,
     & 'SEAICE_lhEvap     =', ' /* latent heat of evaporation */')
      CALL WRITE_0D_RL( SEAICE_lhFusion   ,INDEX_NONE,
     & 'SEAICE_lhFusion   =', ' /* latent heat of fusion */')
      CALL WRITE_0D_RL ( SEAICE_mcPheePiston,   INDEX_NONE,
     & 'SEAICE_mcPheePiston =',
     & ' /* turbulent flux "piston velocity" a la McPhee (m/s) */')
      CALL WRITE_0D_RL ( SEAICE_mcPheeTaper,     INDEX_NONE,
     & 'SEAICE_mcPheeTaper =',
     & ' /* tapering of turbulent flux (0.< <1.) for AREA=1. */')
      CALL WRITE_0D_L ( SEAICE_mcPheeStepFunc,   INDEX_NONE,
     & 'SEAICE_mcPheeStepFunc =',
     & ' /* replace linear tapering with step funct. */')
      CALL WRITE_0D_RL ( SEAICE_frazilFrac,   INDEX_NONE,
     & 'SEAICE_frazilFrac =',
     & ' /* frazil (T<tempFrz) to seaice conversion rate (0.< <1.) */')
      CALL WRITE_0D_RL( SEAICE_tempFrz0   ,INDEX_NONE,
     & 'SEAICE_tempFrz0   =',
     & ' /* freezing temp. of sea water (intercept) */')
      CALL WRITE_0D_RL( SEAICE_dTempFrz_dS,INDEX_NONE,
     & 'SEAICE_dTempFrz_dS=',
     & ' /* freezing temp. of sea water (slope) */')
      CALL WRITE_0D_L ( SEAICE_growMeltByConv , INDEX_NONE,
     & 'SEAICE_growMeltByConv  =', ' /* grow,melt by vert. conv. */')
      CALL WRITE_0D_L ( SEAICE_doOpenWaterGrowth , INDEX_NONE,
     & 'SEAICE_doOpenWaterGrowth =', ' /* grow by open water */')
      CALL WRITE_0D_L ( SEAICE_doOpenWaterMelt , INDEX_NONE,
     & 'SEAICE_doOpenWaterMelt =', ' /* melt by open water */')
c print the various values meining for SEAICE_areaGain/LossFormula
      WRITE(msgBuf,'(2A)') 'SEAICE_areaGainFormula =',
     & ' /* ice cover gain formula (1,2)*/'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      buffI(1) = SEAICE_areaGainFormula
      CALL PRINT_LIST_I( buffI, 1, 1, INDEX_NONE,
     &                   .FALSE., .TRUE., ioUnit )
      WRITE(msgBuf,'(A)') '    1=from growth by ATM '
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '    2=from predicted growth by ATM'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      CALL PRINT_MESSAGE(endList, ioUnit, SQUEEZE_RIGHT, myThid )
c
      WRITE(msgBuf,'(2A)') 'SEAICE_areaLossFormula =',
     & ' /* ice cover loss formula (1,2)*/'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      buffI(1) = SEAICE_areaLossFormula
      CALL PRINT_LIST_I( buffI, 1, 1, INDEX_NONE,
     &                   .FALSE., .TRUE., ioUnit )
      WRITE(msgBuf,'(2A)') '    1=from all but only melt ',
     &                     'conributions by ATM and OCN'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '    2=from net melt-grow>0 by ATM and OCN'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '    3=from predicted melt by ATM'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      CALL PRINT_MESSAGE(endList, ioUnit, SQUEEZE_RIGHT, myThid )
c
#ifdef EXF_SEAICE_FRACTION
      CALL WRITE_0D_RL( SEAICE_tauAreaObsRelax,INDEX_NONE,
     & 'SEAICE_tauAreaObsRelax=',
     & ' /* relaxation timescale of sea-ice concentration */')
#endif
c
      CALL WRITE_0D_RL( HO                ,INDEX_NONE,
     & 'HO                =', ' /* nominal thickness of new ice */')
      CALL WRITE_0D_RL( HO_south          ,INDEX_NONE,
     & 'HO_south               =', ' /* Southern Ocean HO */')
      CALL WRITE_0D_RL( SEAICE_area_max          ,INDEX_NONE,
     & 'SEAICE_area_max        =',
     & ' /* set to les than 1. to mimic open leads */')
#ifdef SEAICE_VARIABLE_SALINITY
      WRITE(msgBuf,'(A)')
     &'   Sea ice has a variable salinity such that '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL WRITE_0D_RL( SEAICE_saltFrac, INDEX_NONE,
     & 'SEAICE_saltFrac =',
     & ' /* fraction of ocn salinity in new ice */')
#else
      CALL WRITE_0D_RL( SEAICE_salt0,   INDEX_NONE,
     & 'SEAICE_salt0   =', ' /* constant sea ice salinity */')
#endif
      CALL WRITE_0D_L ( SEAICE_salinityTracer,   INDEX_NONE,
     & 'SEAICE_salinityTracer =', ' /* test SITR varia. salinity */')
      CALL WRITE_0D_L ( SEAICEuseFlooding, INDEX_NONE,
     & 'SEAICEuseFlooding =', ' /* turn submerged snow into ice */')

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   Seaice air-sea fluxes configuration,   > START <  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   -----------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      CALL WRITE_0D_L ( SEAICEheatConsFix,  INDEX_NONE,
     & 'SEAICEheatConsFix  =',
     & ' /* accound for ocn<->seaice advect. heat flux */')
#ifndef SEAICE_ITD
      CALL WRITE_0D_I (SEAICE_multDim,INDEX_NONE,'SEAICE_multDim    ='
     & , ' /* number of ice categories (1 or 7) */')
#endif
      CALL WRITE_1D_RL( SEAICE_PDF,nITD,INDEX_K, 'SEAICE_PDF        =',
     &  ' /* sea-ice distribution (-) */')
      CALL WRITE_0D_I ( IMAX_TICE,         INDEX_NONE,
     & 'IMAX_TICE         =', ' /* iterations for ice surface temp */')
      CALL WRITE_0D_I ( postSolvTempIter,  INDEX_NONE,
     & 'postSolvTempIter=',
     & ' /* flux calculation after surf. temp iter */')
#ifndef SEAICE_EXTERNAL_FLUXES
      CALL WRITE_0D_RL( SEAICE_waterAlbedo,INDEX_NONE,
     & 'SEAICE_waterAlbedo=', ' /* water albedo */')
      CALL WRITE_0D_RL( SEAICE_emissivity, INDEX_NONE,
     & 'SEAICE_emissivity =', ' /* ocean-surface emissivity */')
#endif /* SEAICE_EXTERNAL_FLUXES */
      CALL WRITE_0D_RL( SEAICE_dryIceAlb  ,INDEX_NONE,
     & 'SEAICE_dryIceAlb  =', ' /* winter albedo */')
      CALL WRITE_0D_RL( SEAICE_wetIceAlb  ,INDEX_NONE,
     & 'SEAICE_wetIceAlb  =', ' /* summer albedo */')
      CALL WRITE_0D_RL( SEAICE_drySnowAlb ,INDEX_NONE,
     & 'SEAICE_drySnowAlb =', ' /* dry snow albedo */')
      CALL WRITE_0D_RL( SEAICE_wetSnowAlb ,INDEX_NONE,
     & 'SEAICE_wetSnowAlb =', ' /* wet snow albedo */')
      CALL WRITE_0D_RL( SEAICE_dryIceAlb_south  ,INDEX_NONE,
     & 'SEAICE_dryIceAlb_south =', ' /* Southern Ocean dryIceAlb */')
      CALL WRITE_0D_RL( SEAICE_wetIceAlb_south  ,INDEX_NONE,
     & 'SEAICE_wetIceAlb_south =', ' /* Southern Ocean wetIceAlb */')
      CALL WRITE_0D_RL( SEAICE_drySnowAlb_south ,INDEX_NONE,
     & 'SEAICE_drySnowAlb_south=', ' /* Southern Ocean drySnowAlb */')
      CALL WRITE_0D_RL( SEAICE_wetSnowAlb_south ,INDEX_NONE,
     & 'SEAICE_wetSnowAlb_south=', ' /* Southern Ocean wetSnowAlb */')
      CALL WRITE_0D_RL( SEAICE_wetAlbTemp ,INDEX_NONE,
     & 'SEAICE_wetAlbTemp=',
     & ' /* Temp (o.C) threshold for wet-albedo */')
      CALL WRITE_0D_RL( SEAICE_snow_emiss ,INDEX_NONE,
     & 'SEAICE_snow_emiss =', ' /* snow emissivity */')
      CALL WRITE_0D_RL( SEAICE_ice_emiss ,INDEX_NONE,
     & 'SEAICE_ice_emiss =', ' /* seaice emissivity */')
      CALL WRITE_0D_RL( SEAICE_cpAir      ,INDEX_NONE,
     & 'SEAICE_cpAir      =', ' /* heat capacity of air */')
      CALL WRITE_0D_RL( SEAICE_dalton     ,INDEX_NONE,
     & 'SEAICE_dalton     =', ' /* constant dalton number */')
      CALL WRITE_0D_RL( SEAICE_iceConduct ,INDEX_NONE,
     & 'SEAICE_iceConduct =', ' /* sea-ice conductivity */')
      CALL WRITE_0D_RL( SEAICE_snowConduct,INDEX_NONE,
     & 'SEAICE_snowConduct=', ' /* snow conductivity */')
      CALL WRITE_0D_RL( SEAICE_snowThick  ,INDEX_NONE,
     & 'SEAICE_snowThick  =',
     & ' /* cutoff snow thickness (for albedo) */')
      CALL WRITE_0D_RL( SEAICE_shortwave  ,INDEX_NONE,
     & 'SEAICE_shortwave  =', ' /* penetration shortwave radiation */')
      CALL WRITE_0D_L ( useMaykutSatVapPoly, INDEX_NONE,
     & 'useMaykutSatVapPoly =',
     & ' /* use Maykut Polynomial for Sat.Vap.Pr */')
      CALL WRITE_0D_RL( MIN_ATEMP         ,INDEX_NONE,
     & 'MIN_ATEMP         =', ' /* minimum air temperature */')
      CALL WRITE_0D_RL( MIN_LWDOWN        ,INDEX_NONE,
     & 'MIN_LWDOWN        =', ' /* minimum downward longwave */')
      CALL WRITE_0D_RL( MIN_TICE          ,INDEX_NONE,
     & 'MIN_TICE          =', ' /* minimum ice temperature */')

C     end if usePW79thermodynamics bloc
      ENDIF

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   Seaice initialization and IO config.,   > START <  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   -------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C--  Initial Condition/Input related param.
      CALL WRITE_0D_RL( SEAICE_initialHEFF,INDEX_NONE,
     & 'SEAICE_initialHEFF=', ' /* initial sea-ice thickness */')
      CALL WRITE_0D_C( AreaFile, -1, INDEX_NONE,
     &  'AreaFile =', ' /* Initial ice concentration File */')
      CALL WRITE_0D_C( HeffFile, -1, INDEX_NONE,
     &  'HeffFile =', ' /* Initial effective ice thickness File */')
      CALL WRITE_0D_C( HsnowFile, -1, INDEX_NONE,
     &  'HsnowFile =', ' /* Initial snow thickness File */')
#ifdef SEAICE_VARIABLE_SALINITY
      CALL WRITE_0D_C( HsaltFile, -1, INDEX_NONE,
     &  'HsaltFile =', ' /* Initial HSALT File */')
#endif
      CALL WRITE_0D_C( uIceFile, -1, INDEX_NONE,
     &  'uIceFile =', ' /* Initial U-ice velocity File */')
      CALL WRITE_0D_C( vIceFile, -1, INDEX_NONE,
     &  'vIceFile =', ' /* Initial V-ice velocity File */')

C--  Output related param.
      CALL WRITE_0D_L ( SEAICEwriteState,  INDEX_NONE,
     & 'SEAICEwriteState  =', ' /* write sea ice state to file */')
      CALL WRITE_0D_RL( SEAICE_monFreq,  INDEX_NONE,
     & 'SEAICE_monFreq  =',' /* monitor frequency */')
      CALL WRITE_0D_RL( SEAICE_dumpFreq   ,INDEX_NONE,
     & 'SEAICE_dumpFreq   =', ' /* dump frequency */')
      CALL WRITE_0D_RL( SEAICE_taveFreq   ,INDEX_NONE,
     & 'SEAICE_taveFreq   =', ' /* time-averaging frequency */')
      CALL WRITE_0D_L ( SEAICE_mon_stdio,  INDEX_NONE,
     & 'SEAICE_mon_stdio  =',' /* write monitor to std-outp */')
      CALL WRITE_0D_L ( SEAICE_dump_mdsio,  INDEX_NONE,
     & 'SEAICE_dump_mdsio =',' /* write snap-shot   using MDSIO */')
      CALL WRITE_0D_L ( SEAICE_tave_mdsio,  INDEX_NONE,
     & 'SEAICE_tave_mdsio =',' /* write TimeAverage using MDSIO */')
#ifdef ALLOW_MNC
      CALL WRITE_0D_L ( SEAICE_mon_mnc,  INDEX_NONE,
     & 'SEAICE_mon_mnc    =',' /* write monitor to netcdf file */')
      CALL WRITE_0D_L ( SEAICE_dump_mnc,  INDEX_NONE,
     & 'SEAICE_dump_mnc   =',' /* write snap-shot   using MNC */')
      CALL WRITE_0D_L ( SEAICE_tave_mnc,  INDEX_NONE,
     & 'SEAICE_tave_mnc   =',' /* write TimeAverage using MNC */')
#endif /* ALLOW_MNC */

#ifdef ALLOW_SITRACER
       CALL WRITE_0D_I ( SItrNumInUse, INDEX_NONE,
     & 'SItrNumInUse =',
     & ' /* number of tracers that are in use (<SItrMaxNum) */')
      DO iTracer = 1, SItrNumInUse
       IF (SItrName(iTracer).NE.' ') THEN
      WRITE(msgBuf,'(A,I3,A)') '== SItracer no. ',iTracer,
     &' is in use and defined as'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
       CALL WRITE_0D_C ( SItrMate(iTracer), -1, INDEX_NONE,
     & '  -- SItrMate =',' /* tracer of HEFF or AREA */')
       CALL WRITE_0D_C ( SItrName(iTracer), -1, INDEX_NONE,
     & '  -- SItrName =',' /* tracer name*/')
       CALL WRITE_0D_C ( SItrNameLong(iTracer), -1, INDEX_NONE,
     & '  -- SItrNameLong =',' /* tracer long name*/')
       CALL WRITE_0D_C ( SItrFile(iTracer), -1, INDEX_NONE,
     & '  -- SItrFile =',' /* tracer initial condition file */')
       CALL WRITE_0D_C ( SItrUnit(iTracer), -1, INDEX_NONE,
     & '  -- SItrUnit =',' /* tracer unit */')
      CALL WRITE_0D_RL ( SItrFromOcean0(iTracer),     INDEX_NONE,
     & '  -- SItrFromOcean0 =',' /* for new ice from freeze */')
      CALL WRITE_0D_RL ( SItrFromOceanFrac(iTracer),  INDEX_NONE,
     & '  -- SItrFromOceanFrac =',' /* for new ice from freeze */')
      CALL WRITE_0D_RL ( SItrFromFlood0(iTracer),     INDEX_NONE,
     & '  -- SItrFromFlood0 =',' /* for new ice from freeze */')
      CALL WRITE_0D_RL ( SItrFromFloodFrac(iTracer),  INDEX_NONE,
     & '  -- SItrFromFloodFrac =',' /* for new ice from freeze */')
      CALL WRITE_0D_RL ( SItrExpand0(iTracer),     INDEX_NONE,
     & '  -- SItrExpand0 =',' /* for ice cover thermo. expans. */')

       ENDIF
      ENDDO
#endif /* ALLOW_SITRACER */

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   Seaice regularization numbers,   > START <  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'   -----------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      CALL WRITE_0D_RL( SEAICE_deltaMin   ,INDEX_NONE,
     & 'SEAICE_deltaMin   =', ' /* reduce singularities in Delta */')
      CALL WRITE_0D_RL( SEAICE_EPS        ,INDEX_NONE,
     & 'SEAICE_EPS        =', ' /* small number */')
      CALL WRITE_0D_RL( SEAICE_EPS_SQ        ,INDEX_NONE,
     & 'SEAICE_EPS_SQ     =', ' /* small number squared */')
      CALL WRITE_0D_RL( SEAICE_area_reg        ,INDEX_NONE,
     & 'SEAICE_area_reg   =', ' /* reduce derivative singularities */')
      CALL WRITE_0D_RL( SEAICE_hice_reg        ,INDEX_NONE,
     & 'SEAICE_hice_reg   =', ' /* reduce derivative singularities */')
      CALL WRITE_0D_RL( SEAICE_area_floor        ,INDEX_NONE,
     & 'SEAICE_area_floor =', ' /* reduce derivative singularities */')

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// Seaice configuration (SEAICE_PARM01) >>> END <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

      RETURN
      END
