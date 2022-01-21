C this needs changes

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
      SUBROUTINE STREAMICE_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize STREAMICE variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_BDRY.h"
c#ifdef ALLOW_STREAMICE_FLUX_CONTROL
c#include "STREAMICE_CTRL_FLUX.h"
c#endif

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_STREAMICE
C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
c     INTEGER iarr, tarr

      NAMELIST /STREAMICE_PARM01/
     &     streamice_density, streamice_density_ocean_avg,
     &     streamice_density_firn,
     &     B_glen_isothermal, n_glen, eps_glen_min, eps_u_min,
     &     C_basal_fric_const, n_basal_friction,
     &     streamice_vel_update,streamice_cg_tol,streamice_nonlin_tol,
     &     streamice_nonlin_tol_fp, streamice_err_norm,
     &     streamice_max_cg_iter, streamice_max_nl_iter,
     &     streamice_maxcgiter_cpl, streamice_maxnliter_cpl,
     &     STREAMICEthickInit,
     &     STREAMICEsigcoordInit,
     &     STREAMICEsigcoordFile,
     &     STREAMICEthickFile,
     &     STREAMICEcalveMaskFile,
     &     STREAMICEcostMaskFile,
     &     STREAMICE_dump_mdsio, STREAMICE_tave_mdsio,
     &     STREAMICE_dump_mnc, STREAMICE_tave_mnc,
     &     STREAMICE_move_front,
     &     STREAMICE_calve_to_mask,
     &     STREAMICE_diagnostic_only,
     &     STREAMICE_lower_cg_tol,
     &     streamice_CFL_factor,
     &     streamice_adjDump,
     &     streamice_bg_surf_slope_x, streamice_bg_surf_slope_y,
     &     streamice_kx_b_init, streamice_ky_b_init,
     &     STREAMICEbasalTracConfig,
     &     STREAMICEBdotConfig,
     &     STREAMICEAdotConfig,
     &     STREAMICEbasalTracFile,
     &     STREAMICEBdotFile,
     &     STREAMICEAdotFile,
     &     STREAMICEBdotTimeDepFile,
     &     STREAMICEvelOptimFile,
     &     STREAMICEtopogFile,
     &     STREAMICEhmaskFile,
     &     STREAMICEHBCyFile,
     &     STREAMICEHBCxFile,
     &     STREAMICEuFaceBdryFile,
     &     STREAMICEvFaceBdryFile,
     &     STREAMICEuDirichValsFile,
     &     STREAMICEvDirichValsFile,
     &     STREAMICEuMassFluxFile,
     &     STREAMICEvMassFluxFile,
     &     STREAMICEuNormalStressFile,
     &     STREAMICEvNormalStressFile,
     &     STREAMICEuShearStressFile,
     &     STREAMICEvShearStressFile,
     &     STREAMICEuNormalTimeDepFile,
     &     STREAMICEvNormalTimeDepFile,
     &     STREAMICEuShearTimeDepFile,
     &     STREAMICEvShearTimeDepFile,
     &     STREAMICEuFluxTimeDepFile, STREAMICEvFluxTimeDepFile,
     &     STREAMICEGlenConstFile, STREAMICEGlenConstConfig,
     &     STREAMICE_ppm_driving_stress,
     &     STREAMICE_h_ctrl_const_surf,
     &     streamice_wgt_drift,streamice_wgt_surf,streamice_wgt_vel,
     &     streamice_wgt_avthick, streamice_wgt_tikh,
     &     streamice_addl_backstress,
     &     streamice_smooth_gl_width,
     &     streamice_adot_uniform,
     &     streamice_firn_correction,
     &     STREAMICE_apply_firn_correction,
     &     STREAMICE_ADV_SCHEME, streamice_forcing_period,
     &     STREAMICE_chkfixedptconvergence,
     &     STREAMICE_chkresidconvergence,
     &     STREAMICE_alt_driving_stress,
     &     STREAMICE_allow_reg_coulomb,
     &     STREAMICE_use_log_ctrl,
     &     STREAMICE_vel_ext,
     &     STREAMICE_vel_ext_cgrid,
     &     STREAMICE_uvel_ext_file,
     &     STREAMICE_vvel_ext_file,
#ifdef STREAMICE_FLOWLINE_BUTTRESS
     &     streamice_buttr_width,
     &     useStreamiceFlowlineButtr,
#endif
     &     STREAMICE_allow_cpl
#ifdef ALLOW_OPENAD
     &     ,streamice_smooth_thick_adjoint
#endif

#ifdef ALLOW_STREAMICE_2DTRACER
      NAMELIST /STREAMICE_PARMTRACER/
     &     STREAMICETrac2DBCxFile,
     &     STREAMICETrac2DBCyFile,
     &     STREAMICETrac2DINITFile
#endif

#ifdef ALLOW_PETSC
      NAMELIST /STREAMICE_PARMPETSC/
     &     PETSC_PRECOND_TYPE, PETSC_SOLVER_TYPE,
     &     streamice_use_petsc, streamice_maxnliter_petsc,
     &     streamice_petsc_pcfactorlevels
#endif

#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP))
      NAMELIST /STREAMICE_PARMOAD/
     &     streamice_nonlin_tol_adjoint,
     &     streamice_nonlin_tol_adjoint_rl
#ifdef ALLOW_PETSC
     &     ,STREAMICE_OAD_petsc_reuse,
     &     PETSC_PRECOND_OAD
#endif
#endif

c#ifdef ALLOW_STREAMICE_FLUX_CONTROL
c      NAMELIST /STREAMICE_PARMFLUXCTRL/
c     &     n_fluxes, n_epochs,
c     &     streamice_ctrl_flux_id,
c     &     streamice_ctrl_flux_scaleVel
c#endif

      NAMELIST /STREAMICE_PARM02/
     &     shelf_max_draft,
     &     shelf_min_draft,
     &     shelf_edge_pos,
     &     shelf_slope_scale,
     &     shelf_flat_width,
     &     flow_dir

      NAMELIST /STREAMICE_PARM03/
     &     min_x_noflow_NORTH, max_x_noflow_NORTH,
     &     min_x_noflow_SOUTH, max_x_noflow_SOUTH,
     &     min_y_noflow_WEST, max_y_noflow_WEST,
     &     min_y_noflow_EAST, max_y_noflow_EAST,
     &     min_x_noStress_NORTH, max_x_noStress_NORTH,
     &     min_x_noStress_SOUTH, max_x_noStress_SOUTH,
     &     min_y_noStress_WEST, max_y_noStress_WEST,
     &     min_y_noStress_EAST, max_y_noStress_EAST,
     &     min_x_FluxBdry_NORTH, max_x_FluxBdry_NORTH,
     &     min_x_FluxBdry_SOUTH, max_x_FluxBdry_SOUTH,
     &     min_y_FluxBdry_WEST, max_y_FluxBdry_WEST,
     &     min_y_FluxBdry_EAST, max_y_FluxBdry_EAST,
     &     min_x_Dirich_NORTH, max_x_Dirich_NORTH,
     &     min_x_Dirich_SOUTH, max_x_Dirich_SOUTH,
     &     min_y_Dirich_WEST, max_y_Dirich_WEST,
     &     min_y_Dirich_EAST, max_y_Dirich_EAST,
     &     min_x_CFBC_NORTH, max_x_CFBC_NORTH,
     &     min_x_CFBC_SOUTH, max_x_CFBC_SOUTH,
     &     min_y_CFBC_WEST, max_y_CFBC_WEST,
     &     min_y_CFBC_EAST, max_y_CFBC_EAST,
     &     flux_bdry_val_SOUTH, flux_bdry_val_NORTH,
     &     flux_bdry_val_WEST, flux_bdry_val_EAST,
     &     STREAMICE_NS_periodic, STREAMICE_EW_periodic

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useStreamIce ) THEN
C-    pkg STREAMICE is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
         STREAMICEisOn = .FALSE.
C     print a (weak) warning if data.streamice is found
         CALL PACKAGES_UNUSED_MSG( 'useStreamIce', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      STREAMICEisOn = .TRUE.

C--   Default values for STREAMICE

      streamice_density = 917.
      streamice_density_ocean_avg = 1024.
      streamice_density_firn = streamice_density
      B_glen_isothermal = 9.461e-18   !  Pa (-1/3) a
      n_glen = 3.
      eps_glen_min = 1.0e-12
      eps_u_min = 1.0e-6
      C_basal_fric_const = 31.71 ! Pa (m/a)-1n
      n_basal_friction = -1.
      streamice_vel_update = deltaT ! seconds
      streamice_cg_tol = 1e-6
      streamice_nonlin_tol = 1e-6
      streamice_nonlin_tol_fp = 1.e-14
      streamice_err_norm = 0.
#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP))
      streamice_nonlin_tol_adjoint = 1.e-14
      streamice_nonlin_tol_adjoint_rl = 1.e-14
#ifdef ALLOW_PETSC
      PETSC_PRECOND_OAD = 'MUMPS'
      STREAMICE_OAD_petsc_reuse =.false.
#endif
#endif
      streamice_max_cg_iter = 2000
      streamice_max_nl_iter = 100
      streamice_maxnliter_petsc = -1
      streamice_maxcgiter_cpl = 0
      streamice_maxnliter_cpl = 0
      streamice_petsc_pcfactorlevels = 0
#ifdef ALLOW_OPENAD
      streamice_smooth_thick_adjoint = 0
#endif
c      streamice_n_sub_regularize = 4
      streamice_CFL_factor = .5
      streamice_adjDump = 0.
      streamice_bg_surf_slope_x = .0
      streamice_bg_surf_slope_y = 0.
      streamice_kx_b_init = 1.
      streamice_ky_b_init = 1.
      streamice_wgt_drift = 0.
      streamice_wgt_tikh = 0.
      streamice_wgt_surf = 0.
      streamice_wgt_vel = 0.
      streamice_wgt_avthick = 0.
      streamice_addl_backstress = 0.0
      streamice_smooth_gl_width = 0.0
      streamice_adot_uniform = 0.0
      streamice_forcing_period = 0
      streamice_firn_correction = 0.
#ifdef STREAMICE_FLOWLINE_BUTTRESS
      streamice_buttr_width = 1000000000.
#endif
      STREAMICE_apply_firn_correction = .false.

      STREAMICEthickInit = 'FILE'
      STREAMICEthickFile = ' '
      STREAMICEcalveMaskFile = ' '
      STREAMICEsigcoordInit = 'UNIFORM'
      STREAMICEsigcoordFile = ' '
      STREAMICEbasalTracConfig = 'UNIFORM'
      STREAMICEBdotConfig = ''
      STREAMICEAdotConfig = ''
      STREAMICEBdotFile = ''
      STREAMICEAdotFile = ''
      STREAMICEBdotTimeDepFile = ' '
      STREAMICEbasalTracFile = ' '
      STREAMICEvelOptimFile = ''
      STREAMICEtopogFile = ''
      STREAMICEhmaskFile = ''
      STREAMICEHBCyFile = ''
      STREAMICEHBCxFile = ''
      STREAMICEuNormalStressFile = ''
      STREAMICEvNormalStressFile = ''
      STREAMICEuShearStressFile = ''
      STREAMICEvShearStressFile = ''
      STREAMICEuNormalTimeDepFile = ' '
      STREAMICEvNormalTimeDepFile = ' '
      STREAMICEuShearTimeDepFile = ' '
      STREAMICEvShearTimeDepFile = ' '
      STREAMICEuFluxTimeDepFile = ' '
      STREAMICEvFluxTimeDepFile = ' '
      STREAMICE_vvel_ext_file = ' '
      STREAMICE_uvel_ext_file = ' '

#ifdef ALLOW_STREAMICE_2DTRACER
      STREAMICETrac2DBCxFile = ''
      STREAMICETrac2DBCyFile = ''
      STREAMICETrac2DInitFile = ''
#endif
      STREAMICEuFaceBdryFile = ''
      STREAMICEvFaceBdryFile = ''
      STREAMICEuDirichValsFile = ''
      STREAMICEvDirichValsFile = ''
      STREAMICEuMassFluxFile = ''
      STREAMICEvMassFluxFile = ''
      STREAMICEGlenConstFile = ''
      STREAMICEcostMaskFile = ''
      STREAMICEGlenConstConfig = 'UNIFORM'
#ifdef ALLOW_PETSC
      PETSC_PRECOND_TYPE = 'PCBJACOBI'
      PETSC_SOLVER_TYPE = 'KSPCG'
      streamice_use_petsc = .true.
#endif
      STREAMICE_ADV_SCHEME = ''

c#ifdef ALLOW_STREAMICE_FLUX_CONTROL
c      n_fluxes = 0
c      n_epochs = 0
c      DO iarr=1,n_fluxes_max
c       streamice_ctrl_flux_id(iarr) = 0
c       DO tarr=1,n_epochs_max
c        streamice_ctrl_flux_scaleVel(iarr,tarr) = 0. _d 0
c       ENDDO
c      ENDDO
c#endif

      STREAMICE_tave_mdsio = .TRUE.
      STREAMICE_dump_mdsio = .TRUE.
      STREAMICE_dump_mnc = .FALSE.
      STREAMICE_tave_mnc = .FALSE.
c      STREAMICE_GL_regularize = .FALSE.
      STREAMICE_move_front = .FALSE.
      STREAMICE_calve_to_mask = .FALSE.
c       STREAMICE_geom_file_setup = .FALSE.
c      STREAMICE_construct_matrix = .TRUE.
      STREAMICE_lower_cg_tol = .FALSE.
      STREAMICE_diagnostic_only = .FALSE.
#ifdef STREAMICE_FLOWLINE_BUTTRESS
      useStreamiceFlowlineButtr=.FALSE.
#endif
      STREAMICE_ppm_driving_stress = .FALSE.
      STREAMICE_chkfixedptconvergence = .true.
      STREAMICE_chkresidconvergence = .true.
      STREAMICE_alt_driving_stress = .FALSE.
      STREAMICE_allow_reg_coulomb = .FALSE.
      STREAMICE_h_ctrl_const_surf = .FALSE.
      STREAMICE_allow_cpl = .false.
      STREAMICE_use_log_ctrl = .false.
      STREAMICE_vel_ext = .FALSE.
      STREAMICE_vel_ext_cgrid = .FALSE.
c       STREAMICE_hybrid_stress= .FALSE.

      min_x_noflow_NORTH = 0.
      max_x_noflow_NORTH = 0.
      min_x_noflow_SOUTH = 0.
      max_x_noflow_SOUTH = 0.
      min_y_noflow_WEST = 0.
      max_y_noflow_WEST = 0.
      min_y_noflow_EAST = 0.
      max_y_noflow_EAST = 0.
      min_x_noStress_NORTH = 0.
      max_x_noStress_NORTH = 0.
      min_x_noStress_SOUTH = 0.
      max_x_noStress_SOUTH = 0.
      min_y_noStress_WEST = 0.
      max_y_noStress_WEST = 0.
      min_y_noStress_EAST = 0.
      max_y_noStress_EAST = 0.
      min_x_FluxBdry_NORTH = 0.
      max_x_FluxBdry_NORTH = 0.
      min_x_FluxBdry_SOUTH = 0.
      max_x_FluxBdry_SOUTH = 0.
      min_y_FluxBdry_WEST = 0.
      max_y_FluxBdry_WEST = 0.
      min_y_FluxBdry_EAST = 0.
      max_y_FluxBdry_EAST = 0.
      min_x_Dirich_NORTH = 0.
      max_x_Dirich_NORTH = 0.
      min_x_Dirich_SOUTH = 0.
      max_x_Dirich_SOUTH = 0.
      min_y_Dirich_WEST = 0.
      max_y_Dirich_WEST = 0.
      min_y_Dirich_EAST = 0.
      max_y_Dirich_EAST = 0.
      min_y_CFBC_WEST = 0.
      max_y_CFBC_WEST = 0.
      min_y_CFBC_EAST = 0.
      max_y_CFBC_EAST = 0.
      flux_bdry_val_SOUTH = 0.
      flux_bdry_val_NORTH = 0.
      flux_bdry_val_WEST = 0.
      flux_bdry_val_EAST = 0.

      STREAMICE_NS_periodic = .FALSE.
      STREAMICE_EW_periodic = .FALSE.

      WRITE(msgBuf,'(A)') 'STREAMICE_READPARMS: opening data.streamice'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.streamice', 'STREAMICE_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=STREAMICE_PARM01)
      WRITE(msgBuf,'(A)')
     &    'STREAMICE_READPARMS: read first param block'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)

      IF (TRIM(STREAMICEthickInit) .eq. "PARAM") THEN
       READ(UNIT=iUnit,NML=STREAMICE_PARM02)
        WRITE(msgBuf,'(A)')
     &    'STREAMICE_READPARMS: read second param block'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
      ENDIF

#ifdef ALLOW_STREAMICE_2DTRACER
      READ(UNIT=iUnit,NML=STREAMICE_PARMTRACER)
      WRITE(msgBuf,'(A)')
     &    'STREAMICE_READPARMS: read tracer param block'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
#endif

#ifdef ALLOW_PETSC
      READ(UNIT=iUnit,NML=STREAMICE_PARMPETSC)
      WRITE(msgBuf,'(A)')
     &    'STREAMICE_READPARMS: read petsc param block'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
#endif

#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP))
      READ(UNIT=iUnit,NML=STREAMICE_PARMOAD)
      WRITE(msgBuf,'(A)')
     &    'STREAMICE_READPARMS: read oad parm block'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
#endif

      READ(UNIT=iUnit,NML=STREAMICE_PARM03)
      WRITE(msgBuf,'(A)')
     &    'STREAMICE_READPARMS: read third param block'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

c#ifdef ALLOW_STREAMICE_FLUX_CONTROL
c
c      CALL OPEN_COPY_DATA_FILE(
c     I                     'data.strmctrlflux', 'STREAMICE_READPARMS',
c     O                     iUnit,
c     I                     myThid )
c
c      READ(UNIT=iUnit,NML=STREAMICE_PARMFLUXCTRL)
c      WRITE(msgBuf,'(A)')
c     &    'STREAMICE_READPARMS: read flux_ctrl param block'
c      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c     &    SQUEEZE_RIGHT , 1)
c#ifdef SINGLE_DISK_IO
c      CLOSE(iUnit)
c#else
c      CLOSE(iUnit,STATUS='DELETE')
c#endif /* SINGLE_DISK_IO */
c#endif

      streamice_nstep_velocity = NINT (streamice_vel_update / deltaT)

C-    Set Output type flags :

#ifdef ALLOW_MNC
      IF (useMNC) THEN
        IF ( .NOT.outputTypesInclusive
     &       .AND. STREAMICE_tave_mnc ) STREAMICE_tave_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. STREAMICE_dump_mnc ) STREAMICE_dump_mdsio = .FALSE.
      ENDIF
#endif

#ifdef ALLOW_PETSC
      IF (streamice_maxnliter_petsc.eq.-1) then
       streamice_maxnliter_petsc = streamice_max_nl_iter
      ENDIF
#endif

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_STREAMICE */

      RETURN
      END
