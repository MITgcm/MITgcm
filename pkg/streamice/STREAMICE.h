C---+----1--+-+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_STREAMICE

C     -------------------------- REAL PARAMS ---------------------------------------------------

C--   COMMON /STREAMICE_PARMS_R/ real valued parameters.
C     streamice_density           :: average ice density
C     streamice_density_ocean_avg :: average ocean density
C                                    determining ice floatation
C     B_glen_isothermal           :: (sqrt of) uniform ice stiffness
C                                    coefficient (Pa 1/2 yr 1/6)
C     n_glen                      :: Glen s law exponent
C     eps_glen_min                :: min strain rate in ice viscosity
C     eps_u_min                   :: min velocity in nonlinear sliding
C                                    law
C     C_basal_fric_const          :: (sqrt of) coefficient in sliding
C                                    law (Pa 1/2 (m/yr) m/2)
C     n_basal_friction            :: exponent in basal sliding law
C                                    (tau = C u^n)
C     streamice_input_flux_unif -- to remove
C     streamice_vel_update        :: frequency of velocity solve (s) --
C                                    coupled ice-ocean only
C     streamice_cg_tol            :: conj gradient tolerance
C     streamice_nonlin_tol        :: nonlinear solver tolerance
C                                    (relative residual, unitless)
C     streamice_nonlin_tol_fp     :: fixed point nonlinear solver
C                                    tolerance(absolute change, m/a)
C     streamice_nonlin_tol_adjoint:: fixed-point error of adjoint
C                                    iterative solve (absolute)
C     streamice_nonlin_tol_adjoint_rl
C                                 :: fixed-point error of adjoint
C                                    iterative solver (relative
C                                    reduction)
C  |  streamice_err_norm: the p-norm to find the error of the residual
C  |  or difference in the nonlin iteration
C  |  if less than 1, the sup-norm is used
C     streamice_CFL_factor        :: time step limiting factor
C     streamice_adjDump           :: write frequency (s) of adjoint
C                                    sensitivity fields
C     streamice_bg_surf_slope_x   :: uniform surface slope, x-dir
C     streamice_bg_surf_slope_y   :: uniform surface slope, y-dir
C     streamice_kx_b_init         :: x-wave number for periodically
C                                    initialised basal friction coeff
C     streamice_ky_b_init         :: y-wave number for periodically
C                                    initialised basal friction coeff
C     streamice_wgt_drift         :: cost function coefficient
C                                    of drift term
C     streamice_wgt_surf          :: cost function coefficient
C                                    of surface misfit term
C     streamice_wgt_avthick       :: cost function coefficient
C                                    of thickness misfit term
C     streamice_wgt_vel           :: cost function coefficient
C                                    of vel misfit term
C     streamice_wgt_tikh          :: cost function coefficient
C                                    of sq gradient penalty
C     streamice_addl_backstress   -- to remove
C     streamice_smooth_gl_width   :: grounding line regularisation
C                                    width (m)
C     streamice_adot_uniform      :: uniform surface mass balance (m/yr)
C     streamice_buttr_width       :: effective width for parameterisation
C                                    of buttressing -- flowline mode only
C     streamice_firn_correction   :: air thickness in column (m)
C     streamice_density_firn      :: firn density in column
C     streamice_forcing_period    :: forcing freq (s)

      COMMON /STREAMICE_PARMS_R/
     & streamice_density, streamice_density_ocean_avg,
     & B_glen_isothermal, n_glen, eps_glen_min, eps_u_min,
     & C_basal_fric_const, n_basal_friction, streamice_input_flux_unif,
     & streamice_vel_update, streamice_cg_tol, streamice_nonlin_tol,
     & streamice_nonlin_tol_fp, streamice_err_norm,
#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP))
     & streamice_nonlin_tol_adjoint, streamice_nonlin_tol_adjoint_rl,
#endif
     & streamice_CFL_factor, streamice_adjDump,
     & streamice_bg_surf_slope_x, streamice_bg_surf_slope_y,
     & streamice_kx_b_init, streamice_ky_b_init,
     & streamice_wgt_drift, streamice_wgt_surf,
     & streamice_wgt_avthick, streamice_wgt_vel,
     & streamice_wgt_tikh,
     & streamice_addl_backstress,
     & streamice_smooth_gl_width,
     & streamice_adot_uniform,
#ifdef STREAMICE_FLOWLINE_BUTTRESS
     & streamice_buttr_width,
#endif
     & streamice_firn_correction, streamice_density_firn,
     & streamice_forcing_period

      _RL streamice_density, streamice_density_ocean_avg
      _RL B_glen_isothermal, n_glen, eps_glen_min, eps_u_min
      _RL C_basal_fric_const
      _RL n_basal_friction
      _RL streamice_input_flux_unif
      _RL streamice_vel_update
      _RL streamice_cg_tol, streamice_nonlin_tol
      _RL streamice_nonlin_tol_fp
      _RL streamice_err_norm

#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP))
      _RL streamice_nonlin_tol_adjoint
      _RL streamice_nonlin_tol_adjoint_rl
#endif
      _RL streamice_CFL_factor
      _RL streamice_adjDump
      _RL streamice_bg_surf_slope_x, streamice_bg_surf_slope_y
      _RL streamice_kx_b_init, streamice_ky_b_init
      _RL streamice_wgt_drift, streamice_wgt_surf
      _RL streamice_wgt_avthick, streamice_wgt_vel
      _RL streamice_wgt_tikh
      _RL streamice_addl_backstress
      _RL streamice_smooth_gl_width
      _RL streamice_adot_uniform
      _RL streamice_forcing_period
#ifdef STREAMICE_FLOWLINE_BUTTRESS
      _RL streamice_buttr_width
#endif
      _RL streamice_firn_correction
      _RL streamice_density_firn

C     parms for parameterized initial thickness
C     SHELF_MAX_DRAFT: max thickness of ice in m
C     SHELF_MIN_DRAFT: min thickness of ice in m
C     SHELF_EDGE_POS: extent of ice shelf in (km?)
C     SHELF_SLOPE_SCALE: dist over which shelf slopes (km?)
C     SHELF_FLAT_WIDTH: width of flat shelf (km?)
C     also must be aware of units (m for cartesian, deg for curvilinear, m/deg for CYLINDRICAL POLAR)
C     FLOW_DIR: 1.0=west, 2.0=east, 3.0=south, 4.0=north

      COMMON /STREAMICE_H_INIT_R/
     & shelf_max_draft,
     & shelf_min_draft,
     & shelf_edge_pos,
     & shelf_slope_scale,
     & shelf_flat_width,
     & flow_dir
      _RL shelf_max_draft
      _RL shelf_min_draft
      _RL shelf_edge_pos
      _RL shelf_slope_scale
      _RL shelf_flat_width
      _RL flow_dir

C     -------------------------- INT PARAMS ---------------------------------------------------
C---+----1--+-+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   COMMON /STREAMICE_PARMS_I/ int valued parameters.
C     streamice_max_cg_iter             :: max CG iterations
C     streamice_max_nl_iter             :: max nonlin iterations in
C                                          vel solve
C     streamice_maxcgiter_cpl           :: max CG iters, coupled mode
C     streamice_maxnliter_cpl           :: max NL iters, coupled mode
C     streamice_maxnliter_Petsc         :: max NL iters with PETSC
C                                          unavailable with OpenAD
C     streamice_smooth_thick_adjoint    :: facility to smooth adjoint
C                                          thickness sensitivity after
C                                          advect_thickness
C                                          0 -> no smoothing
C     streamice_petsc_pcfactorlevels    :: fill level of incomplete
C                                          cholesky preconditioner
C                                          for use with PETSC and
C                                          BLOCKJACOBI precond ONLY

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER streamice_max_nl
      PARAMETER ( streamice_max_nl = 100 )
#endif

      COMMON /STREAMICE_PARMS_I/
     &     streamice_max_cg_iter, streamice_max_nl_iter,
     &     streamice_vel_upd_counter, streamice_nstep_velocity,
     &     streamice_maxcgiter_cpl, streamice_maxnliter_cpl,
     &     streamice_maxnliter_Petsc, petscFlag,
     &     streamice_petsc_pcfactorlevels
#ifdef ALLOW_OPENAD
     &     ,streamice_smooth_thick_adjoint
#endif
c     &     streamice_n_sub_regularize

      INTEGER streamice_max_cg_iter, streamice_max_nl_iter
      INTEGER streamice_vel_upd_counter, streamice_nstep_velocity
      INTEGER streamice_maxcgiter_cpl, streamice_maxnliter_cpl
      INTEGER streamice_maxnliter_Petsc, petscFlag
      INTEGER streamice_petsc_pcfactorlevels
#ifdef ALLOW_OPENAD
      INTEGER streamice_smooth_thick_adjoint
#endif
c      INTEGER streamice_n_sub_regularize

#if (defined (ALLOW_STREAMICE_OAD_FP))
      COMMON /STREAMICE_PARMS_I_OPENAD/
     &     isinloop0, isinloop1, isinloop2

      INTEGER isinloop0, isinloop1, isinloop2

#endif

C     -------------------------- CHAR PARAMS ---------------------------------------------------

C---+----1--+-+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   COMMON /STREAMICE_PARMS_C/ char valued parameters.
C     STREAMICEthickInit          :: mode of thickness initialisation
C                                    FILE - via STREAMICEthickFile
C                                    PARAM - from STREAMICE_H_INIT_R
C                                            common block
C     STREAMICEcalveMaskFile      :: calving mask file
C     STREAMICEsigcoordInit          TO REMOVE
C     STREAMICEsigcoordFile          TO REMOVE
C     STREAMICEdelsigFile            TO REMOVE
C     STREAMICEbasalTracConfig    :: mode of sliding factor init
C                                    FILE - via STREAMICEbasalTracFile
C                                    UNIFORM - C_basal_fric_const
C                                    1DPERIODIC - varies in x-dir
C                                     via streamice_kx_b_init and
C                                     C_basal_fric_const
C                                    2DPERIODIC - varies in x- and
C                                     y-dirs via streamice_kx_b_init
C                                     and streamice_ky_b_init
C     STREAMICEGlenConstConfig    :: mode of Glen s const init
C                                    FILE - via STREAMICEGlenConstFile
C                                    UNIFORM - B_glen_isothermal
C     STREAMICEBdotConfig         :: mode of ice-shelf melt rate init
C                                    FILE - via STREAMICEBdotFile
C                                    overridden in coupled mode
C     STREAMICEAdotConfig         :: mode of SMB init
C                                    FILE - via STREAMICEAdotFile
C                                    o/w streamice_adot_uniform
C     STREAMICEvelOptimFile       :: file prefix for obs velocities
C                                    in inversion e.g. 'velobs'
C                                    indicates 'velobsu.bin'
C                                    and       'velobsv.bin'
C     STREAMICEtopogFile          :: bed topography (separate from
C                                    ocean bathy)
C     STREAMICEhmaskFile          :: ice mask file
C                                    see EXPLANATION OF MASKS below
C     STREAMICEHBCxFile           :: upstream thickness at x-boundaries
C                                    -- to be used only with inhomogen.
C                                    velocity condition
C     STREAMICEHBCyFile           :: upstream thickness at y-boundaries
C     STREAMICEuFaceBdryFile      :: streamice_ufacemask_bdry values
C                                    see EXPLANATION OF MASKS below
C     STREAMICEvFaceBdryFile      :: streamice_vfacemask_bdry values
C                                    see EXPLANATION OF MASKS below
C     STREAMICEuDirichValsFile    :: inhomogeneous x-vel dirich values
C                                    to be set only where bound mask=3
C                                    see EXPLANATION OF MASKS below
C     STREAMICEvDirichValsFile    :: inhomogeneous y-vel dirich values
C                                    to be set only where bound mask=3
C                                    see EXPLANATION OF MASKS below
C     STREAMICEuMassFluxFile      :: file to set u_flux_bdry_SI
C                                    see EXPLANATION OF MASKS below
C     STREAMICEvMassFluxFile      :: file to set v_flux_bdry_SI
C                                    see EXPLANATION OF MASKS below
C
C     following give \gamma_sig and \gamma_tau factors as described
C     in appendix of
C     Goldberg et al, 2015 -- Committed retreat of Smith, Pope, and
C                             Kohler Glaciers over the next 30 years
C                             inferred by transient model calibration
C     applied only where stress boundary condition applies
C
C     STREAMICEuNormalStressFile
C     STREAMICEvNormalStressFile
C     STREAMICEuShearStressFile
C     STREAMICEvShearStressFile
C
C     time-dependent versions of above fields updated on frequency
C      streamice_forcing_period
C
C     STREAMICEuNormalTimeDepFile
C     STREAMICEvNormalTimeDepFile
C     STREAMICEuShearTimeDepFile
C     STREAMICEvShearTimeDepFile
C     STREAMICEuFluxTimeDepFile
C     STREAMICEvFluxTimeDepFile
C     STREAMICEBdotTimeDepFile
C     STREAMICEcostMaskFile       :: mask to be used in "custom" cost
C                                    function
C     STREAMICE_ADV_SCHEME        :: DST3 -- 3rd order direct ST
C                                    o/w 2nd order flux limited
C     PETSC_SOLVER_TYPE           :: CG, BICG, GMRES
C       see https://www.mcs.anl.gov/petsc/petsc-current/docs/manualpages/KSP/
C        default: CG
C     PETSC_PRECOND_TYPE          :: JACOBI -- a jacobi precond
C                                       (equiv to no petsc)
C                                    BLOCKJACOBI -- block incomplete
C                                       cholesky
C                                    GAMG -- Algebraic multigrid
C                                    MUMPS -- Direct
C                                    ILU -- incomplete ILU
C                                     (will not work in parallel)
C       see https://www.mcs.anl.gov/petsc/petsc-current/docs/manualpages/PC/
C     STREAMICE_uvel_ext_file     :: x-velocity file to replace velocity calc
C     STREAMICE_vvel_ext_file     :: y-velocity file to replace velocity calc

      CHARACTER*(MAX_LEN_FNAM) STREAMICEthickFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEthickInit
      CHARACTER*(MAX_LEN_FNAM) STREAMICEcalveMaskFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEsigcoordInit
      CHARACTER*(MAX_LEN_FNAM) STREAMICEsigcoordFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEdelsigFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEbasalTracConfig
      CHARACTER*(MAX_LEN_FNAM) STREAMICEGlenConstConfig
      CHARACTER*(MAX_LEN_FNAM) STREAMICEBdotConfig
      CHARACTER*(MAX_LEN_FNAM) STREAMICEAdotConfig
      CHARACTER*(MAX_LEN_FNAM) STREAMICEbasalTracFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEGlenConstFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEBdotFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEAdotFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEBdotTimeDepFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvelOptimFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEtopogFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEcostMaskFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICE_ADV_SCHEME

C     THE FOLLOWING FILENAMES ARE FOR SPECIFYING IRREGULAR DOMAIN GEOMETRIES
C     (i.e. boundaries that do not conform with rectangular walls)
      CHARACTER*(MAX_LEN_FNAM) STREAMICEhmaskFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEHBCxFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEHBCyFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEuFaceBdryFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvFaceBdryFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEuDirichValsFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvDirichValsFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEuMassFluxFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvMassFluxFile

C     THE FOLLOWING FILENAMES ARE FOR SPECIFYING buttressing along calving fronts
      CHARACTER*(MAX_LEN_FNAM) STREAMICEuNormalStressFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvNormalStressFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEuShearStressFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvShearStressFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEuNormalTimeDepFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvNormalTimeDepFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEuShearTimeDepFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvShearTimeDepFile

      CHARACTER*(MAX_LEN_FNAM) STREAMICEuFluxTimeDepFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvFluxTimeDepFile

      CHARACTER*(MAX_LEN_FNAM) STREAMICE_uvel_ext_file
      CHARACTER*(MAX_LEN_FNAM) STREAMICE_vvel_ext_file

#ifdef ALLOW_PETSC
c     CHARACTER PARAMS FOR PETSC
      CHARACTER*(MAX_LEN_FNAM) PETSC_SOLVER_TYPE
      CHARACTER*(MAX_LEN_FNAM) PETSC_PRECOND_TYPE
#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP))
      CHARACTER*(MAX_LEN_FNAM) PETSC_PRECOND_TMP
      CHARACTER*(MAX_LEN_FNAM) PETSC_PRECOND_OAD
#endif
#endif

#ifdef ALLOW_STREAMICE_2DTRACER
c     CHARACTER PARAMS FOR TRACER
      CHARACTER*(MAX_LEN_FNAM) STREAMICETrac2DBCxFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICETrac2DBCyFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICETrac2DinitFile
#endif

      COMMON /STREAMICE_PARM_C/
     &     STREAMICEthickInit,
     &     STREAMICEthickFile,
     &     STREAMICEcalveMaskFile,
     &     STREAMICEsigcoordInit,
     &     STREAMICEsigcoordFile,
     &     STREAMICEdelsigFile,
     &     STREAMICEbasalTracConfig,
     &     STREAMICEBdotConfig,
     &     STREAMICEAdotConfig,
     &     STREAMICEbasalTracFile,
     &     STREAMICEvelOptimFile,
     &     STREAMICEtopogFile,
     &     STREAMICEhmaskFile,
     &     STREAMICEHBCxFile,
     &     STREAMICEHBCyFile,
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
     &     STREAMICEuFluxTimeDepFile,
     &     STREAMICEvFluxTimeDepFile,
     &     STREAMICEGlenConstFile,
     &     STREAMICEBdotFile,
     &     STREAMICEAdotFile,
     &     STREAMICEBdotTimeDepFile,
     &     STREAMICEGlenConstConfig,
     &     STREAMICEcostMaskFile,
     &     STREAMICE_ADV_SCHEME,
     &     STREAMICE_uvel_ext_file,
     &     STREAMICE_vvel_ext_file

#ifdef ALLOW_PETSC
      COMMON /PETSC_PARM_C/
     &     PETSC_SOLVER_TYPE,
     &     PETSC_PRECOND_TYPE
#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP))
     &     ,PETSC_PRECOND_TMP, PETSC_PRECOND_OAD
#endif
#endif

#ifdef ALLOW_STREAMICE_2DTRACER
      COMMON /STREAMICE_TRAC2D_C/
     &     STREAMICETrac2DBCxFile,
     &     STREAMICETrac2DBCyFile,
     &     STREAMICETrac2DinitFile
#endif

C     -------------------------- LOGICAL PARAMS ---------------------------------------------------

C---+----1--+-+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   COMMON /STREAMICE_PARMS_L/ bool valued parameters.
C     to remove:
C     STREAMICEison
C     STREAMICE_dump_mdsio
C     STREAMICE_dump_mnc
C     STREAMICE_tave_mnc
C     STREAMICE_dump_mnc
C     STREAMICE_construct_matrix
C     STREAMICE_h_ctrl_const_surf

C     STREAMICE_move_front            :: advance ice-shelf front
C     STREAMICE_calve_to_mask         :: do not advance front past
C                                        streamice_calve_mask
C     STREAMICE_lower_cg_tol          :: lower CG tolerance
C                                        when NL error is lowered
C                                        by factor of .5e2
C     STREAMICE_diagnostic_only       :: do not update thickness
C                                     :: or any other timedep flds
C     STREAMICE_ppm_driving_stress    :: use partial parabolic method
C                                        to reconstruct surf slope
C     STREAMICE_alt_driving_stress    :: use finite difference based
C                                        driving stress
C                                        (overrides above option)
C     STREAMICE_chkfixedptconvergence :: terminate velocity iteration
C                                         based on fp_error
C     STREAMICE_chkresidconvergence   :: terminate velocity iteration
C                                        based on residual error
C     STREAMICE_allow_cpl             :: enable streamice-ocean
C                                        coupling
C     STREAMICE_use_petsc
C     STREAMICE_apply_firn_correction
C     STREAMICE_allow_reg_coulomb     :: rather than using power-law
C                                        sliding, implements
C                                        "regularised coulomb"
C                                        sliding law
C        Asay-Davis et al (2016), Geosci. Model Dev.,
C     "Experimental design for three interrelated marine ice sheet..."
C     (eqn 11)
C     STREAMICE_use_log_ctrl          :: fields C_basal_friction
C                                        and Bglen (and initialisation
C                                        values) given as the
C                                        *logarithm* of physical values
C                                        (if false, sqrt is used)
C     STREAMICE_vel_ext           :: impose velocity with external files
C     STREAMICE_vel_ext_cgrid     :: impose velocity with external files on C grid
C                                 ::  (over-rides STREAMICE_vel_ext)

      LOGICAL STREAMICEison
      LOGICAL STREAMICE_dump_mdsio
      LOGICAL STREAMICE_tave_mdsio
      LOGICAL STREAMICE_dump_mnc
      LOGICAL STREAMICE_tave_mnc
      LOGICAL STREAMICE_move_front
      LOGICAL STREAMICE_calve_to_mask
      LOGICAL STREAMICE_construct_matrix
      LOGICAL STREAMICE_lower_cg_tol
      LOGICAL STREAMICE_diagnostic_only
      LOGICAL STREAMICE_ppm_driving_stress
      LOGICAL STREAMICE_h_ctrl_const_surf
      LOGICAL STREAMICE_chkfixedptconvergence
      LOGICAL STREAMICE_chkresidconvergence
      LOGICAL STREAMICE_allow_cpl
      LOGICAL STREAMICE_use_petsc
      LOGICAL STREAMICE_apply_firn_correction
      LOGICAL STREAMICE_alt_driving_stress
      LOGICAL STREAMICE_allow_reg_coulomb
      LOGICAL STREAMICE_use_log_ctrl
      LOGICAL STREAMICE_vel_ext
      LOGICAL STREAMICE_vel_ext_cgrid
#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP) )
#ifdef ALLOW_PETSC
      LOGICAL STREAMICE_need2createmat
      LOGICAL STREAMICE_need2destroymat
      LOGICAL STREAMICE_OAD_petsc_reuse
#endif
#endif
#ifdef STREAMICE_FLOWLINE_BUTTRESS
      LOGICAL useStreamiceFlowlineButtr
#endif

C     The following parameters specify periodic boundary conditions.
C     For now this will completely override all other boundary conditions
C     and apply to the entire boundary

      LOGICAL STREAMICE_NS_periodic
      LOGICAL STREAMICE_EW_periodic

C      LOGICAL STREAMICE_hybrid_stress

      COMMON /STREAMICE_PARM_L/
     & STREAMICEison,
     & STREAMICE_dump_mdsio, STREAMICE_tave_mdsio,
     & STREAMICE_dump_mnc, STREAMICE_tave_mnc,
     & STREAMICE_move_front,
     & STREAMICE_calve_to_mask,
     & STREAMICE_construct_matrix,
     & STREAMICE_lower_cg_tol,
     & STREAMICE_NS_periodic, STREAMICE_EW_periodic,
     & STREAMICE_diagnostic_only,
     & STREAMICE_ppm_driving_stress,
     & STREAMICE_h_ctrl_const_surf,
     & STREAMICE_chkfixedptconvergence,
     & STREAMICE_chkresidconvergence,
     & STREAMICE_allow_cpl, streamice_use_petsc,
     & STREAMICE_alt_driving_stress,
     & STREAMICE_allow_reg_coulomb,
     & STREAMICE_vel_ext,
     & STREAMICE_vel_ext_cgrid,
     & STREAMICE_use_log_ctrl,
#ifdef STREAMICE_FLOWLINE_BUTTRESS
     & useStreamiceFlowlineButtr,
#endif
     & STREAMICE_apply_firn_correction

#if (defined (ALLOW_OPENAD) && defined (ALLOW_STREAMICE_OAD_FP) )
#ifdef ALLOW_PETSC
      COMMON /STREAMICE_PERSIST_PETSC_L
     & STREAMICE_need2createmat, STREAMICE_need2destroymat,
     & STREAMICE_OAD_petsc_reuse
#endif
#endif

C     -------------------------- AND NOW ARRAYS ---------------------------------------------------

C     EXPLANATION OF MASKS

C     STREAMICE_hmask           VALUES  1=ice-covered cell
C                                       2=partially ice-covered cell (no dynamics)
C                                       0=ice-free cell (for now, it means the cell
C                                                           is treated as an open-ocean cell
C                                                           that ice shelf can flow into)
C                                       -1=outside computational domain; will not change
C
C     STREAMICE_umask           VALUES  1=degree of freedom;
C                                       0=homogeneous dirich condition
C                                       3=inhomogeneous dirich condition

C     STREAMICE_vmask           similar to umask

C     STREAMICE_ufacemask       VALUES  -1=unset,
C                                       0=no-flow boundary,
C                                       1=no-stress bdry
C                                       2=stress bdry condition,
C                                       3=inhomogeneous dirichlet boundary,
C                                       4=flux boundary: at these faces a flux will be specified by u_flux_bdry_SI
C
C     STREAMICE_vfacemask       similar to ufacemask
C     STREAMICE_ufacemask_bdry  field initialized at the beginning of simulation
C                               specified all ufacemask values except for calving front
C                               CONSTANT FOR A SIMULATION (ie not changes after streamice_init_fixed)
C     STREAMICE_vfacemask_bdry  CONSTANT FOR A SIMULATION
C     STREAMICE_calve_mask      specified allowed extent of ice shelf
C                                (should be integer, but do not know howvi sre to read ints from file)
C                               not necessarily used, but CONSTANT FOR A SIMULATION
C     STREAMICE_float_cond      will only be used if partial floatation is implemented

C     Short arrays (e.g. masks)
      COMMON /STREAMICE_FIELDS_RS/
     &     STREAMICE_hmask,
     &     STREAMICE_umask,
     &     STREAMICE_vmask,
     &     STREAMICE_ufacemask,
     &     STREAMICE_vfacemask,
     &     STREAMICE_ufacemask_bdry,
     &     STREAMICE_vfacemask_bdry,
     &     STREAMICE_float_cond,
     &     STREAMICE_calve_mask,
     &     STREAMICE_ctrl_mask,
     &     STREAMICE_cost_mask
      _RS STREAMICE_hmask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS STREAMICE_umask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS STREAMICE_vmask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS STREAMICE_ufacemask
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS STREAMICE_vfacemask
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS STREAMICE_ufacemask_bdry
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS STREAMICE_vfacemask_bdry
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS STREAMICE_float_cond
     &  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS STREAMICE_calve_mask
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS STREAMICE_ctrl_mask
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS STREAMICE_cost_mask
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C    NOTES :
C     STREAMICE_ufacemask_bdry, STREAMICE_vfacemask_bdry ARE CONSTANT (FIXED)
C     STREAMICE_hmask IS PART OF **STATE**
C     All other masks are updated within a timestep BASED ON STREAMICE_hmask

C    Number of quadrature points are hardcoded.. could turn into a CPP macro

C    REAL ARRAYS

      COMMON /STREAMICE_FIELDS_RL/
     &     H_streamice,
     &     U_streamice,
     &     V_streamice,
     &     visc_streamice,
     &     tau_beta_eff_streamice,
     &     float_frac_streamice,
     &     base_el_streamice,
     &     surf_el_streamice,
     &     area_shelf_streamice,
     &     mass_ice_streamice,
     &     u_flux_bdry_SI,
     &     v_flux_bdry_SI,
     &     h_ubdry_values_SI,
     &     h_vbdry_values_SI,
     &     u_bdry_values_SI,
     &     v_bdry_values_SI,
     &     STREAMICE_dummy_array,
     &     C_basal_friction,
     &     B_glen,
     &     BDOT_streamice, ADOT_streamice,BDOT_pert,ADOT_pert, ! mass balances in meters per year
     &     streamice_sigma_coord, streamice_delsigma,
     &     H_streamice_prev,
     &     u_new_si, v_new_si, streamice_u_tavg, streamice_v_tavg,
     &     u_streamice_ext, v_streamice_ext

#ifdef ALLOW_STREAMICE_FLUX_CONTROL
      COMMON /STREAMICE_FLUX_CONTROL/
     &      u_flux_bdry_pert,
     &      v_flux_bdry_pert
#endif

#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL
      COMMON /STREAMICE_STRESS_BOUNDARY/
     &      streamice_u_normal_pert,
     &      streamice_v_normal_pert,
     &      streamice_u_shear_pert,
     &      streamice_v_shear_pert,
     &      streamice_u_normal_stress,
     &      streamice_v_normal_stress,
     &      streamice_u_shear_stress,
     &      streamice_v_shear_stress
#endif

#ifdef STREAMICE_HYBRID_STRESS
      COMMON /STREAMICE_HYBRID/
     &     streamice_taubx, streamice_tauby,
     &     streamice_u_surf, streamice_v_surf,
     &     streamice_u_bed, streamice_v_bed,
     &     visc_streamice_full, streamice_omega, streamice_basal_geom,
     &     streamice_vert_shear_uz, streamice_vert_shear_vz
#endif

#ifdef ALLOW_STREAMICE_TIMEDEP_FORCING
      COMMON /STREAMICE_TIMEDEP_FORCE/
     &      bdot_streamice0,
     &      bdot_streamice1
#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL
     &      ,streamice_u_normal_stress0,
     &      streamice_u_normal_stress1,
     &      streamice_v_normal_stress0,
     &      streamice_v_normal_stress1,
     &      streamice_u_shear_stress0,
     &      streamice_u_shear_stress1,
     &      streamice_v_shear_stress0,
     &      streamice_v_shear_stress1
#endif
#ifdef ALLOW_STREAMICE_FLUX_CONTROL
     &      ,u_flux_bdry_SI_0,
     &      u_flux_bdry_SI_1,
     &      v_flux_bdry_SI_0,
     &      v_flux_bdry_SI_1
#endif
#endif

#ifdef ALLOW_STREAMICE_2DTRACER
      COMMON /STREAMICE_TRAC2D_FIELDS_RL/
     &     trac2d_ubdry_values_SI,
     &     trac2d_vbdry_values_SI,
     &     trac2d
#ifdef STREAMICE_TRACER_AB
      COMMON /STREAMICE_TRAC2D_AB_RL/
     &     GAD_trac_2d
#endif
#endif

#ifdef USE_ALT_RLOW
      COMMON /STREAMICE_RLOW/
     &     R_low_si
#endif

      _RL H_streamice           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL U_streamice           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL V_streamice           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL visc_streamice        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tau_beta_eff_streamice(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL float_frac_streamice  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL surf_el_streamice     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL base_el_streamice     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL area_shelf_streamice  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mass_ice_streamice    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL u_flux_bdry_SI    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v_flux_bdry_SI    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL h_ubdry_values_SI    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL h_vbdry_values_SI    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL u_bdry_values_SI    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v_bdry_values_SI    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL c_basal_friction    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL u_streamice_ext     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v_streamice_ext     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C      _RL A_glen    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef STREAMICE_3D_GLEN_CONST
      _RL B_glen    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#else
      _RL B_glen    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
      _RL streamice_sigma_coord (Nr)
      _RL streamice_delsigma (Nr)

#ifdef USE_ALT_RLOW
      _RL R_low_si    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

C     The following arrays are used for the hybrid stress balance
#ifdef STREAMICE_HYBRID_STRESS
      _RL streamice_taubx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_u_surf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_surf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_u_bed (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_bed (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_tauby (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_omega (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_basal_geom
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL visc_streamice_full
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL streamice_vert_shear_uz (Nr)
      _RL streamice_vert_shear_vz (Nr)
#endif

#ifdef ALLOW_STREAMICE_2DTRACER
      _RL trac2d_ubdry_values_SI
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL trac2d_vbdry_values_SI
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL trac2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef STREAMICE_TRACER_AB
      _RL GAD_trac_2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#endif

#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL
      _RL streamice_u_normal_stress
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_normal_stress
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_u_shear_stress
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_shear_stress
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_u_normal_pert
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_normal_pert
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_u_shear_pert
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_shear_pert
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_STREAMICE_FLUX_CONTROL
      _RL u_flux_bdry_pert
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v_flux_bdry_pert
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

      _RL ADOT_streamice (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C  IMPORTANT: MELT RATE IN METERS PER YEAR
C  POSITIVE WHERE MELTING
      _RL BDOT_streamice (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BDOT_pert (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ADOT_pert (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL H_streamice_prev (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v_new_si (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL u_new_si (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL STREAMICE_dummy_array (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_u_tavg (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_tavg (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE_TIMEDEP_FORCING
#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL
      _RL streamice_u_normal_stress0
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_u_normal_stress1
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_normal_stress0
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_normal_stress1
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_u_shear_stress0
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_u_shear_stress1
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_shear_stress0
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_shear_stress1
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_STREAMICE_FLUX_CONTROL
      _RL u_flux_bdry_SI_0
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v_flux_bdry_SI_0
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL u_flux_bdry_SI_1
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v_flux_bdry_SI_1
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
      _RL bdot_streamice0
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL bdot_streamice1
     &   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

      COMMON /STREAMICE_COST_RL/
     &       cost_func1_streamice,
     &       cost_vel_streamice,
     &       cost_surf_streamice
      _RL cost_func1_streamice(nSx,nSy)
      _RL cost_vel_streamice(nSx,nSy)
      _RL cost_surf_streamice(nSx,nSy)

C    NOTES :
C      REAL ARRAYS THAT COMPRISE "STATE":
C       H_streamice,
C       U_streamice,
C       V_streamice,
C       visc_streamice,
C       tau_beta_eff_streamice,
C       area_shelf_streamice
C       (and do not forget STREAMICE_hmask)
C
C       visc & tau are now calculated based on U,V in streamice_vel_solve
C        but with Hybdrid stress formulation they will become part of
C        velocity initial guess, so they are kept

#ifdef ALLOW_PETSC
      COMMON /STREAMICE_PETSC_DOFS_COMMON/
     &      streamice_petsc_dofs_u,
     &      streamice_petsc_dofs_v,
     &      n_dofs_process
      _RS streamice_petsc_dofs_u
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS streamice_petsc_dofs_v
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER n_dofs_process (0:nPx*nPy-1)
#endif

#if (defined (ALLOW_STREAMICE_OAD_FP))
      COMMON /STREAMICE_PHISTAGE_ARRS/
     &      U_streamice_dvals,
     &      V_streamice_dvals
      _RL U_streamice_dvals
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL V_streamice_dvals
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef STREAMICE_HYBRID_STRESS
      COMMON /STREAMICE_PHISTAGE_ARRS_HYBRID/
     & taubx_dvals, tauby_dvals,
     & visc_full_dvals,
     & taubx_new_si, tauby_new_si,
     & visc_full_new_si
      _RL taubx_new_si (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL taubx_dvals (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tauby_new_si (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tauby_dvals (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL visc_full_new_si (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL visc_full_dvals (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#endif

#endif /* ALLOW_STREAMICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
