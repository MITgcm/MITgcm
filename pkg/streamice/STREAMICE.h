C---+----1--+-+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_STREAMICE

C     -------------------------- REAL PARAMS ---------------------------------------------------

      COMMON /STREAMICE_PARMS_R/ 
     & streamice_density, streamice_density_ocean_avg, 
!     & A_glen_isothermal, n_glen, eps_glen_min, eps_u_min,
     & B_glen_isothermal, n_glen, eps_glen_min, eps_u_min,
     & C_basal_fric_const, n_basal_friction, streamice_input_flux_unif,
     & streamice_vel_update, streamice_cg_tol, streamice_nonlin_tol,
     & streamice_nonlin_tol_fp,
     & streamice_CFL_factor, streamice_adjDump, 
     & streamice_bg_surf_slope_x, streamice_bg_surf_slope_y,
     & streamice_kx_b_init, streamice_ky_b_init,
     & streamice_wgt_drift, streamice_wgt_surf,
     & streamice_wgt_avthick, streamice_wgt_vel, 
     & streamice_wgt_tikh,
     & streamice_addl_backstress,
     & streamice_smooth_gl_width,
     & streamice_adot_uniform
      _RL streamice_density, streamice_density_ocean_avg
!      _RL A_glen_isothermal, n_glen, eps_glen_min, eps_u_min
      _RL B_glen_isothermal, n_glen, eps_glen_min, eps_u_min
      _RL C_basal_fric_const
      _RL n_basal_friction 
      _RL streamice_input_flux_unif
      _RL streamice_vel_update 
      _RL streamice_cg_tol, streamice_nonlin_tol
      _RL streamice_nonlin_tol_fp
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

      INTEGER streamice_max_nl
      PARAMETER ( streamice_max_nl = 100 )

      COMMON /STREAMICE_PARMS_I/
     &     streamice_max_cg_iter, streamice_max_nl_iter,
     &     streamice_vel_upd_counter, streamice_nstep_velocity,
     &     streamice_n_sub_regularize
      INTEGER streamice_max_cg_iter, streamice_max_nl_iter
      INTEGER streamice_vel_upd_counter, streamice_nstep_velocity
      INTEGER streamice_n_sub_regularize

C     -------------------------- CHAR PARAMS ---------------------------------------------------

      CHARACTER*(MAX_LEN_FNAM) STREAMICEthickFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEthickInit
      CHARACTER*(MAX_LEN_FNAM) STREAMICEcalveMaskFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEsigcoordInit
      CHARACTER*(MAX_LEN_FNAM) STREAMICEsigcoordFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEdelsigFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEbasalTracConfig
      CHARACTER*(MAX_LEN_FNAM) STREAMICEGlenConstConfig
      CHARACTER*(MAX_LEN_FNAM) STREAMICEbasalTracFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEGlenConstFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvelOptimFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEtopogFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEcostMaskFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICE_ADV_SCHEME
      
!     THE FOLLOWING FILENAMES ARE FOR SPECIFYING IRREGULAR DOMAIN GEOMETRIES 
!     (i.e. boundaries that do not conform with rectangular walls)
      CHARACTER*(MAX_LEN_FNAM) STREAMICEhmaskFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEHBCxFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEHBCyFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEuFaceBdryFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvFaceBdryFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEuDirichValsFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEvDirichValsFile

#ifdef ALLOW_PETSC
!     CHARACTER PARAMS FOR PETSC
      CHARACTER*(MAX_LEN_FNAM) PETSC_SOLVER_TYPE
      CHARACTER*(MAX_LEN_FNAM) PETSC_PRECOND_TYPE
#endif
     
#ifdef ALLOW_STREAMICE_2DTRACER
!     CHARACTER PARAMS FOR TRACER
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
     &     STREAMICEGlenConstFile,
     &     STREAMICEGlenConstConfig,
     &     STREAMICEcostMaskFile,
     &     STREAMICE_ADV_SCHEME

#ifdef ALLOW_PETSC
      COMMON /PETSC_PARM_C/
     &     PETSC_SOLVER_TYPE,
     &     PETSC_PRECOND_TYPE
#endif

#ifdef ALLOW_STREAMICE_2DTRACER
      COMMON /STREAMICE_TRAC2D_C/
     &     STREAMICETrac2DBCxFile,
     &     STREAMICETrac2DBCyFile,
     &     STREAMICETrac2DinitFile
#endif
     
C     -------------------------- LOGICAL PARAMS ---------------------------------------------------

      LOGICAL STREAMICEison
      LOGICAL STREAMICE_dump_mdsio
      LOGICAL STREAMICE_tave_mdsio
      LOGICAL STREAMICE_dump_mnc
      LOGICAL STREAMICE_tave_mnc
      LOGICAL STREAMICE_GL_regularize, STREAMICE_move_front
      LOGICAL STREAMICE_calve_to_mask
      LOGICAL STREAMICE_construct_matrix
      LOGICAL STREAMICE_lower_cg_tol
      LOGICAL STREAMICE_diagnostic_only
      LOGICAL STREAMICE_ppm_driving_stress
      LOGICAL STREAMICE_h_ctrl_const_surf
      
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
     & STREAMICE_GL_regularize, STREAMICE_move_front,
     & STREAMICE_calve_to_mask,
     & STREAMICE_construct_matrix,
     & STREAMICE_lower_cg_tol,
     & STREAMICE_NS_periodic, STREAMICE_EW_periodic,
     & STREAMICE_diagnostic_only,
     & STREAMICE_ppm_driving_stress,
     & STREAMICE_h_ctrl_const_surf

C     -------------------------- AND NOW ARRAYS ---------------------------------------------------

C     EXPLANATION OF MASKS

C     STREAMICE_hmask           VALUES	1=ice-covered cell
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
C                                (should be integer, but don't know howvi sre to read ints from file)
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
!     &     A_glen,
     &     B_glen,
     &     BDOT_streamice, ADOT_streamice,  ! mass balances in meters per year
     &     streamice_sigma_coord, streamice_delsigma,
     &     H_streamice_prev

#ifdef STREAMICE_HYBRID_STRESS
      COMMON /STREAMICE_HYBRID/ 
     &     streamice_taubx, streamice_tauby,
     &     streamice_u_surf, streamice_v_surf,
     &     visc_streamice_full, streamice_omega, streamice_basal_geom,
     &     streamice_vert_shear_uz, streamice_vert_shear_vz     
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
      _RL C_basal_friction    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!      _RL A_glen    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL B_glen    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_sigma_coord (Nr)
      _RL streamice_delsigma (Nr)      

#ifdef USE_ALT_RLOW
      _RL R_low_si    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

C     The following arrays are used for the hybrid stress balance            
#ifdef STREAMICE_HYBRID_STRESS      
      _RL streamice_taubx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_tauby (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_u_surf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL streamice_v_surf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
      
      _RL ADOT_streamice (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!! IMPORTANT: MELT RATE IN METERS PER YEAR
!! POSITIVE WHERE MELTING
      _RL BDOT_streamice (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL H_streamice_prev (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL STREAMICE_dummy_array (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      
      
           
      COMMON /STREAMICE_COST_RL/
     &       cost_func1_streamice
      _RL cost_func1_streamice(nSx,nSy)
      
C    NOTES :
C      REAL ARRAYS THAT COMPRISE "STATE":
C       H_streamice,
C       U_streamice,
C       V_streamice,
C       visc_streamice,
C       tau_beta_eff_streamice,
C       area_shelf_streamice
C       (and don't forget STREAMICE_hmask)
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
      
      
#endif /* ALLOW_STREAMICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
