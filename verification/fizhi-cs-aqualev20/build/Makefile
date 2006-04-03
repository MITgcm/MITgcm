# Multithreaded + multi-processing makefile for:
#    Linux cfe1 2.6.5-7.202.7-sn2 #3 SMP Mon Jan 9 18:06:05 PST 2006 ia64 ia64 ia64 GNU/Linux
# This makefile was generated automatically on
#    Mon Feb 27 10:50:27 PST 2006
# by the command:
#    ../../../tools/genmake2  "-mods=../code" "-of=../../../tools/build_options/linux_ia64_ifort+mpi_altix_nas"
# executed by:
#    molod@cfe1:/u/molod/MITgcm/verification/APE6hr/build
#
# OPTFILE="../../../tools/build_options/linux_ia64_ifort+mpi_altix_nas"
# 
# BUILDDIR     : Directory where object files are written
# SOURCEDIRS   : Directories containing the source (.F) files
# INCLUDEDIRS  : Directories containing the header-source (.h) files
# EXEDIR       : Directory where executable that is generated is written
# EXECUTABLE   : Full path of executable binary
#
# CPP          : C-preprocessor command
# INCLUDES     : Directories searched for header files
# DEFINES      : Macro definitions for CPP
# MAKEDEPEND   : Dependency generator
# KPP          : Special preprocessor command (specific to platform)
# KFLAGS       : Flags for KPP
# FC           : Fortran compiler command
# FFLAGS       : Configuration/debugging options for FC
# FOPTIM       : Optimization options for FC
# LINK         : Command for link editor program
# LIBS         : Library flags /or/ additional optimization/debugging flags

ROOTDIR     = ../../..
BUILDDIR    = .  
SOURCEDIRS  =  ../code ../../../pkg/debug ../../../pkg/diagnostics ../../../pkg/exch2 ../../../pkg/fizhi ../../../pkg/generic_advdiff ../../../pkg/gridalt ../../../pkg/mdsio ../../../pkg/mnc ../../../pkg/mom_common ../../../pkg/mom_vecinv ../../../pkg/monitor ../../../pkg/rw ../../../pkg/shap_filt ../../../pkg/timeave ../../../pkg/atm_common ../../../eesupp/src ../../../model/src
INCLUDEDIRS = /opt/sgi/mpt/1.12.0.0/include ../code ../../../pkg/debug ../../../pkg/diagnostics ../../../pkg/exch2 ../../../pkg/fizhi ../../../pkg/generic_advdiff ../../../pkg/gridalt ../../../pkg/mdsio ../../../pkg/mnc ../../../pkg/mom_common ../../../pkg/mom_vecinv ../../../pkg/monitor ../../../pkg/rw ../../../pkg/shap_filt ../../../pkg/timeave ../../../pkg/atm_common ../../../eesupp/inc ../../../model/inc
EXEDIR      = .
EXECUTABLE  = $(EXEDIR)/mitgcmuv
TOOLSDIR    = ../../../tools

#eh3  new defines for the adjoint work
AUTODIFF    = ../../../pkg/autodiff
EXE_AD      = mitgcmuv_ad
EXE_FTL     = mitgcmuv_ftl
EXE_SVD     = mitgcmuv_svd

ENABLED_PACKAGES =  -DALLOW_DEBUG -DALLOW_DIAGNOSTICS -DALLOW_EXCH2 -DALLOW_FIZHI -DALLOW_GENERIC_ADVDIFF -DALLOW_GRIDALT -DALLOW_MDSIO -DALLOW_MNC -DALLOW_MOM_COMMON -DALLOW_MOM_VECINV -DALLOW_MONITOR -DALLOW_RW -DALLOW_SHAP_FILT -DALLOW_TIMEAVE -DALLOW_ATM_COMMON
DISABLED_PACKAGES =  -UALLOW_ADMTLM -UALLOW_AIM_COMPON_INTERF -UALLOW_AIM_OCN_COUPLER -UALLOW_AIM_V23 -UALLOW_AUTODIFF -UALLOW_BULK_FORCE -UALLOW_CAL -UALLOW_CD_CODE -UALLOW_CFC -UALLOW_CHRONOS -UALLOW_COMPON_COMMUNIC -UALLOW_COST -UALLOW_CTRL -UALLOW_DIC -UALLOW_EBM -UALLOW_ECCO -UALLOW_EMBED_FILES -UALLOW_EX3 -UALLOW_EXF -UALLOW_FLT -UALLOW_GCHEM -UALLOW_GGL90 -UALLOW_GMREDI -UALLOW_GRDCHK -UALLOW_KPP -UALLOW_LAND -UALLOW_MATRIX -UALLOW_MOM_FLUXFORM -UALLOW_MY82 -UALLOW_OBCS -UALLOW_OCN_COMPON_INTERF -UALLOW_OFFLINE -UALLOW_OPPS -UALLOW_PP81 -UALLOW_PTRACERS -UALLOW_RBCS -UALLOW_RUNCLOCK -UALLOW_SBO -UALLOW_SEAICE -UALLOW_SHELFICE -UALLOW_SPHERE -UALLOW_THSICE -UALLOW_ZONAL_FILT

# These files are created by Makefile
SPECIAL_FILES = PACKAGES_CONFIG.h AD_CONFIG.h FC_NAMEMANGLE.h BUILD_INFO.h
EMBEDDED_FILES = 
# Unix ln (link)
LN = ln -s
# C preprocessor
CPP = cat $< | $(TOOLSDIR)/set64bitConst.sh |  /lib/cpp  -traditional -P
# Dependency generator
MAKEDEPEND = makedepend
# Special preprocessor (KAP on DECs, FPP on Crays)
KPP = 
# Fortran compiler
FC = ifort
# Fortran compiler
F90C = 
# C compiler
CC = cc
# Link editor
LINK = ifort 

# Defines for CPP
DEFINES = -DALLOW_USE_MPI -DALWAYS_USE_MPI -DWORDLENGTH=4 -DHAVE_SYSTEM -DHAVE_FDATE -DHAVE_ETIME -DHAVE_CLOC -DHAVE_SETRLSTK -DHAVE_SIGREG -DHAVE_STAT 
# Includes for CPP
INCLUDES = -I/opt/sgi/mpt/1.12.0.0/include -I/u/edhill/pkg/netcdf-3.6.0/ifort/include
# Flags for KPP
KFLAGS1 = 
KFLAGS2 = 
# Optim./debug for FC
FFLAGS = -mp -132 -r8 -i4 -w95 -W0 -WB -convert big_endian -assume byterecl
FOPTIM = -O3 -align
# Optim./debug for FC
F90FLAGS = 
F90OPTIM = 
# Flags for CC
CFLAGS = 
# Files that should not be optimized
NOOPTFILES = 
NOOPTFLAGS = 
# Flags and libraries needed for linking
LIBS = -L/opt/sgi/mpt/1.12.0.0/lib -lmpi -L/u/edhill/pkg/netcdf-3.6.0/ifort/lib -lnetcdf
# Name of the Mekfile
MAKEFILE=Makefile

SRCFILES =  \
 do_fizhi.F \
 fizhi_clockstuff.F \
 fizhi_init_fixed.F \
 fizhi_init_veg.F \
 fizhi_init_vegsurftiles.F \
 fizhi_swrad.F \
 fizhi_turb.F \
 ini_salt.F \
 update_earth_exports.F \
 update_ocean_exports.F \
 chksum_tiled.F \
 debug_call.F \
 debug_cs_corner_uv.F \
 debug_enter.F \
 debug_fld_stats_rl.F \
 debug_fld_stats_rs.F \
 debug_leave.F \
 debug_msg.F \
 debug_stats_rl.F \
 debug_stats_rs.F \
 fill_in_corners_rl.F \
 write_fullarray_rl.F \
 diag_vegtile_fill.F \
 diagnostics_add2list.F \
 diagnostics_check.F \
 diagnostics_clear.F \
 diagnostics_fill.F \
 diagnostics_fill_field.F \
 diagnostics_fill_state.F \
 diagnostics_fract_fill.F \
 diagnostics_init_early.F \
 diagnostics_init_fixed.F \
 diagnostics_init_varia.F \
 diagnostics_interp_vert.F \
 diagnostics_is_on.F \
 diagnostics_main_init.F \
 diagnostics_out.F \
 diagnostics_read_pickup.F \
 diagnostics_readparms.F \
 diagnostics_scale_fill.F \
 diagnostics_set_levels.F \
 diagnostics_set_pointers.F \
 diagnostics_setdiag.F \
 diagnostics_setunits.F \
 diagnostics_summary.F \
 diagnostics_switch_onoff.F \
 diagnostics_utils.F \
 diagnostics_write.F \
 diagnostics_write_pickup.F \
 diagstats_ascii_out.F \
 diagstats_calc.F \
 diagstats_clear.F \
 diagstats_close_io.F \
 diagstats_fill.F \
 diagstats_global.F \
 diagstats_ini_io.F \
 diagstats_local.F \
 diagstats_mnc_out.F \
 diagstats_others_calc.F \
 diagstats_output.F \
 diagstats_set_pointers.F \
 diagstats_set_regions.F \
 diagstats_setdiag.F \
 prestopres.F \
 exch2_get_recv_bounds.F \
 exch2_get_send_bounds.F \
 exch2_r41_cube.F \
 exch2_r42_cube.F \
 exch2_r81_cube.F \
 exch2_r82_cube.F \
 exch2_recv_r41.F \
 exch2_recv_r42.F \
 exch2_recv_r81.F \
 exch2_recv_r82.F \
 exch2_recv_rl1.F \
 exch2_recv_rl2.F \
 exch2_recv_rs1.F \
 exch2_recv_rs2.F \
 exch2_rl1_cube.F \
 exch2_rl2_cube.F \
 exch2_rs1_cube.F \
 exch2_rs2_cube.F \
 exch2_s3d_r4.F \
 exch2_s3d_r8.F \
 exch2_s3d_rl.F \
 exch2_s3d_rs.F \
 exch2_send_r41.F \
 exch2_send_r42.F \
 exch2_send_r81.F \
 exch2_send_r82.F \
 exch2_send_rl1.F \
 exch2_send_rl2.F \
 exch2_send_rs1.F \
 exch2_send_rs2.F \
 exch2_uv_agrid_xy_r4.F \
 exch2_uv_agrid_xy_r8.F \
 exch2_uv_agrid_xy_rl.F \
 exch2_uv_agrid_xy_rs.F \
 exch2_uv_agrid_xyz_r4.F \
 exch2_uv_agrid_xyz_r8.F \
 exch2_uv_agrid_xyz_rl.F \
 exch2_uv_agrid_xyz_rs.F \
 exch2_uv_xy_r4.F \
 exch2_uv_xy_r8.F \
 exch2_uv_xy_rl.F \
 exch2_uv_xy_rs.F \
 exch2_uv_xyz_r4.F \
 exch2_uv_xyz_r8.F \
 exch2_uv_xyz_rl.F \
 exch2_uv_xyz_rs.F \
 exch2_xy_r4.F \
 exch2_xy_r8.F \
 exch2_xy_rl.F \
 exch2_xy_rs.F \
 exch2_xyz_r4.F \
 exch2_xyz_r8.F \
 exch2_xyz_rl.F \
 exch2_xyz_rs.F \
 exch2_z_xy_r4.F \
 exch2_z_xy_r8.F \
 exch2_z_xy_rl.F \
 exch2_z_xy_rs.F \
 w2_e2setup.F \
 w2_eeboot.F \
 w2_print_comm_sequence.F \
 AtoC.F \
 CtoA.F \
 fizhi_alarms.F \
 fizhi_diagalarms.F \
 fizhi_diagnostics_init.F \
 fizhi_driver.F \
 fizhi_fillnegs.F \
 fizhi_gwdrag.F \
 fizhi_init_chem.F \
 fizhi_init_vars.F \
 fizhi_lsm.F \
 fizhi_lwrad.F \
 fizhi_mnc_init.F \
 fizhi_moist.F \
 fizhi_mpistuff.F \
 fizhi_rayleigh.F \
 fizhi_read_pickup.F \
 fizhi_readparms.F \
 fizhi_readwrite_vegtiles.F \
 fizhi_step_diag.F \
 fizhi_tendency_apply.F \
 fizhi_update_time.F \
 fizhi_utils.F \
 fizhi_wrapper.F \
 fizhi_write_datetime.F \
 fizhi_write_pickup.F \
 fizhi_write_state.F \
 getpwhere.F \
 step_fizhi_corr.F \
 step_fizhi_fg.F \
 step_physics.F \
 update_chemistry_exports.F \
 gad_advection.F \
 gad_biharm_x.F \
 gad_biharm_y.F \
 gad_c2_adv_r.F \
 gad_c2_adv_x.F \
 gad_c2_adv_y.F \
 gad_c2_impl_r.F \
 gad_c4_adv_r.F \
 gad_c4_adv_x.F \
 gad_c4_adv_y.F \
 gad_calc_rhs.F \
 gad_del2.F \
 gad_diagnostics_init.F \
 gad_diff_r.F \
 gad_diff_x.F \
 gad_diff_y.F \
 gad_dst2u1_adv_r.F \
 gad_dst2u1_adv_x.F \
 gad_dst2u1_adv_y.F \
 gad_dst2u1_impl_r.F \
 gad_dst3_adv_r.F \
 gad_dst3_adv_x.F \
 gad_dst3_adv_y.F \
 gad_dst3fl_adv_r.F \
 gad_dst3fl_adv_x.F \
 gad_dst3fl_adv_y.F \
 gad_dst3fl_impl_r.F \
 gad_fluxlimit_adv_r.F \
 gad_fluxlimit_adv_x.F \
 gad_fluxlimit_adv_y.F \
 gad_fluxlimit_impl_r.F \
 gad_grad_x.F \
 gad_grad_y.F \
 gad_implicit_r.F \
 gad_init.F \
 gad_u3_adv_r.F \
 gad_u3_adv_x.F \
 gad_u3_adv_y.F \
 gad_u3c4_impl_r.F \
 dyn2phys.F \
 gridalt_diagnostics_init.F \
 gridalt_initialise.F \
 gridalt_update.F \
 make_phys_grid.F \
 phys2dyn.F \
 mdsio_gl.F \
 mdsio_gl_slice.F \
 mdsio_passtorl.F \
 mdsio_passtors.F \
 mdsio_read_rl_vec.F \
 mdsio_read_rs_vec.F \
 mdsio_readfield.F \
 mdsio_readfield_loc.F \
 mdsio_readtile.F \
 mdsio_readvector.F \
 mdsio_seg4torl.F \
 mdsio_seg4tors.F \
 mdsio_seg8torl.F \
 mdsio_seg8tors.F \
 mdsio_slice.F \
 mdsio_slice_loc.F \
 mdsio_write_rl_vec.F \
 mdsio_write_rs_vec.F \
 mdsio_writefield.F \
 mdsio_writefield_loc.F \
 mdsio_writefield_new.F \
 mdsio_writelocal.F \
 mdsio_writemeta.F \
 mdsio_writetile.F \
 mdsio_writevec_loc.F \
 mdsio_writevector.F \
 MNC_CW_READWRITE_I.F \
 MNC_CW_READWRITE_RL.F \
 MNC_CW_READWRITE_RS.F \
 mnc_cw_citer.F \
 mnc_cw_cvars.F \
 mnc_cw_init.F \
 mnc_cw_missingvals.F \
 mnc_cw_model_attr.F \
 mnc_cw_udim.F \
 mnc_cw_write_grid_info.F \
 mnc_cwrapper.F \
 mnc_dim.F \
 mnc_dump.F \
 mnc_file.F \
 mnc_grid.F \
 mnc_init.F \
 mnc_readparms.F \
 mnc_utils.F \
 mnc_var.F \
 mom_calc_absvort3.F \
 mom_calc_hdiv.F \
 mom_calc_hfacz.F \
 mom_calc_ke.F \
 mom_calc_relvort3.F \
 mom_calc_strain.F \
 mom_calc_tension.F \
 mom_calc_visc.F \
 mom_diagnostics_init.F \
 mom_hdissip.F \
 mom_init_fixed.F \
 mom_u_bottomdrag.F \
 mom_u_implicit_r.F \
 mom_u_rviscflux.F \
 mom_u_sidedrag.F \
 mom_v_bottomdrag.F \
 mom_v_implicit_r.F \
 mom_v_rviscflux.F \
 mom_v_sidedrag.F \
 mom_vecinv.F \
 mom_vi_coriolis.F \
 mom_vi_del2uv.F \
 mom_vi_hdissip.F \
 mom_vi_u_coriolis.F \
 mom_vi_u_coriolis_c4.F \
 mom_vi_u_grad_ke.F \
 mom_vi_u_vertshear.F \
 mom_vi_v_coriolis.F \
 mom_vi_v_coriolis_c4.F \
 mom_vi_v_grad_ke.F \
 mom_vi_v_vertshear.F \
 mon_advcfl.F \
 mon_advcflw.F \
 mon_advcflw2.F \
 mon_init.F \
 mon_ke.F \
 mon_out.F \
 mon_printstats_rl.F \
 mon_printstats_rs.F \
 mon_set_iounit.F \
 mon_set_pref.F \
 mon_solution.F \
 mon_stats_latbnd_rl.F \
 mon_stats_rl.F \
 mon_stats_rs.F \
 mon_surfcor.F \
 mon_vort3.F \
 mon_writestats_rl.F \
 monitor.F \
 get_write_global_fld.F \
 read_fld_xy_rl.F \
 read_fld_xy_rs.F \
 read_fld_xyz_rl.F \
 read_fld_xyz_rs.F \
 read_write_rec.F \
 set_write_global_fld.F \
 write_fld_xy_rl.F \
 write_fld_xy_rs.F \
 write_fld_xyz_rl.F \
 write_fld_xyz_rs.F \
 write_local_rl.F \
 write_local_rs.F \
 write_myfield.F \
 shap_filt_apply_ts.F \
 shap_filt_apply_uv.F \
 shap_filt_computvort.F \
 shap_filt_diagnostics_init.F \
 shap_filt_init_fixed.F \
 shap_filt_readparms.F \
 shap_filt_relvort3.F \
 shap_filt_tracer_s1.F \
 shap_filt_tracer_s2.F \
 shap_filt_tracer_s4.F \
 shap_filt_tracerold.F \
 shap_filt_u.F \
 shap_filt_uv_s1.F \
 shap_filt_uv_s2.F \
 shap_filt_uv_s2c.F \
 shap_filt_uv_s4.F \
 shap_filt_v.F \
 timeave_cumul_1k.F \
 timeave_cumul_1k1t.F \
 timeave_cumul_1t.F \
 timeave_cumul_1vfc.F \
 timeave_cumul_2v.F \
 timeave_cumul_2vfc.F \
 timeave_cumul_dif_1t.F \
 timeave_cumul_fc.F \
 timeave_cumulate.F \
 timeave_init_fixed.F \
 timeave_norma_1k.F \
 timeave_norma_2v.F \
 timeave_normaliz.F \
 timeave_reset.F \
 timeave_reset_1k.F \
 timeave_statv_write.F \
 timeave_statvars.F \
 timeave_surf_flux.F \
 atm_diagnostics_init.F \
 bar2.F \
 barrier.F \
 check_threads.F \
 comm_stats.F \
 dfile.F \
 diff_phase_multiple.F \
 different_multiple.F \
 eeboot.F \
 eeboot_minimal.F \
 eedata_example.F \
 eedie.F \
 eeintro_msg.F \
 eeset_parms.F \
 eewrite_eeenv.F \
 exch_cycle_ebl.F \
 exch_init.F \
 exch_jam.F \
 exch_r4.F \
 exch_r4_cube.F \
 exch_r4_recv_get_x.F \
 exch_r4_recv_get_y.F \
 exch_r4_send_put_x.F \
 exch_r4_send_put_y.F \
 exch_r8.F \
 exch_r8_cube.F \
 exch_r8_recv_get_x.F \
 exch_r8_recv_get_y.F \
 exch_r8_send_put_x.F \
 exch_r8_send_put_y.F \
 exch_rl.F \
 exch_rl_cube.F \
 exch_rl_recv_get_x.F \
 exch_rl_recv_get_y.F \
 exch_rl_send_put_x.F \
 exch_rl_send_put_y.F \
 exch_rs.F \
 exch_rs_cube.F \
 exch_rs_recv_get_x.F \
 exch_rs_recv_get_y.F \
 exch_rs_send_put_x.F \
 exch_rs_send_put_y.F \
 exch_s3d_r4.F \
 exch_s3d_r8.F \
 exch_s3d_rl.F \
 exch_s3d_rs.F \
 exch_uv_agrid_xy_r4.F \
 exch_uv_agrid_xy_r8.F \
 exch_uv_agrid_xy_rl.F \
 exch_uv_agrid_xy_rs.F \
 exch_uv_agrid_xyz_r4.F \
 exch_uv_agrid_xyz_r8.F \
 exch_uv_agrid_xyz_rl.F \
 exch_uv_agrid_xyz_rs.F \
 exch_uv_r4_cube.F \
 exch_uv_r8_cube.F \
 exch_uv_rl_cube.F \
 exch_uv_rs_cube.F \
 exch_uv_xy_r4.F \
 exch_uv_xy_r8.F \
 exch_uv_xy_rl.F \
 exch_uv_xy_rs.F \
 exch_uv_xyz_r4.F \
 exch_uv_xyz_r8.F \
 exch_uv_xyz_rl.F \
 exch_uv_xyz_rs.F \
 exch_xy_r4.F \
 exch_xy_r8.F \
 exch_xy_rl.F \
 exch_xy_rs.F \
 exch_xyz_r4.F \
 exch_xyz_r8.F \
 exch_xyz_rl.F \
 exch_xyz_rs.F \
 exch_z_r4_cube.F \
 exch_z_r8_cube.F \
 exch_z_rl_cube.F \
 exch_z_rs_cube.F \
 exch_z_xy_r4.F \
 exch_z_xy_r8.F \
 exch_z_xy_rl.F \
 exch_z_xy_rs.F \
 exch_z_xyz_r4.F \
 exch_z_xyz_r8.F \
 exch_z_xyz_rl.F \
 exch_z_xyz_rs.F \
 fill_cs_corner_tr_rl.F \
 fill_cs_corner_uv_rs.F \
 fool_the_compiler.F \
 gather_2d.F \
 gather_vector.F \
 global_max.F \
 global_sum.F \
 global_vec_sum.F \
 gsum.F \
 gsum_jam.F \
 ini_communication_patterns.F \
 ini_jam.F \
 ini_procs.F \
 ini_threading_environment.F \
 lef_zero.F \
 main.F \
 mds_byteswapi4.F \
 mds_byteswapr4.F \
 mds_byteswapr8.F \
 mds_reclen.F \
 mdsfindunit.F \
 memsync.F \
 nml_filter.F \
 nml_set_terminator.F \
 print.F \
 scatter_2d.F \
 scatter_vector.F \
 timers.F \
 utils.F \
 adams_bashforth2.F \
 adams_bashforth3.F \
 askparam.F \
 calc_3d_diffusivity.F \
 calc_buoyancy.F \
 calc_common_factors.F \
 calc_diffusivity.F \
 calc_div_ghat.F \
 calc_grad_phi_hyd.F \
 calc_grad_phi_surf.F \
 calc_gs.F \
 calc_gt.F \
 calc_gw.F \
 calc_ivdc.F \
 calc_phi_hyd.F \
 calc_r_star.F \
 calc_surf_dr.F \
 calc_viscosity.F \
 cg2d.F \
 cg3d.F \
 checkpoint.F \
 config_check.F \
 config_summary.F \
 convect.F \
 convective_adjustment.F \
 convective_adjustment_ini.F \
 convective_weights.F \
 convectively_mixtracer.F \
 correction_step.F \
 cycle_tracer.F \
 diags_oceanic_surf_flux.F \
 diags_phi_hyd.F \
 diags_phi_rlow.F \
 diags_rho.F \
 do_atmospheric_phys.F \
 do_fields_blocking_exchanges.F \
 do_gterm_blocking_exchanges.F \
 do_oceanic_phys.F \
 do_stagger_fields_exchanges.F \
 do_statevars_diags.F \
 do_statevars_tave.F \
 do_the_model_io.F \
 dynamics.F \
 external_fields_load.F \
 external_forcing.F \
 external_forcing_surf.F \
 find_alpha.F \
 find_rho.F \
 forward_step.F \
 freesurf_rescale_g.F \
 freeze.F \
 freeze_surface.F \
 getcon.F \
 grad_sigma.F \
 impldiff.F \
 ini_cartesian_grid.F \
 ini_cg2d.F \
 ini_cg3d.F \
 ini_cori.F \
 ini_curvilinear_grid.F \
 ini_cylinder_grid.F \
 ini_depths.F \
 ini_dynvars.F \
 ini_eos.F \
 ini_fields.F \
 ini_forcing.F \
 ini_grid.F \
 ini_linear_phisurf.F \
 ini_masks_etc.F \
 ini_mixing.F \
 ini_mnc_vars.F \
 ini_model_io.F \
 ini_nh_vars.F \
 ini_p_ground.F \
 ini_parms.F \
 ini_phiref.F \
 ini_pnh.F \
 ini_pressure.F \
 ini_psurf.F \
 ini_r_star.F \
 ini_spherical_polar_grid.F \
 ini_surf_dr.F \
 ini_theta.F \
 ini_vel.F \
 ini_vertical_grid.F \
 initialise_fixed.F \
 initialise_varia.F \
 integr_continuity.F \
 integrate_for_w.F \
 load_grid_spacing.F \
 modeldata_example.F \
 momentum_correction_step.F \
 open_copy_data_file.F \
 packages_boot.F \
 packages_check.F \
 packages_check_error.F \
 packages_init_fixed.F \
 packages_init_variables.F \
 packages_readparms.F \
 packages_write_pickup.F \
 plot_field.F \
 port_rand.F \
 pressure_for_eos.F \
 quasihydrostaticterms.F \
 read_write.F \
 remove_mean.F \
 set_defaults.F \
 solve_for_pressure.F \
 solve_pentadiagonal.F \
 solve_tridiagonal.F \
 state_summary.F \
 swfrac.F \
 taueddy_external_forcing.F \
 taueddy_init_varia.F \
 the_correction_step.F \
 the_main_loop.F \
 the_model_main.F \
 thermodynamics.F \
 timestep.F \
 timestep_tracer.F \
 timestep_wvel.F \
 tracers_correction_step.F \
 tracers_iigw_correction.F \
 update_cg2d.F \
 update_etah.F \
 update_r_star.F \
 update_surf_dr.F \
 write_grid.F \
 write_state.F
CSRCFILES =  \
 mnc_create_dir.c \
 mnc_cw_fsize.c \
 gsl_ieee_env.c \
 ptwrapper.c \
 setrlstk.c \
 sigreg.c \
 tim.c \
 timer_stats.c
F90SRCFILES = 
HEADERFILES =  \
 CPP_OPTIONS.h \
 DIAGNOSTICS_SIZE.h \
 SIZE.h \
 fizhi_SIZE.h \
 DEBUG_OPTIONS.h \
 DIAGNOSTICS.h \
 DIAGSTATS_REGIONS.h \
 DIAG_OPTIONS.h \
 W2_EXCH2_PARAMS.h \
 W2_EXCH2_TOPOLOGY.h \
 W2_OPTIONS.h \
 FIZHI_OPTIONS.h \
 cah-dat.h \
 cai-dat.h \
 chronos.h \
 co2-tran3.h \
 fizhi_SHP.h \
 fizhi_chemistry_coms.h \
 fizhi_coms.h \
 fizhi_earth_coms.h \
 fizhi_io_comms.h \
 fizhi_land_SIZE.h \
 fizhi_land_coms.h \
 fizhi_ocean_coms.h \
 h2o-tran3.h \
 o3-tran3.h \
 sibber.h \
 snwmid.h \
 GAD.h \
 GAD_FLUX_LIMITER.h \
 GAD_OPTIONS.h \
 GRIDALT_OPTIONS.h \
 gridalt_mapping.h \
 MDSIO_OPTIONS.h \
 MDSIO_SCPU.h \
 MNC_BUFF.h \
 MNC_OPTIONS.h \
 MNC_PARAMS.h \
 MNC_SIZE.h \
 mnc_common.h \
 mnc_id_header.h \
 MOM_COMMON_OPTIONS.h \
 MOM_VECINV_OPTIONS.h \
 MONITOR.h \
 MONITOR_OPTIONS.h \
 RW_OPTIONS.h \
 SHAP_FILT.h \
 SHAP_FILT_OPTIONS.h \
 TIMEAVE_OPTIONS.h \
 TIMEAVE_STATV.h \
 ATM_COMMON_OPTIONS.h \
 mpi++.h \
 mpi.h \
 mpi_ext.h \
 mpi_extf.h \
 mpi_gsm.h \
 mpi_gsmf.h \
 mpif.h \
 mpif_parameters.h \
 mpio.h \
 mpiof.h \
 BAR2.h \
 BARRIER.h \
 CPP_EEMACROS.h \
 CPP_EEOPTIONS.h \
 DFILE.h \
 EEIO.h \
 EEPARAMS.h \
 EESUPPORT.h \
 EXCH.h \
 EXCH_JAM.h \
 GLOBAL_MAX.h \
 GLOBAL_SUM.h \
 JAM_INFO.h \
 MAIN_PDIRECTIVES1.h \
 MAIN_PDIRECTIVES2.h \
 MPI_INFO.h \
 SIGREG.h \
 CG2D.h \
 CG3D.h \
 DXC_MACROS.h \
 DXF_MACROS.h \
 DXG_MACROS.h \
 DXV_MACROS.h \
 DYC_MACROS.h \
 DYF_MACROS.h \
 DYG_MACROS.h \
 DYNVARS.h \
 DYU_MACROS.h \
 EOS.h \
 FCORI_MACROS.h \
 FFIELDS.h \
 GRID.h \
 GRID_MACROS.h \
 HFACC_MACROS.h \
 HFACS_MACROS.h \
 HFACW_MACROS.h \
 MASKS_MACROS.h \
 MASKW_MACROS.h \
 NH_VARS.h \
 PARAMS.h \
 PARAMS_MACROS.h \
 RAS_MACROS.h \
 RAW_MACROS.h \
 RA_MACROS.h \
 RECIP_DXC_MACROS.h \
 RECIP_DXF_MACROS.h \
 RECIP_DXG_MACROS.h \
 RECIP_DXV_MACROS.h \
 RECIP_DYC_MACROS.h \
 RECIP_DYF_MACROS.h \
 RECIP_DYG_MACROS.h \
 RECIP_DYU_MACROS.h \
 RECIP_HFACC_MACROS.h \
 RECIP_HFACS_MACROS.h \
 RECIP_HFACW_MACROS.h \
 SOLVE_FOR_PRESSURE.h \
 SOLVE_FOR_PRESSURE3D.h \
 SURFACE.h \
 TANPHIATU_MACROS.h \
 TANPHIATV_MACROS.h \
 THE_MODEL_COMMON_BLOCKS.h \
 XC_MACROS.h \
 YC_MACROS.h
AD_FLOW_FILES =  \
 debug_ad.flow \
 diagnostics_ad.flow \
 mdsio_ad.flow \
 mnc_ad.flow \
 monitor_ad.flow \
 eesupp.flow

F77FILES =  $(SRCFILES:.F=.f)
F90FILES =  $(F90SRCFILES:.F90=.f90)
OBJFILES =  $(SRCFILES:.F=.o) $(CSRCFILES:.c=.o) $(F90SRCFILES:.F90=.o)

.SUFFIXES:
.SUFFIXES: .o .f .p .F .c .f90 .F90

all: $(EXECUTABLE)
$(EXECUTABLE): $(SPECIAL_FILES) $(SRCFILES) $(CSRCFILES) $(HEADERFILES) $(F90SRCFILES) $(OBJFILES) $(EMBEDDED_FILES)
	@echo Creating $@ ...
	$(LINK) -o $@ $(FFLAGS) $(FOPTIM) $(OBJFILES) $(LIBS)
depend:
	@make links
	$(MAKEDEPEND) -o .f $(DEFINES) $(INCLUDES) $(SRCFILES)
	$(TOOLSDIR)/f90mkdepend >> $(MAKEFILE)
	-rm -f makedepend.out

lib: libmitgcmuv.a

libmitgcmuv.a: $(OBJFILES)
	ar rcv libmitgcmuv.a $(OBJFILES)

links: $(SRCFILES) $(CSRCFILES) $(HEADERFILES) $(F90SRCFILES) $(SPECIAL_FILES)

small_f: $(F77FILES) $(F90FILES)

output.txt: $(EXECUTABLE)
	@printf 'running ... '
	@$(EXECUTABLE) > $@

clean:
	-rm -rf *.o *.f *.p *.f90 *.mod  work.{pc,pcl} *.template
Clean:
	@make clean
	@make cleanlinks
	-rm -f $(SPECIAL_FILES)
	-rm -f genmake_state genmake_*optfile genmake_warnings make.log run.log *.bak
CLEAN:
	@make Clean
	-find $(EXEDIR) -name "*.meta" -exec rm {} \;
	-find $(EXEDIR) -name "*.data" -exec rm {} \;
	-find $(EXEDIR) -name "fort.*" -exec rm {} \;
	-rm -f $(EXECUTABLE) output.txt STD*

#eh3 Makefile: makefile
makefile:
	$(TOOLSDIR)/genmake2  "-mods=../code" "-of=../../../tools/build_options/linux_ia64_ifort+mpi_altix_nas"
cleanlinks:
	-find . -type l -exec rm {} \;

# Special targets (SPECIAL_FILES) which are create by make
PACKAGES_CONFIG.h:
	@echo Creating $@ ...
	@$(TOOLSDIR)/convert_cpp_cmd2defines "Warning - this file is automatically generated - do NOT edit" -bPACKAGES_CONFIG_H "Disabled packages:" $(DISABLED_PACKAGES) " " "Enabled packages:" $(ENABLED_PACKAGES) > $@
AD_CONFIG.h:
	@echo Creating $@ ...
	@$(TOOLSDIR)/convert_cpp_cmd2defines "Warning - this file is automatically generated - do NOT edit" -bAD_CONFIG_H -UALLOW_ADJOINT_RUN -UALLOW_TANGENTLINEAR_RUN -UALLOW_ECCO_OPTIMIZATION > $@
FC_NAMEMANGLE.h:
	@echo Creating $@ ...
	echo "#define FC_NAMEMANGLE(X)  X ## _" > $@

BUILD_INFO.h:
	@echo Creating $@ ...
	-echo "#define THISVER 'checkpoint58a_post'" > $@
	-echo "#define THISUSER 'molod'" >> $@
	-echo "#define THISDATE 'Mon Feb 27 10:50:27 PST 2006'" >> $@
	-echo "#define THISHOST 'cfe1'" >> $@

# The normal chain of rules is (  .F - .f - .o  )

## This nullifies any default implicit rules concerning these two file types:
## %.o : %.F

.F.f:
	$(CPP) $(DEFINES) $(INCLUDES) > $@
.f.o:
	$(FC) $(FFLAGS) $(FOPTIM) -c $<
.F90.f90:
	$(CPP) $(DEFINES) $(INCLUDES) > $@
.f90.o:
	$(F90C) $(F90FLAGS) $(F90OPTIM) -c $<
.c.o:
	$(CC) $(CFLAGS) $(DEFINES) $(INCLUDES) -c $<

# Special exceptions that use the ( .F - .p - .f - .o ) rule-chain
.F.p:
	$(CPP) $(DEFINES) $(INCLUDES) > $@
.p.f:
	$(KPP) $(KFLAGS1)$@ $(KFLAGS2) $<

#=========================================
#===  Automatic Differentiation Rules  ===

TAMC           = tamc
TAF            = taf

TAF_EXTRA      = 
TAMC_EXTRA     = 

AD_TAMC_FLAGS        = -reverse -admark ad -i4 -r4 -l tamc_ad.log -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_diffkr_dummy xx_kapgm_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'
AD_TAF_FLAGS         = -reverse -admark ad -i4 -r4 -l taf_ad.log -flow taf_ad_flow.log -nonew_arg -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_diffkr_dummy xx_kapgm_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'
FTL_TAMC_FLAGS       = -forward -ftlmark g_ -i4 -r4 -l tamc_ftl.log -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_diffkr_dummy xx_kapgm_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'
FTL_TAF_FLAGS        = -forward -ftlmark g_ -i4 -r4 -l taf_ftl.log -flow taf_ftl_flow.log -nonew_arg -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_diffkr_dummy xx_kapgm_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'
SVD_TAMC_FLAGS       = -reverse -forward -pure -i4 -r4 -l tamc_svd.log -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_diffkr_dummy xx_kapgm_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'
SVD_TAF_FLAGS        = -reverse -forward -pure -i4 -r4 -l taf_svd.log -flow taf_flow.log -nonew_arg -toplevel 'the_main_loop' -input 'xx_theta_dummy xx_salt_dummy xx_tr1_dummy xx_hflux_dummy xx_sflux_dummy xx_tauu_dummy xx_tauv_dummy xx_atemp_dummy xx_aqh_dummy xx_precip_dummy xx_swflux_dummy xx_swdown_dummy xx_uwind_dummy xx_vwind_dummy xx_obcsn_dummy xx_obcss_dummy xx_obcsw_dummy xx_obcse_dummy xx_sst_dummy xx_sss_dummy xx_diffkr_dummy xx_kapgm_dummy xx_efluxy_dummy xx_efluxp_dummy xx_edtaux_dummy xx_edtauy_dummy xx_uvel_dummy xx_vvel_dummy xx_etan_dummy xx_relaxsst_dummy xx_relaxsss_dummy' -output 'fc'

AD_FILES =  \
 gad_advection.f \
 gad_biharm_x.f \
 gad_biharm_y.f \
 gad_c2_adv_r.f \
 gad_c2_adv_x.f \
 gad_c2_adv_y.f \
 gad_c4_adv_r.f \
 gad_c4_adv_x.f \
 gad_c4_adv_y.f \
 gad_calc_rhs.f \
 gad_del2.f \
 gad_diff_r.f \
 gad_diff_x.f \
 gad_diff_y.f \
 gad_dst2u1_adv_r.f \
 gad_dst2u1_adv_x.f \
 gad_dst2u1_adv_y.f \
 gad_dst3_adv_x.f \
 gad_dst3_adv_y.f \
 gad_dst3_adv_r.f \
 gad_dst3fl_adv_x.f \
 gad_dst3fl_adv_y.f \
 gad_dst3fl_adv_r.f \
 gad_fluxlimit_adv_r.f \
 gad_fluxlimit_adv_x.f \
 gad_fluxlimit_adv_y.f \
 gad_grad_x.f \
 gad_grad_y.f \
 gad_u3_adv_r.f \
 gad_u3_adv_x.f \
 gad_u3_adv_y.f \
 mom_calc_absvort3.f \
 mom_calc_hdiv.f \
 mom_calc_hfacz.f \
 mom_calc_ke.f \
 mom_calc_relvort3.f \
 mom_calc_strain.f \
 mom_calc_tension.f \
 mom_calc_visc.f \
 mom_hdissip.f \
 mom_u_bottomdrag.f \
 mom_u_rviscflux.f \
 mom_u_sidedrag.f \
 mom_v_bottomdrag.f \
 mom_v_rviscflux.f \
 mom_v_sidedrag.f \
 mom_vecinv.f \
 mom_vi_coriolis.f \
 mom_vi_del2uv.f \
 mom_vi_hdissip.f \
 mom_vi_u_coriolis.f \
 mom_vi_u_coriolis_c4.f \
 mom_vi_u_grad_ke.f \
 mom_vi_u_vertshear.f \
 mom_vi_v_coriolis.f \
 mom_vi_v_coriolis_c4.f \
 mom_vi_v_grad_ke.f \
 mom_vi_v_vertshear.f \
 shap_filt_apply_ts.f \
 shap_filt_apply_uv.f \
 shap_filt_computvort.f \
 shap_filt_relvort3.f \
 shap_filt_tracer_s1.f \
 shap_filt_tracer_s2.f \
 shap_filt_tracer_s4.f \
 shap_filt_uv_s1.f \
 shap_filt_uv_s2.f \
 shap_filt_uv_s2c.f \
 shap_filt_uv_s4.f \
 timeave_cumul_1k1t.f \
 timeave_cumul_1k.f \
 timeave_cumul_1t.f \
 timeave_cumul_1vfc.f \
 timeave_cumul_2v.f \
 timeave_cumul_2vfc.f \
 timeave_cumulate.f \
 timeave_cumul_dif_1t.f \
 timeave_cumul_fc.f \
 timeave_norma_1k.f \
 timeave_norma_2v.f \
 timeave_normaliz.f \
 timeave_reset_1k.f \
 timeave_reset.f \
 timeave_statvars.f \
 timeave_statv_write.f \
 timeave_surf_flux.f \
 different_multiple.f \
 lef_zero.f \
 adams_bashforth2.f \
 calc_3d_diffusivity.f \
 calc_buoyancy.f \
 calc_common_factors.f \
 calc_diffusivity.f \
 calc_div_ghat.f \
 calc_grad_phi_hyd.f \
 calc_grad_phi_surf.f \
 calc_gs.f \
 calc_gt.f \
 calc_ivdc.f \
 calc_phi_hyd.f \
 calc_surf_dr.f \
 calc_viscosity.f \
 convect.f \
 convective_adjustment.f \
 convective_adjustment_ini.f \
 convective_weights.f \
 convectively_mixtracer.f \
 correction_step.f \
 cycle_tracer.f \
 diags_phi_hyd.f \
 diags_phi_rlow.f \
 do_atmospheric_phys.f \
 do_fields_blocking_exchanges.f \
 do_oceanic_phys.f \
 do_stagger_fields_exchanges.f \
 dynamics.f \
 external_fields_load.f \
 external_forcing.f \
 external_forcing_surf.f \
 find_alpha.f \
 find_rho.f \
 forward_step.f \
 freesurf_rescale_g.f \
 freeze.f \
 freeze_surface.f \
 grad_sigma.f \
 impldiff.f \
 ini_autodiff.f \
 ini_surf_dr.f \
 ini_cartesian_grid.f \
 ini_cg2d.f \
 ini_cori.f \
 ini_curvilinear_grid.f \
 ini_cylinder_grid.f \
 ini_depths.f \
 ini_fields.f \
 ini_forcing.f \
 ini_grid.f \
 ini_linear_phisurf.f \
 ini_masks_etc.f \
 ini_mixing.f \
 ini_p_ground.f \
 ini_pressure.f \
 ini_pnh.f \
 ini_psurf.f \
 ini_r_star.f \
 ini_salt.f \
 ini_spherical_polar_grid.f \
 ini_theta.f \
 ini_dynvars.f \
 ini_vel.f \
 ini_vertical_grid.f \
 initialise_varia.f \
 integrate_for_w.f \
 integr_continuity.f \
 load_grid_spacing.f \
 modeldata_example.f \
 momentum_correction_step.f \
 packages_init_variables.f \
 plot_field.f \
 pressure_for_eos.f \
 quasihydrostaticterms.f \
 solve_for_pressure.f \
 state_summary.f \
 taueddy_external_forcing.f \
 taueddy_init_varia.f \
 timestep.f \
 the_correction_step.f \
 the_main_loop.f \
 thermodynamics.f \
 timestep_tracer.f \
 tracers_correction_step.f \
 update_etah.f \
 update_surf_dr.f \
 update_cg2d.f \
 swfrac.f

# ... AD ...
adall: ad_taf
adtaf: ad_taf_output.f
adtamc: ad_tamc_output.f

ad_input_code.f: $(AD_FILES) $(HEADERFILES)
	@$(TOOLSDIR)/convert_cpp_cmd2defines "Warning - this file is automatically generated - do NOT edit" -DALLOW_ADJOINT_RUN -UALLOW_TANGENTLINEAR_RUN -UALLOW_ECCO_OPTIMIZATION > ad_config.template
	cmp ad_config.template AD_CONFIG.h || cat ad_config.template > AD_CONFIG.h
	-rm -f ad_config.template
	@make $(F77FILES)
	@make $(AD_FLOW_FILES)
	cat $(AD_FLOW_FILES) $(AD_FILES) > ad_input_code.f

ad_taf_output.f: ad_input_code.f
	$(TAF) $(AD_TAF_FLAGS) $(TAF_EXTRA) ad_input_code.f
	cat ad_input_code_ad.f | sed -f $(TOOLSDIR)/adjoint_sed > ad_taf_output.f

adtafonly:
	$(TAF) $(AD_TAF_FLAGS) $(TAF_EXTRA) ad_input_code.f
	cat ad_input_code_ad.f | sed -f $(TOOLSDIR)/adjoint_sed > ad_taf_output.f

ad_taf: ad_taf_output.o $(OBJFILES)
	$(LINK) -o mitgcmuv_ad $(FFLAGS) $(FOPTIM) $(OBJFILES) ad_taf_output.o $(LIBS) 

ad_tamc_output.f: ad_input_code.f
	$(TAMC) $(AD_TAMC_FLAGS) $(TAMC_EXTRA) ad_input_code.f
	cat ad_input_code_ad.f | sed -f $(TOOLSDIR)/adjoint_sed > ad_tamc_output.f

ad_tamc: ad_tamc_output.o $(OBJFILES)
	$(LINK) -o mitgcmuv_ad $(FFLAGS) $(FOPTIM) $(OBJFILES) ad_tamc_output.o $(LIBS) 

adonlyfwd:
	patch < $(TOOLSDIR)/ad_taf_output.f.onlyfwd.diff
 
adtrick:
	patch < $(TOOLSDIR)/ad_taf_output.f.adtrick.diff

# ... FTL ...
ftlall: ftl_taf
ftltaf: ftl_taf_output.f
ftltamc: ftl_tamc_output.f

ftl_input_code.f: $(AD_FILES) $(HEADERFILES)
	@$(TOOLSDIR)/convert_cpp_cmd2defines "Warning - this file is automatically generated - do NOT edit" -UALLOW_ADJOINT_RUN -DALLOW_TANGENTLINEAR_RUN -UALLOW_ECCO_OPTIMIZATION > ftl_config.template
	cmp ftl_config.template AD_CONFIG.h || cat ftl_config.template > AD_CONFIG.h
	-rm -f ftl_config.template
	@make $(F77FILES)
	@make $(AD_FLOW_FILES)
	cat $(AD_FLOW_FILES) $(AD_FILES) > ftl_input_code.f

ftl_taf_output.f: ftl_input_code.f
	$(TAF) $(FTL_TAF_FLAGS) $(TAF_EXTRA) ftl_input_code.f
	cat ftl_input_code_ftl.f | sed -f $(TOOLSDIR)/adjoint_sed > ftl_taf_output.f

ftltafonly:
	$(TAF) $(FTL_TAF_FLAGS) $(TAF_EXTRA) ftl_input_code.f
	cat ftl_input_code_ftl.f | sed -f $(TOOLSDIR)/adjoint_sed > ftl_taf_output.f

ftl_taf: ftl_taf_output.o $(OBJFILES)
	$(LINK) -o mitgcmuv_ftl $(FFLAGS) $(FOPTIM) $(OBJFILES) ftl_taf_output.o $(LIBS) 

ftl_tamc_output.f: ftl_input_code.f
	$(TAMC) $(FTL_TAMC_FLAGS) $(TAMC_EXTRA) ftl_input_code.f
	cat ftl_input_code_ftl.f | sed -f $(TOOLSDIR)/adjoint_sed > ftl_tamc_output.f

ftl_tamc: ftl_tamc_output.o $(OBJFILES)
	$(LINK) -o mitgcmuv_ftl $(FFLAGS) $(FOPTIM) $(OBJFILES) ftl_tamc_output.o $(LIBS) 


# ... SVD ...
svdtaf: ad_taf_output.f ftl_taf_output.f
	@echo "--->>> Only ran TAF to generate SVD code!    <<<---"
	@echo "--->>> Do make svdall afterwards to compile. <<<---"
svdall: svd_touch svd_taf

svd_taf: $(OBJFILES)
	$(LINK) -o mitgcmuv_svd $(FFLAGS) $(FOPTIM) $(OBJFILES) ad_taf_output.o ftl_taf_output.o $(LIBS)

	@echo "--->>> Only COMPILE svd code! <<<---"
	@echo "--->>> Assumes you previously <<<---"
	@echo "--->>> did make svdtaf        <<<---"

svd_touch:
	@echo "--->>> Only COMPILE svd code! <<<---"
	@echo "--->>> Assumes you previously <<<---"
	@echo "--->>> did make svdtaf        <<<---"
	touch ad_taf_output.f ftl_taf_output.f
	$(FC) $(FFLAGS) $(FOPTIM) -c ad_taf_output.f
	$(FC) $(FFLAGS) $(FOPTIM) -c ftl_taf_output.f
	@$(TOOLSDIR)/convert_cpp_cmd2defines "Warning - this file is automatically generated - do NOT edit" -UALLOW_ADJOINT_RUN -DALLOW_TANGENTLINEAR_RUN -UALLOW_ECCO_OPTIMIZATION > ftl_config.template
	cmp ftl_config.template AD_CONFIG.h || cat ftl_config.template > AD_CONFIG.h
	-rm -f ftl_config.template

#=========================================

# This section creates symbolic links


#  These files are linked from ../code
 CPP_OPTIONS.h DIAGNOSTICS_SIZE.h SIZE.h do_fizhi.F fizhi_SIZE.h fizhi_clockstuff.F fizhi_init_fixed.F fizhi_init_veg.F fizhi_init_vegsurftiles.F fizhi_swrad.F fizhi_turb.F ini_salt.F update_earth_exports.F update_ocean_exports.F :
	$(LN) ../code/$@ $@

#  These files are linked from ../../../pkg/debug
 DEBUG_OPTIONS.h chksum_tiled.F debug_call.F debug_cs_corner_uv.F debug_enter.F debug_fld_stats_rl.F debug_fld_stats_rs.F debug_leave.F debug_msg.F debug_stats_rl.F debug_stats_rs.F fill_in_corners_rl.F write_fullarray_rl.F debug_ad.flow :
	$(LN) ../../../pkg/debug/$@ $@

#  These files are linked from ../../../pkg/diagnostics
 DIAGNOSTICS.h DIAGSTATS_REGIONS.h DIAG_OPTIONS.h diag_vegtile_fill.F diagnostics_add2list.F diagnostics_check.F diagnostics_clear.F diagnostics_fill.F diagnostics_fill_field.F diagnostics_fill_state.F diagnostics_fract_fill.F diagnostics_init_early.F diagnostics_init_fixed.F diagnostics_init_varia.F diagnostics_interp_vert.F diagnostics_is_on.F diagnostics_main_init.F diagnostics_out.F diagnostics_read_pickup.F diagnostics_readparms.F diagnostics_scale_fill.F diagnostics_set_levels.F diagnostics_set_pointers.F diagnostics_setdiag.F diagnostics_setunits.F diagnostics_summary.F diagnostics_switch_onoff.F diagnostics_utils.F diagnostics_write.F diagnostics_write_pickup.F diagstats_ascii_out.F diagstats_calc.F diagstats_clear.F diagstats_close_io.F diagstats_fill.F diagstats_global.F diagstats_ini_io.F diagstats_local.F diagstats_mnc_out.F diagstats_others_calc.F diagstats_output.F diagstats_set_pointers.F diagstats_set_regions.F diagstats_setdiag.F prestopres.F diagnostics_ad.flow :
	$(LN) ../../../pkg/diagnostics/$@ $@

#  These files are linked from ../../../pkg/exch2
 W2_EXCH2_PARAMS.h W2_EXCH2_TOPOLOGY.h W2_OPTIONS.h exch2_get_recv_bounds.F exch2_get_send_bounds.F exch2_r41_cube.F exch2_r42_cube.F exch2_r81_cube.F exch2_r82_cube.F exch2_recv_r41.F exch2_recv_r42.F exch2_recv_r81.F exch2_recv_r82.F exch2_recv_rl1.F exch2_recv_rl2.F exch2_recv_rs1.F exch2_recv_rs2.F exch2_rl1_cube.F exch2_rl2_cube.F exch2_rs1_cube.F exch2_rs2_cube.F exch2_s3d_r4.F exch2_s3d_r8.F exch2_s3d_rl.F exch2_s3d_rs.F exch2_send_r41.F exch2_send_r42.F exch2_send_r81.F exch2_send_r82.F exch2_send_rl1.F exch2_send_rl2.F exch2_send_rs1.F exch2_send_rs2.F exch2_uv_agrid_xy_r4.F exch2_uv_agrid_xy_r8.F exch2_uv_agrid_xy_rl.F exch2_uv_agrid_xy_rs.F exch2_uv_agrid_xyz_r4.F exch2_uv_agrid_xyz_r8.F exch2_uv_agrid_xyz_rl.F exch2_uv_agrid_xyz_rs.F exch2_uv_xy_r4.F exch2_uv_xy_r8.F exch2_uv_xy_rl.F exch2_uv_xy_rs.F exch2_uv_xyz_r4.F exch2_uv_xyz_r8.F exch2_uv_xyz_rl.F exch2_uv_xyz_rs.F exch2_xy_r4.F exch2_xy_r8.F exch2_xy_rl.F exch2_xy_rs.F exch2_xyz_r4.F exch2_xyz_r8.F exch2_xyz_rl.F exch2_xyz_rs.F exch2_z_xy_r4.F exch2_z_xy_r8.F exch2_z_xy_rl.F exch2_z_xy_rs.F w2_e2setup.F w2_eeboot.F w2_print_comm_sequence.F :
	$(LN) ../../../pkg/exch2/$@ $@

#  These files are linked from ../../../pkg/fizhi
 AtoC.F CtoA.F FIZHI_OPTIONS.h cah-dat.h cai-dat.h chronos.h co2-tran3.h fizhi_SHP.h fizhi_alarms.F fizhi_chemistry_coms.h fizhi_coms.h fizhi_diagalarms.F fizhi_diagnostics_init.F fizhi_driver.F fizhi_earth_coms.h fizhi_fillnegs.F fizhi_gwdrag.F fizhi_init_chem.F fizhi_init_vars.F fizhi_io_comms.h fizhi_land_SIZE.h fizhi_land_coms.h fizhi_lsm.F fizhi_lwrad.F fizhi_mnc_init.F fizhi_moist.F fizhi_mpistuff.F fizhi_ocean_coms.h fizhi_rayleigh.F fizhi_read_pickup.F fizhi_readparms.F fizhi_readwrite_vegtiles.F fizhi_step_diag.F fizhi_tendency_apply.F fizhi_update_time.F fizhi_utils.F fizhi_wrapper.F fizhi_write_datetime.F fizhi_write_pickup.F fizhi_write_state.F getpwhere.F h2o-tran3.h o3-tran3.h sibber.h snwmid.h step_fizhi_corr.F step_fizhi_fg.F step_physics.F update_chemistry_exports.F :
	$(LN) ../../../pkg/fizhi/$@ $@

#  These files are linked from ../../../pkg/generic_advdiff
 GAD.h GAD_FLUX_LIMITER.h GAD_OPTIONS.h gad_advection.F gad_biharm_x.F gad_biharm_y.F gad_c2_adv_r.F gad_c2_adv_x.F gad_c2_adv_y.F gad_c2_impl_r.F gad_c4_adv_r.F gad_c4_adv_x.F gad_c4_adv_y.F gad_calc_rhs.F gad_del2.F gad_diagnostics_init.F gad_diff_r.F gad_diff_x.F gad_diff_y.F gad_dst2u1_adv_r.F gad_dst2u1_adv_x.F gad_dst2u1_adv_y.F gad_dst2u1_impl_r.F gad_dst3_adv_r.F gad_dst3_adv_x.F gad_dst3_adv_y.F gad_dst3fl_adv_r.F gad_dst3fl_adv_x.F gad_dst3fl_adv_y.F gad_dst3fl_impl_r.F gad_fluxlimit_adv_r.F gad_fluxlimit_adv_x.F gad_fluxlimit_adv_y.F gad_fluxlimit_impl_r.F gad_grad_x.F gad_grad_y.F gad_implicit_r.F gad_init.F gad_u3_adv_r.F gad_u3_adv_x.F gad_u3_adv_y.F gad_u3c4_impl_r.F :
	$(LN) ../../../pkg/generic_advdiff/$@ $@

#  These files are linked from ../../../pkg/gridalt
 GRIDALT_OPTIONS.h dyn2phys.F gridalt_diagnostics_init.F gridalt_initialise.F gridalt_mapping.h gridalt_update.F make_phys_grid.F phys2dyn.F :
	$(LN) ../../../pkg/gridalt/$@ $@

#  These files are linked from ../../../pkg/mdsio
 MDSIO_OPTIONS.h MDSIO_SCPU.h mdsio_gl.F mdsio_gl_slice.F mdsio_passtorl.F mdsio_passtors.F mdsio_read_rl_vec.F mdsio_read_rs_vec.F mdsio_readfield.F mdsio_readfield_loc.F mdsio_readtile.F mdsio_readvector.F mdsio_seg4torl.F mdsio_seg4tors.F mdsio_seg8torl.F mdsio_seg8tors.F mdsio_slice.F mdsio_slice_loc.F mdsio_write_rl_vec.F mdsio_write_rs_vec.F mdsio_writefield.F mdsio_writefield_loc.F mdsio_writefield_new.F mdsio_writelocal.F mdsio_writemeta.F mdsio_writetile.F mdsio_writevec_loc.F mdsio_writevector.F mdsio_ad.flow :
	$(LN) ../../../pkg/mdsio/$@ $@

#  These files are linked from ../../../pkg/mnc
 MNC_BUFF.h MNC_CW_READWRITE_I.F MNC_CW_READWRITE_RL.F MNC_CW_READWRITE_RS.F MNC_OPTIONS.h MNC_PARAMS.h MNC_SIZE.h mnc_common.h mnc_create_dir.c mnc_cw_citer.F mnc_cw_cvars.F mnc_cw_fsize.c mnc_cw_init.F mnc_cw_missingvals.F mnc_cw_model_attr.F mnc_cw_udim.F mnc_cw_write_grid_info.F mnc_cwrapper.F mnc_dim.F mnc_dump.F mnc_file.F mnc_grid.F mnc_id_header.h mnc_init.F mnc_readparms.F mnc_utils.F mnc_var.F mnc_ad.flow :
	$(LN) ../../../pkg/mnc/$@ $@

#  These files are linked from ../../../pkg/mom_common
 MOM_COMMON_OPTIONS.h mom_calc_absvort3.F mom_calc_hdiv.F mom_calc_hfacz.F mom_calc_ke.F mom_calc_relvort3.F mom_calc_strain.F mom_calc_tension.F mom_calc_visc.F mom_diagnostics_init.F mom_hdissip.F mom_init_fixed.F mom_u_bottomdrag.F mom_u_implicit_r.F mom_u_rviscflux.F mom_u_sidedrag.F mom_v_bottomdrag.F mom_v_implicit_r.F mom_v_rviscflux.F mom_v_sidedrag.F :
	$(LN) ../../../pkg/mom_common/$@ $@

#  These files are linked from ../../../pkg/mom_vecinv
 MOM_VECINV_OPTIONS.h mom_vecinv.F mom_vi_coriolis.F mom_vi_del2uv.F mom_vi_hdissip.F mom_vi_u_coriolis.F mom_vi_u_coriolis_c4.F mom_vi_u_grad_ke.F mom_vi_u_vertshear.F mom_vi_v_coriolis.F mom_vi_v_coriolis_c4.F mom_vi_v_grad_ke.F mom_vi_v_vertshear.F :
	$(LN) ../../../pkg/mom_vecinv/$@ $@

#  These files are linked from ../../../pkg/monitor
 MONITOR.h MONITOR_OPTIONS.h mon_advcfl.F mon_advcflw.F mon_advcflw2.F mon_init.F mon_ke.F mon_out.F mon_printstats_rl.F mon_printstats_rs.F mon_set_iounit.F mon_set_pref.F mon_solution.F mon_stats_latbnd_rl.F mon_stats_rl.F mon_stats_rs.F mon_surfcor.F mon_vort3.F mon_writestats_rl.F monitor.F monitor_ad.flow :
	$(LN) ../../../pkg/monitor/$@ $@

#  These files are linked from ../../../pkg/rw
 RW_OPTIONS.h get_write_global_fld.F read_fld_xy_rl.F read_fld_xy_rs.F read_fld_xyz_rl.F read_fld_xyz_rs.F read_write_rec.F set_write_global_fld.F write_fld_xy_rl.F write_fld_xy_rs.F write_fld_xyz_rl.F write_fld_xyz_rs.F write_local_rl.F write_local_rs.F write_myfield.F :
	$(LN) ../../../pkg/rw/$@ $@

#  These files are linked from ../../../pkg/shap_filt
 SHAP_FILT.h SHAP_FILT_OPTIONS.h shap_filt_apply_ts.F shap_filt_apply_uv.F shap_filt_computvort.F shap_filt_diagnostics_init.F shap_filt_init_fixed.F shap_filt_readparms.F shap_filt_relvort3.F shap_filt_tracer_s1.F shap_filt_tracer_s2.F shap_filt_tracer_s4.F shap_filt_tracerold.F shap_filt_u.F shap_filt_uv_s1.F shap_filt_uv_s2.F shap_filt_uv_s2c.F shap_filt_uv_s4.F shap_filt_v.F :
	$(LN) ../../../pkg/shap_filt/$@ $@

#  These files are linked from ../../../pkg/timeave
 TIMEAVE_OPTIONS.h TIMEAVE_STATV.h timeave_cumul_1k.F timeave_cumul_1k1t.F timeave_cumul_1t.F timeave_cumul_1vfc.F timeave_cumul_2v.F timeave_cumul_2vfc.F timeave_cumul_dif_1t.F timeave_cumul_fc.F timeave_cumulate.F timeave_init_fixed.F timeave_norma_1k.F timeave_norma_2v.F timeave_normaliz.F timeave_reset.F timeave_reset_1k.F timeave_statv_write.F timeave_statvars.F timeave_surf_flux.F :
	$(LN) ../../../pkg/timeave/$@ $@

#  These files are linked from ../../../pkg/atm_common
 ATM_COMMON_OPTIONS.h atm_diagnostics_init.F :
	$(LN) ../../../pkg/atm_common/$@ $@

#  These files are linked from ../../../eesupp/src
 bar2.F barrier.F check_threads.F comm_stats.F dfile.F diff_phase_multiple.F different_multiple.F eeboot.F eeboot_minimal.F eedata_example.F eedie.F eeintro_msg.F eeset_parms.F eewrite_eeenv.F exch_cycle_ebl.F exch_init.F exch_jam.F exch_r4.F exch_r4_cube.F exch_r4_recv_get_x.F exch_r4_recv_get_y.F exch_r4_send_put_x.F exch_r4_send_put_y.F exch_r8.F exch_r8_cube.F exch_r8_recv_get_x.F exch_r8_recv_get_y.F exch_r8_send_put_x.F exch_r8_send_put_y.F exch_rl.F exch_rl_cube.F exch_rl_recv_get_x.F exch_rl_recv_get_y.F exch_rl_send_put_x.F exch_rl_send_put_y.F exch_rs.F exch_rs_cube.F exch_rs_recv_get_x.F exch_rs_recv_get_y.F exch_rs_send_put_x.F exch_rs_send_put_y.F exch_s3d_r4.F exch_s3d_r8.F exch_s3d_rl.F exch_s3d_rs.F exch_uv_agrid_xy_r4.F exch_uv_agrid_xy_r8.F exch_uv_agrid_xy_rl.F exch_uv_agrid_xy_rs.F exch_uv_agrid_xyz_r4.F exch_uv_agrid_xyz_r8.F exch_uv_agrid_xyz_rl.F exch_uv_agrid_xyz_rs.F exch_uv_r4_cube.F exch_uv_r8_cube.F exch_uv_rl_cube.F exch_uv_rs_cube.F exch_uv_xy_r4.F exch_uv_xy_r8.F exch_uv_xy_rl.F exch_uv_xy_rs.F exch_uv_xyz_r4.F exch_uv_xyz_r8.F exch_uv_xyz_rl.F exch_uv_xyz_rs.F exch_xy_r4.F exch_xy_r8.F exch_xy_rl.F exch_xy_rs.F exch_xyz_r4.F exch_xyz_r8.F exch_xyz_rl.F exch_xyz_rs.F exch_z_r4_cube.F exch_z_r8_cube.F exch_z_rl_cube.F exch_z_rs_cube.F exch_z_xy_r4.F exch_z_xy_r8.F exch_z_xy_rl.F exch_z_xy_rs.F exch_z_xyz_r4.F exch_z_xyz_r8.F exch_z_xyz_rl.F exch_z_xyz_rs.F fill_cs_corner_tr_rl.F fill_cs_corner_uv_rs.F fool_the_compiler.F gather_2d.F gather_vector.F global_max.F global_sum.F global_vec_sum.F gsl_ieee_env.c gsum.F gsum_jam.F ini_communication_patterns.F ini_jam.F ini_procs.F ini_threading_environment.F lef_zero.F main.F mds_byteswapi4.F mds_byteswapr4.F mds_byteswapr8.F mds_reclen.F mdsfindunit.F memsync.F nml_filter.F nml_set_terminator.F print.F ptwrapper.c scatter_2d.F scatter_vector.F setrlstk.c sigreg.c tim.c timer_stats.c timers.F utils.F eesupp.flow :
	$(LN) ../../../eesupp/src/$@ $@

#  These files are linked from ../../../model/src
 adams_bashforth2.F adams_bashforth3.F askparam.F calc_3d_diffusivity.F calc_buoyancy.F calc_common_factors.F calc_diffusivity.F calc_div_ghat.F calc_grad_phi_hyd.F calc_grad_phi_surf.F calc_gs.F calc_gt.F calc_gw.F calc_ivdc.F calc_phi_hyd.F calc_r_star.F calc_surf_dr.F calc_viscosity.F cg2d.F cg3d.F checkpoint.F config_check.F config_summary.F convect.F convective_adjustment.F convective_adjustment_ini.F convective_weights.F convectively_mixtracer.F correction_step.F cycle_tracer.F diags_oceanic_surf_flux.F diags_phi_hyd.F diags_phi_rlow.F diags_rho.F do_atmospheric_phys.F do_fields_blocking_exchanges.F do_gterm_blocking_exchanges.F do_oceanic_phys.F do_stagger_fields_exchanges.F do_statevars_diags.F do_statevars_tave.F do_the_model_io.F dynamics.F external_fields_load.F external_forcing.F external_forcing_surf.F find_alpha.F find_rho.F forward_step.F freesurf_rescale_g.F freeze.F freeze_surface.F getcon.F grad_sigma.F impldiff.F ini_cartesian_grid.F ini_cg2d.F ini_cg3d.F ini_cori.F ini_curvilinear_grid.F ini_cylinder_grid.F ini_depths.F ini_dynvars.F ini_eos.F ini_fields.F ini_forcing.F ini_grid.F ini_linear_phisurf.F ini_masks_etc.F ini_mixing.F ini_mnc_vars.F ini_model_io.F ini_nh_vars.F ini_p_ground.F ini_parms.F ini_phiref.F ini_pnh.F ini_pressure.F ini_psurf.F ini_r_star.F ini_spherical_polar_grid.F ini_surf_dr.F ini_theta.F ini_vel.F ini_vertical_grid.F initialise_fixed.F initialise_varia.F integr_continuity.F integrate_for_w.F load_grid_spacing.F modeldata_example.F momentum_correction_step.F open_copy_data_file.F packages_boot.F packages_check.F packages_check_error.F packages_init_fixed.F packages_init_variables.F packages_readparms.F packages_write_pickup.F plot_field.F port_rand.F pressure_for_eos.F quasihydrostaticterms.F read_write.F remove_mean.F set_defaults.F solve_for_pressure.F solve_pentadiagonal.F solve_tridiagonal.F state_summary.F swfrac.F taueddy_external_forcing.F taueddy_init_varia.F the_correction_step.F the_main_loop.F the_model_main.F thermodynamics.F timestep.F timestep_tracer.F timestep_wvel.F tracers_correction_step.F tracers_iigw_correction.F update_cg2d.F update_etah.F update_r_star.F update_surf_dr.F write_grid.F write_state.F :
	$(LN) ../../../model/src/$@ $@

#  These files are linked from /opt/sgi/mpt/1.12.0.0/include
 mpi++.h mpi.h mpi_ext.h mpi_extf.h mpi_gsm.h mpi_gsmf.h mpif.h mpif_parameters.h mpio.h mpiof.h :
	$(LN) /opt/sgi/mpt/1.12.0.0/include/$@ $@

#  These files are linked from ../../../eesupp/inc
 BAR2.h BARRIER.h CPP_EEMACROS.h CPP_EEOPTIONS.h DFILE.h EEIO.h EEPARAMS.h EESUPPORT.h EXCH.h EXCH_JAM.h GLOBAL_MAX.h GLOBAL_SUM.h JAM_INFO.h MAIN_PDIRECTIVES1.h MAIN_PDIRECTIVES2.h MPI_INFO.h SIGREG.h :
	$(LN) ../../../eesupp/inc/$@ $@

#  These files are linked from ../../../model/inc
 CG2D.h CG3D.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYNVARS.h DYU_MACROS.h EOS.h FCORI_MACROS.h FFIELDS.h GRID.h GRID_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h MASKS_MACROS.h MASKW_MACROS.h NH_VARS.h PARAMS.h PARAMS_MACROS.h RAS_MACROS.h RAW_MACROS.h RA_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h SOLVE_FOR_PRESSURE.h SOLVE_FOR_PRESSURE3D.h SURFACE.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h THE_MODEL_COMMON_BLOCKS.h XC_MACROS.h YC_MACROS.h :
	$(LN) ../../../model/inc/$@ $@


# DO NOT DELETE

do_fizhi.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_fizhi.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h fizhi_SIZE.h chronos.h
fizhi_clockstuff.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_clockstuff.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fizhi_clockstuff.f: EEPARAMS.h EESUPPORT.h mpif.h chronos.h
fizhi_init_fixed.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_init_fixed.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fizhi_init_fixed.f: fizhi_SIZE.h fizhi_land_SIZE.h EEPARAMS.h
fizhi_init_fixed.f: fizhi_chemistry_coms.h fizhi_earth_coms.h
fizhi_init_fixed.f: fizhi_land_coms.h fizhi_ocean_coms.h chronos.h
fizhi_init_fixed.f: gridalt_mapping.h GRID.h GRID_MACROS.h DXC_MACROS.h
fizhi_init_fixed.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
fizhi_init_fixed.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
fizhi_init_fixed.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
fizhi_init_fixed.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
fizhi_init_fixed.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
fizhi_init_fixed.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
fizhi_init_fixed.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
fizhi_init_fixed.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
fizhi_init_fixed.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
fizhi_init_fixed.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h
fizhi_init_fixed.f: FCORI_MACROS.h
fizhi_init_veg.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_init_veg.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
fizhi_init_vegsurftiles.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_init_vegsurftiles.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fizhi_init_vegsurftiles.f: fizhi_SIZE.h fizhi_land_SIZE.h fizhi_coms.h
fizhi_init_vegsurftiles.f: fizhi_land_coms.h fizhi_earth_coms.h EEPARAMS.h
fizhi_init_vegsurftiles.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
fizhi_swrad.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_swrad.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h cai-dat.h
fizhi_swrad.f: cah-dat.h
fizhi_turb.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
fizhi_turb.f: CPP_EEMACROS.h BUILD_INFO.h mpif.h
ini_salt.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
ini_salt.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_salt.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_salt.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_salt.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_salt.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_salt.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_salt.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_salt.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_salt.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_salt.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
update_earth_exports.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
update_earth_exports.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
update_earth_exports.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
update_earth_exports.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
update_earth_exports.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
update_earth_exports.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
update_earth_exports.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
update_earth_exports.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
update_earth_exports.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
update_earth_exports.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
update_earth_exports.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
update_earth_exports.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
update_earth_exports.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
update_earth_exports.f: TANPHIATV_MACROS.h fizhi_land_SIZE.h fizhi_SIZE.h
update_earth_exports.f: fizhi_coms.h chronos.h gridalt_mapping.h
update_earth_exports.f: fizhi_land_coms.h fizhi_earth_coms.h
update_earth_exports.f: fizhi_ocean_coms.h EEPARAMS.h snwmid.h
update_ocean_exports.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
update_ocean_exports.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
update_ocean_exports.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
update_ocean_exports.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
update_ocean_exports.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
update_ocean_exports.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
update_ocean_exports.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
update_ocean_exports.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
update_ocean_exports.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
update_ocean_exports.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
update_ocean_exports.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
update_ocean_exports.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
update_ocean_exports.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
update_ocean_exports.f: TANPHIATV_MACROS.h fizhi_ocean_coms.h EEPARAMS.h
update_ocean_exports.f: chronos.h W2_EXCH2_TOPOLOGY.h W2_EXCH2_PARAMS.h
chksum_tiled.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
chksum_tiled.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
debug_call.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
debug_call.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
debug_cs_corner_uv.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_cs_corner_uv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
debug_cs_corner_uv.f: EEPARAMS.h W2_EXCH2_TOPOLOGY.h W2_EXCH2_PARAMS.h
debug_enter.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_enter.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
debug_fld_stats_rl.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_fld_stats_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
debug_fld_stats_rl.f: EEPARAMS.h
debug_fld_stats_rs.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_fld_stats_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
debug_fld_stats_rs.f: EEPARAMS.h
debug_leave.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_leave.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
debug_msg.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
debug_msg.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
debug_stats_rl.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_stats_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
debug_stats_rl.f: EEPARAMS.h
debug_stats_rs.f: DEBUG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
debug_stats_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
debug_stats_rs.f: EEPARAMS.h
fill_in_corners_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fill_in_corners_rl.f: EEPARAMS.h
write_fullarray_rl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
write_fullarray_rl.f: BUILD_INFO.h SIZE.h EEPARAMS.h
diag_vegtile_fill.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diag_vegtile_fill.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diag_vegtile_fill.f: SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_add2list.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_add2list.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_add2list.f: EEPARAMS.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_check.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_check.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_check.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diagnostics_check.f: DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_clear.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_clear.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagnostics_clear.f: SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_fill.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_fill.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagnostics_fill.f: SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_fill_field.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_fill_field.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diagnostics_fill_field.f: EEPARAMS.h SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_fill_state.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_fill_state.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_fill_state.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diagnostics_fill_state.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
diagnostics_fill_state.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
diagnostics_fill_state.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
diagnostics_fill_state.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
diagnostics_fill_state.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
diagnostics_fill_state.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
diagnostics_fill_state.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
diagnostics_fill_state.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
diagnostics_fill_state.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
diagnostics_fill_state.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h
diagnostics_fill_state.f: RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
diagnostics_fill_state.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
diagnostics_fill_state.f: SURFACE.h
diagnostics_fract_fill.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_fract_fill.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diagnostics_fract_fill.f: EEPARAMS.h SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_init_early.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_init_early.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_init_early.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diagnostics_init_early.f: DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_init_fixed.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_init_fixed.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diagnostics_init_fixed.f: EEPARAMS.h SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_init_fixed.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diagnostics_init_varia.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_init_varia.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_init_varia.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diagnostics_init_varia.f: DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_interp_vert.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_interp_vert.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_interp_vert.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diagnostics_interp_vert.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
diagnostics_interp_vert.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
diagnostics_interp_vert.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
diagnostics_interp_vert.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
diagnostics_interp_vert.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
diagnostics_interp_vert.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
diagnostics_interp_vert.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
diagnostics_interp_vert.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
diagnostics_interp_vert.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
diagnostics_interp_vert.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
diagnostics_interp_vert.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
diagnostics_interp_vert.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
diagnostics_interp_vert.f: TANPHIATV_MACROS.h DIAGNOSTICS_SIZE.h
diagnostics_interp_vert.f: DIAGNOSTICS.h fizhi_SIZE.h
diagnostics_is_on.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_is_on.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagnostics_is_on.f: SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_main_init.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_main_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_main_init.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diagnostics_out.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_out.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_out.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
diagnostics_out.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
diagnostics_out.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
diagnostics_out.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
diagnostics_out.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
diagnostics_out.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
diagnostics_out.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
diagnostics_out.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
diagnostics_out.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
diagnostics_out.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
diagnostics_out.f: TANPHIATV_MACROS.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_out.f: fizhi_SIZE.h
diagnostics_read_pickup.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_read_pickup.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_read_pickup.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diagnostics_read_pickup.f: DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_readparms.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_readparms.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_readparms.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diagnostics_readparms.f: DIAGNOSTICS_SIZE.h DIAGNOSTICS.h DIAGSTATS_REGIONS.h
diagnostics_scale_fill.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_scale_fill.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diagnostics_scale_fill.f: EEPARAMS.h SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_set_levels.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_set_levels.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_set_levels.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diagnostics_set_levels.f: DIAGNOSTICS_SIZE.h DIAGNOSTICS.h fizhi_SIZE.h
diagnostics_set_pointers.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_set_pointers.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diagnostics_set_pointers.f: EEPARAMS.h SIZE.h DIAGNOSTICS_SIZE.h
diagnostics_set_pointers.f: DIAGNOSTICS.h
diagnostics_setdiag.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_setdiag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagnostics_setdiag.f: SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_setunits.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_setunits.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diagnostics_setunits.f: EEPARAMS.h SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_summary.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_summary.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagnostics_summary.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diagnostics_summary.f: DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_switch_onoff.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_switch_onoff.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diagnostics_switch_onoff.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
diagnostics_switch_onoff.f: FCORI_MACROS.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_utils.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_utils.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagnostics_utils.f: SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagnostics_write.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_write.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagnostics_write.f: SIZE.h DIAGNOSTICS_SIZE.h PARAMS.h PARAMS_MACROS.h
diagnostics_write.f: FCORI_MACROS.h DIAGNOSTICS.h
diagnostics_write_pickup.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagnostics_write_pickup.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diagnostics_write_pickup.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
diagnostics_write_pickup.f: FCORI_MACROS.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagstats_ascii_out.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_ascii_out.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagstats_ascii_out.f: EEPARAMS.h EESUPPORT.h mpif.h DIAGNOSTICS_SIZE.h
diagstats_ascii_out.f: DIAGNOSTICS.h
diagstats_calc.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_calc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagstats_calc.f: SIZE.h
diagstats_clear.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_clear.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagstats_clear.f: SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagstats_close_io.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_close_io.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagstats_close_io.f: EEPARAMS.h EESUPPORT.h mpif.h PARAMS.h PARAMS_MACROS.h
diagstats_close_io.f: FCORI_MACROS.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagstats_fill.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_fill.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagstats_fill.f: SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagstats_global.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_global.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagstats_global.f: SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagstats_ini_io.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_ini_io.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagstats_ini_io.f: EEPARAMS.h EESUPPORT.h mpif.h PARAMS.h PARAMS_MACROS.h
diagstats_ini_io.f: FCORI_MACROS.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagstats_local.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_local.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagstats_local.f: SIZE.h DIAGNOSTICS_SIZE.h DIAGSTATS_REGIONS.h PARAMS.h
diagstats_local.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
diagstats_local.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
diagstats_local.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
diagstats_local.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
diagstats_local.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
diagstats_local.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
diagstats_local.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
diagstats_local.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
diagstats_local.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
diagstats_local.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
diagstats_local.f: TANPHIATV_MACROS.h
diagstats_mnc_out.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_mnc_out.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagstats_mnc_out.f: EEPARAMS.h EESUPPORT.h mpif.h PARAMS.h PARAMS_MACROS.h
diagstats_mnc_out.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
diagstats_mnc_out.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
diagstats_mnc_out.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
diagstats_mnc_out.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
diagstats_mnc_out.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
diagstats_mnc_out.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
diagstats_mnc_out.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
diagstats_mnc_out.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
diagstats_mnc_out.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
diagstats_mnc_out.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
diagstats_mnc_out.f: TANPHIATV_MACROS.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagstats_mnc_out.f: fizhi_SIZE.h
diagstats_others_calc.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_others_calc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diagstats_others_calc.f: EEPARAMS.h SIZE.h fizhi_SIZE.h gridalt_mapping.h
diagstats_output.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_output.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
diagstats_output.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
diagstats_output.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
diagstats_output.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
diagstats_output.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
diagstats_output.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
diagstats_output.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
diagstats_output.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
diagstats_output.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
diagstats_output.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
diagstats_output.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
diagstats_output.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DIAGNOSTICS_SIZE.h
diagstats_output.f: DIAGNOSTICS.h fizhi_SIZE.h
diagstats_set_pointers.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_set_pointers.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diagstats_set_pointers.f: EEPARAMS.h SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
diagstats_set_pointers.f: DIAGSTATS_REGIONS.h
diagstats_set_regions.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_set_regions.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diagstats_set_regions.f: EEPARAMS.h SIZE.h PARAMS.h PARAMS_MACROS.h
diagstats_set_regions.f: FCORI_MACROS.h DIAGNOSTICS_SIZE.h
diagstats_set_regions.f: DIAGSTATS_REGIONS.h
diagstats_setdiag.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
diagstats_setdiag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
diagstats_setdiag.f: SIZE.h DIAGNOSTICS_SIZE.h DIAGNOSTICS.h
prestopres.f: DIAG_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
prestopres.f: CPP_EEMACROS.h BUILD_INFO.h
exch2_r41_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_r41_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h W2_EXCH2_TOPOLOGY.h
exch2_r41_cube.f: W2_EXCH2_PARAMS.h
exch2_r42_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_r42_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h W2_EXCH2_TOPOLOGY.h
exch2_r42_cube.f: W2_EXCH2_PARAMS.h
exch2_r81_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_r81_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h W2_EXCH2_TOPOLOGY.h
exch2_r81_cube.f: W2_EXCH2_PARAMS.h
exch2_r82_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_r82_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h W2_EXCH2_TOPOLOGY.h
exch2_r82_cube.f: W2_EXCH2_PARAMS.h
exch2_recv_r41.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_recv_r41.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_recv_r42.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_recv_r42.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_recv_r81.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_recv_r81.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_recv_r82.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_recv_r82.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_recv_rl1.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_recv_rl1.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_recv_rl2.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_recv_rl2.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_recv_rs1.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_recv_rs1.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_recv_rs2.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_recv_rs2.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_rl1_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_rl1_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h W2_EXCH2_TOPOLOGY.h
exch2_rl1_cube.f: W2_EXCH2_PARAMS.h
exch2_rl2_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_rl2_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h W2_EXCH2_TOPOLOGY.h
exch2_rl2_cube.f: W2_EXCH2_PARAMS.h
exch2_rs1_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_rs1_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h W2_EXCH2_TOPOLOGY.h
exch2_rs1_cube.f: W2_EXCH2_PARAMS.h
exch2_rs2_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_rs2_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h W2_EXCH2_TOPOLOGY.h
exch2_rs2_cube.f: W2_EXCH2_PARAMS.h
exch2_s3d_r4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_s3d_r4.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_s3d_r4.f: W2_EXCH2_PARAMS.h
exch2_s3d_r8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_s3d_r8.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_s3d_r8.f: W2_EXCH2_PARAMS.h
exch2_s3d_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_s3d_rl.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_s3d_rl.f: W2_EXCH2_PARAMS.h
exch2_s3d_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_s3d_rs.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_s3d_rs.f: W2_EXCH2_PARAMS.h
exch2_send_r41.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_send_r41.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_send_r42.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_send_r42.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_send_r81.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_send_r81.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_send_r82.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_send_r82.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_send_rl1.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_send_rl1.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_send_rl2.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_send_rl2.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_send_rs1.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_send_rs1.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_send_rs2.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_send_rs2.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_agrid_xy_r4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_uv_agrid_xy_r4.f: EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_agrid_xy_r4.f: W2_EXCH2_PARAMS.h
exch2_uv_agrid_xy_r8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_uv_agrid_xy_r8.f: EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_agrid_xy_r8.f: W2_EXCH2_PARAMS.h
exch2_uv_agrid_xy_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_uv_agrid_xy_rl.f: EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_agrid_xy_rl.f: W2_EXCH2_PARAMS.h
exch2_uv_agrid_xy_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_uv_agrid_xy_rs.f: EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_agrid_xy_rs.f: W2_EXCH2_PARAMS.h
exch2_uv_agrid_xyz_r4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_uv_agrid_xyz_r4.f: EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_agrid_xyz_r4.f: W2_EXCH2_PARAMS.h
exch2_uv_agrid_xyz_r8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_uv_agrid_xyz_r8.f: EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_agrid_xyz_r8.f: W2_EXCH2_PARAMS.h
exch2_uv_agrid_xyz_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_uv_agrid_xyz_rl.f: EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_agrid_xyz_rl.f: W2_EXCH2_PARAMS.h
exch2_uv_agrid_xyz_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch2_uv_agrid_xyz_rs.f: EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_agrid_xyz_rs.f: W2_EXCH2_PARAMS.h
exch2_uv_xy_r4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_uv_xy_r4.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_xy_r4.f: W2_EXCH2_PARAMS.h
exch2_uv_xy_r8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_uv_xy_r8.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_xy_r8.f: W2_EXCH2_PARAMS.h
exch2_uv_xy_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_uv_xy_rl.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_xy_rl.f: W2_EXCH2_PARAMS.h
exch2_uv_xy_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_uv_xy_rs.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_xy_rs.f: W2_EXCH2_PARAMS.h
exch2_uv_xyz_r4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_uv_xyz_r4.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_xyz_r4.f: W2_EXCH2_PARAMS.h
exch2_uv_xyz_r8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_uv_xyz_r8.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_xyz_r8.f: W2_EXCH2_PARAMS.h
exch2_uv_xyz_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_uv_xyz_rl.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_xyz_rl.f: W2_EXCH2_PARAMS.h
exch2_uv_xyz_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_uv_xyz_rs.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_uv_xyz_rs.f: W2_EXCH2_PARAMS.h
exch2_xy_r4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_xy_r4.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_xy_r4.f: W2_EXCH2_PARAMS.h
exch2_xy_r8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_xy_r8.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_xy_r8.f: W2_EXCH2_PARAMS.h
exch2_xy_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_xy_rl.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_xy_rl.f: W2_EXCH2_PARAMS.h
exch2_xy_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_xy_rs.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_xy_rs.f: W2_EXCH2_PARAMS.h
exch2_xyz_r4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_xyz_r4.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_xyz_r4.f: W2_EXCH2_PARAMS.h
exch2_xyz_r8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_xyz_r8.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_xyz_r8.f: W2_EXCH2_PARAMS.h
exch2_xyz_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_xyz_rl.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_xyz_rl.f: W2_EXCH2_PARAMS.h
exch2_xyz_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_xyz_rs.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_xyz_rs.f: W2_EXCH2_PARAMS.h
exch2_z_xy_r4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_z_xy_r4.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_z_xy_r4.f: W2_EXCH2_PARAMS.h
exch2_z_xy_r8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_z_xy_r8.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_z_xy_r8.f: W2_EXCH2_PARAMS.h
exch2_z_xy_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_z_xy_rl.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_z_xy_rl.f: W2_EXCH2_PARAMS.h
exch2_z_xy_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h W2_OPTIONS.h
exch2_z_xy_rs.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
exch2_z_xy_rs.f: W2_EXCH2_PARAMS.h
w2_e2setup.f: W2_EXCH2_TOPOLOGY.h
w2_eeboot.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
w2_eeboot.f: EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h W2_EXCH2_PARAMS.h
w2_print_comm_sequence.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
w2_print_comm_sequence.f: EEPARAMS.h EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h
w2_print_comm_sequence.f: W2_EXCH2_PARAMS.h
AtoC.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
AtoC.f: CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
CtoA.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
CtoA.f: CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
fizhi_alarms.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_alarms.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h mpif.h
fizhi_diagalarms.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_diagalarms.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fizhi_diagalarms.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
fizhi_diagalarms.f: DIAGNOSTICS_SIZE.h DIAGNOSTICS.h chronos.h
fizhi_diagnostics_init.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_diagnostics_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
fizhi_diagnostics_init.f: EEPARAMS.h
fizhi_driver.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_driver.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fizhi_driver.f: DIAGNOSTICS_SIZE.h DIAGNOSTICS.h chronos.h
fizhi_fillnegs.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_fillnegs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
fizhi_gwdrag.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_gwdrag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
fizhi_init_chem.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_init_chem.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
fizhi_init_vars.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_init_vars.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fizhi_init_vars.f: fizhi_SIZE.h fizhi_land_SIZE.h GRID.h GRID_MACROS.h
fizhi_init_vars.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
fizhi_init_vars.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
fizhi_init_vars.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
fizhi_init_vars.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
fizhi_init_vars.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
fizhi_init_vars.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
fizhi_init_vars.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
fizhi_init_vars.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
fizhi_init_vars.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
fizhi_init_vars.f: TANPHIATV_MACROS.h DYNVARS.h gridalt_mapping.h
fizhi_init_vars.f: fizhi_coms.h fizhi_land_coms.h fizhi_earth_coms.h
fizhi_init_vars.f: EEPARAMS.h SURFACE.h PARAMS.h PARAMS_MACROS.h
fizhi_init_vars.f: FCORI_MACROS.h chronos.h
fizhi_lsm.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
fizhi_lsm.f: CPP_EEMACROS.h BUILD_INFO.h sibber.h snwmid.h
fizhi_lwrad.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_lwrad.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h h2o-tran3.h
fizhi_lwrad.f: co2-tran3.h o3-tran3.h
fizhi_mnc_init.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_mnc_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fizhi_mnc_init.f: fizhi_SIZE.h fizhi_land_SIZE.h EEPARAMS.h PARAMS.h
fizhi_mnc_init.f: PARAMS_MACROS.h FCORI_MACROS.h MNC_PARAMS.h mnc_common.h
fizhi_mnc_init.f: MNC_SIZE.h
fizhi_moist.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_moist.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
fizhi_mpistuff.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_mpistuff.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
fizhi_rayleigh.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_rayleigh.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
fizhi_read_pickup.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_read_pickup.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fizhi_read_pickup.f: fizhi_SIZE.h fizhi_land_SIZE.h fizhi_coms.h
fizhi_read_pickup.f: fizhi_land_coms.h fizhi_io_comms.h EEPARAMS.h PARAMS.h
fizhi_read_pickup.f: PARAMS_MACROS.h FCORI_MACROS.h
fizhi_readparms.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_readparms.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h chronos.h
fizhi_readparms.f: fizhi_io_comms.h EEPARAMS.h SIZE.h PARAMS.h
fizhi_readparms.f: PARAMS_MACROS.h FCORI_MACROS.h
fizhi_readwrite_vegtiles.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_readwrite_vegtiles.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
fizhi_readwrite_vegtiles.f: SIZE.h fizhi_SIZE.h fizhi_land_SIZE.h
fizhi_readwrite_vegtiles.f: fizhi_coms.h fizhi_land_coms.h fizhi_earth_coms.h
fizhi_readwrite_vegtiles.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h
fizhi_readwrite_vegtiles.f: FCORI_MACROS.h MNC_PARAMS.h W2_EXCH2_TOPOLOGY.h
fizhi_readwrite_vegtiles.f: W2_EXCH2_PARAMS.h
fizhi_step_diag.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_step_diag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
fizhi_tendency_apply.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_tendency_apply.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fizhi_tendency_apply.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
fizhi_tendency_apply.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
fizhi_tendency_apply.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
fizhi_tendency_apply.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
fizhi_tendency_apply.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
fizhi_tendency_apply.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
fizhi_tendency_apply.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
fizhi_tendency_apply.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
fizhi_tendency_apply.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
fizhi_tendency_apply.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
fizhi_tendency_apply.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
fizhi_tendency_apply.f: TANPHIATV_MACROS.h EEPARAMS.h DYNVARS.h fizhi_SIZE.h
fizhi_tendency_apply.f: fizhi_land_SIZE.h fizhi_coms.h
fizhi_update_time.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_update_time.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h chronos.h
fizhi_utils.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_utils.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
fizhi_wrapper.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_wrapper.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
fizhi_wrapper.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
fizhi_wrapper.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
fizhi_wrapper.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
fizhi_wrapper.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
fizhi_wrapper.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
fizhi_wrapper.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
fizhi_wrapper.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
fizhi_wrapper.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
fizhi_wrapper.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
fizhi_wrapper.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
fizhi_wrapper.f: FCORI_MACROS.h SURFACE.h DYNVARS.h fizhi_land_SIZE.h
fizhi_wrapper.f: fizhi_SIZE.h fizhi_coms.h gridalt_mapping.h
fizhi_wrapper.f: fizhi_land_coms.h fizhi_earth_coms.h fizhi_ocean_coms.h
fizhi_wrapper.f: fizhi_chemistry_coms.h fizhi_SHP.h
fizhi_write_datetime.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_write_datetime.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h chronos.h
fizhi_write_pickup.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_write_pickup.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fizhi_write_pickup.f: fizhi_SIZE.h fizhi_land_SIZE.h fizhi_coms.h
fizhi_write_pickup.f: fizhi_land_coms.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
fizhi_write_pickup.f: FCORI_MACROS.h
fizhi_write_state.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
fizhi_write_state.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
fizhi_write_state.f: fizhi_SIZE.h fizhi_land_SIZE.h fizhi_coms.h
fizhi_write_state.f: fizhi_land_coms.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
fizhi_write_state.f: FCORI_MACROS.h
getpwhere.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
getpwhere.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h fizhi_SIZE.h GRID.h
getpwhere.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
getpwhere.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
getpwhere.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
getpwhere.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
getpwhere.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
getpwhere.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
getpwhere.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
getpwhere.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
getpwhere.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
step_fizhi_corr.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
step_fizhi_corr.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
step_fizhi_corr.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
step_fizhi_corr.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
step_fizhi_corr.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
step_fizhi_corr.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
step_fizhi_corr.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
step_fizhi_corr.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
step_fizhi_corr.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
step_fizhi_corr.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
step_fizhi_corr.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
step_fizhi_corr.f: TANPHIATV_MACROS.h fizhi_SIZE.h fizhi_land_SIZE.h
step_fizhi_corr.f: DYNVARS.h fizhi_coms.h gridalt_mapping.h EEPARAMS.h
step_fizhi_corr.f: SURFACE.h fizhi_SHP.h
step_fizhi_fg.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
step_fizhi_fg.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
step_fizhi_fg.f: fizhi_SIZE.h fizhi_land_SIZE.h fizhi_coms.h
step_fizhi_fg.f: gridalt_mapping.h EEPARAMS.h DYNVARS.h GRID.h GRID_MACROS.h
step_fizhi_fg.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
step_fizhi_fg.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
step_fizhi_fg.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
step_fizhi_fg.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
step_fizhi_fg.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
step_fizhi_fg.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
step_fizhi_fg.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
step_fizhi_fg.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
step_fizhi_fg.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
step_fizhi_fg.f: TANPHIATV_MACROS.h
step_physics.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
step_physics.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
update_chemistry_exports.f: FIZHI_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
update_chemistry_exports.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
update_chemistry_exports.f: SIZE.h fizhi_SIZE.h fizhi_land_SIZE.h GRID.h
update_chemistry_exports.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
update_chemistry_exports.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
update_chemistry_exports.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
update_chemistry_exports.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
update_chemistry_exports.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
update_chemistry_exports.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
update_chemistry_exports.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
update_chemistry_exports.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
update_chemistry_exports.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
update_chemistry_exports.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
update_chemistry_exports.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
update_chemistry_exports.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
update_chemistry_exports.f: TANPHIATV_MACROS.h DYNVARS.h
update_chemistry_exports.f: fizhi_chemistry_coms.h fizhi_coms.h
update_chemistry_exports.f: gridalt_mapping.h EEPARAMS.h chronos.h
gad_advection.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_advection.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
gad_advection.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
gad_advection.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_advection.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_advection.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_advection.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_advection.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_advection.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_advection.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_advection.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_advection.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_advection.f: TANPHIATV_MACROS.h GAD.h W2_EXCH2_TOPOLOGY.h
gad_advection.f: W2_EXCH2_PARAMS.h
gad_biharm_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_biharm_x.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_biharm_x.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_biharm_x.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_biharm_x.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_biharm_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_biharm_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_biharm_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_biharm_x.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_biharm_x.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_biharm_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_biharm_x.f: TANPHIATV_MACROS.h
gad_biharm_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_biharm_y.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_biharm_y.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_biharm_y.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_biharm_y.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_biharm_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_biharm_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_biharm_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_biharm_y.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_biharm_y.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_biharm_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_biharm_y.f: TANPHIATV_MACROS.h
gad_c2_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c2_adv_r.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_c2_adv_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_c2_adv_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_c2_adv_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_c2_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_c2_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_c2_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_c2_adv_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_c2_adv_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_c2_adv_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_c2_adv_r.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_c2_adv_r.f: FCORI_MACROS.h
gad_c2_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c2_adv_x.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_c2_adv_x.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_c2_adv_x.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_c2_adv_x.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_c2_adv_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_c2_adv_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_c2_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_c2_adv_x.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_c2_adv_x.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_c2_adv_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_c2_adv_x.f: TANPHIATV_MACROS.h
gad_c2_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c2_adv_y.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_c2_adv_y.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_c2_adv_y.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_c2_adv_y.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_c2_adv_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_c2_adv_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_c2_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_c2_adv_y.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_c2_adv_y.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_c2_adv_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_c2_adv_y.f: TANPHIATV_MACROS.h
gad_c2_impl_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_c2_impl_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
gad_c2_impl_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_c2_impl_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_c2_impl_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_c2_impl_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_c2_impl_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_c2_impl_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_c2_impl_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_c2_impl_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_c2_impl_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_c2_impl_r.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_c2_impl_r.f: FCORI_MACROS.h
gad_c4_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c4_adv_r.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_c4_adv_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_c4_adv_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_c4_adv_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_c4_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_c4_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_c4_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_c4_adv_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_c4_adv_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_c4_adv_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_c4_adv_r.f: TANPHIATV_MACROS.h GAD.h
gad_c4_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c4_adv_x.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_c4_adv_x.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_c4_adv_x.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_c4_adv_x.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_c4_adv_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_c4_adv_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_c4_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_c4_adv_x.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_c4_adv_x.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_c4_adv_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_c4_adv_x.f: TANPHIATV_MACROS.h GAD.h
gad_c4_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_c4_adv_y.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_c4_adv_y.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_c4_adv_y.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_c4_adv_y.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_c4_adv_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_c4_adv_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_c4_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_c4_adv_y.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_c4_adv_y.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_c4_adv_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_c4_adv_y.f: TANPHIATV_MACROS.h GAD.h
gad_calc_rhs.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_calc_rhs.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
gad_calc_rhs.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
gad_calc_rhs.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_calc_rhs.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_calc_rhs.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_calc_rhs.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_calc_rhs.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_calc_rhs.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_calc_rhs.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_calc_rhs.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_calc_rhs.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_calc_rhs.f: TANPHIATV_MACROS.h SURFACE.h GAD.h
gad_del2.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_del2.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_del2.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_del2.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_del2.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_del2.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_del2.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_del2.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_del2.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_del2.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_del2.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_diagnostics_init.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_diagnostics_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
gad_diagnostics_init.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
gad_diagnostics_init.f: GAD.h
gad_diff_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_diff_r.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_diff_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_diff_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_diff_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_diff_r.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_diff_r.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_diff_r.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_diff_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_diff_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_diff_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h
gad_diff_r.f: PARAMS_MACROS.h FCORI_MACROS.h
gad_diff_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_diff_x.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_diff_x.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_diff_x.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_diff_x.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_diff_x.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_diff_x.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_diff_x.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_diff_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_diff_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_diff_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_diff_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_diff_y.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_diff_y.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_diff_y.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_diff_y.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_diff_y.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_diff_y.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_diff_y.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_diff_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_diff_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_diff_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_dst2u1_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst2u1_adv_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
gad_dst2u1_adv_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst2u1_adv_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst2u1_adv_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst2u1_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst2u1_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst2u1_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst2u1_adv_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst2u1_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst2u1_adv_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst2u1_adv_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst2u1_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst2u1_adv_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
gad_dst2u1_adv_x.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst2u1_adv_x.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst2u1_adv_x.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst2u1_adv_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst2u1_adv_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst2u1_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst2u1_adv_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst2u1_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst2u1_adv_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst2u1_adv_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst2u1_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst2u1_adv_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
gad_dst2u1_adv_y.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst2u1_adv_y.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst2u1_adv_y.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst2u1_adv_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst2u1_adv_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst2u1_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst2u1_adv_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst2u1_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst2u1_adv_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst2u1_adv_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst2u1_impl_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst2u1_impl_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
gad_dst2u1_impl_r.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_dst2u1_impl_r.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_dst2u1_impl_r.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_dst2u1_impl_r.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_dst2u1_impl_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_dst2u1_impl_r.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst2u1_impl_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst2u1_impl_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst2u1_impl_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst2u1_impl_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst3_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3_adv_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
gad_dst3_adv_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst3_adv_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst3_adv_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_dst3_adv_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_dst3_adv_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_dst3_adv_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_dst3_adv_r.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_dst3_adv_r.f: FCORI_MACROS.h GAD.h
gad_dst3_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3_adv_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
gad_dst3_adv_x.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst3_adv_x.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst3_adv_x.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3_adv_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3_adv_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_dst3_adv_x.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_dst3_adv_x.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_dst3_adv_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_dst3_adv_x.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_dst3_adv_x.f: FCORI_MACROS.h GAD.h
gad_dst3_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3_adv_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
gad_dst3_adv_y.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst3_adv_y.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst3_adv_y.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3_adv_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3_adv_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_dst3_adv_y.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_dst3_adv_y.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_dst3_adv_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_dst3_adv_y.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_dst3_adv_y.f: FCORI_MACROS.h GAD.h
gad_dst3fl_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3fl_adv_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
gad_dst3fl_adv_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst3fl_adv_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst3fl_adv_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3fl_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3fl_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3fl_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst3fl_adv_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst3fl_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst3fl_adv_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst3fl_adv_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst3fl_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3fl_adv_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
gad_dst3fl_adv_x.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst3fl_adv_x.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst3fl_adv_x.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3fl_adv_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3fl_adv_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3fl_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst3fl_adv_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst3fl_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst3fl_adv_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst3fl_adv_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst3fl_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3fl_adv_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
gad_dst3fl_adv_y.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_dst3fl_adv_y.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_dst3fl_adv_y.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_dst3fl_adv_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_dst3fl_adv_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_dst3fl_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst3fl_adv_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst3fl_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst3fl_adv_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst3fl_adv_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst3fl_impl_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_dst3fl_impl_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
gad_dst3fl_impl_r.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_dst3fl_impl_r.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_dst3fl_impl_r.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_dst3fl_impl_r.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_dst3fl_impl_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_dst3fl_impl_r.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst3fl_impl_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst3fl_impl_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst3fl_impl_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst3fl_impl_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h EEPARAMS.h
gad_dst3fl_impl_r.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GAD.h
gad_fluxlimit_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_fluxlimit_adv_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
gad_fluxlimit_adv_r.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_fluxlimit_adv_r.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_fluxlimit_adv_r.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_fluxlimit_adv_r.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_fluxlimit_adv_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_fluxlimit_adv_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_fluxlimit_adv_r.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_fluxlimit_adv_r.f: FCORI_MACROS.h GAD_FLUX_LIMITER.h
gad_fluxlimit_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_fluxlimit_adv_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
gad_fluxlimit_adv_x.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_fluxlimit_adv_x.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_fluxlimit_adv_x.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_fluxlimit_adv_x.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_fluxlimit_adv_x.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_fluxlimit_adv_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_fluxlimit_adv_x.f: TANPHIATV_MACROS.h GAD_FLUX_LIMITER.h
gad_fluxlimit_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_fluxlimit_adv_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
gad_fluxlimit_adv_y.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_fluxlimit_adv_y.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_fluxlimit_adv_y.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_fluxlimit_adv_y.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_fluxlimit_adv_y.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_fluxlimit_adv_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_fluxlimit_adv_y.f: TANPHIATV_MACROS.h GAD_FLUX_LIMITER.h
gad_fluxlimit_impl_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_fluxlimit_impl_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
gad_fluxlimit_impl_r.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_fluxlimit_impl_r.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_fluxlimit_impl_r.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_fluxlimit_impl_r.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_fluxlimit_impl_r.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_fluxlimit_impl_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_fluxlimit_impl_r.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_fluxlimit_impl_r.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_fluxlimit_impl_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_fluxlimit_impl_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_fluxlimit_impl_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_fluxlimit_impl_r.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h
gad_fluxlimit_impl_r.f: PARAMS_MACROS.h FCORI_MACROS.h GAD_FLUX_LIMITER.h
gad_grad_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_grad_x.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h GRID.h
gad_grad_x.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_grad_x.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_grad_x.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_grad_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_grad_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_grad_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_grad_x.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_grad_x.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_grad_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_grad_x.f: TANPHIATV_MACROS.h
gad_grad_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_grad_y.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h GRID.h
gad_grad_y.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_grad_y.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_grad_y.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_grad_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_grad_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_grad_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_grad_y.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_grad_y.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_grad_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_grad_y.f: TANPHIATV_MACROS.h
gad_implicit_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_implicit_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
gad_implicit_r.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
gad_implicit_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_implicit_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_implicit_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_implicit_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_implicit_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_implicit_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_implicit_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_implicit_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_implicit_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_implicit_r.f: TANPHIATV_MACROS.h GAD.h
gad_init.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_init.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
gad_init.f: PARAMS_MACROS.h FCORI_MACROS.h GAD.h
gad_u3_adv_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_u3_adv_r.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_u3_adv_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_u3_adv_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_u3_adv_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_u3_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_u3_adv_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_u3_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_u3_adv_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_u3_adv_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_u3_adv_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_u3_adv_r.f: TANPHIATV_MACROS.h GAD.h
gad_u3_adv_x.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_u3_adv_x.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_u3_adv_x.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_u3_adv_x.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_u3_adv_x.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_u3_adv_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_u3_adv_x.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_u3_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_u3_adv_x.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_u3_adv_x.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_u3_adv_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_u3_adv_x.f: TANPHIATV_MACROS.h GAD.h
gad_u3_adv_y.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_u3_adv_y.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
gad_u3_adv_y.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_u3_adv_y.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_u3_adv_y.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_u3_adv_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_u3_adv_y.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_u3_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_u3_adv_y.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_u3_adv_y.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_u3_adv_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_u3_adv_y.f: TANPHIATV_MACROS.h GAD.h
gad_u3c4_impl_r.f: GAD_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gad_u3c4_impl_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
gad_u3c4_impl_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_u3c4_impl_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_u3c4_impl_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_u3c4_impl_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_u3c4_impl_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_u3c4_impl_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_u3c4_impl_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_u3c4_impl_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_u3c4_impl_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_u3c4_impl_r.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_u3c4_impl_r.f: FCORI_MACROS.h GAD.h
dyn2phys.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
gridalt_diagnostics_init.f: GRIDALT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
gridalt_diagnostics_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
gridalt_diagnostics_init.f: EEPARAMS.h
gridalt_initialise.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gridalt_initialise.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
gridalt_initialise.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
gridalt_initialise.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gridalt_initialise.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gridalt_initialise.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gridalt_initialise.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gridalt_initialise.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gridalt_initialise.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gridalt_initialise.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gridalt_initialise.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gridalt_initialise.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
gridalt_initialise.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
gridalt_initialise.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gridalt_initialise.f: fizhi_SIZE.h gridalt_mapping.h SURFACE.h
gridalt_update.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gridalt_update.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
gridalt_update.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
gridalt_update.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gridalt_update.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gridalt_update.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gridalt_update.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gridalt_update.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gridalt_update.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gridalt_update.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gridalt_update.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gridalt_update.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gridalt_update.f: TANPHIATV_MACROS.h fizhi_SIZE.h gridalt_mapping.h SURFACE.h
make_phys_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
phys2dyn.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
mdsio_gl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_gl.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
mdsio_gl.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_gl_slice.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_gl_slice.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_gl_slice.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_passtorl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_passtorl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
mdsio_passtorl.f: SIZE.h
mdsio_passtors.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_passtors.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
mdsio_passtors.f: SIZE.h
mdsio_read_rl_vec.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_read_rl_vec.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
mdsio_read_rl_vec.f: SIZE.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_read_rs_vec.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_read_rs_vec.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
mdsio_read_rs_vec.f: SIZE.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_readfield.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_readfield.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_readfield.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_readfield.f: EESUPPORT.h mpif.h W2_EXCH2_TOPOLOGY.h W2_EXCH2_PARAMS.h
mdsio_readfield.f: MDSIO_SCPU.h
mdsio_readfield_loc.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_readfield_loc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_readfield_loc.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_readtile.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_readtile.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_readtile.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_readvector.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_readvector.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_readvector.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_readvector.f: EESUPPORT.h mpif.h
mdsio_seg4torl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_seg4torl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_seg4tors.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_seg4tors.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_seg8torl.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_seg8torl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_seg8tors.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_seg8tors.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_slice.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_slice.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mdsio_slice.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_slice_loc.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_slice_loc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_slice_loc.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_write_rl_vec.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_write_rl_vec.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
mdsio_write_rl_vec.f: SIZE.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_write_rs_vec.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_write_rs_vec.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
mdsio_write_rs_vec.f: SIZE.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_writefield.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_writefield.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_writefield_loc.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_writefield_loc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_writefield_new.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_writefield_new.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_writefield_new.f: EEPARAMS.h EESUPPORT.h mpif.h PARAMS.h
mdsio_writefield_new.f: PARAMS_MACROS.h FCORI_MACROS.h W2_EXCH2_TOPOLOGY.h
mdsio_writefield_new.f: W2_EXCH2_PARAMS.h
mdsio_writelocal.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_writelocal.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_writelocal.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_writelocal.f: W2_EXCH2_TOPOLOGY.h W2_EXCH2_PARAMS.h
mdsio_writemeta.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_writemeta.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_writemeta.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_writetile.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_writetile.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_writetile.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_writevec_loc.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_writevec_loc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_writevec_loc.f: EEPARAMS.h EESUPPORT.h mpif.h PARAMS.h PARAMS_MACROS.h
mdsio_writevec_loc.f: FCORI_MACROS.h
mdsio_writevector.f: MDSIO_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mdsio_writevector.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mdsio_writevector.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_writevector.f: EESUPPORT.h mpif.h
MNC_CW_READWRITE_I.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
MNC_CW_READWRITE_I.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
MNC_CW_READWRITE_I.f: mnc_id_header.h
MNC_CW_READWRITE_I.f: /u/edhill/pkg/netcdf-3.6.0/ifort/include/netcdf.inc
MNC_CW_READWRITE_I.f: mnc_common.h MNC_SIZE.h SIZE.h MNC_BUFF.h EEPARAMS.h
MNC_CW_READWRITE_I.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h MNC_PARAMS.h
MNC_CW_READWRITE_RL.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
MNC_CW_READWRITE_RL.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
MNC_CW_READWRITE_RL.f: mnc_id_header.h
MNC_CW_READWRITE_RL.f: /u/edhill/pkg/netcdf-3.6.0/ifort/include/netcdf.inc
MNC_CW_READWRITE_RL.f: mnc_common.h MNC_SIZE.h SIZE.h MNC_BUFF.h EEPARAMS.h
MNC_CW_READWRITE_RL.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h MNC_PARAMS.h
MNC_CW_READWRITE_RS.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
MNC_CW_READWRITE_RS.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
MNC_CW_READWRITE_RS.f: mnc_id_header.h
MNC_CW_READWRITE_RS.f: /u/edhill/pkg/netcdf-3.6.0/ifort/include/netcdf.inc
MNC_CW_READWRITE_RS.f: mnc_common.h MNC_SIZE.h SIZE.h MNC_BUFF.h EEPARAMS.h
MNC_CW_READWRITE_RS.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h MNC_PARAMS.h
mnc_cw_citer.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_cw_citer.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h mnc_common.h
mnc_cw_citer.f: MNC_SIZE.h EEPARAMS.h
mnc_cw_cvars.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_cw_cvars.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h
mnc_cw_cvars.f: /u/edhill/pkg/netcdf-3.6.0/ifort/include/netcdf.inc
mnc_cw_cvars.f: mnc_common.h MNC_SIZE.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
mnc_cw_cvars.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mnc_cw_cvars.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mnc_cw_cvars.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mnc_cw_cvars.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mnc_cw_cvars.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mnc_cw_cvars.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mnc_cw_cvars.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mnc_cw_cvars.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mnc_cw_cvars.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mnc_cw_cvars.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mnc_cw_cvars.f: TANPHIATV_MACROS.h W2_EXCH2_TOPOLOGY.h W2_EXCH2_PARAMS.h
mnc_cw_init.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_cw_init.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h mnc_common.h
mnc_cw_init.f: MNC_SIZE.h EEPARAMS.h
mnc_cw_missingvals.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mnc_cw_missingvals.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
mnc_cw_missingvals.f: mnc_id_header.h mnc_common.h MNC_SIZE.h EEPARAMS.h
mnc_cw_model_attr.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mnc_cw_model_attr.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
mnc_cw_model_attr.f: mnc_id_header.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
mnc_cw_model_attr.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h MNC_PARAMS.h
mnc_cw_model_attr.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mnc_cw_model_attr.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mnc_cw_model_attr.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mnc_cw_model_attr.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mnc_cw_model_attr.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mnc_cw_model_attr.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mnc_cw_model_attr.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mnc_cw_model_attr.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mnc_cw_model_attr.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mnc_cw_model_attr.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mnc_cw_model_attr.f: W2_EXCH2_TOPOLOGY.h W2_EXCH2_PARAMS.h
mnc_cw_udim.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_cw_udim.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h mnc_common.h
mnc_cw_udim.f: MNC_SIZE.h EEPARAMS.h
mnc_cw_write_grid_info.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mnc_cw_write_grid_info.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
mnc_cw_write_grid_info.f: mnc_id_header.h mnc_common.h MNC_SIZE.h SIZE.h
mnc_cw_write_grid_info.f: EEPARAMS.h EESUPPORT.h mpif.h PARAMS.h
mnc_cw_write_grid_info.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mnc_cw_write_grid_info.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mnc_cw_write_grid_info.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mnc_cw_write_grid_info.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mnc_cw_write_grid_info.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mnc_cw_write_grid_info.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mnc_cw_write_grid_info.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mnc_cw_write_grid_info.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mnc_cw_write_grid_info.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mnc_cw_write_grid_info.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mnc_cw_write_grid_info.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mnc_cw_write_grid_info.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mnc_cw_write_grid_info.f: TANPHIATV_MACROS.h
mnc_cwrapper.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_cwrapper.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h mnc_common.h
mnc_cwrapper.f: MNC_SIZE.h EEPARAMS.h SIZE.h PARAMS.h PARAMS_MACROS.h
mnc_cwrapper.f: FCORI_MACROS.h W2_EXCH2_TOPOLOGY.h W2_EXCH2_PARAMS.h
mnc_cwrapper.f: /u/edhill/pkg/netcdf-3.6.0/ifort/include/netcdf.inc
mnc_dim.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_dim.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h
mnc_dim.f: /u/edhill/pkg/netcdf-3.6.0/ifort/include/netcdf.inc mnc_common.h
mnc_dim.f: MNC_SIZE.h EEPARAMS.h
mnc_dump.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_dump.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h mnc_common.h
mnc_dump.f: MNC_SIZE.h
mnc_file.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_file.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h
mnc_file.f: /u/edhill/pkg/netcdf-3.6.0/ifort/include/netcdf.inc mnc_common.h
mnc_file.f: MNC_SIZE.h EEPARAMS.h
mnc_grid.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_grid.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h
mnc_grid.f: /u/edhill/pkg/netcdf-3.6.0/ifort/include/netcdf.inc mnc_common.h
mnc_grid.f: MNC_SIZE.h EEPARAMS.h
mnc_init.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_init.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h mnc_common.h
mnc_init.f: MNC_SIZE.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h PARAMS.h
mnc_init.f: PARAMS_MACROS.h FCORI_MACROS.h
mnc_readparms.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mnc_readparms.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h
mnc_readparms.f: SIZE.h mnc_common.h MNC_SIZE.h EEPARAMS.h PARAMS.h
mnc_readparms.f: PARAMS_MACROS.h FCORI_MACROS.h MNC_PARAMS.h
mnc_utils.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_utils.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h
mnc_utils.f: /u/edhill/pkg/netcdf-3.6.0/ifort/include/netcdf.inc EEPARAMS.h
mnc_utils.f: mnc_common.h MNC_SIZE.h
mnc_var.f: MNC_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mnc_var.f: CPP_EEMACROS.h BUILD_INFO.h mnc_id_header.h
mnc_var.f: /u/edhill/pkg/netcdf-3.6.0/ifort/include/netcdf.inc mnc_common.h
mnc_var.f: MNC_SIZE.h EEPARAMS.h
mom_calc_absvort3.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_absvort3.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_calc_absvort3.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_calc_absvort3.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_calc_absvort3.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_calc_absvort3.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_calc_absvort3.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_calc_absvort3.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_calc_absvort3.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_calc_absvort3.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_calc_absvort3.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_calc_absvort3.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_calc_absvort3.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_calc_hdiv.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_hdiv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_calc_hdiv.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_calc_hdiv.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_calc_hdiv.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_calc_hdiv.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_hdiv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_hdiv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_hdiv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_calc_hdiv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_calc_hdiv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_calc_hdiv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_calc_hdiv.f: TANPHIATV_MACROS.h
mom_calc_hfacz.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_hfacz.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
mom_calc_hfacz.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_calc_hfacz.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_calc_hfacz.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_hfacz.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_hfacz.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_hfacz.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_calc_hfacz.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_calc_hfacz.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_calc_hfacz.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_calc_hfacz.f: TANPHIATV_MACROS.h
mom_calc_ke.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_ke.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
mom_calc_ke.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_calc_ke.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_calc_ke.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_ke.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_ke.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_ke.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_calc_ke.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_calc_ke.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_calc_ke.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_calc_ke.f: TANPHIATV_MACROS.h
mom_calc_relvort3.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_relvort3.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_calc_relvort3.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_calc_relvort3.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_calc_relvort3.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_calc_relvort3.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_calc_relvort3.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_calc_relvort3.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_calc_relvort3.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_calc_relvort3.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_calc_relvort3.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_calc_relvort3.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_calc_relvort3.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_calc_relvort3.f: W2_EXCH2_TOPOLOGY.h W2_EXCH2_PARAMS.h
mom_calc_strain.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_strain.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_calc_strain.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_calc_strain.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_calc_strain.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_calc_strain.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_strain.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_strain.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_strain.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_calc_strain.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_calc_strain.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_calc_strain.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_calc_strain.f: TANPHIATV_MACROS.h
mom_calc_tension.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_tension.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_calc_tension.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_calc_tension.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_calc_tension.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_calc_tension.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_tension.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_tension.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_tension.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_calc_tension.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_calc_tension.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_calc_tension.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_calc_tension.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_calc_visc.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_calc_visc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
mom_calc_visc.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_calc_visc.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_calc_visc.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_calc_visc.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_calc_visc.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_calc_visc.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_calc_visc.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_calc_visc.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_calc_visc.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_calc_visc.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mom_calc_visc.f: FCORI_MACROS.h
mom_diagnostics_init.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_diagnostics_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_diagnostics_init.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_hdissip.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_hdissip.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mom_hdissip.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_hdissip.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_hdissip.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_hdissip.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_hdissip.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_hdissip.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_hdissip.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_hdissip.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_hdissip.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_hdissip.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_init_fixed.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_init_fixed.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_init_fixed.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_init_fixed.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_init_fixed.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_init_fixed.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_init_fixed.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_init_fixed.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_init_fixed.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_init_fixed.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_init_fixed.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_init_fixed.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_init_fixed.f: TANPHIATV_MACROS.h SURFACE.h
mom_u_bottomdrag.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_bottomdrag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_u_bottomdrag.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_u_bottomdrag.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_u_bottomdrag.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_u_bottomdrag.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_bottomdrag.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_bottomdrag.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_bottomdrag.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_u_bottomdrag.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_u_bottomdrag.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_u_bottomdrag.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_u_bottomdrag.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_u_implicit_r.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_implicit_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_u_implicit_r.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_u_implicit_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_u_implicit_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_u_implicit_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_implicit_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_implicit_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_implicit_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_u_implicit_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_u_implicit_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_u_implicit_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_u_implicit_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
mom_u_rviscflux.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_rviscflux.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_u_rviscflux.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_u_rviscflux.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_u_rviscflux.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_u_rviscflux.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_rviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_rviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_rviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_rviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_rviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_rviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_rviscflux.f: TANPHIATV_MACROS.h
mom_u_sidedrag.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_u_sidedrag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_u_sidedrag.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_u_sidedrag.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_u_sidedrag.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_u_sidedrag.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_sidedrag.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_sidedrag.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_sidedrag.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_sidedrag.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_sidedrag.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_sidedrag.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_sidedrag.f: TANPHIATV_MACROS.h SURFACE.h
mom_v_bottomdrag.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_bottomdrag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_v_bottomdrag.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_v_bottomdrag.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_v_bottomdrag.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_v_bottomdrag.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_bottomdrag.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_bottomdrag.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_bottomdrag.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_v_bottomdrag.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_v_bottomdrag.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_v_bottomdrag.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_v_bottomdrag.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_v_implicit_r.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_implicit_r.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_v_implicit_r.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_v_implicit_r.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_v_implicit_r.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_v_implicit_r.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_implicit_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_implicit_r.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_implicit_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_v_implicit_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_v_implicit_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_v_implicit_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_v_implicit_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
mom_v_rviscflux.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_rviscflux.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_v_rviscflux.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_v_rviscflux.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_v_rviscflux.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_v_rviscflux.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_rviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_rviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_rviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_rviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_rviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_rviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_rviscflux.f: TANPHIATV_MACROS.h
mom_v_sidedrag.f: MOM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_v_sidedrag.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_v_sidedrag.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_v_sidedrag.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_v_sidedrag.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_v_sidedrag.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_sidedrag.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_sidedrag.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_sidedrag.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_sidedrag.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_sidedrag.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_sidedrag.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_sidedrag.f: TANPHIATV_MACROS.h SURFACE.h
mom_vecinv.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vecinv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h DYNVARS.h
mom_vecinv.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h MNC_PARAMS.h
mom_vecinv.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vecinv.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vecinv.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vecinv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vecinv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vecinv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vecinv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vecinv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vecinv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vecinv.f: TANPHIATV_MACROS.h TIMEAVE_STATV.h TIMEAVE_OPTIONS.h
mom_vi_coriolis.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_coriolis.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_vi_coriolis.f: EEPARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_vi_coriolis.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_vi_coriolis.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_coriolis.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_coriolis.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_vi_coriolis.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_coriolis.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_coriolis.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_vi_coriolis.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_vi_coriolis.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h PARAMS.h
mom_vi_coriolis.f: PARAMS_MACROS.h FCORI_MACROS.h
mom_vi_del2uv.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_del2uv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
mom_vi_del2uv.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_del2uv.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_del2uv.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vi_del2uv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_del2uv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_del2uv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_del2uv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_del2uv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_del2uv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_del2uv.f: TANPHIATV_MACROS.h EEPARAMS.h
mom_vi_hdissip.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_hdissip.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h
mom_vi_hdissip.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_hdissip.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_hdissip.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vi_hdissip.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_hdissip.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_hdissip.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_hdissip.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_hdissip.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_hdissip.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_hdissip.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mom_vi_hdissip.f: FCORI_MACROS.h
mom_vi_u_coriolis.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_u_coriolis.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_vi_u_coriolis.f: EEPARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h
mom_vi_u_coriolis.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
mom_vi_u_coriolis.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_vi_u_coriolis.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_vi_u_coriolis.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_vi_u_coriolis.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_vi_u_coriolis.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_u_coriolis.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_u_coriolis.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_u_coriolis.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_u_coriolis.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h
mom_vi_u_coriolis.f: FCORI_MACROS.h
mom_vi_u_coriolis_c4.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_u_coriolis_c4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_vi_u_coriolis_c4.f: EEPARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h
mom_vi_u_coriolis_c4.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
mom_vi_u_coriolis_c4.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_vi_u_coriolis_c4.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_vi_u_coriolis_c4.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_u_coriolis_c4.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_vi_u_coriolis_c4.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_vi_u_coriolis_c4.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_u_coriolis_c4.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_u_coriolis_c4.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_u_coriolis_c4.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_u_coriolis_c4.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h
mom_vi_u_coriolis_c4.f: FCORI_MACROS.h
mom_vi_u_grad_ke.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_u_grad_ke.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_vi_u_grad_ke.f: EEPARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_vi_u_grad_ke.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_vi_u_grad_ke.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_u_grad_ke.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_u_grad_ke.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_vi_u_grad_ke.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_u_grad_ke.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_u_grad_ke.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_vi_u_grad_ke.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_vi_u_grad_ke.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_u_vertshear.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_u_vertshear.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_vi_u_vertshear.f: EEPARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h
mom_vi_u_vertshear.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
mom_vi_u_vertshear.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_vi_u_vertshear.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_vi_u_vertshear.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_u_vertshear.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_vi_u_vertshear.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_vi_u_vertshear.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_u_vertshear.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_u_vertshear.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_u_vertshear.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_u_vertshear.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h
mom_vi_u_vertshear.f: FCORI_MACROS.h
mom_vi_v_coriolis.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_v_coriolis.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_vi_v_coriolis.f: EEPARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h
mom_vi_v_coriolis.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
mom_vi_v_coriolis.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_vi_v_coriolis.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_vi_v_coriolis.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_vi_v_coriolis.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_vi_v_coriolis.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_v_coriolis.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_v_coriolis.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_v_coriolis.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_v_coriolis.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h
mom_vi_v_coriolis.f: FCORI_MACROS.h
mom_vi_v_coriolis_c4.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_v_coriolis_c4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_vi_v_coriolis_c4.f: EEPARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h
mom_vi_v_coriolis_c4.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
mom_vi_v_coriolis_c4.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_vi_v_coriolis_c4.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_vi_v_coriolis_c4.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_v_coriolis_c4.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_vi_v_coriolis_c4.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_vi_v_coriolis_c4.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_v_coriolis_c4.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_v_coriolis_c4.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_v_coriolis_c4.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_v_coriolis_c4.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h
mom_vi_v_coriolis_c4.f: FCORI_MACROS.h
mom_vi_v_grad_ke.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_v_grad_ke.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_vi_v_grad_ke.f: EEPARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_vi_v_grad_ke.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_vi_v_grad_ke.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_v_grad_ke.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_v_grad_ke.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_vi_v_grad_ke.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_v_grad_ke.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_v_grad_ke.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_vi_v_grad_ke.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_vi_v_grad_ke.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_v_vertshear.f: MOM_VECINV_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mom_vi_v_vertshear.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mom_vi_v_vertshear.f: EEPARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h
mom_vi_v_vertshear.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
mom_vi_v_vertshear.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_vi_v_vertshear.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_vi_v_vertshear.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_v_vertshear.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_vi_v_vertshear.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_vi_v_vertshear.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_v_vertshear.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_v_vertshear.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_v_vertshear.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_v_vertshear.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h
mom_vi_v_vertshear.f: FCORI_MACROS.h
mon_advcfl.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_advcfl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mon_advcfl.f: MONITOR.h
mon_advcflw.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_advcflw.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mon_advcflw.f: MONITOR.h
mon_advcflw2.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_advcflw2.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mon_advcflw2.f: MONITOR.h
mon_init.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mon_init.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
mon_init.f: PARAMS_MACROS.h FCORI_MACROS.h MONITOR.h
mon_ke.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mon_ke.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h DYNVARS.h MONITOR.h
mon_ke.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mon_ke.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mon_ke.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mon_ke.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mon_ke.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mon_ke.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mon_ke.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mon_ke.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mon_ke.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
mon_out.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mon_out.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
mon_out.f: PARAMS_MACROS.h FCORI_MACROS.h EESUPPORT.h mpif.h MONITOR.h
mon_printstats_rl.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_printstats_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mon_printstats_rl.f: EEPARAMS.h MONITOR.h
mon_printstats_rs.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_printstats_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mon_printstats_rs.f: EEPARAMS.h MONITOR.h
mon_set_iounit.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_set_iounit.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mon_set_iounit.f: EEPARAMS.h MONITOR.h
mon_set_pref.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_set_pref.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mon_set_pref.f: MONITOR.h
mon_solution.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_solution.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mon_solution.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
mon_solution.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mon_solution.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mon_solution.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mon_solution.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mon_solution.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mon_solution.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mon_solution.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mon_solution.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mon_solution.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mon_solution.f: TANPHIATV_MACROS.h MONITOR.h
mon_stats_latbnd_rl.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_stats_latbnd_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mon_stats_latbnd_rl.f: EEPARAMS.h
mon_stats_rl.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_stats_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mon_stats_rs.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_stats_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mon_surfcor.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_surfcor.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mon_surfcor.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h SURFACE.h
mon_surfcor.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mon_surfcor.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mon_surfcor.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mon_surfcor.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mon_surfcor.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mon_surfcor.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mon_surfcor.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mon_surfcor.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mon_surfcor.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mon_surfcor.f: TANPHIATV_MACROS.h MONITOR.h
mon_vort3.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_vort3.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mon_vort3.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h MONITOR.h
mon_vort3.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mon_vort3.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mon_vort3.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mon_vort3.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mon_vort3.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mon_vort3.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mon_vort3.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mon_vort3.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mon_vort3.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h W2_EXCH2_TOPOLOGY.h
mon_vort3.f: W2_EXCH2_PARAMS.h
mon_writestats_rl.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
mon_writestats_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
mon_writestats_rl.f: EEPARAMS.h MONITOR.h
monitor.f: MONITOR_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
monitor.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
monitor.f: PARAMS_MACROS.h FCORI_MACROS.h MNC_PARAMS.h EESUPPORT.h mpif.h
monitor.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
monitor.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
monitor.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
monitor.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
monitor.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
monitor.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
monitor.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
monitor.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
monitor.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h MONITOR.h
get_write_global_fld.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
read_fld_xy_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
read_fld_xy_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
read_fld_xy_rl.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
read_fld_xy_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
read_fld_xy_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
read_fld_xy_rs.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
read_fld_xyz_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
read_fld_xyz_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
read_fld_xyz_rl.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
read_fld_xyz_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
read_fld_xyz_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
read_fld_xyz_rs.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
read_write_rec.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
read_write_rec.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
read_write_rec.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
set_write_global_fld.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
write_fld_xy_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_xy_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
write_fld_xy_rl.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_fld_xy_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_xy_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
write_fld_xy_rs.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_fld_xyz_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_xyz_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
write_fld_xyz_rl.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_fld_xyz_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_fld_xyz_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
write_fld_xyz_rs.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_local_rl.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_local_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
write_local_rl.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_local_rs.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
write_local_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
write_local_rs.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_myfield.f: RW_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
write_myfield.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
write_myfield.f: PARAMS_MACROS.h FCORI_MACROS.h
shap_filt_apply_ts.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_apply_ts.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_apply_ts.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
shap_filt_apply_ts.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h
shap_filt_apply_ts.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
shap_filt_apply_ts.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
shap_filt_apply_ts.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
shap_filt_apply_ts.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_apply_ts.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
shap_filt_apply_ts.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
shap_filt_apply_ts.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
shap_filt_apply_ts.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
shap_filt_apply_ts.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
shap_filt_apply_ts.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
shap_filt_apply_ts.f: TANPHIATV_MACROS.h SHAP_FILT.h
shap_filt_apply_uv.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_apply_uv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_apply_uv.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
shap_filt_apply_uv.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
shap_filt_apply_uv.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
shap_filt_apply_uv.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
shap_filt_apply_uv.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
shap_filt_apply_uv.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
shap_filt_apply_uv.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
shap_filt_apply_uv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
shap_filt_apply_uv.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
shap_filt_apply_uv.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
shap_filt_apply_uv.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
shap_filt_apply_uv.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
shap_filt_apply_uv.f: SHAP_FILT.h
shap_filt_computvort.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_computvort.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_computvort.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
shap_filt_computvort.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
shap_filt_computvort.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
shap_filt_computvort.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
shap_filt_computvort.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
shap_filt_computvort.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_computvort.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
shap_filt_computvort.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
shap_filt_computvort.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
shap_filt_computvort.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
shap_filt_computvort.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
shap_filt_computvort.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
shap_filt_computvort.f: TANPHIATV_MACROS.h W2_EXCH2_TOPOLOGY.h
shap_filt_computvort.f: W2_EXCH2_PARAMS.h
shap_filt_diagnostics_init.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h
shap_filt_diagnostics_init.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
shap_filt_diagnostics_init.f: BUILD_INFO.h EEPARAMS.h
shap_filt_init_fixed.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_init_fixed.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
shap_filt_init_fixed.f: EEPARAMS.h SIZE.h PARAMS.h PARAMS_MACROS.h
shap_filt_init_fixed.f: FCORI_MACROS.h
shap_filt_readparms.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_readparms.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_readparms.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
shap_filt_readparms.f: SHAP_FILT.h
shap_filt_relvort3.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_relvort3.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_relvort3.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
shap_filt_relvort3.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
shap_filt_relvort3.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
shap_filt_relvort3.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
shap_filt_relvort3.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
shap_filt_relvort3.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
shap_filt_relvort3.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
shap_filt_relvort3.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
shap_filt_relvort3.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
shap_filt_relvort3.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
shap_filt_relvort3.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
shap_filt_relvort3.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
shap_filt_relvort3.f: W2_EXCH2_TOPOLOGY.h W2_EXCH2_PARAMS.h
shap_filt_tracer_s1.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_tracer_s1.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_tracer_s1.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
shap_filt_tracer_s1.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
shap_filt_tracer_s1.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
shap_filt_tracer_s1.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
shap_filt_tracer_s1.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
shap_filt_tracer_s1.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_tracer_s1.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
shap_filt_tracer_s1.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
shap_filt_tracer_s1.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
shap_filt_tracer_s1.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
shap_filt_tracer_s1.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
shap_filt_tracer_s1.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
shap_filt_tracer_s1.f: TANPHIATV_MACROS.h SHAP_FILT.h
shap_filt_tracer_s2.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_tracer_s2.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_tracer_s2.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
shap_filt_tracer_s2.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
shap_filt_tracer_s2.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
shap_filt_tracer_s2.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
shap_filt_tracer_s2.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
shap_filt_tracer_s2.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_tracer_s2.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
shap_filt_tracer_s2.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
shap_filt_tracer_s2.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
shap_filt_tracer_s2.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
shap_filt_tracer_s2.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
shap_filt_tracer_s2.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
shap_filt_tracer_s2.f: TANPHIATV_MACROS.h SHAP_FILT.h
shap_filt_tracer_s4.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_tracer_s4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_tracer_s4.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
shap_filt_tracer_s4.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
shap_filt_tracer_s4.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
shap_filt_tracer_s4.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
shap_filt_tracer_s4.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
shap_filt_tracer_s4.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_tracer_s4.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
shap_filt_tracer_s4.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
shap_filt_tracer_s4.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
shap_filt_tracer_s4.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
shap_filt_tracer_s4.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
shap_filt_tracer_s4.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
shap_filt_tracer_s4.f: TANPHIATV_MACROS.h SHAP_FILT.h
shap_filt_tracerold.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_tracerold.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_tracerold.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
shap_filt_tracerold.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
shap_filt_tracerold.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
shap_filt_tracerold.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
shap_filt_tracerold.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
shap_filt_tracerold.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_tracerold.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
shap_filt_tracerold.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
shap_filt_tracerold.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
shap_filt_tracerold.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
shap_filt_tracerold.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
shap_filt_tracerold.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
shap_filt_tracerold.f: TANPHIATV_MACROS.h SHAP_FILT.h
shap_filt_u.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_u.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
shap_filt_u.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
shap_filt_u.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
shap_filt_u.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
shap_filt_u.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
shap_filt_u.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_u.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
shap_filt_u.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
shap_filt_u.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
shap_filt_u.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
shap_filt_u.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
shap_filt_u.f: TANPHIATV_MACROS.h SHAP_FILT.h
shap_filt_uv_s1.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_uv_s1.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_uv_s1.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
shap_filt_uv_s1.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
shap_filt_uv_s1.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
shap_filt_uv_s1.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
shap_filt_uv_s1.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_uv_s1.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
shap_filt_uv_s1.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
shap_filt_uv_s1.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
shap_filt_uv_s1.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
shap_filt_uv_s1.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
shap_filt_uv_s1.f: TANPHIATV_MACROS.h SHAP_FILT.h
shap_filt_uv_s2.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_uv_s2.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_uv_s2.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
shap_filt_uv_s2.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
shap_filt_uv_s2.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
shap_filt_uv_s2.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
shap_filt_uv_s2.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_uv_s2.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
shap_filt_uv_s2.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
shap_filt_uv_s2.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
shap_filt_uv_s2.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
shap_filt_uv_s2.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
shap_filt_uv_s2.f: TANPHIATV_MACROS.h SHAP_FILT.h W2_EXCH2_TOPOLOGY.h
shap_filt_uv_s2.f: W2_EXCH2_PARAMS.h
shap_filt_uv_s2c.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_uv_s2c.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_uv_s2c.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
shap_filt_uv_s2c.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
shap_filt_uv_s2c.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
shap_filt_uv_s2c.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
shap_filt_uv_s2c.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_uv_s2c.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
shap_filt_uv_s2c.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
shap_filt_uv_s2c.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
shap_filt_uv_s2c.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
shap_filt_uv_s2c.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
shap_filt_uv_s2c.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SHAP_FILT.h
shap_filt_uv_s4.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_uv_s4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
shap_filt_uv_s4.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
shap_filt_uv_s4.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
shap_filt_uv_s4.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
shap_filt_uv_s4.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
shap_filt_uv_s4.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_uv_s4.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
shap_filt_uv_s4.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
shap_filt_uv_s4.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
shap_filt_uv_s4.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
shap_filt_uv_s4.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
shap_filt_uv_s4.f: TANPHIATV_MACROS.h SHAP_FILT.h
shap_filt_v.f: SHAP_FILT_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
shap_filt_v.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
shap_filt_v.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
shap_filt_v.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
shap_filt_v.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
shap_filt_v.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
shap_filt_v.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
shap_filt_v.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
shap_filt_v.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
shap_filt_v.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
shap_filt_v.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
shap_filt_v.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
shap_filt_v.f: TANPHIATV_MACROS.h SHAP_FILT.h
timeave_cumul_1k.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_cumul_1k.f: EEPARAMS.h
timeave_cumul_1k1t.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_cumul_1k1t.f: EEPARAMS.h
timeave_cumul_1t.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_cumul_1t.f: EEPARAMS.h
timeave_cumul_1vfc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_cumul_1vfc.f: EEPARAMS.h
timeave_cumul_2v.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_cumul_2v.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_cumul_2v.f: EEPARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
timeave_cumul_2v.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
timeave_cumul_2v.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
timeave_cumul_2v.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
timeave_cumul_2v.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
timeave_cumul_2v.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
timeave_cumul_2v.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
timeave_cumul_2v.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
timeave_cumul_2v.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
timeave_cumul_2v.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
timeave_cumul_2vfc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_cumul_2vfc.f: EEPARAMS.h
timeave_cumul_dif_1t.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_cumul_dif_1t.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_cumul_dif_1t.f: EEPARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h
timeave_cumul_dif_1t.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
timeave_cumul_dif_1t.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
timeave_cumul_dif_1t.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
timeave_cumul_dif_1t.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
timeave_cumul_dif_1t.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
timeave_cumul_dif_1t.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
timeave_cumul_dif_1t.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
timeave_cumul_dif_1t.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
timeave_cumul_dif_1t.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
timeave_cumul_dif_1t.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
timeave_cumul_dif_1t.f: TANPHIATV_MACROS.h
timeave_cumul_fc.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_cumul_fc.f: EEPARAMS.h
timeave_cumulate.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_cumulate.f: EEPARAMS.h
timeave_init_fixed.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_init_fixed.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_init_fixed.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
timeave_init_fixed.f: MNC_PARAMS.h
timeave_norma_1k.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_norma_1k.f: EEPARAMS.h
timeave_norma_2v.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_norma_2v.f: EEPARAMS.h
timeave_normaliz.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_normaliz.f: EEPARAMS.h
timeave_reset.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_reset.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_reset.f: EEPARAMS.h
timeave_reset_1k.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_reset_1k.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_reset_1k.f: EEPARAMS.h
timeave_statv_write.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_statv_write.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_statv_write.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
timeave_statv_write.f: MNC_PARAMS.h DYNVARS.h GRID.h GRID_MACROS.h
timeave_statv_write.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
timeave_statv_write.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
timeave_statv_write.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
timeave_statv_write.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
timeave_statv_write.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
timeave_statv_write.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
timeave_statv_write.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
timeave_statv_write.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
timeave_statv_write.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
timeave_statv_write.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
timeave_statv_write.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
timeave_statv_write.f: TIMEAVE_STATV.h
timeave_statvars.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_statvars.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_statvars.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
timeave_statvars.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
timeave_statvars.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
timeave_statvars.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
timeave_statvars.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
timeave_statvars.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
timeave_statvars.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
timeave_statvars.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
timeave_statvars.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
timeave_statvars.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
timeave_statvars.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h TIMEAVE_STATV.h
timeave_surf_flux.f: TIMEAVE_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
timeave_surf_flux.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
timeave_surf_flux.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
timeave_surf_flux.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
timeave_surf_flux.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
timeave_surf_flux.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
timeave_surf_flux.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
timeave_surf_flux.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
timeave_surf_flux.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
timeave_surf_flux.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
timeave_surf_flux.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
timeave_surf_flux.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
timeave_surf_flux.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
timeave_surf_flux.f: SURFACE.h FFIELDS.h TIMEAVE_STATV.h
atm_diagnostics_init.f: ATM_COMMON_OPTIONS.h PACKAGES_CONFIG.h CPP_OPTIONS.h
atm_diagnostics_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
atm_diagnostics_init.f: EEPARAMS.h
bar2.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
bar2.f: EESUPPORT.h mpif.h BAR2.h
barrier.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
barrier.f: EESUPPORT.h mpif.h BARRIER.h
check_threads.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
check_threads.f: EEPARAMS.h EESUPPORT.h mpif.h
comm_stats.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
comm_stats.f: EESUPPORT.h mpif.h EXCH.h BAR2.h
dfile.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
dfile.f: EESUPPORT.h mpif.h DFILE.h
diff_phase_multiple.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
different_multiple.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
eeboot.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h PACKAGES_CONFIG.h
eeboot.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
eeboot_minimal.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
eeboot_minimal.f: EEPARAMS.h EESUPPORT.h mpif.h
eedata_example.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
eedata_example.f: EEPARAMS.h
eedie.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
eedie.f: EESUPPORT.h mpif.h
eeintro_msg.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
eeintro_msg.f: EESUPPORT.h mpif.h
eeset_parms.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
eeset_parms.f: EESUPPORT.h mpif.h
eewrite_eeenv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
eewrite_eeenv.f: EEPARAMS.h EESUPPORT.h mpif.h
exch_cycle_ebl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_cycle_ebl.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_init.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
exch_init.f: EESUPPORT.h mpif.h EXCH.h
exch_jam.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
exch_jam.f: EXCH_JAM.h MPI_INFO.h JAM_INFO.h
exch_r4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
exch_r4.f: EESUPPORT.h mpif.h EXCH.h
exch_r4_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
exch_r4_cube.f: EESUPPORT.h mpif.h EXCH.h
exch_r4_recv_get_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_r4_recv_get_x.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_r4_recv_get_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_r4_recv_get_y.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_r4_send_put_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_r4_send_put_x.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_r4_send_put_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_r4_send_put_y.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_r8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
exch_r8.f: EESUPPORT.h mpif.h EXCH.h
exch_r8_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
exch_r8_cube.f: EESUPPORT.h mpif.h EXCH.h
exch_r8_recv_get_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_r8_recv_get_x.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_r8_recv_get_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_r8_recv_get_y.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_r8_send_put_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_r8_send_put_x.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_r8_send_put_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_r8_send_put_y.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
exch_rl.f: EESUPPORT.h mpif.h EXCH.h
exch_rl_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
exch_rl_cube.f: EESUPPORT.h mpif.h EXCH.h
exch_rl_recv_get_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_rl_recv_get_x.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_rl_recv_get_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_rl_recv_get_y.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_rl_send_put_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_rl_send_put_x.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_rl_send_put_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_rl_send_put_y.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
exch_rs.f: EESUPPORT.h mpif.h EXCH.h
exch_rs_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
exch_rs_cube.f: EESUPPORT.h mpif.h EXCH.h
exch_rs_recv_get_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_rs_recv_get_x.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_rs_recv_get_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_rs_recv_get_y.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_rs_send_put_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_rs_send_put_x.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_rs_send_put_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_rs_send_put_y.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_s3d_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_s3d_r4.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_s3d_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_s3d_r8.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_s3d_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_s3d_rl.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_s3d_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_s3d_rs.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_agrid_xy_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_xy_r4.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
exch_uv_agrid_xy_r4.f: EXCH.h
exch_uv_agrid_xy_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_xy_r8.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
exch_uv_agrid_xy_r8.f: EXCH.h
exch_uv_agrid_xy_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_xy_rl.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
exch_uv_agrid_xy_rl.f: EXCH.h
exch_uv_agrid_xy_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_xy_rs.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
exch_uv_agrid_xy_rs.f: EXCH.h
exch_uv_agrid_xyz_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_xyz_r4.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
exch_uv_agrid_xyz_r4.f: EXCH.h
exch_uv_agrid_xyz_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_xyz_r8.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
exch_uv_agrid_xyz_r8.f: EXCH.h
exch_uv_agrid_xyz_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_xyz_rl.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
exch_uv_agrid_xyz_rl.f: EXCH.h
exch_uv_agrid_xyz_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_agrid_xyz_rs.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
exch_uv_agrid_xyz_rs.f: EXCH.h
exch_uv_r4_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_uv_r4_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_r8_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_uv_r8_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_rl_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_uv_rl_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_rs_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_uv_rs_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_xy_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_xy_r4.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_xy_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_xy_r8.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_xy_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_xy_rl.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_xy_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_xy_rs.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_xyz_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_xyz_r4.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_xyz_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_xyz_r8.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_xyz_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_xyz_rl.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_uv_xyz_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
exch_uv_xyz_rs.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_xy_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_xy_r4.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_xy_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_xy_r8.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_xy_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_xy_rl.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_xy_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_xy_rs.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_xyz_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_xyz_r4.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_xyz_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_xyz_r8.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_xyz_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_xyz_rl.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_xyz_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_xyz_rs.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_r4_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_z_r4_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_r8_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_z_r8_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_rl_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_z_rl_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_rs_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_z_rs_cube.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_xy_r4.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_z_xy_r4.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_xy_r8.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_z_xy_r8.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_xy_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_z_xy_rl.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_xy_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
exch_z_xy_rs.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_xyz_r4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_z_xyz_r4.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_xyz_r8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_z_xyz_r8.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_xyz_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_z_xyz_rl.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
exch_z_xyz_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
exch_z_xyz_rs.f: EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
fill_cs_corner_tr_rl.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
fill_cs_corner_tr_rl.f: BUILD_INFO.h SIZE.h EEPARAMS.h W2_EXCH2_TOPOLOGY.h
fill_cs_corner_tr_rl.f: W2_EXCH2_PARAMS.h
fill_cs_corner_uv_rs.f: PACKAGES_CONFIG.h CPP_EEOPTIONS.h CPP_EEMACROS.h
fill_cs_corner_uv_rs.f: BUILD_INFO.h SIZE.h EEPARAMS.h W2_EXCH2_TOPOLOGY.h
fill_cs_corner_uv_rs.f: W2_EXCH2_PARAMS.h
fool_the_compiler.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
gather_2d.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
gather_2d.f: EESUPPORT.h mpif.h
gather_vector.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
gather_vector.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
global_max.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
global_max.f: EESUPPORT.h mpif.h GLOBAL_MAX.h
global_sum.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
global_sum.f: EESUPPORT.h mpif.h GLOBAL_SUM.h
global_vec_sum.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
global_vec_sum.f: EEPARAMS.h EESUPPORT.h mpif.h GLOBAL_SUM.h
gsum.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
gsum.f: EESUPPORT.h mpif.h
gsum_jam.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
ini_communication_patterns.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
ini_communication_patterns.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h EXCH.h
ini_jam.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
ini_procs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
ini_procs.f: EESUPPORT.h mpif.h
ini_threading_environment.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
ini_threading_environment.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
lef_zero.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
main.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
main.f: BUILD_INFO.h SIZE.h EEPARAMS.h EESUPPORT.h mpif.h PARAMS.h
main.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
main.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
main.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
main.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
main.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
main.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
main.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
main.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
main.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
main.f: DYNVARS.h FFIELDS.h SURFACE.h TIMEAVE_STATV.h TIMEAVE_OPTIONS.h
main.f: SIGREG.h MAIN_PDIRECTIVES1.h MAIN_PDIRECTIVES2.h
mds_byteswapi4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
mds_byteswapr4.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
mds_byteswapr8.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
mds_reclen.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
mds_reclen.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsfindunit.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h EEPARAMS.h
memsync.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
nml_filter.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
nml_filter.f: EEPARAMS.h
nml_set_terminator.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
nml_set_terminator.f: BUILD_INFO.h EEPARAMS.h
print.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
print.f: EESUPPORT.h mpif.h
scatter_2d.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
scatter_2d.f: EESUPPORT.h mpif.h
scatter_vector.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
scatter_vector.f: SIZE.h EEPARAMS.h EESUPPORT.h mpif.h
timers.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
timers.f: EESUPPORT.h mpif.h
utils.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
adams_bashforth2.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
adams_bashforth2.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
adams_bashforth3.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
adams_bashforth3.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
askparam.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
askparam.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
askparam.f: FCORI_MACROS.h
calc_3d_diffusivity.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
calc_3d_diffusivity.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
calc_3d_diffusivity.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
calc_3d_diffusivity.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_3d_diffusivity.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
calc_3d_diffusivity.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
calc_3d_diffusivity.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
calc_3d_diffusivity.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_3d_diffusivity.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_3d_diffusivity.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
calc_3d_diffusivity.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_3d_diffusivity.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
calc_3d_diffusivity.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
calc_3d_diffusivity.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
calc_3d_diffusivity.f: GAD.h GAD_OPTIONS.h
calc_buoyancy.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
calc_buoyancy.f: SIZE.h DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_buoyancy.f: FCORI_MACROS.h
calc_common_factors.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
calc_common_factors.f: BUILD_INFO.h SIZE.h DYNVARS.h EEPARAMS.h PARAMS.h
calc_common_factors.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
calc_common_factors.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
calc_common_factors.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
calc_common_factors.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_common_factors.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
calc_common_factors.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_common_factors.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_common_factors.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
calc_common_factors.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_common_factors.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
calc_common_factors.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
calc_common_factors.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
calc_diffusivity.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
calc_diffusivity.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
calc_diffusivity.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
calc_diffusivity.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_diffusivity.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
calc_diffusivity.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_diffusivity.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_diffusivity.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_diffusivity.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
calc_diffusivity.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_diffusivity.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_diffusivity.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_diffusivity.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
calc_div_ghat.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
calc_div_ghat.f: SIZE.h DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_div_ghat.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
calc_div_ghat.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
calc_div_ghat.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
calc_div_ghat.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_div_ghat.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_div_ghat.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_div_ghat.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_div_ghat.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_div_ghat.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_div_ghat.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SOLVE_FOR_PRESSURE3D.h
calc_grad_phi_hyd.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
calc_grad_phi_hyd.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_grad_phi_hyd.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
calc_grad_phi_hyd.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
calc_grad_phi_hyd.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
calc_grad_phi_hyd.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_grad_phi_hyd.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_grad_phi_hyd.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_grad_phi_hyd.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_grad_phi_hyd.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_grad_phi_hyd.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_grad_phi_hyd.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_grad_phi_hyd.f: TANPHIATV_MACROS.h SURFACE.h DYNVARS.h
calc_grad_phi_surf.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
calc_grad_phi_surf.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_grad_phi_surf.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
calc_grad_phi_surf.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
calc_grad_phi_surf.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
calc_grad_phi_surf.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_grad_phi_surf.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_grad_phi_surf.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
calc_grad_phi_surf.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_grad_phi_surf.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_grad_phi_surf.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_grad_phi_surf.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_grad_phi_surf.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_grad_phi_surf.f: TANPHIATV_MACROS.h SURFACE.h
calc_gs.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
calc_gs.f: BUILD_INFO.h SIZE.h DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_gs.f: FCORI_MACROS.h GAD.h GAD_OPTIONS.h
calc_gt.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
calc_gt.f: BUILD_INFO.h SIZE.h DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_gt.f: FCORI_MACROS.h GAD.h GAD_OPTIONS.h
calc_gw.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
calc_gw.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
calc_gw.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
calc_gw.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
calc_gw.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_gw.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_gw.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_gw.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_gw.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_gw.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_gw.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h NH_VARS.h
calc_ivdc.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
calc_ivdc.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
calc_ivdc.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_ivdc.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
calc_ivdc.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_ivdc.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_ivdc.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_ivdc.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_ivdc.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_ivdc.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_ivdc.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
calc_phi_hyd.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
calc_phi_hyd.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
calc_phi_hyd.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
calc_phi_hyd.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
calc_phi_hyd.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_phi_hyd.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_phi_hyd.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_phi_hyd.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_phi_hyd.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_phi_hyd.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_phi_hyd.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_phi_hyd.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_phi_hyd.f: FCORI_MACROS.h SURFACE.h DYNVARS.h
calc_r_star.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
calc_r_star.f: BUILD_INFO.h W2_OPTIONS.h SIZE.h EEPARAMS.h PARAMS.h
calc_r_star.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
calc_r_star.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
calc_r_star.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
calc_r_star.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_r_star.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_r_star.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_r_star.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_r_star.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_r_star.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_r_star.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_r_star.f: TANPHIATV_MACROS.h SURFACE.h
calc_surf_dr.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
calc_surf_dr.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_surf_dr.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_surf_dr.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
calc_surf_dr.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_surf_dr.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_surf_dr.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_surf_dr.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_surf_dr.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_surf_dr.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_surf_dr.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_surf_dr.f: TANPHIATV_MACROS.h SURFACE.h
calc_viscosity.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
calc_viscosity.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
calc_viscosity.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
calc_viscosity.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_viscosity.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
calc_viscosity.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_viscosity.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_viscosity.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_viscosity.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_viscosity.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_viscosity.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_viscosity.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_viscosity.f: TANPHIATV_MACROS.h
cg2d.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
cg2d.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
cg2d.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
cg2d.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
cg2d.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
cg2d.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
cg2d.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
cg2d.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
cg2d.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
cg2d.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
cg2d.f: CG2D.h SURFACE.h
cg3d.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
cg3d.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
cg3d.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
cg3d.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
cg3d.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
cg3d.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
cg3d.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
cg3d.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
cg3d.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
cg3d.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
cg3d.f: CG3D.h
checkpoint.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
checkpoint.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
checkpoint.f: FCORI_MACROS.h MNC_PARAMS.h DYNVARS.h SURFACE.h
config_check.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
config_check.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
config_check.f: PARAMS_MACROS.h FCORI_MACROS.h
config_summary.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
config_summary.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
config_summary.f: PARAMS_MACROS.h FCORI_MACROS.h EOS.h GRID.h GRID_MACROS.h
config_summary.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
config_summary.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
config_summary.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
config_summary.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
config_summary.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
config_summary.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
config_summary.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
config_summary.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
config_summary.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
config_summary.f: TANPHIATV_MACROS.h DYNVARS.h MNC_PARAMS.h
convect.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
convect.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
convect.f: FCORI_MACROS.h DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h
convect.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
convect.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
convect.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
convect.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
convect.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
convect.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
convect.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
convect.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
convect.f: TANPHIATV_MACROS.h
convective_adjustment.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
convective_adjustment.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
convective_adjustment.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
convective_adjustment.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
convective_adjustment.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
convective_adjustment.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
convective_adjustment.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
convective_adjustment.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
convective_adjustment.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
convective_adjustment.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
convective_adjustment.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
convective_adjustment.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
convective_adjustment.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h
convective_adjustment.f: RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
convective_adjustment.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
convective_adjustment.f: TIMEAVE_STATV.h TIMEAVE_OPTIONS.h
convective_adjustment_ini.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
convective_adjustment_ini.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
convective_adjustment_ini.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
convective_adjustment_ini.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h
convective_adjustment_ini.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
convective_adjustment_ini.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
convective_adjustment_ini.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
convective_adjustment_ini.f: HFACW_MACROS.h RECIP_DXC_MACROS.h
convective_adjustment_ini.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
convective_adjustment_ini.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
convective_adjustment_ini.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
convective_adjustment_ini.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
convective_adjustment_ini.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
convective_adjustment_ini.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h
convective_adjustment_ini.f: RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
convective_adjustment_ini.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
convective_adjustment_ini.f: TIMEAVE_STATV.h TIMEAVE_OPTIONS.h
convective_weights.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
convective_weights.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
convective_weights.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
convective_weights.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
convective_weights.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
convective_weights.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
convective_weights.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
convective_weights.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
convective_weights.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
convective_weights.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
convective_weights.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
convective_weights.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
convective_weights.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
convective_weights.f: TANPHIATV_MACROS.h
convectively_mixtracer.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
convectively_mixtracer.f: BUILD_INFO.h SIZE.h EEPARAMS.h
correction_step.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
correction_step.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
correction_step.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
correction_step.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
correction_step.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
correction_step.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
correction_step.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
correction_step.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
correction_step.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
correction_step.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
correction_step.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
correction_step.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
cycle_tracer.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
cycle_tracer.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diags_oceanic_surf_flux.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
diags_oceanic_surf_flux.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
diags_oceanic_surf_flux.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
diags_oceanic_surf_flux.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
diags_oceanic_surf_flux.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
diags_oceanic_surf_flux.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
diags_oceanic_surf_flux.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
diags_oceanic_surf_flux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
diags_oceanic_surf_flux.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
diags_oceanic_surf_flux.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
diags_oceanic_surf_flux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
diags_oceanic_surf_flux.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
diags_oceanic_surf_flux.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
diags_oceanic_surf_flux.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
diags_oceanic_surf_flux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
diags_oceanic_surf_flux.f: TANPHIATV_MACROS.h DYNVARS.h SURFACE.h FFIELDS.h
diags_phi_hyd.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diags_phi_hyd.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diags_phi_hyd.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
diags_phi_hyd.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
diags_phi_hyd.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
diags_phi_hyd.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
diags_phi_hyd.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
diags_phi_hyd.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
diags_phi_hyd.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
diags_phi_hyd.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
diags_phi_hyd.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
diags_phi_hyd.f: TANPHIATV_MACROS.h SURFACE.h DYNVARS.h
diags_phi_rlow.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
diags_phi_rlow.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
diags_phi_rlow.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
diags_phi_rlow.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
diags_phi_rlow.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
diags_phi_rlow.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
diags_phi_rlow.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
diags_phi_rlow.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
diags_phi_rlow.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
diags_phi_rlow.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
diags_phi_rlow.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
diags_phi_rlow.f: TANPHIATV_MACROS.h SURFACE.h DYNVARS.h
diags_rho.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
diags_rho.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
diags_rho.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
diags_rho.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
diags_rho.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
diags_rho.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
diags_rho.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
diags_rho.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
diags_rho.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
diags_rho.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
diags_rho.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
do_atmospheric_phys.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_atmospheric_phys.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
do_atmospheric_phys.f: PARAMS_MACROS.h FCORI_MACROS.h
do_fields_blocking_exchanges.f: PACKAGES_CONFIG.h CPP_OPTIONS.h
do_fields_blocking_exchanges.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
do_fields_blocking_exchanges.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
do_fields_blocking_exchanges.f: FCORI_MACROS.h DYNVARS.h
do_gterm_blocking_exchanges.f: PACKAGES_CONFIG.h CPP_OPTIONS.h
do_gterm_blocking_exchanges.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
do_gterm_blocking_exchanges.f: SIZE.h EEPARAMS.h DYNVARS.h
do_oceanic_phys.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_oceanic_phys.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
do_oceanic_phys.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
do_oceanic_phys.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
do_oceanic_phys.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
do_oceanic_phys.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
do_oceanic_phys.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
do_oceanic_phys.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
do_oceanic_phys.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
do_oceanic_phys.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
do_oceanic_phys.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
do_oceanic_phys.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
do_oceanic_phys.f: TANPHIATV_MACROS.h TIMEAVE_STATV.h TIMEAVE_OPTIONS.h
do_stagger_fields_exchanges.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
do_stagger_fields_exchanges.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
do_stagger_fields_exchanges.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
do_statevars_diags.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_statevars_diags.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
do_statevars_diags.f: PARAMS_MACROS.h FCORI_MACROS.h
do_statevars_tave.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_statevars_tave.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
do_statevars_tave.f: PARAMS_MACROS.h FCORI_MACROS.h
do_the_model_io.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
do_the_model_io.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
do_the_model_io.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
dynamics.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
dynamics.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
dynamics.f: FCORI_MACROS.h DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h
dynamics.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
dynamics.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
dynamics.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
dynamics.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
dynamics.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
dynamics.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
dynamics.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
dynamics.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
dynamics.f: TANPHIATV_MACROS.h
external_fields_load.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
external_fields_load.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
external_fields_load.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h FFIELDS.h
external_fields_load.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
external_fields_load.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
external_fields_load.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
external_fields_load.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
external_fields_load.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
external_fields_load.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
external_fields_load.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
external_fields_load.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
external_fields_load.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
external_fields_load.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
external_fields_load.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
external_fields_load.f: TANPHIATV_MACROS.h DYNVARS.h
external_forcing.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
external_forcing.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
external_forcing.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
external_forcing.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
external_forcing.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
external_forcing.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
external_forcing.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
external_forcing.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
external_forcing.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
external_forcing.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
external_forcing.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
external_forcing.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
external_forcing.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h FFIELDS.h
external_forcing_surf.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
external_forcing_surf.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
external_forcing_surf.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h FFIELDS.h
external_forcing_surf.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h
external_forcing_surf.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
external_forcing_surf.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
external_forcing_surf.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
external_forcing_surf.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
external_forcing_surf.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
external_forcing_surf.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
external_forcing_surf.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
external_forcing_surf.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
external_forcing_surf.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
external_forcing_surf.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
external_forcing_surf.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
external_forcing_surf.f: SURFACE.h
find_alpha.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
find_alpha.f: SIZE.h DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
find_alpha.f: FCORI_MACROS.h EOS.h GRID.h GRID_MACROS.h DXC_MACROS.h
find_alpha.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
find_alpha.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
find_alpha.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
find_alpha.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
find_alpha.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
find_alpha.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
find_alpha.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
find_alpha.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
find_alpha.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
find_rho.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
find_rho.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h EOS.h GRID.h
find_rho.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
find_rho.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
find_rho.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
find_rho.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
find_rho.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
find_rho.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
find_rho.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
find_rho.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
find_rho.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
forward_step.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
forward_step.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
forward_step.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h MNC_PARAMS.h
forward_step.f: SIGREG.h SHAP_FILT.h
freesurf_rescale_g.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
freesurf_rescale_g.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
freesurf_rescale_g.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
freesurf_rescale_g.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
freesurf_rescale_g.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
freesurf_rescale_g.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
freesurf_rescale_g.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
freesurf_rescale_g.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
freesurf_rescale_g.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
freesurf_rescale_g.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
freesurf_rescale_g.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
freesurf_rescale_g.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
freesurf_rescale_g.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
freesurf_rescale_g.f: TANPHIATV_MACROS.h SURFACE.h
freeze.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
freeze.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
freeze_surface.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
freeze_surface.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
freeze_surface.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
freeze_surface.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
freeze_surface.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
freeze_surface.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
freeze_surface.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
freeze_surface.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
freeze_surface.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
freeze_surface.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
freeze_surface.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
freeze_surface.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
freeze_surface.f: TANPHIATV_MACROS.h FFIELDS.h
getcon.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
grad_sigma.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
grad_sigma.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
grad_sigma.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
grad_sigma.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
grad_sigma.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
grad_sigma.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
grad_sigma.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
grad_sigma.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
grad_sigma.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
grad_sigma.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
grad_sigma.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
grad_sigma.f: FCORI_MACROS.h
impldiff.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
impldiff.f: BUILD_INFO.h SIZE.h DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
impldiff.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
impldiff.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
impldiff.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
impldiff.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
impldiff.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
impldiff.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
impldiff.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
impldiff.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
impldiff.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_cartesian_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_cartesian_grid.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_cartesian_grid.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
ini_cartesian_grid.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_cartesian_grid.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_cartesian_grid.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_cartesian_grid.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_cartesian_grid.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_cartesian_grid.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_cartesian_grid.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_cartesian_grid.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_cartesian_grid.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_cartesian_grid.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_cartesian_grid.f: TANPHIATV_MACROS.h
ini_cg2d.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_cg2d.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_cg2d.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_cg2d.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_cg2d.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_cg2d.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_cg2d.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_cg2d.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_cg2d.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
ini_cg2d.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
ini_cg2d.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
ini_cg2d.f: CG2D.h
ini_cg3d.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_cg3d.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_cg3d.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_cg3d.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_cg3d.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_cg3d.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_cg3d.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_cg3d.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_cg3d.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
ini_cg3d.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
ini_cg3d.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
ini_cg3d.f: CG3D.h SOLVE_FOR_PRESSURE3D.h
ini_cori.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_cori.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_cori.f: FCORI_MACROS.h MNC_PARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h
ini_cori.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_cori.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_cori.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_cori.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_cori.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_cori.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
ini_cori.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_cori.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_cori.f: TANPHIATV_MACROS.h DYNVARS.h MONITOR.h
ini_curvilinear_grid.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
ini_curvilinear_grid.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
ini_curvilinear_grid.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_curvilinear_grid.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_curvilinear_grid.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_curvilinear_grid.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_curvilinear_grid.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_curvilinear_grid.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_curvilinear_grid.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_curvilinear_grid.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_curvilinear_grid.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_curvilinear_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
ini_curvilinear_grid.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
ini_curvilinear_grid.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_curvilinear_grid.f: W2_EXCH2_TOPOLOGY.h W2_EXCH2_PARAMS.h MNC_PARAMS.h
ini_cylinder_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_cylinder_grid.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_cylinder_grid.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
ini_cylinder_grid.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_cylinder_grid.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_cylinder_grid.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_cylinder_grid.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_cylinder_grid.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_cylinder_grid.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_cylinder_grid.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_cylinder_grid.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_cylinder_grid.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_cylinder_grid.f: TANPHIATV_MACROS.h
ini_depths.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_depths.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_depths.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_depths.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_depths.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_depths.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_depths.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_depths.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_depths.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
ini_depths.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_depths.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_depths.f: TANPHIATV_MACROS.h SURFACE.h
ini_dynvars.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
ini_dynvars.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_dynvars.f: DYNVARS.h
ini_eos.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
ini_eos.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h EOS.h GRID.h
ini_eos.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_eos.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_eos.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_eos.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_eos.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_eos.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_eos.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_eos.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_eos.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_fields.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_fields.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_fields.f: FCORI_MACROS.h
ini_forcing.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_forcing.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_forcing.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_forcing.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_forcing.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_forcing.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_forcing.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_forcing.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_forcing.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
ini_forcing.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_forcing.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_forcing.f: TANPHIATV_MACROS.h FFIELDS.h
ini_grid.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_grid.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_grid.f: FCORI_MACROS.h MNC_PARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h
ini_grid.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_grid.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_grid.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_grid.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_grid.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_grid.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
ini_grid.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_grid.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_grid.f: TANPHIATV_MACROS.h MONITOR.h
ini_linear_phisurf.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_linear_phisurf.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_linear_phisurf.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
ini_linear_phisurf.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_linear_phisurf.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_linear_phisurf.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_linear_phisurf.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_linear_phisurf.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_linear_phisurf.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_linear_phisurf.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_linear_phisurf.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_linear_phisurf.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_linear_phisurf.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_linear_phisurf.f: TANPHIATV_MACROS.h SURFACE.h
ini_masks_etc.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
ini_masks_etc.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
ini_masks_etc.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_masks_etc.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_masks_etc.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_masks_etc.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_masks_etc.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_masks_etc.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_masks_etc.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_masks_etc.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_masks_etc.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_masks_etc.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_masks_etc.f: TANPHIATV_MACROS.h SURFACE.h
ini_mixing.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
ini_mixing.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_mixing.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_mixing.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_mixing.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_mixing.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_mixing.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_mixing.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_mixing.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_mixing.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_mixing.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_mixing.f: TANPHIATV_MACROS.h DYNVARS.h
ini_mnc_vars.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
ini_mnc_vars.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h GRID.h GRID_MACROS.h
ini_mnc_vars.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_mnc_vars.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_mnc_vars.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_mnc_vars.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_mnc_vars.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_mnc_vars.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_mnc_vars.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_mnc_vars.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_mnc_vars.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_mnc_vars.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_mnc_vars.f: FCORI_MACROS.h
ini_model_io.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
ini_model_io.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
ini_model_io.f: PARAMS_MACROS.h FCORI_MACROS.h
ini_nh_vars.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
ini_nh_vars.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_nh_vars.f: NH_VARS.h
ini_p_ground.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
ini_p_ground.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_p_ground.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_p_ground.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_p_ground.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_p_ground.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_p_ground.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_p_ground.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_p_ground.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_p_ground.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_p_ground.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h
ini_p_ground.f: PARAMS_MACROS.h FCORI_MACROS.h SURFACE.h
ini_parms.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_parms.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_parms.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_parms.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_parms.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_parms.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_parms.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_parms.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_parms.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_parms.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
ini_parms.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h EOS.h
ini_phiref.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
ini_phiref.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_phiref.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_phiref.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_phiref.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_phiref.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_phiref.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_phiref.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_phiref.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_phiref.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_phiref.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_phiref.f: TANPHIATV_MACROS.h EOS.h
ini_pnh.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_pnh.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_pnh.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_pnh.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_pnh.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_pnh.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_pnh.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_pnh.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_pnh.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
ini_pnh.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
ini_pnh.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h NH_VARS.h
ini_pressure.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
ini_pressure.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_pressure.f: EOS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_pressure.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_pressure.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_pressure.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_pressure.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_pressure.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_pressure.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_pressure.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_pressure.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_pressure.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_psurf.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_psurf.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_psurf.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_psurf.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_psurf.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_psurf.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_psurf.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_psurf.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_psurf.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_psurf.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
ini_psurf.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_psurf.f: SURFACE.h
ini_r_star.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_r_star.f: BUILD_INFO.h W2_OPTIONS.h SIZE.h EEPARAMS.h PARAMS.h
ini_r_star.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_r_star.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_r_star.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_r_star.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_r_star.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_r_star.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_r_star.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_r_star.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_r_star.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_r_star.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
ini_spherical_polar_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_spherical_polar_grid.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
ini_spherical_polar_grid.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_spherical_polar_grid.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_spherical_polar_grid.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_spherical_polar_grid.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_spherical_polar_grid.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_spherical_polar_grid.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_spherical_polar_grid.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_spherical_polar_grid.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_spherical_polar_grid.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_spherical_polar_grid.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_spherical_polar_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
ini_spherical_polar_grid.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_spherical_polar_grid.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_spherical_polar_grid.f: TANPHIATV_MACROS.h
ini_surf_dr.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
ini_surf_dr.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_surf_dr.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_surf_dr.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_surf_dr.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_surf_dr.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_surf_dr.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_surf_dr.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_surf_dr.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_surf_dr.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_surf_dr.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_surf_dr.f: TANPHIATV_MACROS.h SURFACE.h
ini_theta.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
ini_theta.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_theta.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_theta.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_theta.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_theta.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_theta.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_theta.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_theta.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_theta.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_theta.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_vel.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
ini_vel.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_vel.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_vel.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_vel.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_vel.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_vel.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_vel.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_vel.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_vel.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_vel.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_vertical_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_vertical_grid.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_vertical_grid.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
ini_vertical_grid.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_vertical_grid.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_vertical_grid.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_vertical_grid.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_vertical_grid.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_vertical_grid.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_vertical_grid.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_vertical_grid.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_vertical_grid.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_vertical_grid.f: TANPHIATV_MACROS.h
initialise_fixed.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
initialise_fixed.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
initialise_fixed.f: PARAMS_MACROS.h FCORI_MACROS.h
initialise_varia.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
initialise_varia.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
initialise_varia.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h SURFACE.h
initialise_varia.f: SHAP_FILT.h
integr_continuity.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
integr_continuity.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
integr_continuity.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
integr_continuity.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
integr_continuity.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
integr_continuity.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
integr_continuity.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
integr_continuity.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
integr_continuity.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
integr_continuity.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
integr_continuity.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
integr_continuity.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
integr_continuity.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
integr_continuity.f: FFIELDS.h
integrate_for_w.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
integrate_for_w.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
integrate_for_w.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
integrate_for_w.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
integrate_for_w.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
integrate_for_w.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
integrate_for_w.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
integrate_for_w.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
integrate_for_w.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
integrate_for_w.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
integrate_for_w.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
integrate_for_w.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
load_grid_spacing.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
load_grid_spacing.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
load_grid_spacing.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
load_grid_spacing.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
load_grid_spacing.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
load_grid_spacing.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
load_grid_spacing.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
load_grid_spacing.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
load_grid_spacing.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
load_grid_spacing.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
load_grid_spacing.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
load_grid_spacing.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
load_grid_spacing.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
modeldata_example.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
modeldata_example.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
modeldata_example.f: FCORI_MACROS.h
momentum_correction_step.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
momentum_correction_step.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
momentum_correction_step.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
momentum_correction_step.f: SHAP_FILT.h
open_copy_data_file.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
open_copy_data_file.f: BUILD_INFO.h SIZE.h EEPARAMS.h
packages_boot.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
packages_boot.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
packages_check.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
packages_check.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
packages_check.f: PARAMS_MACROS.h FCORI_MACROS.h
packages_check_error.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
packages_check_error.f: BUILD_INFO.h SIZE.h EEPARAMS.h
packages_init_fixed.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
packages_init_fixed.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
packages_init_fixed.f: PARAMS_MACROS.h FCORI_MACROS.h
packages_init_variables.f: PACKAGES_CONFIG.h AD_CONFIG.h CPP_OPTIONS.h
packages_init_variables.f: CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h SIZE.h
packages_init_variables.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
packages_readparms.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
packages_readparms.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
packages_readparms.f: PARAMS_MACROS.h FCORI_MACROS.h
packages_write_pickup.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
packages_write_pickup.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
packages_write_pickup.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
plot_field.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
plot_field.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
pressure_for_eos.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
pressure_for_eos.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
pressure_for_eos.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
pressure_for_eos.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
pressure_for_eos.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
pressure_for_eos.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
pressure_for_eos.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
pressure_for_eos.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
pressure_for_eos.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
pressure_for_eos.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
pressure_for_eos.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
pressure_for_eos.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h SURFACE.h
quasihydrostaticterms.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
quasihydrostaticterms.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
quasihydrostaticterms.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
quasihydrostaticterms.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
quasihydrostaticterms.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
quasihydrostaticterms.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
quasihydrostaticterms.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
quasihydrostaticterms.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
quasihydrostaticterms.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
quasihydrostaticterms.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
quasihydrostaticterms.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
quasihydrostaticterms.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
quasihydrostaticterms.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
quasihydrostaticterms.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
quasihydrostaticterms.f: DYNVARS.h
read_write.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
read_write.f: SIZE.h EEPARAMS.h
remove_mean.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
remove_mean.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
set_defaults.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
set_defaults.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
set_defaults.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
set_defaults.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
set_defaults.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
set_defaults.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
set_defaults.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
set_defaults.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
set_defaults.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
set_defaults.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
set_defaults.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
set_defaults.f: TANPHIATV_MACROS.h
solve_for_pressure.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
solve_for_pressure.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
solve_for_pressure.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
solve_for_pressure.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
solve_for_pressure.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
solve_for_pressure.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
solve_for_pressure.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
solve_for_pressure.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
solve_for_pressure.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
solve_for_pressure.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
solve_for_pressure.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
solve_for_pressure.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
solve_for_pressure.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
solve_for_pressure.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
solve_for_pressure.f: SURFACE.h FFIELDS.h DYNVARS.h SOLVE_FOR_PRESSURE.h
solve_pentadiagonal.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
solve_pentadiagonal.f: BUILD_INFO.h SIZE.h EEPARAMS.h
solve_tridiagonal.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
solve_tridiagonal.f: BUILD_INFO.h SIZE.h EEPARAMS.h
state_summary.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
state_summary.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
state_summary.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
state_summary.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
state_summary.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
state_summary.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
state_summary.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
state_summary.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
state_summary.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
state_summary.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
state_summary.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
state_summary.f: TANPHIATV_MACROS.h DYNVARS.h
swfrac.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
swfrac.f: BUILD_INFO.h
taueddy_external_forcing.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
taueddy_external_forcing.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
taueddy_external_forcing.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h
taueddy_external_forcing.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
taueddy_external_forcing.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
taueddy_external_forcing.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
taueddy_external_forcing.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
taueddy_external_forcing.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
taueddy_external_forcing.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
taueddy_external_forcing.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
taueddy_external_forcing.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
taueddy_external_forcing.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
taueddy_external_forcing.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
taueddy_external_forcing.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
taueddy_external_forcing.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
taueddy_external_forcing.f: TANPHIATV_MACROS.h DYNVARS.h FFIELDS.h
taueddy_init_varia.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
taueddy_init_varia.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
taueddy_init_varia.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
taueddy_init_varia.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
taueddy_init_varia.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
taueddy_init_varia.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
taueddy_init_varia.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
taueddy_init_varia.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
taueddy_init_varia.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
taueddy_init_varia.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
taueddy_init_varia.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
taueddy_init_varia.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
taueddy_init_varia.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
taueddy_init_varia.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
taueddy_init_varia.f: FFIELDS.h
the_correction_step.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
the_correction_step.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
the_correction_step.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
the_main_loop.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
the_main_loop.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
the_main_loop.f: PARAMS_MACROS.h FCORI_MACROS.h
the_model_main.f: AD_CONFIG.h PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
the_model_main.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
the_model_main.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
thermodynamics.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
thermodynamics.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
thermodynamics.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
thermodynamics.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
thermodynamics.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
thermodynamics.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
thermodynamics.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
thermodynamics.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
thermodynamics.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
thermodynamics.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
thermodynamics.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
thermodynamics.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
thermodynamics.f: TANPHIATV_MACROS.h GAD.h GAD_OPTIONS.h TIMEAVE_STATV.h
thermodynamics.f: TIMEAVE_OPTIONS.h
timestep.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
timestep.f: BUILD_INFO.h SIZE.h DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
timestep.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
timestep.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
timestep.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
timestep.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
timestep.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
timestep.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
timestep.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
timestep.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
timestep.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
timestep_tracer.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h BUILD_INFO.h
timestep_tracer.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
timestep_wvel.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
timestep_wvel.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
timestep_wvel.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
timestep_wvel.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
timestep_wvel.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
timestep_wvel.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
timestep_wvel.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
timestep_wvel.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
timestep_wvel.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
timestep_wvel.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
timestep_wvel.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
timestep_wvel.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
timestep_wvel.f: TANPHIATV_MACROS.h DYNVARS.h NH_VARS.h
tracers_correction_step.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
tracers_correction_step.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h
tracers_correction_step.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
tracers_iigw_correction.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
tracers_iigw_correction.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
tracers_iigw_correction.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h
tracers_iigw_correction.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
tracers_iigw_correction.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
tracers_iigw_correction.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
tracers_iigw_correction.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
tracers_iigw_correction.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
tracers_iigw_correction.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
tracers_iigw_correction.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
tracers_iigw_correction.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
tracers_iigw_correction.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
tracers_iigw_correction.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
tracers_iigw_correction.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
tracers_iigw_correction.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
tracers_iigw_correction.f: TANPHIATV_MACROS.h DYNVARS.h
update_cg2d.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
update_cg2d.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
update_cg2d.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
update_cg2d.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
update_cg2d.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
update_cg2d.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
update_cg2d.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
update_cg2d.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
update_cg2d.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
update_cg2d.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
update_cg2d.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
update_cg2d.f: TANPHIATV_MACROS.h SURFACE.h CG2D.h
update_etah.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
update_etah.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
update_etah.f: FCORI_MACROS.h DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h
update_etah.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
update_etah.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
update_etah.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
update_etah.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
update_etah.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
update_etah.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
update_etah.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
update_etah.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
update_etah.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h FFIELDS.h
update_r_star.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
update_r_star.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
update_r_star.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
update_r_star.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
update_r_star.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
update_r_star.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
update_r_star.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
update_r_star.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
update_r_star.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
update_r_star.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
update_r_star.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
update_r_star.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
update_r_star.f: TANPHIATV_MACROS.h SURFACE.h
update_surf_dr.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h
update_surf_dr.f: CPP_EEMACROS.h BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h
update_surf_dr.f: PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
update_surf_dr.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
update_surf_dr.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
update_surf_dr.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
update_surf_dr.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
update_surf_dr.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
update_surf_dr.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
update_surf_dr.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
update_surf_dr.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
update_surf_dr.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
update_surf_dr.f: TANPHIATV_MACROS.h SURFACE.h
write_grid.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
write_grid.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
write_grid.f: FCORI_MACROS.h MNC_PARAMS.h GRID.h GRID_MACROS.h DXC_MACROS.h
write_grid.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
write_grid.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
write_grid.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
write_grid.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
write_grid.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
write_grid.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
write_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
write_grid.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
write_grid.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
write_state.f: PACKAGES_CONFIG.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
write_state.f: BUILD_INFO.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
write_state.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
write_state.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
write_state.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
write_state.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
write_state.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
write_state.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
write_state.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
write_state.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
write_state.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
write_state.f: TANPHIATV_MACROS.h DYNVARS.h MNC_PARAMS.h
