# Multithreaded + multi-processing makefile for Linux ayana 2.2.14-5.0 #1 Tue Mar 7 21:07:39 EST 2000 i686 unknown
# This makefile was generated automatically on
#    Thu Sep 13 10:56:27 EDT 2001
# by the command:
#    ../../../tools/genmake -disable=grdchk,kpp,gmredi,shap_filt,zonal_filt -mods=../code
# executed by:
#    adcroft@ayana:/home/adcroft/mitgcm/c40pre1/verification/exp4_flt/input
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
# KPP          : Special preprocessor command (specific to platform)
# KFLAGS       : Flags for KPP
# FC           : Fortran compiler command
# FFLAGS       : Configuration/debugging options for FC
# FOPTIM       : Optimization options for FC
# LINK         : Command for link editor program
# LIBS         : Library flags /or/ additional optimization/debugging flags

ROOTDIR     = ../../..
BUILDDIR    = .  
SOURCEDIRS  = ../code ../../../pkg/debug ../../../pkg/flt ../../../pkg/generic_advdiff ../../../pkg/mdsio ../../../pkg/mom_fluxform ../../../pkg/mom_vecinv ../../../pkg/monitor ../../../pkg/obcs ../../../pkg/rw ../../../pkg/timeave ../../../eesupp/src ../../../model/src
INCLUDEDIRS = ../code ../../../pkg/debug ../../../pkg/flt ../../../pkg/generic_advdiff ../../../pkg/mdsio ../../../pkg/mom_fluxform ../../../pkg/mom_vecinv ../../../pkg/monitor ../../../pkg/obcs ../../../pkg/rw ../../../pkg/timeave ../../../eesupp/inc ../../../model/inc
EXEDIR      = .
EXECUTABLE  = $(EXEDIR)/mitgcmuv
TOOLSDIR    = ../../../tools

# Unix ln (link)
LN = /bin/ln -s
# C preprocessor
CPP = cat $< | $(TOOLSDIR)/set64bitConst.sh | /lib/cpp  -traditional -P
# Special preprocessor (KAP on DECs, FPP on Crays)
KPP = 
# Fortran compiler
FC = g77
# Link editor
LINK = g77

# Defines for CPP
DEFINES = -D_BYTESWAPIO -DWORDLENGTH=4
# Includes for CPP
INCLUDES = -I. -I../code -I../../../pkg/debug -I../../../pkg/flt -I../../../pkg/generic_advdiff -I../../../pkg/mdsio -I../../../pkg/mom_fluxform -I../../../pkg/mom_vecinv -I../../../pkg/monitor -I../../../pkg/obcs -I../../../pkg/rw -I../../../pkg/timeave -I../../../eesupp/inc -I../../../model/inc
# Flags for KPP
KFLAGS1 = 
KFLAGS2 = 
# Optim./debug for FC
FFLAGS = -Wimplicit -Wunused -Wuninitialized
FOPTIM = -O3 -malign-double -funroll-loops
# Flags for CC
CFLAGS = 
# Files that should not be optimized
NOOPTFILES = 
NOOPTFLAGS = 
# Flags and libraries needed for linking
LIBS =  $(XLIBS)

SRCFILES =  \
            obcs_calc.F \
            chksum_tiled.F \
            debug_stats_rl.F \
            debug_stats_rs.F \
            exch_recv_get_vec.F \
            exch_send_put_vec.F \
            flt_bilinear.F \
            flt_down.F \
            flt_exchg.F \
            flt_functions.F \
            flt_init.F \
            flt_main.F \
            flt_mdsreadvector.F \
            flt_restart.F \
            flt_runga2.F \
            flt_traj.F \
            flt_up.F \
            calc_gs.F \
            calc_gt.F \
            calc_gtr1.F \
            gad_advection.F \
            gad_biharm_x.F \
            gad_biharm_y.F \
            gad_c2_adv_r.F \
            gad_c2_adv_x.F \
            gad_c2_adv_y.F \
            gad_c4_adv_r.F \
            gad_c4_adv_x.F \
            gad_c4_adv_y.F \
            gad_calc_rhs.F \
            gad_del2.F \
            gad_diff_r.F \
            gad_diff_x.F \
            gad_diff_y.F \
            gad_dst3_adv_r.F \
            gad_dst3_adv_x.F \
            gad_dst3_adv_y.F \
            gad_dst3fl_adv_r.F \
            gad_dst3fl_adv_x.F \
            gad_dst3fl_adv_y.F \
            gad_fluxlimit_adv_r.F \
            gad_fluxlimit_adv_x.F \
            gad_fluxlimit_adv_y.F \
            gad_grad_x.F \
            gad_grad_y.F \
            gad_u3_adv_r.F \
            gad_u3_adv_x.F \
            gad_u3_adv_y.F \
            mdsio.F \
            mdsio_byteswapr4.F \
            mdsio_byteswapr8.F \
            mdsio_findunit.F \
            mdsio_gl.F \
            mdsio_read_rl_vec.F \
            mdsio_read_rs_vec.F \
            mdsio_readfield.F \
            mdsio_readtile.F \
            mdsio_readvector.F \
            mdsio_reclen.F \
            mdsio_seg4torl.F \
            mdsio_seg4tors.F \
            mdsio_seg8torl.F \
            mdsio_seg8tors.F \
            mdsio_slice.F \
            mdsio_write_rl_vec.F \
            mdsio_write_rs_vec.F \
            mdsio_writefield.F \
            mdsio_writelocal.F \
            mdsio_writemeta.F \
            mdsio_writetile.F \
            mdsio_writevector.F \
            mom_calc_hfacz.F \
            mom_calc_ke.F \
            mom_cdscheme.F \
            mom_fluxform.F \
            mom_u_adv_uu.F \
            mom_u_adv_vu.F \
            mom_u_adv_wu.F \
            mom_u_bottomdrag.F \
            mom_u_coriolis.F \
            mom_u_del2u.F \
            mom_u_metric_nh.F \
            mom_u_metric_sphere.F \
            mom_u_rviscflux.F \
            mom_u_sidedrag.F \
            mom_u_xviscflux.F \
            mom_u_yviscflux.F \
            mom_v_adv_uv.F \
            mom_v_adv_vv.F \
            mom_v_adv_wv.F \
            mom_v_bottomdrag.F \
            mom_v_coriolis.F \
            mom_v_del2v.F \
            mom_v_metric_nh.F \
            mom_v_metric_sphere.F \
            mom_v_rviscflux.F \
            mom_v_sidedrag.F \
            mom_v_xviscflux.F \
            mom_v_yviscflux.F \
            mom_calc_strain.F \
            mom_calc_tension.F \
            mom_hdissip.F \
            mom_vecinv.F \
            mom_vi_calc_absvort3.F \
            mom_vi_calc_hdiv.F \
            mom_vi_calc_ke.F \
            mom_vi_calc_relvort3.F \
            mom_vi_coriolis.F \
            mom_vi_del2uv.F \
            mom_vi_hdissip.F \
            mom_vi_u_coriolis.F \
            mom_vi_u_coriolis_c4.F \
            mom_vi_u_grad_ke.F \
            mom_vi_u_vertshear.F \
            mom_vi_u_xviscflux.F \
            mom_vi_u_yviscflux.F \
            mom_vi_v_coriolis.F \
            mom_vi_v_coriolis_c4.F \
            mom_vi_v_grad_ke.F \
            mom_vi_v_vertshear.F \
            mom_vi_v_xviscflux.F \
            mom_vi_v_yviscflux.F \
            mon_advcfl.F \
            mon_advcflw.F \
            mon_advcflw2.F \
            mon_init.F \
            mon_ke.F \
            mon_out.F \
            mon_printstats_rl.F \
            mon_printstats_rs.F \
            mon_set.F \
            mon_stats_rl.F \
            mon_stats_rs.F \
            monitor.F \
            obcs_apply_sloc.F \
            obcs_apply_tloc.F \
            obcs_apply_ts.F \
            obcs_apply_uv.F \
            obcs_apply_w.F \
            obcs_check.F \
            obcs_checkpoint.F \
            obcs_init_fixed.F \
            obcs_init_variables.F \
            obcs_readparms.F \
            orlanski_checkpoint.F \
            orlanski_east.F \
            orlanski_init.F \
            orlanski_north.F \
            orlanski_south.F \
            orlanski_west.F \
            read_fld_xy_rl.F \
            read_fld_xy_rs.F \
            read_fld_xyz_rl.F \
            read_fld_xyz_rs.F \
            set_write_global_fld.F \
            write_fld_xy_rl.F \
            write_fld_xy_rs.F \
            write_fld_xyz_rl.F \
            write_fld_xyz_rs.F \
            write_local_rl.F \
            write_local_rs.F \
            timeave_cumul_1k.F \
            timeave_cumul_1t.F \
            timeave_cumul_2v.F \
            timeave_cumulate.F \
            timeave_norma_1k.F \
            timeave_normaliz.F \
            timeave_reset.F \
            timeave_reset_1k.F \
            timeave_statv_write.F \
            timeave_statvars.F \
            bar2.F \
            barrier.F \
            check_threads.F \
            comm_stats.F \
            dfile.F \
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
            exch_uv_rl_cube.F \
            exch_uv_rs_cube.F \
            exch_uv_xy_rl.F \
            exch_uv_xy_rs.F \
            exch_uv_xyz_rl.F \
            exch_uv_xyz_rs.F \
            exch_xy_rl.F \
            exch_xy_rs.F \
            exch_xyz_rl.F \
            exch_xyz_rs.F \
            exch_z_rl_cube.F \
            exch_z_rs_cube.F \
            exch_z_xy_rl.F \
            exch_z_xy_rs.F \
            exch_z_xyz_rl.F \
            exch_z_xyz_rs.F \
            fool_the_compiler.F \
            global_max.F \
            global_sum.F \
            gsum.F \
            gsum_jam.F \
            ini_communication_patterns.F \
            ini_jam.F \
            ini_procs.F \
            ini_threading_environment.F \
            main.F \
            memsync.F \
            nml_filter.F \
            print.F \
            timers.F \
            utils.F \
            calc_buoyancy.F \
            calc_common_factors.F \
            calc_diffusivity.F \
            calc_div_ghat.F \
            calc_exact_eta.F \
            calc_grad_phi_surf.F \
            calc_gw.F \
            calc_ivdc.F \
            calc_phi_hyd.F \
            calc_surf_dr.F \
            calc_viscosity.F \
            cg2d.F \
            cg3d.F \
            checkpoint.F \
            config_summary.F \
            convect.F \
            convective_adjustment.F \
            convective_adjustment_ini.F \
            correction_step.F \
            cycle_tracer.F \
            do_fields_blocking_exchanges.F \
            do_gterm_blocking_exchanges.F \
            do_the_model_io.F \
            dynamics.F \
            external_fields_load.F \
            external_forcing.F \
            external_forcing_surf.F \
            find_alpha.F \
            find_rho.F \
            forward_step.F \
            freeze.F \
            grad_sigma.F \
            impldiff.F \
            ini_cartesian_grid.F \
            ini_cg2d.F \
            ini_cg3d.F \
            ini_cori.F \
            ini_curvilinear_grid.F \
            ini_depths.F \
            ini_dynvars.F \
            ini_fields.F \
            ini_forcing.F \
            ini_grid.F \
            ini_gw.F \
            ini_linear_phisurf.F \
            ini_masks_etc.F \
            ini_mixing.F \
            ini_p_ground.F \
            ini_parms.F \
            ini_pnh.F \
            ini_psurf.F \
            ini_salt.F \
            ini_spherical_polar_grid.F \
            ini_theta.F \
            ini_tr1.F \
            ini_vel.F \
            ini_vertical_grid.F \
            initialise_fixed.F \
            initialise_varia.F \
            integrate_for_w.F \
            modeldata_example.F \
            open_copy_data_file.F \
            packages_boot.F \
            packages_check.F \
            packages_check_error.F \
            packages_init_fixed.F \
            packages_init_variables.F \
            packages_readparms.F \
            plot_field.F \
            port_rand.F \
            read_write.F \
            read_write_rec.F \
            set_defaults.F \
            solve_for_pressure.F \
            state_summary.F \
            swfrac.F \
            the_correction_step.F \
            the_main_loop.F \
            the_model_main.F \
            thermodynamics.F \
            timestep.F \
            timestep_tracer.F \
            update_cg2d.F \
            update_surf_dr.F \
            write_state.F
CSRCFILES =  \
            tim.c
HEADERFILES =  \
            CPP_EEOPTIONS.h \
            CPP_OPTIONS.h \
            SIZE.h \
            FLT.h \
            FLT_CPPOPTIONS.h \
            GAD.h \
            GAD_FLUX_LIMITER.h \
            GAD_OPTIONS.h \
            MDSIO_OPTIONS.h \
            MONITOR.h \
            OBCS.h \
            OBCS_OPTIONS.h \
            ORLANSKI.h \
            TIMEAVE_STATV.h \
            BAR2.h \
            BARRIER.h \
            CPP_EEMACROS.h \
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
            CG2D.h \
            CG2D_INTERNAL.h \
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
            FCORI_MACROS.h \
            FFIELDS.h \
            GRID.h \
            GRID_MACROS.h \
            GW.h \
            HFACC_MACROS.h \
            HFACS_MACROS.h \
            HFACW_MACROS.h \
            INI_DEPTHS.h \
            MASKS_MACROS.h \
            MASKW_MACROS.h \
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
            TR1.h \
            XC_MACROS.h \
            YC_MACROS.h
F77FILES =  $(SRCFILES:.F=.f)
OBJFILES =  $(SRCFILES:.F=.o) $(CSRCFILES:.c=.o)

.SUFFIXES:
.SUFFIXES: .o .f .p .F .c

all: $(EXECUTABLE)
$(EXECUTABLE): $(OBJFILES)
	$(LINK) -o $@ $(FFLAGS) $(FOPTIM) $(OBJFILES) $(LIBS)
depend:
	@make links
	makedepend -o .f $(INCLUDES) $(SRCFILES)

links: $(SRCFILES) $(CSRCFILES) $(HEADERFILES)

small_f: $(F77FILES)

output.txt: $(EXECUTABLE)
	@printf 'running ... '
	@$(EXECUTABLE) > $@

clean:
	-rm -rf *.o *.f *.p 
Clean:
	@make clean
	@make cleanlinks
	-rm -f Makefile.bak
CLEAN:
	@make Clean
	-find $(EXEDIR) -name "*.meta" -exec rm {} \;
	-find $(EXEDIR) -name "*.data" -exec rm {} \;
	-rm -f $(EXECUTABLE)

makefile:
	../../../tools/genmake -disable=grdchk,kpp,gmredi,shap_filt,zonal_filt -mods=../code
cleanlinks:
	-find . -type l -exec rm {} \;

# The normal chain of rules is (  .F - .f - .o  )
.F.f:
	$(CPP) $(DEFINES) $(INCLUDES) > $@
.f.o:
	$(FC) $(FFLAGS) $(FOPTIM) -c $<
.c.o:
	$(CC) $(CFLAGS) -c $<

# Special exceptions that use the ( .F - .p - .f - .o ) rule-chain
.F.p:
	$(CPP) $(DEFINES) $(INCLUDES) > $@
.p.f:
	$(KPP) $(KFLAGS1)$@ $(KFLAGS2) $<

# This section creates symbolic links

# These files are linked from ../code
CPP_EEOPTIONS.h CPP_OPTIONS.h SIZE.h obcs_calc.F:
	$(LN) ../code/$@ $@
# These files are linked from ../../../pkg/debug
chksum_tiled.F debug_stats_rl.F debug_stats_rs.F:
	$(LN) ../../../pkg/debug/$@ $@
# These files are linked from ../../../pkg/flt
FLT.h FLT_CPPOPTIONS.h exch_recv_get_vec.F exch_send_put_vec.F flt_bilinear.F flt_down.F flt_exchg.F flt_functions.F flt_init.F flt_main.F flt_mdsreadvector.F flt_restart.F flt_runga2.F flt_traj.F flt_up.F:
	$(LN) ../../../pkg/flt/$@ $@
# These files are linked from ../../../pkg/generic_advdiff
GAD.h GAD_FLUX_LIMITER.h GAD_OPTIONS.h calc_gs.F calc_gt.F calc_gtr1.F gad_advection.F gad_biharm_x.F gad_biharm_y.F gad_c2_adv_r.F gad_c2_adv_x.F gad_c2_adv_y.F gad_c4_adv_r.F gad_c4_adv_x.F gad_c4_adv_y.F gad_calc_rhs.F gad_del2.F gad_diff_r.F gad_diff_x.F gad_diff_y.F gad_dst3_adv_r.F gad_dst3_adv_x.F gad_dst3_adv_y.F gad_dst3fl_adv_r.F gad_dst3fl_adv_x.F gad_dst3fl_adv_y.F gad_fluxlimit_adv_r.F gad_fluxlimit_adv_x.F gad_fluxlimit_adv_y.F gad_grad_x.F gad_grad_y.F gad_u3_adv_r.F gad_u3_adv_x.F gad_u3_adv_y.F:
	$(LN) ../../../pkg/generic_advdiff/$@ $@
# These files are linked from ../../../pkg/mdsio
MDSIO_OPTIONS.h mdsio.F mdsio_byteswapr4.F mdsio_byteswapr8.F mdsio_findunit.F mdsio_gl.F mdsio_read_rl_vec.F mdsio_read_rs_vec.F mdsio_readfield.F mdsio_readtile.F mdsio_readvector.F mdsio_reclen.F mdsio_seg4torl.F mdsio_seg4tors.F mdsio_seg8torl.F mdsio_seg8tors.F mdsio_slice.F mdsio_write_rl_vec.F mdsio_write_rs_vec.F mdsio_writefield.F mdsio_writelocal.F mdsio_writemeta.F mdsio_writetile.F mdsio_writevector.F:
	$(LN) ../../../pkg/mdsio/$@ $@
# These files are linked from ../../../pkg/mom_fluxform
mom_calc_hfacz.F mom_calc_ke.F mom_cdscheme.F mom_fluxform.F mom_u_adv_uu.F mom_u_adv_vu.F mom_u_adv_wu.F mom_u_bottomdrag.F mom_u_coriolis.F mom_u_del2u.F mom_u_metric_nh.F mom_u_metric_sphere.F mom_u_rviscflux.F mom_u_sidedrag.F mom_u_xviscflux.F mom_u_yviscflux.F mom_v_adv_uv.F mom_v_adv_vv.F mom_v_adv_wv.F mom_v_bottomdrag.F mom_v_coriolis.F mom_v_del2v.F mom_v_metric_nh.F mom_v_metric_sphere.F mom_v_rviscflux.F mom_v_sidedrag.F mom_v_xviscflux.F mom_v_yviscflux.F:
	$(LN) ../../../pkg/mom_fluxform/$@ $@
# These files are linked from ../../../pkg/mom_vecinv
mom_calc_strain.F mom_calc_tension.F mom_hdissip.F mom_vecinv.F mom_vi_calc_absvort3.F mom_vi_calc_hdiv.F mom_vi_calc_ke.F mom_vi_calc_relvort3.F mom_vi_coriolis.F mom_vi_del2uv.F mom_vi_hdissip.F mom_vi_u_coriolis.F mom_vi_u_coriolis_c4.F mom_vi_u_grad_ke.F mom_vi_u_vertshear.F mom_vi_u_xviscflux.F mom_vi_u_yviscflux.F mom_vi_v_coriolis.F mom_vi_v_coriolis_c4.F mom_vi_v_grad_ke.F mom_vi_v_vertshear.F mom_vi_v_xviscflux.F mom_vi_v_yviscflux.F:
	$(LN) ../../../pkg/mom_vecinv/$@ $@
# These files are linked from ../../../pkg/monitor
MONITOR.h mon_advcfl.F mon_advcflw.F mon_advcflw2.F mon_init.F mon_ke.F mon_out.F mon_printstats_rl.F mon_printstats_rs.F mon_set.F mon_stats_rl.F mon_stats_rs.F monitor.F:
	$(LN) ../../../pkg/monitor/$@ $@
# These files are linked from ../../../pkg/obcs
OBCS.h OBCS_OPTIONS.h ORLANSKI.h obcs_apply_sloc.F obcs_apply_tloc.F obcs_apply_ts.F obcs_apply_uv.F obcs_apply_w.F obcs_check.F obcs_checkpoint.F obcs_init_fixed.F obcs_init_variables.F obcs_readparms.F orlanski_checkpoint.F orlanski_east.F orlanski_init.F orlanski_north.F orlanski_south.F orlanski_west.F:
	$(LN) ../../../pkg/obcs/$@ $@
# These files are linked from ../../../pkg/rw
read_fld_xy_rl.F read_fld_xy_rs.F read_fld_xyz_rl.F read_fld_xyz_rs.F set_write_global_fld.F write_fld_xy_rl.F write_fld_xy_rs.F write_fld_xyz_rl.F write_fld_xyz_rs.F write_local_rl.F write_local_rs.F:
	$(LN) ../../../pkg/rw/$@ $@
# These files are linked from ../../../pkg/timeave
TIMEAVE_STATV.h timeave_cumul_1k.F timeave_cumul_1t.F timeave_cumul_2v.F timeave_cumulate.F timeave_norma_1k.F timeave_normaliz.F timeave_reset.F timeave_reset_1k.F timeave_statv_write.F timeave_statvars.F:
	$(LN) ../../../pkg/timeave/$@ $@
# These files are linked from ../../../eesupp/src
bar2.F barrier.F check_threads.F comm_stats.F dfile.F different_multiple.F eeboot.F eeboot_minimal.F eedata_example.F eedie.F eeintro_msg.F eeset_parms.F eewrite_eeenv.F exch_cycle_ebl.F exch_init.F exch_jam.F exch_rl.F exch_rl_cube.F exch_rl_recv_get_x.F exch_rl_recv_get_y.F exch_rl_send_put_x.F exch_rl_send_put_y.F exch_rs.F exch_rs_cube.F exch_rs_recv_get_x.F exch_rs_recv_get_y.F exch_rs_send_put_x.F exch_rs_send_put_y.F exch_uv_rl_cube.F exch_uv_rs_cube.F exch_uv_xy_rl.F exch_uv_xy_rs.F exch_uv_xyz_rl.F exch_uv_xyz_rs.F exch_xy_rl.F exch_xy_rs.F exch_xyz_rl.F exch_xyz_rs.F exch_z_rl_cube.F exch_z_rs_cube.F exch_z_xy_rl.F exch_z_xy_rs.F exch_z_xyz_rl.F exch_z_xyz_rs.F fool_the_compiler.F global_max.F global_sum.F gsum.F gsum_jam.F ini_communication_patterns.F ini_jam.F ini_procs.F ini_threading_environment.F main.F memsync.F nml_filter.F print.F tim.c timers.F utils.F:
	$(LN) ../../../eesupp/src/$@ $@
# These files are linked from ../../../model/src
calc_buoyancy.F calc_common_factors.F calc_diffusivity.F calc_div_ghat.F calc_exact_eta.F calc_grad_phi_surf.F calc_gw.F calc_ivdc.F calc_phi_hyd.F calc_surf_dr.F calc_viscosity.F cg2d.F cg3d.F checkpoint.F config_summary.F convect.F convective_adjustment.F convective_adjustment_ini.F correction_step.F cycle_tracer.F do_fields_blocking_exchanges.F do_gterm_blocking_exchanges.F do_the_model_io.F dynamics.F external_fields_load.F external_forcing.F external_forcing_surf.F find_alpha.F find_rho.F forward_step.F freeze.F grad_sigma.F impldiff.F ini_cartesian_grid.F ini_cg2d.F ini_cg3d.F ini_cori.F ini_curvilinear_grid.F ini_depths.F ini_dynvars.F ini_fields.F ini_forcing.F ini_grid.F ini_gw.F ini_linear_phisurf.F ini_masks_etc.F ini_mixing.F ini_p_ground.F ini_parms.F ini_pnh.F ini_psurf.F ini_salt.F ini_spherical_polar_grid.F ini_theta.F ini_tr1.F ini_vel.F ini_vertical_grid.F initialise_fixed.F initialise_varia.F integrate_for_w.F modeldata_example.F open_copy_data_file.F packages_boot.F packages_check.F packages_check_error.F packages_init_fixed.F packages_init_variables.F packages_readparms.F plot_field.F port_rand.F read_write.F read_write_rec.F set_defaults.F solve_for_pressure.F state_summary.F swfrac.F the_correction_step.F the_main_loop.F the_model_main.F thermodynamics.F timestep.F timestep_tracer.F update_cg2d.F update_surf_dr.F write_state.F:
	$(LN) ../../../model/src/$@ $@
# These files are linked from ../../../eesupp/inc
BAR2.h BARRIER.h CPP_EEMACROS.h DFILE.h EEIO.h EEPARAMS.h EESUPPORT.h EXCH.h EXCH_JAM.h GLOBAL_MAX.h GLOBAL_SUM.h JAM_INFO.h MAIN_PDIRECTIVES1.h MAIN_PDIRECTIVES2.h MPI_INFO.h:
	$(LN) ../../../eesupp/inc/$@ $@
# These files are linked from ../../../model/inc
CG2D.h CG2D_INTERNAL.h CG3D.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYNVARS.h DYU_MACROS.h FCORI_MACROS.h FFIELDS.h GRID.h GRID_MACROS.h GW.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h INI_DEPTHS.h MASKS_MACROS.h MASKW_MACROS.h PARAMS.h PARAMS_MACROS.h RAS_MACROS.h RAW_MACROS.h RA_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h SOLVE_FOR_PRESSURE.h SOLVE_FOR_PRESSURE3D.h SURFACE.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h THE_MODEL_COMMON_BLOCKS.h TR1.h XC_MACROS.h YC_MACROS.h:
	$(LN) ../../../model/inc/$@ $@

# DO NOT DELETE

obcs_calc.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
obcs_calc.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h OBCS.h
chksum_tiled.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
chksum_tiled.f: EEPARAMS.h
debug_stats_rl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
debug_stats_rl.f: EEPARAMS.h
debug_stats_rs.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
debug_stats_rs.f: EEPARAMS.h
exch_recv_get_vec.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_recv_get_vec.f: EEPARAMS.h EESUPPORT.h FLT.h EXCH.h
exch_send_put_vec.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
exch_send_put_vec.f: EEPARAMS.h EESUPPORT.h FLT.h EXCH.h
flt_bilinear.f: FLT_CPPOPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
flt_bilinear.f: SIZE.h
flt_down.f: FLT_CPPOPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
flt_down.f: EEPARAMS.h SIZE.h FLT.h
flt_exchg.f: FLT_CPPOPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
flt_exchg.f: SIZE.h EEPARAMS.h EESUPPORT.h EXCH.h FLT.h GRID.h GRID_MACROS.h
flt_exchg.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
flt_exchg.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
flt_exchg.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
flt_exchg.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
flt_exchg.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
flt_exchg.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
flt_exchg.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
flt_exchg.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
flt_exchg.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h
flt_exchg.f: FCORI_MACROS.h
flt_functions.f: FLT_CPPOPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
flt_functions.f: CPP_EEMACROS.h EEPARAMS.h SIZE.h GRID.h GRID_MACROS.h
flt_functions.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
flt_functions.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
flt_functions.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
flt_functions.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
flt_functions.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
flt_functions.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
flt_functions.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
flt_functions.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
flt_functions.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
flt_functions.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
flt_init.f: FLT_CPPOPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
flt_init.f: EEPARAMS.h SIZE.h FLT.h GRID.h GRID_MACROS.h DXC_MACROS.h
flt_init.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
flt_init.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
flt_init.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
flt_init.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
flt_init.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
flt_init.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
flt_init.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
flt_init.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
flt_init.f: TANPHIATV_MACROS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
flt_main.f: FLT_CPPOPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
flt_main.f: EEPARAMS.h SIZE.h FLT.h
flt_mdsreadvector.f: FLT_CPPOPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
flt_mdsreadvector.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
flt_mdsreadvector.f: PARAMS_MACROS.h FCORI_MACROS.h
flt_restart.f: FLT_CPPOPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
flt_restart.f: EEPARAMS.h SIZE.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
flt_restart.f: FLT.h
flt_runga2.f: FLT_CPPOPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
flt_runga2.f: EEPARAMS.h SIZE.h DYNVARS.h PARAMS.h PARAMS_MACROS.h
flt_runga2.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
flt_runga2.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
flt_runga2.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
flt_runga2.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
flt_runga2.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
flt_runga2.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
flt_runga2.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
flt_runga2.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
flt_runga2.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
flt_runga2.f: TANPHIATV_MACROS.h FLT.h
flt_traj.f: FLT_CPPOPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
flt_traj.f: EEPARAMS.h SIZE.h DYNVARS.h PARAMS.h PARAMS_MACROS.h
flt_traj.f: FCORI_MACROS.h SOLVE_FOR_PRESSURE.h GRID.h GRID_MACROS.h
flt_traj.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
flt_traj.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
flt_traj.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
flt_traj.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
flt_traj.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
flt_traj.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
flt_traj.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
flt_traj.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
flt_traj.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h FLT.h
flt_up.f: FLT_CPPOPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
flt_up.f: EEPARAMS.h SIZE.h DYNVARS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
flt_up.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
flt_up.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
flt_up.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
flt_up.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
flt_up.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
flt_up.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
flt_up.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
flt_up.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
flt_up.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h FLT.h SOLVE_FOR_PRESSURE.h
calc_gs.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
calc_gs.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GAD.h
calc_gt.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
calc_gt.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GAD.h
calc_gtr1.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
calc_gtr1.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GAD.h TR1.h
gad_advection.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_advection.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
gad_advection.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_advection.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_advection.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_advection.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_advection.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_advection.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_advection.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_advection.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_advection.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_advection.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_biharm_x.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_biharm_x.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_biharm_x.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_biharm_x.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_biharm_x.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_biharm_x.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_biharm_x.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_biharm_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_biharm_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_biharm_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_biharm_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_biharm_y.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_biharm_y.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_biharm_y.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_biharm_y.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_biharm_y.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_biharm_y.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_biharm_y.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_biharm_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_biharm_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_biharm_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_biharm_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_c2_adv_r.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_c2_adv_r.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_c2_adv_r.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_c2_adv_r.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_c2_adv_r.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_c2_adv_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_c2_adv_r.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_c2_adv_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c2_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c2_adv_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c2_adv_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h
gad_c2_adv_r.f: PARAMS_MACROS.h FCORI_MACROS.h
gad_c2_adv_x.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_c2_adv_x.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_c2_adv_x.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_c2_adv_x.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_c2_adv_x.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_c2_adv_x.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_c2_adv_x.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_c2_adv_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c2_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c2_adv_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c2_adv_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_c2_adv_y.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_c2_adv_y.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_c2_adv_y.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_c2_adv_y.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_c2_adv_y.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_c2_adv_y.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_c2_adv_y.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_c2_adv_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c2_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c2_adv_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c2_adv_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_c4_adv_r.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_c4_adv_r.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_c4_adv_r.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_c4_adv_r.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_c4_adv_r.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_c4_adv_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_c4_adv_r.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_c4_adv_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c4_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c4_adv_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c4_adv_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_c4_adv_x.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_c4_adv_x.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_c4_adv_x.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_c4_adv_x.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_c4_adv_x.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_c4_adv_x.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_c4_adv_x.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_c4_adv_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c4_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c4_adv_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c4_adv_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_c4_adv_y.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_c4_adv_y.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_c4_adv_y.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_c4_adv_y.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_c4_adv_y.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_c4_adv_y.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_c4_adv_y.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_c4_adv_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_c4_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_c4_adv_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_c4_adv_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_calc_rhs.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_calc_rhs.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
gad_calc_rhs.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_calc_rhs.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
gad_calc_rhs.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_calc_rhs.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
gad_calc_rhs.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_calc_rhs.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_calc_rhs.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_calc_rhs.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_calc_rhs.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_calc_rhs.f: TANPHIATV_MACROS.h DYNVARS.h GAD.h
gad_del2.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
gad_del2.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
gad_del2.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_del2.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_del2.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_del2.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_del2.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_del2.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_del2.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_del2.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_diff_r.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_diff_r.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_diff_r.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_diff_r.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_diff_r.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_diff_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_diff_r.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_diff_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
gad_diff_r.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_diff_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_diff_r.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_diff_r.f: FCORI_MACROS.h
gad_diff_x.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_diff_x.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_diff_x.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_diff_x.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_diff_x.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_diff_x.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_diff_x.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_diff_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
gad_diff_x.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_diff_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_diff_x.f: TANPHIATV_MACROS.h
gad_diff_y.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_diff_y.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_diff_y.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_diff_y.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_diff_y.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_diff_y.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_diff_y.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_diff_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
gad_diff_y.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_diff_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_diff_y.f: TANPHIATV_MACROS.h
gad_dst3_adv_r.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_dst3_adv_r.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_dst3_adv_r.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_dst3_adv_r.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_dst3_adv_r.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_dst3_adv_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_dst3_adv_r.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst3_adv_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst3_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst3_adv_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst3_adv_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h
gad_dst3_adv_r.f: PARAMS_MACROS.h FCORI_MACROS.h GAD.h
gad_dst3_adv_x.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_dst3_adv_x.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_dst3_adv_x.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_dst3_adv_x.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_dst3_adv_x.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_dst3_adv_x.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_dst3_adv_x.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst3_adv_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst3_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst3_adv_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst3_adv_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst3_adv_y.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_dst3_adv_y.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_dst3_adv_y.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_dst3_adv_y.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_dst3_adv_y.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_dst3_adv_y.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_dst3_adv_y.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_dst3_adv_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_dst3_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_dst3_adv_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_dst3_adv_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_dst3fl_adv_r.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_dst3fl_adv_r.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_dst3fl_adv_r.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_dst3fl_adv_r.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_dst3fl_adv_r.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_dst3fl_adv_r.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_dst3fl_adv_r.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_dst3fl_adv_r.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_dst3fl_adv_r.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_dst3fl_adv_r.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_dst3fl_adv_r.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_dst3fl_adv_r.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
gad_dst3fl_adv_r.f: FCORI_MACROS.h GAD.h
gad_dst3fl_adv_x.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_dst3fl_adv_x.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_dst3fl_adv_x.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_dst3fl_adv_x.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_dst3fl_adv_x.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_dst3fl_adv_x.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_dst3fl_adv_x.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_dst3fl_adv_x.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_dst3fl_adv_x.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_dst3fl_adv_x.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_dst3fl_adv_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_dst3fl_adv_x.f: TANPHIATV_MACROS.h GAD.h
gad_dst3fl_adv_y.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_dst3fl_adv_y.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h
gad_dst3fl_adv_y.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
gad_dst3fl_adv_y.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
gad_dst3fl_adv_y.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
gad_dst3fl_adv_y.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_dst3fl_adv_y.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
gad_dst3fl_adv_y.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
gad_dst3fl_adv_y.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
gad_dst3fl_adv_y.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_dst3fl_adv_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_dst3fl_adv_y.f: TANPHIATV_MACROS.h GAD.h
gad_fluxlimit_adv_r.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_fluxlimit_adv_r.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
gad_fluxlimit_adv_r.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_fluxlimit_adv_r.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_fluxlimit_adv_r.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_fluxlimit_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
gad_fluxlimit_adv_r.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
gad_fluxlimit_adv_r.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_fluxlimit_adv_r.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
gad_fluxlimit_adv_r.f: GAD_FLUX_LIMITER.h
gad_fluxlimit_adv_x.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_fluxlimit_adv_x.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
gad_fluxlimit_adv_x.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_fluxlimit_adv_x.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_fluxlimit_adv_x.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_fluxlimit_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
gad_fluxlimit_adv_x.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
gad_fluxlimit_adv_x.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_fluxlimit_adv_x.f: GAD_FLUX_LIMITER.h
gad_fluxlimit_adv_y.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
gad_fluxlimit_adv_y.f: CPP_EEMACROS.h SIZE.h GRID.h GRID_MACROS.h
gad_fluxlimit_adv_y.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
gad_fluxlimit_adv_y.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
gad_fluxlimit_adv_y.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_fluxlimit_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
gad_fluxlimit_adv_y.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
gad_fluxlimit_adv_y.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
gad_fluxlimit_adv_y.f: GAD_FLUX_LIMITER.h
gad_grad_x.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_grad_x.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_grad_x.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_grad_x.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_grad_x.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_grad_x.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_grad_x.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_grad_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
gad_grad_x.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_grad_x.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_grad_x.f: TANPHIATV_MACROS.h
gad_grad_y.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_grad_y.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_grad_y.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_grad_y.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_grad_y.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_grad_y.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_grad_y.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_grad_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
gad_grad_y.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
gad_grad_y.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
gad_grad_y.f: TANPHIATV_MACROS.h
gad_u3_adv_r.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_u3_adv_r.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_u3_adv_r.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_u3_adv_r.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_u3_adv_r.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_u3_adv_r.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_u3_adv_r.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_u3_adv_r.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_u3_adv_r.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_u3_adv_r.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_u3_adv_r.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_u3_adv_x.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_u3_adv_x.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_u3_adv_x.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_u3_adv_x.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_u3_adv_x.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_u3_adv_x.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_u3_adv_x.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_u3_adv_x.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_u3_adv_x.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_u3_adv_x.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_u3_adv_x.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
gad_u3_adv_y.f: GAD_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
gad_u3_adv_y.f: SIZE.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
gad_u3_adv_y.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
gad_u3_adv_y.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
gad_u3_adv_y.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
gad_u3_adv_y.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
gad_u3_adv_y.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
gad_u3_adv_y.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
gad_u3_adv_y.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
gad_u3_adv_y.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
gad_u3_adv_y.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h
mdsio_byteswapr4.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_byteswapr4.f: CPP_EEMACROS.h
mdsio_byteswapr8.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_byteswapr8.f: CPP_EEMACROS.h
mdsio_findunit.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_findunit.f: CPP_EEMACROS.h EEPARAMS.h
mdsio_gl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mdsio_gl.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_read_rl_vec.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_read_rl_vec.f: CPP_EEMACROS.h EEPARAMS.h
mdsio_read_rs_vec.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_read_rs_vec.f: CPP_EEMACROS.h EEPARAMS.h
mdsio_readfield.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_readfield.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mdsio_readfield.f: FCORI_MACROS.h
mdsio_readtile.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_readtile.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mdsio_readtile.f: FCORI_MACROS.h
mdsio_readvector.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_readvector.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mdsio_readvector.f: FCORI_MACROS.h
mdsio_reclen.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
mdsio_reclen.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_seg4torl.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_seg4torl.f: CPP_EEMACROS.h SIZE.h
mdsio_seg4tors.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_seg4tors.f: CPP_EEMACROS.h SIZE.h
mdsio_seg8torl.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_seg8torl.f: CPP_EEMACROS.h SIZE.h
mdsio_seg8tors.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_seg8tors.f: CPP_EEMACROS.h SIZE.h
mdsio_slice.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mdsio_slice.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mdsio_write_rl_vec.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_write_rl_vec.f: CPP_EEMACROS.h EEPARAMS.h
mdsio_write_rs_vec.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_write_rs_vec.f: CPP_EEMACROS.h EEPARAMS.h
mdsio_writefield.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_writefield.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mdsio_writefield.f: FCORI_MACROS.h
mdsio_writelocal.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_writelocal.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mdsio_writelocal.f: FCORI_MACROS.h
mdsio_writemeta.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_writemeta.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mdsio_writemeta.f: FCORI_MACROS.h
mdsio_writetile.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_writetile.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mdsio_writetile.f: FCORI_MACROS.h
mdsio_writevector.f: MDSIO_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
mdsio_writevector.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
mdsio_writevector.f: PARAMS_MACROS.h FCORI_MACROS.h
mom_calc_hfacz.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
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
mom_calc_ke.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
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
mom_cdscheme.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
mom_cdscheme.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_cdscheme.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_cdscheme.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_cdscheme.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_cdscheme.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_cdscheme.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_cdscheme.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_cdscheme.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_cdscheme.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_cdscheme.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_cdscheme.f: TANPHIATV_MACROS.h SURFACE.h
mom_fluxform.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
mom_fluxform.f: FFIELDS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_fluxform.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_fluxform.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_fluxform.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_fluxform.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_fluxform.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_fluxform.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_fluxform.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_fluxform.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_fluxform.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_fluxform.f: TANPHIATV_MACROS.h SURFACE.h
mom_u_adv_uu.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_u_adv_uu.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_u_adv_vu.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_u_adv_vu.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_u_adv_vu.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_u_adv_vu.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_u_adv_vu.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_adv_vu.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_adv_vu.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_adv_vu.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_adv_vu.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_adv_vu.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_adv_vu.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_adv_vu.f: TANPHIATV_MACROS.h
mom_u_adv_wu.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_u_adv_wu.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_u_adv_wu.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_u_adv_wu.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_u_adv_wu.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_adv_wu.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_adv_wu.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_adv_wu.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_adv_wu.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_adv_wu.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_adv_wu.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_adv_wu.f: TANPHIATV_MACROS.h
mom_u_bottomdrag.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_u_coriolis.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_u_coriolis.f: DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mom_u_coriolis.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
mom_u_coriolis.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
mom_u_coriolis.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_u_coriolis.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_u_coriolis.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_u_coriolis.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_u_coriolis.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_coriolis.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_coriolis.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_coriolis.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_coriolis.f: TANPHIATV_MACROS.h SURFACE.h
mom_u_del2u.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_u_del2u.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_u_del2u.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_u_del2u.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_u_del2u.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_del2u.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_del2u.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_del2u.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_del2u.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_del2u.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_del2u.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_del2u.f: TANPHIATV_MACROS.h DYNVARS.h
mom_u_metric_nh.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_u_metric_nh.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_u_metric_nh.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_u_metric_nh.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_u_metric_nh.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_metric_nh.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_metric_nh.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_metric_nh.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_metric_nh.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_metric_nh.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_metric_nh.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_metric_nh.f: TANPHIATV_MACROS.h
mom_u_metric_sphere.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_u_metric_sphere.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_u_metric_sphere.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_u_metric_sphere.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_u_metric_sphere.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_u_metric_sphere.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_u_metric_sphere.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_metric_sphere.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_u_metric_sphere.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_u_metric_sphere.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_metric_sphere.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_metric_sphere.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_metric_sphere.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_metric_sphere.f: TANPHIATV_MACROS.h
mom_u_rviscflux.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_u_sidedrag.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_u_sidedrag.f: TANPHIATV_MACROS.h
mom_u_xviscflux.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_u_xviscflux.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_u_xviscflux.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_u_xviscflux.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_u_xviscflux.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_xviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_xviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_xviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_xviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_xviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_xviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_xviscflux.f: TANPHIATV_MACROS.h
mom_u_yviscflux.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_u_yviscflux.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_u_yviscflux.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_u_yviscflux.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_u_yviscflux.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_u_yviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_u_yviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_u_yviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_u_yviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_u_yviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_u_yviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_u_yviscflux.f: TANPHIATV_MACROS.h
mom_v_adv_uv.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_v_adv_uv.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_v_adv_uv.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_v_adv_uv.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_v_adv_uv.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_adv_uv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_adv_uv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_adv_uv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_adv_uv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_adv_uv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_adv_uv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_adv_uv.f: TANPHIATV_MACROS.h
mom_v_adv_vv.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_v_adv_vv.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_v_adv_wv.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_v_adv_wv.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_v_adv_wv.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_v_adv_wv.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_v_adv_wv.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_adv_wv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_adv_wv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_adv_wv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_adv_wv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_adv_wv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_adv_wv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_adv_wv.f: TANPHIATV_MACROS.h
mom_v_bottomdrag.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_v_coriolis.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_v_coriolis.f: DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
mom_v_coriolis.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
mom_v_coriolis.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
mom_v_coriolis.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_v_coriolis.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_v_coriolis.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_v_coriolis.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_v_coriolis.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_coriolis.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_coriolis.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_coriolis.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_coriolis.f: TANPHIATV_MACROS.h SURFACE.h
mom_v_del2v.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mom_v_del2v.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
mom_v_del2v.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
mom_v_del2v.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
mom_v_del2v.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_del2v.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_del2v.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_del2v.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_del2v.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_del2v.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_del2v.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_del2v.f: TANPHIATV_MACROS.h
mom_v_metric_nh.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_v_metric_nh.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_v_metric_nh.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_v_metric_nh.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_v_metric_nh.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_metric_nh.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_metric_nh.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_metric_nh.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_metric_nh.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_metric_nh.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_metric_nh.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_metric_nh.f: TANPHIATV_MACROS.h
mom_v_metric_sphere.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_v_metric_sphere.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_v_metric_sphere.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_v_metric_sphere.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_v_metric_sphere.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_v_metric_sphere.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_v_metric_sphere.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_metric_sphere.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_v_metric_sphere.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_v_metric_sphere.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_metric_sphere.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_metric_sphere.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_metric_sphere.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_metric_sphere.f: TANPHIATV_MACROS.h
mom_v_rviscflux.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_v_sidedrag.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_v_sidedrag.f: TANPHIATV_MACROS.h
mom_v_xviscflux.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_v_xviscflux.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_v_xviscflux.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_v_xviscflux.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_v_xviscflux.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_xviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_xviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_xviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_xviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_xviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_xviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_xviscflux.f: TANPHIATV_MACROS.h
mom_v_yviscflux.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_v_yviscflux.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_v_yviscflux.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_v_yviscflux.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_v_yviscflux.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_v_yviscflux.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_v_yviscflux.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_v_yviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_v_yviscflux.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_v_yviscflux.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_v_yviscflux.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_v_yviscflux.f: TANPHIATV_MACROS.h
mom_calc_strain.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_calc_tension.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_hdissip.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
mom_hdissip.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_hdissip.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_hdissip.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_hdissip.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_hdissip.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_hdissip.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_hdissip.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_hdissip.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_hdissip.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_hdissip.f: TANPHIATV_MACROS.h
mom_vecinv.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
mom_vecinv.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_vecinv.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vecinv.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vecinv.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vecinv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vecinv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vecinv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vecinv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vecinv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vecinv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vecinv.f: TANPHIATV_MACROS.h
mom_vi_calc_absvort3.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_vi_calc_absvort3.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_vi_calc_absvort3.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_vi_calc_absvort3.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_vi_calc_absvort3.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_vi_calc_absvort3.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_vi_calc_absvort3.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_calc_absvort3.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_vi_calc_absvort3.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_vi_calc_absvort3.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_calc_absvort3.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_calc_absvort3.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_calc_absvort3.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_calc_absvort3.f: TANPHIATV_MACROS.h
mom_vi_calc_hdiv.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_vi_calc_hdiv.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
mom_vi_calc_hdiv.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_calc_hdiv.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_calc_hdiv.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vi_calc_hdiv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_calc_hdiv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_calc_hdiv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_calc_hdiv.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_calc_hdiv.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
mom_vi_calc_hdiv.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
mom_vi_calc_hdiv.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_calc_ke.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
mom_vi_calc_ke.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_calc_ke.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_calc_ke.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vi_calc_ke.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_calc_ke.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_calc_ke.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_calc_ke.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_calc_ke.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_calc_ke.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_calc_ke.f: TANPHIATV_MACROS.h
mom_vi_calc_relvort3.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_vi_calc_relvort3.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_vi_calc_relvort3.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_vi_calc_relvort3.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_vi_calc_relvort3.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
mom_vi_calc_relvort3.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
mom_vi_calc_relvort3.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_calc_relvort3.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
mom_vi_calc_relvort3.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
mom_vi_calc_relvort3.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_calc_relvort3.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_calc_relvort3.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_calc_relvort3.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_calc_relvort3.f: TANPHIATV_MACROS.h
mom_vi_coriolis.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_vi_del2uv.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
mom_vi_del2uv.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
mom_vi_del2uv.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
mom_vi_del2uv.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
mom_vi_del2uv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
mom_vi_del2uv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_del2uv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
mom_vi_del2uv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
mom_vi_del2uv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
mom_vi_del2uv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
mom_vi_del2uv.f: TANPHIATV_MACROS.h
mom_vi_hdissip.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
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
mom_vi_u_coriolis.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_vi_u_coriolis_c4.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_vi_u_grad_ke.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_vi_u_vertshear.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_vi_u_xviscflux.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_vi_u_xviscflux.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_vi_u_xviscflux.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_vi_u_xviscflux.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_vi_u_xviscflux.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_u_xviscflux.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_u_xviscflux.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_vi_u_xviscflux.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_u_xviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_u_xviscflux.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_u_xviscflux.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mom_vi_u_xviscflux.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
mom_vi_u_xviscflux.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_u_yviscflux.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_vi_u_yviscflux.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_vi_u_yviscflux.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_vi_u_yviscflux.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_vi_u_yviscflux.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_u_yviscflux.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_u_yviscflux.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_vi_u_yviscflux.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_u_yviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_u_yviscflux.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_u_yviscflux.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mom_vi_u_yviscflux.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
mom_vi_u_yviscflux.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_v_coriolis.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_vi_v_coriolis_c4.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_vi_v_grad_ke.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_vi_v_vertshear.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
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
mom_vi_v_xviscflux.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_vi_v_xviscflux.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_vi_v_xviscflux.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_vi_v_xviscflux.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_vi_v_xviscflux.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_v_xviscflux.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_v_xviscflux.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_vi_v_xviscflux.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_v_xviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_v_xviscflux.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_v_xviscflux.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mom_vi_v_xviscflux.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
mom_vi_v_xviscflux.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mom_vi_v_yviscflux.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mom_vi_v_yviscflux.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
mom_vi_v_yviscflux.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
mom_vi_v_yviscflux.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
mom_vi_v_yviscflux.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
mom_vi_v_yviscflux.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
mom_vi_v_yviscflux.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
mom_vi_v_yviscflux.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
mom_vi_v_yviscflux.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
mom_vi_v_yviscflux.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
mom_vi_v_yviscflux.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
mom_vi_v_yviscflux.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
mom_vi_v_yviscflux.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
mon_advcfl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_advcfl.f: MONITOR.h
mon_advcflw.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_advcflw.f: MONITOR.h
mon_advcflw2.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mon_advcflw2.f: EEPARAMS.h MONITOR.h
mon_init.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_init.f: MONITOR.h
mon_ke.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_ke.f: DYNVARS.h MONITOR.h
mon_out.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_out.f: MONITOR.h
mon_printstats_rl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mon_printstats_rl.f: EEPARAMS.h MONITOR.h
mon_printstats_rs.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mon_printstats_rs.f: EEPARAMS.h MONITOR.h
mon_set.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
mon_set.f: MONITOR.h
mon_stats_rl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mon_stats_rl.f: EEPARAMS.h
mon_stats_rs.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
mon_stats_rs.f: EEPARAMS.h
monitor.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
monitor.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
monitor.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
monitor.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
monitor.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
monitor.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
monitor.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
monitor.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
monitor.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
monitor.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
monitor.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h MONITOR.h
obcs_apply_sloc.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
obcs_apply_sloc.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
obcs_apply_sloc.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
obcs_apply_sloc.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
obcs_apply_sloc.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
obcs_apply_sloc.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
obcs_apply_sloc.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
obcs_apply_sloc.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
obcs_apply_sloc.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_apply_sloc.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_apply_sloc.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_apply_sloc.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_apply_sloc.f: TANPHIATV_MACROS.h OBCS.h
obcs_apply_tloc.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
obcs_apply_tloc.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
obcs_apply_tloc.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
obcs_apply_tloc.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
obcs_apply_tloc.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
obcs_apply_tloc.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
obcs_apply_tloc.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
obcs_apply_tloc.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
obcs_apply_tloc.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_apply_tloc.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_apply_tloc.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_apply_tloc.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_apply_tloc.f: TANPHIATV_MACROS.h OBCS.h
obcs_apply_ts.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
obcs_apply_ts.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
obcs_apply_ts.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
obcs_apply_ts.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
obcs_apply_ts.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_apply_ts.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_apply_ts.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_ts.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_apply_ts.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_apply_ts.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_apply_ts.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_apply_ts.f: TANPHIATV_MACROS.h OBCS.h
obcs_apply_uv.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
obcs_apply_uv.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
obcs_apply_uv.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
obcs_apply_uv.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
obcs_apply_uv.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_apply_uv.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_apply_uv.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_uv.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_apply_uv.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_apply_uv.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_apply_uv.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_apply_uv.f: TANPHIATV_MACROS.h OBCS.h
obcs_apply_w.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
obcs_apply_w.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
obcs_apply_w.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
obcs_apply_w.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
obcs_apply_w.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
obcs_apply_w.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
obcs_apply_w.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
obcs_apply_w.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
obcs_apply_w.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
obcs_apply_w.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
obcs_apply_w.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
obcs_apply_w.f: TANPHIATV_MACROS.h OBCS.h
obcs_check.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
obcs_check.f: SIZE.h EEPARAMS.h OBCS.h
obcs_checkpoint.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
obcs_checkpoint.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
obcs_checkpoint.f: FCORI_MACROS.h OBCS.h
obcs_init_fixed.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
obcs_init_fixed.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
obcs_init_fixed.f: FCORI_MACROS.h OBCS.h
obcs_init_variables.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
obcs_init_variables.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
obcs_init_variables.f: PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h OBCS.h
obcs_readparms.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
obcs_readparms.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
obcs_readparms.f: OBCS.h ORLANSKI.h
orlanski_checkpoint.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h
orlanski_checkpoint.f: CPP_EEMACROS.h SIZE.h EEPARAMS.h PARAMS.h
orlanski_checkpoint.f: PARAMS_MACROS.h FCORI_MACROS.h OBCS.h ORLANSKI.h
orlanski_east.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
orlanski_east.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
orlanski_east.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
orlanski_east.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
orlanski_east.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
orlanski_east.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
orlanski_east.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
orlanski_east.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
orlanski_east.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
orlanski_east.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
orlanski_east.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
orlanski_east.f: TANPHIATV_MACROS.h OBCS.h ORLANSKI.h
orlanski_init.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
orlanski_init.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
orlanski_init.f: ORLANSKI.h
orlanski_north.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
orlanski_north.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
orlanski_north.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
orlanski_north.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
orlanski_north.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
orlanski_north.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
orlanski_north.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
orlanski_north.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
orlanski_north.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
orlanski_north.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
orlanski_north.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
orlanski_north.f: TANPHIATV_MACROS.h OBCS.h ORLANSKI.h
orlanski_south.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
orlanski_south.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
orlanski_south.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
orlanski_south.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
orlanski_south.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
orlanski_south.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
orlanski_south.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
orlanski_south.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
orlanski_south.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
orlanski_south.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
orlanski_south.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
orlanski_south.f: TANPHIATV_MACROS.h OBCS.h ORLANSKI.h
orlanski_west.f: OBCS_OPTIONS.h CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
orlanski_west.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
orlanski_west.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
orlanski_west.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
orlanski_west.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
orlanski_west.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
orlanski_west.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
orlanski_west.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
orlanski_west.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
orlanski_west.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
orlanski_west.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
orlanski_west.f: TANPHIATV_MACROS.h OBCS.h ORLANSKI.h
read_fld_xy_rl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
read_fld_xy_rl.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
read_fld_xy_rs.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
read_fld_xy_rs.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
read_fld_xyz_rl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
read_fld_xyz_rl.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
read_fld_xyz_rs.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
read_fld_xyz_rs.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
set_write_global_fld.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
write_fld_xy_rl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
write_fld_xy_rl.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_fld_xy_rs.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
write_fld_xy_rs.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_fld_xyz_rl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
write_fld_xyz_rl.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_fld_xyz_rs.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
write_fld_xyz_rs.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_local_rl.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
write_local_rl.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
write_local_rs.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
write_local_rs.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
timeave_cumul_1k.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timeave_cumul_1k.f: EEPARAMS.h
timeave_cumul_1t.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timeave_cumul_1t.f: EEPARAMS.h
timeave_cumul_2v.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timeave_cumul_2v.f: EEPARAMS.h
timeave_cumulate.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timeave_cumulate.f: EEPARAMS.h
timeave_norma_1k.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timeave_norma_1k.f: EEPARAMS.h
timeave_normaliz.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timeave_normaliz.f: EEPARAMS.h
timeave_reset.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timeave_reset.f: EEPARAMS.h
timeave_reset_1k.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timeave_reset_1k.f: EEPARAMS.h
timeave_statv_write.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timeave_statv_write.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
timeave_statv_write.f: DYNVARS.h TIMEAVE_STATV.h
timeave_statvars.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timeave_statvars.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
timeave_statvars.f: DYNVARS.h TIMEAVE_STATV.h
bar2.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h BAR2.h
barrier.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
barrier.f: BARRIER.h
check_threads.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
comm_stats.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
comm_stats.f: EXCH.h BAR2.h
dfile.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h DFILE.h
different_multiple.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
eeboot.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
eeboot_minimal.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
eeboot_minimal.f: EESUPPORT.h
eedata_example.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
eedie.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
eeintro_msg.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
eeset_parms.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
eewrite_eeenv.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_cycle_ebl.f: SIZE.h EEPARAMS.h EESUPPORT.h EXCH.h
exch_init.f: SIZE.h EEPARAMS.h EESUPPORT.h EXCH.h
exch_jam.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EXCH_JAM.h
exch_jam.f: MPI_INFO.h JAM_INFO.h
exch_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_rl.f: EXCH.h
exch_rl_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_rl_cube.f: EXCH.h
exch_rl_recv_get_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rl_recv_get_x.f: EESUPPORT.h EXCH.h
exch_rl_recv_get_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rl_recv_get_y.f: EESUPPORT.h EXCH.h
exch_rl_send_put_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rl_send_put_x.f: EESUPPORT.h EXCH.h
exch_rl_send_put_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rl_send_put_y.f: EESUPPORT.h EXCH.h
exch_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_rs.f: EXCH.h
exch_rs_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_rs_cube.f: EXCH.h
exch_rs_recv_get_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rs_recv_get_x.f: EESUPPORT.h EXCH.h
exch_rs_recv_get_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rs_recv_get_y.f: EESUPPORT.h EXCH.h
exch_rs_send_put_x.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rs_send_put_x.f: EESUPPORT.h EXCH.h
exch_rs_send_put_y.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_rs_send_put_y.f: EESUPPORT.h EXCH.h
exch_uv_rl_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_uv_rl_cube.f: EESUPPORT.h EXCH.h
exch_uv_rs_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_uv_rs_cube.f: EESUPPORT.h EXCH.h
exch_uv_xy_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_uv_xy_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_uv_xyz_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_uv_xyz_rl.f: EESUPPORT.h
exch_uv_xyz_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_uv_xyz_rs.f: EESUPPORT.h
exch_xy_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_xy_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_xyz_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_xyz_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_z_rl_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_z_rl_cube.f: EESUPPORT.h EXCH.h
exch_z_rs_cube.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
exch_z_rs_cube.f: EESUPPORT.h EXCH.h
exch_z_xy_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_z_xy_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_z_xyz_rl.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
exch_z_xyz_rs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
fool_the_compiler.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
global_max.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
global_max.f: GLOBAL_MAX.h
global_sum.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
global_sum.f: GLOBAL_SUM.h
gsum.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
gsum_jam.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_communication_patterns.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_communication_patterns.f: EEPARAMS.h EESUPPORT.h EXCH.h
ini_jam.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_procs.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
ini_threading_environment.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_threading_environment.f: EESUPPORT.h
main.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
main.f: EESUPPORT.h THE_MODEL_COMMON_BLOCKS.h PARAMS.h PARAMS_MACROS.h
main.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
main.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
main.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
main.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
main.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
main.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
main.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
main.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
main.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h FFIELDS.h SURFACE.h
main.f: TIMEAVE_STATV.h MAIN_PDIRECTIVES1.h MAIN_PDIRECTIVES2.h
memsync.f: CPP_EEOPTIONS.h CPP_EEMACROS.h
nml_filter.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h EEPARAMS.h
print.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
timers.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h EESUPPORT.h
utils.f: CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
calc_buoyancy.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_buoyancy.f: DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_common_factors.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_common_factors.f: DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_common_factors.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
calc_common_factors.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
calc_common_factors.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
calc_common_factors.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_common_factors.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_common_factors.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
calc_common_factors.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_common_factors.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_common_factors.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_common_factors.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_common_factors.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_common_factors.f: TANPHIATV_MACROS.h GW.h
calc_diffusivity.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_diffusivity.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_diffusivity.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
calc_diffusivity.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
calc_diffusivity.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
calc_diffusivity.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
calc_diffusivity.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
calc_diffusivity.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
calc_diffusivity.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_diffusivity.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_diffusivity.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_diffusivity.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
calc_div_ghat.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_div_ghat.f: DYNVARS.h FFIELDS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
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
calc_exact_eta.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_exact_eta.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_exact_eta.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
calc_exact_eta.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
calc_exact_eta.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
calc_exact_eta.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
calc_exact_eta.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
calc_exact_eta.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
calc_exact_eta.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_exact_eta.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_exact_eta.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_exact_eta.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
calc_grad_phi_surf.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_grad_phi_surf.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_grad_phi_surf.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
calc_grad_phi_surf.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
calc_grad_phi_surf.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
calc_grad_phi_surf.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
calc_grad_phi_surf.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_grad_phi_surf.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_grad_phi_surf.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
calc_grad_phi_surf.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_grad_phi_surf.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
calc_grad_phi_surf.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
calc_grad_phi_surf.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
calc_grad_phi_surf.f: SURFACE.h
calc_gw.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
calc_gw.f: FFIELDS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_gw.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_gw.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
calc_gw.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_gw.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_gw.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_gw.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_gw.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_gw.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_gw.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GW.h CG3D.h
calc_ivdc.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
calc_ivdc.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
calc_ivdc.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_ivdc.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
calc_ivdc.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
calc_ivdc.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
calc_ivdc.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
calc_ivdc.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_ivdc.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_ivdc.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_ivdc.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
calc_phi_hyd.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
calc_phi_hyd.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_phi_hyd.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
calc_phi_hyd.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_phi_hyd.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_phi_hyd.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_phi_hyd.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_phi_hyd.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_phi_hyd.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_phi_hyd.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_phi_hyd.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
calc_phi_hyd.f: FCORI_MACROS.h
calc_surf_dr.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_surf_dr.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
calc_surf_dr.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
calc_surf_dr.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
calc_surf_dr.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
calc_surf_dr.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
calc_surf_dr.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
calc_surf_dr.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
calc_surf_dr.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
calc_surf_dr.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
calc_surf_dr.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
calc_surf_dr.f: TANPHIATV_MACROS.h SURFACE.h
calc_viscosity.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
calc_viscosity.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
calc_viscosity.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
calc_viscosity.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
calc_viscosity.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
calc_viscosity.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
calc_viscosity.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
calc_viscosity.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
calc_viscosity.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
calc_viscosity.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
calc_viscosity.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
calc_viscosity.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
cg2d.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
cg2d.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
cg2d.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
cg2d.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
cg2d.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
cg2d.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
cg2d.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
cg2d.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
cg2d.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
cg2d.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
cg2d.f: CG2D.h SURFACE.h
cg3d.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
cg3d.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
cg3d.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
cg3d.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
cg3d.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
cg3d.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
cg3d.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
cg3d.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
cg3d.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
cg3d.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
cg3d.f: CG3D.h
checkpoint.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
checkpoint.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GW.h
checkpoint.f: SOLVE_FOR_PRESSURE3D.h
config_summary.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
config_summary.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
config_summary.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
config_summary.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
config_summary.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
config_summary.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
config_summary.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
config_summary.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
config_summary.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
config_summary.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
config_summary.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
config_summary.f: TANPHIATV_MACROS.h DYNVARS.h
convect.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
convect.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
convect.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
convect.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
convect.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
convect.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
convect.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
convect.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
convect.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
convect.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
convect.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
convective_adjustment.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
convective_adjustment.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
convective_adjustment.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h
convective_adjustment.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
convective_adjustment.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
convective_adjustment.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
convective_adjustment.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
convective_adjustment.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
convective_adjustment.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
convective_adjustment.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
convective_adjustment.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
convective_adjustment.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
convective_adjustment.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
convective_adjustment.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
convective_adjustment.f: TIMEAVE_STATV.h
convective_adjustment_ini.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
convective_adjustment_ini.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
convective_adjustment_ini.f: FCORI_MACROS.h DYNVARS.h GRID.h GRID_MACROS.h
convective_adjustment_ini.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
convective_adjustment_ini.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
convective_adjustment_ini.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
convective_adjustment_ini.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
convective_adjustment_ini.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
convective_adjustment_ini.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
convective_adjustment_ini.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
convective_adjustment_ini.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
convective_adjustment_ini.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
convective_adjustment_ini.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h
convective_adjustment_ini.f: RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
convective_adjustment_ini.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
convective_adjustment_ini.f: TIMEAVE_STATV.h
correction_step.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
correction_step.f: DYNVARS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
correction_step.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
correction_step.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
correction_step.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
correction_step.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
correction_step.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
correction_step.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
correction_step.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
correction_step.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
correction_step.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
correction_step.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
correction_step.f: TANPHIATV_MACROS.h SOLVE_FOR_PRESSURE3D.h
cycle_tracer.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
cycle_tracer.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
do_fields_blocking_exchanges.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
do_fields_blocking_exchanges.f: SIZE.h EEPARAMS.h DYNVARS.h
do_gterm_blocking_exchanges.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
do_gterm_blocking_exchanges.f: SIZE.h EEPARAMS.h DYNVARS.h
do_the_model_io.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
do_the_model_io.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
do_the_model_io.f: DYNVARS.h
dynamics.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
dynamics.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GRID.h
dynamics.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
dynamics.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
dynamics.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
dynamics.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
dynamics.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
dynamics.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
dynamics.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
dynamics.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
dynamics.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h TIMEAVE_STATV.h
external_fields_load.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
external_fields_load.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
external_fields_load.f: FFIELDS.h GRID.h GRID_MACROS.h DXC_MACROS.h
external_fields_load.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
external_fields_load.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
external_fields_load.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
external_fields_load.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
external_fields_load.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
external_fields_load.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
external_fields_load.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
external_fields_load.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
external_fields_load.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
external_fields_load.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
external_fields_load.f: TANPHIATV_MACROS.h DYNVARS.h
external_forcing.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
external_forcing.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
external_forcing.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
external_forcing.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
external_forcing.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
external_forcing.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
external_forcing.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
external_forcing.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
external_forcing.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
external_forcing.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
external_forcing.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
external_forcing.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h FFIELDS.h
external_forcing_surf.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
external_forcing_surf.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
external_forcing_surf.f: FFIELDS.h DYNVARS.h GRID.h GRID_MACROS.h
external_forcing_surf.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
external_forcing_surf.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
external_forcing_surf.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
external_forcing_surf.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
external_forcing_surf.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
external_forcing_surf.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
external_forcing_surf.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
external_forcing_surf.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
external_forcing_surf.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
external_forcing_surf.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
external_forcing_surf.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
find_alpha.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
find_alpha.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
find_rho.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
find_rho.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
forward_step.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
forward_step.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
forward_step.f: FFIELDS.h CG3D.h
freeze.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
freeze.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
freeze.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
freeze.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
freeze.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
freeze.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
freeze.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
freeze.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
freeze.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
freeze.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
grad_sigma.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
grad_sigma.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
grad_sigma.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
grad_sigma.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
grad_sigma.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
grad_sigma.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
grad_sigma.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
grad_sigma.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
grad_sigma.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
grad_sigma.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
grad_sigma.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
grad_sigma.f: FCORI_MACROS.h
impldiff.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
impldiff.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
impldiff.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
impldiff.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
impldiff.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
impldiff.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
impldiff.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
impldiff.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
impldiff.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
impldiff.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
impldiff.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_cartesian_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_cartesian_grid.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_cartesian_grid.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_cartesian_grid.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_cartesian_grid.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_cartesian_grid.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_cartesian_grid.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_cartesian_grid.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_cartesian_grid.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_cartesian_grid.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_cartesian_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
ini_cartesian_grid.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
ini_cartesian_grid.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_cg2d.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_cg2d.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_cg2d.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_cg2d.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_cg2d.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_cg2d.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_cg2d.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_cg2d.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_cg2d.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_cg2d.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_cg2d.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h CG2D.h OBCS.h
ini_cg3d.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_cg3d.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_cg3d.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_cg3d.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_cg3d.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_cg3d.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_cg3d.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_cg3d.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_cg3d.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_cg3d.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_cg3d.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h CG3D.h
ini_cg3d.f: SOLVE_FOR_PRESSURE3D.h OBCS.h
ini_cori.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_cori.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_cori.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_cori.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_cori.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_cori.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_cori.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_cori.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_cori.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_cori.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_cori.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_curvilinear_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_curvilinear_grid.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_curvilinear_grid.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_curvilinear_grid.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_curvilinear_grid.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_curvilinear_grid.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_curvilinear_grid.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_curvilinear_grid.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_curvilinear_grid.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_curvilinear_grid.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_curvilinear_grid.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_curvilinear_grid.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_curvilinear_grid.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_curvilinear_grid.f: TANPHIATV_MACROS.h
ini_depths.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_depths.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_depths.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_depths.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_depths.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_depths.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_depths.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_depths.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_depths.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_depths.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_depths.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_dynvars.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_dynvars.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h GW.h
ini_fields.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_fields.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_forcing.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_forcing.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_forcing.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_forcing.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_forcing.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_forcing.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_forcing.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_forcing.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_forcing.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_forcing.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_forcing.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_forcing.f: TANPHIATV_MACROS.h FFIELDS.h
ini_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_grid.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_grid.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_grid.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_grid.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_grid.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_grid.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_grid.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_grid.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_grid.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_gw.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_gw.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_gw.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_gw.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_gw.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_gw.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_gw.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_gw.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
ini_gw.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_gw.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_gw.f: GW.h
ini_linear_phisurf.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_linear_phisurf.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_linear_phisurf.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_linear_phisurf.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_linear_phisurf.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_linear_phisurf.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_linear_phisurf.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_linear_phisurf.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_linear_phisurf.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_linear_phisurf.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_linear_phisurf.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h
ini_linear_phisurf.f: RA_MACROS.h RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h
ini_linear_phisurf.f: MASKS_MACROS.h TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_linear_phisurf.f: SURFACE.h
ini_masks_etc.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_masks_etc.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
ini_masks_etc.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_masks_etc.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_masks_etc.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_masks_etc.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_masks_etc.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_masks_etc.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_masks_etc.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_masks_etc.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_masks_etc.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_masks_etc.f: TANPHIATV_MACROS.h SURFACE.h
ini_mixing.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_mixing.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_mixing.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_mixing.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
ini_mixing.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_mixing.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_mixing.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_mixing.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_mixing.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_mixing.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_mixing.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_p_ground.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h GRID.h
ini_p_ground.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
ini_p_ground.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_p_ground.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
ini_p_ground.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_p_ground.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
ini_p_ground.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_p_ground.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
ini_p_ground.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
ini_p_ground.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
ini_p_ground.f: TANPHIATV_MACROS.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_p_ground.f: FCORI_MACROS.h
ini_parms.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_parms.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_parms.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_parms.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_parms.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_parms.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_parms.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_parms.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_parms.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_parms.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_parms.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_pnh.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_pnh.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_pnh.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_pnh.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_pnh.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_pnh.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_pnh.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_pnh.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_pnh.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_pnh.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_pnh.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_psurf.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_psurf.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_psurf.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_psurf.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_psurf.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_psurf.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_psurf.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_psurf.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_psurf.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_psurf.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_psurf.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_salt.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_salt.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_salt.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_salt.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_salt.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_salt.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_salt.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_salt.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_salt.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_salt.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_salt.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_spherical_polar_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
ini_spherical_polar_grid.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
ini_spherical_polar_grid.f: FCORI_MACROS.h GRID.h GRID_MACROS.h DXC_MACROS.h
ini_spherical_polar_grid.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
ini_spherical_polar_grid.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
ini_spherical_polar_grid.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_spherical_polar_grid.f: HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_spherical_polar_grid.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
ini_spherical_polar_grid.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_spherical_polar_grid.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_spherical_polar_grid.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
ini_spherical_polar_grid.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h
ini_spherical_polar_grid.f: XC_MACROS.h YC_MACROS.h RA_MACROS.h RAW_MACROS.h
ini_spherical_polar_grid.f: RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_spherical_polar_grid.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
ini_theta.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_theta.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_theta.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_theta.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_theta.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_theta.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_theta.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_theta.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_theta.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_theta.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_theta.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_tr1.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_tr1.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_tr1.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_tr1.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_tr1.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_tr1.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_tr1.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_tr1.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_tr1.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_tr1.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_tr1.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h TR1.h
ini_vel.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
ini_vel.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
ini_vel.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
ini_vel.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
ini_vel.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
ini_vel.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
ini_vel.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
ini_vel.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_vel.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_vel.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_vel.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h DYNVARS.h
ini_vertical_grid.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
ini_vertical_grid.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
ini_vertical_grid.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
ini_vertical_grid.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
ini_vertical_grid.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
ini_vertical_grid.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
ini_vertical_grid.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
ini_vertical_grid.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
ini_vertical_grid.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
ini_vertical_grid.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
ini_vertical_grid.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
ini_vertical_grid.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h
initialise_fixed.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
initialise_fixed.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
initialise_varia.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
initialise_varia.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
initialise_varia.f: DYNVARS.h
integrate_for_w.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
integrate_for_w.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
integrate_for_w.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
integrate_for_w.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
integrate_for_w.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
integrate_for_w.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
integrate_for_w.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
integrate_for_w.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
integrate_for_w.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
integrate_for_w.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
integrate_for_w.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
integrate_for_w.f: TANPHIATV_MACROS.h
modeldata_example.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
modeldata_example.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
open_copy_data_file.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
open_copy_data_file.f: EEPARAMS.h
packages_boot.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
packages_boot.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
packages_check.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
packages_check.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
packages_check_error.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
packages_check_error.f: EEPARAMS.h
packages_init_fixed.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
packages_init_fixed.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
packages_init_variables.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
packages_init_variables.f: SIZE.h EEPARAMS.h PARAMS.h PARAMS_MACROS.h
packages_init_variables.f: FCORI_MACROS.h
packages_readparms.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
packages_readparms.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
plot_field.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
plot_field.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
read_write.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
read_write_rec.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
read_write_rec.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
set_defaults.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
set_defaults.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
set_defaults.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
set_defaults.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
set_defaults.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
set_defaults.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
set_defaults.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
set_defaults.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
set_defaults.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
set_defaults.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
set_defaults.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
set_defaults.f: TANPHIATV_MACROS.h
solve_for_pressure.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
solve_for_pressure.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
solve_for_pressure.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h
solve_for_pressure.f: DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h
solve_for_pressure.f: DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h
solve_for_pressure.f: HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
solve_for_pressure.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
solve_for_pressure.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
solve_for_pressure.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
solve_for_pressure.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
solve_for_pressure.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
solve_for_pressure.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
solve_for_pressure.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
solve_for_pressure.f: TANPHIATV_MACROS.h SURFACE.h SOLVE_FOR_PRESSURE3D.h
solve_for_pressure.f: GW.h OBCS.h SOLVE_FOR_PRESSURE.h
state_summary.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
state_summary.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
state_summary.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
state_summary.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
state_summary.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
state_summary.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
state_summary.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
state_summary.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
state_summary.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
state_summary.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
state_summary.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
state_summary.f: TANPHIATV_MACROS.h DYNVARS.h
swfrac.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h
the_correction_step.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
the_correction_step.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
the_correction_step.f: DYNVARS.h
the_main_loop.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
the_main_loop.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
the_model_main.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
the_model_main.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
the_model_main.f: DYNVARS.h CG3D.h
thermodynamics.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
thermodynamics.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h
thermodynamics.f: DYNVARS.h GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
thermodynamics.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
thermodynamics.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
thermodynamics.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
thermodynamics.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
thermodynamics.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
thermodynamics.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
thermodynamics.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
thermodynamics.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
thermodynamics.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h GAD.h TIMEAVE_STATV.h
timestep.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h DYNVARS.h
timestep.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
timestep.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
timestep.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
timestep.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h RECIP_DXC_MACROS.h
timestep.f: RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h
timestep.f: RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h
timestep.f: RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
timestep.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
timestep.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
timestep.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
timestep_tracer.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
timestep_tracer.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GAD.h
timestep_tracer.f: GRID.h GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h
timestep_tracer.f: DXG_MACROS.h DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h
timestep_tracer.f: DYG_MACROS.h DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h
timestep_tracer.f: HFACW_MACROS.h RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h
timestep_tracer.f: RECIP_DXG_MACROS.h RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h
timestep_tracer.f: RECIP_DYF_MACROS.h RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h
timestep_tracer.f: RECIP_HFACC_MACROS.h RECIP_HFACS_MACROS.h
timestep_tracer.f: RECIP_HFACW_MACROS.h XC_MACROS.h YC_MACROS.h RA_MACROS.h
timestep_tracer.f: RAW_MACROS.h RAS_MACROS.h MASKW_MACROS.h MASKS_MACROS.h
timestep_tracer.f: TANPHIATU_MACROS.h TANPHIATV_MACROS.h SURFACE.h
update_cg2d.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
update_cg2d.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h GRID_MACROS.h
update_cg2d.f: DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h DXV_MACROS.h
update_cg2d.f: DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h DYU_MACROS.h
update_cg2d.f: HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
update_cg2d.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
update_cg2d.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
update_cg2d.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
update_cg2d.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
update_cg2d.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
update_cg2d.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
update_cg2d.f: TANPHIATV_MACROS.h SURFACE.h CG2D.h OBCS.h
update_surf_dr.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h
update_surf_dr.f: EEPARAMS.h PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h GRID.h
update_surf_dr.f: GRID_MACROS.h DXC_MACROS.h DXF_MACROS.h DXG_MACROS.h
update_surf_dr.f: DXV_MACROS.h DYC_MACROS.h DYF_MACROS.h DYG_MACROS.h
update_surf_dr.f: DYU_MACROS.h HFACC_MACROS.h HFACS_MACROS.h HFACW_MACROS.h
update_surf_dr.f: RECIP_DXC_MACROS.h RECIP_DXF_MACROS.h RECIP_DXG_MACROS.h
update_surf_dr.f: RECIP_DXV_MACROS.h RECIP_DYC_MACROS.h RECIP_DYF_MACROS.h
update_surf_dr.f: RECIP_DYG_MACROS.h RECIP_DYU_MACROS.h RECIP_HFACC_MACROS.h
update_surf_dr.f: RECIP_HFACS_MACROS.h RECIP_HFACW_MACROS.h XC_MACROS.h
update_surf_dr.f: YC_MACROS.h RA_MACROS.h RAW_MACROS.h RAS_MACROS.h
update_surf_dr.f: MASKW_MACROS.h MASKS_MACROS.h TANPHIATU_MACROS.h
update_surf_dr.f: TANPHIATV_MACROS.h SURFACE.h
write_state.f: CPP_OPTIONS.h CPP_EEOPTIONS.h CPP_EEMACROS.h SIZE.h EEPARAMS.h
write_state.f: PARAMS.h PARAMS_MACROS.h FCORI_MACROS.h DYNVARS.h
