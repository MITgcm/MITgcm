SHELL		 = /bin/tcsh
RM               = rm -f
LOC              = $(PWD)
TAMC             = tamc
TAF              = ~fastopt/bin/taf
DEST             = .
SOURCE_CODE      = ../bin
ADJOINT_SCRIPT   = ../adjoint
BASE             = ..
PKG		= $(BASE)/pkg
ADTAMCFLAGS      = -reverse -i4 -r4 -admark  ad
ADTAFFLAGS       = -reverse -i4 -r4 -admark  ad
TLTAMCFLAGS      = -forward -i4 -r4 -ftlmark g_
TLTAFFLAGS       = -forward -i4 -r4 -ftlmark g_
# -warn_step -flow taf_flow.log

DIFF_FLAGS       = -toplevel the_main_loop		\
		   -input  '    xx_theta_dummy		\
				xx_salt_dummy		\
                                xx_tr1_dummy		\
				xx_hflux_dummy		\
				xx_sflux_dummy		\
				xx_tauu_dummy		\
				xx_tauv_dummy		\
				xx_sss_dummy		\
				xx_sst_dummy		\
				xx_diffkr_dummy		\
				xx_kapgm_dummy'		\
                   -output 'fc'

ADTAMCFLAG         = $(ADTAMCFLAGS) $(DIFF_FLAGS)
ADTAFFLAG          = $(ADTAFFLAGS)  $(DIFF_FLAGS)

TLTAMCFLAG         = $(TLTAMCFLAGS) $(DIFF_FLAGS)
TLTAFFLAG          = $(TLTAFFLAGS)  $(DIFF_FLAGS)

# Source files for the MITgcmUV
SRC_MODEL	= \
		$(PKG)/autodiff/read_write.flow	\
		$(PKG)/autodiff/read_write_fld.flow	\
		$(PKG)/autodiff/read_write_rec.flow	\
		$(PKG)/autodiff/diags.flow		\
		$(PKG)/autodiff/cg2d.flow		\
		$(PKG)/autodiff/debug.flow		\
		$(PKG)/autodiff/dummy_in_stepping.flow	\
		$(PKG)/autodiff/external_fields_load.flow	\
		$(PKG)/autodiff/eesupp.flow		\
		$(PKG)/autodiff/active_file.flow	\
		$(PKG)/autodiff/write_state.flow	\
		$(PKG)/autodiff/ecco_check_exp.flow	\
		$(PKG)/autodiff/exch_ad.flow		\
		$(PKG)/autodiff/exch_z_ad.flow		\
		$(PKG)/autodiff/exch_uv_ad.flow		\
		$(PKG)/autodiff/global_sum_ad.flow	\
		$(PKG)/autodiff/global_max_ad.flow	\
		$(PKG)/autodiff/initialisation.flow	\
		$(PKG)/autodiff/mdsio.flow		\
		$(PKG)/autodiff/monitor.flow		\
		$(PKG)/autodiff/checkpoint.flow		\
		$(PKG)/autodiff/print_message.flow	\
		$(PKG)/autodiff/open_copy_data_file.flow\
		$(PKG)/autodiff/timeave.flow		\
		  		  				\
		$(SOURCE_CODE)/adams_bashforth2.f		\
		$(SOURCE_CODE)/calc_buoyancy.f			\
		$(SOURCE_CODE)/calc_common_factors.f		\
		$(SOURCE_CODE)/calc_diffusivity.f		\
		$(SOURCE_CODE)/calc_div_ghat.f			\
		$(SOURCE_CODE)/calc_grad_phi_surf.f		\
		$(SOURCE_CODE)/calc_gs.f			\
		$(SOURCE_CODE)/calc_gt.f			\
		$(SOURCE_CODE)/calc_gtr1.f			\
		$(SOURCE_CODE)/calc_ivdc.f			\
		$(SOURCE_CODE)/calc_phi_hyd.f			\
		$(SOURCE_CODE)/calc_viscosity.f			\
		$(SOURCE_CODE)/comm_stats.f			\
		$(SOURCE_CODE)/convect.f			\
		$(SOURCE_CODE)/convective_adjustment.f		\
		$(SOURCE_CODE)/convective_adjustment_ini.f	\
		$(SOURCE_CODE)/correction_step.f		\
		$(SOURCE_CODE)/cycle_tracer.f			\
		$(SOURCE_CODE)/different_multiple.f		\
		$(SOURCE_CODE)/do_fields_blocking_exchanges.f	\
		$(SOURCE_CODE)/do_the_model_io.f		\
		$(SOURCE_CODE)/dynamics.f			\
		$(SOURCE_CODE)/external_forcing.f		\
		$(SOURCE_CODE)/external_forcing_surf.f		\
		$(SOURCE_CODE)/find_alpha.f			\
		$(SOURCE_CODE)/find_rho.f			\
		$(SOURCE_CODE)/forward_step.f			\
		$(SOURCE_CODE)/freeze.f				\
								\
		$(SOURCE_CODE)/gad_advection.f			\
		$(SOURCE_CODE)/gad_biharm_x.f			\
		$(SOURCE_CODE)/gad_biharm_y.f			\
		$(SOURCE_CODE)/gad_c2_adv_r.f			\
		$(SOURCE_CODE)/gad_c2_adv_x.f			\
		$(SOURCE_CODE)/gad_c2_adv_y.f			\
		$(SOURCE_CODE)/gad_c4_adv_r.f			\
		$(SOURCE_CODE)/gad_c4_adv_x.f			\
		$(SOURCE_CODE)/gad_c4_adv_y.f			\
		$(SOURCE_CODE)/gad_calc_rhs.f			\
		$(SOURCE_CODE)/gad_del2.f			\
		$(SOURCE_CODE)/gad_diff_r.f			\
		$(SOURCE_CODE)/gad_diff_x.f			\
		$(SOURCE_CODE)/gad_diff_y.f			\
		$(SOURCE_CODE)/gad_dst3_adv_x.f                 \
		$(SOURCE_CODE)/gad_dst3_adv_y.f                 \
		$(SOURCE_CODE)/gad_dst3_adv_r.f			\
		$(SOURCE_CODE)/gad_dst3fl_adv_x.f               \
		$(SOURCE_CODE)/gad_dst3fl_adv_y.f               \
		$(SOURCE_CODE)/gad_dst3fl_adv_r.f		\
		$(SOURCE_CODE)/gad_fluxlimit_adv_r.f		\
		$(SOURCE_CODE)/gad_fluxlimit_adv_x.f		\
		$(SOURCE_CODE)/gad_fluxlimit_adv_y.f		\
		$(SOURCE_CODE)/gad_grad_x.f			\
		$(SOURCE_CODE)/gad_grad_y.f			\
		$(SOURCE_CODE)/gad_u3_adv_r.f			\
		$(SOURCE_CODE)/gad_u3_adv_x.f			\
		$(SOURCE_CODE)/gad_u3_adv_y.f			\
								\
		$(SOURCE_CODE)/grad_sigma.f			\
		$(SOURCE_CODE)/impldiff.f			\
		$(SOURCE_CODE)/ini_cartesian_grid.f		\
		$(SOURCE_CODE)/ini_cg2d.f			\
		$(SOURCE_CODE)/ini_communication_patterns.f	\
		$(SOURCE_CODE)/ini_cori.f			\
		$(SOURCE_CODE)/ini_curvilinear_grid.f		\
		$(SOURCE_CODE)/ini_depths.f			\
		$(SOURCE_CODE)/ini_fields.f			\
		$(SOURCE_CODE)/ini_forcing.f			\
		$(SOURCE_CODE)/ini_grid.f			\
		$(SOURCE_CODE)/ini_linear_phisurf.f		\
		$(SOURCE_CODE)/ini_masks_etc.f			\
		$(SOURCE_CODE)/ini_mixing.f			\
		$(SOURCE_CODE)/ini_p_ground.f			\
		$(SOURCE_CODE)/ini_pnh.f			\
		$(SOURCE_CODE)/ini_procs.f			\
		$(SOURCE_CODE)/ini_psurf.f			\
		$(SOURCE_CODE)/ini_salt.f			\
		$(SOURCE_CODE)/ini_spherical_polar_grid.f	\
		$(SOURCE_CODE)/ini_theta.f			\
		$(SOURCE_CODE)/ini_tr1.f			\
		$(SOURCE_CODE)/ini_dynvars.f			\
		$(SOURCE_CODE)/ini_vel.f			\
		$(SOURCE_CODE)/ini_vertical_grid.f		\
		$(SOURCE_CODE)/initialise_varia.f		\
		$(SOURCE_CODE)/integrate_for_w.f		\
		$(SOURCE_CODE)/modeldata_example.f		\
								\
		$(SOURCE_CODE)/mom_fluxform.f			\
		$(SOURCE_CODE)/mom_hdissip.f			\
		$(SOURCE_CODE)/mom_vecinv.f			\
		$(SOURCE_CODE)/mom_calc_hfacz.f			\
		$(SOURCE_CODE)/mom_calc_ke.f			\
		$(SOURCE_CODE)/mom_calc_strain.f		\
		$(SOURCE_CODE)/mom_calc_tension.f		\
		$(SOURCE_CODE)/mom_cdscheme.f			\
		$(SOURCE_CODE)/mom_u_adv_uu.f			\
		$(SOURCE_CODE)/mom_u_adv_vu.f			\
		$(SOURCE_CODE)/mom_u_adv_wu.f			\
		$(SOURCE_CODE)/mom_u_bottomdrag.f		\
		$(SOURCE_CODE)/mom_u_del2u.f			\
		$(SOURCE_CODE)/mom_u_metric_nh.f		\
		$(SOURCE_CODE)/mom_u_metric_sphere.f		\
		$(SOURCE_CODE)/mom_u_rviscflux.f		\
		$(SOURCE_CODE)/mom_u_sidedrag.f			\
		$(SOURCE_CODE)/mom_u_xviscflux.f		\
		$(SOURCE_CODE)/mom_u_yviscflux.f		\
		$(SOURCE_CODE)/mom_v_adv_uv.f			\
		$(SOURCE_CODE)/mom_v_adv_vv.f			\
		$(SOURCE_CODE)/mom_v_adv_wv.f			\
		$(SOURCE_CODE)/mom_v_bottomdrag.f		\
		$(SOURCE_CODE)/mom_v_del2v.f			\
		$(SOURCE_CODE)/mom_v_metric_nh.f		\
		$(SOURCE_CODE)/mom_v_metric_sphere.f		\
		$(SOURCE_CODE)/mom_v_rviscflux.f		\
		$(SOURCE_CODE)/mom_v_sidedrag.f			\
		$(SOURCE_CODE)/mom_v_xviscflux.f		\
		$(SOURCE_CODE)/mom_v_yviscflux.f		\
		$(SOURCE_CODE)/mom_vi_calc_absvort3.f		\
		$(SOURCE_CODE)/mom_vi_calc_hdiv.f		\
		$(SOURCE_CODE)/mom_vi_calc_ke.f			\
		$(SOURCE_CODE)/mom_vi_calc_relvort3.f		\
		$(SOURCE_CODE)/mom_vi_coriolis.f		\
		$(SOURCE_CODE)/mom_vi_del2uv.f			\
		$(SOURCE_CODE)/mom_vi_hdissip.f			\
		$(SOURCE_CODE)/mom_vi_u_coriolis.f		\
		$(SOURCE_CODE)/mom_vi_u_grad_ke.f		\
		$(SOURCE_CODE)/mom_vi_u_vertshear.f		\
		$(SOURCE_CODE)/mom_vi_v_coriolis.f		\
		$(SOURCE_CODE)/mom_vi_v_grad_ke.f		\
		$(SOURCE_CODE)/mom_vi_v_vertshear.f		\
		  						\
		$(SOURCE_CODE)/packages_init_variables.f	\
		$(SOURCE_CODE)/plot_field.f			\
		$(SOURCE_CODE)/solve_for_pressure.f		\
		$(SOURCE_CODE)/state_summary.f			\
		$(SOURCE_CODE)/timestep.f			\
		$(SOURCE_CODE)/the_correction_step.f		\
		$(SOURCE_CODE)/the_main_loop.f			\
		$(SOURCE_CODE)/thermodynamics.f			\
		$(SOURCE_CODE)/timestep_tracer.f		\
		$(SOURCE_CODE)/swfrac.f				\
		  						\
		$(SOURCE_CODE)/kpp_calc.f			\
		$(SOURCE_CODE)/kpp_calc_diff.f			\
		$(SOURCE_CODE)/kpp_calc_visc.f			\
		$(SOURCE_CODE)/kpp_do_diags.f			\
		$(SOURCE_CODE)/kpp_init.f			\
		$(SOURCE_CODE)/kpp_open_diags.f			\
		$(SOURCE_CODE)/kpp_routines.f			\
		$(SOURCE_CODE)/kpp_transport_s.f		\
		$(SOURCE_CODE)/kpp_transport_t.f		\
		  						\
		$(SOURCE_CODE)/gmredi_calc_diff.f		\
		$(SOURCE_CODE)/gmredi_calc_tensor.f		\
		$(SOURCE_CODE)/gmredi_diags.f			\
		$(SOURCE_CODE)/gmredi_init.f			\
		$(SOURCE_CODE)/gmredi_rtransport.f		\
		$(SOURCE_CODE)/gmredi_slope_limit.f		\
		$(SOURCE_CODE)/gmredi_xtransport.f		\
		$(SOURCE_CODE)/gmredi_ytransport.f		\
								\
		$(SOURCE_CODE)/ctrl_map_ini.f			\
		$(SOURCE_CODE)/ctrl_map_forcing.f		\
		  						\
		$(SOURCE_CODE)/cost_final.f			\
		$(SOURCE_CODE)/cost_init.f			\
		$(SOURCE_CODE)/cost_tile.f			\
		$(SOURCE_CODE)/cost_test.f			\
		$(SOURCE_CODE)/cost_tracer.f			\
		  						\


#--------------------------------------------------------
# rules
#--------------------------------------------------------

allcode:
	cd $(DEST) ; cat $(SRC_MODEL) >! tamc_code_ecco.f

admodeltamc:
	cd $(DEST) ; $(TAMC) $(ADTAMCFLAG) tamc_code_ecco.f	\
           >! tamc_code_ecco_ad.prot

admodeltaf:
	cd $(DEST) ; $(TAF) $(ADTAFFLAG) tamc_code_ecco.f

tlmodeltamc:
	cd $(DEST) ; $(TAMC) $(TLTAMCFLAG) tamc_code_ecco.f	\
           >! tamc_code_ecco_tl.prot

tlmodeltaf:
	cd $(DEST) ; $(TAF) $(TLTAFFLAG) tamc_code_ecco.f

adchange:
	${ADJOINT_SCRIPT}/adjoint_ecco_sed.com \
           >! tamc_code_ecco_sed_ad.f ;  \
        cp $(PKG)/autodiff/adjoint_model.F		\
           $(PKG)/autodiff/adjoint_model.F_old ;	\
        cat tamc_code_ecco_sed_ad.f >> $(PKG)/autodiff/adjoint_model.F
adrestore:
	cp $(PKG)/autodiff/adjoint_model.F_template	\
           $(PKG)/autodiff/adjoint_model.F

tltamc:  allcode tlmodeltamc

tltaf:  allcode tlmodeltaf

adtamc:  adrestore allcode admodeltamc

adtaf:  adrestore allcode admodeltaf
