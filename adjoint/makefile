SHELL		 = /bin/tcsh
RM               = rm -f
LOC              = $(PWD)
TAMC             = tamc
DEST             = .
SOURCE_CODE      = ../bin
ADJOINT_SCRIPT   = ../adjoint
BASE             = ..
TAMCFLAGS        = -adjoint -i4 -r4
PKG		= $(BASE)/pkg

DIFF_FLAGS       = -module the_main_sens		\
		   -input  '    xx_theta_dummy		\
				xx_salt_dummy		\
				xx_hflux_dummy		\
				xx_sflux_dummy		\
				xx_tauu_dummy		\
				xx_tauv_dummy		\
				xx_sss_dummy		\
				xx_sst_dummy'		\
                   -output 'fc'

TAMCFLAG         = $(TAMCFLAGS) $(DIFF_FLAGS) -l list_uv -admark ad


# Source files for the MITgcmUV
SRC_MODEL	= $(PKG)/autodiff/read_write.flow	\
		$(PKG)/autodiff/read_write_fld.flow	\
		$(PKG)/autodiff/read_write_rec.flow	\
		$(PKG)/autodiff/cg2d.flow		\
		$(PKG)/autodiff/initialisation.flow	\
		$(PKG)/autodiff/diags.flow		\
		$(PKG)/autodiff/dummy_in_stepping.flow	\
		$(PKG)/autodiff/external_fields_load.flow	\
		$(PKG)/autodiff/eesupp.flow		\
		$(PKG)/autodiff/active_file.flow	\
		$(PKG)/autodiff/write_state.flow	\
		$(PKG)/autodiff/write_time_averages.flow\
		$(PKG)/autodiff/exch_ad.flow		\
		$(PKG)/autodiff/global_sum_ad.flow	\
		$(PKG)/autodiff/global_max_ad.flow	\
		$(PKG)/autodiff/mdsio.flow		\
		$(PKG)/autodiff/checkpoint.flow		\
		$(PKG)/autodiff/print_message.flow	\
		$(PKG)/autodiff/open_copy_data_file.flow\
		  		  				\
		$(SOURCE_CODE)/grad_sigma.f			\
		$(SOURCE_CODE)/calc_buoyancy.f			\
		$(SOURCE_CODE)/calc_common_factors.f		\
		$(SOURCE_CODE)/calc_diffusivity.f		\
		$(SOURCE_CODE)/calc_div_ghat.f			\
		$(SOURCE_CODE)/calc_grad_phi_surf.f		\
		$(SOURCE_CODE)/calc_gs.f			\
		$(SOURCE_CODE)/calc_gt.f			\
		$(SOURCE_CODE)/calc_ivdc.f			\
		$(SOURCE_CODE)/calc_mom_rhs.f			\
		$(SOURCE_CODE)/calc_phi_hyd.f			\
		$(SOURCE_CODE)/comm_stats.f			\
		$(SOURCE_CODE)/convect.f			\
		$(SOURCE_CODE)/convective_adjustment.f		\
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
		$(SOURCE_CODE)/freeze.f				\
		$(SOURCE_CODE)/get_offline_fields.f		\
		$(SOURCE_CODE)/impldiff.f			\
		$(SOURCE_CODE)/ini_cartesian_grid.f		\
		$(SOURCE_CODE)/ini_cg2d.f			\
		$(SOURCE_CODE)/ini_communication_patterns.f	\
		$(SOURCE_CODE)/ini_cori.f			\
		$(SOURCE_CODE)/ini_depths.f			\
		$(SOURCE_CODE)/ini_fields.f			\
		$(SOURCE_CODE)/ini_forcing.f			\
		$(SOURCE_CODE)/ini_grid.f			\
		$(SOURCE_CODE)/ini_masks_etc.f			\
		$(SOURCE_CODE)/ini_procs.f			\
		$(SOURCE_CODE)/ini_psurf.f			\
		$(SOURCE_CODE)/ini_salt.f			\
		$(SOURCE_CODE)/ini_spherical_polar_grid.f	\
		$(SOURCE_CODE)/ini_theta.f			\
		$(SOURCE_CODE)/ini_uvel.f			\
		$(SOURCE_CODE)/ini_vertical_grid.f		\
		$(SOURCE_CODE)/ini_vvel.f			\
		$(SOURCE_CODE)/ini_wvel.f			\
		$(SOURCE_CODE)/initialise_varia.f		\
		$(SOURCE_CODE)/integrate_for_w.f		\
		$(SOURCE_CODE)/modeldata_example.f		\
		$(SOURCE_CODE)/nml_filter.f			\
		$(SOURCE_CODE)/packages_init_variables.f	\
		$(SOURCE_CODE)/plot_field.f			\
		$(SOURCE_CODE)/solve_for_pressure.f		\
		$(SOURCE_CODE)/state_summary.f			\
		$(SOURCE_CODE)/timestep.f			\
		$(SOURCE_CODE)/the_correction_step.f		\
		$(SOURCE_CODE)/the_main_sens.f			\
		$(SOURCE_CODE)/timestep_tracer.f		\
		$(SOURCE_CODE)/swfrac.f				\
		  						\
		$(SOURCE_CODE)/kpp_calc.f			\
		$(SOURCE_CODE)/kpp_calc_diff.f			\
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
		$(SOURCE_CODE)/cost_test.f			\
		  						\


#--------------------------------------------------------
# rules
#--------------------------------------------------------

adcode:
	cd $(DEST) ; cat $(SRC_MODEL) >! tamc_code_ecco.f
admodel:
	cd $(DEST) ; $(TAMC) $(TAMCFLAG) tamc_code_ecco.f	\
           >! tamc_code_ecco.prot

adchange:
	${ADJOINT_SCRIPT}/adjoint_ecco_sed.com \
           >! tamc_code_ecco_sed_ad.f ;  \
        cp $(PKG)/autodiff/adjoint_model.F		\
           $(PKG)/autodiff/adjoint_model.F_old ;	\
        cat tamc_code_ecco_sed_ad.f >> $(PKG)/autodiff/adjoint_model.F
adrestore:
	cp $(PKG)/autodiff/adjoint_model.F_template	\
           $(PKG)/autodiff/adjoint_model.F
adall:  adcode admodel
