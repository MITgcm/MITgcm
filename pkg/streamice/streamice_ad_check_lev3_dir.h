C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_ad_check_lev3_dir.h,v 1.1 2013/06/12 21:30:22 dgoldberg Exp $
C $Name:  $

#ifdef ALLOW_STREAMICE

CADJ STORE area_shelf_streamice = tapelev3, key = ilev_3
CADJ STORE streamice_hmask = tapelev3, key = ilev_3
CADJ STORE u_streamice = tapelev3, key = ilev_3
CADJ STORE v_streamice = tapelev3, key = ilev_3
CADJ STORE h_streamice = tapelev3, key = ilev_3
CADJ STORE visc_streamice = tapelev3, key = ilev_3
CADJ STORE tau_beta_eff_streamice = tapelev3, key = ilev_3
CADJ STORE streamice_cg_a1 = tapelev3, key = ilev_3
CADJ STORE streamice_cg_a2 = tapelev3, key = ilev_3
CADJ STORE streamice_cg_a3 = tapelev3, key = ilev_3
CADJ STORE streamice_cg_a4 = tapelev3, key = ilev_3

#ifdef ALLOW_STREAMICE_2DTRACER
CADJ STORE trac2d
CADJ &     = tapelev3, key = ilev_3
#endif


CADJ STORE cost_func1_streamice
CADJ &     = tapelev3, key = ilev_3

CADJ STORE surf_el_streamice
CADJ &     = tapelev3, key=ilev_3
CADJ STORE float_frac_streamice
CADJ &     = tapelev3, key=ilev_3

CADJ STORE c_basal_friction
CADJ &     = tapelev3, key=ilev_3

# ifdef STREAMICE_HYBRID_STRESS
CADJ STORE visc_streamice_full
CADJ &     = tapelev3, key = ilev_3
# endif

# ifdef ALLOW_GENTIM2D_CONTROL
CADJ STORE bdot_streamice
CADJ &     = tapelev3, key = ilev_3
# endif

#ifdef STREAMICE_TRACER_AB
CADJ STORE GAD_trac_2d
CADJ &     = tapelev3, key = ilev_3
#endif


#endif /* ALLOW_STREAMICE */
