#ifdef ALLOW_STREAMICE
CADJ STORE area_shelf_streamice
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_hmask
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE u_streamice
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE v_streamice
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE h_streamice
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_cg_a1
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_cg_a2
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_cg_a3
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_cg_a4
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte

CADJ STORE cost_func1_streamice
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte

#ifdef ALLOW_STREAMICE_2DTRACER
CADJ STORE trac2d
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
#endif

CADJ STORE surf_el_streamice
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE float_frac_streamice
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte

CADJ STORE c_basal_friction
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE b_glen
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte

# ifdef STREAMICE_HYBRID_STRESS
CADJ STORE visc_streamice_full
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
# endif

CADJ STORE bdot_streamice
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
#ifdef  STREAMICE_STRESS_BOUNDARY_CONTROL
CADJ STORE streamice_u_normal_stress
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_v_normal_stress
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_u_shear_stress
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_v_shear_stress
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_u_normal_pert
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_v_normal_pert
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_u_shear_pert
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_v_shear_pert
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
#endif

#ifdef ALLOW_STREAMICE_TIMEDEP_FORCING
CADJ STORE bdot_streamice0
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE bdot_streamice1
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
#ifdef  STREAMICE_STRESS_BOUNDARY_CONTROL
CADJ STORE streamice_u_normal_stress0
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_v_normal_stress0
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_u_shear_stress0
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_v_shear_stress0
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_u_normal_stress1
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_v_normal_stress1
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_u_shear_stress1
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE streamice_v_shear_stress1
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
#endif
#endif



#ifdef STREAMICE_TRACER_AB
CADJ STORE GAD_trac_2d
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
#endif

#endif /* ALLOW_STREAMICE */
