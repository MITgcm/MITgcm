
c     ==================================================================
c     HEADER G_COST
c     ==================================================================
c
c     o Header for model-data comparison; adjoint part.
c
c     The individual cost function contributions are multiplied by
c     factors mult_"var" which allow to switch off these contributions
c     without removing them in the adjoint code. This is useful for
c     doing tests with the adjoint and perhaps useful in assimilation
c     experiments where individual contributions are successively
c     switched on. For future applications it would be better to place
c     the initialisation of the multipliers somewhere else, for example
c     in a namelist, which is read in at the start of the model.
c
c     started: Christian Eckert eckert@mit.edu  06-Apr-2000
c
c     changed: Christian Eckert eckert@mit.edu
c
c
c     ==================================================================
c     HEADER G_COST
c     ==================================================================

      common /g_cost_r/
     &                  g_fc,
     &                  g_objf_hflux,
     &                  g_objf_sflux,
     &                  g_objf_tauu,
     &                  g_objf_tauv,
     &                  g_objf_hmean,
     &                  g_objf_h,
     &                  g_objf_temp,
     &                  g_objf_salt,
     &                  g_objf_sst,
     &                  g_objf_ctds,
     &                  g_objf_ctdt
      _RL  g_fc
      _RL  g_objf_hflux   (nsx,nsy)
      _RL  g_objf_sflux   (nsx,nsy)
      _RL  g_objf_tauu (nsx,nsy)
      _RL  g_objf_tauv (nsx,nsy)
      _RL  g_objf_hmean
      _RL  g_objf_h    (nsx,nsy)
      _RL  g_objf_temp (nsx,nsy)
      _RL  g_objf_salt (nsx,nsy)
      _RL  g_objf_sst  (nsx,nsy)
      _RL  g_objf_ctds (nsx,nsy)
      _RL  g_objf_ctdt (nsx,nsy)

#ifdef ALLOW_COST_STATE_FINAL
      common /g_cost_state_final_r/
     &                g_objf_state_final
      _RL  g_objf_state_final (snx,sny,nsx,nsy)
cph      _RL  g_objf_state_final (snx,sny,nr,nsx,nsy,2)
#endif

#ifdef ALLOW_COST_VECTOR
      common /g_cost_vector_r/
     &                  g_objf_vector
      _RL  g_objf_vector(snx,nsx,nsy)
#endif

c     ==================================================================
c     END OF HEADER G_COST
c     ==================================================================

