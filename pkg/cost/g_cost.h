
c     ==================================================================
c     HEADER G_COST
c     ==================================================================
c
c     o Header for model-data comparison; adjoint part.
c
c     started: Christian Eckert eckert@mit.edu  06-Apr-2000
c     changed: Christian Eckert eckert@mit.edu
c     heimbach@mit.edu 5-Nov-2003 retain only adfc
c
c     ==================================================================
c     HEADER G_COST
c     ==================================================================

      common /g_cost_r/
     &                  g_fc
      _RL  g_fc

#ifdef ALLOW_COST_STATE_FINAL
      common /g_cost_state_final_r/
     &                g_objf_state_final
cph      _RL  g_objf_state_final (snx,sny,nsx,nsy)
      _RL  g_objf_state_final (snx,sny,nsx,nsy,4*Nr+1)
#endif

#ifdef ALLOW_COST_VECTOR
      common /g_cost_vector_r/
     &                  g_objf_vector
      _RL  g_objf_vector(snx,nsx,nsy)
#endif

c     ==================================================================
c     END OF HEADER G_COST
c     ==================================================================

