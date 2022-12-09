c     ==================================================================
c     HEADER TAP_TLM_COST
c     ==================================================================

      common /cost_r_d/
     &                  fcd
      _RL  fcd

#ifdef ALLOW_COST_STATE_FINAL
      common /cost_state_final_r_d/
     &               objf_state_finald
      _RL  objf_state_finald (snx,sny,nsx,nsy,4*Nr+1)
#endif

#ifdef ALLOW_COST_VECTOR
      common /cost_vector_r_d/
     &                  objf_vectord
      _RL  objf_vectord(snx,nsx,nsy)
#endif

c     ==================================================================
c     END OF HEADER TAP_TLM_COST
c     ==================================================================

