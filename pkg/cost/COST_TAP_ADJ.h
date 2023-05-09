c     ==================================================================
c     HEADER TAP_ADJ_COST
c     ==================================================================

      common /cost_r_b/
     &                  fcb
      _RL  fcb

#ifdef ALLOW_COST_STATE_FINAL
      common /cost_state_final_r_b/
     &                objf_state_finalb
      _RL  objf_state_finalb (sNx,sNy,nSx,nSy,4*Nr+1)
#endif

#ifdef ALLOW_COST_VECTOR
      common /cost_vector_r_b/
     &                  objf_vectorb
      _RL  objf_vectorb(sNx,nSx,nSy)
#endif

#ifdef ALLOW_DIC
         COMMON /dic_cost_ctrl_b/
     &    totcostb
      _RL  totcostb
#endif
c     ==================================================================
c     END OF HEADER TAP_ADJ_COST
c     ==================================================================

