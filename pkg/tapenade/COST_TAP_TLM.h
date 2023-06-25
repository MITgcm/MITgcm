C     ==================================================================
C     HEADER COST_TAP_TLM
C     ==================================================================

      COMMON /COST_R_D/
     &        fcd
      _RL  fcd

#ifdef ALLOW_COST_STATE_FINAL
      COMMON /COST_STATE_FINAL_R_D/
     &        objf_state_finald
      _RL  objf_state_finald (sNx,sNy,nSx,nSy,4*Nr+1)
#endif

#ifdef ALLOW_COST_VECTOR
      COMMON /COST_VECTOR_R_D/
     &        objf_vectord
      _RL  objf_vectord(sNx,nSx,nSy)
#endif

C     ==================================================================
C     END OF HEADER COST_TAP_TLM
C     ==================================================================
