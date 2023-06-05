C     ==================================================================
C     HEADER COST_TAP_ADJ
C     ==================================================================

      COMMON /COST_R_B/
     &        fcb
      _RL  fcb

#ifdef ALLOW_COST_STATE_FINAL
      COMMON /COST_STATE_FINAL_R_B/
     &        objf_state_finalb
      _RL  objf_state_finalb (sNx,sNy,nSx,nSy,4*Nr+1)
#endif

#ifdef ALLOW_COST_VECTOR
      COMMON /COST_VECTOR_R_B/
     &        objf_vectorb
      _RL  objf_vectorb(sNx,nSx,nSy)
#endif

#ifdef ALLOW_DIC
      COMMON /DIC_COST_CTRL_B/
     &        totcostb
      _RL  totcostb
#endif
C     ==================================================================
C     END OF HEADER COST_TAP_ADJ
C     ==================================================================
