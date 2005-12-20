
c     ==================================================================
c     HEADER ADCOST
c     ==================================================================
c
c     o Header for model-data comparison; adjoint part.
c
c     started: Christian Eckert eckert@mit.edu  06-Apr-2000
c     changed: Christian Eckert eckert@mit.edu
c     heimbach@mit.edu 5-Nov-2003 retain only adfc
c
c     ==================================================================
c     HEADER ADCOST
c     ==================================================================

      common /adcost_r/
     &                  adfc
      _RL  adfc

#ifdef ALLOW_COST_STATE_FINAL
      common /adcost_state_final_r/
     &                adobjf_state_final
cph      _RL  adobjf_state_final (snx,sny,nsx,nsy)
      _RL  adobjf_state_final (snx,sny,nsx,nsy,4*Nr+1)
#endif

#ifdef ALLOW_COST_VECTOR
      common /adcost_vector_r/
     &                  adobjf_vector
      _RL  adobjf_vector(snx,nsx,nsy)
#endif

c     ==================================================================
c     END OF HEADER ADCOST
c     ==================================================================

