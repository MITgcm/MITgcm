
c     ==================================================================
c     HEADER ADCOST
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
c     HEADER ADCOST
c     ==================================================================

      common /adcost_r/
     &                  adfc,
     &                  adobjf_hflux,
     &                  adobjf_sflux,
     &                  adobjf_tauu,
     &                  adobjf_tauv,
     &                  adobjf_hmean,
     &                  adobjf_h,
     &                  adobjf_temp,
     &                  adobjf_salt,
     &                  adobjf_sst,
     &                  adobjf_ctds,
     &                  adobjf_ctdt
      _RL  adfc
      _RL  adobjf_hflux   (nsx,nsy)
      _RL  adobjf_sflux   (nsx,nsy)
      _RL  adobjf_tauu (nsx,nsy)
      _RL  adobjf_tauv (nsx,nsy)
      _RL  adobjf_hmean
      _RL  adobjf_h    (nsx,nsy)
      _RL  adobjf_temp (nsx,nsy)
      _RL  adobjf_salt (nsx,nsy)
      _RL  adobjf_sst  (nsx,nsy)
      _RL  adobjf_ctds (nsx,nsy)
      _RL  adobjf_ctdt (nsx,nsy)

c     ==================================================================
c     END OF HEADER ADCOST
c     ==================================================================

