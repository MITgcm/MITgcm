C     /==========================================================\
C     | SEAICE_COST.h                                            |
C     | o Sea ice cost terms.                                    |
C     \==========================================================/
C
C
c     objf_ice    - sea-ice volume

      common /seaice_cost_objf/
     &                objf_ice
     &              , objf_smrarea
     &              , objf_smrsst
     &              , objf_smrsss
     &              , objf_ice_export
      _RL  objf_ice        (nsx,nsy)
      _RL  objf_smrarea    (nsx,nsy)
      _RL  objf_smrsst     (nsx,nsy)
      _RL  objf_smrsss     (nsx,nsy)
      _RL  objf_ice_export (nsx,nsy)

      common /seaice_cost_aux_r/
     &                num_ice
     &              , num_smrarea
     &              , num_smrsst
     &              , num_smrsss
     &              , mult_ice
     &              , mult_smrarea
     &              , mult_smrsst
     &              , mult_smrsss
     &              , mult_ice_export
     &              , SEAICE_clamp_salt
     &              , SEAICE_clamp_theta
     &              , SEAICE_cutoff_area
     &              , SEAICE_cutoff_heff
      _RL  num_ice  (nsx,nsy)
      _RL  num_smrarea (nsx,nsy)
      _RL  num_smrsst  (nsx,nsy)
      _RL  num_smrsss  (nsx,nsy)
      _RL  mult_ice
      _RL  mult_smrarea
      _RL  mult_smrsst
      _RL  mult_smrsss
      _RL  mult_ice_export
      _RL  SEAICE_clamp_salt
      _RL  SEAICE_clamp_theta
      _RL  SEAICE_cutoff_area
      _RL  SEAICE_cutoff_heff

      common /seaice_cost_data_aux_i/
     &                           costIceStart1,
     &                           costIceStart2,
     &                           costIceEnd1,
     &                           costIceEnd2
      integer costIceStart1
      integer costIceStart2
      integer costIceEnd1
      integer costIceEnd2

      common /seaice_cost_data_times_r/
     &                           costIceStart,
     &                           costIceEnd
      _RL costIceStart
      _RL costIceEnd

c     cost_ice_flag  - cost_ice flag (see cost_ice.F)

      common /ecco_cost_ice_i/
     &                           cost_ice_flag
      integer cost_ice_flag

#ifdef ALLOW_SEAICE_COST_SMR_AREA
      _RL smrareabar(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL smrsstbar(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL smrsssbar(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#else
      _RL smrareabar
      _RL smrsstbar
      _RL smrsssbar
#endif
      common /seaice_cost_averages_r/
     &       smrareabar,
     &       smrsstbar,
     &       smrsssbar

      _RL wsmrarea(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wsmrarea0
      _RL wmean_smrarea
      common /seaice_cost_weights_r/
     &       wsmrarea
     &     , wsmrarea0
     &     , wmean_smrarea

      _RL smrareadat(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /seaice_cost_data_r/
     &       smrareadat

      character*(MAX_LEN_FNAM) smrarea_errfile
      character*(MAX_LEN_FNAM) smrareadatfile
      character*(MAX_LEN_FNAM) smrareabarfile
      character*(MAX_LEN_FNAM) smrsstbarfile
      character*(MAX_LEN_FNAM) smrsssbarfile
      common /seaice_cost_c/
     &       smrarea_errfile
     &     , smrareadatfile
     &     , smrareabarfile
     &     , smrsstbarfile
     &     , smrsssbarfile

      integer smrareastartdate(4)
      integer smrareastartdate1
      integer smrareastartdate2
      common /seaice_cost_i/
     &       smrareastartdate
     &     , smrareastartdate1
     &     , smrareastartdate2

      _RL smrareaperiod
      common /seaice_cost_period_r/
     &       smrareaperiod

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
