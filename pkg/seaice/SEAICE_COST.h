C $Header: /u/gcmpack/MITgcm/pkg/seaice/SEAICE_COST.h,v 1.2 2005/09/01 05:34:30 heimbach Exp $
C $Name:  $

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
      _RL  objf_ice  (nsx,nsy)
      _RL  objf_smrarea (nsx,nsy)

      common /seaice_cost_num/
     &                num_ice
     &              , num_smrarea
      _RL  num_ice  (nsx,nsy)
      _RL  num_smrarea (nsx,nsy)

      common /seaice_cost_aux_r/
     &                mult_ice
     &              , mult_smrarea
      _RL  mult_ice
      _RL  mult_smrarea

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
#else
      _RL smrareabar
#endif
      common /seaice_cost_averages_r/ 
     &       smrareabar

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
      common /seaice_cost_c/
     &       smrarea_errfile
     &     , smrareadatfile
     &     , smrareabarfile

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
