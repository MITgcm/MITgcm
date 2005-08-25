C $Header: /u/gcmpack/MITgcm/pkg/seaice/SEAICE_COST.h,v 1.1 2005/08/25 16:10:42 heimbach Exp $
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
      _RL  objf_ice  (nsx,nsy)

      common /seaice_cost_num/
     &                num_ice
      _RL  num_ice  (nsx,nsy)

      common /seaice_cost_aux_r/
     &                mult_ice
      _RL  mult_ice

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


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
