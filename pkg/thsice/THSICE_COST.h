C     /==========================================================\
C     | THSICE_COST.h                                            |
C     | o Sea ice cost terms.                                    |
C     \==========================================================/
C
C
c     objf_thsice    - sea-ice volume

#ifdef ALLOW_COST
      common /thsice_cost_objf/
     &                objf_thsice
      _RL  objf_thsice        (nsx,nsy)

      common /thsice_cost_aux_r/
     &                num_thsice,
     &                mult_thsice
      _RL  num_thsice  (nsx,nsy)
      _RL  mult_thsice

c     cost_ice_flag  - cost_ice flag
      common /thsice_cost_ice_i/ 
     &                           thsice_cost_ice_flag
      integer thsice_cost_ice_flag
#endif

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
