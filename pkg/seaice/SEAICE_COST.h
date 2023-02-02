C     /==========================================================\
C     | SEAICE_COST.h                                            |
C     | o Sea ice cost terms.                                    |
C     \==========================================================/
C
C
c     objf_ice    - sea-ice volume

      common /seaice_cost_objf/
     &                objf_ice
     &              , objf_ice_export
      _RL  objf_ice        (nsx,nsy)
      _RL  objf_ice_export (nsx,nsy)

      common /seaice_cost_aux_r/
     &                num_ice
     &              , mult_ice
     &              , mult_ice_export
     &              , SEAICE_cutoff_area
     &              , SEAICE_cutoff_heff
      _RL  num_ice  (nsx,nsy)
      _RL  mult_ice
      _RL  mult_ice_export
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

c     cost_ice_flag  - cost_ice flag (see seaice_cost_test.F)

      common /seaice_cost_i/
     &                           cost_ice_flag
      integer cost_ice_flag

#ifdef ALLOW_SEAICE_COST_EXPORT
      _RL uHeffExportCell(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vHeffExportCell(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /SEAICE_COST_EXPORT_R/
     &       uHeffExportCell, vHeffExportCell
#endif

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
