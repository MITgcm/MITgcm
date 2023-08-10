C     *==========================================================*
C     | SEAICE_COST.h                                            |
C     | o Sea ice cost terms.                                    |
C     *==========================================================*

C     objf_ice   :: sea-ice volume
      COMMON /seaice_cost_objf/
     &                objf_ice,
     &                objf_ice_export
      _RL  objf_ice        (nSx,nSy)
      _RL  objf_ice_export (nSx,nSy)

C-   SEAICE_cutoff_area & _heff :: only used in pkg/ecco GET_EXCONC_DECONC S/R
      COMMON /seaice_cost_aux_r/
     &                num_ice,
     &                mult_ice,
     &                mult_ice_export,
     &                SEAICE_cutoff_area,
     &                SEAICE_cutoff_heff
      _RL  num_ice  (nSx,nSy)
      _RL  mult_ice
      _RL  mult_ice_export
      _RL  SEAICE_cutoff_area
      _RL  SEAICE_cutoff_heff

      COMMON /seaice_cost_data_aux_i/
     &                           costIceStart1,
     &                           costIceStart2,
     &                           costIceEnd1,
     &                           costIceEnd2
      INTEGER costIceStart1
      INTEGER costIceStart2
      INTEGER costIceEnd1
      INTEGER costIceEnd2

      COMMON /seaice_cost_data_times_r/ costIceStart, costIceEnd
      _RL costIceStart
      _RL costIceEnd

C     cost_ice_flag  :: cost_ice flag (see seaice_cost_test.F)
      COMMON /seaice_cost_i/ cost_ice_flag
      INTEGER cost_ice_flag

#ifdef ALLOW_SEAICE_COST_EXPORT
      _RL uHeffExportCell(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vHeffExportCell(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /SEAICE_COST_EXPORT_R/
     &       uHeffExportCell, vHeffExportCell
#endif
