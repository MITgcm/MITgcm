C---+----1--+-+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_STREAMICE_FLUX_CONTROL

      INTEGER n_fluxes_max
      INTEGER n_epochs_max
      
      COMMON /STREAMICE_CTRL_FLUX_INT_PARAM/
     & n_fluxes, n_epochs
      INTEGER n_fluxes
      INTEGER n_epochs

      COMMON /STREAMICE_CTRL_FLUX_INT/
     & streamice_ctrl_flux_id
      INTEGER streamice_ctrl_flux_id (n_fluxes_max)

      COMMON /STREAMICE_CTRL_FLUX_RS/
     & streamice_ctrl_flux_mask
      _RS streamice_ctrl_flux_mask
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)   ! need to initialize this from alex's file

      COMMON /STREAMICE_CTRL_FLUX_RL/
     & streamice_ctrl_flux_scaleVel,
     & u_bdry_values_SI_base,
     & v_bdry_values_SI_base
      _RL streamice_ctrl_flux_scaleVel (n_fluxes_max, n_epochs_max)
      _RL u_bdry_values_SI_base 
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v_bdry_values_SI_base 
     & (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      


#endif
