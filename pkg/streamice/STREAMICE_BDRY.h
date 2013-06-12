C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_STREAMICE

C     params to construct boundary conditions for ice flow
C     on each boundary of rect domain, ranges for different boundary 
C      conditions are specified; it is up to the user to ensure that 
C      the ranges do not overlap
C     along with a flux boundary range, a flux value is given
C     if a dirichlet boundary is specified, velocities must be given
C      in a file
C     also must be aware of units (m for cartesian, deg for curvilinear, m/deg for CYLINDRICAL POLAR)

C     real params
      COMMON /STREAMICE_BDRY_PARMS/ 
     & min_x_noflow_NORTH, max_x_noflow_NORTH,
     & min_x_noflow_SOUTH, max_x_noflow_SOUTH,
     & min_y_noflow_WEST, max_y_noflow_WEST,
     & min_y_noflow_EAST, max_y_noflow_EAST,
     & min_x_noStress_NORTH, max_x_noStress_NORTH,
     & min_x_noStress_SOUTH, max_x_noStress_SOUTH,
     & min_y_noStress_WEST, max_y_noStress_WEST,
     & min_y_noStress_EAST, max_y_noStress_EAST,
     & min_x_FluxBdry_NORTH, max_x_FluxBdry_NORTH,
     & min_x_FluxBdry_SOUTH, max_x_FluxBdry_SOUTH,
     & min_y_FluxBdry_WEST, max_y_FluxBdry_WEST,
     & min_y_FluxBdry_EAST, max_y_FluxBdry_EAST,
     & min_x_Dirich_NORTH, max_x_Dirich_NORTH,
     & min_x_Dirich_SOUTH, max_x_Dirich_SOUTH,
     & min_y_Dirich_WEST, max_y_Dirich_WEST,
     & min_y_Dirich_EAST, max_y_Dirich_EAST,
     & min_x_CFBC_NORTH, max_x_CFBC_NORTH,
     & min_x_CFBC_SOUTH, max_x_CFBC_SOUTH,
     & min_y_CFBC_WEST, max_y_CFBC_WEST,
     & min_y_CFBC_EAST, max_y_CFBC_EAST,
     & flux_bdry_val_SOUTH, flux_bdry_val_NORTH,
     & flux_bdry_val_WEST, flux_bdry_val_EAST
      _RL min_x_noflow_NORTH, max_x_noflow_NORTH
      _RL min_x_noflow_SOUTH, max_x_noflow_SOUTH
      _RL min_y_noflow_WEST, max_y_noflow_WEST
      _RL min_y_noflow_EAST, max_y_noflow_EAST
      _RL min_x_noStress_NORTH, max_x_noStress_NORTH
      _RL min_x_noStress_SOUTH, max_x_noStress_SOUTH
      _RL min_y_noStress_WEST, max_y_noStress_WEST
      _RL min_y_noStress_EAST, max_y_noStress_EAST
      _RL min_x_FluxBdry_NORTH, max_x_FluxBdry_NORTH
      _RL min_x_FluxBdry_SOUTH, max_x_FluxBdry_SOUTH
      _RL min_y_FluxBdry_WEST, max_y_FluxBdry_WEST
      _RL min_y_FluxBdry_EAST, max_y_FluxBdry_EAST
      _RL min_x_Dirich_NORTH, max_x_Dirich_NORTH
      _RL min_x_Dirich_SOUTH, max_x_Dirich_SOUTH
      _RL min_y_Dirich_WEST, max_y_Dirich_WEST
      _RL min_y_Dirich_EAST, max_y_Dirich_EAST
      _RL min_x_CFBC_NORTH, max_x_CFBC_NORTH
      _RL min_x_CFBC_SOUTH, max_x_CFBC_SOUTH
      _RL min_y_CFBC_WEST, max_y_CFBC_WEST
      _RL min_y_CFBC_EAST, max_y_CFBC_EAST
      _RL flux_bdry_val_SOUTH, flux_bdry_val_NORTH
      _RL flux_bdry_val_WEST, flux_bdry_val_EAST

#endif /* ALLOW_STREAMICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***

