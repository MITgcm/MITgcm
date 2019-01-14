#ifdef ALLOW_ATM_PHYS

C--   ATM_PHYS parameters
C     atmPhys_addTendT :: apply ATM_PHYS tendency to temperature
C     atmPhys_addTendS :: apply ATM_PHYS tendency to Specific Humid
C     atmPhys_addTendU :: apply ATM_PHYS tendency to U-component wind
C     atmPhys_addTendV :: apply ATM_PHYS tendency to V-component wind
C     atmPhys_stepSST  :: step forward SST
      LOGICAL atmPhys_addTendT
      LOGICAL atmPhys_addTendS
      LOGICAL atmPhys_addTendU
      LOGICAL atmPhys_addTendV
      LOGICAL atmPhys_stepSST

C-    horizontal momentum damping (e.g., stratospheric drag)
C     atmPhys_tauDampUV :: damping time-scale (s)
C     atmPhys_dampUVfac :: damping coefficient for each level
      _RL     atmPhys_tauDampUV
      _RL     atmPhys_dampUVfac(Nr)

C-    file names for initial conditions:
C     atmPhys_sstFile  :: name of initial SST [in K] file
C     atmPhys_qFlxFile :: name of Q-flux file
C     atmPhys_mxldFile :: name of Mixed-Layer Depth file
C     atmPhys_albedoFile :: name of Albedo file
C     atmPhys_ozoneFile  :: name of annual mean ozone concentration file
C                           (units: mol/mol i.e. volume mixing ratio)
      CHARACTER*(MAX_LEN_FNAM) atmPhys_sstFile
      CHARACTER*(MAX_LEN_FNAM) atmPhys_qFlxFile
      CHARACTER*(MAX_LEN_FNAM) atmPhys_mxldFile
      CHARACTER*(MAX_LEN_FNAM) atmPhys_albedoFile
      CHARACTER*(MAX_LEN_FNAM) atmPhys_ozoneFile

      COMMON /ATM_PHYS_PARAMS_L/
     &       atmPhys_addTendT, atmPhys_addTendS,
     &       atmPhys_addTendU, atmPhys_addTendV,
     &       atmPhys_stepSST
      COMMON /ATM_PHYS_PARAMS_R/
     &       atmPhys_tauDampUV, atmPhys_dampUVfac
      COMMON /ATM_PHYS_PARAMS_C/
     &       atmPhys_sstFile, atmPhys_qFlxFile, atmPhys_mxldFile,
     &       atmPhys_albedoFile, atmPhys_ozoneFile

C-- from driver-atmosphere module:
      logical  module_is_initialized
      logical  turb
      logical  ldry_convection
      logical  do_virtual
      logical  lwet_convection
      logical  two_stream
      logical  mixed_layer_bc
      COMMON /ATMOS_DRIVER_L/
     &   module_is_initialized,
     &   turb, ldry_convection, do_virtual,
     &   lwet_convection, two_stream, mixed_layer_bc

      _RL  roughness_heat
      _RL  roughness_moist
      _RL  roughness_mom
      COMMON /ATMOS_DRIVER_RL/
     &   roughness_heat, roughness_moist, roughness_mom

#endif /* ALLOW_ATM_PHYS */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
