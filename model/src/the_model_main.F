C $Header: /u/gcmpack/MITgcm/model/src/the_model_main.F,v 1.110 2011/06/08 01:45:24 jmc Exp $
C $Name:  $

CBOI
C
C !TITLE: MITGCM KERNEL CODE SYNOPSIS
C !AUTHORS: mitgcm developers ( support@mitgcm.org )
C !AFFILIATION: Massachussetts Institute of Technology
C !DATE:
C !INTRODUCTION: Kernel dynamical routines
C This document summarises MITgcm code under the model/ subdirectory.
C The code under model/ ( src/ and inc/ ) contains most of
C the driver routines for the baseline forms of the kernel equations in the
C MITgcm algorithm. Numerical code for much of the baseline forms of
C these equations is also under the model/ directory. Other numerical code
C used for the kernel equations is contained in packages in the pkg/
C directory tree.
C Code for auxilliary equations and alternate dicretizations of the kernel
C equations and algorithm can also be found in the pkg/ directory tree.
C
C \subsection{Getting Help and Reporting Errors and Problems}
C If you have questions please e-mail support@mitgcm.org. We also welcome
C reports of errors and inconsistencies in the code or in the accompanying
C documentation. Please feel free to send these
C to support@mitgcm.org. For further information and to review
C problems reported to support@mitgcm.org please visit http://mitgcm.org.
C
C \subsection{MITgcm Kernel Code Calling Sequence}
C \bv
C
C Invocation from WRAPPER level...
C  :
C  :
C  |
C  |-THE_MODEL_MAIN :: Primary driver for the MITgcm algorithm
C    |              :: Called from WRAPPER level numerical
C    |              :: code invocation routine. On entry
C    |              :: to THE_MODEL_MAIN separate thread and
C    |              :: separate processes will have been established.
C    |              :: Each thread and process will have a unique ID
C    |              :: but as yet it will not be associated with a
C    |              :: specific region in decomposed discrete space.
C    |
C    |-INITIALISE_FIXED :: Set fixed model arrays such as topography,
C    | |                :: grid, solver matrices etc..
C    | |
C    | |-INI_PARMS :: Routine to set kernel model parameters.
C    | |           :: By default kernel parameters are read from file
C    | |           :: "data" in directory in which code executes.
C    | |
C    | |-MON_INIT :: Initializes monitor package ( see pkg/monitor )
C    | |
C    | |-INI_GRID :: Control grid array (vert. and hori.) initialization.
C    | | |        :: Grid arrays are held and described in GRID.h.
C    | | |
C    | | |-INI_VERTICAL_GRID        :: Initialize vertical grid arrays.
C    | | |
C    | | |-INI_CARTESIAN_GRID       :: Cartesian horiz. grid initialization
C    | | |                          :: (calculate grid from kernel parameters).
C    | | |
C    | | |-INI_SPHERICAL_POLAR_GRID :: Spherical polar horiz. grid
C    | | |                          :: initialization (calculate grid from
C    | | |                          :: kernel parameters).
C    | | |
C    | | |-INI_CURVILINEAR_GRID     :: General orthogonal, structured horiz.
C    | |                            :: grid initializations. ( input from raw
C    | |                            :: grid files, LONC.bin, DXF.bin etc... )
C    | |
C    | |-INI_DEPTHS    :: Read (from "bathyFile") or set bathymetry/orgography.
C    | |
C    | |-INI_MASKS_ETC :: Derive horizontal and vertical cell fractions and
C    | |               :: land masking for solid-fluid boundaries.
C    | |
C    | |-INI_LINEAR_PHSURF :: Set ref. surface Bo_surf
C    | |
C    | |-INI_CORI          :: Set coriolis term. zero, f-plane, beta-plane,
C    | |                   :: sphere options are coded.
C    | |
C    | |-PACAKGES_BOOT      :: Start up the optional package environment.
C    | |                    :: Runtime selection of active packages.
C    | |
C    | |-PACKAGES_READPARMS :: Call active package internal parameter load.
C    | | |
C    | | |-GMREDI_READPARMS    :: GM Package. see pkg/gmredi
C    | | |-KPP_READPARMS       :: KPP Package. see pkg/kpp
C    | | |-SHAP_FILT_READPARMS :: Shapiro filter package. see pkg/shap_filt
C    | | |-OBCS_READPARMS      :: Open bndy package. see pkg/obcs
C    | | |-AIM_READPARMS       :: Intermediate Atmos. pacakage. see pkg/aim
C    | | |-COST_READPARMS      :: Cost function package. see pkg/cost
C    | | |-CTRL_INIT           :: Control vector support package. see pkg/ctrl
C    | | |-OPTIM_READPARMS     :: Optimisation support package. see pkg/ctrl
C    | | |-GRDCHK_READPARMS    :: Gradient check package. see pkg/grdchk
C    | | |-ECCO_READPARMS      :: ECCO Support Package. see pkg/ecco
C    | | |-PTRACERS_READPARMS  :: multiple tracer package, see pkg/ptracers
C    | | |-GCHEM_READPARMS     :: tracer interface package, see pkg/gchem
C    | |
C    | |-PACKAGES_CHECK
C    | | |
C    | | |-KPP_CHECK           :: KPP Package. pkg/kpp
C    | | |-OBCS_CHECK          :: Open bndy Pacakge. pkg/obcs
C    | | |-GMREDI_CHECK        :: GM Package. pkg/gmredi
C    | |
C    | |-PACKAGES_INIT_FIXED
C    | | |-OBCS_INIT_FIXED     :: Open bndy Package. see pkg/obcs
C    | | |-FLT_INIT            :: Floats Package. see pkg/flt
C    | | |-GCHEM_INIT_FIXED    :: tracer interface pachage, see pkg/gchem
C    | |
C    | |-ZONAL_FILT_INIT       :: FFT filter Package. see pkg/zonal_filt
C    | |
C    | |-INI_CG2D              :: 2d con. grad solver initialization.
C    | |
C    | |-INI_CG3D              :: 3d con. grad solver initialization.
C    | |
C    | |-CONFIG_SUMMARY        :: Provide synopsis of kernel setup.
C    |                         :: Includes annotated table of kernel
C    |                         :: parameter settings.
C    |
C    |-CTRL_UNPACK :: Control vector support package. see pkg/ctrl
C    |
C    |-ADTHE_MAIN_LOOP :: Derivative evaluating form of main time stepping loop
C    !                 :: Auotmatically generated by TAMC/TAF.
C    |
C    |-CTRL_PACK   :: Control vector support package. see pkg/ctrl
C    |
C    |-GRDCHK_MAIN :: Gradient check package. see pkg/grdchk
C    |
C    |-THE_MAIN_LOOP :: Main timestepping loop routine.
C    | |
C    | |-INITIALISE_VARIA :: Set the initial conditions for time evolving
C    | | |                :: variables
C    | | |
C    | | |-INI_LINEAR_PHISURF :: Set ref. surface Bo_surf
C    | | |
C    | | |-INI_CORI     :: Set coriolis term. zero, f-plane, beta-plane,
C    | | |              :: sphere options are coded.
C    | | |
C    | | |-INI_CG2D     :: 2d con. grad solver initialization.
C    | | |-INI_CG3D     :: 3d con. grad solver initialization.
C    | | |-INI_MIXING   :: Initialize diapycnal diffusivity.
C    | | |-INI_DYNVARS  :: Initialize to zero all DYNVARS.h arrays (dynamical
C    | | |              :: fields).
C    | | |
C    | | |-INI_FIELDS   :: Control initializing model fields to non-zero
C    | | | |-INI_VEL    :: Initialize 3D flow field.
C    | | | |-INI_THETA  :: Set model initial temperature field.
C    | | | |-INI_SALT   :: Set model initial salinity field.
C    | | | |-INI_PSURF  :: Set model initial free-surface height/pressure.
C    | | | |-INI_PRESSURE :: Compute model initial hydrostatic pressure
C    | | | |-READ_CHECKPOINT :: Read the checkpoint
C    | | |
C    | | |-THE_CORRECTION_STEP :: Step forward to next time step.
C    | | | |                   :: Here applied to move restart conditions
C    | | | |                   :: (saved in mid timestep) to correct level in
C    | | | |                   :: time (only used for pre-c35).
C    | | | |
C    | | | |-CALC_GRAD_PHI_SURF :: Return DDx and DDy of surface pressure
C    | | | |-CORRECTION_STEP    :: Pressure correction to momentum
C    | | | |-CYCLE_TRACER       :: Move tracers forward in time.
C    | | | |-OBCS_APPLY         :: Open bndy package. see pkg/obcs
C    | | | |-SHAP_FILT_APPLY    :: Shapiro filter package. see pkg/shap_filt
C    | | | |-ZONAL_FILT_APPLY   :: FFT filter package. see pkg/zonal_filt
C    | | | |-CONVECTIVE_ADJUSTMENT :: Control static instability mixing.
C    | | | | |-FIND_RHO  :: Find adjacent densities.
C    | | | | |-CONVECT   :: Mix static instability.
C    | | | | |-TIMEAVE_CUMULATE :: Update convection statistics.
C    | | | |
C    | | | |-CALC_EXACT_ETA        :: Change SSH to flow divergence.
C    | | |
C    | | |-CONVECTIVE_ADJUSTMENT_INI :: Control static instability mixing
C    | | | |                         :: Extra time history interactions.
C    | | | |
C    | | | |-FIND_RHO  :: Find adjacent densities.
C    | | | |-CONVECT   :: Mix static instability.
C    | | | |-TIMEAVE_CUMULATE :: Update convection statistics.
C    | | |
C    | | |-PACKAGES_INIT_VARIABLES :: Does initialization of time evolving
C    | | | |                       :: package data.
C    | | | |
C    | | | |-GMREDI_INIT          :: GM package. ( see pkg/gmredi )
C    | | | |-KPP_INIT             :: KPP package. ( see pkg/kpp )
C    | | | |-KPP_OPEN_DIAGS
C    | | | |-OBCS_INIT_VARIABLES  :: Open bndy. package. ( see pkg/obcs )
C    | | | |-PTRACERS_INIT        :: multi. tracer package,(see pkg/ptracers)
C    | | | |-GCHEM_INIT           :: tracer interface pkg (see pkh/gchem)
C    | | | |-AIM_INIT             :: Interm. atmos package. ( see pkg/aim )
C    | | | |-CTRL_MAP_INI         :: Control vector package.( see pkg/ctrl )
C    | | | |-COST_INIT            :: Cost function package. ( see pkg/cost )
C    | | | |-ECCO_INIT            :: ECCO support package. ( see pkg/ecco )
C    | | | |-INI_FORCING          :: Set model initial forcing fields.
C    | | |   |                    :: Either set in-line or from file as shown.
C    | | |   |-READ_FLD_XY_RS(zonalWindFile)
C    | | |   |-READ_FLD_XY_RS(meridWindFile)
C    | | |   |-READ_FLD_XY_RS(surfQFile)
C    | | |   |-READ_FLD_XY_RS(EmPmRfile)
C    | | |   |-READ_FLD_XY_RS(thetaClimFile)
C    | | |   |-READ_FLD_XY_RS(saltClimFile)
C    | | |   |-READ_FLD_XY_RS(surfQswFile)
C    | | |
C    | | |-CALC_SURF_DR   :: Calculate the new surface level thickness.
C    | | |-UPDATE_SURF_DR :: Update the surface-level thickness fraction.
C    | | |-UPDATE_CG2D    :: Update 2d conjugate grad. for Free-Surf.
C    | | |-STATE_SUMMARY    :: Summarize model prognostic variables.
C    | | |-TIMEAVE_STATVARS :: Time averaging package ( see pkg/timeave ).
C    | |
C    | |-WRITE_STATE      :: Controlling routine for IO to dump model state.
C    | | |-WRITE_REC_XYZ_RL :: Single file I/O
C    | | |-WRITE_FLD_XYZ_RL :: Multi-file I/O
C    | |
C    | |-MONITOR          :: Monitor state ( see pkg/monitor )
C    | |-CTRL_MAP_FORCING :: Control vector support package. ( see pkg/ctrl )
C====|>|
C====|>| ****************************
C====|>| BEGIN MAIN TIMESTEPPING LOOP
C====|>| ****************************
C====|>|
C/\  | |-FORWARD_STEP     :: Step forward a time-step ( AT LAST !!! )
C/\  | | |
C/\  | | |-DUMMY_IN_STEPPING :: autodiff package ( pkg/autoduff ).
C/\  | | |-CALC_EXACT_ETA :: Change SSH to flow divergence.
C/\  | | |-CALC_SURF_DR   :: Calculate the new surface level thickness.
C/\  | | |-EXF_GETFORCING :: External forcing package. ( pkg/exf )
C/\  | | |-EXTERNAL_FIELDS_LOAD :: Control loading time dep. external data.
C/\  | | | |                    :: Simple interpolation between end-points
C/\  | | | |                    :: for forcing datasets.
C/\  | | | |
C/\  | | | |-EXCH :: Sync forcing. in overlap regions.
C/\  | | |-SEAICE_MODEL   :: Compute sea-ice terms. ( pkg/seaice )
C/\  | | |-FREEZE         :: Limit surface temperature.
C/\  | | |-GCHEM_FIELD_LOAD :: load tracer forcing fields (pkg/gchem)
C/\  | | |
C/\  | | |-THERMODYNAMICS :: theta, salt + tracer equations driver.
C/\  | | | |
C/\  | | | |-INTEGRATE_FOR_W :: Integrate for vertical velocity.
C/\  | | | |-OBCS_APPLY_W    :: Open bndy. package ( see pkg/obcs ).
C/\  | | | |-FIND_RHO        :: Calculates [rho(S,T,z)-RhoConst] of a slice
C/\  | | | |-GRAD_SIGMA      :: Calculate isoneutral gradients
C/\  | | | |-CALC_IVDC       :: Set Implicit Vertical Diffusivity for Convection
C/\  | | | |
C/\  | | | |-OBCS_CALC            :: Open bndy. package ( see pkg/obcs ).
C/\  | | | |-EXTERNAL_FORCING_SURF:: Accumulates appropriately dimensioned
C/\  | | | | |                    :: forcing terms.
C/\  | | | | |-PTRACERS_FORCING_SURF :: Tracer package ( see pkg/ptracers ).
C/\  | | | |
C/\  | | | |-GMREDI_CALC_TENSOR   :: GM package ( see pkg/gmredi ).
C/\  | | | |-GMREDI_CALC_TENSOR_DUMMY :: GM package ( see pkg/gmredi ).
C/\  | | | |-KPP_CALC             :: KPP package ( see pkg/kpp ).
C/\  | | | |-KPP_CALC_DUMMY       :: KPP package ( see pkg/kpp ).
C/\  | | | |-AIM_DO_ATMOS_PHYSICS :: Intermed. atmos package ( see pkg/aim ).
C/\  | | | |-GAD_ADVECTION        :: Generalised advection driver (multi-dim
C/\  | | | |                         advection case) (see pkg/gad).
C/\  | | | |-CALC_COMMON_FACTORS  :: Calculate common data (such as volume flux)
C/\  | | | |-CALC_DIFFUSIVITY     :: Calculate net vertical diffusivity
C/\  | | | | |
C/\  | | | | |-GMREDI_CALC_DIFF   :: GM package ( see pkg/gmredi ).
C/\  | | | | |-KPP_CALC_DIFF      :: KPP package ( see pkg/kpp ).
C/\  | | | |
C/\  | | | |-CALC_GT              :: Calculate the temperature tendency terms
C/\  | | | | |
C/\  | | | | |-GAD_CALC_RHS       :: Generalised advection package
C/\  | | | | | |                  :: ( see pkg/gad )
C/\  | | | | | |-KPP_TRANSPORT_T  :: KPP non-local transport ( see pkg/kpp ).
C/\  | | | | |
C/\  | | | | |-EXTERNAL_FORCING_T :: Problem specific forcing for temperature.
C/\  | | | | |-ADAMS_BASHFORTH2   :: Extrapolate tendencies forward in time.
C/\  | | | | |-FREESURF_RESCALE_G :: Re-scale Gt for free-surface height.
C/\  | | | |
C/\  | | | |-TIMESTEP_TRACER      :: Step tracer field forward in time
C/\  | | | |
C/\  | | | |-CALC_GS              :: Calculate the salinity tendency terms
C/\  | | | | |
C/\  | | | | |-GAD_CALC_RHS       :: Generalised advection package
C/\  | | | | | |                  :: ( see pkg/gad )
C/\  | | | | | |-KPP_TRANSPORT_S  :: KPP non-local transport ( see pkg/kpp ).
C/\  | | | | |
C/\  | | | | |-EXTERNAL_FORCING_S :: Problem specific forcing for salt.
C/\  | | | | |-ADAMS_BASHFORTH2   :: Extrapolate tendencies forward in time.
C/\  | | | | |-FREESURF_RESCALE_G :: Re-scale Gs for free-surface height.
C/\  | | | |
C/\  | | | |-TIMESTEP_TRACER      :: Step tracer field forward in time
C/\  | | | |
C/\  | | | |-TIMESTEP_TRACER      :: Step tracer field forward in time
C/\  | | | |
C/\  | | | |-PTRACERS_INTEGRATE   :: Integrate other tracer(s) (see pkg/ptracers).
C/\  | | | | |
C/\  | | | | |-GAD_CALC_RHS       :: Generalised advection package
C/\  | | | | | |                  :: ( see pkg/gad )
C/\  | | | | | |-KPP_TRANSPORT_PTR:: KPP non-local transport ( see pkg/kpp ).
C/\  | | | | |
C/\  | | | | |-PTRACERS_FORCING   :: Problem specific forcing for tracer.
C/\  | | | | |-GCHEM_FORCING_INT  :: tracer forcing for gchem pkg (if all
C/\  | | | | |                       tendency terms calcualted together)
C/\  | | | | |-ADAMS_BASHFORTH2   :: Extrapolate tendencies forward in time.
C/\  | | | | |-FREESURF_RESCALE_G :: Re-scale Gs for free-surface height.
C/\  | | | | |-TIMESTEP_TRACER    :: Step tracer field forward in time
C/\  | | | |
C/\  | | | |-OBCS_APPLY_TS        :: Open bndy. package (see pkg/obcs ).
C/\  | | | |
C/\  | | | |-IMPLDIFF             :: Solve vertical implicit diffusion equation.
C/\  | | | |-OBCS_APPLY_TS        :: Open bndy. package (see pkg/obcs ).
C/\  | | | |
C/\  | | | |-AIM_AIM2DYN_EXCHANGES :: Inetermed. atmos (see pkg/aim).
C/\  | | | |-EXCH                 :: Update overlaps
C/\  | | |
C/\  | | |-DYNAMICS       :: Momentum equations driver.
C/\  | | | |
C/\  | | | |-CALC_GRAD_PHI_SURF :: Calculate the gradient of the surface
C/\  | | | |                       Potential anomaly.
C/\  | | | |-CALC_VISCOSITY     :: Calculate net vertical viscosity
C/\  | | | | |-KPP_CALC_VISC    :: KPP package ( see pkg/kpp ).
C/\  | | | |
C/\  | | | |-CALC_PHI_HYD       :: Integrate the hydrostatic relation.
C/\  | | | |-MOM_FLUXFORM       :: Flux form mom eqn. package ( see
C/\  | | | |                       pkg/mom_fluxform ).
C/\  | | | | |-MOM_CALC_VISC    :: Calculate Horizontal Viscosities
C/\  | | | |-MOM_VECINV         :: Vector invariant form mom eqn. package ( see
C/\  | | | |                       pkg/mom_vecinv   ).
C/\  | | | | |-MOM_CALC_VISC    :: Calculate Horizontal Viscosities
C/\  | | | |-TIMESTEP           :: Step momentum fields forward in time
C/\  | | | |-OBCS_APPLY_UV      :: Open bndy. package (see pkg/obcs ).
C/\  | | | |
C/\  | | | |-IMPLDIFF           :: Solve vertical implicit diffusion equation.
C/\  | | | |-OBCS_APPLY_UV      :: Open bndy. package (see pkg/obcs ).
C/\  | | | |
C/\  | | | |-TIMEAVE_CUMUL_1T   :: Time averaging package ( see pkg/timeave ).
C/\  | | | |-TIMEAVE_CUMUATE    :: Time averaging package ( see pkg/timeave ).
C/\  | | | |-DEBUG_STATS_RL     :: Quick debug package ( see pkg/debug ).
C/\  | | |
C/\  | | |-CALC_GW        :: vert. momentum tendency terms ( NH, QH only ).
C/\  | | |
C/\  | | |-UPDATE_SURF_DR :: Update the surface-level thickness fraction.
C/\  | | |
C/\  | | |-UPDATE_CG2D    :: Update 2d conjugate grad. for Free-Surf.
C/\  | | |
C/\  | | |-SOLVE_FOR_PRESSURE           :: Find surface pressure.
C/\  | | | |-CALC_DIV_GHAT     :: Form the RHS of the surface pressure eqn.
C/\  | | | |-CG2D              :: Two-dim pre-con. conjugate-gradient.
C/\  | | | |-CG3D              :: Three-dim pre-con. conjugate-gradient solver.
C/\  | | |
C/\  | | |-THE_CORRECTION_STEP          :: Step forward to next time step.
C/\  | | | |
C/\  | | | |-CALC_GRAD_PHI_SURF :: Return DDx and DDy of surface pressure
C/\  | | | |-CORRECTION_STEP    :: Pressure correction to momentum
C/\  | | | |-CYCLE_TRACER       :: Move tracers forward in time.
C/\  | | | |-OBCS_APPLY         :: Open bndy package. see pkg/obcs
C/\  | | | |-SHAP_FILT_APPLY    :: Shapiro filter package. see pkg/shap_filt
C/\  | | | |-ZONAL_FILT_APPLY   :: FFT filter package. see pkg/zonal_filt
C/\  | | | |-CONVECTIVE_ADJUSTMENT :: Control static instability mixing.
C/\  | | | | |-FIND_RHO  :: Find adjacent densities.
C/\  | | | | |-CONVECT   :: Mix static instability.
C/\  | | | | |-TIMEAVE_CUMULATE :: Update convection statistics.
C/\  | | | |
C/\  | | | |-CALC_EXACT_ETA        :: Change SSH to flow divergence.
C/\  | | |
C/\  | | |-DO_FIELDS_BLOCKING_EXCHANGES :: Sync up overlap regions.
C/\  | | | |-EXCH
C/\  | | |
C/\  | | |-GCHEM_FORCING_SEP :: tracer forcing for gchem pkg (if
C/\  | | |                      tracer dependent tendencies calculated
C/\  | | |                      separatly)
C/\  | | |
C/\  | | |-FLT_MAIN         :: Float package ( pkg/flt ).
C/\  | | |
C/\  | | |-MONITOR          :: Monitor package ( pkg/monitor ).
C/\  | | |
C/\  | | |-DO_THE_MODEL_IO  :: Standard diagnostic I/O.
C/\  | | | |-WRITE_STATE    :: Core state I/O
C/\  | | | |-TIMEAVE_STATV_WRITE :: Time averages. see pkg/timeave
C/\  | | | |-AIM_WRITE_DIAGS     :: Intermed. atmos diags. see pkg/aim
C/\  | | | |-GMREDI_DIAGS        :: GM diags. see pkg/gmredi
C/\  | | | |-KPP_DO_DIAGS        :: KPP diags. see pkg/kpp
C/\  | | | |-SBO_CALC            :: SBO diags. see pkg/sbo
C/\  | | | |-SBO_DIAGS           :: SBO diags. see pkg/sbo
C/\  | | | |-SEAICE_DO_DIAGS     :: SEAICE diags. see pkg/seaice
C/\  | | | |-GCHEM_DIAGS         :: gchem diags. see pkg/gchem
C/\  | | |
C/\  | | |-WRITE_CHECKPOINT :: Do I/O for restart files.
C/\  | |
C/\  | |-COST_TILE        :: Cost function package. ( see pkg/cost )
C<===|=|
C<===|=| **************************
C<===|=| END MAIN TIMESTEPPING LOOP
C<===|=| **************************
C<===|=|
C    | |-COST_FINAL       :: Cost function package. ( see pkg/cost )
C    |
C    |-WRITE_CHECKPOINT :: Final state storage, for restart.
C    |
C    |-TIMER_PRINTALL :: Computational timing summary
C    |
C    |-COMM_STATS     :: Summarise inter-proc and inter-thread communication
C                     :: events.
C
C \ev
C
C
CEOI


#include "AD_CONFIG.h"
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP

C     !ROUTINE: THE_MODEL_MAIN

C     !INTERFACE:
      SUBROUTINE THE_MODEL_MAIN(myThid)
      IMPLICIT NONE

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE THE_MODEL_MAIN
C     | o Master controlling routine for model using the MITgcm
C     |   UV parallel wrapper.
C     *==========================================================*
C     | THE_MODEL_MAIN is invoked by the MITgcm UV parallel
C     | wrapper with a single integer argument "myThid". This
C     | variable identifies the thread number of an instance of
C     | THE_MODEL_MAIN. Each instance of THE_MODEL_MAIN works
C     | on a particular region of the models domain and
C     | synchronises with other instances as necessary. The
C     | routine has to "understand" the MITgcm parallel
C     | environment and the numerical algorithm. Editing this
C     | routine is best done with some knowledge of both aspects.
C     | Notes
C     | =====
C     | C*P* comments indicating place holders for which code is
C     |      presently being developed.
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     THE_MODEL_MAIN()
C       |
C       |
C       |--INITIALISE_FIXED
C       |   o Set model configuration (fixed arrays)
C       |     Topography, hydrography, timestep, grid, etc..
C       |
C       |--CTRL_UNPACK      o Derivative mode. Unpack control vector.
C       |
C       |--ADTHE_MAIN_LOOP  o Main timestepping loop for combined
C       |                     prognostic and reverse mode integration.
C       |
C       |--CTRL_PACK        o Derivative mode. Unpack control vector.
C       |
C       |--GRDCHK_MAIN      o Gradient check control routine.
C       |
C       |--THE_MAIN_LOOP    o Main timestepping loop for pure prognostic
C       |                     integration.
C       |
C       |--WRITE_CHECKPOINT o Write retsart information.
C       |
C       |--TIMER_PRINTALL   o Print out timing statistics.
C       |
C       |--COMM_STATS       o Print out communication statistics.

C     !USES:
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif
#ifdef ALLOW_CTRL
# include "ctrl.h"
# include "optim.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Note: Under the multi-threaded model myCurrentIter and
C           myCurrentTime are local variables passed around as routine
C           arguments. Although this is fiddly it saves the need to
C           impose additional synchronisation points when they are
C           updated.
C     myCurrentTime - Time counter for this thread
C     myCurrentIter - Iteration counter for this thread
      INTEGER myCurrentIter
      _RL     myCurrentTime
      logical  exst
      logical  lastdiva
CEOP
c--   set default:
      lastdiva = .TRUE.

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_MODEL_MAIN',myThid)
#endif

#if defined(USE_PAPI) || defined(USE_PCL_FLOPS_SFP) || defined(USE_PCL_FLOPS) || defined(USE_PCL)
      CALL TIMER_CONTROL('','INIT','THE_MODEL_MAIN',myThid)
#endif
C--   This timer encompasses the whole code
      CALL TIMER_START('ALL                    [THE_MODEL_MAIN]',myThid)

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INITIALISE_FIXED',myThid)
#endif
C--   Set model configuration (fixed arrays)
      CALL TIMER_START('INITIALISE_FIXED       [THE_MODEL_MAIN]',myThid)
      CALL INITIALISE_FIXED( myThid )
      CALL TIMER_STOP ('INITIALISE_FIXED       [THE_MODEL_MAIN]',myThid)

      myCurrentTime = startTime
      myCurrentIter = nIter0

#if ( defined (ALLOW_ADMTLM) )
c
      STOP 'should never get here; ADMTLM_DSVD calls ADMTLM_DRIVER'
c
#elif ( defined (ALLOW_AUTODIFF))

# ifndef EXCLUDE_CTRL_PACK
         inquire( file='costfinal', exist=exst )
         IF ( .NOT. exst ) THEN
            IF ( (optimcycle.NE.0 .OR. .NOT.doinitxx)
     &           .AND. doMainUnpack ) THEN
               CALL TIMER_START('CTRL_UNPACK   [THE_MODEL_MAIN]',mythid)
               CALL CTRL_UNPACK( .TRUE. , mythid )
               CALL TIMER_STOP ('CTRL_UNPACK   [THE_MODEL_MAIN]',mythid)
            ENDIF
         ENDIF
# endif /* EXCLUDE_CTRL_PACK */

# ifdef ALLOW_COST
      CALL COST_DEPENDENT_INIT ( mythid )
      _BARRIER
# endif

# if ( defined (ALLOW_TANGENTLINEAR_RUN) )
c
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('G_THE_MAIN_LOOP',myThid)
#endif
      CALL TIMER_START('G_THE_MAIN_LOOP           [TANGENT RUN]',mythid)
      CALL G_THE_MAIN_LOOP ( myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('G_THE_MAIN_LOOP           [TANGENT RUN]',mythid)
c
# elif ( defined (ALLOW_ADJOINT_RUN) || \
         defined (ALLOW_ECCO_OPTIMIZATION) )
c
#  ifdef ALLOW_DIVIDED_ADJOINT
c-- The following assumes the TAF option '-pure'
      inquire( file='costfinal', exist=exst )
      IF ( .NOT. exst) THEN
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('MDTHE_MAIN_LOOP',myThid)
#endif
         CALL TIMER_START('MDTHE_MAIN_LOOP            [MD RUN]', mythid)
         CALL MDTHE_MAIN_LOOP ( myCurrentTime, myCurrentIter, mythid )
         CALL TIMER_STOP ('MDTHE_MAIN_LOOP            [MD RUN]', mythid)
         CALL COST_FINAL_STORE ( mythid, lastdiva )
      ELSE
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('ADTHE_MAIN_LOOP',myThid)
#endif
         CALL TIMER_START('ADTHE_MAIN_LOOP       [ADJOINT RUN]', mythid)
         CALL ADTHE_MAIN_LOOP (  mythid )
         CALL TIMER_STOP ('ADTHE_MAIN_LOOP       [ADJOINT RUN]', mythid)
         CALL COST_FINAL_RESTORE ( mythid, lastdiva )
      ENDIF
c--
#  else /* ALLOW_DIVIDED_ADJOINT undef */
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('ADTHE_MAIN_LOOP',myThid)
#endif
      CALL TIMER_START('ADTHE_MAIN_LOOP          [ADJOINT RUN]', mythid)
#ifdef ALLOW_ECCO
      CALL ADTHE_MAIN_LOOP ( myCurrentTime, mythid )
#else
      CALL ADTHE_MAIN_LOOP ( mythid )
#endif
      CALL TIMER_STOP ('ADTHE_MAIN_LOOP          [ADJOINT RUN]', mythid)
#  endif /* ALLOW_DIVIDED_ADJOINT */
c
# else /* forward run only within AD setting */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('THE_MAIN_LOOP',myThid)
#endif
C--   Call time stepping loop of full model
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('THE_MAIN_LOOP',myThid)
#endif
      CALL TIMER_START('THE_MAIN_LOOP          [THE_MODEL_MAIN]',myThid)
      CALL THE_MAIN_LOOP( myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('THE_MAIN_LOOP          [THE_MODEL_MAIN]',myThid)

# endif

# ifndef EXCLUDE_CTRL_PACK
      IF ( lastdiva .AND. doMainPack ) THEN
         CALL TIMER_START('CTRL_PACK           [THE_MODEL_MAIN]',mythid)
         CALL CTRL_PACK( .FALSE. , mythid )
         CALL TIMER_STOP ('CTRL_PACK           [THE_MODEL_MAIN]',mythid)
         IF ( ( optimcycle.EQ.0 .OR. (.NOT. doMainUnpack) )
     &        .AND. mycurrentiter.EQ.niter0 ) THEN
            CALL TIMER_START('CTRL_PACK     [THE_MODEL_MAIN]',mythid)
            CALL CTRL_PACK( .TRUE. , mythid )
            CALL TIMER_STOP ('CTRL_PACK     [THE_MODEL_MAIN]',mythid)
         ENDIF
      ENDIF
# endif /* EXCLUDE_CTRL_PACK */


# ifdef ALLOW_GRDCHK
      IF ( useGrdchk .AND. lastdiva ) THEN
         CALL TIMER_START('GRDCHK_MAIN         [THE_MODEL_MAIN]',mythid)
         CALL GRDCHK_MAIN( mythid )
         CALL TIMER_STOP ('GRDCHK_MAIN         [THE_MODEL_MAIN]',mythid)
         _BARRIER
      ENDIF
# endif

#else /* ALL AD-related undef */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('THE_MAIN_LOOP',myThid)
#endif
C--   Call time stepping loop of full model
      CALL TIMER_START('THE_MAIN_LOOP          [THE_MODEL_MAIN]',myThid)
      CALL THE_MAIN_LOOP( myCurrentTime, myCurrentIter, myThid )
      CALL TIMER_STOP ('THE_MAIN_LOOP          [THE_MODEL_MAIN]',myThid)

#endif /* ALLOW_TANGENTLINEAR_RUN ALLOW_ADJOINT_RUN ALLOW_ADMTLM */

#ifdef ALLOW_MNC
      IF (useMNC) THEN
C       Close all open NetCDF files
        _BEGIN_MASTER( mythid )
        CALL MNC_FILE_CLOSE_ALL( myThid )
        _END_MASTER( mythid )
      ENDIF
#endif

C--   This timer encompasses the whole code
      CALL TIMER_STOP ('ALL                    [THE_MODEL_MAIN]',myThid)

C--   Write timer statistics
      IF ( myThid .EQ. 1 ) THEN
       CALL TIMER_PRINTALL( myThid )
       CALL COMM_STATS
      ENDIF

C--   Check threads synchronization :
      CALL BAR_CHECK( 9, myThid )

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THE_MODEL_MAIN',myThid)
#endif

      RETURN
      END
