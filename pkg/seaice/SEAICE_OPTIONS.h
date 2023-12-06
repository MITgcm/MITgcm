#ifndef SEAICE_OPTIONS_H
#define SEAICE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C     *==========================================================*
C     | SEAICE_OPTIONS.h
C     | o CPP options file for sea ice package.
C     *==========================================================*
C     | Use this file for selecting options within the sea ice
C     | package.
C     *==========================================================*

#ifdef ALLOW_SEAICE
C---  Package-specific Options & Macros go here

C--   Write "text-plots" of certain fields in STDOUT for debugging.
#undef SEAICE_DEBUG

C--   By default, the sea-ice package uses its own integrated bulk
C     formulae to compute fluxes (fu, fv, EmPmR, Qnet, and Qsw) over
C     open-ocean.  When this flag is set, these variables are computed
C     in a separate external package, for example, pkg/exf, and then
C     modified for sea-ice effects by pkg/seaice.
#define SEAICE_EXTERNAL_FLUXES

C--   This CPP flag has been retired.  The number of ice categories
C     used to solve for seaice flux is now specified by run-time
C     parameter SEAICE_multDim.
C     Note: be aware of pickup_seaice.* compatibility issues when
C     restarting a simulation with a different number of categories.
c#define SEAICE_MULTICATEGORY

C--   run with sea Ice Thickness Distribution (ITD);
C     set number of categories (nITD) in SEAICE_SIZE.h
#undef SEAICE_ITD

C--   Since the missing sublimation term is now included
C     this flag is needed for backward compatibility
#undef SEAICE_DISABLE_SUBLIM

C--   Suspected missing term in coupled ocn-ice heat budget (to be confirmed)
#undef SEAICE_DISABLE_HEATCONSFIX

C--   Default is constant seaice salinity (SEAICE_salt0); Define the following
C     flag to consider (space & time) variable salinity: advected and forming
C     seaice with a fraction (=SEAICE_saltFrac) of freezing seawater salinity.
C- Note: SItracer also offers an alternative way to handle variable salinity.
#undef SEAICE_VARIABLE_SALINITY

C--   Enable grease ice parameterization (requires to define ALLOW_SITRACER):
C     The grease ice parameterization delays formation of solid sea ice from
C     frazil ice by a time constant and provides a dynamic calculation of the
C     initial solid sea ice thickness HO as a function of winds, currents and
C     available grease ice volume. Grease ice does not significantly reduce heat
C     loss from the ocean in winter and area covered by grease is thus handled
C     like open water (For details see Smedsrud and Martin, 2014, Ann.Glac.).
C     Set SItrName(1) = 'grease' in namelist SEAICE_PARM03 in data.seaice
C     then output SItr01 is SItrNameLong(1) = 'grease ice volume fraction',
C     with SItrUnit(1) = '[0-1]', which needs to be multiplied by SIheff to
C     yield grease ice volume. Additionally, the actual grease ice layer
C     thickness (diagnostic SIgrsLT) can be saved.
#undef SEAICE_GREASE

C--   Tracers of ice and/or ice cover.
#ifdef SEAICE_GREASE
C     SEAICE_GREASE code requires to define ALLOW_SITRACER
# define ALLOW_SITRACER
#else
# undef ALLOW_SITRACER
#endif
#ifdef ALLOW_SITRACER
C-    To try avoid 'spontaneous generation' of tracer maxima by advdiff.
# define ALLOW_SITRACER_ADVCAP

C-    Include code to diagnose sea ice tracer budgets in
C     seaice_advdiff.F and seaice_tracer_phys.F. Diagnostics are
C     computed the "call diagnostics_fill" statement is commented out.
# undef ALLOW_SITRACER_DEBUG_DIAG
#endif /* ALLOW_SITRACER */

C--   Allow sea-ice dynamic code. These options are provided so that,
C     if turned off (#undef), to compile (and process with TAF) only the
C     the thermodynamics component of the code. Note that, if needed,
C     sea-ice dynamics can be turned off at runtime (SEAICEuseDYNAMICS=F).

C--   Historically, the seaice model was discretized on a B-Grid. This
C     discretization should still work but it is not longer actively
C     tested and supported. Define this flag to compile it. It cannot be
C     defined together with SEAICE_CGRID
#undef SEAICE_BGRID_DYNAMICS

C--   The following flag should always be set in order to use C the
C--   operational C-grid discretization.
#define SEAICE_CGRID

#ifdef SEAICE_CGRID
C--   Options for the C-grid version only:

C     enable advection of sea ice momentum
# undef SEAICE_ALLOW_MOM_ADVECTION

C     enable JFNK code by defining the following flag
# define SEAICE_ALLOW_JFNK

C     enable Krylov code by defining the following flag
# define SEAICE_ALLOW_KRYLOV

C--   Use a different order when mapping 2D velocity arrays to 1D vector
C     before passing it to FGMRES.
# undef SEAICE_JFNK_MAP_REORDER

C     to reproduce old verification results for JFNK
# undef SEAICE_PRECOND_EXTRA_EXCHANGE

C     enable LSR to use global (multi-tile) tri-diagonal solver
# undef SEAICE_GLOBAL_3DIAG_SOLVER

C     enable EVP code by defining the following flag
# define SEAICE_ALLOW_EVP
# ifdef SEAICE_ALLOW_EVP
C-    When set use SEAICE_zetaMin and SEAICE_evpDampC to limit viscosities
C     from below and above in seaice_evp: not necessary, and not recommended
#  undef SEAICE_ALLOW_CLIPZETA

C     Include code to avoid underflows in EVP-code (copied from CICE).
C     Many compilers can handle this more efficiently with the help of a flag.
#  undef SEAICE_EVP_ELIMINATE_UNDERFLOWS

C     Include code to print residual of EVP iteration for debugging/diagnostics
#  undef ALLOW_SEAICE_EVP_RESIDUAL
# endif /* SEAICE_ALLOW_EVP */

C     smooth regularization (without max-function) of delta for
C     better differentiability
# undef SEAICE_DELTA_SMOOTHREG

C     regularize zeta to zmax with a smooth tanh-function instead
C     of a min(zeta,zmax). This improves convergence of iterative
C     solvers (Lemieux and Tremblay 2009, JGR). No effect on EVP
# define SEAICE_ZETA_SMOOTHREG

C--   Different yield curves within the VP rheology framework
C     allow the truncated ellipse rheology (runtime flag SEAICEuseTEM)
# undef SEAICE_ALLOW_TEM

C     allow the use of the Mohr Coulomb rheology (runtime flag SEAICEuseMCS)
C     as defined in (Ip 1991) /!\ This is known to give unstable results,
C     use with caution
# undef SEAICE_ALLOW_MCS

C     allow the use of Mohr Coulomb with elliptical plastic potential
C     (runtime flag SEAICEuseMCE)
# undef SEAICE_ALLOW_MCE

C     allow the teardrop and parabolic lens  rheology
C     (runtime flag SEAICEuseTD and SEAICEusePL)
# undef SEAICE_ALLOW_TEARDROP

C--   LSR solver settings
C     Use LSR vector code; not useful on non-vector machines, because it
C     slows down convergence considerably, but the extra iterations are
C     more than made up by the much faster code on vector machines. For
C     the only regularly test vector machine these flags a specified
C     in the build options file SUPER-UX_SX-8_sxf90_awi, so that we comment
C     them out here.
# undef SEAICE_VECTORIZE_LSR

C     Use zebra-method (alternate lines) for line-successive-relaxation
C     This modification improves the convergence of the vector code
C     dramatically, so that is may actually be useful in general, but
C     that needs to be tested. Can be used without vectorization options.
# undef SEAICE_LSR_ZEBRA

C     Include code to print residual of nonlinear outer loop of LSR
# undef SEAICE_ALLOW_CHECK_LSR_CONVERGENCE

C     This flag is also required for an actual adjoint of seaice_lsr;
C     increases memory requirements a lot.
# undef SEAICE_LSR_ADJOINT_ITER

C     Use parameterisation of grounding ice for a better representation
C     of fastice in shallow seas
# undef SEAICE_ALLOW_BOTTOMDRAG

C     Allow using the flexible LSR solver, where the number of non-linear
C     iteration depends on the residual. Good for when a non-linear
C     convergence criterion must be satified
# undef SEAICE_ALLOW_LSR_FLEX

#endif /* SEAICE_CGRID */

#ifdef SEAICE_BGRID_DYNAMICS
C--   Options for the B-grid version only:

C-    By default for B-grid dynamics solver wind stress under sea-ice is
C     set to the same value as it would be if there was no sea-ice.
C     Define following CPP flag for B-grid ice-ocean stress coupling.
# define SEAICE_BICE_STRESS

C-    By default for B-grid dynamics solver surface tilt is obtained
C     indirectly via geostrophic velocities. Define following CPP
C     in order to use ETAN instead.
# define EXPLICIT_SSH_SLOPE

C-    Defining this flag turns on FV-discretization of the B-grid LSOR solver.
C     It is smoother and includes all metric terms, similar to C-grid solvers.
C     It is here for completeness, but its usefulness is unclear.
# undef SEAICE_LSRBNEW

#endif /* SEAICE_BGRID_DYNAMICS */

C--   Some regularisations
C-    When set limit the Ice-Loading to mass of 1/5 of Surface ocean grid-box
#undef SEAICE_CAP_ICELOAD

C-    When set use SEAICE_clipVelocties = .true., to clip U/VICE at 40cm/s,
C     not recommended
#undef SEAICE_ALLOW_CLIPVELS

C-    When set cap the sublimation latent heat flux in solve4temp according
C     to the available amount of ice+snow. Otherwise this term is treated
C     like all of the others -- residuals heat and fw stocks are passed to
C     the ocean at the end of seaice_growth in a conservative manner.
C     SEAICE_CAP_SUBLIM is not needed as of now, but kept just in case.
#undef SEAICE_CAP_SUBLIM

C--   AD flags
C-    TAF related flag, currently only used in seaice_ad_check_lev[1-4]_dir.h;
C     it is unclear if this is ever needed.
#undef AUTODIFF_SOMETIMES_NEEDED

C-    Reset fields to zero to stabilise AD code of dynamics solver
C     (resulting in wrong gradients)
#undef SEAICE_DYN_STABLE_ADJOINT

C-    Another flag to simplify dependencies for TAF-generated AD-code
C     the thermodynamic part, mostly by resetting variables to zero
#undef SEAICE_MODIFY_GROWTH_ADJ

C-    Special seaice flag for AD testing
#undef SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING

C--   Use the adjointable sea-ice thermodynamic model
C     in seaice_growth_adx.F instead of seaice_growth.F
C     This options excludes more complex physics such
C     as sublimation, ITD, and frazil.
#undef SEAICE_USE_GROWTH_ADX

C--   These flags are not strictly AD-related but may help obtaining
C     simpler AD-code:
C-    Do not compile code that resets AREA (or AREAITD) to a mininum value
C     of SEAICE_area_floor (=SIeps with default of 1e-5) if there is
C     some finite sea ice thickness
#undef DISABLE_AREA_FLOOR

C-    Do not compile growth/thermodynamics code (avoiding this code can
C     also be done by setting runtime parameter usePWthermodynamics=F)
#undef DISABLE_SEAICE_GROWTH

C-    Do not compile/use seaice-related obcs code when using obcs.
#undef DISABLE_SEAICE_OBCS

C--   Enable free drift code
#undef SEAICE_ALLOW_FREEDRIFT

C--   pkg/seaice cost functions compile flags
C-    Sea-ice volume (requires pkg/cost)
#undef ALLOW_COST_ICE
#ifdef ALLOW_COST_ICE
C-    Enable template for sea-ice volume export in seaice_cost_export.F
C     (requires pkg/cost & ALLOW_COST_ICE defined)
# undef ALLOW_SEAICE_COST_EXPORT
#endif /* ALLOW_COST_ICE */

#endif /* ALLOW_SEAICE */
#endif /* SEAICE_OPTIONS_H */
