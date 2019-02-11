C     *==========================================================*
C     | SEAICE_OPTIONS.h
C     | o CPP options file for sea ice package.
C     *==========================================================*
C     | Use this file for selecting options within the sea ice
C     | package.
C     *==========================================================*

#ifndef SEAICE_OPTIONS_H
#define SEAICE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_SEAICE
C     Package-specific Options & Macros go here

C--   Write "text-plots" of certain fields in STDOUT for debugging.
#undef SEAICE_DEBUG

C--   Allow sea-ice dynamic code.
C     This option is provided to allow use of TAMC
C     on the thermodynamics component of the code only.
C     Sea-ice dynamics can also be turned off at runtime
C     using variable SEAICEuseDYNAMICS.
#define SEAICE_ALLOW_DYNAMICS

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
#define SEAICE_DISABLE_SUBLIM

C--   Suspected missing term in coupled ocn-ice heat budget (to be confirmed)
#define SEAICE_DISABLE_HEATCONSFIX

C--   Default is constant seaice salinity (SEAICE_salt0); Define the following
C     flag to consider (space & time) variable salinity: advected and forming
C     seaice with a fraction (=SEAICE_saltFrac) of freezing seawater salinity.
C- Note: SItracer also offers an alternative way to handle variable salinity.
#define SEAICE_VARIABLE_SALINITY

C--   Tracers of ice and/or ice cover.
#undef ALLOW_SITRACER
#ifdef ALLOW_SITRACER
C--   To try avoid 'spontaneous generation' of tracer maxima by advdiff.
# define ALLOW_SITRACER_ADVCAP
#endif

C--   Enable grease ice parameterization
C     The grease ice parameterization delays formation of solid
C     sea ice from frazil ice by a time constant and provides a
C     dynamic calculation of the initial solid sea ice thickness
C     HO as a function of winds, currents and available grease ice
C     volume. Grease ice does not significantly reduce heat loss
C     from the ocean in winter and area covered by grease is thus
C     handled like open water.
C     (For details see Smedsrud and Martin, 2014, Ann.Glac.)
C     Set SItrName(1) = 'grease' in namelist SEAICE_PARM03 in data.seaice
C     then output SItr01 is SItrNameLong(1) = 'grease ice volume fraction',
C     with SItrUnit(1) = '[0-1]', which needs to be multiplied by SIheff
C     to yield grease ice volume. Additionally, the actual grease ice
C     layer thickness (diagnostic SIgrsLT) can be saved.
#undef SEAICE_GREASE
C--   grease ice uses SItracer:
#ifdef SEAICE_GREASE
# define ALLOW_SITRACER
# define ALLOW_SITRACER_ADVCAP
#endif

C--   Historically, the seaice model was discretized on a B-Grid. This
C     discretization should still work but it is not longer actively tested
C     and supported. The following flag should always be set in order to use
C     the operational C-grid discretization.
#define SEAICE_CGRID

C--   Only for the C-grid version it is possible to
#ifdef SEAICE_CGRID
C     enable advection of sea ice momentum
# undef SEAICE_ALLOW_MOM_ADVECTION
C     enable JFNK code by defining the following flag
# undef SEAICE_ALLOW_JFNK
C     enable Krylov code by defining the following flag
# undef SEAICE_ALLOW_KRYLOV
C     enable LSR to use global (multi-tile) tri-diagonal solver
# undef SEAICE_GLOBAL_3DIAG_SOLVER
C     enable EVP code by defining the following flag
# define SEAICE_ALLOW_EVP
# ifdef SEAICE_ALLOW_EVP
C--   When set use SEAICE_zetaMin and SEAICE_evpDampC to limit viscosities
C     from below and above in seaice_evp: not necessary, and not recommended
#  undef SEAICE_ALLOW_CLIPZETA
# endif /* SEAICE_ALLOW_EVP */
C     smooth regularization (without max-function) of delta for
C     better differentiability
# undef SEAICE_DELTA_SMOOTHREG
C     regularize zeta to zmax with a smooth tanh-function instead
C     of a min(zeta,zmax). This improves convergence of iterative
C     solvers (Lemieux and Tremblay 2009, JGR). No effect on EVP
# undef SEAICE_ZETA_SMOOTHREG
C     allow the truncated ellipse rheology (runtime flag SEAICEuseTEM)
# undef SEAICE_ALLOW_TEM
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
# define SEAICE_LSR_ZEBRA
C     This flag is also required for an actual adjoint of seaice_lsr; 
C     increases memory requirements a lot.
# define SEAICE_LSR_ADJOINT_ITER
C     Use parameterisation of grounding ice for a better representation
C     of fastice in shallow seas
# undef SEAICE_ALLOW_BOTTOMDRAG
#else /* not SEAICE_CGRID, but old B-grid */
C--   By default for B-grid dynamics solver wind stress under sea-ice is
C     set to the same value as it would be if there was no sea-ice.
C     Define following CPP flag for B-grid ice-ocean stress coupling.
# define SEAICE_BICE_STRESS

C--   By default for B-grid dynamics solver surface tilt is obtained
C     indirectly via geostrophic velocities. Define following CPP
C     in order to use ETAN instead.
# define EXPLICIT_SSH_SLOPE
C--   Defining this flag turns on FV-discretization of the B-grid LSOR solver.
C     It is smoother and includes all metric terms, similar to C-grid solvers.
C     It is here for completeness, but its usefulness is unclear.
# undef SEAICE_LSRBNEW
#endif /* SEAICE_CGRID */

C--   When set limit the Ice-Loading to mass of 1/5 of Surface ocean grid-box
#undef SEAICE_CAP_ICELOAD
C--   When set use SEAICE_clipVelocties = .true., to clip U/VICE at 40cm/s,
C     not recommended
#undef SEAICE_ALLOW_CLIPVELS
C--   When set cap the sublimation latent heat flux in solve4temp according
C     to the available amount of ice+snow. Otherwise this term is treated
C     like all of the others -- residuals heat and fw stocks are passed to
C     the ocean at the end of seaice_growth in a conservative manner.
C     SEAICE_CAP_SUBLIM is not needed as of now, but kept just in case.
#undef SEAICE_CAP_SUBLIM

C--   Enable free drift code
#define SEAICE_ALLOW_FREEDRIFT

C--   pkg/seaice cost functions compile flags
c       >>> Sea-ice volume (requires pkg/cost)
#undef ALLOW_COST_ICE
c       >>> Sea-ice misfit to obs (requires pkg/cost and ecco)
#define ALLOW_SEAICE_COST_SMR_AREA

#endif /* ALLOW_SEAICE */
#endif /* SEAICE_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
