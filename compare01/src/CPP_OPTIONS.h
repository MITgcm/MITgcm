C $Id: CPP_OPTIONS.h,v 1.2 1998/06/08 21:42:59 cnh Exp $
C Set compile time options.
C Notes:
C Names of the flags are meant to be self-explanatory.
C All flags are set here and not left to defaults or
C set as compiler options.
C Using #define implies switching the corresponding feature on.
C Using #undef  implies switching the corresponding feature off.

C Axis ordering
C =============
C This option controls the ordering of axes in three-dimensional arrays.
C Note: Only one of these should be switched "on" ( i.e. only one #define ).
#define _IJK
#undef  _KIJ

C Debugging
C =========
C This option controls whether debug statements are active.
#undef   _WRITE_DEBUG

C Precision
C =========
C This option controls the precision to which variables are held.
#define _ALL_DOUBLE

C Eddy parameterisation
C =====================
C Use to switch on or off the GMGS code.
#undef  GMGS

C Momentum equation terms
C =======================
C These options control which terms are active in the momentum equations.
#define _LAPLACIAN_DIFFUSION_OF_MOMENTUM
#undef  _BIHARMONIC_DIFFUSION_OF_MOMENTUM
#define _ADVECTION_OF_MOMENTUM
#define _SPHERICAL_POLAR_METRIC_TERMS
#undef  _XZ_CORIOLIS
#define _XY_CORIOLIS
#define _MOMENTUM_FORCING

C Temperature equation terms
C ==========================
C These options control which terms are active in the temperature equation.
#define _LAPLACIAN_DIFFUSION_OF_TEMPERATURE
#undef  _BIHARMONIC_DIFFUSION_OF_TEMPERATURE
#define _ADVECTION_OF_TEMPERATURE
#undef  _TEMPERATURE_FORCING
