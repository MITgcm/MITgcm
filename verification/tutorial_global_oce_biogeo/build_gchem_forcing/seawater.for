

  








CBOP
C !ROUTINE: CPP_OPTIONS.h
C !INTERFACE:
C #include "CPP_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | main CPP options file for the model:
C | Control which optional features to compile in model/src code.
C *==================================================================*
CEOP

C CPP flags controlling particular source code features

C-- Forcing code options:

C o Shortwave heating as extra term in APPLY_FORCING_T (apply_forcing.F)


C o Include/exclude Geothermal Heat Flux at the bottom of the ocean


C o Allow to account for heating due to friction (and momentum dissipation)


C o Allow mass source or sink of Fluid in the interior
C   (3-D generalisation of oceanic real-fresh water flux)


C o Include pressure loading code


C o Include/exclude balancing surface forcing fluxes code


C o Include/exclude balancing surface forcing relaxation code


C o Include/exclude checking for negative salinity


C-- Options to discard parts of the main code:

C o Exclude/allow external forcing-fields load
C   this allows to read & do simple linear time interpolation of oceanic
C   forcing fields, if no specific pkg (e.g., EXF) is used to compute them.

C   If defined, use same method (with pkg/autodiff compiled or not) for checking
C   when to load new reccord ; by default, use simpler method with pkg/autodiff.


C o Include/exclude phi_hyd calculation code


C o Include/exclude sound speed calculation code
C o (Note that this is a diagnostic from Del Grasso algorithm, not derived
C    from EOS)


C-- Vertical mixing code options:

C o Include/exclude calling S/R CONVECTIVE_ADJUSTMENT


C o Include/exclude calling S/R CONVECTIVE_ADJUSTMENT_INI, turned off by
C   default because it is an unpopular historical left-over


C o Include/exclude call to S/R CALC_DIFFUSIVITY


C o Allow full 3D specification of vertical diffusivity


C o Allow latitudinally varying BryanLewis79 vertical diffusivity


C o Exclude/allow partial-cell effect (physical or enhanced) in vertical mixing
C   this allows to account for partial-cell in vertical viscosity and diffusion,
C   either from grid-spacing reduction effect or as artificially enhanced mixing
C   near surface & bottom for too thin grid-cell


C o Exclude/allow to use isotropic 3-D Smagorinsky viscosity as diffusivity
C   for tracers (after scaling by constant Prandtl number)


C-- Time-stepping code options:

C o Include/exclude combined Surf.Pressure and Drag Implicit solver code


C o Include/exclude Implicit vertical advection code


C o Include/exclude AdamsBashforth-3rd-Order code


C o Include/exclude Quasi-Hydrostatic Stagger Time-step AdamsBashforth code


C-- Model formulation options:

C o Allow the use of Non-Linear Free-Surface formulation
C   this implies that grid-cell thickness (hFactors) varies with time

C o Disable code for rStar coordinate and/or code for Sigma coordinate
c#define DISABLE_RSTAR_CODE
c#define DISABLE_SIGMA_CODE

C o Include/exclude nonHydrostatic code


C o Include/exclude GM-like eddy stress in momentum code


C-- Algorithm options:

C o Include/exclude code for Non Self-Adjoint (NSA) conjugate-gradient solver


C o Include/exclude code for single reduction Conjugate-Gradient solver


C o Choices for implicit solver routines solve_*diagonal.F
C   The following has low memory footprint, but not suitable for AD

C   The following one suitable for AD but does not vectorize


C   Implementation alternative (might be faster on some platforms ?)


C-- Retired code options:

C-  These 2 flags: ISOTROPIC_COS_SCALING & COSINEMETH_III have no effect
C   here as they are reset in GAD_OPTIONS.h and in MOM_COMMON_OPTIONS.h
C   for tracer diffusivity and momentum viscosity respectively

C o Use "OLD" UV discretisation near boundaries (*not* recommended)
C   Note - only works with pkg/mom_fluxform and "no_slip_sides=.FALSE."
C          because the old code did not have no-slip BCs


C o Use LONG.bin, LATG.bin, etc., initialization for ini_curviliear_grid.F
C   Default is to use "new" grid files (OLD_GRID_IO undef) but OLD_GRID_IO
C   is still useful with, e.g., single-domain curvilinear configurations.


C o Use old EXTERNAL_FORCING_U,V,T,S subroutines (for backward compatibility)


C-- Other option files:

C o Execution environment support options


CBOP
C     !ROUTINE: CPP_EEOPTIONS.h
C     !INTERFACE:
C     include "CPP_EEOPTIONS.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | CPP\_EEOPTIONS.h                                         |
C     *==========================================================*
C     | C preprocessor "execution environment" supporting        |
C     | flags. Use this file to set flags controlling the        |
C     | execution environment in which a model runs - as opposed |
C     | to the dynamical problem the model solves.               |
C     | Note: Many options are implemented with both compile time|
C     |       and run-time switches. This allows options to be   |
C     |       removed altogether, made optional at run-time or   |
C     |       to be permanently enabled. This convention helps   |
C     |       with the data-dependence analysis performed by the |
C     |       adjoint model compiler. This data dependency       |
C     |       analysis can be upset by runtime switches that it  |
C     |       is unable to recoginise as being fixed for the     |
C     |       duration of an integration.                        |
C     |       A reasonable way to use these flags is to          |
C     |       set all options as selectable at runtime but then  |
C     |       once an experimental configuration has been        |
C     |       identified, rebuild the code with the appropriate  |
C     |       options set at compile time.                       |
C     *==========================================================*
CEOP

C     In general the following convention applies:
C     ALLOW  - indicates an feature will be included but it may
C     CAN      have a run-time flag to allow it to be switched
C              on and off.
C              If ALLOW or CAN directives are "undef'd" this generally
C              means that the feature will not be available i.e. it
C              will not be included in the compiled code and so no
C              run-time option to use the feature will be available.
C
C     ALWAYS - indicates the choice will be fixed at compile time
C              so no run-time option will be present

C=== Macro related options ===
C--   Control storage of floating point operands
C     On many systems it improves performance only to use
C     8-byte precision for time stepped variables.
C     Constant in time terms ( geometric factors etc.. )
C     can use 4-byte precision, reducing memory utilisation and
C     boosting performance because of a smaller working set size.
C     However, on vector CRAY systems this degrades performance.
C     Enable to switch REAL4_IS_SLOW from genmake2 (with LET_RS_BE_REAL4):


C--   Control use of "double" precision constants.
C     Use D0 where it means REAL*8 but not where it means REAL*16


C=== IO related options ===
C--   Flag used to indicate whether Fortran formatted write
C     and read are threadsafe. On SGI the routines can be thread
C     safe, on Sun it is not possible - if you are unsure then
C     undef this option.


C--   Flag used to indicate whether Binary write to Local file (i.e.,
C     a different file for each tile) and read are thread-safe.


C--   Flag to turn off the writing of error message to ioUnit zero


C--   Flag to turn on old default of opening scratch files with the
C     STATUS='SCRATCH' option. This method, while perfectly FORTRAN-standard,
C     caused filename conflicts on some multi-node/multi-processor platforms
C     in the past and has been replace by something (hopefully) more robust.


C--   Flag defined for eeboot_minimal.F, eeset_parms.F and open_copy_data_file.F
C     to write STDOUT, STDERR and scratch files from process 0 only.
C WARNING: to use only when absolutely confident that the setup is working
C     since any message (error/warning/print) from any proc <> 0 will be lost.


C=== MPI, EXCH and GLOBAL_SUM related options ===
C--   Flag turns off MPI_SEND ready_to_receive polling in the
C     gather_* subroutines to speed up integrations.


C--   Control MPI based parallel processing
CXXX We no longer select the use of MPI via this file (CPP_EEOPTIONS.h)
CXXX To use MPI, use an appropriate genmake2 options file or use
CXXX genmake2 -mpi .
CXXX #undef  ALLOW_USE_MPI

C--   Control use of communication that might overlap computation.
C     Under MPI selects/deselects "non-blocking" sends and receives.

C--   Control use of communication that is atomic to computation.
C     Under MPI selects/deselects "blocking" sends and receives.


C--   Control XY periodicity in processor to grid mappings
C     Note: Model code does not need to know whether a domain is
C           periodic because it has overlap regions for every box.
C           Model assume that these values have been
C           filled in some way.


C--   disconnect tiles (no exchange between tiles, just fill-in edges
C     assuming locally periodic subdomain)


C--   Always cumulate tile local-sum in the same order by applying MPI allreduce
C     to array of tiles ; can get slower with large number of tiles (big set-up)


C--   Alternative way of doing global sum without MPI allreduce call
C     but instead, explicit MPI send & recv calls. Expected to be slower.


C--   Alternative way of doing global sum on a single CPU
C     to eliminate tiling-dependent roundoff errors. Note: This is slow.


C=== Other options (to add/remove pieces of code) ===
C--   Flag to turn on checking for errors from all threads and procs
C     (calling S/R STOP_IF_ERROR) before stopping.


C--   Control use of communication with other component:
C     allow to import and export from/to Coupler interface.


C--   Activate some pieces of code for coupling to GEOS AGCM


C=== And define Macros ===

CBOP
C     !ROUTINE: CPP_EEMACROS.h
C     !INTERFACE:
C     include "CPP_EEMACROS.h"
C     !DESCRIPTION:
C     *==========================================================*
C     | CPP_EEMACROS.h
C     *==========================================================*
C     | C preprocessor "execution environment" supporting
C     | macros. Use this file to define macros for  simplifying
C     | execution environment in which a model runs - as opposed
C     | to the dynamical problem the model solves.
C     *==========================================================*
CEOP



C     In general the following convention applies:
C     ALLOW  - indicates an feature will be included but it may
C     CAN      have a run-time flag to allow it to be switched
C              on and off.
C              If ALLOW or CAN directives are "undef'd" this generally
C              means that the feature will not be available i.e. it
C              will not be included in the compiled code and so no
C              run-time option to use the feature will be available.
C
C     ALWAYS - indicates the choice will be fixed at compile time
C              so no run-time option will be present

C     Flag used to indicate which flavour of multi-threading
C     compiler directives to use. Only set one of these.
C     USE_SOLARIS_THREADING  - Takes directives for SUN Workshop
C                              compiler.
C     USE_KAP_THREADING      - Takes directives for Kuck and
C                              Associates multi-threading compiler
C                              ( used on Digital platforms ).
C     USE_IRIX_THREADING     - Takes directives for SGI MIPS
C                              Pro Fortran compiler.
C     USE_EXEMPLAR_THREADING - Takes directives for HP SPP series
C                              compiler.
C     USE_C90_THREADING      - Takes directives for CRAY/SGI C90
C                              system F90 compiler.












C--   Define the mapping for the _BARRIER macro
C     On some systems low-level hardware support can be accessed through
C     compiler directives here.


C--   Define the mapping for the BEGIN_CRIT() and  END_CRIT() macros.
C     On some systems we simply execute this section only using the
C     master thread i.e. its not really a critical section. We can
C     do this because we do not use critical sections in any critical
C     sections of our code!


C--   Define the mapping for the BEGIN_MASTER_SECTION() and
C     END_MASTER_SECTION() macros. These are generally implemented by
C     simply choosing a particular thread to be "the master" and have
C     it alone execute the BEGIN_MASTER..., END_MASTER.. sections.


CcnhDebugStarts
C      Alternate form to the above macros that increments (decrements) a counter each
C      time a MASTER section is entered (exited). This counter can then be checked in barrier
C      to try and detect calls to BARRIER within single threaded sections.
C      Using these macros requires two changes to Makefile - these changes are written
C      below.
C      1 - add a filter to the CPP command to kill off commented _MASTER lines
C      2 - add a filter to the CPP output the converts the string N EWLINE to an actual newline.
C      The N EWLINE needs to be changes to have no space when this macro and Makefile changes
C      are used. Its in here with a space to stop it getting parsed by the CPP stage in these
C      comments.
C      #define IF ( a .EQ. 1 ) THEN  IF ( a .EQ. 1 ) THEN  N EWLINE      CALL BARRIER_MS(a)
C      #define ENDIF    CALL BARRIER_MU(a) N EWLINE        ENDIF
C      'CPP = cat $< | $(TOOLSDIR)/set64bitConst.sh |  grep -v '^[cC].*_MASTER' | cpp  -traditional -P'
C      .F.f:
C      $(CPP) $(DEFINES) $(INCLUDES) |  sed 's/N EWLINE/\n/' > $@
CcnhDebugEnds

C--   Control storage of floating point operands
C     On many systems it improves performance only to use
C     8-byte precision for time stepped variables.
C     Constant in time terms ( geometric factors etc.. )
C     can use 4-byte precision, reducing memory utilisation and
C     boosting performance because of a smaller working
C     set size. However, on vector CRAY systems this degrades
C     performance.
C- Note: global_sum/max macros were used to switch to  JAM routines (obsolete);
C  in addition, since only the R4 & R8 S/R are coded, GLOBAL RS & RL macros
C  enable to call the corresponding R4 or R8 S/R.






C- Note: a) exch macros were used to switch to  JAM routines (obsolete)
C        b) exch R4 & R8 macros are not practically used ; if needed,
C           will directly call the corrresponding S/R.


C--   Control use of JAM routines for Artic network (no longer supported)
C     These invoke optimized versions of "exchange" and "sum" that
C     utilize the programmable aspect of Artic cards.
CXXX No longer supported ; started to remove JAM routines.
CXXX #ifdef LETS_MAKE_JAM
CXXX #define CALL GLOBAL_SUM_R8 ( a, b) CALL GLOBAL_SUM_R8_JAM ( a, b)
CXXX #define CALL GLOBAL_SUM_R8 ( a, b ) CALL GLOBAL_SUM_R8_JAM ( a, b )
CXXX #define CALL EXCH_XY_RS ( a, b ) CALL EXCH_XY_R8_JAM ( a, b )
CXXX #define CALL EXCH_XY_RL ( a, b ) CALL EXCH_XY_R8_JAM ( a, b )
CXXX #define CALL EXCH_XYZ_RS ( a, b ) CALL EXCH_XYZ_R8_JAM ( a, b )
CXXX #define CALL EXCH_XYZ_RL ( a, b ) CALL EXCH_XYZ_R8_JAM ( a, b )
CXXX #endif

C--   Control use of "double" precision constants.
C     Use d0 where it means REAL*8 but not where it means REAL*16


C--   Substitue for 1.D variables
C     Sun compilers do not use 8-byte precision for literals
C     unless .Dnn is specified. CRAY vector machines use 16-byte
C     precision when they see .Dnn which runs very slowly!


C--   Set the format for writing processor IDs, e.g. in S/R eeset_parms
C     and S/R open_copy_data_file. The default of I9.9 should work for
C     a long time (until we will use 10e10 processors and more)


C--   Set the format for writing ensemble task IDs in S/R eeset_parms
C     and S/R open_copy_data_file.


C--   Set ACTION= in OPEN instruction for input file (before doing IO)
C     leave it empty (if EXCLUDE_OPEN_ACTION) or set it to proper value






C-  Place where multi-pkg header file ECCO_CPPOPTIONS.h used to be included



C--  File seawater.F: routines that compute quantities related to seawater.
C--   Contents
C     Seawater (SW) librabry routines
C--   o SW_PTMP: function to compute potential temperature
C--   o SW_TEMP: function to compute in-situ temperature from pot. temp.
C--   o SW_ADTG: function to compute adiabatic temperature gradient
C--              (used by both SW_PTMP & SW_TEMP)
C     TEOS10 routines (renamed and modified from MOM6 implementation)
C--   o CONVERT_CT2PT: S/R to convert conservative to potential temperature
C--   o CONVERT_PT2CT: S/R to convert potential to conservative temperature
C--                    (used by CONVERT_CT2PT)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SW_PTMP
C     !INTERFACE:
      Real*8 FUNCTION SW_PTMP  (S,T,P,PR)

C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  SW_PTMP
C     | o compute potential temperature as per UNESCO 1983 report.
C     *=============================================================*
C     \ev
C     started:
C              Armin Koehl akoehl@ucsd.edu
C
C     ==================================================================
C     SUBROUTINE SW_PTMP
C     ==================================================================
C     S  :: salinity    [         (PSS-78) ]
C     T  :: temperature [degree C (IPTS-68)]
C     P  :: pressure    [dbar]
C     PR :: Reference pressure  [dbar]
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
      Real*8 S,T,P,PR

C     !FUNCTIONS:
      Real*8 SW_ADTG
      EXTERNAL SW_ADTG

C     !LOCAL VARIABLES
      Real*8 del_P ,del_th, th, q
      Real*8 onehalf, two, three
      PARAMETER ( onehalf = 0.5D0, two = 2.D0, three = 3.D0 )
CEOP

C theta1
      del_P  = PR - P
      del_th = del_P*SW_ADTG(S,T,P)
      th     = T + onehalf*del_th
      q      = del_th
C theta2
      del_th = del_P*SW_ADTG(S,th,P+onehalf*del_P)

      th     = th + (1 - 1/sqrt(two))*(del_th - q)
      q      = (two-sqrt(two))*del_th + (-two+three/sqrt(two))*q

C theta3
      del_th = del_P*SW_ADTG(S,th,P+onehalf*del_P)
      th     = th + (1 + 1/sqrt(two))*(del_th - q)
      q      = (two + sqrt(two))*del_th + (-two-three/sqrt(two))*q

C theta4
      del_th = del_P*SW_ADTG(S,th,P+del_P)
      SW_PTMP     = th + (del_th - two*q)/(two*three)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SW_TEMP
C     !INTERFACE:
      Real*8 FUNCTION SW_TEMP( S, T, P, PR )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  SW_TEMP
C     | o compute in-situ temperature from potential temperature
C     *=============================================================*
C
C     REFERENCES:
C     Fofonoff, P. and Millard, R.C. Jr
C     Unesco 1983. Algorithms for computation of fundamental properties of
C     seawater, 1983. _Unesco Tech. Pap. in Mar. Sci._, No. 44, 53 pp.
C     Eqn.(31) p.39
C
C     Bryden, H. 1973.
C     New Polynomials for thermal expansion, adiabatic temperature gradient
C     and potential temperature of sea water.
C     DEEP-SEA RES., 1973, Vol20,401-408.
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     S      :: salinity
C     T      :: potential temperature
C     P      :: pressure
C     PR     :: reference pressure
      Real*8  S, T, P, PR
CEOP

C     !FUNCTIONS:
      Real*8 SW_PTMP
      EXTERNAL SW_PTMP

      SW_TEMP = SW_PTMP (S,T,PR,P)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SW_ADTG
C     !INTERFACE:
      Real*8 FUNCTION SW_ADTG  (S,T,P)

C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  SW_ADTG
C     | o compute adiabatic temperature gradient as per UNESCO 1983 routines.
C     *=============================================================*
C
C     started:
C              Armin Koehl akoehl@ucsd.edu
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
      Real*8 S,T,P

C     !LOCAL VARIABLES:
      Real*8 a0,a1,a2,a3,b0,b1,c0,c1,c2,c3,d0,d1,e0,e1,e2
      Real*8 sref
CEOP

      sref = 35.D0
      a0 =  3.5803D-5
      a1 = +8.5258D-6
      a2 = -6.836D-8
      a3 =  6.6228D-10

      b0 = +1.8932D-6
      b1 = -4.2393D-8

      c0 = +1.8741D-8
      c1 = -6.7795D-10
      c2 = +8.733D-12
      c3 = -5.4481D-14

      d0 = -1.1351D-10
      d1 =  2.7759D-12

      e0 = -4.6206D-13
      e1 = +1.8676D-14
      e2 = -2.1687D-16

      SW_ADTG =      a0 + (a1 + (a2 + a3*T)*T)*T
     &     + (b0 + b1*T)*(S-sref)
     &     + ( (c0 + (c1 + (c2 + c3*T)*T)*T) + (d0 + d1*T)*(S-sref) )*P
     &     + (  e0 + (e1 + e2*T)*T )*P*P

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: CONVERT_PT2CT

C !INTERFACE:
      SUBROUTINE CONVERT_PT2CT(
     I     Tp, Sa,
     O     Tc,
     I     myTime, myIter, myThid )
C     !DESCRIPTION:
C     Convert input potential temperature (degC) and absolute salinity
C     (g kg-1) to returned conservative temperature (degC) using the
C     polynomial expressions from TEOS-10.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===

CBOP
C    !ROUTINE: SIZE.h
C    !INTERFACE:
C    include SIZE.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | SIZE.h Declare size of underlying computational grid.
C     *==========================================================*
C     | The design here supports a three-dimensional model grid
C     | with indices I,J and K. The three-dimensional domain
C     | is comprised of nPx*nSx blocks (or tiles) of size sNx
C     | along the first (left-most index) axis, nPy*nSy blocks
C     | of size sNy along the second axis and one block of size
C     | Nr along the vertical (third) axis.
C     | Blocks/tiles have overlap regions of size OLx and OLy
C     | along the dimensions that are subdivided.
C     *==========================================================*
C     \ev
C
C     Voodoo numbers controlling data layout:
C     sNx :: Number of X points in tile.
C     sNy :: Number of Y points in tile.
C     OLx :: Tile overlap extent in X.
C     OLy :: Tile overlap extent in Y.
C     nSx :: Number of tiles per process in X.
C     nSy :: Number of tiles per process in Y.
C     nPx :: Number of processes to use in X.
C     nPy :: Number of processes to use in Y.
C     Nx  :: Number of points in X for the full domain.
C     Ny  :: Number of points in Y for the full domain.
C     Nr  :: Number of points in vertical direction.
CEOP
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  64,
     &           sNy =  32,
     &           OLx =   4,
     &           OLy =   4,
     &           nSx =   2,
     &           nSy =   2,
     &           nPx =   1,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  15)

C     MAX_OLX :: Set to the maximum overlap region size of any array
C     MAX_OLY    that will be exchanged. Controls the sizing of exch
C                routine buffers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )


CBOP
C    !ROUTINE: EOS.h
C    !INTERFACE:
C    include EOS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | EOS.h
C     | o Header file defining coefficients for equation of state.
C     *==========================================================*
C     | The values from the model standard input file are
C     | stored into the variables held here.
C     *==========================================================*
C     \ev
CEOP

C     SItoBar  :: conversion factor for pressure, from Pa (SI Unit) to Bar
C     SItodBar :: conversion factor for pressure, from Pa (SI Unit) to deci Bar
      Real*8 SItoBar, SItodBar
      PARAMETER ( SItoBar  = 1.D-05 )
      PARAMETER ( SItodBar = 1.D-04 )

C Shared EOS Parameter
C     eosRefP0  :: reference atmospheric pressure used in EOS formulation
      COMMON /PARM_EOS_SHARED/ eosRefP0, equationOfState
      Real*8 eosRefP0
      CHARACTER*(6) equationOfState

C Linear equation of state
C     tAlpha    :: Linear EOS thermal expansion coefficient ( 1/degree ).
C     sBeta     :: Linear EOS haline contraction coefficient.
      COMMON /PARM_EOS_LIN/ tAlpha, sBeta
      Real*8 tAlpha
      Real*8 sBeta

C Equation of State (polynomial coeffients)
      COMMON /PARM_EOS_POLY3/ eosC,eosSig0,eosRefT,eosRefS
      Real*8 eosC(9,Nr+1),eosSig0(Nr+1),eosRefT(Nr+1),eosRefS(Nr+1)

C Full Equation of State
C     eosType = 'JMD95' (Jackett and McDougall 1995, JAOT)
C     eosType = 'UNESCO' (Millero et al. 1980, DSR)
C     COMMON /PARM_EOS_JMD95/
C     eosJMDCFw  :: of fresh water at pressure 0
C     eosJMDCSw  :: of sea water at pressure 0
C     eosJMDCKFw :: of secant bulk modulus K of fresh water at pressure 0
C     eosJMDCKSw :: of secant bulk modulus K of sea water at pressure 0
C     eosJMDCKP  :: of secant bulk modulus K at pressure p
C     eosType = 'MDJWF' (McDougall et al. 2003, JAOT)
C     COMMON /PARM_EOS_MDJWF/
C     eosMDJWFnum :: coefficients of numerator
C     eosMDJWFden :: coefficients of denominator
C     eosType = 'TEOS10' (McDougall et al. 2011, http://www.teos-10.org)
C     Note: this eos implies that variables THETA and SALT are interpreted
C     as conservative temperature and absolute salinity
C     COMMON /PARM_TEOS10/
C     teos        :: 48 coeffiencts of numerator and denominator
C     end nonlinear equation of state
      Real*8 eosJMDCFw(6), eosJMDCSw(9)
      Real*8 eosJMDCKFw(5), eosJMDCKSw(7), eosJMDCKP(14)
      COMMON /PARM_EOS_JMD95/
     &     eosJMDCFw, eosJMDCSw, eosJMDCKFw, eosJMDCKSw, eosJMDCKP
      Real*8 eosMDJWFnum(0:11), eosMDJWFden(0:12)
      COMMON /PARM_EOS_MDJWF/
     &     eosMDJWFnum, eosMDJWFden

C     TEOS10 coefficients
      Real*8 teos(48)

C     Parameters in the temperature conversion code for TEOS10
C     The TEOS 10 conversion factor to go from reference salinity to
C     practical salinity (nondim)
      Real*8 Sprac_Sref
C     The inverse of a plausible range of oceanic salinities (kg g-1)
      Real*8 I_S0
C     The inverse of a plausible range of oceanic temperatures (degC-1)
      Real*8 I_Ts
C     The inverse of the "specific heat" for use
C     with Conservative Temperature, as defined with TEOS10 (degC kg J-1)
      Real*8 I_cp0

C     The following are coefficients of contributions to conservative
C     temperature as a function of the square root of normalized
C     absolute salinity with an offset (zS) and potential temperature
C     (T) with a contribution Hab * zS**a * T**b.  The numbers here are
C     copied directly from the corresponding gsw module, but the
C     expressions here do not use the same nondimensionalization for
C     pressure or temperature as they do.

C     Tp to Tc fit constant (degC)
      Real*8 H00
C     Tp to Tc fit T coef. (nondim)
      Real*8 H01
C     Tp to Tc fit T**2 coef. (degC-1)
      Real*8 H02
C     Tp to Tc fit T**3 coef. (degC-2)
      Real*8 H03
C     Tp to Tc fit T**4 coef. (degC-3)
      Real*8 H04
C     Tp to Tc fit T**5 coef. (degC-4)
      Real*8 H05
C     Tp to Tc fit T**6 coef. (degC-5)
      Real*8 H06
C     Tp to Tc fit T**7 coef. (degC-6)
      Real*8 H07
C     Tp to Tc fit zS**2 coef. (degC)
      Real*8 H20
C     Tp to Tc fit zS**2 * T coef. (nondim)
      Real*8 H21
C     Tp to Tc fit zS**2 * T**2 coef. (degC-1)
      Real*8 H22
C     Tp to Tc fit zS**2 * T**3 coef. (degC-2)
      Real*8 H23
C     Tp to Tc fit zS**2 * T**4 coef. (degC-3)
      Real*8 H24
C     Tp to Tc fit zS**2 * T**5 coef. (degC-4)
      Real*8 H25
C     Tp to Tc fit zS**2 * T**6 coef. (degC-5)
      Real*8 H26
C     Tp to Tc fit zS**3 coef. (degC)
      Real*8 H30
C     Tp to Tc fit zS** 3* T coef. (nondim)
      Real*8 H31
C     Tp to Tc fit zS**3 * T**2 coef. (degC-1)
      Real*8 H32
C     Tp to Tc fit zS**3 * T**3 coef. (degC-2)
      Real*8 H33
C     Tp to Tc fit zS**3 * T**4 coef. (degC-3)
      Real*8 H34
C     Tp to Tc fit zS**4 coef. (degC)
      Real*8 H40
C     Tp to Tc fit zS**4 * T coef. (nondim)
      Real*8 H41
C     Tp to Tc fit zS**4 * T**2 coef. (degC-1)
      Real*8 H42
C     Tp to Tc fit zS**4 * T**3 coef. (degC-2)
      Real*8 H43
C     Tp to Tc fit zS**4 * T**4 coef. (degC-3)
      Real*8 H44
C     Tp to Tc fit zS**4 * T**5 coef. (degC-4)
      Real*8 H45
C     Tp to Tc fit zS**5 coef. (degC)
      Real*8 H50
C     Tp to Tc fit zS**6 coef. (degC)
      Real*8 H60
C     Tp to Tc fit zS**7 coef. (degC)
      Real*8 H70

C     The following are coefficients in the nominator (TPNxx) or
C     denominator (TPDxx) of a simple rational expression that
C     approximately converts conservative temperature to potential
C     temperature.
C     Simple fit numerator constant (degC)
      Real*8 TPN00
C     Simple fit numerator Sa coef. (degC ppt-1)
      Real*8 TPN10
C     Simple fit numerator Sa**2 coef. (degC ppt-2)
      Real*8 TPN20
C     Simple fit numerator Tc coef. (nondim)
      Real*8 TPN01
C     Simple fit numerator Sa * Tc coef. (ppt-1)
      Real*8 TPN11
C     Simple fit numerator Tc**2 coef. (degC-1)
      Real*8 TPN02
C     Simple fit denominator Sa coef. (ppt-1)
      Real*8 TPD10
C     Simple fit denominator Tc coef. (degC-1)
      Real*8 TPD01
C     Simple fit denominator Tc**2 coef. (degC-2)
      Real*8 TPD02

      COMMON /PARM_TEOS10/
     &     teos,
     &     Sprac_Sref, I_S0, I_Ts, I_cp0,
     &     H00, H01, H02, H03, H04, H05, H06, H07,
     &     H20, H21, H22, H23, H24, H25, H26,
     &     H30, H31, H32, H33, H34,
     &     H40, H41, H42, H43, H44, H45,
     &     H50, H60, H70,
     &     TPN00, TPN10, TPN20,
     &     TPN01, TPN11, TPN02, TPD10, TPD01, TPD02


C     !INPUT/OUTPUT PARAMETERS:
C     Tc        :: Conservative temperature (degC)
C     Sa        :: Absolute salinity        (g kg-1)
C     T         :: potential temperature (degC)
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      Real*8 Tc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      Real*8 Sa(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      Real*8 Tp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      Real*8 myTime
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j
C     Absolute salinity normalized by a plausible salinity range and its
C     square root (nondim)
      Real*8 x2, x
C
      Real*8     zeRL
      PARAMETER ( zeRL = 0.0D0 )

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        x2 = MAX(I_S0 * Sa(i,j), zeRL)

        x = SQRT(x2)

        Tc(i,j) = H00 + (Tp(i,j)*(H01 + Tp(i,j)*(H02 +  Tp(i,j)*(H03
     &       +  Tp(i,j)*(H04  + Tp(i,j)*(H05
     &       + Tp(i,j)*(H06 + Tp(i,j)* H07))))))
     &       + x2*(H20 + (Tp(i,j)*(H21 +  Tp(i,j)*(H22  + Tp(i,j)*(H23
     &       + Tp(i,j)*(H24 + Tp(i,j)*(H25 + Tp(i,j)*H26)))))
     &       +  x*(H30 + (Tp(i,j)*(H31  + Tp(i,j)*(H32
     &       + Tp(i,j)*(H33 + Tp(i,j)* H34)))
     &       + x*(H40 + (Tp(i,j)*(H41 + Tp(i,j)*(H42 + Tp(i,j)*(H43
     &       + Tp(i,j)*(H44 + Tp(i,j)*H45))))
     &       + x*(H50 + x*(H60 + x* H70)) )) )) )) )
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: CONVERT_CT2PT

C !INTERFACE:
      SUBROUTINE CONVERT_CT2PT(
     I         Tc, Sa,
     O         Tp,
     I         myTime, myIter, myThid )

C     !DESCRIPTION:
C     Convert input conservative (degC) and absolute salinity
C     (g kg-1) to returned potential temperature (degC) by inverting
C     the polynomial expressions from TEOS-10.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===

CBOP
C    !ROUTINE: SIZE.h
C    !INTERFACE:
C    include SIZE.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | SIZE.h Declare size of underlying computational grid.
C     *==========================================================*
C     | The design here supports a three-dimensional model grid
C     | with indices I,J and K. The three-dimensional domain
C     | is comprised of nPx*nSx blocks (or tiles) of size sNx
C     | along the first (left-most index) axis, nPy*nSy blocks
C     | of size sNy along the second axis and one block of size
C     | Nr along the vertical (third) axis.
C     | Blocks/tiles have overlap regions of size OLx and OLy
C     | along the dimensions that are subdivided.
C     *==========================================================*
C     \ev
C
C     Voodoo numbers controlling data layout:
C     sNx :: Number of X points in tile.
C     sNy :: Number of Y points in tile.
C     OLx :: Tile overlap extent in X.
C     OLy :: Tile overlap extent in Y.
C     nSx :: Number of tiles per process in X.
C     nSy :: Number of tiles per process in Y.
C     nPx :: Number of processes to use in X.
C     nPy :: Number of processes to use in Y.
C     Nx  :: Number of points in X for the full domain.
C     Ny  :: Number of points in Y for the full domain.
C     Nr  :: Number of points in vertical direction.
CEOP
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  64,
     &           sNy =  32,
     &           OLx =   4,
     &           OLy =   4,
     &           nSx =   2,
     &           nSy =   2,
     &           nPx =   1,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  15)

C     MAX_OLX :: Set to the maximum overlap region size of any array
C     MAX_OLY    that will be exchanged. Controls the sizing of exch
C                routine buffers.
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )


CBOP
C    !ROUTINE: EOS.h
C    !INTERFACE:
C    include EOS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | EOS.h
C     | o Header file defining coefficients for equation of state.
C     *==========================================================*
C     | The values from the model standard input file are
C     | stored into the variables held here.
C     *==========================================================*
C     \ev
CEOP

C     SItoBar  :: conversion factor for pressure, from Pa (SI Unit) to Bar
C     SItodBar :: conversion factor for pressure, from Pa (SI Unit) to deci Bar
      Real*8 SItoBar, SItodBar
      PARAMETER ( SItoBar  = 1.D-05 )
      PARAMETER ( SItodBar = 1.D-04 )

C Shared EOS Parameter
C     eosRefP0  :: reference atmospheric pressure used in EOS formulation
      COMMON /PARM_EOS_SHARED/ eosRefP0, equationOfState
      Real*8 eosRefP0
      CHARACTER*(6) equationOfState

C Linear equation of state
C     tAlpha    :: Linear EOS thermal expansion coefficient ( 1/degree ).
C     sBeta     :: Linear EOS haline contraction coefficient.
      COMMON /PARM_EOS_LIN/ tAlpha, sBeta
      Real*8 tAlpha
      Real*8 sBeta

C Equation of State (polynomial coeffients)
      COMMON /PARM_EOS_POLY3/ eosC,eosSig0,eosRefT,eosRefS
      Real*8 eosC(9,Nr+1),eosSig0(Nr+1),eosRefT(Nr+1),eosRefS(Nr+1)

C Full Equation of State
C     eosType = 'JMD95' (Jackett and McDougall 1995, JAOT)
C     eosType = 'UNESCO' (Millero et al. 1980, DSR)
C     COMMON /PARM_EOS_JMD95/
C     eosJMDCFw  :: of fresh water at pressure 0
C     eosJMDCSw  :: of sea water at pressure 0
C     eosJMDCKFw :: of secant bulk modulus K of fresh water at pressure 0
C     eosJMDCKSw :: of secant bulk modulus K of sea water at pressure 0
C     eosJMDCKP  :: of secant bulk modulus K at pressure p
C     eosType = 'MDJWF' (McDougall et al. 2003, JAOT)
C     COMMON /PARM_EOS_MDJWF/
C     eosMDJWFnum :: coefficients of numerator
C     eosMDJWFden :: coefficients of denominator
C     eosType = 'TEOS10' (McDougall et al. 2011, http://www.teos-10.org)
C     Note: this eos implies that variables THETA and SALT are interpreted
C     as conservative temperature and absolute salinity
C     COMMON /PARM_TEOS10/
C     teos        :: 48 coeffiencts of numerator and denominator
C     end nonlinear equation of state
      Real*8 eosJMDCFw(6), eosJMDCSw(9)
      Real*8 eosJMDCKFw(5), eosJMDCKSw(7), eosJMDCKP(14)
      COMMON /PARM_EOS_JMD95/
     &     eosJMDCFw, eosJMDCSw, eosJMDCKFw, eosJMDCKSw, eosJMDCKP
      Real*8 eosMDJWFnum(0:11), eosMDJWFden(0:12)
      COMMON /PARM_EOS_MDJWF/
     &     eosMDJWFnum, eosMDJWFden

C     TEOS10 coefficients
      Real*8 teos(48)

C     Parameters in the temperature conversion code for TEOS10
C     The TEOS 10 conversion factor to go from reference salinity to
C     practical salinity (nondim)
      Real*8 Sprac_Sref
C     The inverse of a plausible range of oceanic salinities (kg g-1)
      Real*8 I_S0
C     The inverse of a plausible range of oceanic temperatures (degC-1)
      Real*8 I_Ts
C     The inverse of the "specific heat" for use
C     with Conservative Temperature, as defined with TEOS10 (degC kg J-1)
      Real*8 I_cp0

C     The following are coefficients of contributions to conservative
C     temperature as a function of the square root of normalized
C     absolute salinity with an offset (zS) and potential temperature
C     (T) with a contribution Hab * zS**a * T**b.  The numbers here are
C     copied directly from the corresponding gsw module, but the
C     expressions here do not use the same nondimensionalization for
C     pressure or temperature as they do.

C     Tp to Tc fit constant (degC)
      Real*8 H00
C     Tp to Tc fit T coef. (nondim)
      Real*8 H01
C     Tp to Tc fit T**2 coef. (degC-1)
      Real*8 H02
C     Tp to Tc fit T**3 coef. (degC-2)
      Real*8 H03
C     Tp to Tc fit T**4 coef. (degC-3)
      Real*8 H04
C     Tp to Tc fit T**5 coef. (degC-4)
      Real*8 H05
C     Tp to Tc fit T**6 coef. (degC-5)
      Real*8 H06
C     Tp to Tc fit T**7 coef. (degC-6)
      Real*8 H07
C     Tp to Tc fit zS**2 coef. (degC)
      Real*8 H20
C     Tp to Tc fit zS**2 * T coef. (nondim)
      Real*8 H21
C     Tp to Tc fit zS**2 * T**2 coef. (degC-1)
      Real*8 H22
C     Tp to Tc fit zS**2 * T**3 coef. (degC-2)
      Real*8 H23
C     Tp to Tc fit zS**2 * T**4 coef. (degC-3)
      Real*8 H24
C     Tp to Tc fit zS**2 * T**5 coef. (degC-4)
      Real*8 H25
C     Tp to Tc fit zS**2 * T**6 coef. (degC-5)
      Real*8 H26
C     Tp to Tc fit zS**3 coef. (degC)
      Real*8 H30
C     Tp to Tc fit zS** 3* T coef. (nondim)
      Real*8 H31
C     Tp to Tc fit zS**3 * T**2 coef. (degC-1)
      Real*8 H32
C     Tp to Tc fit zS**3 * T**3 coef. (degC-2)
      Real*8 H33
C     Tp to Tc fit zS**3 * T**4 coef. (degC-3)
      Real*8 H34
C     Tp to Tc fit zS**4 coef. (degC)
      Real*8 H40
C     Tp to Tc fit zS**4 * T coef. (nondim)
      Real*8 H41
C     Tp to Tc fit zS**4 * T**2 coef. (degC-1)
      Real*8 H42
C     Tp to Tc fit zS**4 * T**3 coef. (degC-2)
      Real*8 H43
C     Tp to Tc fit zS**4 * T**4 coef. (degC-3)
      Real*8 H44
C     Tp to Tc fit zS**4 * T**5 coef. (degC-4)
      Real*8 H45
C     Tp to Tc fit zS**5 coef. (degC)
      Real*8 H50
C     Tp to Tc fit zS**6 coef. (degC)
      Real*8 H60
C     Tp to Tc fit zS**7 coef. (degC)
      Real*8 H70

C     The following are coefficients in the nominator (TPNxx) or
C     denominator (TPDxx) of a simple rational expression that
C     approximately converts conservative temperature to potential
C     temperature.
C     Simple fit numerator constant (degC)
      Real*8 TPN00
C     Simple fit numerator Sa coef. (degC ppt-1)
      Real*8 TPN10
C     Simple fit numerator Sa**2 coef. (degC ppt-2)
      Real*8 TPN20
C     Simple fit numerator Tc coef. (nondim)
      Real*8 TPN01
C     Simple fit numerator Sa * Tc coef. (ppt-1)
      Real*8 TPN11
C     Simple fit numerator Tc**2 coef. (degC-1)
      Real*8 TPN02
C     Simple fit denominator Sa coef. (ppt-1)
      Real*8 TPD10
C     Simple fit denominator Tc coef. (degC-1)
      Real*8 TPD01
C     Simple fit denominator Tc**2 coef. (degC-2)
      Real*8 TPD02

      COMMON /PARM_TEOS10/
     &     teos,
     &     Sprac_Sref, I_S0, I_Ts, I_cp0,
     &     H00, H01, H02, H03, H04, H05, H06, H07,
     &     H20, H21, H22, H23, H24, H25, H26,
     &     H30, H31, H32, H33, H34,
     &     H40, H41, H42, H43, H44, H45,
     &     H50, H60, H70,
     &     TPN00, TPN10, TPN20,
     &     TPN01, TPN11, TPN02, TPD10, TPD01, TPD02


C     !INPUT/OUTPUT PARAMETERS:
C     Tc        :: Conservative temperature (degC)
C     Sa        :: Absolute salinity        (g kg-1)
C     Tp        :: potential temperature (degC)
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      Real*8 Tc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      Real*8 Sa(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      Real*8 Tp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      Real*8 myTime
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j
C     The numerator of a simple expression for potential temperature (degC)
      Real*8 Tp_num
C     The inverse of the denominator of a simple expression for
C     potential temperature (nondim)
      Real*8 I_Tp_den
C     The difference between an estimate of conservative temperature and
C     its target (degC)
      Real*8 Tc_diff(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     A previous estimate of the potential tempearture (degC)
      Real*8 Tp_old(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     The partial derivative of potential temperature with conservative
C     temperature and inverse (nondim)
      Real*8 dTp_dTc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      Real*8 dTc_dTp
C     Absolute salinity normalized by a plausible salinity range and its
C     square root (nondim)
      Real*8 x2, x
C     intermediate temperature (degC)
      Real*8 Tmp
C
      Real*8     zeRL
      PARAMETER ( zeRL = 0.0D0 )

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        Tp_num = TPN00 + (Sa(i,j)*(TPN10 + TPN20*Sa(i,j))
     &       + Tc(i,j)*(TPN01 + (TPN11*Sa(i,j) + TPN02*Tc(i,j))))
        I_Tp_den = 1.0D0 / (1.0D0
     &       + (TPD10*Sa(i,j) + Tc(i,j)*(TPD01 + TPD02*Tc(i,j))))
        Tp(i,j) = Tp_num*I_Tp_den
        dTp_dTc(i,j) = ((TPN01 + (TPN11*Sa(i,j) + 2.*TPN02*Tc(i,j)))
     &       - (TPD01 + 2.*TPD02*Tc(i,j))*Tp(i,j))*I_Tp_den
       ENDDO
      ENDDO

C--   Start the 1.5 iterations through the modified Newton-Raphson
C     iterative method, which is also known as the Newton-McDougall
C     method.  In this case 1.5 iterations converge to 64-bit machine
C     precision for oceanographically relevant temperatures and
C     salinities.
      CALL CONVERT_PT2CT (
     I     Tp, Sa,
     O     Tc_diff,
     I     myTime, myIter, myThid )
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        Tc_diff(i,j) = Tc_diff(i,j) - Tc(i,j)
        Tp_old(i,j) = Tp(i,j)

        Tp(i,j) = Tp_old(i,j) - Tc_diff(i,j)*dTp_dTc(i,j)
C--   Estimate the potential temperature and its derivative from an C
C     approximate rational function fit.
        x2 = MAX(I_S0 * Sa(i,j), zeRL)

        x = SQRT(x2)

        Tmp = 0.5D0 *(Tp(i,j) + Tp_old(i,j))
        dTc_dTp = (     H01 + Tmp*(2.*H02 + Tmp*(3.*H03 + Tmp*(4.*H04
     &       + Tmp*(5.*H05 + Tmp*(6.*H06 + Tmp*(7.*H07)))))) )
     &       + x2*(     (H21 + Tmp*(2.*H22 + Tmp*(3.*H23 + Tmp*(4.*H24
     &       + Tmp*(5.*H25 + Tmp*(6.*H26))))))
     &       +  x*(  (H31 + Tmp*(2.*H32 + Tmp*(3.*H33 + Tmp*(4.*H34))))
     &       + x*(H41 + Tmp*(2.*H42 + Tmp*(3.*H43
     &       + Tmp*(4.*H44 + Tmp*(5.*H45))))) ) )
        dTp_dTc(i,j) = 1.D0 / dTc_dTp

        Tp(i,j) = Tp_old(i,j) - Tc_diff(i,j)*dTp_dTc(i,j)
       ENDDO
      ENDDO
      CALL CONVERT_PT2CT (
     I     Tp, Sa,
     O     Tc_diff,
     I     myTime, myIter, myThid )
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        Tc_diff(i,j) = Tc_diff(i,j) - Tc(i,j)
        Tp(i,j) = Tp(i,j) - Tc_diff(i,j)*dTp_dTc(i,j)
       ENDDO
      ENDDO

      RETURN
      END

