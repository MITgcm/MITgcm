

  








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



C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: INI_PARMS
C !INTERFACE:
      SUBROUTINE INI_PARMS( myThid )

C !DESCRIPTION:
C     Routine to load model "parameters" from parameter file "data"

C     !USES:
      IMPLICIT NONE

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
C     !ROUTINE: EEPARAMS.h
C     !INTERFACE:
C     include "EEPARAMS.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | EEPARAMS.h                                               |
C     *==========================================================*
C     | Parameters for "execution environemnt". These are used   |
C     | by both the particular numerical model and the execution |
C     | environment support routines.                            |
C     *==========================================================*
CEOP

C     ========  EESIZE.h  ========================================

C     MAX_LEN_MBUF  :: Default message buffer max. size
C     MAX_LEN_FNAM  :: Default file name max. size
C     MAX_LEN_PREC  :: Default rec len for reading "parameter" files

      INTEGER MAX_LEN_MBUF
      PARAMETER ( MAX_LEN_MBUF = 512 )
      INTEGER MAX_LEN_FNAM
      PARAMETER ( MAX_LEN_FNAM = 512 )
      INTEGER MAX_LEN_PREC
      PARAMETER ( MAX_LEN_PREC = 200 )

C     MAX_NO_THREADS  :: Maximum number of threads allowed.
C     GSVec_size      :: Maximum buffer size for Global Sum Vector array
      INTEGER MAX_NO_THREADS
      PARAMETER ( MAX_NO_THREADS =  4 )
      INTEGER GSVec_size
      PARAMETER ( GSVec_size = 1024 )

C     Particularly weird and obscure voodoo numbers
C     lShare :: This wants to be the length in
C               [148]-byte words of the size of
C               the address "window" that is snooped
C               on an SMP bus. By separating elements in
C               the global sum buffer we can avoid generating
C               extraneous invalidate traffic between
C               processors. The length of this window is usually
C               a cache line i.e. small O(64 bytes).
C               The buffer arrays are usually short arrays
C               and are declared REAL ARRA(lShare[148],LBUFF).
C               Setting lShare[148] to 1 is like making these arrays
C               one dimensional.
      INTEGER cacheLineSize
      INTEGER lShare1
      INTEGER lShare4
      INTEGER lShare8
      PARAMETER ( cacheLineSize = 256 )
      PARAMETER ( lShare1 =  cacheLineSize )
      PARAMETER ( lShare4 =  cacheLineSize/4 )
      PARAMETER ( lShare8 =  cacheLineSize/8 )

C     ========  EESIZE.h  ========================================

C     Symbolic values
C     precXXXX :: precision used for I/O
      INTEGER precFloat32
      PARAMETER ( precFloat32 = 32 )
      INTEGER precFloat64
      PARAMETER ( precFloat64 = 64 )

C     Real-type constant for some frequently used simple number (0,1,2,1/2):
      Real*8     zeroRS, oneRS, twoRS, halfRS
      PARAMETER ( zeroRS = 0.0D0 , oneRS  = 1.0D0 )
      PARAMETER ( twoRS  = 2.0D0 , halfRS = 0.5D0 )
      Real*8     zeroRL, oneRL, twoRL, halfRL
      PARAMETER ( zeroRL = 0.0D0 , oneRL  = 1.0D0 )
      PARAMETER ( twoRL  = 2.0D0 , halfRL = 0.5D0 )

C     UNSET_xxx :: Used to indicate variables that have not been given a value
      Real*8  UNSET_FLOAT8
      PARAMETER ( UNSET_FLOAT8 = 1.234567D5 )
      Real*4  UNSET_FLOAT4
      PARAMETER ( UNSET_FLOAT4 = 1.234567E5 )
      Real*8     UNSET_RL
      PARAMETER ( UNSET_RL     = 1.234567D5 )
      Real*8     UNSET_RS
      PARAMETER ( UNSET_RS     = 1.234567D5 )
      INTEGER UNSET_I
      PARAMETER ( UNSET_I      = 123456789  )

C     debLevX  :: used to decide when to print debug messages
      INTEGER debLevZero
      INTEGER debLevA, debLevB,  debLevC, debLevD, debLevE
      PARAMETER ( debLevZero=0 )
      PARAMETER ( debLevA=1 )
      PARAMETER ( debLevB=2 )
      PARAMETER ( debLevC=3 )
      PARAMETER ( debLevD=4 )
      PARAMETER ( debLevE=5 )

C     SQUEEZE_RIGHT      :: Flag indicating right blank space removal
C                           from text field.
C     SQUEEZE_LEFT       :: Flag indicating left blank space removal
C                           from text field.
C     SQUEEZE_BOTH       :: Flag indicating left and right blank
C                           space removal from text field.
C     PRINT_MAP_XY       :: Flag indicating to plot map as XY slices
C     PRINT_MAP_XZ       :: Flag indicating to plot map as XZ slices
C     PRINT_MAP_YZ       :: Flag indicating to plot map as YZ slices
C     commentCharacter   :: Variable used in column 1 of parameter
C                           files to indicate comments.
C     INDEX_I            :: Variable used to select an index label
C     INDEX_J               for formatted input parameters.
C     INDEX_K
C     INDEX_NONE
      CHARACTER*(*) SQUEEZE_RIGHT
      PARAMETER ( SQUEEZE_RIGHT = 'R' )
      CHARACTER*(*) SQUEEZE_LEFT
      PARAMETER ( SQUEEZE_LEFT = 'L' )
      CHARACTER*(*) SQUEEZE_BOTH
      PARAMETER ( SQUEEZE_BOTH = 'B' )
      CHARACTER*(*) PRINT_MAP_XY
      PARAMETER ( PRINT_MAP_XY = 'XY' )
      CHARACTER*(*) PRINT_MAP_XZ
      PARAMETER ( PRINT_MAP_XZ = 'XZ' )
      CHARACTER*(*) PRINT_MAP_YZ
      PARAMETER ( PRINT_MAP_YZ = 'YZ' )
      CHARACTER*(*) commentCharacter
      PARAMETER ( commentCharacter = '#' )
      INTEGER INDEX_I
      INTEGER INDEX_J
      INTEGER INDEX_K
      INTEGER INDEX_NONE
      PARAMETER ( INDEX_I    = 1,
     &            INDEX_J    = 2,
     &            INDEX_K    = 3,
     &            INDEX_NONE = 4 )

C     EXCH_IGNORE_CORNERS :: Flag to select ignoring or
C     EXCH_UPDATE_CORNERS    updating of corners during an edge exchange.
      INTEGER EXCH_IGNORE_CORNERS
      INTEGER EXCH_UPDATE_CORNERS
      PARAMETER ( EXCH_IGNORE_CORNERS = 0,
     &            EXCH_UPDATE_CORNERS = 1 )

C     FORWARD_SIMULATION
C     REVERSE_SIMULATION
C     TANGENT_SIMULATION
      INTEGER FORWARD_SIMULATION
      INTEGER REVERSE_SIMULATION
      INTEGER TANGENT_SIMULATION
      PARAMETER ( FORWARD_SIMULATION = 0,
     &            REVERSE_SIMULATION = 1,
     &            TANGENT_SIMULATION = 2 )

C--   COMMON /EEPARAMS_L/ Execution environment public logical variables.
C     eeBootError    :: Flags indicating error during multi-processing
C     eeEndError     :: initialisation and termination.
C     fatalError     :: Flag used to indicate that the model is ended with an error
C     debugMode      :: controls printing of debug msg (sequence of S/R calls).
C     useSingleCpuIO :: When useSingleCpuIO is set, MDS_WRITE_FIELD outputs from
C                       master MPI process only. -- NOTE: read from main parameter
C                       file "data" and not set until call to INI_PARMS.
C     useSingleCpuInput :: When useSingleCpuInput is set, EXF_INTERP_READ
C                       reads forcing files from master MPI process only.
C                       -- NOTE: read from main parameter file "data"
C                          and defaults to useSingleCpuInput = useSingleCpuIO
C     printMapIncludesZeros  :: Flag that controls whether character constant
C                               map code ignores exact zero values.
C     useCubedSphereExchange :: use Cubed-Sphere topology domain.
C     useCoupler     :: use Coupler for a multi-components set-up.
C     useNEST_PARENT :: use Parent Nesting interface (pkg/nest_parent)
C     useNEST_CHILD  :: use Child  Nesting interface (pkg/nest_child)
C     useNest2W_parent :: use Parent 2-W Nesting interface (pkg/nest2w_parent)
C     useNest2W_child  :: use Child  2-W Nesting interface (pkg/nest2w_child)
C     useOASIS       :: use OASIS-coupler for a multi-components set-up.
      COMMON /EEPARAMS_L/
c    &  eeBootError, fatalError, eeEndError,
     &  eeBootError, eeEndError, fatalError, debugMode,
     &  useSingleCpuIO, useSingleCpuInput, printMapIncludesZeros,
     &  useCubedSphereExchange, useCoupler,
     &  useNEST_PARENT, useNEST_CHILD,
     &  useNest2W_parent, useNest2W_child, useOASIS,
     &  useSETRLSTK, useSIGREG
      LOGICAL eeBootError
      LOGICAL eeEndError
      LOGICAL fatalError
      LOGICAL debugMode
      LOGICAL useSingleCpuIO
      LOGICAL useSingleCpuInput
      LOGICAL printMapIncludesZeros
      LOGICAL useCubedSphereExchange
      LOGICAL useCoupler
      LOGICAL useNEST_PARENT
      LOGICAL useNEST_CHILD
      LOGICAL useNest2W_parent
      LOGICAL useNest2W_child
      LOGICAL useOASIS
      LOGICAL useSETRLSTK
      LOGICAL useSIGREG

C--   COMMON /EPARAMS_I/ Execution environment public integer variables.
C     errorMessageUnit    :: Fortran IO unit for error messages
C     standardMessageUnit :: Fortran IO unit for informational messages
C     maxLengthPrt1D :: maximum length for printing (to Std-Msg-Unit) 1-D array
C     scrUnit1      :: Scratch file 1 unit number
C     scrUnit2      :: Scratch file 2 unit number
C     eeDataUnit    :: Unit # for reading "execution environment" parameter file
C     modelDataUnit :: Unit number for reading "model" parameter file.
C     numberOfProcs :: Number of processes computing in parallel
C     pidIO         :: Id of process to use for I/O.
C     myBxLo, myBxHi :: Extents of domain in blocks in X and Y
C     myByLo, myByHi :: that each threads is responsble for.
C     myProcId      :: My own "process" id.
C     myPx          :: My X coord on the proc. grid.
C     myPy          :: My Y coord on the proc. grid.
C     myXGlobalLo   :: My bottom-left (south-west) x-index global domain.
C                      The x-coordinate of this point in for example m or
C                      degrees is *not* specified here. A model needs to
C                      provide a mechanism for deducing that information
C                      if it is needed.
C     myYGlobalLo   :: My bottom-left (south-west) y-index in global domain.
C                      The y-coordinate of this point in for example m or
C                      degrees is *not* specified here. A model needs to
C                      provide a mechanism for deducing that information
C                      if it is needed.
C     nThreads      :: No. of threads
C     nTx, nTy      :: No. of threads in X and in Y
C                      This assumes a simple cartesian gridding of the threads
C                      which is not required elsewhere but that makes it easier
C     ioErrorCount  :: IO Error Counter. Set to zero initially and increased
C                      by one every time an IO error occurs.
      COMMON /EEPARAMS_I/
     &  errorMessageUnit, standardMessageUnit, maxLengthPrt1D,
     &  scrUnit1, scrUnit2, eeDataUnit, modelDataUnit,
     &  numberOfProcs, pidIO, myProcId,
     &  myPx, myPy, myXGlobalLo, myYGlobalLo, nThreads,
     &  myBxLo, myBxHi, myByLo, myByHi,
     &  nTx, nTy, ioErrorCount
      INTEGER errorMessageUnit
      INTEGER standardMessageUnit
      INTEGER maxLengthPrt1D
      INTEGER scrUnit1
      INTEGER scrUnit2
      INTEGER eeDataUnit
      INTEGER modelDataUnit
      INTEGER ioErrorCount(MAX_NO_THREADS)
      INTEGER myBxLo(MAX_NO_THREADS)
      INTEGER myBxHi(MAX_NO_THREADS)
      INTEGER myByLo(MAX_NO_THREADS)
      INTEGER myByHi(MAX_NO_THREADS)
      INTEGER myProcId
      INTEGER myPx
      INTEGER myPy
      INTEGER myXGlobalLo
      INTEGER myYGlobalLo
      INTEGER nThreads
      INTEGER nTx
      INTEGER nTy
      INTEGER numberOfProcs
      INTEGER pidIO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: PARAMS.h
C     !INTERFACE:
C     #include PARAMS.h

C     !DESCRIPTION:
C     Header file defining model "parameters".  The values from the
C     model standard input file are stored into the variables held
C     here. Notes describing the parameters can also be found here.

CEOP

C--   Contants
C     Useful physical values
      Real*8 PI
      PARAMETER ( PI    = 3.14159265358979323844D0   )
      Real*8 deg2rad
      PARAMETER ( deg2rad = 2.D0*PI/360.D0           )

C--   COMMON /PARM_C/ Character valued parameters used by the model.
C     buoyancyRelation :: Flag used to indicate which relation to use to
C                         get buoyancy.
C     eosType         :: choose the equation of state:
C                        LINEAR, POLY3, UNESCO, JMD95Z, JMD95P, MDJWF, IDEALGAS
C     pickupSuff      :: force to start from pickup files (even if nIter0=0)
C                        and read pickup files with this suffix (max 10 Char.)
C     mdsioLocalDir   :: read-write tiled file from/to this directory name
C                        (+ 4 digits Processor-Rank) instead of current dir.
C     adTapeDir       :: read-write checkpointing tape files from/to this
C                        directory name instead of current dir. Conflicts
C                        mdsioLocalDir, so only one of the two can be set.
C     tRefFile      :: File containing reference Potential Temperat.  tRef (1.D)
C     sRefFile      :: File containing reference salinity/spec.humid. sRef (1.D)
C     rhoRefFile    :: File containing reference density profile rhoRef (1.D)
C     gravityFile   :: File containing gravity vertical profile (1.D)
C     delRFile      :: File containing vertical grid spacing delR  (1.D array)
C     delRcFile     :: File containing vertical grid spacing delRc (1.D array)
C     hybSigmFile   :: File containing hybrid-sigma vertical coord. coeff. (2x 1.D)
C     delXFile      :: File containing X-spacing grid definition (1.D array)
C     delYFile      :: File containing Y-spacing grid definition (1.D array)
C     horizGridFile :: File containing horizontal-grid definition
C                        (only when using curvilinear_grid)
C     bathyFile       :: File containing bathymetry. If not defined bathymetry
C                        is taken from inline function.
C     topoFile        :: File containing the topography of the surface (unit=m)
C                        (mainly used for the atmosphere = ground height).
C     addWwallFile    :: File containing 2-D additional Western  cell-edge wall
C     addSwallFile    :: File containing 2-D additional Southern cell-edge wall
C                        (e.g., to add "thin-wall" where it is =1)
C     hydrogThetaFile :: File containing initial hydrographic data (3-D)
C                        for potential temperature.
C     hydrogSaltFile  :: File containing initial hydrographic data (3-D)
C                        for salinity.
C     diffKrFile      :: File containing 3D specification of vertical diffusivity
C     viscAhDfile     :: File containing 3D specification of horizontal viscosity
C     viscAhZfile     :: File containing 3D specification of horizontal viscosity
C     viscA4Dfile     :: File containing 3D specification of horizontal viscosity
C     viscA4Zfile     :: File containing 3D specification of horizontal viscosity
C     zonalWindFile   :: File containing zonal wind data
C     meridWindFile   :: File containing meridional wind data
C     thetaClimFile   :: File containing surface theta climataology used
C                        in relaxation term -lambda(theta-theta*)
C     saltClimFile    :: File containing surface salt climataology used
C                        in relaxation term -lambda(salt-salt*)
C     surfQfile       :: File containing surface heat flux, excluding SW
C                        (old version, kept for backward compatibility)
C     surfQnetFile    :: File containing surface net heat flux
C     surfQswFile     :: File containing surface shortwave radiation
C     EmPmRfile       :: File containing surface fresh water flux
C           NOTE: for backward compatibility EmPmRfile is specified in
C                 m/s when using external_fields_load.F.  It is converted
C                 to kg/m2/s by multiplying by rhoConstFresh.
C     saltFluxFile    :: File containing surface salt flux
C     pLoadFile       :: File containing pressure loading
C     geoPotAnomFile  :: File containing constant geopotential anomaly due to
C                        density structure
C     addMassFile     :: File containing source/sink of fluid in the interior
C     eddyPsiXFile    :: File containing zonal Eddy streamfunction data
C     eddyPsiYFile    :: File containing meridional Eddy streamfunction data
C     geothermalFile  :: File containing geothermal heat flux
C     lambdaThetaFile :: File containing SST relaxation coefficient
C     lambdaSaltFile  :: File containing SSS relaxation coefficient
C     wghtBalanceFile :: File containing weight used in balancing net EmPmR
C     the_run_name    :: string identifying the name of the model "run"
      COMMON /PARM_C/
     &                buoyancyRelation, eosType,
     &                pickupSuff, mdsioLocalDir, adTapeDir,
     &                tRefFile, sRefFile, rhoRefFile, gravityFile,
     &                delRFile, delRcFile, hybSigmFile,
     &                delXFile, delYFile, horizGridFile,
     &                bathyFile, topoFile, addWwallFile, addSwallFile,
     &                viscAhDfile, viscAhZfile,
     &                viscA4Dfile, viscA4Zfile,
     &                hydrogThetaFile, hydrogSaltFile, diffKrFile,
     &                zonalWindFile, meridWindFile, thetaClimFile,
     &                saltClimFile,
     &                EmPmRfile, saltFluxFile,
     &                surfQfile, surfQnetFile, surfQswFile,
     &                uVelInitFile, vVelInitFile, pSurfInitFile,
     &                pLoadFile, geoPotAnomFile, addMassFile,
     &                eddyPsiXFile, eddyPsiYFile, geothermalFile,
     &                lambdaThetaFile, lambdaSaltFile, wghtBalanceFile,
     &                the_run_name
      CHARACTER*(MAX_LEN_FNAM) buoyancyRelation
      CHARACTER*(6)  eosType
      CHARACTER*(10) pickupSuff
      CHARACTER*(MAX_LEN_FNAM) mdsioLocalDir
      CHARACTER*(MAX_LEN_FNAM) adTapeDir
      CHARACTER*(MAX_LEN_FNAM) tRefFile
      CHARACTER*(MAX_LEN_FNAM) sRefFile
      CHARACTER*(MAX_LEN_FNAM) rhoRefFile
      CHARACTER*(MAX_LEN_FNAM) gravityFile
      CHARACTER*(MAX_LEN_FNAM) delRFile
      CHARACTER*(MAX_LEN_FNAM) delRcFile
      CHARACTER*(MAX_LEN_FNAM) hybSigmFile
      CHARACTER*(MAX_LEN_FNAM) delXFile
      CHARACTER*(MAX_LEN_FNAM) delYFile
      CHARACTER*(MAX_LEN_FNAM) horizGridFile
      CHARACTER*(MAX_LEN_FNAM) bathyFile, topoFile
      CHARACTER*(MAX_LEN_FNAM) addWwallFile, addSwallFile
      CHARACTER*(MAX_LEN_FNAM) hydrogThetaFile, hydrogSaltFile
      CHARACTER*(MAX_LEN_FNAM) diffKrFile
      CHARACTER*(MAX_LEN_FNAM) viscAhDfile
      CHARACTER*(MAX_LEN_FNAM) viscAhZfile
      CHARACTER*(MAX_LEN_FNAM) viscA4Dfile
      CHARACTER*(MAX_LEN_FNAM) viscA4Zfile
      CHARACTER*(MAX_LEN_FNAM) zonalWindFile
      CHARACTER*(MAX_LEN_FNAM) meridWindFile
      CHARACTER*(MAX_LEN_FNAM) thetaClimFile
      CHARACTER*(MAX_LEN_FNAM) saltClimFile
      CHARACTER*(MAX_LEN_FNAM) surfQfile
      CHARACTER*(MAX_LEN_FNAM) surfQnetFile
      CHARACTER*(MAX_LEN_FNAM) surfQswFile
      CHARACTER*(MAX_LEN_FNAM) EmPmRfile
      CHARACTER*(MAX_LEN_FNAM) saltFluxFile
      CHARACTER*(MAX_LEN_FNAM) uVelInitFile
      CHARACTER*(MAX_LEN_FNAM) vVelInitFile
      CHARACTER*(MAX_LEN_FNAM) pSurfInitFile
      CHARACTER*(MAX_LEN_FNAM) pLoadFile
      CHARACTER*(MAX_LEN_FNAM) geoPotAnomFile
      CHARACTER*(MAX_LEN_FNAM) addMassFile
      CHARACTER*(MAX_LEN_FNAM) eddyPsiXFile
      CHARACTER*(MAX_LEN_FNAM) eddyPsiYFile
      CHARACTER*(MAX_LEN_FNAM) geothermalFile
      CHARACTER*(MAX_LEN_FNAM) lambdaThetaFile
      CHARACTER*(MAX_LEN_FNAM) lambdaSaltFile
      CHARACTER*(MAX_LEN_FNAM) wghtBalanceFile
      CHARACTER*(MAX_LEN_PREC/2) the_run_name

C--   COMMON /PARM_I/ Integer valued parameters used by the model.
C     cg2dMaxIters        :: Maximum number of iterations in the
C                            two-dimensional con. grad solver.
C     cg2dMinItersNSA     :: Minimum number of iterations in the
C                            not-self-adjoint version (cg2d_nsa.F) of the
C                            two-dimensional con. grad solver (default = 0).
C     cg2dPreCondFreq     :: Frequency for updating cg2d preconditioner
C                            (non-linear free-surf.)
C     cg2dUseMinResSol    :: =0 : use last-iteration/converged solution
C                            =1 : use solver minimum-residual solution
C     cg3dMaxIters        :: Maximum number of iterations in the
C                            three-dimensional con. grad solver.
C     printResidualFreq   :: Frequency for printing residual in CG iterations
C     nIter0              :: Start time-step number of for this run
C     nTimeSteps          :: Number of timesteps to execute
C     nTimeSteps_l2       :: Number of inner timesteps to execute per timestep
C     selectCoriMap       :: select setting of Coriolis parameter map:
C                           =0 f-Plane (Constant Coriolis, = f0)
C                           =1 Beta-Plane Coriolis (= f0 + beta.y)
C                           =2 Spherical Coriolis (= 2.omega.sin(phi))
C                           =3 Read Coriolis 2-d fields from files.
C     selectSigmaCoord    :: option related to sigma vertical coordinate
C     nonlinFreeSurf      :: option related to non-linear free surface
C                           =0 Linear free surface ; >0 Non-linear
C     select_rStar        :: option related to r* vertical coordinate
C                           =0 (default) use r coord. ; > 0 use r*
C     selectNHfreeSurf    :: option for Non-Hydrostatic (free-)Surface formulation:
C                           =0 (default) hydrostatic surf. ; > 0 add NH effects.
C     selectP_inEOS_Zc    :: select which pressure to use in EOS (for z-coords)
C                           =0: simply: -g*rhoConst*z
C                           =1: use pRef = integral{-g*rho(Tref,Sref,pRef)*dz}
C                           =2: use hydrostatic dynamical pressure
C                           =3: use full (Hyd+NH) dynamical pressure
C     selectAddFluid      :: option to add mass source/sink of fluid in the interior
C                            (3-D generalisation of oceanic real-fresh water flux)
C                           =0 off ; =1 add fluid ; =-1 virtual flux (no mass added)
C     selectBalanceEmPmR  :: option to balance net surface fresh-water flux:
C                           =0 off ; =1 uniform correction ; = 2 weighted correction
C     selectImplicitDrag  :: select Implicit treatment of bottom/top drag
C                           = 0: fully explicit
C                           = 1: implicit on provisional velocity
C                                (i.e., before grad.Eta increment)
C                           = 2: fully implicit (combined with Impl Surf.Press)
C     selectPenetratingSW :: select treatment of penetrating shortwave radiation
C                            (requires to define SHORTWAVE_HEATING):
C                           = 0: no shortwave penetration
C                           = 1: constant in time and horizontally uniform
C                                fraction of shortwave penetration (default)
C                           = 2: constant in time, but non-uniform fraction of
C                                shortwave penetration (not yet coded)
C                           > 2: time varying fraction of shortwave penetration
C                                according to external function (e.g. BGC model,
C                                not yet coded)
C     momForcingOutAB     :: =1: take momentum forcing contribution
C                            out of (=0: in) Adams-Bashforth time stepping.
C     tracForcingOutAB    :: =1: take tracer (Temp,Salt,pTracers) forcing contribution
C                            out of (=0: in) Adams-Bashforth time stepping.
C     tempAdvScheme       :: Temp. Horiz.Advection scheme selector
C     tempVertAdvScheme   :: Temp. Vert. Advection scheme selector
C     saltAdvScheme       :: Salt. Horiz.advection scheme selector
C     saltVertAdvScheme   :: Salt. Vert. Advection scheme selector
C     selectKEscheme      :: Kinetic Energy scheme selector (Vector Inv.)
C     selectVortScheme    :: Scheme selector for Vorticity term (Vector Inv.)
C     selectMetricTerms   :: Scheme selector for Metric terms (Flux-Form)
C     selectCoriScheme    :: Scheme selector for Coriolis term
C     select3dCoriScheme  :: Scheme selector for 3-D Coriolis (in Omega.cos Phi)
C     selectBotDragQuadr  :: quadratic bottom drag discretisation option:
C                           =0: average KE from grid center to U & V location
C                           =1: use local velocity norm @ U & V location
C                           =2: same with wet-point averaging of other component
C     pCellMix_select     :: select option to enhance mixing near surface & bottom
C                            unit digit: near bottom ; tens digit: near surface
C                            with digit =0 : disable ;
C                           = 1 : increases mixing linearly with recip_hFac
C                           = 2,3,4 : increases mixing by recip_hFac^(2,3,4)
C     readBinaryPrec      :: Precision used for reading binary files
C     writeBinaryPrec     :: Precision used for writing binary files
C     rwSuffixType        :: controls the format of the mds file suffix.
C                          =0 (default): use iteration number (myIter, I10.10);
C                          =1: 100*myTime (100th sec); =2: myTime (seconds);
C                          =3: myTime/360 (10th of hr); =4: myTime/3600 (hours).
C     monitorSelect       :: select group of variables to monitor
C                            =1 : dynvars ; =2 : + vort ; =3 : + surface
C-    debugLevel          :: controls printing of algorithm intermediate results
C                            and statistics ; higher -> more writing
C-    plotLevel           :: controls printing of field maps ; higher -> more flds

      COMMON /PARM_I/
     &        cg2dMaxIters, cg2dMinItersNSA,
     &        cg2dPreCondFreq, cg2dUseMinResSol,
     &        cg3dMaxIters, printResidualFreq,
     &        nIter0, nTimeSteps, nTimeSteps_l2, nEndIter,
     &        selectCoriMap,
     &        selectSigmaCoord,
     &        nonlinFreeSurf, select_rStar,
     &        selectNHfreeSurf, selectP_inEOS_Zc,
     &        selectAddFluid, selectBalanceEmPmR, selectImplicitDrag,
     &        momForcingOutAB, tracForcingOutAB,
     &        tempAdvScheme, tempVertAdvScheme,
     &        saltAdvScheme, saltVertAdvScheme,
     &        selectKEscheme, selectVortScheme, selectMetricTerms,
     &        selectCoriScheme, select3dCoriScheme,
     &        selectBotDragQuadr, selectPenetratingSW, pCellMix_select,
     &        readBinaryPrec, writeBinaryPrec,
     &        rwSuffixType, monitorSelect, debugLevel, plotLevel
      INTEGER cg2dMaxIters
      INTEGER cg2dMinItersNSA
      INTEGER cg2dPreCondFreq
      INTEGER cg2dUseMinResSol
      INTEGER cg3dMaxIters
      INTEGER printResidualFreq
      INTEGER nIter0
      INTEGER nTimeSteps
      INTEGER nTimeSteps_l2
      INTEGER nEndIter
      INTEGER selectCoriMap
      INTEGER selectSigmaCoord
      INTEGER nonlinFreeSurf
      INTEGER select_rStar
      INTEGER selectNHfreeSurf
      INTEGER selectP_inEOS_Zc
      INTEGER selectAddFluid
      INTEGER selectBalanceEmPmR
      INTEGER selectImplicitDrag
      INTEGER momForcingOutAB, tracForcingOutAB
      INTEGER tempAdvScheme, tempVertAdvScheme
      INTEGER saltAdvScheme, saltVertAdvScheme
      INTEGER selectKEscheme
      INTEGER selectVortScheme
      INTEGER selectMetricTerms
      INTEGER selectCoriScheme
      INTEGER select3dCoriScheme
      INTEGER selectBotDragQuadr
      INTEGER selectPenetratingSW
      INTEGER pCellMix_select
      INTEGER readBinaryPrec
      INTEGER writeBinaryPrec
      INTEGER rwSuffixType
      INTEGER monitorSelect
      INTEGER debugLevel
      INTEGER plotLevel

C--   COMMON /PARM_L/ Logical valued parameters used by the model.
C- Coordinate + Grid params:
C     fluidIsAir       :: Set to indicate that the fluid major constituent
C                         is air
C     fluidIsWater     :: Set to indicate that the fluid major constituent
C                         is water
C     usingPCoords     :: Set to indicate that we are working in a pressure
C                         type coordinate (p or p*).
C     usingZCoords     :: Set to indicate that we are working in a height
C                         type coordinate (z or z*)
C     usingCartesianGrid :: If TRUE grid generation will be in a cartesian
C                           coordinate frame.
C     usingSphericalPolarGrid :: If TRUE grid generation will be in a
C                                spherical polar frame.
C     rotateGrid      :: rotate grid coordinates to geographical coordinates
C                        according to Euler angles phiEuler, thetaEuler, psiEuler
C     usingCylindricalGrid :: If TRUE grid generation will be Cylindrical
C     usingCurvilinearGrid :: If TRUE, use a curvilinear grid (to be provided)
C     hasWetCSCorners :: domain contains CS-type corners where dynamics is solved
C     deepAtmosphere :: deep model (drop the shallow-atmosphere approximation)
C     setInterFDr    :: set Interface depth (put cell-Center at the middle)
C     setCenterDr    :: set cell-Center depth (put Interface at the middle)
C     useMin4hFacEdges :: set hFacW,hFacS as minimum of adjacent hFacC factor
C     interViscAr_pCell :: account for partial-cell in interior vert. viscosity
C     interDiffKr_pCell :: account for partial-cell in interior vert. diffusion
C- Momentum params:
C     no_slip_sides  :: Impose "no-slip" at lateral boundaries.
C     no_slip_bottom :: Impose "no-slip" at bottom boundary.
C     bottomVisc_pCell :: account for partial-cell in bottom visc. (no-slip BC)
C     useSmag3D      :: Use isotropic 3-D Smagorinsky
C     useFullLeith   :: Set to true to use full Leith viscosity(may be unstable
C                       on irregular grids)
C     useStrainTensionVisc:: Set to true to use Strain-Tension viscous terms
C     useAreaViscLength :: Set to true to use old scaling for viscous lengths,
C                          e.g., L2=Raz.  May be preferable for cube sphere.
C     momViscosity  :: Flag which turns momentum friction terms on and off.
C     momAdvection  :: Flag which turns advection of momentum on and off.
C     momForcing    :: Flag which turns external forcing of momentum on and off.
C     momTidalForcing    :: Flag which turns tidal forcing on and off.
C     momPressureForcing :: Flag which turns pressure term in momentum equation
C                          on and off.
C     useNHMTerms   :: If TRUE use non-hydrostatic metric terms.
C     useCoriolis   :: Flag which turns the coriolis terms on and off.
C     useCDscheme   :: use CD-scheme to calculate Coriolis terms.
C     vectorInvariantMomentum :: use Vector-Invariant form (mom_vecinv package)
C                                (default = F = use mom_fluxform package)
C     useJamartMomAdv :: Use wet-point method for V.I. non-linear term
C     upwindVorticity :: bias interpolation of vorticity in the Coriolis term
C     highOrderVorticity :: use 3rd/4th order interp. of vorticity (V.I., advection)
C     useAbsVorticity :: work with f+zeta in Coriolis terms
C     upwindShear     :: use 1rst order upwind interp. (V.I., vertical advection)
C     momStepping    :: Turns momentum equation time-stepping off
C     calc_wVelocity :: Turns vertical velocity calculation off
C- Temp. & Salt params:
C     tempStepping   :: Turns temperature equation time-stepping on/off
C     saltStepping   :: Turns salinity equation time-stepping on/off
C     addFrictionHeating :: account for frictional heating
C     temp_stayPositive :: use Smolarkiewicz Hack to ensure Temp stays positive
C     salt_stayPositive :: use Smolarkiewicz Hack to ensure Salt stays positive
C     tempAdvection  :: Flag which turns advection of temperature on and off.
C     tempVertDiff4  :: use vertical bi-harmonic diffusion for temperature
C     tempIsActiveTr :: Pot.Temp. is a dynamically active tracer
C     tempForcing    :: Flag which turns external forcing of temperature on/off
C     saltAdvection  :: Flag which turns advection of salinity on and off.
C     saltVertDiff4  :: use vertical bi-harmonic diffusion for salinity
C     saltIsActiveTr :: Salinity  is a dynamically active tracer
C     saltForcing    :: Flag which turns external forcing of salinity on/off
C     maskIniTemp    :: apply mask to initial Pot.Temp.
C     maskIniSalt    :: apply mask to initial salinity
C     checkIniTemp   :: check for points with identically zero initial Pot.Temp.
C     checkIniSalt   :: check for points with identically zero initial salinity
C- Pressure solver related parameters (PARM02)
C     useNSACGSolver :: Set to true to use "not self-adjoint" conjugate
C                       gradient solver that stores the iteration history
C                       for an iterative adjoint as accuate as possible
C     useSRCGSolver  :: Set to true to use conjugate gradient
C                       solver with single reduction (only one call of
C                       s/r mpi_allreduce), default is false
C- Time-stepping & free-surface params:
C     rigidLid            :: Set to true to use rigid lid
C     implicitFreeSurface :: Set to true to use implicit free surface
C     uniformLin_PhiSurf  :: Set to true to use a uniform Bo_surf in the
C                            linear relation Phi_surf = Bo_surf*eta
C     uniformFreeSurfLev  :: TRUE if free-surface level-index is uniform (=1)
C     exactConserv        :: Set to true to conserve exactly the total Volume
C     linFSConserveTr     :: Set to true to correct source/sink of tracer
C                            at the surface due to Linear Free Surface
C     useRealFreshWaterFlux :: if True (=Natural BCS), treats P+R-E flux
C                         as a real Fresh Water (=> changes the Sea Level)
C                         if F, converts P+R-E to salt flux (no SL effect)
C     storePhiHyd4Phys :: store hydrostatic potential for use in Physics/EOS
C                         this requires specific code for restart & exchange
C     quasiHydrostatic :: Using non-hydrostatic terms in hydrostatic algorithm
C     nonHydrostatic   :: Using non-hydrostatic algorithm
C     use3Dsolver      :: set to true to use 3-D pressure solver
C     implicitIntGravWave :: treat Internal Gravity Wave implicitly
C     staggerTimeStep   :: enable a Stagger time stepping U,V (& W) then T,S
C     applyExchUV_early :: Apply EXCH to U,V earlier, just before integr_continuity
C     doResetHFactors   :: Do reset thickness factors @ beginning of each time-step
C     implicitDiffusion :: Turns implicit vertical diffusion on
C     implicitViscosity :: Turns implicit vertical viscosity on
C     tempImplVertAdv   :: Turns on implicit vertical advection for Temperature
C     saltImplVertAdv   :: Turns on implicit vertical advection for Salinity
C     momImplVertAdv    :: Turns on implicit vertical advection for Momentum
C     multiDimAdvection :: Flag that enable multi-dimension advection
C     useMultiDimAdvec  :: True if multi-dim advection is used at least once
C     momDissip_In_AB   :: if False, put Dissipation tendency contribution
C                          out off Adams-Bashforth time stepping.
C     doAB_onGtGs       :: if the Adams-Bashforth time stepping is used, always
C                          apply AB on tracer tendencies (rather than on Tracer)
C- Other forcing params -
C     balanceQnet     :: substract global mean of Qnet at every time step
C     balancePrintMean:: print substracted global means to STDOUT
C     doThetaClimRelax :: Set true if relaxation to temperature
C                        climatology is required.
C     doSaltClimRelax  :: Set true if relaxation to salinity
C                        climatology is required.
C     balanceThetaClimRelax :: substract global mean effect at every time step
C     balanceSaltClimRelax :: substract global mean effect at every time step
C     allowFreezing  :: Allows surface water to freeze and form ice
C     periodicExternalForcing :: Set true if forcing is time-dependant
C- I/O parameters -
C     globalFiles    :: Selects between "global" and "tiled" files.
C                       On some platforms with MPI, option globalFiles is either
C                       slow or does not work. Use useSingleCpuIO instead.
C     useSingleCpuIO :: moved to EEPARAMS.h
C     pickupStrictlyMatch :: check and stop if pickup-file do not stricly match
C     startFromPickupAB2 :: with AB-3 code, start from an AB-2 pickup
C     usePickupBeforeC54 :: start from old-pickup files, generated with code from
C                           before checkpoint-54a, Jul 06, 2004.
C     pickup_write_mdsio :: use mdsio to write pickups
C     pickup_read_mdsio  :: use mdsio to read  pickups
C     pickup_write_immed :: echo the pickup immediately (for conversion)
C     writePickupAtEnd   :: write pickup at the last timestep
C     snapshot_mdsio     :: use mdsio for "snapshot" (dumpfreq/diagfreq) output
C     monitor_stdio      :: use stdio for monitor output
C     dumpInitAndLast :: dumps model state to files at Initial (nIter0)
C                        & Last iteration, in addition multiple of dumpFreq iter.

      COMMON /PARM_L/
     & fluidIsAir, fluidIsWater,
     & usingPCoords, usingZCoords,
     & usingCartesianGrid, usingSphericalPolarGrid, rotateGrid,
     & usingCylindricalGrid, usingCurvilinearGrid, hasWetCSCorners,
     & deepAtmosphere, setInterFDr, setCenterDr, useMin4hFacEdges,
     & interViscAr_pCell, interDiffKr_pCell,
     & no_slip_sides, no_slip_bottom, bottomVisc_pCell, useSmag3D,
     & useFullLeith, useStrainTensionVisc, useAreaViscLength,
     & momViscosity, momAdvection, momForcing, momTidalForcing,
     & momPressureForcing, useNHMTerms,
     & useCoriolis, useCDscheme, vectorInvariantMomentum,
     & useJamartMomAdv, upwindVorticity, highOrderVorticity,
     & useAbsVorticity, upwindShear,
     & momStepping, calc_wVelocity, tempStepping, saltStepping,
     & addFrictionHeating, temp_stayPositive, salt_stayPositive,
     & tempAdvection, tempVertDiff4, tempIsActiveTr, tempForcing,
     & saltAdvection, saltVertDiff4, saltIsActiveTr, saltForcing,
     & maskIniTemp, maskIniSalt, checkIniTemp, checkIniSalt,
     & useNSACGSolver, useSRCGSolver,
     & rigidLid, implicitFreeSurface,
     & uniformLin_PhiSurf, uniformFreeSurfLev,
     & exactConserv, linFSConserveTr, useRealFreshWaterFlux,
     & storePhiHyd4Phys, quasiHydrostatic, nonHydrostatic,
     & use3Dsolver, implicitIntGravWave, staggerTimeStep,
     & applyExchUV_early, doResetHFactors,
     & implicitDiffusion, implicitViscosity,
     & tempImplVertAdv, saltImplVertAdv, momImplVertAdv,
     & multiDimAdvection, useMultiDimAdvec,
     & momDissip_In_AB, doAB_onGtGs,
     & balanceQnet, balancePrintMean,
     & balanceThetaClimRelax, balanceSaltClimRelax,
     & doThetaClimRelax, doSaltClimRelax,
     & allowFreezing,
     & periodicExternalForcing,
     & globalFiles,
     & pickupStrictlyMatch, usePickupBeforeC54, startFromPickupAB2,
     & pickup_read_mdsio, pickup_write_mdsio, pickup_write_immed,
     & writePickupAtEnd,
     & snapshot_mdsio, monitor_stdio,
     & outputTypesInclusive, dumpInitAndLast

      LOGICAL fluidIsAir
      LOGICAL fluidIsWater
      LOGICAL usingPCoords
      LOGICAL usingZCoords
      LOGICAL usingCartesianGrid
      LOGICAL usingSphericalPolarGrid, rotateGrid
      LOGICAL usingCylindricalGrid
      LOGICAL usingCurvilinearGrid, hasWetCSCorners
      LOGICAL deepAtmosphere
      LOGICAL setInterFDr
      LOGICAL setCenterDr
      LOGICAL useMin4hFacEdges
      LOGICAL interViscAr_pCell
      LOGICAL interDiffKr_pCell

      LOGICAL no_slip_sides
      LOGICAL no_slip_bottom
      LOGICAL bottomVisc_pCell
      LOGICAL useSmag3D
      LOGICAL useFullLeith
      LOGICAL useStrainTensionVisc
      LOGICAL useAreaViscLength
      LOGICAL momViscosity
      LOGICAL momAdvection
      LOGICAL momForcing
      LOGICAL momTidalForcing
      LOGICAL momPressureForcing
      LOGICAL useNHMTerms

      LOGICAL useCoriolis
      LOGICAL useCDscheme
      LOGICAL vectorInvariantMomentum
      LOGICAL useJamartMomAdv
      LOGICAL upwindVorticity
      LOGICAL highOrderVorticity
      LOGICAL useAbsVorticity
      LOGICAL upwindShear
      LOGICAL momStepping
      LOGICAL calc_wVelocity
      LOGICAL tempStepping
      LOGICAL saltStepping
      LOGICAL addFrictionHeating
      LOGICAL temp_stayPositive
      LOGICAL salt_stayPositive
      LOGICAL tempAdvection
      LOGICAL tempVertDiff4
      LOGICAL tempIsActiveTr
      LOGICAL tempForcing
      LOGICAL saltAdvection
      LOGICAL saltVertDiff4
      LOGICAL saltIsActiveTr
      LOGICAL saltForcing
      LOGICAL maskIniTemp
      LOGICAL maskIniSalt
      LOGICAL checkIniTemp
      LOGICAL checkIniSalt
      LOGICAL useNSACGSolver
      LOGICAL useSRCGSolver
      LOGICAL rigidLid
      LOGICAL implicitFreeSurface
      LOGICAL uniformLin_PhiSurf
      LOGICAL uniformFreeSurfLev
      LOGICAL exactConserv
      LOGICAL linFSConserveTr
      LOGICAL useRealFreshWaterFlux
      LOGICAL storePhiHyd4Phys
      LOGICAL quasiHydrostatic
      LOGICAL nonHydrostatic
      LOGICAL use3Dsolver
      LOGICAL implicitIntGravWave
      LOGICAL staggerTimeStep
      LOGICAL applyExchUV_early
      LOGICAL doResetHFactors
      LOGICAL implicitDiffusion
      LOGICAL implicitViscosity
      LOGICAL tempImplVertAdv
      LOGICAL saltImplVertAdv
      LOGICAL momImplVertAdv
      LOGICAL multiDimAdvection
      LOGICAL useMultiDimAdvec
      LOGICAL momDissip_In_AB
      LOGICAL doAB_onGtGs
      LOGICAL balanceQnet
      LOGICAL balancePrintMean
      LOGICAL doThetaClimRelax
      LOGICAL doSaltClimRelax
      LOGICAL balanceThetaClimRelax
      LOGICAL balanceSaltClimRelax
      LOGICAL allowFreezing
      LOGICAL periodicExternalForcing
      LOGICAL globalFiles
      LOGICAL pickupStrictlyMatch
      LOGICAL usePickupBeforeC54
      LOGICAL startFromPickupAB2
      LOGICAL pickup_read_mdsio, pickup_write_mdsio
      LOGICAL pickup_write_immed, writePickupAtEnd
      LOGICAL snapshot_mdsio, monitor_stdio
      LOGICAL outputTypesInclusive
      LOGICAL dumpInitAndLast

C--   COMMON /PARM_R/ "Real" valued parameters used by the model.
C     cg2dTargetResidual
C          :: Target residual for cg2d solver ; no unit (RHS normalisation)
C     cg2dTargetResWunit
C          :: Target residual for cg2d solver ; W unit (No RHS normalisation)
C     cg3dTargetResidual
C          :: Target residual for cg3d solver ; no unit (RHS normalisation)
C     cg3dTargetResWunit
C          :: Target residual for cg3d solver ; W unit (No RHS normalisation)
C     cg2dpcOffDFac :: Averaging weight for preconditioner off-diagonal.
C     Note. 20th May 1998
C           I made a weird discovery! In the model paper we argue
C           for the form of the preconditioner used here ( see
C           A Finite-volume, Incompressible Navier-Stokes Model
C           ...., Marshall et. al ). The algebra gives a simple
C           0.5 factor for the averaging of ac and aCw to get a
C           symmettric pre-conditioner. By using a factor of 0.51
C           i.e. scaling the off-diagonal terms in the
C           preconditioner down slightly I managed to get the
C           number of iterations for convergence in a test case to
C           drop form 192 -> 134! Need to investigate this further!
C           For now I have introduced a parameter cg2dpcOffDFac which
C           defaults to 0.51 but can be set at runtime.
C     delR      :: Vertical grid spacing ( units of r ).
C     delRc     :: Vertical grid spacing between cell centers (r unit).
C     delX      :: Separation between cell faces (m) or (deg), depending
C     delY         on input flags. Note: moved to header file SET_GRID.h
C     xgOrigin   :: Origin of the X-axis (Cartesian Grid) / Longitude of Western
C                :: most cell face (Lat-Lon grid) (Note: this is an "inert"
C                :: parameter but it makes geographical references simple.)
C     ygOrigin   :: Origin of the Y-axis (Cartesian Grid) / Latitude of Southern
C                :: most face (Lat-Lon grid).
C     rSphere    :: Radius of sphere for a spherical polar grid ( m ).
C     recip_rSphere :: Reciprocal radius of sphere ( m^-1 ).
C     radius_fromHorizGrid :: sphere Radius of input horiz. grid (Curvilinear Grid)
C     seaLev_Z   :: the reference height of sea-level (usually zero)
C     top_Pres   :: pressure (P-Coords) or reference pressure (Z-Coords) at the top
C     rSigmaBnd  :: vertical position (in r-unit) of r/sigma transition (Hybrid-Sigma)
C     gravity    :: Acceleration due to constant gravity ( m/s^2 )
C     recip_gravity :: Reciprocal gravity acceleration ( s^2/m )
C     gBaro      :: Accel. due to gravity used in barotropic equation ( m/s^2 )
C     gravFacC   :: gravity factor (vs surf. gravity) vert. profile at cell-Center
C     gravFacF   :: gravity factor (vs surf. gravity) vert. profile at cell-interF
C     rhoNil     :: Reference density for the linear equation of state
C     rhoConst   :: Vertically constant reference density (Boussinesq)
C     rho1Ref    :: reference vertical profile for density (anelastic)
C     rhoFacC    :: normalized (by rhoConst) reference density at cell-Center
C     rhoFacF    :: normalized (by rhoConst) reference density at cell-interFace
C     rhoConstFresh :: Constant reference density for fresh water (rain)
C     thetaConst :: Constant reference for potential temperature
C     tRef       :: reference vertical profile for potential temperature
C     sRef       :: reference vertical profile for salinity/specific humidity
C     rhoRef     :: density vertical profile from (tRef,sRef) [kg/m^3]
C     dBdrRef    :: vertical gradient of reference buoyancy  [(m/s/r)^2]:
C                :: z-coord: = N^2_ref = Brunt-Vaissala frequency [s^-2]
C                :: p-coord: = -(d.alpha/dp)_ref          [(m^2.s/kg)^2]
C     surf_pRef  :: surface reference pressure ( Pa )
C     pRef4EOS   :: reference pressure used in EOS (case selectP_inEOS_Zc=1)
C     phiRef     :: reference potential (press/rho, geopot) profile (m^2/s^2)
C     rVel2wUnit :: units conversion factor (Non-Hydrostatic code),
C                :: from r-coordinate vertical velocity to vertical velocity [m/s].
C                :: z-coord: = 1 ; p-coord: wSpeed [m/s] = rVel [Pa/s] * rVel2wUnit
C     wUnit2rVel :: units conversion factor (Non-Hydrostatic code),
C                :: from vertical velocity [m/s] to r-coordinate vertical velocity.
C                :: z-coord: = 1 ; p-coord: rVel [Pa/s] = wSpeed [m/s] * wUnit2rVel
C     rUnit2z    :: units conversion factor (for ocean in P-coord, only fct of k),
C                :: from r-coordinate to z [m] (at level center):
C                :: z-coord: = 1 ; p-coord: dz [m] = dr [Pa] * rUnit2z
C     z2rUnit    :: units conversion factor (for ocean in P-coord, only fct of k),
C                :: from z [m] to r-coordinate (at level center):
C                :: z-coord: = 1 ; p-coord: dr [Pa] = dz [m] * z2rUnit
C     mass2rUnit :: units conversion factor (surface forcing),
C                :: from mass per unit area [kg/m2] to vertical r-coordinate unit.
C                :: z-coord: = 1/rhoConst ( [kg/m2] / rho = [m] ) ;
C                :: p-coord: = gravity    ( [kg/m2] *  g = [Pa] ) ;
C     rUnit2mass :: units conversion factor (surface forcing),
C                :: from vertical r-coordinate unit to mass per unit area [kg/m2].
C                :: z-coord: = rhoConst  ( [m] * rho = [kg/m2] ) ;
C                :: p-coord: = 1/gravity ( [Pa] /  g = [kg/m2] ) ;
C     sIceLoadFac:: factor to scale (and turn off) sIceLoad (sea-ice loading)
C                   default = 1
C     f0         :: Reference coriolis parameter ( 1/s )
C                   ( Southern edge f for beta plane )
C     beta       :: df/dy ( s^-1.m^-1 )
C     fPrime     :: Second Coriolis parameter ( 1/s ), related to Y-component
C                   of rotation (reference value = 2.Omega.Cos(Phi))
C     omega      :: Angular velocity ( rad/s )
C     rotationPeriod :: Rotation period (s) (= 2.pi/omega)
C     viscArNr   :: vertical profile of Eddy viscosity coeff.
C                   for vertical mixing of momentum ( units of r^2/s )
C     viscAh     :: Eddy viscosity coeff. for mixing of
C                   momentum laterally ( m^2/s )
C     viscAhW    :: Eddy viscosity coeff. for mixing of vertical
C                   momentum laterally, no effect for hydrostatic
C                   model, defaults to viscAhD if unset ( m^2/s )
C                   Not used if variable horiz. viscosity is used.
C     viscA4     :: Biharmonic viscosity coeff. for mixing of
C                   momentum laterally ( m^4/s )
C     viscA4W    :: Biharmonic viscosity coeff. for mixing of vertical
C                   momentum laterally, no effect for hydrostatic
C                   model, defaults to viscA4D if unset ( m^2/s )
C                   Not used if variable horiz. viscosity is used.
C     viscAhD    :: Eddy viscosity coeff. for mixing of momentum laterally
C                   (act on Divergence part) ( m^2/s )
C     viscAhZ    :: Eddy viscosity coeff. for mixing of momentum laterally
C                   (act on Vorticity  part) ( m^2/s )
C     viscA4D    :: Biharmonic viscosity coeff. for mixing of momentum laterally
C                   (act on Divergence part) ( m^4/s )
C     viscA4Z    :: Biharmonic viscosity coeff. for mixing of momentum laterally
C                   (act on Vorticity  part) ( m^4/s )
C     smag3D_coeff     :: Isotropic 3-D Smagorinsky viscosity coefficient (-)
C     smag3D_diffCoeff :: Isotropic 3-D Smagorinsky diffusivity coefficient (-)
C     viscC2leith  :: Leith non-dimensional viscosity factor (grad(vort))
C     viscC2leithD :: Modified Leith non-dimensional visc. factor (grad(div))
C     viscC2LeithQG:: QG Leith non-dimensional viscosity factor
C     viscC4leith  :: Leith non-dimensional viscosity factor (grad(vort))
C     viscC4leithD :: Modified Leith non-dimensional viscosity factor (grad(div))
C     viscC2smag   :: Smagorinsky non-dimensional viscosity factor (harmonic)
C     viscC4smag   :: Smagorinsky non-dimensional viscosity factor (biharmonic)
C     viscAhMax    :: Maximum eddy viscosity coeff. for mixing of
C                    momentum laterally ( m^2/s )
C     viscAhReMax  :: Maximum gridscale Reynolds number for eddy viscosity
C                     coeff. for mixing of momentum laterally (non-dim)
C     viscAhGrid   :: non-dimensional grid-size dependent viscosity
C     viscAhGridMax:: maximum and minimum harmonic viscosity coefficients ...
C     viscAhGridMin::  in terms of non-dimensional grid-size dependent visc.
C     viscA4Max    :: Maximum biharmonic viscosity coeff. for mixing of
C                     momentum laterally ( m^4/s )
C     viscA4ReMax  :: Maximum Gridscale Reynolds number for
C                     biharmonic viscosity coeff. momentum laterally (non-dim)
C     viscA4Grid   :: non-dimensional grid-size dependent bi-harmonic viscosity
C     viscA4GridMax:: maximum and minimum biharmonic viscosity coefficients ...
C     viscA4GridMin::  in terms of non-dimensional grid-size dependent viscosity
C     diffKhT   :: Laplacian diffusion coeff. for mixing of
C                 heat laterally ( m^2/s )
C     diffK4T   :: Biharmonic diffusion coeff. for mixing of
C                 heat laterally ( m^4/s )
C     diffKrNrT :: vertical profile of Laplacian diffusion coeff.
C                 for mixing of heat vertically ( units of r^2/s )
C     diffKr4T  :: vertical profile of Biharmonic diffusion coeff.
C                 for mixing of heat vertically ( units of r^4/s )
C     diffKhS  ::  Laplacian diffusion coeff. for mixing of
C                 salt laterally ( m^2/s )
C     diffK4S   :: Biharmonic diffusion coeff. for mixing of
C                 salt laterally ( m^4/s )
C     diffKrNrS :: vertical profile of Laplacian diffusion coeff.
C                 for mixing of salt vertically ( units of r^2/s ),
C     diffKr4S  :: vertical profile of Biharmonic diffusion coeff.
C                 for mixing of salt vertically ( units of r^4/s )
C     diffKrBL79surf :: T/S surface diffusivity (m^2/s) Bryan and Lewis, 1979
C     diffKrBL79deep :: T/S deep diffusivity (m^2/s) Bryan and Lewis, 1979
C     diffKrBL79scl  :: depth scale for arctan fn (m) Bryan and Lewis, 1979
C     diffKrBL79Ho   :: depth offset for arctan fn (m) Bryan and Lewis, 1979
C     BL79LatVary    :: polarwise of this latitude diffKrBL79 is applied with
C                       gradual transition to diffKrBLEQ towards Equator
C     diffKrBLEQsurf :: same as diffKrBL79surf but at Equator
C     diffKrBLEQdeep :: same as diffKrBL79deep but at Equator
C     diffKrBLEQscl  :: same as diffKrBL79scl but at Equator
C     diffKrBLEQHo   :: same as diffKrBL79Ho but at Equator
C     pCellMix_maxFac :: maximum enhanced mixing factor for thin partial-cell
C     pCellMix_delR   :: thickness criteria   for too thin partial-cell
C     pCellMix_viscAr :: vertical viscosity   for too thin partial-cell
C     pCellMix_diffKr :: vertical diffusivity for too thin partial-cell
C     deltaT    :: Default timestep ( s )
C     deltaTClock  :: Timestep used as model "clock". This determines the
C                    IO frequencies and is used in tagging output. It can
C                    be totally different to the dynamical time. Typically
C                    it will be the deep-water timestep for accelerated runs.
C                    Frequency of checkpointing and dumping of the model state
C                    are referenced to this clock. ( s )
C     deltaTMom    :: Timestep for momemtum equations ( s )
C     dTtracerLev  :: Timestep for tracer equations ( s ), function of level k
C     deltaTFreeSurf :: Timestep for free-surface equation ( s )
C     freeSurfFac  :: Parameter to turn implicit free surface term on or off
C                     freeSurFac = 1. uses implicit free surface
C                     freeSurFac = 0. uses rigid lid
C     abEps        :: Adams-Bashforth-2 stabilizing weight
C     alph_AB      :: Adams-Bashforth-3 primary factor
C     beta_AB      :: Adams-Bashforth-3 secondary factor
C     implicSurfPress :: parameter of the Crank-Nickelson time stepping :
C                     Implicit part of Surface Pressure Gradient ( 0-1 )
C     implicDiv2DFlow :: parameter of the Crank-Nickelson time stepping :
C                     Implicit part of barotropic flow Divergence ( 0-1 )
C     implicitNHPress :: parameter of the Crank-Nickelson time stepping :
C                     Implicit part of Non-Hydrostatic Pressure Gradient ( 0-1 )
C     hFacMin      :: Minimum fraction size of a cell (affects hFacC etc...)
C     hFacMinDz    :: Minimum dimensional size of a cell (affects hFacC etc..., m)
C     hFacMinDp    :: Minimum dimensional size of a cell (affects hFacC etc..., Pa)
C     hFacMinDr    :: Minimum dimensional size of a cell (-> hFacC etc..., r units)
C     hFacInf      :: Threshold (inf and sup) for fraction size of surface cell
C     hFacSup          that control vanishing and creating levels
C     tauCD         :: CD scheme coupling timescale ( s )
C     rCD           :: CD scheme normalised coupling parameter (= 1 - deltaT/tauCD)
C     epsAB_CD      :: Adams-Bashforth-2 stabilizing weight used in CD scheme
C     baseTime      :: model base time (time origin) = time @ iteration zero
C     startTime     :: Starting time for this integration ( s ).
C     endTime       :: Ending time for this integration ( s ).
C     chkPtFreq     :: Frequency of rolling check pointing ( s ).
C     pChkPtFreq    :: Frequency of permanent check pointing ( s ).
C     dumpFreq      :: Frequency with which model state is written to
C                      post-processing files ( s ).
C     diagFreq      :: Frequency with which model writes diagnostic output
C                      of intermediate quantities.
C     afFacMom      :: Advection of momentum term multiplication factor
C     vfFacMom      :: Momentum viscosity term    multiplication factor
C     pfFacMom      :: Momentum pressure forcing  multiplication factor
C     cfFacMom      :: Coriolis term              multiplication factor
C     foFacMom      :: Momentum forcing           multiplication factor
C     mtFacMom      :: Metric terms               multiplication factor
C     cosPower      :: Power of cosine of latitude to multiply viscosity
C     cAdjFreq      :: Frequency of convective adjustment
C
C     tauThetaClimRelax :: Relaxation to climatology time scale ( s ).
C     tauSaltClimRelax :: Relaxation to climatology time scale ( s ).
C     latBandClimRelax :: latitude band where Relaxation to Clim. is applied,
C                         i.e. where |yC| <= latBandClimRelax
C     externForcingPeriod :: Is the period of which forcing varies (eg. 1 month)
C     externForcingCycle :: Is the repeat time of the forcing (eg. 1 year)
C                          (note: externForcingCycle must be an integer
C                           number times externForcingPeriod)
C     convertFW2Salt :: salinity, used to convert Fresh-Water Flux to Salt Flux
C                       (use model surface (local) value if set to -1)
C     temp_EvPrRn :: temperature of Rain & Evap.
C     salt_EvPrRn :: salinity of Rain & Evap.
C     temp_addMass :: temperature of addMass field
C     salt_addMass :: salinity of addMass field
C        (notes: a) tracer content of Rain/Evap only used if both
C                     NonLin_FrSurf & useRealFreshWater are set.
C                b) use model surface (local) value if set to UNSET_RL)
C     hMixCriteria:: criteria for mixed-layer diagnostic
C     dRhoSmall   :: parameter for mixed-layer diagnostic
C     hMixSmooth  :: Smoothing parameter for mixed-layer diag
C                    (default=0: no smoothing)
C     ivdc_kappa  :: implicit vertical diffusivity for convection [m^2/s]
C     sideDragFactor     :: side-drag scaling factor (used only if no_slip_sides)
C                           (default=2: full drag ; =1: gives half-slip BC)
C     bottomDragLinear    :: Linear    bottom-drag coefficient (units of [r]/s)
C     bottomDragQuadratic :: Quadratic bottom-drag coefficient (units of [r]/m)
C               (if using zcoordinate, units becomes linear: m/s, quadratic: [-])
C     zRoughBot :: roughness length for quadratic bottom friction coefficient
C                  (in m, typical values are order 0.01 m)
C     smoothAbsFuncRange :: 1/2 of interval around zero, for which FORTRAN ABS
C                           is to be replace by a smoother function
C                           (affects myabs, mymin, mymax)
C     nh_Am2        :: scales non-hydrostatic terms and changes internal scales
C                      (i.e. allows convection at different Rayleigh numbers)
C     tCylIn        :: Temperature of the cylinder inner boundary
C     tCylOut       :: Temperature of the cylinder outer boundary
C     phiEuler      :: Euler angle, rotation about original z-axis
C     thetaEuler    :: Euler angle, rotation about new x-axis
C     psiEuler      :: Euler angle, rotation about new z-axis
      COMMON /PARM_R/ cg2dTargetResidual, cg2dTargetResWunit,
     & cg2dpcOffDFac, cg3dTargetResidual, cg3dTargetResWunit,
     & delR, delRc, xgOrigin, ygOrigin, rSphere, recip_rSphere,
     & radius_fromHorizGrid, seaLev_Z, top_Pres, rSigmaBnd,
     & deltaT, deltaTMom, dTtracerLev, deltaTFreeSurf, deltaTClock,
     & abEps, alph_AB, beta_AB,
     & f0, beta, fPrime, omega, rotationPeriod,
     & viscFacAdj, viscAh, viscAhW, smag3D_coeff, smag3D_diffCoeff,
     & viscAhMax, viscAhGrid, viscAhGridMax, viscAhGridMin,
     & viscC2leith, viscC2leithD, viscC2LeithQG,
     & viscC2smag, viscC4smag,
     & viscAhD, viscAhZ, viscA4D, viscA4Z,
     & viscA4, viscA4W, viscA4Max,
     & viscA4Grid, viscA4GridMax, viscA4GridMin,
     & viscAhReMax, viscA4ReMax,
     & viscC4leith, viscC4leithD, viscArNr,
     & diffKhT, diffK4T, diffKrNrT, diffKr4T,
     & diffKhS, diffK4S, diffKrNrS, diffKr4S,
     & diffKrBL79surf, diffKrBL79deep, diffKrBL79scl, diffKrBL79Ho,
     & BL79LatVary,
     & diffKrBLEQsurf, diffKrBLEQdeep, diffKrBLEQscl, diffKrBLEQHo,
     & pCellMix_maxFac, pCellMix_delR, pCellMix_viscAr, pCellMix_diffKr,
     & tauCD, rCD, epsAB_CD,
     & freeSurfFac, implicSurfPress, implicDiv2DFlow, implicitNHPress,
     & hFacMin, hFacMinDz, hFacInf, hFacSup,
     & gravity, recip_gravity, gBaro,
     & gravFacC, recip_gravFacC, gravFacF, recip_gravFacF,
     & rhoNil, rhoConst, recip_rhoConst, rho1Ref,
     & rhoFacC, recip_rhoFacC, rhoFacF, recip_rhoFacF, rhoConstFresh,
     & thetaConst, tRef, sRef, rhoRef, dBdrRef,
     & surf_pRef, pRef4EOS, phiRef,
     & rVel2wUnit, wUnit2rVel, rUnit2z, z2rUnit, mass2rUnit, rUnit2mass,
     & baseTime, startTime, endTime,
     & chkPtFreq, pChkPtFreq, dumpFreq, adjDumpFreq,
     & diagFreq, monitorFreq, adjMonitorFreq,
     & afFacMom, vfFacMom, pfFacMom, cfFacMom, foFacMom, mtFacMom,
     & cosPower, cAdjFreq,
     & tauThetaClimRelax, tauSaltClimRelax, latBandClimRelax,
     & externForcingCycle, externForcingPeriod,
     & convertFW2Salt, temp_EvPrRn, salt_EvPrRn,
     & temp_addMass, salt_addMass, hFacMinDr, hFacMinDp,
     & ivdc_kappa, hMixCriteria, dRhoSmall, hMixSmooth,
     & sideDragFactor, bottomDragLinear, bottomDragQuadratic,
     & zRoughBot, nh_Am2, smoothAbsFuncRange, sIceLoadFac,
     & tCylIn, tCylOut,
     & phiEuler, thetaEuler, psiEuler

      Real*8 cg2dTargetResidual
      Real*8 cg2dTargetResWunit
      Real*8 cg3dTargetResidual
      Real*8 cg3dTargetResWunit
      Real*8 cg2dpcOffDFac
      Real*8 delR(Nr)
      Real*8 delRc(Nr+1)
      Real*8 xgOrigin
      Real*8 ygOrigin
      Real*8 rSphere
      Real*8 recip_rSphere
      Real*8 radius_fromHorizGrid
      Real*8 seaLev_Z
      Real*8 top_Pres
      Real*8 rSigmaBnd
      Real*8 deltaT
      Real*8 deltaTClock
      Real*8 deltaTMom
      Real*8 dTtracerLev(Nr)
      Real*8 deltaTFreeSurf
      Real*8 abEps, alph_AB, beta_AB
      Real*8 f0
      Real*8 beta
      Real*8 fPrime
      Real*8 omega
      Real*8 rotationPeriod
      Real*8 freeSurfFac
      Real*8 implicSurfPress
      Real*8 implicDiv2DFlow
      Real*8 implicitNHPress
      Real*8 hFacMin
      Real*8 hFacMinDz
      Real*8 hFacMinDp
      Real*8 hFacMinDr
      Real*8 hFacInf
      Real*8 hFacSup
      Real*8 viscArNr(Nr)
      Real*8 viscFacAdj
      Real*8 viscAh
      Real*8 viscAhW
      Real*8 viscAhD
      Real*8 viscAhZ
      Real*8 smag3D_coeff, smag3D_diffCoeff
      Real*8 viscAhMax
      Real*8 viscAhReMax
      Real*8 viscAhGrid, viscAhGridMax, viscAhGridMin
      Real*8 viscC2leith
      Real*8 viscC2leithD
      Real*8 viscC2LeithQG
      Real*8 viscC2smag
      Real*8 viscA4
      Real*8 viscA4W
      Real*8 viscA4D
      Real*8 viscA4Z
      Real*8 viscA4Max
      Real*8 viscA4ReMax
      Real*8 viscA4Grid, viscA4GridMax, viscA4GridMin
      Real*8 viscC4leith
      Real*8 viscC4leithD
      Real*8 viscC4smag
      Real*8 diffKhT
      Real*8 diffK4T
      Real*8 diffKrNrT(Nr)
      Real*8 diffKr4T(Nr)
      Real*8 diffKhS
      Real*8 diffK4S
      Real*8 diffKrNrS(Nr)
      Real*8 diffKr4S(Nr)
      Real*8 diffKrBL79surf
      Real*8 diffKrBL79deep
      Real*8 diffKrBL79scl
      Real*8 diffKrBL79Ho
      Real*8 BL79LatVary
      Real*8 diffKrBLEQsurf
      Real*8 diffKrBLEQdeep
      Real*8 diffKrBLEQscl
      Real*8 diffKrBLEQHo
      Real*8 pCellMix_maxFac
      Real*8 pCellMix_delR
      Real*8 pCellMix_viscAr(Nr)
      Real*8 pCellMix_diffKr(Nr)
      Real*8 tauCD, rCD, epsAB_CD
      Real*8 gravity,       recip_gravity
      Real*8 gBaro
      Real*8 gravFacC(Nr),   recip_gravFacC(Nr)
      Real*8 gravFacF(Nr+1), recip_gravFacF(Nr+1)
      Real*8 rhoNil
      Real*8 rhoConst,      recip_rhoConst
      Real*8 rho1Ref(Nr)
      Real*8 rhoFacC(Nr),   recip_rhoFacC(Nr)
      Real*8 rhoFacF(Nr+1), recip_rhoFacF(Nr+1)
      Real*8 rhoConstFresh
      Real*8 thetaConst
      Real*8 tRef(Nr)
      Real*8 sRef(Nr)
      Real*8 rhoRef(Nr)
      Real*8 dBdrRef(Nr)
      Real*8 surf_pRef, pRef4EOS(Nr)
      Real*8 phiRef(2*Nr+1)
      Real*8 rVel2wUnit(Nr+1), wUnit2rVel(Nr+1)
      Real*8 rUnit2z(Nr), z2rUnit(Nr)
      Real*8 mass2rUnit, rUnit2mass
      Real*8 baseTime
      Real*8 startTime
      Real*8 endTime
      Real*8 chkPtFreq
      Real*8 pChkPtFreq
      Real*8 dumpFreq
      Real*8 adjDumpFreq
      Real*8 diagFreq
      Real*8 monitorFreq
      Real*8 adjMonitorFreq
      Real*8 afFacMom
      Real*8 vfFacMom
      Real*8 pfFacMom
      Real*8 cfFacMom
      Real*8 foFacMom
      Real*8 mtFacMom
      Real*8 cosPower
      Real*8 cAdjFreq
      Real*8 tauThetaClimRelax
      Real*8 tauSaltClimRelax
      Real*8 latBandClimRelax
      Real*8 externForcingCycle
      Real*8 externForcingPeriod
      Real*8 convertFW2Salt
      Real*8 temp_EvPrRn
      Real*8 salt_EvPrRn
      Real*8 temp_addMass
      Real*8 salt_addMass
      Real*8 ivdc_kappa
      Real*8 hMixCriteria
      Real*8 dRhoSmall
      Real*8 hMixSmooth
      Real*8 sideDragFactor
      Real*8 bottomDragLinear
      Real*8 bottomDragQuadratic
      Real*8 zRoughBot
      Real*8 smoothAbsFuncRange
      Real*8 sIceLoadFac
      Real*8 nh_Am2
      Real*8 tCylIn, tCylOut
      Real*8 phiEuler, thetaEuler, psiEuler

C--   COMMON /PARM_A/ Thermodynamics constants ?
      COMMON /PARM_A/ HeatCapacity_Cp
      Real*8 HeatCapacity_Cp

C--   COMMON /PARM_ATM/ Atmospheric physical parameters (Ideal Gas EOS, ...)
C     celsius2K :: convert centigrade (Celsius) degree to Kelvin
C     atm_Po    :: standard reference pressure
C     atm_Cp    :: specific heat (Cp) of the (dry) air at constant pressure
C     atm_Rd    :: gas constant for dry air
C     atm_kappa :: kappa = R/Cp (R: constant of Ideal Gas EOS)
C     atm_Rq    :: water vapour specific volume anomaly relative to dry air
C                  (e.g. typical value = (29/18 -1) 10^-3 with q [g/kg])
C     integr_GeoPot :: option to select the way we integrate the geopotential
C                     (still a subject of discussions ...)
C     selectFindRoSurf :: select the way surf. ref. pressure (=Ro_surf) is
C             derived from the orography. Implemented: 0,1 (see INI_P_GROUND)
      COMMON /PARM_ATM/
     &            celsius2K,
     &            atm_Cp, atm_Rd, atm_kappa, atm_Rq, atm_Po,
     &            integr_GeoPot, selectFindRoSurf
      Real*8 celsius2K
      Real*8 atm_Po, atm_Cp, atm_Rd, atm_kappa, atm_Rq
      INTEGER integr_GeoPot, selectFindRoSurf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Logical flags for selecting packages
      LOGICAL useGAD
      LOGICAL useOBCS
      LOGICAL useSHAP_FILT
      LOGICAL useZONAL_FILT
      LOGICAL useOPPS
      LOGICAL usePP81
      LOGICAL useKL10
      LOGICAL useMY82
      LOGICAL useGGL90
      LOGICAL useKPP
      LOGICAL useGMRedi
      LOGICAL useDOWN_SLOPE
      LOGICAL useBBL
      LOGICAL useCAL
      LOGICAL useEXF
      LOGICAL useBulkForce
      LOGICAL useEBM
      LOGICAL useCheapAML
      LOGICAL useAUTODIFF
      LOGICAL useGrdchk
      LOGICAL useSMOOTH
      LOGICAL usePROFILES
      LOGICAL useECCO
      LOGICAL useCTRL
      LOGICAL useSBO
      LOGICAL useFLT
      LOGICAL usePTRACERS
      LOGICAL useGCHEM
      LOGICAL useRBCS
      LOGICAL useOffLine
      LOGICAL useMATRIX
      LOGICAL useFRAZIL
      LOGICAL useSEAICE
      LOGICAL useSALT_PLUME
      LOGICAL useShelfIce
      LOGICAL useSTIC
      LOGICAL useStreamIce
      LOGICAL useICEFRONT
      LOGICAL useThSIce
      LOGICAL useLand
      LOGICAL useATM2d
      LOGICAL useAIM
      LOGICAL useAtm_Phys
      LOGICAL useFizhi
      LOGICAL useGridAlt
      LOGICAL useDiagnostics
      LOGICAL useREGRID
      LOGICAL useLayers
      LOGICAL useMNC
      LOGICAL useRunClock
      LOGICAL useEMBED_FILES
      LOGICAL useMYPACKAGE
      COMMON /PARM_PACKAGES/
     &        useGAD, useOBCS, useSHAP_FILT, useZONAL_FILT,
     &        useOPPS, usePP81, useKL10, useMY82, useGGL90, useKPP,
     &        useGMRedi, useBBL, useDOWN_SLOPE,
     &        useCAL, useEXF, useBulkForce, useEBM, useCheapAML,
     &        useGrdchk, useSMOOTH, usePROFILES, useECCO, useCTRL,
     &        useSBO, useFLT, useAUTODIFF,
     &        usePTRACERS, useGCHEM, useRBCS, useOffLine, useMATRIX,
     &        useFRAZIL, useSEAICE, useSALT_PLUME, useShelfIce, useSTIC,
     &        useStreamIce, useICEFRONT, useThSIce, useLand,
     &        useATM2d, useAIM, useAtm_Phys, useFizhi, useGridAlt,
     &        useDiagnostics, useREGRID, useLayers, useMNC,
     &        useRunClock, useEMBED_FILES,
     &        useMYPACKAGE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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

C- need GRID.h to save, in rF(1), half retired param Ro_SeaLevel value:

C
CBOP
C    !ROUTINE: GRID.h
C    !INTERFACE:
C    include GRID.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | GRID.h
C     | o Header file defining model grid.
C     *==========================================================*
C     | Model grid is defined for each process by reference to
C     | the arrays set here.
C     | Notes
C     | =====
C     | The standard MITgcm convention of westmost, southern most
C     | and upper most having the (1,1,1) index is used here.
C     | i.e.
C     |----------------------------------------------------------
C     | (1)  Plan view schematic of model grid (top layer i.e. )
C     |      ================================= ( ocean surface )
C     |                                        ( or top of     )
C     |                                        ( atmosphere    )
C     |      This diagram shows the location of the model
C     |      prognostic variables on the model grid. The "T"
C     |      location is used for all tracers. The figure also
C     |      shows the southern most, western most indexing
C     |      convention that is used for all model variables.
C     |
C     |
C     |             V(i=1,                     V(i=Nx,
C     |               j=Ny+1,                    j=Ny+1,
C     |               k=1)                       k=1)
C     |                /|\                       /|\  "PWX"
C     |       |---------|------------------etc..  |---- *---
C     |       |                     |                   *  |
C     |"PWY"*******************************etc..  **********"PWY"
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |U(i=1, ==>       x           |             x     *==>U
C     |  j=Ny,|      T(i=1,         |          T(i=Nx,  *(i=Nx+1,
C     |  k=1) |        j=Ny,        |            j=Ny,  *  |j=Ny,
C     |       |        k=1)         |            k=1)   *  |k=1)
C     |
C     |       .                     .                      .
C     |       .                     .                      .
C     |       .                     .                      .
C     |       e                     e                   *  e
C     |       t                     t                   *  t
C     |       c                     c                   *  c
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |U(i=1, ==>       x           |             x     *  |
C     |  j=2, |      T(i=1,         |          T(i=Nx,  *  |
C     |  k=1) |        j=2,         |            j=2,   *  |
C     |       |        k=1)         |            k=1)   *  |
C     |       |                     |                   *  |
C     |       |        /|\          |            /|\    *  |
C     |      -----------|------------------etc..  |-----*---
C     |       |       V(i=1,        |           V(i=Nx, *  |
C     |       |         j=2,        |             j=2,  *  |
C     |       |         k=1)        |             k=1)  *  |
C     |       |                     |                   *  |
C     |U(i=1, ==>       x         ==>U(i=2,       x     *==>U
C     |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,
C     |  k=1) |        j=1,         |  k=1)      j=1,   *  |j=1,
C     |       |        k=1)         |            k=1)   *  |k=1)
C     |       |                     |                   *  |
C     |       |        /|\          |            /|\    *  |
C     |"SB"++>|---------|------------------etc..  |-----*---
C     |      /+\      V(i=1,                    V(i=Nx, *
C     |       +         j=1,                      j=1,  *
C     |       +         k=1)                      k=1)  *
C     |     "WB"                                      "PWX"
C     |
C     |   N, y increasing northwards
C     |  /|\ j increasing northwards
C     |   |
C     |   |
C     |   ======>E, x increasing eastwards
C     |             i increasing eastwards
C     |
C     |    i: East-west index
C     |    j: North-south index
C     |    k: up-down index
C     |    U: x-velocity (m/s)
C     |    V: y-velocity (m/s)
C     |    T: potential temperature (oC)
C     | "SB": Southern boundary
C     | "WB": Western boundary
C     |"PWX": Periodic wrap around in X.
C     |"PWY": Periodic wrap around in Y.
C     |----------------------------------------------------------
C     | (2) South elevation schematic of model grid
C     |     =======================================
C     |     This diagram shows the location of the model
C     |     prognostic variables on the model grid. The "T"
C     |     location is used for all tracers. The figure also
C     |     shows the upper most, western most indexing
C     |     convention that is used for all model variables.
C     |
C     |      "WB"
C     |       +
C     |       +
C     |      \+/       /|\                       /|\       .
C     |"UB"++>|-------- | -----------------etc..  | ----*---
C     |       |    rVel(i=1,        |        rVel(i=Nx, *  |
C     |       |         j=1,        |             j=1,  *  |
C     |       |         k=1)        |             k=1)  *  |
C     |       |                     |                   *  |
C     |U(i=1, ==>       x         ==>U(i=2,       x     *==>U
C     |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,
C     |  k=1) |        j=1,         |  k=1)      j=1,   *  |j=1,
C     |       |        k=1)         |            k=1)   *  |k=1)
C     |       |                     |                   *  |
C     |       |        /|\          |            /|\    *  |
C     |       |-------- | -----------------etc..  | ----*---
C     |       |    rVel(i=1,        |        rVel(i=Nx, *  |
C     |       |         j=1,        |             j=1,  *  |
C     |       |         k=2)        |             k=2)  *  |
C     |
C     |       .                     .                      .
C     |       .                     .                      .
C     |       .                     .                      .
C     |       e                     e                   *  e
C     |       t                     t                   *  t
C     |       c                     c                   *  c
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |       |                     |                   *  |
C     |       |        /|\          |            /|\    *  |
C     |       |-------- | -----------------etc..  | ----*---
C     |       |    rVel(i=1,        |        rVel(i=Nx, *  |
C     |       |         j=1,        |             j=1,  *  |
C     |       |         k=Nr)       |             k=Nr) *  |
C     |U(i=1, ==>       x         ==>U(i=2,       x     *==>U
C     |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,
C     |  k=Nr)|        j=1,         |  k=Nr)     j=1,   *  |j=1,
C     |       |        k=Nr)        |            k=Nr)  *  |k=Nr)
C     |       |                     |                   *  |
C     |"LB"++>==============================================
C     |                                               "PWX"
C     |
C     | Up   increasing upwards.
C     |/|\                                                       .
C     | |
C     | |
C     | =====> E  i increasing eastwards
C     | |         x increasing eastwards
C     | |
C     |\|/
C     | Down,k increasing downwards.
C     |
C     | Note: r => height (m) => r increases upwards
C     |       r => pressure (Pa) => r increases downwards
C     |
C     |
C     |    i: East-west index
C     |    j: North-south index
C     |    k: up-down index
C     |    U: x-velocity (m/s)
C     | rVel: z-velocity ( units of r )
C     |       The vertical velocity variable rVel is in units of
C     |       "r" the vertical coordinate. r in m will give
C     |       rVel m/s. r in Pa will give rVel Pa/s.
C     |    T: potential temperature (oC)
C     | "UB": Upper boundary.
C     | "LB": Lower boundary (always solid - therefore om|w == 0)
C     | "WB": Western boundary
C     |"PWX": Periodic wrap around in X.
C     |----------------------------------------------------------
C     | (3) Views showing nomenclature and indexing
C     |     for grid descriptor variables.
C     |
C     |      Fig 3a. shows the orientation, indexing and
C     |      notation for the grid spacing terms used internally
C     |      for the evaluation of gradient and averaging terms.
C     |      These varaibles are set based on the model input
C     |      parameters which define the model grid in terms of
C     |      spacing in X, Y and Z.
C     |
C     |      Fig 3b. shows the orientation, indexing and
C     |      notation for the variables that are used to define
C     |      the model grid. These varaibles are set directly
C     |      from the model input.
C     |
C     | Figure 3a
C     | =========
C     |       |------------------------------------
C     |       |                       |
C     |"PWY"********************************* etc...
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |
C     |       .                       .
C     |       .                       .
C     |       .                       .
C     |       e                       e
C     |       t                       t
C     |       c                       c
C     |       |-----------v-----------|-----------v----------|-
C     |       |                       |                      |
C     |       |                       |                      |
C     |       |                       |                      |
C     |       |                       |                      |
C     |       |                       |                      |
C     |       u<--dxF(i=1,j=2,k=1)--->u           t          |
C     |       |/|\       /|\          |                      |
C     |       | |         |           |                      |
C     |       | |         |           |                      |
C     |       | |         |           |                      |
C     |       |dyU(i=1,  dyC(i=1,     |                      |
C     | ---  ---|--j=2,---|--j=2,-----------------v----------|-
C     | /|\   | |  k=1)   |  k=1)     |          /|\         |
C     |  |    | |         |           |          dyF(i=2,    |
C     |  |    | |         |           |           |  j=1,    |
C     |dyG(   |\|/       \|/          |           |  k=1)    |
C     |   i=1,u---        t<---dxC(i=2,j=1,k=1)-->t          |
C     |   j=1,|                       |           |          |
C     |   k=1)|                       |           |          |
C     |  |    |                       |           |          |
C     |  |    |                       |           |          |
C     | \|/   |           |<---dxV(i=2,j=1,k=1)--\|/         |
C     |"SB"++>|___________v___________|___________v__________|_
C     |       <--dxG(i=1,j=1,k=1)----->
C     |      /+\                                              .
C     |       +
C     |       +
C     |     "WB"
C     |
C     |   N, y increasing northwards
C     |  /|\ j increasing northwards
C     |   |
C     |   |
C     |   ======>E, x increasing eastwards
C     |             i increasing eastwards
C     |
C     |    i: East-west index
C     |    j: North-south index
C     |    k: up-down index
C     |    u: x-velocity point
C     |    V: y-velocity point
C     |    t: tracer point
C     | "SB": Southern boundary
C     | "WB": Western boundary
C     |"PWX": Periodic wrap around in X.
C     |"PWY": Periodic wrap around in Y.
C     |
C     | Figure 3b
C     | =========
C     |
C     |       .                       .
C     |       .                       .
C     |       .                       .
C     |       e                       e
C     |       t                       t
C     |       c                       c
C     |       |-----------v-----------|-----------v--etc...
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       u<--delX(i=1)---------->u           t
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |                       |
C     |       |-----------v-----------------------v--etc...
C     |       |          /|\          |
C     |       |           |           |
C     |       |           |           |
C     |       |           |           |
C     |       u        delY(j=1)      |           t
C     |       |           |           |
C     |       |           |           |
C     |       |           |           |
C     |       |           |           |
C     |       |          \|/          |
C     |"SB"++>|___________v___________|___________v__etc...
C     |      /+\                                                 .
C     |       +
C     |       +
C     |     "WB"
C     |
C     *==========================================================*
C     \ev
CEOP

C     Macros that override/modify standard definitions

C
CBOP
C    !ROUTINE: GRID_MACROS.h
C    !INTERFACE:
C    include GRID_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | GRID_MACROS.h
C     *==========================================================*
C     | These macros are used to substitute definitions for
C     | GRID.h variables for particular configurations.
C     | In setting these variables the following convention
C     | applies.
C     | undef  phi_CONST   - Indicates the variable phi is fixed
C     |                      in X, Y and Z.
C     | undef  phi_FX      - Indicates the variable phi only
C     |                      varies in X (i.e.not in X or Z).
C     | undef  phi_FY      - Indicates the variable phi only
C     |                      varies in Y (i.e.not in X or Z).
C     | undef  phi_FXY     - Indicates the variable phi only
C     |                      varies in X and Y ( i.e. not Z).
C     *==========================================================*
C     \ev
CEOP


C
CBOP
C    !ROUTINE: DXC_MACROS.h
C    !INTERFACE:
C    include DXC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DXC_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DXF_MACROS.h
C    !INTERFACE:
C    include DXF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DXF_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DXG_MACROS.h
C    !INTERFACE:
C    include DXG_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DXG_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DXV_MACROS.h
C    !INTERFACE:
C    include DXV_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DXV_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DYC_MACROS.h
C    !INTERFACE:
C    include DYC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DYC_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DYF_MACROS.h
C    !INTERFACE:
C    include DYF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DYF_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DYG_MACROS.h
C    !INTERFACE:
C    include DYG_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DYG_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: DYU_MACROS.h
C    !INTERFACE:
C    include DYU_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | DYU_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: HFACC_MACROS.h
C    !INTERFACE:
C    include HFACC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | HFACC_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: HFACS_MACROS.h
C    !INTERFACE:
C    include HFACS_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | HFACS_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: HFACW_MACROS.h
C    !INTERFACE:
C    include HFACW_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | HFACW_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: RECIP_DXC_MACROS.h
C    !INTERFACE:
C    include RECIP_DXC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DXC_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DXF_MACROS.h
C    !INTERFACE:
C    include RECIP_DXF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DXF_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DXG_MACROS.h
C    !INTERFACE:
C    include RECIP_DXG_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DXG_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DXV_MACROS.h
C    !INTERFACE:
C    include RECIP_DXV_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DXV_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DYC_MACROS.h
C    !INTERFACE:
C    include RECIP_DYC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DYC_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DYF_MACROS.h
C    !INTERFACE:
C    include RECIP_DYF_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DYF_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DYG_MACROS.h
C    !INTERFACE:
C    include RECIP_DYG_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DYG_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_DYU_MACROS.h
C    !INTERFACE:
C    include RECIP_DYU_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_DYU_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RECIP_HFACC_MACROS.h
C    !INTERFACE:
C    include RECIP_HFACC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_HFACC_MACROS.h                                      
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: RECIP_HFACS_MACROS.h
C    !INTERFACE:
C    include RECIP_HFACS_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_HFACS_MACROS.h                                      
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: RECIP_HFACW_MACROS.h
C    !INTERFACE:
C    include RECIP_HFACW_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RECIP_HFACW_MACROS.h                                      
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP














C
CBOP
C    !ROUTINE: XC_MACROS.h
C    !INTERFACE:
C    include XC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | XC_MACROS.h                                               
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: YC_MACROS.h
C    !INTERFACE:
C    include YC_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | YC_MACROS.h                                               
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: RA_MACROS.h
C    !INTERFACE:
C    include RA_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RA_MACROS.h                                               
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP








C
CBOP
C    !ROUTINE: RAW_MACROS.h
C    !INTERFACE:
C    include RAW_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RAW_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP








C
CBOP
C    !ROUTINE: RAS_MACROS.h
C    !INTERFACE:
C    include RAS_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | RAS_MACROS.h                                              
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: MASKW_MACROS.h
C    !INTERFACE:
C    include MASKW_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | MASKW_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP












C
CBOP
C    !ROUTINE: MASKS_MACROS.h
C    !INTERFACE:
C    include MASKS_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | MASKS_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP












C
CBOP
C    !ROUTINE: TANPHIATU_MACROS.h
C    !INTERFACE:
C    include TANPHIATU_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | TANPHIATU_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: TANPHIATV_MACROS.h
C    !INTERFACE:
C    include TANPHIATV_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | TANPHIATV_MACROS.h                                        
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP










C
CBOP
C    !ROUTINE: FCORI_MACROS.h
C    !INTERFACE:
C    include FCORI_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | FCORI_MACROS.h                                            
C     *==========================================================*
C     | These macros are used to reduce memory requirement and/or 
C     | memory references when variables are fixed along a given  
C     | axis or axes.                                             
C     *==========================================================*
C     \ev
CEOP









C--   COMMON /GRID_RL/ RL valued grid defining variables.
C     deepFacC  :: deep-model grid factor (fct of vertical only) for dx,dy
C     deepFacF     at level-center (deepFacC)  and level interface (deepFacF)
C     deepFac2C :: deep-model grid factor (fct of vertical only) for area dx*dy
C     deepFac2F    at level-center (deepFac2C) and level interface (deepFac2F)
C     gravitySign :: indicates the direction of gravity relative to R direction
C                   (= -1 for R=Z (Z increases upward, -gravity direction  )
C                   (= +1 for R=P (P increases downward, +gravity direction)
C     rkSign     :: Vertical coordinate to vertical index orientation.
C                   ( +1 same orientation, -1 opposite orientation )
C     globalArea :: Domain Integrated horizontal Area [m2]
      COMMON /GRID_RL/
     &  cosFacU, cosFacV, sqCosFacU, sqCosFacV,
     &  deepFacC, deepFac2C, recip_deepFacC, recip_deepFac2C,
     &  deepFacF, deepFac2F, recip_deepFacF, recip_deepFac2F,
     &  gravitySign, rkSign, globalArea
      Real*8 cosFacU        (1-OLy:sNy+OLy,nSx,nSy)
      Real*8 cosFacV        (1-OLy:sNy+OLy,nSx,nSy)
      Real*8 sqCosFacU      (1-OLy:sNy+OLy,nSx,nSy)
      Real*8 sqCosFacV      (1-OLy:sNy+OLy,nSx,nSy)
      Real*8 deepFacC       (Nr)
      Real*8 deepFac2C      (Nr)
      Real*8 deepFacF       (Nr+1)
      Real*8 deepFac2F      (Nr+1)
      Real*8 recip_deepFacC (Nr)
      Real*8 recip_deepFac2C(Nr)
      Real*8 recip_deepFacF (Nr+1)
      Real*8 recip_deepFac2F(Nr+1)
      Real*8 gravitySign
      Real*8 rkSign
      Real*8 globalArea

C--   COMMON /GRID_RS/ RS valued grid defining variables.
C     dxC     :: Cell center separation in X across western cell wall (m)
C     dxG     :: Cell face separation in X along southern cell wall (m)
C     dxF     :: Cell face separation in X thru cell center (m)
C     dxV     :: V-point separation in X across south-west corner of cell (m)
C     dyC     :: Cell center separation in Y across southern cell wall (m)
C     dyG     :: Cell face separation in Y along western cell wall (m)
C     dyF     :: Cell face separation in Y thru cell center (m)
C     dyU     :: U-point separation in Y across south-west corner of cell (m)
C     drC     :: Cell center separation along Z axis ( units of r ).
C     drF     :: Cell face separation along Z axis ( units of r ).
C     R_low   :: base of fluid in r_unit (Depth(m) / Pressure(Pa) at top Atmos.)
C     rLowW   :: base of fluid column in r_unit at Western  edge location.
C     rLowS   :: base of fluid column in r_unit at Southern edge location.
C     Ro_surf :: surface reference (at rest) position, r_unit.
C     rSurfW  :: surface reference position at Western  edge location [r_unit].
C     rSurfS  :: surface reference position at Southern edge location [r_unit].
C     hFac    :: Fraction of cell in vertical which is open i.e how
C              "lopped" a cell is (dimensionless scale factor).
C              Note: The code needs terms like MIN(hFac,hFac(I-1))
C                    On some platforms it may be better to precompute
C                    hFacW, hFacS, ... here than do MIN on the fly.
C     maskInC :: Cell Center 2-D Interior mask (i.e., zero beyond OB)
C     maskInW :: West  face 2-D Interior mask (i.e., zero on and beyond OB)
C     maskInS :: South face 2-D Interior mask (i.e., zero on and beyond OB)
C     maskC   :: cell Center land mask
C     maskW   :: West face land mask
C     maskS   :: South face land mask
C     recip_dxC   :: Reciprocal of dxC
C     recip_dxG   :: Reciprocal of dxG
C     recip_dxF   :: Reciprocal of dxF
C     recip_dxV   :: Reciprocal of dxV
C     recip_dyC   :: Reciprocal of dxC
C     recip_dyG   :: Reciprocal of dyG
C     recip_dyF   :: Reciprocal of dyF
C     recip_dyU   :: Reciprocal of dyU
C     recip_drC   :: Reciprocal of drC
C     recip_drF   :: Reciprocal of drF
C     recip_Rcol  :: Inverse of cell center column thickness (1/r_unit)
C     recip_hFacC :: Inverse of cell open-depth f[X,Y,Z] ( dimensionless ).
C     recip_hFacW    rhFacC center, rhFacW west, rhFacS south.
C     recip_hFacS   Note: This is precomputed here because it involves division.
C     xC     :: X-coordinate of cell center f[X,Y]. The units of xc, yc
C               depend on the grid. They are not used in differencing or
C               averaging but are just a convient quantity for I/O,
C               diagnostics etc.. As such xc is in m for cartesian
C               coordinates but degrees for spherical polar.
C     yC     :: Y-coordinate of center of cell f[X,Y].
C     yG     :: Y-coordinate of corner of cell (c-grid vorticity point) f[X,Y].
C     rA     :: R-face are f[X,Y] ( m^2 ).
C               Note: In a cartesian framework rA is simply dx*dy,
C                   however we use rA to allow for non-globally
C                   orthogonal coordinate frames (with appropriate
C                   metric terms).
C     rC     :: R-coordinate of center of cell f[Z] (units of r).
C     rF     :: R-coordinate of face of cell f[Z] (units of r).
C - *HybSigm* - :: Hybrid-Sigma vert. Coord coefficients
C     aHybSigmF    at level-interface (*HybSigmF) and level-center (*HybSigmC)
C     aHybSigmC    aHybSigm* = constant r part, bHybSigm* = sigma part, such as
C     bHybSigmF    r(ij,k,t) = rLow(ij) + aHybSigm(k)*[rF(1)-rF(Nr+1)]
C     bHybSigmC              + bHybSigm(k)*[eta(ij,t)+Ro_surf(ij) - rLow(ij)]
C     dAHybSigF :: vertical increment of Hybrid-Sigma coeff.: constant r part,
C     dAHybSigC    between interface (dAHybSigF) and between center (dAHybSigC)
C     dBHybSigF :: vertical increment of Hybrid-Sigma coefficient: sigma part,
C     dBHybSigC    between interface (dBHybSigF) and between center (dBHybSigC)
C     tanPhiAtU :: tan of the latitude at U point. Used for spherical polar
C                  metric term in U equation.
C     tanPhiAtV :: tan of the latitude at V point. Used for spherical polar
C                  metric term in V equation.
C     angleCosC :: cosine of grid orientation angle relative to Geographic
C direction at cell center: alpha=(Eastward_dir,grid_uVel_dir)=(North_d,vVel_d)
C     angleSinC :: sine   of grid orientation angle relative to Geographic
C direction at cell center: alpha=(Eastward_dir,grid_uVel_dir)=(North_d,vVel_d)
C     u2zonDir  :: cosine of grid orientation angle at U point location
C     v2zonDir  :: minus sine of  orientation angle at V point location
C     fCori     :: Coriolis parameter at grid Center point
C     fCoriG    :: Coriolis parameter at grid Corner point
C     fCoriCos  :: Coriolis Cos(phi) parameter at grid Center point (for NH)

      COMMON /GRID_RS/
     &  dxC,dxF,dxG,dxV,dyC,dyF,dyG,dyU,
     &  rLowW, rLowS,
     &  Ro_surf, rSurfW, rSurfS,
     &  recip_dxC,recip_dxF,recip_dxG,recip_dxV,
     &  recip_dyC,recip_dyF,recip_dyG,recip_dyU,
     &  xC,yC,rA,rAw,rAs,rAz,xG,yG,
     &  maskInC, maskInW, maskInS,
     &  maskC, maskW, maskS,
     &  recip_rA,recip_rAw,recip_rAs,recip_rAz,
     &  drC, drF, recip_drC, recip_drF, rC, rF,
     &  aHybSigmF, bHybSigmF, aHybSigmC, bHybSigmC,
     &  dAHybSigF, dBHybSigF, dBHybSigC, dAHybSigC,
     &  tanPhiAtU, tanPhiAtV,
     &  angleCosC, angleSinC, u2zonDir, v2zonDir,
     &  fCori, fCoriG, fCoriCos
      Real*8 dxC            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dxF            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dxG            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dxV            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dyC            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dyF            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dyG            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dyU            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rLowW          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rLowS          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 Ro_surf        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rSurfW         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rSurfS         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dxC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dxF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dxG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dxV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dyC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dyF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dyG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_dyU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 xC             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 xG             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 yC             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 yG             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rA             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rAw            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rAs            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 rAz            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_rA       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_rAw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_rAs      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_rAz      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 maskInC        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 maskInW        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 maskInS        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 maskC          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 maskW          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 maskS          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 drC            (Nr+1)
      Real*8 drF            (Nr)
      Real*8 recip_drC      (Nr+1)
      Real*8 recip_drF      (Nr)
      Real*8 rC             (Nr)
      Real*8 rF             (Nr+1)
      Real*8 aHybSigmF      (Nr+1)
      Real*8 bHybSigmF      (Nr+1)
      Real*8 aHybSigmC      (Nr)
      Real*8 bHybSigmC      (Nr)
      Real*8 dAHybSigF      (Nr)
      Real*8 dBHybSigF      (Nr)
      Real*8 dBHybSigC      (Nr+1)
      Real*8 dAHybSigC      (Nr+1)
      Real*8 tanPhiAtU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 tanPhiAtV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 angleCosC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 angleSinC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 u2zonDir       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 v2zonDir       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 fCori          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 fCoriG         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 fCoriCos       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   COMMON /GRID_VAR_RS/ potentially time-dependent or active RS
C     valued grid defining variables. These grid defining variables are
C     time-dependent when using a non-linear free surface, or they are
C     active in an AD sense when using depth as a control parameter, or
C     both.
      COMMON /GRID_VAR_RS/
     &  hFacC, hFacW, hFacS,
     &  recip_hFacC,recip_hFacW,recip_hFacS,
     &  R_low, recip_Rcol
      Real*8 hFacC          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 hFacW          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 hFacS          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 recip_hFacC    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 recip_hFacW    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 recip_hFacS    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 R_low          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 recip_Rcol     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)



C--   COMMON /GRID_I/ INTEGER valued grid defining variables.
C     kSurfC  :: vertical index of the surface tracer cell
C     kSurfW  :: vertical index of the surface U point
C     kSurfS  :: vertical index of the surface V point
C     kLowC   :: index of the r-lowest "wet cell" (2D)
C IMPORTANT: kLowC = 0 and kSurfC,W,S = Nr+1 (or =Nr+2 on a thin-wall)
C            where the fluid column is empty (continent)
      COMMON /GRID_I/
     &  kSurfC, kSurfW, kSurfS,
     &  kLowC
      INTEGER kSurfC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER kSurfW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER kSurfS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER kLowC (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C

CBOP
C     !ROUTINE: SET_GRID.h
C     !INTERFACE:
C     #include SET_GRID.h

C     !DESCRIPTION:
C     Header file holding some 1-D arrays which are used to set-up the model grid.
C==========================================================================
C  IMPORTANT : This header file can only be included after the options
C              file PACKAGES_CONFIG.h and the header file W2_EXCH2_SIZE.h
C==========================================================================

CEOP

C    grid_maxNx :: Maximum length of delX vector
C    grid_maxNy :: Maximum length of delY vector
      INTEGER grid_maxNx, grid_maxNy

      PARAMETER( grid_maxNx = Nx )
      PARAMETER( grid_maxNy = Ny )


C--   COMMON /SET_GRID_R/ "Real" valued parameters used to set-up the model grid.
C     delX      :: Separation between cell faces (m) or (deg), depending on type
C     delY         type of horizontal grid choice (cartesian/spherical-polar ...)
      COMMON /SET_GRID_R/
     & delX, delY

      Real*8 delX(grid_maxNx)
      Real*8 delY(grid_maxNy)

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***


C !INPUT/OUTPUT PARAMETERS:
C   myThid :: Number of this instance of INI_PARMS
      INTEGER myThid

C !LOCAL VARIABLES:
C   dxSpacing, dySpacing :: Default spacing in X and Y.
C                        :: Units are that of coordinate system
C                        :: i.e. cartesian => metres ; s. polar => degrees
C   deltaTtracer  :: Timestep for tracer equations ( s )
C   forcing_In_AB :: flag to put all forcings (Temp,Salt,Tracers,Momentum)
C                 :: contribution in (or out of) Adams-Bashforth time stepping.
C   goptCount  :: Used to count the nuber of grid options (only one is allowed!)
C   msgBuf     :: Informational/error message buffer
C   errIO      :: IO error flag
C   errCount   :: error counter (inconsitent params and other errors)
C   iUnit      :: Work variable for IO unit number
C   k, i, j    :: Loop counters
C   xxxDefault :: Default value for variable xxx
      Real*8  dxSpacing
      Real*8  dySpacing
      Real*8 deltaTtracer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL forcing_In_AB
      INTEGER goptCount
      INTEGER gridNx, gridNy
      INTEGER k, i, j, iUnit
      INTEGER errIO, errCount
C   Default values for variables which have vertical coordinate system
C   dependency.
      Real*8 viscArDefault
      Real*8 diffKrTDefault
      Real*8 diffKrSDefault
      Real*8 hFacMinDrDefault
      Real*8 delRDefault(Nr)
C-- Variables used to select between different coordinate systems:
C   zCoordInputData :: The vertical coordinate system in the rest of the  model
C   pCoordInputData :: is written in terms of r. In the model "data" file,
C   rCoordInputData :: input params can be in terms of z, p or r.
C   coordsSet       :: e.g. delZ or delP or delR for the vertical grid spacing.
C                   :: The following rules apply:
C                   :: All parameters must use the same vertical coordinate
C                   :: system.  e.g. delZ and viscAz is legal
C                   ::           but delZ and viscAr is an error.
C                   :: Similarly specifying delZ and delP is an error.
C                   :: zCoord..., pCoord..., rCoord... are used to flag when
C                   :: z, p or r are used.
C                   :: coordsSet counts how many vertical coordinates have been
C                   :: used to specify variables. coordsSet > 1 is an error.
C   vertSetCount    :: to count number of vertical array elements which are set.
C   specifiedDiffKrT :: internal flag, true if any diffK[r,z,p,Nr]T is specified
C   specifiedDiffKrS :: internal flag, true if any diffK[r,z,p,Nr]S is specified

      LOGICAL zCoordInputData
      LOGICAL pCoordInputData
      LOGICAL rCoordInputData
      INTEGER coordsSet
      INTEGER vertSetCount
      LOGICAL specifiedDiffKrT, specifiedDiffKrS

C-- Variables which have vertical coordinate system dependency.
C   delZ    :: Vertical grid spacing ( m  ).
C   delP    :: Vertical grid spacing ( Pa ).
C   viscAz  :: Eddy viscosity coeff. for mixing of momentum vertically (m^2/s)
C   viscAp  :: Eddy viscosity coeff. for mixing of momentum vertically (Pa^2/s)
C   diffKzT :: Laplacian diffusion coeff. for mixing of heat vertically (m^2/s)
C   diffKpT :: Laplacian diffusion coeff. for mixing of heat vertically (Pa^2/s)
C   diffKzS :: Laplacian diffusion coeff. for mixing of salt vertically (m^2/s)
C   diffKpS :: Laplacian diffusion coeff. for mixing of salt vertically (Pa^2/s)
      Real*8 delZ(Nr)
      Real*8 delP(Nr)
      Real*8 viscAz
      Real*8 viscAp
      Real*8 viscAr
      Real*8 diffKzT
      Real*8 diffKpT
      Real*8 diffKrT
      Real*8 diffKzS
      Real*8 diffKpS
      Real*8 diffKrS

C-- Retired main data file parameters. Kept here to trap use of old data files.
C   nRetired :: Counter used to trap gracefully namelists containing "retired"
C            :: parameters. These are parameters that are either no-longer used
C            :: or that have moved to a different input file and/or namelist.
C Namelist PARM01:
C   useConstantF  :: Coriolis coeff set to f0     (replaced by selectCoriMap=0)
C   useBetaPlaneF :: Coriolis coeff = f0 + beta.y (replaced by selectCoriMap=1)
C   useSphereF    :: Coriolis = 2.omega.sin(phi)  (replaced by selectCoriMap=2)
C   useJamartWetPoints :: for backward compat. (replaced by selectCoriScheme=1)
C                      :: Use wet-point method for Coriolis (Jamart & Ozer 1986)
C   SadournyCoriolis :: for backward compat. (replaced by selectVortScheme=2)
C   use3dCoriolis :: for backward compat. (=F,T same as select3dCoriScheme=0,1)
C   metricTerms   :: for backward compat. (=F,T same as selectMetricTerms=0,1)
C   tracerAdvScheme :: tracer advection scheme (old passive tracer code)
C   trac_EvPrRn :: tracer conc. in Rain & Evap (old passive tracer code)
C   saltDiffusion :: diffusion of salinity    on/off (flag not used)
C   tempDiffusion :: diffusion of temperature on/off (flag not used)
C   zonal_filt_lat :: Moved to package "zonal_filt"
C   gravitySign  :: direction of gravity relative to R direction
C                :: (removed from namelist and set according to z/p coordinate)
C   viscAstrain  :: replaced by standard viscosity coeff & useStrainTensionVisc
C   viscAtension :: replaced by standard viscosity coeff & useStrainTensionVisc
C   useAnisotropicViscAgridMax :: Changed to be default behavior.  Can
C                   use old method by setting useAreaViscLength=.true.
C   usePickupBeforeC35 :: to restart from old-pickup files (generated with code
C                 from before checkpoint-35, Feb 08, 2001): disabled (Jan 2007)
C   debugMode    :: to print debug msg. now read from parameter file eedata
C   allowInteriorFreezing :: Allow water at depth to freeze
C                            and rise to the surface (replaced by pkg/frazil)
C   useOldFreezing :: use the old version (before checkpoint52a_pre, 2003-11-12)
C   balanceEmPmR   :: for backward compat. (replaced by selectBalanceEmPmR=1),
C                  :: substract global mean of EmPmR at every time step
C   writeStatePrec :: Precision used for writing model state (not used)
C Namelist PARM02:
C   cg2dChkResFreq :: Frequency with which to check 2-D con. grad solver
C                       residual (was never coded)
C   cg3dChkResFreq :: Frequency with which to check 3-D con. grad solver
C                       residual (was never coded)
C Namelist PARM03:
C   tauThetaClimRelax3Dim :: replaced by pkg/rbcs (3.D Relaxation B.Cs)
C   tauSaltClimRelax3Dim  :: replaced by pkg/rbcs (3.D Relaxation B.Cs)
C   taveFreq       :: Frequency output for time-averaged state vars ;
C                     disabled as pkg/timeave has been removed
C   tave_lastIter  :: last (within taveFreq period) time-step fraction in time-
C                     averaded output ; disabled as pkg/timeave has been removed
C   calendarDumps  :: moved to package "cal" (calendar)
C Namelist PARM04:
C   Ro_SeaLevel :: origin of the vertical R-coords axis ;
C               :: replaced by top_Pres or seaLev_Z setting
C   groundAtK1 :: put the surface(k=1) at the ground (replaced by usingPCoords)
C   rkFac      :: removed from namelist ; replaced by -rkSign
C   thetaMin   :: unfortunate variable name ; replaced by xgOrigin
C   phiMin     :: unfortunate variable name ; replaced by ygOrigin
C Namelist PARM05:
C   shelfIceFile :: File containing the topography of the shelfice draught
C                   (replaced by SHELFICEtopoFile in SHELFICE.h)
C   dQdTfile     :: File containing thermal relaxation coefficient
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      INTEGER nRetired
      LOGICAL useConstantF, useBetaPlaneF, useSphereF
      LOGICAL useJamartWetPoints, useEnergyConservingCoriolis
      LOGICAL SadournyCoriolis
      LOGICAL use3dCoriolis, metricTerms
      LOGICAL tempDiffusion, saltDiffusion
      INTEGER tracerAdvScheme
      Real*8 trac_EvPrRn
      Real*8 zonal_filt_lat
c     Real*8 gravitySign
      Real*8 viscAstrain, viscAtension
      LOGICAL useAnisotropicViscAgridMax
      LOGICAL usePickupBeforeC35
      LOGICAL saveDebugMode
      LOGICAL allowInteriorFreezing, useOldFreezing
      LOGICAL balanceEmPmR
      INTEGER writeStatePrec
C-
      INTEGER cg2dChkResFreq, cg3dChkResFreq
C-
      Real*8 tauThetaClimRelax3Dim, tauSaltClimRelax3Dim
      Real*8 taveFreq, tave_lastIter
      LOGICAL calendarDumps
C-
      LOGICAL groundAtK1
      Real*8 Ro_SeaLevel
      Real*8 rkFac
      Real*8 thetaMin, phiMin
C-
      CHARACTER*(MAX_LEN_FNAM) shelfIceFile
      CHARACTER*(MAX_LEN_FNAM) dQdTfile

C--   Continuous equation parameters
      NAMELIST /PARM01/
     & gravitySign, nh_Am2,
     & gravity, gBaro, gravityFile, rhonil, tAlpha, sBeta,
     & selectCoriMap, f0, beta, fPrime, omega, rotationPeriod,
     & viscAh, viscAhW, viscAhMax,
     & viscAhGrid, viscAhGridMax, viscAhGridMin,
     & viscC2leith, viscC4leith, smag3D_coeff, useSmag3D,
     & useFullLeith, useAnisotropicViscAgridMax, useStrainTensionVisc,
     & useAreaViscLength, viscC2leithD, viscC4leithD, viscC2LeithQG,
     & viscC2smag, viscC4smag, viscAhD, viscAhZ, viscA4D, viscA4Z,
     & viscA4, viscA4W,
     & viscA4Max, viscA4Grid, viscA4GridMax, viscA4GridMin,
     & viscA4ReMax, viscAhReMax,
     & cosPower, viscAstrain, viscAtension,
     & viscAz, diffKzT, diffKzS, viscAp, diffKpT, diffKpS,
     & viscAr, diffKrT, diffKrS, viscArNr, diffKrNrT, diffKrNrS,
     & diffKhT, diffK4T, diffKhS, diffK4S, smag3D_diffCoeff,
     & diffKr4T, diffKr4S, BL79LatVary,
     & diffKrBL79surf, diffKrBL79deep, diffKrBL79scl, diffKrBL79Ho,
     & diffKrBLEQsurf, diffKrBLEQdeep, diffKrBLEQscl, diffKrBLEQHo,
     & surf_pRef, tRef, sRef, tRefFile, sRefFile, rhoRefFile,
     & eosType, selectP_inEOS_Zc, integr_GeoPot, selectFindRoSurf,
     & HeatCapacity_Cp, celsius2K, atm_Cp, atm_Rd, atm_Rq, atm_Po,
     & no_slip_sides, sideDragFactor, no_slip_bottom, bottomVisc_pCell,
     & bottomDragLinear, bottomDragQuadratic,  zRoughBot,
     & selectBotDragQuadr,
     & momPressureForcing, momForcing, momTidalForcing,
     & momViscosity, momAdvection, vectorInvariantMomentum,
     & useConstantF, useBetaPlaneF, useSphereF, useCoriolis,
     & use3dCoriolis, select3dCoriScheme, selectCoriScheme,
     & useCDscheme, useJamartWetPoints, useEnergyConservingCoriolis,
     & useAbsVorticity, selectVortScheme, SadournyCoriolis,
     & useJamartMomAdv, upwindVorticity, highOrderVorticity,
     & upwindShear, selectKEscheme,
     & selectMetricTerms, metricTerms, useNHMTerms, addFrictionHeating,
     & tempDiffusion, tempAdvection, tempForcing, temp_stayPositive,
     & saltDiffusion, saltAdvection, saltForcing, salt_stayPositive,
     & implicSurfPress, implicDiv2DFlow, implicitNHPress,
     & implicitFreeSurface, rigidLid, freeSurfFac,
     & hFacMin, hFacMinDz, hFacMinDp, hFacMinDr,
     & exactConserv, linFSConserveTr, uniformLin_PhiSurf,
     & nonlinFreeSurf, hFacInf, hFacSup, select_rStar,
     & nonHydrostatic, selectNHfreeSurf, quasiHydrostatic,
     & implicitIntGravWave, staggerTimeStep, doResetHFactors,
     & tempStepping, saltStepping, momStepping,
     & implicitDiffusion, implicitViscosity, selectImplicitDrag,
     & tempImplVertAdv, saltImplVertAdv, momImplVertAdv,
     & rhoConst, thetaConst, rhoConstFresh, buoyancyRelation,
     & allowFreezing, allowInteriorFreezing, useOldFreezing, ivdc_kappa,
     & hMixCriteria, dRhoSmall, hMixSmooth,
     & tempAdvScheme, tempVertAdvScheme, tracerAdvScheme,
     & saltAdvScheme, saltVertAdvScheme, multiDimAdvection,
     & selectAddFluid, useRealFreshWaterFlux, convertFW2Salt,
     & temp_EvPrRn, salt_EvPrRn, trac_EvPrRn,
     & temp_addMass, salt_addMass, zonal_filt_lat,
     & smoothAbsFuncRange, sIceLoadFac, selectPenetratingSW,
     & selectBalanceEmPmR, balanceEmPmR, balanceQnet, balancePrintMean,
     & balanceThetaClimRelax, balanceSaltClimRelax,
     & readBinaryPrec, writeBinaryPrec, writeStatePrec, globalFiles,
     & useSingleCpuIO, useSingleCpuInput, usePickupBeforeC54,
     & usePickupBeforeC35, debugMode, debugLevel, plotLevel

C--   Elliptic solver parameters
      NAMELIST /PARM02/
     & cg2dMaxIters, cg2dMinItersNSA, cg2dChkResFreq, cg2dUseMinResSol,
     & cg2dTargetResidual, cg2dTargetResWunit,
     & cg2dpcOffDFac, cg2dPreCondFreq,
     & cg3dMaxIters, cg3dChkResFreq,
     & cg3dTargetResidual, cg3dTargetResWunit,
     & useNSACGSolver, useSRCGSolver, printResidualFreq

C--   Time stepping parammeters
      NAMELIST /PARM03/
     & nIter0, nTimeSteps, nTimeSteps_l2, nEndIter,
     & baseTime, startTime, endTime,
     & deltaT, deltaTClock, deltaTMom,
     & deltaTtracer, dTtracerLev, deltaTFreeSurf,
     & forcing_In_AB, momForcingOutAB, tracForcingOutAB,
     & momDissip_In_AB, doAB_onGtGs,
     & abEps, alph_AB, beta_AB, startFromPickupAB2, applyExchUV_early,
     & tauCD, rCD, epsAB_CD, cAdjFreq,
     & chkPtFreq, pChkPtFreq, pickupSuff, pickupStrictlyMatch,
     & writePickupAtEnd,
     & dumpFreq, dumpInitAndLast, adjDumpFreq, taveFreq, tave_lastIter,
     & diagFreq, monitorFreq, adjMonitorFreq, monitorSelect,
     & outputTypesInclusive, rwSuffixType,
     & tauThetaClimRelax, tauSaltClimRelax, latBandClimRelax,
     & tauThetaClimRelax3Dim, tauSaltClimRelax3Dim,
     & periodicExternalForcing, externForcingPeriod, externForcingCycle,
     & calendarDumps

C--   Gridding parameters
      NAMELIST /PARM04/
     & usingCartesianGrid, usingCylindricalGrid,
     & usingSphericalPolarGrid, usingCurvilinearGrid,
     & xgOrigin, ygOrigin, dxSpacing, dySpacing,
     & delX, delY, delXFile, delYFile, horizGridFile,
     & phiEuler, thetaEuler, psiEuler,
     & rSphere, radius_fromHorizGrid, deepAtmosphere, seaLev_Z,
     & top_Pres, delZ, delP, delR, delRc, delRFile, delRcFile,
     & useMin4hFacEdges, interViscAr_pCell, interDiffKr_pCell,
     & pCellMix_select, pCellMix_maxFac, pCellMix_delR,
     & pCellMix_viscAr, pCellMix_diffKr,
     & selectSigmaCoord, rSigmaBnd, hybSigmFile,
     & Ro_SeaLevel, rkFac, groundAtK1, thetaMin, phiMin

C--   Input files
      NAMELIST /PARM05/
     & bathyFile, topoFile, addWwallFile, addSwallFile, shelfIceFile,
     & diffKrFile, viscAhDfile, viscAhZfile, viscA4Dfile, viscA4Zfile,
     & hydrogThetaFile, hydrogSaltFile,
     & maskIniTemp, maskIniSalt, checkIniTemp, checkIniSalt,
     & zonalWindFile, meridWindFile,
     & thetaClimFile, saltClimFile,
     & surfQfile, surfQnetFile, surfQswFile, EmPmRfile, saltFluxFile,
     & uVelInitFile, vVelInitFile, pSurfInitFile,
     & dQdTFile, ploadFile, geoPotAnomFile, addMassFile,
     & tCylIn, tCylOut,
     & eddyPsiXFile, eddyPsiYFile, geothermalFile,
     & lambdaThetaFile, lambdaSaltFile, wghtBalanceFile,
     & mdsioLocalDir, adTapeDir,
     & the_run_name
CEOP


      gridNx = Nx
      gridNy = Ny


      IF ( myThid .EQ. 1 ) THEN

C     Defaults values for input parameters
      CALL SET_DEFAULTS(
     O   viscArDefault, diffKrTDefault, diffKrSDefault,
     O   hFacMinDrDefault, delRDefault,
     I   myThid )

      useJamartWetPoints = .FALSE.
      useEnergyConservingCoriolis = .FALSE.
      SadournyCoriolis = .FALSE.
      use3dCoriolis   = .TRUE.
      metricTerms     = .TRUE.
      balanceEmPmR    = .FALSE.

C--   Initialise "which vertical coordinate system used" flags.
      zCoordInputData = .FALSE.
      pCoordInputData = .FALSE.
      rCoordInputData = .FALSE.
      coordsSet       = 0

C--   Initialise retired parameters to unlikely value
      nRetired = 0
      useConstantF    = .FALSE.
      useBetaPlaneF   = .FALSE.
      useSphereF      = .TRUE.
      tempDiffusion   = .TRUE.
      saltDiffusion   = .TRUE.
      tracerAdvScheme = UNSET_I
      trac_EvPrRn     = UNSET_RL
      zonal_filt_lat  = UNSET_RL
      gravitySign     = UNSET_RL
      viscAstrain     = UNSET_RL
      viscAtension    = UNSET_RL
      useAnisotropicViscAgridMax=.TRUE.
      usePickupBeforeC35    = .FALSE.
      saveDebugMode   = debugMode
      allowInteriorFreezing = .FALSE.
      useOldFreezing  = .FALSE.
      writeStatePrec  = UNSET_I
      cg2dChkResFreq  = UNSET_I
      cg3dChkResFreq  = UNSET_I
      tauThetaClimRelax3Dim = UNSET_RL
      tauSaltClimRelax3Dim  = UNSET_RL
      taveFreq        = UNSET_RL
      tave_lastIter   = UNSET_RL
      calendarDumps   = .FALSE.
      Ro_SeaLevel     = UNSET_RL
      rkFac           = UNSET_RL
      groundAtK1      = .FALSE.
      thetaMin        = UNSET_RL
      phiMin          = UNSET_RL
      shelfIceFile    = ' '
      dQdTFile        = ' '

C--   Open the parameter file
      WRITE(msgBuf,'(A)')
     &     ' INI_PARMS: opening model parameter file "data"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE( 'data', 'INI_PARMS',
     O                          iUnit, myThid )

C--   Read settings from iUnit (= a copy of model parameter file "data").
      errIO = 0
      errCount = 0

C--   Set default "physical" parameters
      viscAhW  = UNSET_RL
      viscA4W  = UNSET_RL
      viscAhD  = UNSET_RL
      viscAhZ  = UNSET_RL
      viscA4D  = UNSET_RL
      viscA4Z  = UNSET_RL
      viscAz   = UNSET_RL
      viscAp   = UNSET_RL
      viscAr   = UNSET_RL
      diffKzT  = UNSET_RL
      diffKpT  = UNSET_RL
      diffKrT  = UNSET_RL
      diffKzS  = UNSET_RL
      diffKpS  = UNSET_RL
      diffKrS  = UNSET_RL
      hFacMinDr         = UNSET_RL
      hFacMinDz         = UNSET_RL
      hFacMinDp         = UNSET_RL
      tAlpha            = UNSET_RL
      sBeta             = UNSET_RL
      implicitNHPress   = UNSET_RL
      tempVertAdvScheme = 0
      saltVertAdvScheme = 0
      plotLevel         = UNSET_I
C--   z,p,r coord input switching.
      WRITE(msgBuf,'(A)') ' INI_PARMS ; starts to read PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM01) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading model parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS: Problem in namelist PARM01'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') ' INI_PARMS ; read PARM01 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

C-    set the type of vertical coordinate and type of fluid
C     according to buoyancyRelation
      usingPCoords    = .FALSE.
      usingZCoords    = .FALSE.
      fluidIsAir      = .FALSE.
      fluidIsWater    = .FALSE.
      IF ( buoyancyRelation.EQ.'ATMOSPHERIC' ) THEN
        usingPCoords = .TRUE.
        fluidIsAir   = .TRUE.
      ELSEIF ( buoyancyRelation.EQ.'OCEANICP') THEN
        usingPCoords  = .TRUE.
        fluidIsWater  = .TRUE.
      ELSEIF ( buoyancyRelation.EQ.'OCEANIC' ) THEN
        usingZCoords  = .TRUE.
        fluidIsWater  = .TRUE.
      ELSE
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS:',
     &      ' Bad value of buoyancyRelation '
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( .NOT.rigidLid .AND.
     &     .NOT.implicitFreeSurface ) THEN
C-     No barotropic solver selected => use implicitFreeSurface as default
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: No request for barotropic solver'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: => Use implicitFreeSurface as default'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
        implicitFreeSurface = .TRUE.
      ENDIF
      IF ( implicitFreeSurface ) freeSurfFac = 1.D0
      IF ( rigidLid            ) freeSurfFac = 0.D0
      IF ( gBaro .EQ. UNSET_RL ) gBaro=gravity
      IF ( rhoConst .EQ. UNSET_RL ) rhoConst=rhoNil
      IF ( rhoConstFresh .EQ. UNSET_RL ) rhoConstFresh=rhoConst
      IF ( implicitNHPress.EQ.UNSET_RL )
     &     implicitNHPress = implicSurfPress
      IF ( omega .EQ. UNSET_RL ) THEN
        omega = 0.D0
        IF ( rotationPeriod .NE. 0.D0 )
     &  omega = 2.D0 * PI / rotationPeriod
      ELSEIF ( omega .EQ. 0.D0 ) THEN
        rotationPeriod = 0.D0
      ELSE
        rotationPeriod = 2.D0 * PI / omega
      ENDIF
      IF ( atm_Rd .EQ. UNSET_RL ) THEN
        atm_Rd = atm_Cp * atm_kappa
      ELSE
        atm_kappa = atm_Rd / atm_Cp
      ENDIF
C--   Non-hydrostatic/quasi-hydrostatic
      IF (nonHydrostatic.AND.quasiHydrostatic) THEN
       WRITE(msgBuf,'(A)')
     &   'Illegal: both nonHydrostatic = quasiHydrostatic = TRUE'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
C--  Advection and Forcing for Temp and salt
      IF (tempVertAdvScheme.EQ.0) tempVertAdvScheme = tempAdvScheme
      IF (saltVertAdvScheme.EQ.0) saltVertAdvScheme = saltAdvScheme
C--  horizontal viscosity (acting on Divergence or Vorticity)
      IF ( viscAhD .EQ. UNSET_RL ) viscAhD = viscAh
      IF ( viscAhZ .EQ. UNSET_RL ) viscAhZ = viscAh
      IF ( viscA4D .EQ. UNSET_RL ) viscA4D = viscA4
      IF ( viscA4Z .EQ. UNSET_RL ) viscA4Z = viscA4
C--  horizontal viscosity for vertical momentum
      IF ( viscAhW .EQ. UNSET_RL ) viscAhW = viscAhD
      IF ( viscA4W .EQ. UNSET_RL ) viscA4W = viscA4D
C--   z,p,r coord input switching.
      IF ( viscAz .NE. UNSET_RL ) zCoordInputData = .TRUE.
      IF ( viscAp .NE. UNSET_RL ) pCoordInputData = .TRUE.
      IF ( viscAr .NE. UNSET_RL ) rCoordInputData = .TRUE.
      IF ( viscAr .EQ. UNSET_RL )          viscAr = viscAz
      IF ( viscAr .EQ. UNSET_RL )          viscAr = viscAp
      vertSetCount = 0
      DO k=1,Nr
        IF ( viscArNr(k).NE.UNSET_RL ) vertSetCount = vertSetCount + 1
      ENDDO
      IF ( vertSetCount.GT.0 .AND. vertSetCount.LT.Nr ) THEN
        WRITE(msgBuf,'(A,2(I5,A))') 'S/R INI_PARMS: Partial setting (',
     &        vertSetCount, ' /', Nr, ') of viscArNr is not allowed'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( viscAr .EQ. UNSET_RL ) THEN
        viscAr = viscArDefault
      ELSEIF ( vertSetCount.GT.0 ) THEN
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: Cannot set both ',
     &     'viscArNr and viscAr (or Ap,Az) in param file data'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( vertSetCount.EQ.0 ) THEN
        DO k=1,Nr
          viscArNr(k) = viscAr
        ENDDO
      ENDIF

C-    set default scheme for quadratic bottom-drag
      IF ( selectBotDragQuadr.EQ.-1 .AND. bottomDragQuadratic.NE.0. )
     &  selectBotDragQuadr = 0
      IF ( selectBotDragQuadr.EQ.-1 .AND. zRoughBot.NE.0. )
     &  selectBotDragQuadr = 0


      IF ( smag3D_diffCoeff.NE.zeroRL .AND. .NOT.useSmag3D ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &    'will not use "smag3D_diffCoeff" without useSmag3D'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &    '==> reset "smag3D_diffCoeff" to zero'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       smag3D_diffCoeff = zeroRL
      ENDIF
      IF ( diffKzT .NE. UNSET_RL ) zCoordInputData  = .TRUE.
      IF ( diffKpT .NE. UNSET_RL ) pCoordInputData  = .TRUE.
      IF ( diffKrT .NE. UNSET_RL ) rCoordInputData  = .TRUE.
      IF ( diffKrT .EQ. UNSET_RL )          diffKrT = diffKzT
      IF ( diffKrT .EQ. UNSET_RL )          diffKrT = diffKpT
      vertSetCount = 0
      DO k=1,Nr
        IF ( diffKrNrT(k).NE.UNSET_RL ) vertSetCount = vertSetCount + 1
      ENDDO
      IF ( vertSetCount.GT.0 .AND. vertSetCount.LT.Nr ) THEN
        WRITE(msgBuf,'(A,2(I5,A))') 'S/R INI_PARMS: Partial setting (',
     &        vertSetCount, ' /', Nr, ') of diffKrNrT is not allowed'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      specifiedDiffKrT = vertSetCount.EQ.Nr
      IF ( diffKrT .EQ. UNSET_RL ) THEN
        diffKrT = diffKrTDefault
      ELSEIF ( vertSetCount.GT.0 ) THEN
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: Cannot set both ',
     &     'diffKrNrT and diffKrT (or Kp,Kz) in param file data'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ELSE
        specifiedDiffKrT = .TRUE.
      ENDIF
      IF ( vertSetCount.EQ.0 ) THEN
        DO k=1,Nr
          diffKrNrT(k) = diffKrT
        ENDDO
      ENDIF

      IF ( diffKzS .NE. UNSET_RL ) zCoordInputData  = .TRUE.
      IF ( diffKpS .NE. UNSET_RL ) pCoordInputData  = .TRUE.
      IF ( diffKrS .NE. UNSET_RL ) rCoordInputData  = .TRUE.
      IF ( diffKrS .EQ. UNSET_RL )          diffKrS = diffKzS
      IF ( diffKrS .EQ. UNSET_RL )          diffKrS = diffKpS
      vertSetCount = 0
      DO k=1,Nr
        IF ( diffKrNrS(k).NE.UNSET_RL ) vertSetCount = vertSetCount + 1
      ENDDO
      IF ( vertSetCount.GT.0 .AND. vertSetCount.LT.Nr ) THEN
        WRITE(msgBuf,'(A,2(I5,A))') 'S/R INI_PARMS: Partial setting (',
     &        vertSetCount, ' /', Nr, ') of diffKrNrS is not allowed'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( vertSetCount.EQ.Nr ) THEN
        specifiedDiffKrS = .TRUE.
        IF ( diffKrS.NE.UNSET_RL ) THEN
         WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: Cannot set both ',
     &     'diffKrNrS and diffKrS (or Kp,Kz) in param file data'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
      ELSEIF ( diffKrS.NE.UNSET_RL ) THEN
        specifiedDiffKrS = .TRUE.
        DO k=1,Nr
          diffKrNrS(k) = diffKrS
        ENDDO
      ELSE
        specifiedDiffKrS = .FALSE.
        diffKrS = diffKrSDefault
C-    use temp diffusivity as default salt diffusivity:
        DO k=1,Nr
          diffKrNrS(k) = diffKrNrT(k)
        ENDDO
      ENDIF

      IF (diffKrBLEQsurf .EQ. UNSET_RL) diffKrBLEQsurf = diffKrBL79surf
      IF (diffKrBLEQdeep .EQ. UNSET_RL) diffKrBLEQdeep = diffKrBL79deep
      IF (diffKrBLEQscl  .EQ. UNSET_RL) diffKrBLEQscl  = diffKrBL79scl
      IF (diffKrBLEQHo   .EQ. UNSET_RL) diffKrBLEQHo   = diffKrBL79Ho

      IF ( hFacMinDz .NE. UNSET_RL ) zCoordInputData = .TRUE.
      IF ( hFacMinDp .NE. UNSET_RL ) pCoordInputData = .TRUE.
      IF ( hFacMinDr .NE. UNSET_RL ) rCoordInputData = .TRUE.
      IF ( hFacMinDr .EQ. UNSET_RL ) hFacMinDr       = hFacMinDz
      IF ( hFacMinDr .EQ. UNSET_RL ) hFacMinDr       = hFacMinDp
      IF ( hFacMinDr .EQ. UNSET_RL ) hFacMinDr       = hFacMinDrDefault

      IF (convertFW2Salt.EQ.UNSET_RL) THEN
        convertFW2Salt = 35.
        IF (useRealFreshWaterFlux) convertFW2Salt=-1
        IF ( selectAddFluid.GE.1 ) convertFW2Salt=-1
      ENDIF

C--   for backward compatibility :
      IF ( selectCoriScheme.EQ.UNSET_I ) THEN
        selectCoriScheme = 0
        IF ( useJamartWetPoints ) selectCoriScheme = 1
        IF ( useEnergyConservingCoriolis .AND.
     &       .NOT.vectorInvariantMomentum )
     &    selectCoriScheme = selectCoriScheme + 2
      ENDIF
      IF ( vectorInvariantMomentum ) THEN
        IF ( useJamartWetPoints .AND. selectCoriScheme.NE.1 ) THEN
          WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectCoriScheme=', selectCoriScheme,
     &     ' conflicts with "useJamartWetPoints"'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ELSE
        IF ( useEnergyConservingCoriolis
     &       .AND. selectCoriScheme.LT.2 ) THEN
          WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectCoriScheme=', selectCoriScheme,
     &     ' conflicts with "useEnergyConservingCoriolis"'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
        IF ( useJamartWetPoints .AND. selectCoriScheme.NE.1
     &                          .AND. selectCoriScheme.NE.3 ) THEN
          WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectCoriScheme=', selectCoriScheme,
     &     ' conflicts with "useJamartWetPoints"'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ENDIF
      IF ( SadournyCoriolis ) THEN
C--   for backward compatibility :
        IF ( selectVortScheme.EQ.UNSET_I ) selectVortScheme = 2
        IF ( selectVortScheme.NE.2 ) THEN
          WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectVortScheme=', selectVortScheme,
     &     ' conflicts with "SadournyCoriolis"'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ENDIF
      IF ( select3dCoriScheme.EQ.UNSET_I ) THEN
C--   for backward compatibility :
        select3dCoriScheme = 0
        IF ( use3dCoriolis ) select3dCoriScheme = 1
      ELSEIF ( select3dCoriScheme.NE.0 .AND. .NOT.use3dCoriolis ) THEN
        WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: select3dCoriScheme=', select3dCoriScheme,
     &     ' conflicts with "use3dCoriolis=F"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( selectMetricTerms.EQ.UNSET_I ) THEN
C--   for backward compatibility :
        selectMetricTerms = 0
        IF ( metricTerms ) selectMetricTerms = 1
      ELSEIF ( selectMetricTerms.NE.0 .AND. .NOT.metricTerms ) THEN
        WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectMetricTerms=', selectMetricTerms,
     &     ' conflicts with "metricTerms=F"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( selectBalanceEmPmR.EQ.UNSET_I ) THEN
        selectBalanceEmPmR = 0
        IF ( balanceEmPmR ) selectBalanceEmPmR = 1
      ELSEIF ( selectBalanceEmPmR.NE.1 .AND. balanceEmPmR ) THEN
        WRITE(msgBuf,'(A,I5,A)')
     &     'S/R INI_PARMS: selectBalanceEmPmR=', selectBalanceEmPmR,
     &     ' conflicts with "balanceEmPmR"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( ivdc_kappa.NE.zeroRL .AND. .NOT.implicitDiffusion ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: To use ivdc_kappa you must enable implicit',
     &  ' vertical diffusion.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

      coordsSet = 0
      IF ( zCoordInputData ) coordsSet = coordsSet + 1
      IF ( pCoordInputData ) coordsSet = coordsSet + 1
      IF ( rCoordInputData ) coordsSet = coordsSet + 1
      IF ( coordsSet .GT. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Cannot mix z, p and r in the input data.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( rhoConst .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: rhoConst must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
       recip_rhoConst = 1.D0
      ELSE
       recip_rhoConst = 1.D0 / rhoConst
      ENDIF
      IF ( eosType.EQ.'LINEAR' .AND. rhoNil.LE.0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: rhoNil must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( HeatCapacity_Cp .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: HeatCapacity_Cp must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( gravity .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: gravity must be greater than 0.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
       recip_gravity = 1.D0
      ELSE
       recip_gravity = 1.D0 / gravity
      ENDIF

C-    set default printResidualFreq according to debugLevel
      printResidualFreq = -1
      IF ( debugLevel.GE.debLevE ) printResidualFreq = 1
      IF ( plotLevel.EQ.UNSET_I ) plotLevel = debugLevel

C-    set useSingleCpuInput=.TRUE. if useSingleCpuIO==.TRUE.
      IF ( useSingleCpuIO ) useSingleCpuInput=.TRUE.

C     Check for retired parameters still being used
      nRetired = 0
      IF ( useConstantF ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "useConstantF" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: set "selectCoriMap"',
     &  ' [0,1,2,3] to impose a setting over grid default'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( useBetaPlaneF ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "useBetaPlaneF" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: set "selectCoriMap"',
     &  ' [0,1,2,3] to impose a setting over grid default'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( .NOT. useSphereF ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "useSphereF" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: set "selectCoriMap"',
     &  ' [0,1,2,3] to impose a setting over grid default'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( zonal_filt_lat .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: Paramater "zonal_filt_lat" is',
     &  ' no longer allowed in file "data".'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: Paramater "zonal_filt_lat" is',
     &  ' now read from file "data.zonfilt".'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( gravitySign .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "gravitySign" is set according to vertical ',
     &  ' coordinate and is no longer allowed in file "data".'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( tracerAdvScheme .NE. UNSET_I ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "tracerAdvScheme" ',
     &  '(old passive tracer code) is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( trac_EvPrRn .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "trac_EvPrRn" ',
     &  '(old passive tracer code) is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( .NOT. tempDiffusion ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "tempDiffusion" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: to turn off diffusion',
     &  ' => set diffusivity to zero'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( .NOT. saltDiffusion ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "saltDiffusion" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: to turn off diffusion',
     &  ' => set diffusivity to zero'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( viscAstrain .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "viscAstrain" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: to use Strain & Tension',
     &  ' formulation => set useStrainTensionVisc to TRUE'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( viscAtension .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "viscAtension" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: to use Strain & Tension',
     &  ' formulation => set useStrainTensionVisc to TRUE'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( .NOT.useAnisotropicViscAgridMax ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "useAnisotropicViscAgridMax" ',
     &  'is not allowed in "data" substitute useAreaViscLength=true'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( usePickupBeforeC35 ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "usePickupBeforeC35" ',
     &  'is no longer supported & not longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( debugMode.NEQV.saveDebugMode ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "debugMode" has been moved to "eedata"',
     &  ' and is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( allowInteriorFreezing ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "allowInteriorFreezing" has been replaced',
     &  ' by pkg/frazil and is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( useOldFreezing ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "useOldFreezing" ',
     &  'is no longer supported & not longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( writeStatePrec .NE. UNSET_I ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "writeStatePrec" ',
     &  '(un-used) is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C--   Elliptic solver parameters
      WRITE(msgBuf,'(A)') ' INI_PARMS ; starts to read PARM02'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM02) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading model parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS: Problem in namelist PARM02'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') ' INI_PARMS ; read PARM02 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
C     Check for retired parameters still being used
      IF ( cg2dChkResFreq .NE. UNSET_I ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: unused "cg2dChkResFreq"',
     &  ' is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( cg3dChkResFreq .NE. UNSET_I ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: unused "cg3dChkResFreq"',
     &  ' is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C--   Time stepping parameters
      rCD               = -1.D0
      epsAB_CD          = UNSET_RL
      latBandClimRelax  = UNSET_RL
      deltaTtracer      = 0.D0
      forcing_In_AB     = .TRUE.
      WRITE(msgBuf,'(A)') ' INI_PARMS ; starts to read PARM03'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM03) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading model parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS: Problem in namelist PARM03'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') ' INI_PARMS ; read PARM03 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
C     Check for retired parameters still being used
      IF ( tauThetaClimRelax3Dim .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "tauThetaClimRelax3Dim" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: 3-dim. relaxation code',
     & ' has moved to separate pkg/rbcs.'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( tauSaltClimRelax3Dim .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "tauSaltClimRelax3Dim" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: 3-dim. relaxation code',
     & ' has moved to separate pkg/rbcs.'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( taveFreq .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "taveFreq" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( tave_lastIter .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "tave_lastIter" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( taveFreq.NE.UNSET_RL .OR. tave_lastIter.NE.UNSET_RL ) THEN
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: ',
     & ' since "pkg/timeave" has been removed.'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( calendarDumps ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: "calendarDumps" ',
     &  'is no longer allowed in file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R INI_PARMS: calendarDumps',
     & ' has moved to "data.cal"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C     Process "timestepping" params
C     o Time step size
      IF ( deltaTtracer .NE. dTtracerLev(1) .AND.
     &     deltaTtracer .NE. 0. .AND. dTtracerLev(1) .NE. 0. ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: deltaTtracer & dTtracerLev(1) not equal'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ELSEIF ( dTtracerLev(1) .NE. 0. ) THEN
        deltaTtracer = dTtracerLev(1)
      ENDIF
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTClock
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTtracer
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTMom
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTFreeSurf
      IF ( deltaT       .EQ. 0. ) THEN
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: ',
     &   'need to specify in file "data", namelist "PARM03"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: ',
     &   ' a model timestep (in s) deltaT or deltaTClock= ?'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
        deltaT = 1.
      ENDIF
      IF ( deltaTMom    .EQ. 0. ) deltaTMom    = deltaT
      IF ( deltaTtracer .EQ. 0. ) deltaTtracer = deltaT
      IF ( deltaTClock  .EQ. 0. ) deltaTClock  = deltaT
      DO k=1,Nr
       IF (dTtracerLev(k).EQ.0.) dTtracerLev(k)= deltaTtracer
      ENDDO
C Note that this line should set deltaFreesurf=deltaTtracer
C but this would change a lot of existing set-ups so we are
C obliged to set the default inappropriately.
C Be advised that when using asynchronous time stepping
C it is better to set deltaTreesurf=deltaTtracer
      IF ( deltaTFreeSurf .EQ. 0. ) deltaTFreeSurf  = deltaTMom
      IF ( periodicExternalForcing ) THEN
       IF ( externForcingCycle*externForcingPeriod .EQ. 0. ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle,externForcingPeriod =0'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ELSEIF ( INT(externForcingCycle/externForcingPeriod) .NE.
     &              externForcingCycle/externForcingPeriod ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle <> N*externForcingPeriod'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ELSEIF ( externForcingCycle.LT.externForcingPeriod ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle < externForcingPeriod'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( externForcingPeriod.LT.deltaTClock ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingPeriod < deltaTClock'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
C     o Adams-Bashforth time stepping:
      IF ( momForcingOutAB  .EQ. UNSET_I ) THEN
        momForcingOutAB  = 1
        IF ( forcing_In_AB ) momForcingOutAB  = 0
      ENDIF
      IF ( tracForcingOutAB .EQ. UNSET_I ) THEN
        tracForcingOutAB = 1
        IF ( forcing_In_AB ) tracForcingOutAB = 0
      ENDIF
C     o Convection frequency
      IF ( cAdjFreq .LT. 0. ) THEN
       cAdjFreq = deltaTClock
      ENDIF
      IF ( ivdc_kappa .NE. 0. .AND. cAdjFreq .NE. 0. ) THEN
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: You have enabled both ivdc_kappa and',
     &  ' convective adjustment.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF (useCDscheme) THEN
C     o CD coupling (CD scheme):
        IF ( tauCD .EQ. 0.D0 ) tauCD = deltaTMom
        IF ( rCD .LT. 0. ) rCD = 1.D0 - deltaTMom/tauCD
        IF ( epsAB_CD .EQ. UNSET_RL ) epsAB_CD = abEps
      ENDIF

      IF ( startTime.EQ.UNSET_RL .AND. nIter0.EQ.-1 ) THEN
C     o set default value for start time & nIter0
        startTime = baseTime
        nIter0 = 0
      ELSEIF ( startTime.EQ.UNSET_RL ) THEN
C     o set start time from nIter0
         startTime = baseTime + deltaTClock*DFLOAT(nIter0)
      ELSEIF ( nIter0.EQ.-1 ) THEN
C     o set nIter0 from start time
         nIter0 = NINT( (startTime-baseTime)/deltaTClock )
      ELSEIF ( baseTime.EQ.0. ) THEN
C     o set base time from the 2 others
         baseTime = startTime - deltaTClock*DFLOAT(nIter0)
      ENDIF

      nTimeSteps_l2 = 4
C     o nTimeSteps 1
      IF ( nTimeSteps .EQ. 0 .AND. nEndIter .NE. 0 )
     &     nTimeSteps = nEndIter-nIter0
C     o nTimeSteps 2
      IF ( nTimeSteps .EQ. 0 .AND. endTime .NE. 0. )
     &     nTimeSteps = NINT((endTime-startTime)/deltaTClock)
C     o nEndIter 1
      IF ( nEndIter .EQ. 0 .AND. nTimeSteps .NE. 0 )
     &     nEndIter = nIter0+nTimeSteps
C     o nEndIter 2
      IF ( nEndIter .EQ. 0 .AND. endTime .NE. 0. )
     &     nEndIter = NINT((endTime-baseTime)/deltaTClock)
C     o End Time 1
      IF ( endTime .EQ. 0. .AND. nTimeSteps .NE. 0 )
     &     endTime = startTime + deltaTClock*DFLOAT(nTimeSteps)
C     o End Time 2
      IF ( endTime .EQ. 0. .AND. nEndIter .NE. 0 )
     &     endTime = baseTime + deltaTClock*DFLOAT(nEndIter)

C     o Consistent?
      IF ( startTime .NE. baseTime+deltaTClock*DFLOAT(nIter0) ) THEN
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: startTime, baseTime and nIter0 are inconsistent'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: Perhaps more than two were set at once'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( nEndIter .NE. nIter0+nTimeSteps ) THEN
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: nIter0, nTimeSteps and nEndIter are inconsistent'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: Perhaps more than two were set at once'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( nTimeSteps .NE. NINT((endTime-startTime)/deltaTClock)
     &   ) THEN
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: both endTime and nTimeSteps have been set'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     & 'S/R INI_PARMS: but are inconsistent'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

C     o Monitor (should also add CPP flag for monitor?)
      IF (monitorFreq.LT.0.) THEN
       monitorFreq=0.
       IF (dumpFreq.NE.0.) monitorFreq=dumpFreq
       IF (diagFreq.NE.0..AND.diagFreq.LT.monitorFreq)
     &         monitorFreq=diagFreq
       IF (chkPtFreq.NE.0..AND.chkPtFreq.LT.monitorFreq)
     &         monitorFreq=chkPtFreq
       IF (pChkPtFreq.NE.0..AND.pChkPtFreq.LT.monitorFreq)
     &         monitorFreq=pChkPtFreq
       IF (monitorFreq.EQ.0.) monitorFreq=deltaTClock
      ENDIF
      IF ( monitorSelect.EQ.UNSET_I ) THEN
        monitorSelect = 2
        IF ( fluidIsWater ) monitorSelect = 3
      ENDIF

C--   Grid parameters
C     In cartesian coords distances are in metres
      DO k =1,Nr
       delZ(k) = UNSET_RL
       delP(k) = UNSET_RL
       delR(k) = UNSET_RL
      ENDDO
C     In spherical polar distances are in degrees
      dxSpacing = UNSET_RL
      dySpacing = UNSET_RL
C-    pCell-Mix  parameters:
      interViscAr_pCell = .FALSE.
      interDiffKr_pCell = .FALSE.
      pCellMix_select = 0
      pCellMix_maxFac = 1.D4
      pCellMix_delR = 0.
      DO k=1,Nr
        pCellMix_viscAr(k) = viscArNr(k)
        pCellMix_diffKr(k) = diffKrNrT(k)
      ENDDO

      WRITE(msgBuf,'(A)') ' INI_PARMS ; starts to read PARM04'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM04) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading model parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS: Problem in namelist PARM04'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') ' INI_PARMS ; read PARM04 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

C     Check for retired parameters still being used
      IF ( Ro_SeaLevel .NE. UNSET_RL ) THEN
c      nRetired = nRetired+1
       IF ( usingPCoords ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &   '"Ro_SeaLevel" (P @ bottom) depreciated (backward compat'
       ELSEIF ( usingZCoords ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &   '"Ro_SeaLevel" (Z @ top) depreciated (backward compat'
       ENDIF
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       IF ( usingPCoords ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &   ' only). To set vert. axis, use instead "top_Pres".'
       ELSEIF ( usingZCoords ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_PARMS: ',
     &   ' only). To set vert. axis, use instead "seaLev_Z".'
       ENDIF
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( rkFac .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "rkFac" has been replaced by -rkSign ',
     &  ' and is no longer allowed in file "data".'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( groundAtK1 ) THEN
c      nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "groundAtK1" is set according to vertical ',
     &  ' coordinate and is no longer allowed in file "data".'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( thetaMin .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "thetaMin" no longer allowed,',
     &  ' has been replaced by "xgOrigin"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( phiMin .NE. UNSET_RL ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "phiMin" no longer allowed,',
     &  ' has been replaced by "ygOrigin"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C     X coordinate : Check for multiple definitions
      goptCount = 0
      IF ( delX(1)   .NE. UNSET_RL ) goptCount = goptCount + 1
      IF ( dxSpacing .NE. UNSET_RL ) goptCount = goptCount + 1
      IF ( delXFile  .NE. ' ' )      goptCount = goptCount + 1
      IF ( goptCount.GT.1 ) THEN
       WRITE(msgBuf,'(A,A)') 'Too many specifications for delX:',
     &   'Specify only one of delX, dxSpacing or delXfile'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( dxSpacing .NE. UNSET_RL ) THEN
       DO i=1,gridNx
        delX(i) = dxSpacing
       ENDDO
      ENDIF
C     Y coordinate : Check for multiple definitions
      goptCount = 0
      IF ( delY(1)   .NE. UNSET_RL ) goptCount = goptCount + 1
      IF ( dySpacing .NE. UNSET_RL ) goptCount = goptCount + 1
      IF ( delYFile  .NE. ' ' )      goptCount = goptCount + 1
      IF ( goptCount.GT.1 ) THEN
       WRITE(msgBuf,'(A,A)') 'Too many specifications for delY:',
     &   'Specify only one of delY, dySpacing or delYfile'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( dySpacing .NE. UNSET_RL ) THEN
       DO j=1,gridNy
        delY(j) = dySpacing
       ENDDO
      ENDIF

C--   Check for conflicting grid definitions.
      goptCount = 0
      IF ( usingCartesianGrid )      goptCount = goptCount+1
      IF ( usingSphericalPolarGrid ) goptCount = goptCount+1
      IF ( usingCurvilinearGrid )    goptCount = goptCount+1
      IF ( usingCylindricalGrid )    goptCount = goptCount+1
      IF ( goptCount .GT. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: More than one coordinate system requested'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( goptCount .LT. 1 ) THEN
C-     No horizontal grid is specified => use Cartesian grid as default:
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: No horizontal grid requested'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: => Use Cartesian Grid as default'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       usingCartesianGrid = .TRUE.
      ENDIF
C-    Radius of the Planet:
      IF ( rSphere.EQ.UNSET_RL ) THEN
        IF ( usingCurvilinearGrid .AND.
     &       radius_fromHorizGrid.NE.UNSET_RL ) THEN
           rSphere = radius_fromHorizGrid
        ELSE
           rSphere = 6370.D3
        ENDIF
      ENDIF
      IF ( radius_fromHorizGrid.EQ.UNSET_RL ) THEN
           radius_fromHorizGrid = rSphere
      ENDIF
      IF ( rSphere .NE. 0. ) THEN
       recip_rSphere = 1.D0/rSphere
      ELSE
       recip_rSphere = 0.
      ENDIF
C--   Grid rotation
      IF ( phiEuler .NE. 0.D0 .OR. thetaEuler .NE. 0.D0
     &     .OR. psiEuler .NE. 0.D0 ) rotateGrid = .TRUE.

C--   Default vertical axis origin
      IF ( Ro_SeaLevel .NE. UNSET_RL ) THEN
       IF ( usingPCoords .AND. top_Pres.NE.UNSET_RL ) THEN
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: ',
     &       'Cannot set both "Ro_SeaLevel" and "top_Pres"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( usingZCoords .AND. seaLev_Z.NE.UNSET_RL ) THEN
        WRITE(msgBuf,'(2A)') 'S/R INI_PARMS: ',
     &       'Cannot set both "Ro_SeaLevel" and "seaLev_Z"'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       rF(1) = Ro_SeaLevel
      ELSE
       rF(1) = UNSET_RS
      ENDIF
      IF ( top_Pres.EQ.UNSET_RL ) top_Pres = 0.
      IF ( seaLev_Z.EQ.UNSET_RL ) seaLev_Z = 0.
C--   Default origin for  X & Y axis (longitude & latitude origin):
      IF ( xgOrigin .EQ. UNSET_RL ) xgOrigin = 0.
      IF ( ygOrigin .EQ. UNSET_RL ) THEN
        IF ( usingSphericalPolarGrid )  THEN
          ygOrigin = 0.
        ELSEIF ( usingCartesianGrid )   THEN
          ygOrigin = 0.
        ELSEIF ( usingCylindricalGrid ) THEN
          ygOrigin = 0.
        ELSEIF ( usingCurvilinearGrid ) THEN
          ygOrigin = 0.
        ELSE
          WRITE(msgBuf,'(A)')
     &    'S/R INI_PARMS: found no coordinate system to set ygOrigin'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ENDIF

C--   set cell Center depth and put Interface at the middle between 2 C
      setCenterDr = .FALSE.
      DO k=1,Nr+1
       IF ( delRc(k).EQ.UNSET_RL ) THEN
        IF ( setCenterDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delRc at k =', k
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ELSE
        IF ( k.EQ.1 ) setCenterDr = .TRUE.
        IF ( .NOT.setCenterDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delRc at k <', k
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ENDIF
      ENDDO
      IF ( setCenterDr ) rCoordInputData = .TRUE.
C--   p, z, r coord parameters
      setInterFDr = .FALSE.
      DO k = 1, Nr
       IF ( delZ(k) .NE. UNSET_RL ) zCoordInputData = .TRUE.
       IF ( delP(k) .NE. UNSET_RL ) pCoordInputData = .TRUE.
       IF ( delR(k) .NE. UNSET_RL ) rCoordInputData = .TRUE.
       IF ( delR(k) .EQ. UNSET_RL ) delR(k) = delZ(k)
       IF ( delR(k) .EQ. UNSET_RL ) delR(k) = delP(k)
       IF ( delR(k) .EQ. UNSET_RL ) THEN
        IF ( setInterFDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delZ/delP/delR at k =', k
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ELSE
        IF ( k.EQ.1 ) setInterFDr = .TRUE.
        IF ( .NOT.setInterFDr ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'S/R INI_PARMS: No value for delZ/delP/delR at k <', k
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ENDIF
      ENDDO
C     Check for multiple coordinate systems
      coordsSet = 0
      IF ( zCoordInputData ) coordsSet = coordsSet + 1
      IF ( pCoordInputData ) coordsSet = coordsSet + 1
      IF ( rCoordInputData ) coordsSet = coordsSet + 1
      IF ( coordsSet .GT. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Cannot mix z, p and r in the input data.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
C-    Check for double definition (file & namelist)
      IF ( delRcFile.NE.' ' ) THEN
        IF ( setCenterDr ) THEN
          WRITE(msgBuf,'(A)')
     &         'S/R INI_PARMS: Cannot set both delRc and delRcFile'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
        setCenterDr = .TRUE.
      ENDIF
      IF ( delRFile.NE.' ' ) THEN
        IF ( setInterFDr ) THEN
          WRITE(msgBuf,'(A)')
     &         'S/R INI_PARMS: Cannot set both delR and delRFile'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
        setInterFDr = .TRUE.
      ENDIF

C--   Input files
      WRITE(msgBuf,'(A)') ' INI_PARMS ; starts to read PARM05'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=PARM05) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading model parameter file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'S/R INI_PARMS: Problem in namelist PARM05'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       WRITE(msgBuf,'(A)') ' INI_PARMS ; read PARM05 : OK'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
C     Check for retired parameters still being used
      IF ( shelfIceFile .NE. ' ' ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "shelfIceFile" is not allowed in "data", ',
     &  'substitute "SHELFICEtopoFile" in data.shelfice'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( dQdTFile .NE. ' ' ) THEN
       nRetired = nRetired+1
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_PARMS: "dQdTFile" has been retired from file "data"'
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
C     Check if two conflicting I/O directories have been specified
      IF (mdsioLocalDir .NE. ' ' .AND. adTapeDir .NE. ' ') THEN
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: mdsioLocalDir and adTapeDir cannot be'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: specified at the same time'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

C     Check for relaxation term:
       IF ( (tauThetaClimRelax.GT.0.).AND.
     &      (thetaClimFile.EQ.' ') ) THEN
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: tauThetaClimRelax > 0 but'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: thetaClimFile is undefined'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
       ENDIF
       IF ( (tauSaltClimRelax.GT.0.).AND.
     &      (saltClimFile.EQ.' ') ) THEN
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: tauSaltClimRelax > 0 but'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: saltClimFile is undefined'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
       ENDIF
C     Check vertical diffusivity setting:


C--   Set Units conversion factor required to incorporate
C     surface forcing into z-p isomorphic equations:
C     mass2rUnit: from mass per unit area [kg/m2] to r-coordinate
C     rUnit2mass: from r-coordinate to mass per unit area [kg/m2]
      IF ( usingPCoords ) THEN
         mass2rUnit = gravity
         rUnit2mass = recip_gravity
      ELSE
         mass2rUnit = recip_rhoConst
         rUnit2mass = rhoConst
      ENDIF

C--   For backward compatibility, set temp_addMass and salt_addMass
C     to temp_EvPrRn and salt_EvPrRn if not set in parameter file "data"
      IF (temp_addMass .EQ. UNSET_RL) temp_addMass = temp_EvPrRn
      IF (salt_addMass .EQ. UNSET_RL) salt_addMass = salt_EvPrRn

C--   Make a choice (if unset) regarding using CG2D solver min.-residual sol.
C      for simple set-up (cartesian grid + flat bottom), default is to
C      use the solver minimum residual solution (cg2dUseMinResSol=1).
      IF ( cg2dUseMinResSol.EQ.UNSET_I ) THEN
        cg2dUseMinResSol = 0
        IF ( topoFile.EQ.' ' .AND. bathyFile.EQ.' '
     &       .AND. usingCartesianGrid ) cg2dUseMinResSol = 1
      ENDIF

C--   Close the open data file

      CLOSE(iUnit,STATUS='DELETE')


      WRITE(msgBuf,'(A)') ' INI_PARMS: finished reading file "data"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid )

C--   Check whether any retired parameters were found.
      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Error reading parameter file "data":'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(I4,A)') nRetired,
     &      ' out of date parameters were found in the namelist'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
C--   Stop if any error was found (including retired params):
      IF ( errCount .GE. 1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'S/R INI_PARMS: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF

      ENDIF

C--   Everyone else must wait for the parameters to be loaded
      CALL BARRIER(myThid)

      RETURN
      END

