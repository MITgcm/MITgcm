CBOP
C !ROUTINE: GAD_OPTIONS.h

C !INTERFACE:
C #include "GAD_OPTIONS.h"

C !DESCRIPTION:
C Contains CPP macros/flags for controlling optional features of package.
CEOP

C CPP options file for GAD (Generic Advection Diffusion) package
C Use this file for selecting options within the GAD package




  








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




C     Package-specific Options & Macros go here

C This flag selects the form of COSINE(lat) scaling of horizontal
C bi-harmonic diffusivity -- only on lat-lon grid.
C Setting this flag here only affects tracer diffusivity; to use it
C in the momentum equations it needs to be set in MOM_COMMON_OPTIONS.h


C This selects isotropic scaling of horizontal harmonic and bi-harmonic
C diffusivity when using the COSINE(lat) scaling -- only on lat-lon grid.
C Setting this flag here only affects tracer diffusivity; to use it
C in the momentum equations it needs to be set in MOM_COMMON_OPTIONS.h


C As of checkpoint41, the inclusion of multi-dimensional advection
C introduces excessive recomputation/storage for the adjoint.
C We can disable it here using CPP because run-time flags are insufficient.


C Use compressible flow method for multi-dim advection instead of old, less
C accurate jmc method. Note: option has no effect on SOM advection which
C always use compressible flow method.


C This enable the use of 2nd-Order Moment advection scheme (Prather, 1986) for
C Temperature and Salinity ; due to large memory space (10 times more / tracer)
C requirement, by default, this part of the code is not compiled.


C Hack to get rid of negatives caused by Redi.  Works by restricting the
C outgoing flux (only contributions computed in gad_calc_rhs) for each cell
C to be no more than the amount of tracer in the cell (see Smolarkiewicz
C MWR 1989 and Bott MWR 1989).
C The flux contributions computed in gad_calc_rhs which are affected by
C this hack are:
C - explicit diffusion, Redi and the non-local part of KPP
C - advection is affected only if multiDimAdvection=.FALSE.
C - vertical diffusion (including the diagonal contribution from GMRedi)
C   only if implicitDiffusion=.FALSE.
C - GM is affected only if GMREDI_AdvForm=.FALSE.
C
C The parameter SmolarkiewiczMaxFrac (defined in gad_init_fixed.F)
C specifies the maximal fraction of tracer that can leave a cell.
C By default it is 1.  This will prevent the tracer from going negative
C due to contributions from gad_calc_rhs alone.  In the presence of other
C contributions (or roundoff errors), it may be necessary to reduce this
C value to achieve strict positivity.
C
C This hack applies to all tracers except temperature and salinity!
C Do not use with Adams-Bashforth (for ptracers)!
C Do not use with OBCS!




CBOP
C     !ROUTINE: GAD_SOM_PREP_CS_CORNER
C     !INTERFACE:
      SUBROUTINE GAD_SOM_PREP_CS_CORNER(
     U           smVol, smTr0, smTr, smCorners,
     I           prep4dirX, overlapOnly, interiorOnly,
     I           N_edge, S_edge, E_edge, W_edge,
     I           iPass, k, myNz, bi, bj, myThid )


C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GAD_SOM_PREP_CS_CORNER
C     | o Prepare for Horizontal SOM Advection :
C     |   when using Cubed-Sphere Grid, fill corner-halo regions
C     |   of all Tracer-moments with proper values
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===

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
C !ROUTINE: GAD.h

C !INTERFACE:
C #include "GAD.h"

C !DESCRIPTION:
C Contains enumerated constants for distinguishing between different
C advection schemes and tracers.
C
C Unfortunately, there is no easy way to make use of the
C tokens in namelist input so for now we have to enter the
C tokens value into "data" (ie. 2 for 2nd order etc.)

C !USES:

C !DEFINED PARAMETERS:

C ENUM_UPWIND_1RST :: 1rst Order Upwind
      INTEGER ENUM_UPWIND_1RST
      PARAMETER(ENUM_UPWIND_1RST=1)

C ENUM_CENTERED_2ND :: Centered 2nd order
      INTEGER ENUM_CENTERED_2ND
      PARAMETER(ENUM_CENTERED_2ND=2)

C ENUM_UPWIND_3RD :: 3rd order upwind
      INTEGER ENUM_UPWIND_3RD
      PARAMETER(ENUM_UPWIND_3RD=3)

C ENUM_CENTERED_4TH :: Centered 4th order
      INTEGER ENUM_CENTERED_4TH
      PARAMETER(ENUM_CENTERED_4TH=4)

C ENUM_DST2 :: 2nd Order Direct Space and Time (= Lax-Wendroff)
      INTEGER ENUM_DST2
      PARAMETER(ENUM_DST2=20)

C ENUM_FLUX_LIMIT :: Non-linear flux limiter
      INTEGER ENUM_FLUX_LIMIT
      PARAMETER(ENUM_FLUX_LIMIT=77)

C ENUM_DST3 :: 3rd Order Direct Space and Time
      INTEGER ENUM_DST3
      PARAMETER(ENUM_DST3=30)

C ENUM_DST3_FLUX_LIMIT :: 3-DST flux limited
      INTEGER ENUM_DST3_FLUX_LIMIT
      PARAMETER(ENUM_DST3_FLUX_LIMIT=33)

C ENUM_OS7MP :: 7th Order One Step method with Monotonicity Preserving Limiter
      INTEGER ENUM_OS7MP
      PARAMETER(ENUM_OS7MP=7)

C ENUM_SOM_PRATHER :: 2nd Order-Moment Advection Scheme, Prather, 1986
      INTEGER ENUM_SOM_PRATHER
      PARAMETER(ENUM_SOM_PRATHER=80)

C ENUM_SOM_LIMITER :: 2nd Order-Moment Advection Scheme, Prather Limiter
      INTEGER ENUM_SOM_LIMITER
      PARAMETER(ENUM_SOM_LIMITER=81)

C ENUM_PPM_NULL :: piecewise parabolic method with "null" limiter
      INTEGER ENUM_PPM_NULL_LIMIT
      PARAMETER(ENUM_PPM_NULL_LIMIT=40)

C ENUM_PPM_MONO :: piecewise parabolic method with "mono" limiter
      INTEGER ENUM_PPM_MONO_LIMIT
      PARAMETER(ENUM_PPM_MONO_LIMIT=41)

C ENUM_PPM_WENO :: piecewise parabolic method with "weno" limiter
      INTEGER ENUM_PPM_WENO_LIMIT
      PARAMETER(ENUM_PPM_WENO_LIMIT=42)

C ENUM_PQM_NULL :: piecewise quartic method with "null" limiter
      INTEGER ENUM_PQM_NULL_LIMIT
      PARAMETER(ENUM_PQM_NULL_LIMIT=50)

C ENUM_PQM_MONO :: piecewise quartic method with "mono" limiter
      INTEGER ENUM_PQM_MONO_LIMIT
      PARAMETER(ENUM_PQM_MONO_LIMIT=51)

C ENUM_PQM_WENO :: piecewise quartic method with "weno" limiter
      INTEGER ENUM_PQM_WENO_LIMIT
      PARAMETER(ENUM_PQM_WENO_LIMIT=52)

C GAD_Scheme_MaxNum :: maximum possible number for an advection scheme
      INTEGER GAD_Scheme_MaxNum
      PARAMETER( GAD_Scheme_MaxNum = 100 )

C nSOM :: number of 1rst & 2nd Order-Moments: 1+1 (1D), 2+3 (2D), 3+6 (3D)
      INTEGER nSOM
      PARAMETER( nSOM = 3+6 )

C oneSixth :: Third/fourth order interpolation factor
      Real*8 oneSixth
      PARAMETER(oneSixth=1.D0/6.D0)

C loop range for computing vertical advection tendency
C  iMinAdvR,iMaxAdvR  :: 1rst index (X-dir) loop range for vertical advection
C  jMinAdvR,jMaxAdvR  :: 2nd  index (Y-dir) loop range for vertical advection
      INTEGER iMinAdvR, iMaxAdvR, jMinAdvR, jMaxAdvR
c     PARAMETER ( iMinAdvR = 1-OLx , iMaxAdvR = sNx+OLx )
c     PARAMETER ( jMinAdvR = 1-OLy , jMaxAdvR = sNy+OLy )
C- note: we use to compute vertical advection tracer tendency everywhere
C        (overlap included) as above, but really needs valid tracer tendency
C        in interior only (as below):
      PARAMETER ( iMinAdvR = 1 , iMaxAdvR = sNx )
      PARAMETER ( jMinAdvR = 1 , jMaxAdvR = sNy )

C Differentiate between tracers (needed for KPP - arrgh!!!)
cph                              and GMRedi arrgh*arrgh!!!)
cph  indices are used for TAF key computations, so need to
cph  running from 1, 2, ...
c
C GAD_TEMPERATURE :: temperature
      INTEGER GAD_TEMPERATURE
      PARAMETER(GAD_TEMPERATURE=1)
C GAD_SALINITY :: salinity
      INTEGER GAD_SALINITY
      PARAMETER(GAD_SALINITY=2)
C GAD_TR1 :: passive tracer 1
      INTEGER GAD_TR1
      PARAMETER(GAD_TR1=3)
CEOP

C--   COMMON /GAD_PARM_C/ Character parameters for GAD pkg routines
C      somSfx       :: 1rst & 2nd Order moment suffix
      CHARACTER*2 somSfx(nSOM)
      COMMON /GAD_PARM_C/
     & somSfx

C--   COMMON /GAD_PARM_I/ Integer parameters for GAD pkg routines
C GAD_OlMinSize     :: overlap minimum size for GAD routines
C           1: min required; 2: to add to current min; 3: factor to apply
      INTEGER GAD_OlMinSize(3)
      COMMON /GAD_PARM_I/
     &        GAD_OlMinSize

C--   COMMON /GAD_PARM_L/ Logical parameters for GAD pkg routines
C tempSOM_Advection :: set to T if using 2nd-Order Moment advection for Temp
C saltSOM_Advection :: set to T if using 2nd-Order Moment advection for Salt
C tempMultiDimAdvec :: set to T if using multi-dim advection for Temp
C saltMultiDimAdvec :: set to T if using multi-dim advection for Salt
C AdamsBashforthGt  :: apply Adams-Bashforth extrapolation on T tendency (=Gt)
C AdamsBashforthGs  :: apply Adams-Bashforth extrapolation on S tendency (=Gs)
C AdamsBashforth_T  :: apply Adams-Bashforth extrapolation on Pot.Temp.
C AdamsBashforth_S  :: apply Adams-Bashforth extrapolation on Salinity
      LOGICAL tempSOM_Advection
      LOGICAL saltSOM_Advection
      LOGICAL tempMultiDimAdvec
      LOGICAL saltMultiDimAdvec
      LOGICAL AdamsBashforthGt
      LOGICAL AdamsBashforthGs
      LOGICAL AdamsBashforth_T
      LOGICAL AdamsBashforth_S
      COMMON /GAD_PARM_L/
     & tempSOM_Advection, saltSOM_Advection,
     & tempMultiDimAdvec, saltMultiDimAdvec,
     & AdamsBashforthGt, AdamsBashforthGs,
     & AdamsBashforth_T, AdamsBashforth_S

      Real*8 SmolarkiewiczMaxFrac
      COMMON /GAD_SMOL/ SmolarkiewiczMaxFrac

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***


C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     smVol      :: grid-cell volume
C     smTr0      :: tracer Zero Order moment
C     smTr       :: tracer 1rst & 2nd Order moments
C     smCorners  :: Temporary storage of Corner-halo-regions values
C                   ( 3rd dim = Number of corners = 4 : SW, SE, NE, NW )
C     prep4dirX  :: True = prepare for X direction advection
C                   otherwise, prepare for Y direction advection.
C  overlapOnly   :: only update the edges of myTile, but not the interior
C  interiorOnly  :: only update the interior of myTile, but not the edges
C [N,S,E,W]_edge :: true if N,S,E,W edge of myTile is an Edge of the cube
C     iPass      :: current passage index in SOM_ADVECT
C     k          :: current level index
C     myNz       :: 3rd dimension of array to exchange
C     bi,bj      :: current tile indices
C     myThid     :: my Thread Id number
      INTEGER myNz
      Real*8     smVol(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz)
      Real*8     smTr0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz)
      Real*8     smTr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz,nSx,nSy,nSOM)
      Real*8     smCorners(OLx,OLy,4,-1:nSOM)
      LOGICAL prep4dirX, overlapOnly, interiorOnly
      LOGICAL N_edge, S_edge, E_edge, W_edge
      INTEGER iPass, k, bi, bj
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j, jPass, n
      LOGICAL southWestCorner
      LOGICAL southEastCorner
      LOGICAL northEastCorner
      LOGICAL northWestCorner

      southWestCorner = S_edge .AND. W_edge
      southEastCorner = S_edge .AND. E_edge
      northEastCorner = N_edge .AND. E_edge
      northWestCorner = N_edge .AND. W_edge

      IF ( overlapOnly ) THEN
C--   to avoid repeating 2 times the full sequence of FILL_CS_CORNER calls,
C     add a loop on jPass (from iPass to 2) to reproduce the following logic:
C     1 ) overlapOnly & iPass = 1  (face 3 & 6)
C         - fill corners for the other direction
C         - then store the corner values
C     2 ) IF overlapOnly           :: fill corners for the current direction
C         ELSEIF .NOT.interiorOnly :: get the corner values back from storage
C         ENDIF
       DO jPass = iPass,2

        IF ( ( jPass.EQ.2 .AND. prep4dirX ) .OR.
     &       ( jPass.EQ.1 .AND. .NOT.prep4dirX ) ) THEN
C--   Fill corners to prepare for calculations in X
          CALL GAD_SOM_FILL_CS_CORNER( .TRUE.,
     U                                 smVol(1-OLx,1-OLy,k),
     U                                 smTr0(1-OLx,1-OLy,k),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,1),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,2),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,3),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,4),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,5),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,6),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,7),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,8),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,9),
     I                                 bi, bj, myThid )
C--   End of filling for X dir
c       ENDIF

        ELSE
C Note: the 2 IF tests are equivalent to just 1 if/else test;
C       use this later option and leave the former commented.

c       IF ( ( jPass.EQ.1 .AND. prep4dirX ) .OR.
c    &       ( jPass.EQ.2 .AND. .NOT.prep4dirX ) ) THEN
C--   Fill corners to prepare for calculations in Y
          CALL GAD_SOM_FILL_CS_CORNER( .FALSE.,
     U                                 smVol(1-OLx,1-OLy,k),
     U                                 smTr0(1-OLx,1-OLy,k),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,1),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,2),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,3),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,4),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,5),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,6),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,7),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,8),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,9),
     I                                 bi, bj, myThid )
C--   End of filling for Y dir
        ENDIF

        IF ( jPass.EQ.1 ) THEN
C--   Store corner values (to be used on the next iPass)
         IF ( southWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smCorners(i,j,1,-1) = smVol(i-OLx,j-OLy,k)
            smCorners(i,j,1, 0) = smTr0(i-OLx,j-OLy,k)
            DO n=1,nSOM
             smCorners(i,j,1,n) = smTr (i-OLx,j-OLy,k,bi,bj,n)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         IF ( southEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smCorners(i,j,2,-1) = smVol(sNx+i,j-OLy,k)
            smCorners(i,j,2, 0) = smTr0(sNx+i,j-OLy,k)
            DO n=1,nSOM
             smCorners(i,j,2,n) = smTr (sNx+i,j-OLy,k,bi,bj,n)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         IF ( northEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smCorners(i,j,3,-1) = smVol(sNx+i,sNy+j,k)
            smCorners(i,j,3, 0) = smTr0(sNx+i,sNy+j,k)
            DO n=1,nSOM
             smCorners(i,j,3,n) = smTr (sNx+i,sNy+j,k,bi,bj,n)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         IF ( northWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smCorners(i,j,4,-1) = smVol(i-OLx,sNy+j,k)
            smCorners(i,j,4, 0) = smTr0(i-OLx,sNy+j,k)
            DO n=1,nSOM
             smCorners(i,j,4,n) = smTr (i-OLx,sNy+j,k,bi,bj,n)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
C--   End storing block
        ENDIF

C--   End of loop on jPass
       ENDDO

      ELSEIF ( .NOT.interiorOnly ) THEN

C--   Get back corner values from storage
        IF ( southWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smVol(i-OLx,j-OLy,k )        = smCorners(i,j,1,-1)
            smTr0(i-OLx,j-OLy,k )        = smCorners(i,j,1, 0)
            DO n=1,nSOM
             smTr(i-OLx,j-OLy,k,bi,bj,n) = smCorners(i,j,1, n)
            ENDDO
           ENDDO
          ENDDO
        ENDIF
        IF ( southEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smVol(sNx+i,j-OLy,k )        = smCorners(i,j,2,-1)
            smTr0(sNx+i,j-OLy,k )        = smCorners(i,j,2, 0)
            DO n=1,nSOM
             smTr(sNx+i,j-OLy,k,bi,bj,n) = smCorners(i,j,2, n)
            ENDDO
           ENDDO
          ENDDO
        ENDIF
        IF ( northEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smVol(sNx+i,sNy+j,k )        = smCorners(i,j,3,-1)
            smTr0(sNx+i,sNy+j,k )        = smCorners(i,j,3, 0)
            DO n=1,nSOM
             smTr(sNx+i,sNy+j,k,bi,bj,n) = smCorners(i,j,3, n)
            ENDDO
           ENDDO
          ENDDO
        ENDIF
        IF ( northWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smVol(i-OLx,sNy+j,k )        = smCorners(i,j,4,-1)
            smTr0(i-OLx,sNy+j,k )        = smCorners(i,j,4, 0)
            DO n=1,nSOM
             smTr(i-OLx,sNy+j,k,bi,bj,n) = smCorners(i,j,4, n)
            ENDDO
           ENDDO
          ENDDO
        ENDIF
C--   End getting back corner values from storage

C---  End if/else - overlapOnly - block
      ENDIF

      RETURN
      END

