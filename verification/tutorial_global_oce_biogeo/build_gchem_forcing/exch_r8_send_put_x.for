
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






CBOP

C     !ROUTINE: EXCH_R8_SEND_PUT_X

C     !INTERFACE:
      SUBROUTINE EXCH_R8_SEND_PUT_X( array,
     I             myOLw, myOLe, myOLs, myOLn, myNz,
     I             exchWidthX, exchWidthY,
     I             thesimulationMode, thecornerMode, myThid )
      IMPLICIT NONE
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EXCH_R8_SEND_PUT_X
C     | o "Send" or "put" X edges for R8 array.
C     *==========================================================*
C     | Routine that invokes actual message passing send or
C     | direct "put" of data to update X faces of an XY[R] array.
C     *==========================================================*

C     !USES:
C     == Global variables ==

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
C     !ROUTINE: EESUPPORT.h
C     !INTERFACE:
C     include "EESUPPORT.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | EESUPPORT.h                                              |
C     *==========================================================*
C     | Support data structures for the MITgcm UV                |
C     | "execution environment" code. This data should be        |
C     | private to the execution environment routines. Data      |
C     | which needs to be accessed directly by a numerical model |
C     | goes in EEPARAMS.h.                                      |
C     *==========================================================*
CEOP

C     ERROR_HEADER        - String which prefixes error messages
      CHARACTER*(*) ERROR_HEADER
      PARAMETER ( ERROR_HEADER = ' *** ERROR ***' )
C     PROCESS_HEADER      - String which prefixes processor number
      CHARACTER*(*) PROCESS_HEADER
      PARAMETER ( PROCESS_HEADER = 'PID.TID' )

C     MAX_NUM_COMM_MODES - Maximum number of communication modes
C     COMM_NONE       - No edge communication
C     COMM_MSG        - Use messages to communicate edges
C     COMM_PUT        - Use put to communicate edges
C     COMM_GET        - Use get to communicate edges
C     Note - commName holds an identifying name for each communication
C            mode. The COMM_ parameters are used to index commName
C            so the COMM_ parameters need to be in the range
C            1 : MAX_NUM_COMM_MODES.
      INTEGER MAX_NUM_COMM_MODES
      PARAMETER ( MAX_NUM_COMM_MODES = 4 )
      INTEGER COMM_NONE
      PARAMETER ( COMM_NONE   =   1 )
      INTEGER COMM_MSG
      PARAMETER ( COMM_MSG    =   2 )
      INTEGER COMM_PUT
      PARAMETER ( COMM_PUT    =   3 )
      INTEGER COMM_GET
      PARAMETER ( COMM_GET    =   4 )
      COMMON /EESUPP_COMMNAME/ commName
      CHARACTER*10 commName(MAX_NUM_COMM_MODES)

C     Tile identifiers
C     Tiles have a number that is unique over the global domain.
C     A tile that is not there has its number set to NULL_TILE
      INTEGER NULL_TILE
      PARAMETER ( NULL_TILE = -1 )


C--   COMMON /EESUPP_C/ Execution environment support character variables
C     myProcessStr - String identifying my process number
      COMMON /EESUPP_C/ myProcessStr
      CHARACTER*128 myProcessStr

C--   COMMON /EESUPP_L/ Execution environment support logical variables
C     initMPError - Flag indicating error during multi-processing
C                   initialisation.
C     finMPError  - Flag indicating error during multi-processing
C                   termination.
C     ThError     - Thread detected an error.
C     usingMPI    - Flag controlling use of MPI routines. This flag
C                   allows either MPI or threads to be used in a
C                   shared memory environment which can be a useful
C                   debugging/performance analysis tool.
C     usingSyncMessages - Flag that causes blocking communication to be used
C                         if possible. When false non-blocking EXCH routines
C                         will be used if possible.
C     notUsingXPeriodicity - Flag indicating no X/Y boundary wrap around
C     notUsingYPeriodicity   This affects the communication routines but
C                            is generally ignored in the numerical model
C                            code.
C     threadIsRunning, threadIsComplete - Flags used to check for correct behaviour
C                                         of multi-threaded code.
C                                         threadIsRunning is used to check that the
C                                         threads we need are running. This catches the
C                                         situation where a program eedata file has nTthreads
C                                         greater than the setenv PARALLEL or NCPUS variable.
C                                         threadIsComplete is used to flag that a thread has
C                                         reached the end of the model. This is used as a check to
C                                         trap problems that might occur if one thread "escapes"
C                                         the main.F master loop. This should not happen
C                                         if the multi-threading compilation tools works right.
C                                         But (see for example KAP) this is not always the case!
      COMMON /EESUPP_L/ thError, threadIsRunning, threadIsComplete,
     & allMyEdgesAreSharedMemory, usingMPI, usingSyncMessages,
     & notUsingXPeriodicity, notUsingYPeriodicity
      LOGICAL thError(MAX_NO_THREADS)
      LOGICAL threadIsRunning(MAX_NO_THREADS)
      LOGICAL threadIsComplete(MAX_NO_THREADS)
      LOGICAL allMyEdgesAreSharedMemory(MAX_NO_THREADS)
      LOGICAL usingMPI
      LOGICAL usingSyncMessages
      LOGICAL notUsingXPeriodicity
      LOGICAL notUsingYPeriodicity

C--   COMMON /EESUPP_I/ Parallel support integer globals
C     pidW   -  Process  ID of neighbor to West
C     pidE   -           ditto             East
C     pidN   -           ditto             North
C     pidS   -           ditto             South
C              Note: pid[XY] is not necessairily the UNIX
C                    process id - it is just an identifying
C                    number.
C     myPid  - My own process id
C     nProcs - Number of processes
C     westCommunicationMode  - Mode of communication for each tile face
C     eastCommunicationMode
C     northCommunicationMode
C     southCommunicationMode
C     bi0   - Low cartesian tile index for this process
C     bj0     Note - In a tile distribution with holes bi0 and bj0
C                    are not useful. Neighboring tile indices must
C                    be derived some other way.
C     tileNo       - Tile identification number for my tile and
C     tileNo[WENS]   my N,S,E,W neighbor tiles.
C     tilePid[WENS] - Process identification number for
C                     my N,S,E,W neighbor tiles.
C     nTx, nTy    - No. threads in X and Y. This assumes a simple
C                   cartesian gridding of the threads which is not
C                   required elsewhere but that makes it easier.
      COMMON /EESUPP_I/
     & myPid, nProcs, pidW, pidE, pidN, pidS,
     & tileCommModeW,  tileCommModeE,
     & tileCommModeN,  tileCommModeS,
     & tileNo, tileNoW, tileNoE, tileNoS, tileNoN,
     &  tilePidW, tilePidE, tilePidS, tilePidN,
     &  tileBiW, tileBiE, tileBiS, tileBiN,
     & tileBjW, tileBjE, tileBjS, tileBjN,
     & tileTagSendW, tileTagSendE, tileTagSendS, tileTagSendN,
     & tileTagRecvW, tileTagRecvE, tileTagRecvS, tileTagRecvN
      INTEGER myPid
      INTEGER nProcs
      INTEGER pidW
      INTEGER pidE
      INTEGER pidN
      INTEGER pidS
      INTEGER tileCommModeW ( nSx, nSy )
      INTEGER tileCommModeE ( nSx, nSy )
      INTEGER tileCommModeN ( nSx, nSy )
      INTEGER tileCommModeS ( nSx, nSy )
      INTEGER tileNo( nSx, nSy )
      INTEGER tileNoW( nSx, nSy )
      INTEGER tileNoE( nSx, nSy )
      INTEGER tileNoN( nSx, nSy )
      INTEGER tileNoS( nSx, nSy )
      INTEGER tilePidW( nSx, nSy )
      INTEGER tilePidE( nSx, nSy )
      INTEGER tilePidN( nSx, nSy )
      INTEGER tilePidS( nSx, nSy )
      INTEGER tileBiW( nSx, nSy )
      INTEGER tileBiE( nSx, nSy )
      INTEGER tileBiN( nSx, nSy )
      INTEGER tileBiS( nSx, nSy )
      INTEGER tileBjW( nSx, nSy )
      INTEGER tileBjE( nSx, nSy )
      INTEGER tileBjN( nSx, nSy )
      INTEGER tileBjS( nSx, nSy )
      INTEGER tileTagSendW( nSx, nSy )
      INTEGER tileTagSendE( nSx, nSy )
      INTEGER tileTagSendN( nSx, nSy )
      INTEGER tileTagSendS( nSx, nSy )
      INTEGER tileTagRecvW( nSx, nSy )
      INTEGER tileTagRecvE( nSx, nSy )
      INTEGER tileTagRecvN( nSx, nSy )
      INTEGER tileTagRecvS( nSx, nSy )


C
CBOP
C     !ROUTINE: EXCH.h
C     !INTERFACE:
C     include "EXCH.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | EXCH.h
C     *==========================================================*
C     | Support data structures for
C     | the MITgcm-UV "exchange routines" code. This data should
C     | be private to the execution environment routines.
C     *==========================================================*
CEOP





























































C      MAX_OLX_EXCH - Maximum overlap region allowed in X
C      MAX_OLY_EXCH - Maximum overlap region allowed in Y
C      MAX_NR_EXCH  - Maximum number of vertical levels allowed
C      NUMBER_OF_BUFFER_LEVELS - Number of levels of buffer allowed.
C      EXCH_SPIN_LIMIT - Error trapping threshold for deadlocked exchange
       INTEGER MAX_OLX_EXCH
       PARAMETER ( MAX_OLX_EXCH = MAX_OLX )
       INTEGER MAX_OLY_EXCH
       PARAMETER ( MAX_OLY_EXCH = MAX_OLY )
       INTEGER MAX_NR_EXCH
       PARAMETER ( MAX_NR_EXCH  = nR + 1 )
       INTEGER NUMBER_OF_BUFFER_LEVELS
       PARAMETER ( NUMBER_OF_BUFFER_LEVELS = 1 )
       INTEGER EXCH_SPIN_LIMIT
       PARAMETER ( EXCH_SPIN_LIMIT = 100000000 )

C
C      L_BUFFER[XY]  - Maximum size for exchange buffer in
C      L_WBUFFER    west,
C      L_EBUFFER    east,
C      L_SBUFFER   south,
C      L_NBUFFER   north.
       INTEGER L_BUFFERX
       PARAMETER ( L_BUFFERX =
     &  (sNy+2*MAX_OLY_EXCH)*MAX_OLX_EXCH*MAX_NR_EXCH )
       INTEGER L_BUFFERY
       PARAMETER ( L_BUFFERY =
     &  (sNx+2*MAX_OLX_EXCH)*MAX_OLY_EXCH*MAX_NR_EXCH )
       INTEGER L_WBUFFER
       INTEGER L_EBUFFER
       INTEGER L_SBUFFER
       INTEGER L_NBUFFER
       PARAMETER ( L_WBUFFER = L_BUFFERX,
     &             L_EBUFFER = L_BUFFERX,
     &             L_SBUFFER = L_BUFFERY,
     &             L_NBUFFER = L_BUFFERY )

C--    COMMON / EXCH_L / LOGICAL number common arrays for exchanges
C      exchNeedsMemSync - TRUE if memory sync. required to ensure
C                         memory consistency during exchange
C      exchUsesBarrier  - TRUE if we use a call to BAR to do sync.
C                         between processes. On some machines we wont
C                         spin on the Ack setting ( the T90 ),
C                         instead we will use s system barrier.
C                         On the T90 the system barrier is very fast and
C                         switches out the thread while it waits. On most
C                         machines the system barrier is much too slow and if
C                         we own the machine and have one thread per process
C                         preemption is not a problem.
C      exchCollectStatistics - Turns exchange statistics collecting on and off.

       COMMON / EXCH_L / exchNeedsMemSync, exchUsesBarrier,
     &                   exchCollectStatistics
       LOGICAL exchNeedsMemSync
       LOGICAL exchUsesBarrier
       LOGICAL exchCollectStatistics

C--    COMMON / EXCH_R / REAL number common arrays for exchanges
C      xxxxSendBuf - Buffer used for sending data to another tile.
C      xxxxRecvBuf - Buffer used for receiving data from another tile.
       COMMON / EXCH_R /
     &  westSendBuf_RL, eastSendBuf_RL,
     &  southSendBuf_RL, northSendBuf_RL,
     &  westRecvBuf_RL, eastRecvBuf_RL,
     &  southRecvBuf_RL, northRecvBuf_RL,
     &  westSendBuf_RS, eastSendBuf_RS,
     &  southSendBuf_RS, northSendBuf_RS,
     &  westRecvBuf_RS, eastRecvBuf_RS,
     &  southRecvBuf_RS, northRecvBuf_RS,
     &  westSendBuf_R8, eastSendBuf_R8,
     &  southSendBuf_R8, northSendBuf_R8,
     &  westRecvBuf_R8, eastRecvBuf_R8,
     &  southRecvBuf_R8, northRecvBuf_R8,
     &  westSendBuf_R4, eastSendBuf_R4,
     &  southSendBuf_R4, northSendBuf_R4,
     &  westRecvBuf_R4, eastRecvBuf_R4,
     &  southRecvBuf_R4, northRecvBuf_R4
       Real*8   westSendBuf_RL( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8   eastSendBuf_RL( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  southSendBuf_RL( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  northSendBuf_RL( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8   westRecvBuf_RL( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8   eastRecvBuf_RL( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  southRecvBuf_RL( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  northRecvBuf_RL( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8   westSendBuf_RS( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8   eastSendBuf_RS( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  southSendBuf_RS( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  northSendBuf_RS( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8   westRecvBuf_RS( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8   eastRecvBuf_RS( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  southRecvBuf_RS( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  northRecvBuf_RS( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8   westSendBuf_R8( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8   eastSendBuf_R8( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  southSendBuf_R8( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  northSendBuf_R8( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8   westRecvBuf_R8( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8   eastRecvBuf_R8( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  southRecvBuf_R8( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*8  northRecvBuf_R8( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*4   westSendBuf_R4( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*4   eastSendBuf_R4( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*4  southSendBuf_R4( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*4  northSendBuf_R4( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*4   westRecvBuf_R4( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*4   eastRecvBuf_R4( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*4  southRecvBuf_R4( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       Real*4  northRecvBuf_R4( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )

C--    COMMON / EXCH_I / INTEGER common arrays for exchanges
C      xxxxSendAck - Flag indicating ready to send data.
C      xxxxRecvAck - Falg indicating receive data is ready.
C      exchBufferLevel - Current cyclic buffer level.
C      exchNReqsX, exchNReqsY - Pending message counts
C      exchReqIdX, exchReqIdY -Pending message identifiers
C      *Spin* - Exchange statistics holder
C       Count - No. spins for each thread
C         Max - Maximum spins for an exchange
C         Min - Minimum spins for an exchange
       COMMON / EXCH_I /
     &  westSendAck, eastSendAck, southSendAck, northSendAck,
     &  westRecvAck, eastRecvAck, southRecvAck, northRecvAck,
     &  exchangeBufLevel,
     &  exchNReqsX, exchNReqsY, exchReqIdX, exchReqIdY,
     &  exchRecvXSpinCount, exchRecvXSpinMax, exchRecvXSpinMin,
     &  exchRecvXExchCount,
     &  exchRecvYSpinCount, exchRecvYSpinMax, exchRecvYSpinMin,
     &  exchRecvYExchCount
       INTEGER  westSendAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER  eastSendAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER southSendAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER northSendAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER  westRecvAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER  eastRecvAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER southRecvAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER northRecvAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER exchangeBufLevel(  cacheLineSize/4, nSx, nSy )
       INTEGER exchNReqsX(cacheLineSize/4,nSx,nSy)
       INTEGER exchNReqsY(cacheLineSize/4,nSx,nSy)
       INTEGER exchReqIdX(2*nSx+2*nSy,cacheLineSize/4,nSx,nSy)
       INTEGER exchReqIdY(2*nSx+2*nSy,cacheLineSize/4,nSx,nSy)
       INTEGER exchRecvXSpinCount(cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvXExchCount(cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvXSpinMax  (cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvXSpinMin  (cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvYSpinCount(cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvYExchCount(cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvYSpinMax  (cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvYSpinMin  (cacheLineSize/4, nSx, nSy)



C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     array :: Array with edges to exchange.
C     myOLw :: West, East, North and South overlap region sizes.
C     myOLe
C     myOLn
C     myOLs
C     exchWidthX :: Width of data region exchanged.
C     exchWidthY
C     theSimulationMode :: Forward or reverse mode exchange ( provides
C                          support for adjoint integration of code. )
C     theCornerMode     :: Flag indicating whether corner updates are
C                          needed.
C     myThid            :: Thread number of this instance of S/R EXCH...
C     eBl               :: Edge buffer level
      INTEGER myOLw
      INTEGER myOLe
      INTEGER myOLs
      INTEGER myOLn
      INTEGER myNz
      Real*8 array(1-myOLw:sNx+myOLe,
     &          1-myOLs:sNy+myOLn,
     &          myNZ, nSx, nSy)
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER theSimulationMode
      INTEGER theCornerMode
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j, k, iMin, iMax, iB    :: Loop counters and extents
C     bi, bj
C     biW, bjW                   :: West tile indices
C     biE, bjE                   :: East tile indices
C     eBl                        :: Current exchange buffer level
C     theProc, theTag, theType,  :: Variables used in message building
C     theSize
C     westCommMode               :: Working variables holding type
C     eastCommMode                  of communication a particular
C                                   tile face uses.
      INTEGER i, j, k, iMin, iMax, iB
      INTEGER bi, bj, biW, bjW, biE, bjE
      INTEGER eBl
      INTEGER westCommMode
      INTEGER eastCommMode


C--   Write data to exchange buffer
C     Various actions are possible depending on the communication mode
C     as follows:
C       Mode      Action
C     --------   ---------------------------
C     COMM_NONE  Do nothing
C
C     COMM_MSG   Message passing communication ( e.g. MPI )
C                Fill west send buffer from this tile.
C                Send data with tag identifying tile and direction.
C                Fill east send buffer from this tile.
C                Send data with tag identifying tile and direction.
C
C     COMM_PUT   "Put" communication ( UMP_, shmemput, etc... )
C                Fill east receive buffer of west-neighbor tile
C                Fill west receive buffer of east-neighbor tile
C                Sync. memory
C                Write data-ready Ack for east edge of west-neighbor
C                tile
C                Write data-ready Ack for west edge of east-neighbor
C                tile
C                Sync. memory
C
CEOP



C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Prevent anyone to access shared buffer while an other thread modifies it
      CALL BARRIER(myThid)

C     Fill shared buffers from array values
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        eBl = exchangeBufLevel(1,bi,bj)
        westCommMode = tileCommModeW(bi,bj)
        eastCommMode = tileCommModeE(bi,bj)
        biE = tileBiE(bi,bj)
        bjE = tileBjE(bi,bj)
        biW = tileBiW(bi,bj)
        bjW = tileBjW(bi,bj)

C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<

        IF ( theSimulationMode .EQ. FORWARD_SIMULATION ) THEN

C       o Send or Put west edge
         iMin = 1
         iMax = 1+exchWidthX-1
         IF ( westCommMode .EQ. COMM_MSG  ) THEN
          iB = 0
          DO k=1,myNz
           DO j=1,sNy
            DO i=iMin,iMax
             iB = iB + 1
             westSendBuf_R8(iB,eBl,bi,bj) = array(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( westCommMode .EQ. COMM_PUT  ) THEN
          iB  = 0
          DO k=1,myNz
           DO j=1,sNy
            DO i=iMin,iMax
             iB = iB + 1
             eastRecvBuf_R8(iB,eBl,biW,bjW) = array(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( westCommMode .NE. COMM_NONE
     &    .AND.   westCommMode .NE. COMM_GET ) THEN
          STOP ' S/R EXCH: Invalid commW mode.'
         ENDIF

C       o Send or Put east edge
         iMin = sNx-exchWidthX+1
         iMax = sNx
         IF ( eastCommMode .EQ. COMM_MSG  ) THEN
          iB = 0
          DO k=1,myNz
           DO j=1,sNy
            DO i=iMin,iMax
             iB = iB + 1
             eastSendBuf_R8(iB,eBl,bi,bj) = array(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( eastCommMode .EQ. COMM_PUT  ) THEN
          iB  = 0
          DO k=1,myNz
           DO j=1,sNy
            DO i=iMin,iMax
             iB = iB + 1
             westRecvBuf_R8(iB,eBl,biE,bjE) = array(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( eastCommMode .NE. COMM_NONE
     &    .AND.   eastCommMode .NE. COMM_GET  ) THEN
          STOP ' S/R EXCH: Invalid commE mode.'
         ENDIF

C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
        ELSEIF ( theSimulationMode .EQ. REVERSE_SIMULATION ) THEN

C       o Send or Put west edge
         iMin = 1-exchWidthX
         iMax = 0
         IF ( westCommMode .EQ. COMM_MSG  ) THEN
          iB = 0
          DO k=1,myNz
           DO j=1,sNy
            DO i=iMin,iMax
             iB = iB + 1
             westSendBuf_R8(iB,eBl,bi,bj) = array(i,j,k,bi,bj)
             array(i,j,k,bi,bj) = 0.0
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( westCommMode .EQ. COMM_PUT  ) THEN
          iB  = 0
          DO k=1,myNz
           DO j=1,sNy
            DO i=iMin,iMax
             iB = iB + 1
             eastRecvBuf_R8(iB,eBl,biW,bjW) = array(i,j,k,bi,bj)
             array(i,j,k,bi,bj) = 0.0
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( westCommMode .NE. COMM_NONE
     &    .AND.   westCommMode .NE. COMM_GET ) THEN
          STOP ' S/R EXCH: Invalid commW mode.'
         ENDIF

C       o Send or Put east edge
         iMin = sNx+1
         iMax = sNx+exchWidthX
         IF ( eastCommMode .EQ. COMM_MSG  ) THEN
          iB = 0
          DO k=1,myNz
           DO j=1,sNy
            DO i=iMin,iMax
             iB = iB + 1
             eastSendBuf_R8(iB,eBl,bi,bj) = array(i,j,k,bi,bj)
             array(i,j,k,bi,bj) = 0.0
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( eastCommMode .EQ. COMM_PUT  ) THEN
          iB  = 0
          DO k=1,myNz
           DO j=1,sNy
            DO i=iMin,iMax
             iB = iB + 1
             westRecvBuf_R8(iB,eBl,biE,bjE) = array(i,j,k,bi,bj)
             array(i,j,k,bi,bj) = 0.0
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( eastCommMode .NE. COMM_NONE
     &    .AND.   eastCommMode .NE. COMM_GET  ) THEN
          STOP ' S/R EXCH: Invalid commE mode.'
         ENDIF

        ENDIF

       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Signal completetion ( making sure system-wide memory state is
C--                         consistent ).

C     ** NOTE ** We are relying on being able to produce strong-ordered
C     memory semantics here. In other words we assume that there is a
C     mechanism which can ensure that by the time the Ack is seen the
C     overlap region data that will be exchanged is up to date.
      IF ( exchNeedsMemSync  ) CALL MEMSYNC

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        eBl = exchangeBufLevel(1,bi,bj)
        biE = tileBiE(bi,bj)
        bjE = tileBjE(bi,bj)
        biW = tileBiW(bi,bj)
        bjW = tileBjW(bi,bj)
        westCommMode = tileCommModeW(bi,bj)
        eastCommMode = tileCommModeE(bi,bj)
        IF ( westCommMode.EQ.COMM_PUT ) eastRecvAck(eBl,biW,bjW) = 1
        IF ( eastCommMode.EQ.COMM_PUT ) westRecvAck(eBl,biE,bjE) = 1
        IF ( westCommMode.EQ.COMM_GET ) eastRecvAck(eBl,biW,bjW) = 1
        IF ( eastCommMode.EQ.COMM_GET ) westRecvAck(eBl,biE,bjE) = 1
       ENDDO
      ENDDO

C--   Make sure "ack" setting is seen system-wide.
C     Here strong-ordering is not an issue but we want to make
C     sure that processes that might spin on the above Ack settings
C     will see the setting.
C     ** NOTE ** On some machines we wont spin on the Ack setting
C     ( particularly the T90 ), instead we will use s system barrier.
C     On the T90 the system barrier is very fast and switches out the
C     thread while it waits. On most machines the system barrier
C     is much too slow and if we own the machine and have one thread
C     per process preemption is not a problem.
      IF ( exchNeedsMemSync  ) CALL MEMSYNC

C     Wait until all threads finish filling buffer
      CALL BARRIER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|



C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      RETURN
      END

