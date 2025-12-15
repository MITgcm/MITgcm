
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






C--  File printf.F: Routines for performing formatted textual I/O
C--                 in the MITgcm UV implementation environment.
C--   Contents
C--   o PRINT_MESSAGE  Does IO with unhighlighted header
C--   o PRINT_ERROR    Does IO with **ERROR** highlighted header
C--   o PRINT_LIST_I   Prints one-dimensional list of INTEGER
C--                    numbers.
C--   o PRINT_LIST_L   Prints one-dimensional list of LOGICAL
C--                    variables.
C--   o PRINT_LIST_RL  Prints one-dimensional list of Real(Real*8)
C--                    numbers.
C--   o PRINT_MAPRS    Formats ABCD... contour map of a Real(Real*8) field
C--                    Uses print_message for writing
C--   o PRINT_MAPRL    Formats ABCD... contour map of a Real(Real*8) field
C--                    Uses print_message for writing

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_MESSAGE
C     !INTERFACE:
      SUBROUTINE PRINT_MESSAGE( message, unit, sq , myThid )

C     !DESCRIPTION:
C     *============================================================*
C     | SUBROUTINE PRINT\_MESSAGE
C     | o Write out informational message using "standard" format.
C     *============================================================*
C     | Notes
C     | =====
C     | o Some system   I/O is not "thread-safe". For this reason
C     |   without the FMTFTN\_IO\_THREAD\_SAFE directive set a
C     |   critical region is defined around the write here. In some
C     |   cases  BEGIN\_CRIT() is approximated by only doing writes
C     |   for thread number 1 - writes for other threads are
C     |   ignored!
C     | o In a non-parallel form these routines can still be used.
C     |   to produce pretty printed output!
C     *============================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==

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



C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     message :: Message to write
C     unit    :: Unit number to write to
C     sq      :: Justification option
      CHARACTER*(*) message
      INTEGER       unit
      CHARACTER*(*) sq
      INTEGER  myThid

C     !FUNCTIONS:
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iStart, iEnd :: String indexing variables
C     idString     :: Temp. for building prefix.
C     fmtStr, iTmp :: Temp. for building prefix.
C     iTmpThid     :: Temp. for building prefix.
      INTEGER iStart
      INTEGER iEnd
      INTEGER iTmp, iTmpThid
      CHARACTER*13 fmtStr
      CHARACTER*13 idString
CEOP

C--   Find beginning and end of message
      IF ( sq .EQ. SQUEEZE_BOTH .OR.
     &     sq .EQ. SQUEEZE_LEFT ) THEN
       iStart = IFNBLNK( message )
      ELSE
       iStart = 1
      ENDIF
      IF ( sq .EQ. SQUEEZE_BOTH .OR.
     &     sq .EQ. SQUEEZE_RIGHT ) THEN
       iEnd   = ILNBLNK( message )
      ELSE
       iEnd   = LEN(message)
      ENDIF
C--   Test to see if in multi-process ( or multi-threaded ) mode.
C     If so include process or thread identifier.
      IF ( numberOfProcs .EQ. 0 .AND. nThreads .EQ. 1 ) THEN
C--    Write single process format
       IF ( message .EQ. ' ' ) THEN
        WRITE(unit,'(A)') ' '
       ELSE
        WRITE(unit,'(A)') message(iStart:iEnd)
       ENDIF
      ELSEIF ( pidIO .EQ. myProcId ) THEN
C--    Write multi-process format
C      PRINT can be called by several threads simultaneously.
C      The write statement may need to be marked as a critical section.

       IF ( myThid .EQ. 1 ) THEN

       fmtStr = '(I4.4,A,I4.4)'
       IF ( nPx*nPy .GE. 10000 ) THEN
         iTmp     = 1 + INT(LOG10(DFLOAT(nPx*nPy)))
         iTmpThid = 1 + INT(LOG10(DFLOAT(MAX_NO_THREADS)))
         iTmpThid = MAX( iTmpThid, 2, 8-iTmp )
         WRITE(fmtStr,'(4(A,I1),A)')
     &        '(I',iTmp,'.',iTmp,',A,I',iTmpThid,'.',iTmpThid,')'
       ENDIF
       WRITE(idString,fmtStr) myProcId,'.',myThid
       iTmp = ILNBLNK( idString )
       IF ( message .EQ. ' ' ) THEN
        WRITE(unit,'(A,A,A,A,A,A)',ERR=999)
     &   '(',PROCESS_HEADER,' ',idString(1:iTmp),')',' '
       ELSE
        WRITE(unit,'(A,A,A,A,A,A,A)',ERR=999)
     &   '(',PROCESS_HEADER,' ',idString(1:iTmp),')',' ',
     &   message(iStart:iEnd)
       ENDIF
       IF ( debugMode ) THEN
        CALL MDS_FLUSH( unit, myThid )
       ENDIF
       GOTO 1000
  999  CONTINUE
       ioErrorCount(myThid) = ioErrorCount(myThid)+1
 1000  CONTINUE

       ENDIF

      ENDIF


C--   if error message, also write directly to unit 0 :
      IF ( numberOfProcs .EQ. 1 .AND. myThid .EQ. 1
     &     .AND. unit.EQ.errorMessageUnit
     &     .AND. message .NE. ' ' ) THEN
        IF ( nThreads.LE.1 ) THEN
          WRITE(0,'(A)') message(iStart:iEnd)
        ELSE
          WRITE(0,'(A,I4.4,A,A)') '(TID ', myThid, ') ',
     &                   message(iStart:iEnd)
        ENDIF
      ENDIF


      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_ERROR
C     !INTERFACE:
      SUBROUTINE PRINT_ERROR( message , myThid )

C     !DESCRIPTION:
C     *============================================================*
C     | SUBROUTINE PRINT\_ERROR
C     | o Write out error message using "standard" format.
C     *============================================================*
C     | Notes
C     | =====
C     | o Some system   I/O is not "thread-safe". For this reason
C     |   without the FMTFTN\_IO\_THREAD\_SAFE directive set a
C     |   critical region is defined around the write here. In some
C     |   cases  BEGIN\_CRIT() is approximated by only doing writes
C     |   for thread number 1 - writes for other threads are
C     |   ignored!
C     | o In a non-parallel form these routines are still used
C     |   to produce pretty printed output. The process and thread
C     |   id prefix is omitted in this case.
C     *============================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==

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



C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     message :: Text string to print
C     myThid  :: Thread number of this instance
      CHARACTER*(*) message
      INTEGER       myThid

C     !FUNCTIONS:
c     INTEGER  IFNBLNK
c     EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iStart, iEnd :: Temps. for string indexing
C     idString     :: Temp. for building message prefix
C     fmtStr, iTmp :: Temp. for building prefix.
C     iTmpThid     :: Temp. for building prefix.
c     INTEGER iStart
      INTEGER iEnd
      INTEGER iTmp, iTmpThid
      CHARACTER*13 fmtStr
      CHARACTER*13 idString
CEOP

C--   Find beginning and end of message
c     iStart = IFNBLNK( message )
      iEnd   = ILNBLNK( message )
C--   Test to see if in multi-process ( or multi-threaded ) mode.
C     If so include process or thread identifier.
      IF ( numberOfProcs .EQ. 0 .AND. nThreads .EQ. 1 ) THEN
C--    Write single process format
       IF ( iEnd.EQ.0 ) THEN
        WRITE(errorMessageUnit,'(A,1X,A)') ERROR_HEADER, ' '
       ELSE
        WRITE(errorMessageUnit,'(A,1X,A)') ERROR_HEADER,
     &        message(1:iEnd)
c    &    message(iStart:iEnd)
       ENDIF
      ELSE
C       PRINT_ERROR can be called by several threads simulataneously.
C       The write statement may need to be marked as a critical section.

       IF ( myThid .EQ. 1 ) THEN

       IF ( pidIO .EQ. myProcId ) THEN
C--    Write multi-process format
         fmtStr = '(I4.4,A,I4.4)'
         IF ( nPx*nPy .GE. 10000 ) THEN
           iTmp     = 1 + INT(LOG10(DFLOAT(nPx*nPy)))
           iTmpThid = 1 + INT(LOG10(DFLOAT(MAX_NO_THREADS)))
           iTmpThid = MAX( iTmpThid, 2, 8-iTmp )
           WRITE(fmtStr,'(4(A,I1),A)')
     &          '(I',iTmp,'.',iTmp,',A,I',iTmpThid,'.',iTmpThid,')'
         ENDIF
         WRITE(idString,fmtStr) myProcId,'.',myThid
         iTmp = ILNBLNK( idString )

         IF ( iEnd.EQ.0 ) THEN
          WRITE(errorMessageUnit,'(A,A,1X,A,A,A,A,A)',ERR=999)
     &    '(',PROCESS_HEADER,idString(1:iTmp),')',ERROR_HEADER,' ',
     &    ' '
         ELSE
          WRITE(errorMessageUnit,'(A,A,1X,A,A,A,A,A)',ERR=999)
     &    '(',PROCESS_HEADER,idString(1:iTmp),')',ERROR_HEADER,' ',
     &        message(1:iEnd)
c    &    message(iStart:iEnd)
         ENDIF
       ENDIF
       IF ( debugMode ) THEN
        CALL MDS_FLUSH( errorMessageUnit, myThid )
       ENDIF
       GOTO 1000
  999  CONTINUE
       ioErrorCount(myThid) = ioErrorCount(myThid)+1
 1000  CONTINUE


C--    also write directly to unit 0 :
       IF ( numberOfProcs.EQ.1 .AND. iEnd.NE.0 ) THEN
        IF ( nThreads.LE.1 ) THEN
          WRITE(0,'(A)') message(1:iEnd)
        ELSE
          WRITE(0,'(A,I4.4,A,A)') '(TID ', myThid, ') ',
     &                   message(1:iEnd)
        ENDIF
       ENDIF



        ENDIF

      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_LIST_I
C     !INTERFACE:
      SUBROUTINE PRINT_LIST_I( fld, iFirst, iLast, index_type,
     &                         markEnd, compact, ioUnit )

C     !DESCRIPTION:
C     *==========================================================*
C     | o SUBROUTINE PRINT\_LIST\_I
C     *==========================================================*
C     | Routine for producing list of values for a field with
C     | duplicate values collected into
C     |    n \@ value
C     | record.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==

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


C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld     :: Data to be printed
C     iFirst  :: First element to print
C     iLast   :: Last element to print
C  index_type :: Flag indicating which type of index to print
C                  INDEX_K    => 
C                  INDEX_I    => 
C                  INDEX_J    => 
C                  INDEX_NONE =>
C     markEnd :: Flag to control whether there is a separator after the
C                last element
C     compact :: Flag to control use of repeat symbol for same valued
C                fields.
C     ioUnit  :: Unit number for IO.
      INTEGER iFirst, iLast
      INTEGER fld(iFirst:iLast)
      INTEGER index_type
      LOGICAL markEnd
      LOGICAL compact
      INTEGER ioUnit

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iLo  - Range index holders for selecting elements with
C     iHi    with the same value
C     nDup - Number of duplicates
C     xNew, xOld - Hold current and previous values of field
C     punc - Field separator
C     msgBuf - IO buffer
C     index_lab - Index for labelling elements
C     K    - Loop counter
      INTEGER iLo
      INTEGER iHi
      INTEGER nDup
      INTEGER xNew, xOld
      CHARACTER punc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*2 commOpen,commClose
      CHARACTER*3 index_lab
      CHARACTER*25 fmt1, fmt2
      INTEGER K
CEOP

      IF     ( index_type .EQ. INDEX_I ) THEN
       index_lab = 'I ='
      ELSEIF ( index_type .EQ. INDEX_J ) THEN
       index_lab = 'J ='
      ELSEIF ( index_type .EQ. INDEX_K ) THEN
       index_lab = 'K ='
      ELSE
       index_lab = '?='
      ENDIF
C-    fortran format to write 1 or 2 indices:
      fmt1='(A,1X,A,I3,1X,A)'
      fmt2='(A,1X,A,I3,A,I3,1X,A)'
      IF ( iLast.GE.1000 ) THEN
        K = 1+INT(LOG10(FLOAT(iLast)))
        WRITE(fmt1,'(A,I1,A)')      '(A,1X,A,I',K,',1X,A)'
        WRITE(fmt2,'(A,I1,A,I1,A)') '(A,1X,A,I',K,',A,I',K,',1X,A)'
      ENDIF
      commOpen  = '/*'
      commClose = '*/'
      iLo = iFirst
      iHi = iFirst
      punc = ','
      xOld = fld(iFirst)
      DO K = iFirst+1,iLast
       xNew = fld(K  )
       IF ( .NOT. compact .OR. (xNew .NE. xOld) ) THEN
        nDup = iHi-iLo+1
        IF ( nDup .EQ. 1 ) THEN
         WRITE(msgBuf,'(A,I9,A)') '              ',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt1)
     &    commOpen,index_lab,iLo,commClose
        ELSE
         WRITE(msgBuf,'(I5,'' '',A,I9,A)') nDup,'@',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt2)
     &    commOpen,index_lab,iLo,':',iHi,commClose
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
        iLo  = K
        iHi  = K
        xOld = xNew
       ELSE
        iHi = K
       ENDIF
      ENDDO
      punc = ' '
      IF ( markEnd ) punc = ','
      nDup = iHi-iLo+1
      IF    ( nDup .EQ. 1 ) THEN
       WRITE(msgBuf,'(A,I9,A)') '              ',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt1)
     &    commOpen,index_lab,iLo,commClose
      ELSEIF( nDup .GT. 1 ) THEN
       WRITE(msgBuf,'(I5,'' '',A,I9,A)') nDup,'@',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt2)
     &    commOpen,index_lab,iLo,':',iHi,commClose
      ENDIF
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_LIST_L
C     !INTERFACE:
      SUBROUTINE PRINT_LIST_L( fld, iFirst, iLast, index_type,
     &                         markEnd, compact, ioUnit )

C     !DESCRIPTION:
C     *==========================================================*
C     | o SUBROUTINE PRINT\_LIST\_L
C     *==========================================================*
C     | Routine for producing list of values for a field with
C     | duplicate values collected into
C     |    n \@ value
C     | record.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==

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


C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld     :: Data to be printed
C     iFirst  :: First element to print
C     iLast   :: Last element to print
C  index_type :: Flag indicating which type of index to print
C                  INDEX_K    => 
C                  INDEX_I    => 
C                  INDEX_J    => 
C                  INDEX_NONE =>
C     markEnd :: Flag to control whether there is a separator after the
C                last element
C     compact :: Flag to control use of repeat symbol for same valued
C                fields.
C     ioUnit  :: Unit number for IO.
      INTEGER iFirst, iLast
      LOGICAL fld(iFirst:iLast)
      INTEGER index_type
      LOGICAL markEnd
      LOGICAL compact
      INTEGER ioUnit

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iLo  - Range index holders for selecting elements with
C     iHi    with the same value
C     nDup - Number of duplicates
C     xNew, xOld - Hold current and previous values of field
C     punc - Field separator
C     msgBuf - IO buffer
C     index_lab - Index for labelling elements
C     K    - Loop counter
      INTEGER iLo
      INTEGER iHi
      INTEGER nDup
      LOGICAL xNew, xOld
      CHARACTER punc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*2 commOpen,commClose
      CHARACTER*3 index_lab
      CHARACTER*25 fmt1, fmt2
      INTEGER K
CEOP

      IF     ( index_type .EQ. INDEX_I ) THEN
       index_lab = 'I ='
      ELSEIF ( index_type .EQ. INDEX_J ) THEN
       index_lab = 'J ='
      ELSEIF ( index_type .EQ. INDEX_K ) THEN
       index_lab = 'K ='
      ELSE
       index_lab = '?='
      ENDIF
C-    fortran format to write 1 or 2 indices:
      fmt1='(A,1X,A,I3,1X,A)'
      fmt2='(A,1X,A,I3,A,I3,1X,A)'
      IF ( iLast.GE.1000 ) THEN
        K = 1+INT(LOG10(FLOAT(iLast)))
        WRITE(fmt1,'(A,I1,A)')      '(A,1X,A,I',K,',1X,A)'
        WRITE(fmt2,'(A,I1,A,I1,A)') '(A,1X,A,I',K,',A,I',K,',1X,A)'
      ENDIF
      commOpen  = '/*'
      commClose = '*/'
      iLo = iFirst
      iHi = iFirst
      punc = ','
      xOld = fld(iFirst)
      DO K = iFirst+1,iLast
       xNew = fld(K  )
       IF ( .NOT. compact .OR. (xNew .NEQV. xOld) ) THEN
        nDup = iHi-iLo+1
        IF ( nDup .EQ. 1 ) THEN
         WRITE(msgBuf,'(A,L5,A)') '              ',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt1)
     &    commOpen,index_lab,iLo,commClose
        ELSE
         WRITE(msgBuf,'(I5,'' '',A,L5,A)') nDup,'@',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt2)
     &    commOpen,index_lab,iLo,':',iHi,commClose
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
        iLo  = K
        iHi  = K
        xOld = xNew
       ELSE
        iHi = K
       ENDIF
      ENDDO
      punc = ' '
      IF ( markEnd ) punc = ','
      nDup = iHi-iLo+1
      IF    ( nDup .EQ. 1 ) THEN
       WRITE(msgBuf,'(A,L5,A)') '              ',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &  WRITE(msgBuf(45:),'(A,1X,A,I3,1X,A)')
     &    commOpen,index_lab,iLo,commClose
      ELSEIF( nDup .GT. 1 ) THEN
       WRITE(msgBuf,'(I5,'' '',A,L5,A)') nDup,'@',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &  WRITE(msgBuf(45:),'(A,1X,A,I3,A,I3,1X,A)')
     &  commOpen,index_lab,iLo,':',iHi,commClose
      ENDIF
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_LIST_RL
C     !INTERFACE:
      SUBROUTINE PRINT_LIST_RL( fld, iFirst, iLast, index_type,
     &                          markEnd, compact, ioUnit )

C     !DESCRIPTION:
C     *==========================================================*
C     | o SUBROUTINE PRINT\_LIST\Real*8
C     *==========================================================*
C     | Routine for producing list of values for a field with
C     | duplicate values collected into
C     |    n \@ value
C     | record.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==

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


C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld     :: Data to be printed
C     iFirst  :: First element to print
C     iLast   :: Last element to print
C  index_type :: Flag indicating which type of index to print
C                  INDEX_K    => 
C                  INDEX_I    => 
C                  INDEX_J    => 
C                  INDEX_NONE =>
C     markEnd :: Flag to control whether there is a separator after the
C                last element
C     compact :: Flag to control use of repeat symbol for same valued
C                fields.
C     ioUnit  :: Unit number for IO.
      INTEGER iFirst, iLast
      Real*8     fld(iFirst:iLast)
      INTEGER index_type
      LOGICAL markEnd
      LOGICAL compact
      INTEGER ioUnit

C     !LOCA VARIABLES:
C     == Local variables ==
C     iLo  - Range index holders for selecting elements with
C     iHi    with the same value
C     nDup - Number of duplicates
C     xNew, xOld - Hold current and previous values of field
C     punc - Field separator
C     msgBuf - IO buffer
C     index_lab - Index for labelling elements
C     K    - Loop counter
      INTEGER iLo
      INTEGER iHi
      INTEGER nDup
      Real*8     xNew, xOld
      CHARACTER punc
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*2 commOpen,commClose
      CHARACTER*3 index_lab
      CHARACTER*25 fmt1, fmt2
      INTEGER K
CEOP

      IF     ( index_type .EQ. INDEX_I ) THEN
       index_lab = 'I ='
      ELSEIF ( index_type .EQ. INDEX_J ) THEN
       index_lab = 'J ='
      ELSEIF ( index_type .EQ. INDEX_K ) THEN
       index_lab = 'K ='
      ELSE
       index_lab = '?='
      ENDIF
C-    fortran format to write 1 or 2 indices:
      fmt1='(A,1X,A,I3,1X,A)'
      fmt2='(A,1X,A,I3,A,I3,1X,A)'
      IF ( iLast.GE.1000 ) THEN
        K = 1+INT(LOG10(FLOAT(iLast)))
        WRITE(fmt1,'(A,I1,A)')      '(A,1X,A,I',K,',1X,A)'
        WRITE(fmt2,'(A,I1,A,I1,A)') '(A,1X,A,I',K,',A,I',K,',1X,A)'
      ENDIF
      commOpen  = '/*'
      commClose = '*/'
      iLo = iFirst
      iHi = iFirst
      punc = ','
      xOld = fld(iFirst)
      DO K = iFirst+1,iLast
       xNew = fld(K  )
       IF ( .NOT. compact .OR. (xNew .NE. xOld) ) THEN
        nDup = iHi-iLo+1
        IF ( nDup .EQ. 1 ) THEN
         WRITE(msgBuf,'(A,1PE23.15,A)') '              ',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt1)
     &    commOpen,index_lab,iLo,commClose
        ELSE
         WRITE(msgBuf,'(I5,'' '',A,1PE23.15,A)') nDup,'@',xOld,punc
         IF ( index_type .NE. INDEX_NONE )
     &    WRITE(msgBuf(45:),fmt2)
     &    commOpen,index_lab,iLo,':',iHi,commClose
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
        iLo  = K
        iHi  = K
        xOld = xNew
       ELSE
        iHi = K
       ENDIF
      ENDDO
      punc = ' '
      IF ( markEnd ) punc = ','
      nDup = iHi-iLo+1
      IF    ( nDup .EQ. 1 ) THEN
       WRITE(msgBuf,'(A,1PE23.15,A)') '              ',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &  WRITE(msgBuf(45:),fmt1)
     &    commOpen,index_lab,iLo,commClose
      ELSEIF( nDup .GT. 1 ) THEN
       WRITE(msgBuf,'(I5,'' '',A,1PE23.15,A)') nDup,'@',xOld,punc
       IF ( index_type .NE. INDEX_NONE )
     &  WRITE(msgBuf(45:),fmt2)
     &  commOpen,index_lab,iLo,':',iHi,commClose
      ENDIF
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_MAPRS
C     !INTERFACE:
      SUBROUTINE PRINT_MAPRS ( fld, fldTitle, plotMode,
     I        iLo,   iHi,   jLo,   jHi,  kLo,  kHi, nBx, nBy,
     I       iMin,  iMax,  iStr,
     I       jMin,  jMax,  jStr,
     I       kMin, kMax,   kStr,
     I      bxMin, bxMax,  bxStr,
     I      byMin, byMax,  byStr )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE PRINT\_MAPRS
C     | o Does textual mapping printing of a field.
C     *==========================================================*
C     | This routine does the actual formatting of the data
C     | and printing to a file. It assumes an array using the
C     | MITgcm UV indexing scheme and base index variables.
C     | User code should call an interface routine like
C     | PLOT\_FIELD\_XYRS( ... ) rather than this code directly.
C     | Text plots can be oriented XY, YZ, XZ. An orientation
C     | is specficied through the "plotMode" argument. All the
C     | plots made by a single call to this routine will use the
C     | same contour interval. The plot range (iMin,...,byStr)
C     | can be three-dimensional. A separate plot is made for
C     | each point in the plot range normal to the orientation.
C     | e.g. if the orientation is XY (plotMode = PRINT\_MAP\_XY).
C     |      kMin =1, kMax = 5 and kStr = 2 will produce three XY
C     |      plots - one for K=1, one for K=3 and one for K=5.
C     |      Each plot would have extents iMin:iMax step iStr
C     |      and jMin:jMax step jStr.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

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


C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        - Real*4 array holding data to be plotted
C     fldTitle   - Name of field to be plotted
C     plotMode   - Text string indicating plot orientation
C                  ( see - EEPARAMS.h for valid values ).
C     iLo, iHi,  - Dimensions of array fld. fld is assumed to
C     jLo, jHi     be five-dimensional.
C     kLo, kHi
C     nBx, nBy
C     iMin, iMax - Indexing for points to plot. Points from
C     iStr         iMin -> iMax in steps of iStr are plotted
C     jMin. jMax   and similarly for jMin, jMax, jStr and
C     jStr         kMin, kMax, kStr and bxMin, bxMax, bxStr
C     kMin, kMax   byMin, byMax, byStr.
C     kStr
      CHARACTER*(*) fldTitle
      CHARACTER*(*) plotMode
      INTEGER iLo, iHi
      INTEGER jLo, jHi
      INTEGER kLo, kHi
      INTEGER nBx, nBy
      Real*8 fld(iLo:iHi,jLo:jHi,kLo:kHi,nBx,nBy)
      INTEGER iMin, iMax, iStr
      INTEGER jMin, jMax, jStr
      INTEGER kMin, kMax, kStr
      INTEGER bxMin, bxMax, bxStr
      INTEGER byMin, byMax, byStr

C     !FUNCTIONS:
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     plotBuf - Buffer for building plot record
C     chList  - Character string used for plot
C     fMin, fMax - Contour min, max and range
C     fRange
C     val     - Value of element to be "plotted"
C     small   - Lowest range for which contours are plotted
C     accXXX  - Variables used in indexing accross page records.
C     dwnXXX    Variables used in indexing down the page.
C     pltXXX    Variables used in indexing multiple plots ( multiple
C               plots use same contour range).
C               Lab  - Label
C               Base - Base number for element indexing
C                      The process bottom, left coordinate in the
C                      global domain.
C               Step - Block size
C               Blo  - Start block
C               Bhi  - End block
C               Bstr - Block stride
C               Min  - Start index within block
C               Max  - End index within block
C               Str  - stride within block
      INTEGER MAX_LEN_PLOTBUF
      PARAMETER ( MAX_LEN_PLOTBUF = MAX_LEN_MBUF-20 )
      CHARACTER*(MAX_LEN_PLOTBUF) plotBuf
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER lChList
      PARAMETER ( lChList = 28 )
      CHARACTER*(lChList) chList
      Real*8  fMin
      Real*8  fMax
      Real*8  fRange
      Real*8  val
      Real*8  small
      CHARACTER*2  accLab
      CHARACTER*7  dwnLab
      CHARACTER*3  pltLab
      INTEGER     accBase, dwnBase, pltBase
      INTEGER     accStep, dwnStep, pltStep
      INTEGER     accBlo,  dwnBlo,  pltBlo
      INTEGER     accBhi,  dwnBhi,  pltBhi
      INTEGER     accBstr, dwnBstr, pltBstr
      INTEGER     accMin,  dwnMin,  pltMin
      INTEGER     accMax,  dwnMax,  pltMax
      INTEGER     accStr,  dwnStr,  pltStr
      INTEGER I, J, K, iStrngLo, iStrngHi, iBuf, iDx
      INTEGER bi, bj, bk
      LOGICAL validRange
CEOP

      chList = '-abcdefghijklmnopqrstuvwxyz+'
      small  =  1.D-15
      fMin   =  1.D32
      fMax   = -1.D32
      validRange = .FALSE.

C--   Calculate field range
      DO bj=byMin, byMax, byStr
       DO bi=bxMin, bxMax, bxStr
        DO K=kMin, kMax, kStr
         DO J=jMin, jMax, jStr
          DO I=iMin, iMax, iStr
           IF (printMapIncludesZeros .OR. fld(I,J,K,bi,bj) .NE. 0.) THEN
            IF ( fld(I,J,K,bi,bj) .LT. fMin )
     &       fMin = fld(I,J,K,bi,bj)
            IF ( fld(I,J,K,bi,bj) .GT. fMax )
     &       fMax = fld(I,J,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      fRange = fMax-fMin
      IF ( fRange .GT. small ) validRange = .TRUE.

C--   Write field title and statistics
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      iStrngLo = IFNBLNK(fldTitle)
      iStrngHi = ILNBLNK(fldTitle)
      IF ( iStrngLo .LE. iStrngHi ) THEN
       WRITE(msgBuf,'(A)') fldTitle(iStrngLo:iStrngHi)
      ELSE
       msgBuf = '// UNKNOWN FIELD'
      ENDIF
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1PE30.15)')
     & '// CMIN = ', fMin
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1PE30.15)')
     & '// CMAX = ', fMax
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      IF ( validRange ) THEN
       WRITE(msgBuf,'(A,1PE30.15)')
     &  '// CINT = ', fRange/FLOAT(lChlist-1)
      ELSE
       WRITE(msgBuf,'(A,1PE30.15)')
     &  '// CINT = ', 0.
      ENDIF
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1024A1)')
     & '// SYMBOLS (CMIN->CMAX): ',(chList(I:I),I=1,lChList)
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1024A1)')
     & '//                  0.0: ','.'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I6),A)')
     & '// RANGE I (Lo:Hi:Step):',
     &  '(',myXGlobalLo-1+(bxMin-1)*sNx+iMin,
     &  ':',myXGlobalLo-1+(bxMax-1)*sNx+iMax,
     &  ':',iStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I6),A)')
     & '// RANGE J (Lo:Hi:Step):',
     &  '(',myYGlobalLo-1+(byMin-1)*sNy+jMin,
     &  ':',myYGlobalLo-1+(byMax-1)*sNy+jMax,
     &  ':',jStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I4),A)')
     & '// RANGE K (Lo:Hi:Step):',
     &  '(',kMin,
     &  ':',kMax,
     &  ':',kStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)

c     if (Nx.gt.MAX_LEN_PLOTBUF-20) THEN
c      msgBuf =
c    &  'Model domain too big to print to terminal - skipping I/O'
c      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
c    &                   SQUEEZE_RIGHT, 1)
c      RETURN
c     endif

C--   Write field
C     Figure out slice type and set plotting parameters appropriately
C     acc = accross the page
C     dwn = down the page
      IF ( plotMode .EQ. PRINT_MAP_XY ) THEN
C      X across, Y down slice
       accLab  = 'I='
       accBase = myXGlobalLo
       accStep = sNx
       accBlo  = bxMin
       accBhi  = bxMax
       accBStr = bxStr
       accMin  = iMin
       accMax  = iMax
       accStr  = iStr
       dwnLab  = '|--J--|'
       dwnBase = myYGlobalLo
       dwnStep = sNy
       dwnBlo  = byMin
       dwnBhi  = byMax
       dwnBStr = byStr
       dwnMin  = jMin
       dwnMax  = jMax
       dwnStr  = jStr
       pltBlo  = 1
       pltBhi  = 1
       pltBstr = 1
       pltMin  = kMin
       pltMax  = kMax
       pltStr  = kStr
       pltBase = 1
       pltStep = 1
       pltLab  = 'K ='
      ELSEIF ( plotMode .EQ. PRINT_MAP_YZ ) THEN
C      Y across, Z down slice
       accLab  = 'J='
       accBase = myYGlobalLo
       accStep = sNy
       accBlo  = byMin
       accBhi  = byMax
       accBStr = byStr
       accMin  = jMin
       accMax  = jMax
       accStr  = jStr
       dwnLab  = '|--K--|'
       dwnBase = 1
       dwnStep = 1
       dwnBlo  = 1
       dwnBhi  = 1
       dwnBStr = 1
       dwnMin  = kMin
       dwnMax  = kMax
       dwnStr  = kStr
       pltBlo  = bxMin
       pltBhi  = bxMax
       pltBstr = bxStr
       pltMin  = iMin
       pltMax  = iMax
       pltStr  = iStr
       pltBase = myXGlobalLo
       pltStep = sNx
       pltLab  = 'I ='
      ELSEIF ( plotMode .EQ. PRINT_MAP_XZ ) THEN
C      X across, Z down slice
       accLab  = 'I='
       accBase = myXGlobalLo
       accStep = sNx
       accBlo  = bxMin
       accBhi  = bxMax
       accBStr = bxStr
       accMin  = iMin
       accMax  = iMax
       accStr  = iStr
       dwnLab  = '|--K--|'
       dwnBase = 1
       dwnStep = 1
       dwnBlo  = 1
       dwnBhi  = 1
       dwnBStr = 1
       dwnMin  = kMin
       dwnMax  = kMax
       dwnStr  = kStr
       pltBlo  = byMin
       pltBhi  = byMax
       pltBstr = byStr
       pltMin  = jMin
       pltMax  = jMax
       pltStr  = jStr
       pltBase = myYGlobalLo
       pltStep = sNy
       pltLab  = 'J ='
      ENDIF
C-    check if it fits into buffer (-10 should be enough but -12 is safer):
      IF ( (accMax-accMin+1)*(accBhi-accBlo+1).GT.MAX_LEN_PLOTBUF-12
     &     .AND. validRange ) THEN
       msgBuf =
     &  'Model domain too big to print to terminal - skipping I/O'
       CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       validRange = .FALSE.
      ENDIF
      IF ( validRange ) THEN
C      Header
C      Data
       DO bk=pltBlo, pltBhi, pltBstr
        DO K=pltMin,pltMax,pltStr
         WRITE(plotBuf,'(A,I4,I4,I4,I4)') pltLab,
     &   pltBase-1+(bk-1)*pltStep+K
         CALL PRINT_MESSAGE(plotBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, 1)
         plotBuf = ' '
         iBuf = 6
         DO bi=accBlo, accBhi, accBstr
          DO I=accMin, accMax, accStr
           iDx = accBase-1+(bi-1)*accStep+I
           iBuf = iBuf + 1
           IF ( 10*((iBuf-6)/10) .EQ. iBuf-6 ) THEN
            IF ( iDx .LT. 10 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I1)') accLab,iDx
            ELSEIF ( iDx .LT. 100 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I2)') accLab,iDx
            ELSEIF ( iDx .LT. 1000 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I3)') accLab,iDx
            ELSEIF ( iDx .LT. 10000 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I4)') accLab,iDx
            ENDIF
           ENDIF
          ENDDO
         ENDDO
         WRITE(msgBuf,'(A,A)') '// ',plotBuf
         CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, 1)
         plotBuf = dwnLab
         iBuf = 7
         DO bi=accBlo, accBhi, accBstr
          DO I=accMin, accMax, accStr
           iDx = accBase-1+(bi-1)*accStep+I
           iBuf = iBuf+1
           IF ( 10*((iBuf-7)/10) .EQ. iBuf-7 ) THEN
            WRITE(plotBuf(iBuf:),'(A)')  '|'
           ELSE
            WRITE(plotBuf(iBuf:iBuf),'(I1)') MOD(ABS(iDx),10)
           ENDIF
          ENDDO
         ENDDO
         WRITE(msgBuf,'(A,A)') '// ',plotBuf
         CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, 1)
         DO bj=dwnBlo, dwnBhi, dwnBStr
          DO J=dwnMin, dwnMax, dwnStr
           WRITE(plotBuf,'(1X,I5,1X)')
     &      dwnBase-1+(bj-1)*dwnStep+J
           iBuf = 7
           DO bi=accBlo,accBhi,accBstr
            DO I=accMin,accMax,accStr
             iBuf = iBuf + 1
             IF     ( plotMode .EQ. PRINT_MAP_XY ) THEN
              val = fld(I,J,K,bi,bj)
             ELSEIF ( plotMode .EQ. PRINT_MAP_XZ ) THEN
              val = fld(I,K,J,bi,bk)
             ELSEIF ( plotMode .EQ. PRINT_MAP_YZ ) THEN
              val = fld(K,I,J,bk,bi)
             ENDIF
             IF ( validRange .AND. val .NE. 0. ) THEN
              IDX = NINT(
     &              FLOAT( lChList-1 )*( val-fMin ) / (fRange)
     &             )+1
             ELSE
              IDX = 1
             ENDIF
             IF ( iBuf .LE. MAX_LEN_PLOTBUF )
     &        plotBuf(iBuf:iBuf) = chList(IDX:IDX)
             IF ( val .EQ. 0. ) THEN
              IF ( iBuf .LE. MAX_LEN_PLOTBUF )
     &         plotBuf(iBuf:iBuf) = '.'
             ENDIF
            ENDDO
           ENDDO
           WRITE(msgBuf,'(A,A)') '// ',plotBuf
           CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, 1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C--   Write delimiter
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// END OF FIELD                                          ='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf = ' '
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PRINT_MAPRL
C     !INTERFACE:
      SUBROUTINE PRINT_MAPRL ( fld, fldTitle, plotMode,
     I        iLo,   iHi,   jLo,   jHi,  kLo,  kHi, nBx, nBy,
     I       iMin,  iMax,  iStr,
     I       jMin,  jMax,  jStr,
     I       kMin, kMax,   kStr,
     I      bxMin, bxMax,  bxStr,
     I      byMin, byMax,  byStr )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE PRINT\_MAPRL
C     | o Does textual mapping printing of a field.
C     *==========================================================*
C     | This routine does the actual formatting of the data
C     | and printing to a file. It assumes an array using the
C     | MITgcm UV indexing scheme and base index variables.
C     | User code should call an interface routine like
C     | PLOT\_FIELD\_XYRL( ... ) rather than this code directly.
C     | Text plots can be oriented XY, YZ, XZ. An orientation
C     | is specficied through the "plotMode" argument. All the
C     | plots made by a single call to this routine will use the
C     | same contour interval. The plot range (iMin,...,byStr)
C     | can be three-dimensional. A separate plot is made for
C     | each point in the plot range normal to the orientation.
C     | e.g. if the orientation is XY (plotMode = PRINT\_MAP\_XY).
C     |      kMin =1, kMax = 5 and kStr = 2 will produce three XY
C     |      plots - one for K=1, one for K=3 and one for K=5.
C     |      Each plot would have extents iMin:iMax step iStr
C     |      and jMin:jMax step jStr.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

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


C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     fld        - Real*8 array holding data to be plotted
C     fldTitle   - Name of field to be plotted
C     plotMode   - Text string indicating plot orientation
C                  ( see - EEPARAMS.h for valid values ).
C     iLo, iHi,  - Dimensions of array fld. fld is assumed to
C     jLo, jHi     be five-dimensional.
C     kLo, kHi
C     nBx, nBy
C     iMin, iMax - Indexing for points to plot. Points from
C     iStr         iMin -> iMax in steps of iStr are plotted
C     jMin. jMax   and similarly for jMin, jMax, jStr and
C     jStr         kMin, kMax, kStr and bxMin, bxMax, bxStr
C     kMin, kMax   byMin, byMax, byStr.
C     kStr
      CHARACTER*(*) fldTitle
      CHARACTER*(*) plotMode
      INTEGER iLo, iHi
      INTEGER jLo, jHi
      INTEGER kLo, kHi
      INTEGER nBx, nBy
      Real*8 fld(iLo:iHi,jLo:jHi,kLo:kHi,nBx,nBy)
      INTEGER iMin, iMax, iStr
      INTEGER jMin, jMax, jStr
      INTEGER kMin, kMax, kStr
      INTEGER bxMin, bxMax, bxStr
      INTEGER byMin, byMax, byStr

C     !FUNCTIONS:
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     plotBuf - Buffer for building plot record
C     chList  - Character string used for plot
C     fMin, fMax - Contour min, max and range
C     fRange
C     val     - Value of element to be "plotted"
C     small   - Lowest range for which contours are plotted
C     accXXX  - Variables used in indexing accross page records.
C     dwnXXX    Variables used in indexing down the page.
C     pltXXX    Variables used in indexing multiple plots ( multiple
C               plots use same contour range).
C               Lab  - Label
C               Base - Base number for element indexing
C                      The process bottom, left coordinate in the
C                      global domain.
C               Step - Block size
C               Blo  - Start block
C               Bhi  - End block
C               Bstr - Block stride
C               Min  - Start index within block
C               Max  - End index within block
C               Str  - stride within block
      INTEGER MAX_LEN_PLOTBUF
      PARAMETER ( MAX_LEN_PLOTBUF = MAX_LEN_MBUF-20 )
      CHARACTER*(MAX_LEN_PLOTBUF) plotBuf
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER lChList
      PARAMETER ( lChList = 28 )
      CHARACTER*(lChList) chList
      Real*8  fMin
      Real*8  fMax
      Real*8  fRange
      Real*8  val
      Real*8  small
      CHARACTER*2  accLab
      CHARACTER*7  dwnLab
      CHARACTER*3  pltLab
      INTEGER     accBase, dwnBase, pltBase
      INTEGER     accStep, dwnStep, pltStep
      INTEGER     accBlo,  dwnBlo,  pltBlo
      INTEGER     accBhi,  dwnBhi,  pltBhi
      INTEGER     accBstr, dwnBstr, pltBstr
      INTEGER     accMin,  dwnMin,  pltMin
      INTEGER     accMax,  dwnMax,  pltMax
      INTEGER     accStr,  dwnStr,  pltStr
      INTEGER I, J, K, iStrngLo, iStrngHi, iBuf, iDx
      INTEGER bi, bj, bk
      LOGICAL validRange
CEOP

      chList = '-abcdefghijklmnopqrstuvwxyz+'
      small  = 1.D-15
      fMin   =  1.D32
      fMax   = -1.D32
      validRange = .FALSE.

C--   Calculate field range
      DO bj=byMin, byMax, byStr
       DO bi=bxMin, bxMax, bxStr
        DO K=kMin, kMax, kStr
         DO J=jMin, jMax, jStr
          DO I=iMin, iMax, iStr
           IF ( printMapIncludesZeros .OR. fld(I,J,K,bi,bj) .NE. 0. )
     &     THEN
            IF ( fld(I,J,K,bi,bj) .LT. fMin )
     &       fMin = fld(I,J,K,bi,bj)
            IF ( fld(I,J,K,bi,bj) .GT. fMax )
     &       fMax = fld(I,J,K,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      fRange = fMax-fMin
      IF ( fRange .GT. small ) validRange = .TRUE.

C--   Write field title and statistics
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      iStrngLo = IFNBLNK(fldTitle)
      iStrngHi = ILNBLNK(fldTitle)
      IF ( iStrngLo .LE. iStrngHi ) THEN
       WRITE(msgBuf,'(A)') fldTitle(iStrngLo:iStrngHi)
      ELSE
       msgBuf = '// UNKNOWN FIELD'
      ENDIF
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1PE30.15)')
     & '// CMIN = ', fMin
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1PE30.15)')
     & '// CMAX = ', fMax
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      IF ( validRange ) THEN
       WRITE(msgBuf,'(A,1PE30.15)')
     & '// CINT = ', fRange/FLOAT(lChlist-1)
      ELSE
       WRITE(msgBuf,'(A,1PE30.15)')
     & '// CINT = ', 0.
      ENDIF
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1024A1)')
     & '// SYMBOLS (CMIN->CMAX): ',(chList(I:I),I=1,lChList)
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      WRITE(msgBuf,'(A,1024A1)')
     & '//                  0.0: ','.'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I6),A)')
     & '// RANGE I (Lo:Hi:Step):',
     &  '(',myXGlobalLo-1+(bxMin-1)*sNx+iMin,
     &  ':',myXGlobalLo-1+(bxMax-1)*sNx+iMax,
     &  ':',iStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I6),A)')
     & '// RANGE J (Lo:Hi:Step):',
     &  '(',myYGlobalLo-1+(byMin-1)*sNy+jMin,
     &  ':',myYGlobalLo-1+(byMax-1)*sNy+jMax,
     &  ':',jStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A,3(A,I4),A)')
     & '// RANGE K (Lo:Hi:Step):',
     &  '(',kMin,
     &  ':',kMax,
     &  ':',kStr,')'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)

c     if (Nx.gt.MAX_LEN_PLOTBUF-20) THEN
c      msgBuf =
c    &  'Model domain too big to print to terminal - skipping I/O'
c      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
c    &                   SQUEEZE_RIGHT, 1)
c      RETURN
c     endif

C--   Write field
C     Figure out slice type and set plotting parameters appropriately
C     acc = accross the page
C     dwn = down the page
      IF ( plotMode .EQ. PRINT_MAP_XY ) THEN
C      X across, Y down slice
       accLab  = 'I='
       accBase = myXGlobalLo
       accStep = sNx
       accBlo  = bxMin
       accBhi  = bxMax
       accBStr = bxStr
       accMin  = iMin
       accMax  = iMax
       accStr  = iStr
       dwnLab  = '|--J--|'
       dwnBase = myYGlobalLo
       dwnStep = sNy
       dwnBlo  = byMin
       dwnBhi  = byMax
       dwnBStr = byStr
       dwnMin  = jMin
       dwnMax  = jMax
       dwnStr  = jStr
       pltBlo  = 1
       pltBhi  = 1
       pltBstr = 1
       pltMin  = kMin
       pltMax  = kMax
       pltStr  = kStr
       pltBase = 1
       pltStep = 1
       pltLab  = 'K ='
      ELSEIF ( plotMode .EQ. PRINT_MAP_YZ ) THEN
C      Y across, Z down slice
       accLab  = 'J='
       accBase = myYGlobalLo
       accStep = sNy
       accBlo  = byMin
       accBhi  = byMax
       accBStr = byStr
       accMin  = jMin
       accMax  = jMax
       accStr  = jStr
       dwnLab  = '|--K--|'
       dwnBase = 1
       dwnStep = 1
       dwnBlo  = 1
       dwnBhi  = 1
       dwnBStr = 1
       dwnMin  = kMin
       dwnMax  = kMax
       dwnStr  = kStr
       pltBlo  = bxMin
       pltBhi  = bxMax
       pltBstr = bxStr
       pltMin  = iMin
       pltMax  = iMax
       pltStr  = iStr
       pltBase = myXGlobalLo
       pltStep = sNx
       pltLab  = 'I ='
      ELSEIF ( plotMode .EQ. PRINT_MAP_XZ ) THEN
C      X across, Z down slice
       accLab  = 'I='
       accBase = myXGlobalLo
       accStep = sNx
       accBlo  = bxMin
       accBhi  = bxMax
       accBStr = bxStr
       accMin  = iMin
       accMax  = iMax
       accStr  = iStr
       dwnLab  = '|--K--|'
       dwnBase = 1
       dwnStep = 1
       dwnBlo  = 1
       dwnBhi  = 1
       dwnBStr = 1
       dwnMin  = kMin
       dwnMax  = kMax
       dwnStr  = kStr
       pltBlo  = byMin
       pltBhi  = byMax
       pltBstr = byStr
       pltMin  = jMin
       pltMax  = jMax
       pltStr  = jStr
       pltBase = myYGlobalLo
       pltStep = sNy
       pltLab  = 'J ='
      ENDIF
C-    check if it fits into buffer (-10 should be enough but -12 is safer):
      IF ( (accMax-accMin+1)*(accBhi-accBlo+1).GT.MAX_LEN_PLOTBUF-12
     &     .AND. validRange ) THEN
       msgBuf =
     &  'Model domain too big to print to terminal - skipping I/O'
       CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
       validRange = .FALSE.
      ENDIF
      IF ( validRange ) THEN
C      Header
C      Data
       DO bk=pltBlo, pltBhi, pltBstr
        DO K=pltMin,pltMax,pltStr
         WRITE(plotBuf,'(A,I4,I4,I4,I4)') pltLab,
     &   pltBase-1+(bk-1)*pltStep+K
         CALL PRINT_MESSAGE(plotBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, 1)
         plotBuf = ' '
         iBuf = 6
         DO bi=accBlo, accBhi, accBstr
          DO I=accMin, accMax, accStr
           iDx = accBase-1+(bi-1)*accStep+I
           iBuf = iBuf + 1
           IF ( 10*((iBuf-6)/10) .EQ. iBuf-6 ) THEN
            IF ( iDx .LT. 10 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I1)') accLab,iDx
            ELSEIF ( iDx .LT. 100 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I2)') accLab,iDx
            ELSEIF ( iDx .LT. 1000 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I3)') accLab,iDx
            ELSEIF ( iDx .LT. 10000 ) THEN
             WRITE(plotBuf(iBuf:),'(A,I4)') accLab,iDx
            ENDIF
           ENDIF
          ENDDO
         ENDDO
         CALL PRINT_MESSAGE(plotBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, 1)
         plotBuf = dwnLab
         iBuf = 7
         DO bi=accBlo, accBhi, accBstr
          DO I=accMin, accMax, accStr
           iDx = accBase-1+(bi-1)*accStep+I
           iBuf = iBuf+1
           IF ( 10*((iBuf-7)/10) .EQ. iBuf-7 ) THEN
            WRITE(plotBuf(iBuf:),'(A)')  '|'
           ELSE
            WRITE(plotBuf(iBuf:iBuf),'(I1)') MOD(ABS(iDx),10)
           ENDIF
          ENDDO
         ENDDO
         CALL PRINT_MESSAGE(plotBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, 1)
         DO bj=dwnBlo, dwnBhi, dwnBStr
          DO J=dwnMin, dwnMax, dwnStr
           WRITE(plotBuf,'(1X,I5,1X)')
     &      dwnBase-1+(bj-1)*dwnStep+J
           iBuf = 7
           DO bi=accBlo,accBhi,accBstr
            DO I=accMin,accMax,accStr
             iBuf = iBuf + 1
             IF     ( plotMode .EQ. PRINT_MAP_XY ) THEN
              val = fld(I,J,K,bi,bj)
             ELSEIF ( plotMode .EQ. PRINT_MAP_XZ ) THEN
              val = fld(I,K,J,bi,bk)
             ELSEIF ( plotMode .EQ. PRINT_MAP_YZ ) THEN
              val = fld(K,I,J,bk,bi)
             ENDIF
             IF ( validRange .AND. val .NE. 0. ) THEN
              IDX = NINT(
     &               FLOAT( lChList-1 )*( val-fMin ) / (fRange)
     &              )+1
             ELSE
              IDX = 1
             ENDIF
             IF ( iBuf .LE. MAX_LEN_PLOTBUF )
     &        plotBuf(iBuf:iBuf) = chList(IDX:IDX)
             IF ( val .EQ. 0. ) THEN
              IF ( iBuf .LE. MAX_LEN_PLOTBUF )
     &         plotBuf(iBuf:iBuf) = '.'
             ENDIF
            ENDDO
           ENDDO
           CALL PRINT_MESSAGE(plotBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, 1)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C--   Write delimiter
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// END OF FIELD                                          ='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf =
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)
      msgBuf = ' '
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, 1)

      RETURN
      END

