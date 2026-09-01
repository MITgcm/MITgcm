!BOP
! !ROUTINE: EEPARAMS.h
! !INTERFACE:
! include "EEPARAMS.h"
!
! !DESCRIPTION:
! *==========================================================*
! | EEPARAMS.h                                               |
! *==========================================================*
! | Parameters for "execution environemnt". These are used   |
! | by both the particular numerical model and the execution |
! | environment support routines.                            |
! *==========================================================*
!EOP

! ========  EESIZE.h  ========================================

! MAX_LEN_MBUF  :: Default message buffer max. size
! MAX_LEN_FNAM  :: Default file name max. size
! MAX_LEN_PREC  :: Default rec len for reading "parameter" files

      INTEGER :: MAX_LEN_MBUF
      PARAMETER ( MAX_LEN_MBUF = 512 )
      INTEGER :: MAX_LEN_FNAM
      PARAMETER ( MAX_LEN_FNAM = 512 )
      INTEGER :: MAX_LEN_PREC
      PARAMETER ( MAX_LEN_PREC = 200 )

! MAX_NO_THREADS  :: Maximum number of threads allowed.
! GSVec_size      :: Maximum buffer size for Global Sum Vector array
      INTEGER :: MAX_NO_THREADS
      PARAMETER ( MAX_NO_THREADS =  4 )
      INTEGER :: GSVec_size
      PARAMETER ( GSVec_size = 1024 )

! Particularly weird and obscure voodoo numbers
! lShare :: This wants to be the length in
!           [148]-byte words of the size of
!           the address "window" that is snooped
!           on an SMP bus. By separating elements in
!           the global sum buffer we can avoid generating
!           extraneous invalidate traffic between
!           processors. The length of this window is usually
!           a cache line i.e. small O(64 bytes).
!           The buffer arrays are usually short arrays
!           and are declared REAL ARRA(lShare[148],LBUFF).
!           Setting lShare[148] to 1 is like making these arrays
!           one dimensional.
      INTEGER :: cacheLineSize
      INTEGER :: lShare1
      INTEGER :: lShare4
      INTEGER :: lShare8
      PARAMETER ( cacheLineSize = 256 )
      PARAMETER ( lShare1 =  cacheLineSize )
      PARAMETER ( lShare4 =  cacheLineSize/4 )
      PARAMETER ( lShare8 =  cacheLineSize/8 )

! ========  EESIZE.h  ========================================

! Symbolic values
! precXXXX :: precision used for I/O
      INTEGER :: precFloat32
      PARAMETER ( precFloat32 = 32 )
      INTEGER :: precFloat64
      PARAMETER ( precFloat64 = 64 )

! Real-type constant for some frequently used simple number (0,1,2,1/2):
      _RS     zeroRS, oneRS, twoRS, halfRS
      PARAMETER ( zeroRS = 0.0 _d 0 , oneRS  = 1.0 _d 0 )
      PARAMETER ( twoRS  = 2.0 _d 0 , halfRS = 0.5 _d 0 )
      _RL     zeroRL, oneRL, twoRL, halfRL
      PARAMETER ( zeroRL = 0.0 _d 0 , oneRL  = 1.0 _d 0 )
      PARAMETER ( twoRL  = 2.0 _d 0 , halfRL = 0.5 _d 0 )

! UNSET_xxx :: Used to indicate variables that have not been given a value
      Real(kind=8) :: UNSET_FLOAT8
      PARAMETER ( UNSET_FLOAT8 = 1.234567D5 )
      Real(kind=4) :: UNSET_FLOAT4
      PARAMETER ( UNSET_FLOAT4 = 1.234567E5 )
      _RL     UNSET_RL
      PARAMETER ( UNSET_RL     = 1.234567D5 )
      _RS     UNSET_RS
      PARAMETER ( UNSET_RS     = 1.234567D5 )
      INTEGER :: UNSET_I
      PARAMETER ( UNSET_I      = 123456789  )

! debLevX  :: used to decide when to print debug messages
      INTEGER :: debLevZero
      INTEGER :: debLevA, debLevB,  debLevC, debLevD, debLevE
      PARAMETER ( debLevZero=0 )
      PARAMETER ( debLevA=1 )
      PARAMETER ( debLevB=2 )
      PARAMETER ( debLevC=3 )
      PARAMETER ( debLevD=4 )
      PARAMETER ( debLevE=5 )

! SQUEEZE_RIGHT      :: Flag indicating right blank space removal
!                       from text field.
! SQUEEZE_LEFT       :: Flag indicating left blank space removal
!                       from text field.
! SQUEEZE_BOTH       :: Flag indicating left and right blank
!                       space removal from text field.
! PRINT_MAP_XY       :: Flag indicating to plot map as XY slices
! PRINT_MAP_XZ       :: Flag indicating to plot map as XZ slices
! PRINT_MAP_YZ       :: Flag indicating to plot map as YZ slices
! commentCharacter   :: Variable used in column 1 of parameter
!                       files to indicate comments.
! INDEX_I            :: Variable used to select an index label
! INDEX_J               for formatted input parameters.
! INDEX_K
! INDEX_NONE
      CHARACTER(len=*) :: SQUEEZE_RIGHT
      PARAMETER ( SQUEEZE_RIGHT = 'R' )
      CHARACTER(len=*) :: SQUEEZE_LEFT
      PARAMETER ( SQUEEZE_LEFT = 'L' )
      CHARACTER(len=*) :: SQUEEZE_BOTH
      PARAMETER ( SQUEEZE_BOTH = 'B' )
      CHARACTER(len=*) :: PRINT_MAP_XY
      PARAMETER ( PRINT_MAP_XY = 'XY' )
      CHARACTER(len=*) :: PRINT_MAP_XZ
      PARAMETER ( PRINT_MAP_XZ = 'XZ' )
      CHARACTER(len=*) :: PRINT_MAP_YZ
      PARAMETER ( PRINT_MAP_YZ = 'YZ' )
      CHARACTER(len=*) :: commentCharacter
      PARAMETER ( commentCharacter = '#' )
      INTEGER :: INDEX_I
      INTEGER :: INDEX_J
      INTEGER :: INDEX_K
      INTEGER :: INDEX_NONE
      PARAMETER ( INDEX_I    = 1,                                                 &
     &      INDEX_J    = 2,                                                       &
     &      INDEX_K    = 3,                                                       &
     &      INDEX_NONE = 4 )

! EXCH_IGNORE_CORNERS :: Flag to select ignoring or
! EXCH_UPDATE_CORNERS    updating of corners during an edge exchange.
      INTEGER :: EXCH_IGNORE_CORNERS
      INTEGER :: EXCH_UPDATE_CORNERS
      PARAMETER ( EXCH_IGNORE_CORNERS = 0,                                        &
     &      EXCH_UPDATE_CORNERS = 1 )

! FORWARD_SIMULATION
! REVERSE_SIMULATION
! TANGENT_SIMULATION
      INTEGER :: FORWARD_SIMULATION
      INTEGER :: REVERSE_SIMULATION
      INTEGER :: TANGENT_SIMULATION
      PARAMETER ( FORWARD_SIMULATION = 0,                                         &
     &      REVERSE_SIMULATION = 1,                                               &
     &      TANGENT_SIMULATION = 2 )

!--   COMMON /EEPARAMS_L/ Execution environment public logical variables.
! eeBootError    :: Flags indicating error during multi-processing
! eeEndError     :: initialisation and termination.
! fatalError     :: Flag used to indicate that the model is ended with an error
! debugMode      :: controls printing of debug msg (sequence of S/R calls).
! useSingleCpuIO :: When useSingleCpuIO is set, MDS_WRITE_FIELD outputs from
!                   master MPI process only. -- NOTE: read from main parameter
!                   file "data" and not set until call to INI_PARMS.
! useSingleCpuInput :: When useSingleCpuInput is set, EXF_INTERP_READ
!                   reads forcing files from master MPI process only.
!                   -- NOTE: read from main parameter file "data"
!                      and defaults to useSingleCpuInput = useSingleCpuIO
! printMapIncludesZeros  :: Flag that controls whether character constant
!                           map code ignores exact zero values.
! useCubedSphereExchange :: use Cubed-Sphere topology domain.
! useCoupler     :: use Coupler for a multi-components set-up.
! useNEST_PARENT :: use Parent Nesting interface (pkg/nest_parent)
! useNEST_CHILD  :: use Child  Nesting interface (pkg/nest_child)
! useNest2W_parent :: use Parent 2-W Nesting interface (pkg/nest2w_parent)
! useNest2W_child  :: use Child  2-W Nesting interface (pkg/nest2w_child)
! useOASIS       :: use OASIS-coupler for a multi-components set-up.
      COMMON /EEPARAMS_L/                                                         &
!    &  eeBootError, fatalError, eeEndError,
     &      eeBootError, eeEndError, fatalError, debugMode,                       &
     &      useSingleCpuIO, useSingleCpuInput, printMapIncludesZeros,             &
     &      useCubedSphereExchange, useCoupler,                                   &
     &      useNEST_PARENT, useNEST_CHILD,                                        &
     &      useNest2W_parent, useNest2W_child, useOASIS,                          &
     &      useSETRLSTK, useSIGREG
      LOGICAL :: eeBootError
      LOGICAL :: eeEndError
      LOGICAL :: fatalError
      LOGICAL :: debugMode
      LOGICAL :: useSingleCpuIO
      LOGICAL :: useSingleCpuInput
      LOGICAL :: printMapIncludesZeros
      LOGICAL :: useCubedSphereExchange
      LOGICAL :: useCoupler
      LOGICAL :: useNEST_PARENT
      LOGICAL :: useNEST_CHILD
      LOGICAL :: useNest2W_parent
      LOGICAL :: useNest2W_child
      LOGICAL :: useOASIS
      LOGICAL :: useSETRLSTK
      LOGICAL :: useSIGREG

!--   COMMON /EPARAMS_I/ Execution environment public integer variables.
! errorMessageUnit    :: Fortran IO unit for error messages
! standardMessageUnit :: Fortran IO unit for informational messages
! maxLengthPrt1D :: maximum length for printing (to Std-Msg-Unit) 1-D array
! scrUnit1      :: Scratch file 1 unit number
! scrUnit2      :: Scratch file 2 unit number
! eeDataUnit    :: Unit # for reading "execution environment" parameter file
! modelDataUnit :: Unit number for reading "model" parameter file.
! numberOfProcs :: Number of processes computing in parallel
! pidIO         :: Id of process to use for I/O.
! myBxLo, myBxHi :: Extents of domain in blocks in X and Y
! myByLo, myByHi :: that each threads is responsble for.
! myProcId      :: My own "process" id.
! myPx          :: My X coord on the proc. grid.
! myPy          :: My Y coord on the proc. grid.
! myXGlobalLo   :: My bottom-left (south-west) x-index global domain.
!                  The x-coordinate of this point in for example m or
!                  degrees is *not* specified here. A model needs to
!                  provide a mechanism for deducing that information
!                  if it is needed.
! myYGlobalLo   :: My bottom-left (south-west) y-index in global domain.
!                  The y-coordinate of this point in for example m or
!                  degrees is *not* specified here. A model needs to
!                  provide a mechanism for deducing that information
!                  if it is needed.
! nThreads      :: No. of threads
! nTx, nTy      :: No. of threads in X and in Y
!                  This assumes a simple cartesian gridding of the threads
!                  which is not required elsewhere but that makes it easier
! ioErrorCount  :: IO Error Counter. Set to zero initially and increased
!                  by one every time an IO error occurs.
      COMMON /EEPARAMS_I/                                                         &
     &      errorMessageUnit, standardMessageUnit, maxLengthPrt1D,                &
     &      scrUnit1, scrUnit2, eeDataUnit, modelDataUnit,                        &
     &      numberOfProcs, pidIO, myProcId,                                       &
     &      myPx, myPy, myXGlobalLo, myYGlobalLo, nThreads,                       &
     &      myBxLo, myBxHi, myByLo, myByHi,                                       &
     &      nTx, nTy, ioErrorCount
      INTEGER :: errorMessageUnit
      INTEGER :: standardMessageUnit
      INTEGER :: maxLengthPrt1D
      INTEGER :: scrUnit1
      INTEGER :: scrUnit2
      INTEGER :: eeDataUnit
      INTEGER :: modelDataUnit
      INTEGER :: ioErrorCount(MAX_NO_THREADS)
      INTEGER :: myBxLo(MAX_NO_THREADS)
      INTEGER :: myBxHi(MAX_NO_THREADS)
      INTEGER :: myByLo(MAX_NO_THREADS)
      INTEGER :: myByHi(MAX_NO_THREADS)
      INTEGER :: myProcId
      INTEGER :: myPx
      INTEGER :: myPy
      INTEGER :: myXGlobalLo
      INTEGER :: myYGlobalLo
      INTEGER :: nThreads
      INTEGER :: nTx
      INTEGER :: nTy
      INTEGER :: numberOfProcs
      INTEGER :: pidIO

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
