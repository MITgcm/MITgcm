C ======================================================================
C  Common blocks for diagnostics package.
C  - DIAG_DEFINE contains the definition of all available diagnostics
C        ndiagt :: total number of available diagnostics
C         kdiag :: number of levels associated with the diagnostic
C         hdiag :: mate number (in available diag. list) of the diagnostic
C         cdiag :: list of available diagnostic names
C         gdiag :: parser field with characteristics of the diagnostics
C         tdiag :: description of field in diagnostic
C         udiag :: physical units of the diagnostic field
C  - DIAG_STORE  contains the large array to store diagnostic fields
C         qdiag :: storage array for 2D/3D diagnostic fields
C        qSdiag :: storage array for diagnostics of (per level) statistics
C         ndiag :: holds number of times a diagnostic is filled (for time-mean diag)
C  - DIAG_SELECT contains the user selection of diagnostics to write
C         idiag :: slot number in large diagnostic array
C         mdiag :: slot number in large diagnostic array for the mate
C         jdiag :: short-list (active diag.) to long-list (available diag.)
C                  pointer
C  - DIAG_PARAMS contains general parameters (used for both 2D/3D & Stats diags)
C  - DIAG_STATIS contains the user selection of statistics-diags to write
C ======================================================================

C--   DIAG_STATUS common block:
C  diag_pkgStatus  :: internal parameter to track status of this pkg settings
C             = -1 :: pkg is not used ;   = 1 :: user params are loaded
C             =  2 :: early initialisation is done (enable to add diags to list)
C             =  3 :: diagnostics setting is done (no more diags to add to list)
C             = 10 :: storage is initialised (init_varia)
C             = 20 :: ready for active section (filling diagnostics & output)
C             = 99 :: active section is over (end of the run)
C  ready2setDiags  :: pkgStatus level required to add any diagnostics to list
C  ready2fillDiags :: pkgStatus level required to fill any diagnostics
C  blkName         :: blank diagnostics name

      INTEGER  ready2setDiags, ready2fillDiags
      PARAMETER ( ready2setDiags = 2 , ready2fillDiags = 20 )
      CHARACTER*8 blkName
      PARAMETER ( blkName = '        ' )

      INTEGER  diag_pkgStatus
      COMMON / DIAG_STATUS_I /
     &  diag_pkgStatus

C--   DIAG_DEFINE common block:
C       ndiagt :: total number of available diagnostics
C       kdiag  :: number of levels associated with the diagnostic
C       hdiag  :: mate number (in available diag. list) of the diagnostic
C       cdiag  :: list of available diagnostic names
C       gdiag  :: parser field with characteristics of the diagnostics
C       tdiag  :: description of field in diagnostic
C       udiag  :: physical units of the diagnostic field

      INTEGER        ndiagt
      INTEGER        kdiag(ndiagMax)
      INTEGER        hdiag(ndiagMax)
      CHARACTER*8    cdiag(ndiagMax)
      CHARACTER*80   tdiag(ndiagMax)
      CHARACTER*16   gdiag(ndiagMax)
      CHARACTER*16   udiag(ndiagMax)

      COMMON / DIAG_DEFINE_I /
     &  ndiagt, kdiag, hdiag
      COMMON / DIAG_DEFINE_C /
     &  cdiag, gdiag, tdiag, udiag

C--   DIAG_STORE common block:
C       qdiag  :: storage array for 2D/3D diagnostic fields
C       qSdiag :: storage array for (per level) statistics
C       ndiag  :: holds number of times a diagnostic is filled (for time-mean diag)
C       pdiag  :: index of current averaging interval within the averaging-cycle

      _RL qdiag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,numDiags,nSx,nSy)
      _RL qSdiag(0:nStats,0:nRegions,diagSt_size,nSx,nSy)
      INTEGER  ndiag(numDiags,nSx,nSy)
      INTEGER  pdiag(numLists,nSx,nSy)

      COMMON / DIAG_STORE_R / qdiag, qSdiag
      COMMON / DIAG_STORE_I / ndiag, pdiag

C--   DIAG_SELECT common block:
C     freq(n)     :: frequency (in s) to write output stream # n
C     phase(n)    :: phase     (in s) to write output stream # n
C     averageFreq :: frequency (in s) for periodic averaging interval
C     averagePhase:: phase     (in s) for periodic averaging interval
C     averageCycle:: number of averaging intervals in 1 cycle
C     misValFlt(n):: missing value for floats   to use in output stream #n
Cc    misValInt(n):: missing value for integers to use in output stream #n
C     levs(:,n)   :: list of selected levels to write for output stream # n
C     nlevels(n)  :: number of levels to write for output stream # n
C     nfields(n)  :: number of active diagnostics for output stream # n
C     nActive(n)  :: number of active diagnostics (including counters)
C                    for output stream # n
C     nlists      :: effective number of output streams
C     idiag(:,n)  :: list of diag slot number in long-list of available diag.
C     mdiag(:,n)  :: list of mate slot number in long-list of available diag.
C     jdiag(:,n)  :: short-list (active diag.) to long-list (available diag.) pointer
C     flds(:,n)   :: list of field names in output stream # n
C     fnames(n)   :: output file name for output stream # n
C     fflags(n)   :: character string with per-file flags
C                 :: 1rst: file precision ('R','D' or ' ' to use default outp prec)
C                 :: 2nd: 'I'; integrate vertically ; 'P': interpolate vertically
C                 :: 3rd: 'h'; cumulate thickness weighted field (if permitted)
C useMissingValue :: put MissingValue where mask = 0 (NetCDF output only)

      _RL freq(numLists), phase(numLists)
      _RL averageFreq(numLists), averagePhase(numLists)
      _RL misValFlt(numLists)
      _RL levs (numLevels,numLists)
      INTEGER averageCycle(numLists)
c     INTEGER misValInt(numLists)
      INTEGER nlevels(numLists)
      INTEGER nfields(numLists)
      INTEGER nActive(numLists)
      INTEGER nlists
      INTEGER idiag(numperList,numLists)
      INTEGER mdiag(numperList,numLists)
      INTEGER jdiag(numperList,numLists)
      CHARACTER*8  flds  (numperList,numLists)
      CHARACTER*80 fnames(numLists)
      CHARACTER*8  fflags(numLists)
      LOGICAL diag_mdsio, diag_mnc, useMissingValue

      COMMON / DIAG_SELECT_R /
     &     freq, phase, averageFreq, averagePhase,
     &     misValFlt, levs
      COMMON / DIAG_SELECT_I /
     &     averageCycle,
     &     nlevels, nfields, nActive,
     &     nlists, idiag, mdiag, jdiag
c    &   , misValInt
      COMMON / DIAG_SELECT_C /
     &     flds, fnames, fflags
      COMMON / DIAG_SELECT_L /
     &     diag_mdsio, diag_mnc, useMissingValue

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C  - DIAG_PARAMS common block:
C    useDiag4AdjOutp :: use diagnostics pkg for Adjoint variables
C    diagLoc_ioUnit :: internal parameter: I/O unit for local diagnostics output
C    dumpAtLast :: always write time-ave (freq>0) diagnostics at the end of the run
C    diagMdsDir :: directory where diagnostics will be written when using mds
C    diagMdsDirCreate :: system call to mkdir to create diagMdsDir
      INTEGER diagLoc_ioUnit
      LOGICAL useDiag4AdjOutp
      LOGICAL dumpAtLast,              diagMdsDirCreate
      LOGICAL diag_pickup_read,        diag_pickup_write
      LOGICAL diag_pickup_read_mdsio,  diag_pickup_write_mdsio
      LOGICAL diag_pickup_read_mnc,    diag_pickup_write_mnc
      CHARACTER*(MAX_LEN_FNAM) diagMdsDir

      COMMON / DIAG_PARAMS_I /
     &     diagLoc_ioUnit
      COMMON / DIAG_PARAMS_L /      useDiag4AdjOutp,
     &     dumpAtLast,              diagMdsDirCreate,
     &     diag_pickup_read,        diag_pickup_write,
     &     diag_pickup_read_mdsio,  diag_pickup_write_mdsio,
     &     diag_pickup_read_mnc,    diag_pickup_write_mnc
      COMMON / DIAG_PARAMS_C /
     &     diagMdsDir

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   DIAG_STATIS common block:
C     diagSt_freq(n)   :: frequency (in s) to write output stream # n
C     diagSt_phase(n)  :: phase     (in s) to write output stream # n
C     iSdiag(:,n)      :: list of diag slot number in long-list of available diag.
C     mSdiag(:,n)      :: list of mate slot number in long-list of available diag.
C     jSdiag(:,n)      :: short-list (active diag.) to long-list (available
C                         diag.) pointer
C     diagSt_region(j,n) :: flag to perform (=1) or not (=0) regional-statistics
C                           over region # j for output stream # n
C     diagSt_nbFlds(n) :: number of active diagnostics for output stream # n
C     diagSt_nbActv(n) :: number of active diagnostics (including counters)
C                         for output stream # n
C     diagSt_nbLists   :: effective number of output streams
C     diagSt_ioUnit(n) :: fortran IO unit for output stream # n (ascii output)
C     diagSt_Flds(:,n) :: list of field names in output stream # n
C     diagSt_Fname(n)  :: output file name for output stream # n

      _RL       diagSt_freq(numLists), diagSt_phase(numLists)
      INTEGER   iSdiag(numperList,numLists)
      INTEGER   mSdiag(numperList,numLists)
      INTEGER   jSdiag(numperList,numLists)
      INTEGER   diagSt_region(0:nRegions,numLists)
      INTEGER   diagSt_nbFlds(numLists)
      INTEGER   diagSt_nbActv(numLists)
      INTEGER   diagSt_nbLists
      INTEGER   diagSt_ioUnit(numLists)
      CHARACTER*8  diagSt_Flds(numperList,numLists)
      CHARACTER*80 diagSt_Fname(numLists)
      LOGICAL   diagSt_ascii, diagSt_mnc

      COMMON / DIAG_STATIS_R /
     &     diagSt_freq, diagSt_phase
      COMMON / DIAG_STATIS_I /
     &     iSdiag, mSdiag, jSdiag, diagSt_region,
     &     diagSt_nbFlds, diagSt_nbActv, diagSt_nbLists,
     &     diagSt_ioUnit
      COMMON / DIAG_STATIS_C /
     &     diagSt_Flds, diagSt_Fname
      COMMON / DIAG_STATIS_L /
     &     diagSt_Ascii, diagSt_mnc

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
