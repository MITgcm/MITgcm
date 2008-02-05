C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/DIAGNOSTICS.h,v 1.14 2008/02/05 15:13:01 jmc Exp $
C $Name:  $

C ======================================================================
C  Common blocks for diagnostics package.
C  - DIAG_DEFINE contains the definition of all available diagnostics
C        ndiagt :: total number of available diagnostics
C         kdiag :: number of levels associated with the diagnostic
C         cdiag :: list of available diagnostic names
C         tdiag :: description of field in diagnostic
C         gdiag :: parser field with characteristics of the diagnostics
C         hdiag :: mate number (in available diag. list) of the diagnostic
C         udiag :: physical units of the diagnostic field
C  - DIAG_STORE  contains the large array to store diagnostic fields
C         qdiag :: storage array for 2D/3D diagnostic fields
C        qSdiag :: storage array for diagnostics of (per level) statistics
C         ndiag :: holds number of times a diagnostic is filled (for time-mean diag)
C  - DIAG_SELECT  contains the user selection of diagnostics to write
C         idiag :: slot number in large diagnostic array
C         mdiag :: slot number in large diagnostic array for the mate
C         jdiag :: short-list (active diag.) to long-list (available diag.)
C                  pointer
C  - DIAG_STATIS  contains the user selection of statistics to write
C ======================================================================

C--   DIAG_DEFINE common block:
C       ndiagt :: total number of available diagnostics
C       kdiag  :: number of levels associated with the diagnostic
C       cdiag  :: list of available diagnostic names
C       tdiag  :: description of field in diagnostic
C       gdiag  :: parser field with characteristics of the diagnostics
C       hdiag  :: mate number (in available diag. list) of the diagnostic
C       udiag  :: physical units of the diagnostic field

      INTEGER        ndiagt
      INTEGER        kdiag(ndiagMax)
      INTEGER        hdiag(ndiagMax)
      CHARACTER*8    cdiag(ndiagMax)
      CHARACTER*80   tdiag(ndiagMax)
      CHARACTER*16   gdiag(ndiagMax)
      CHARACTER*16   udiag(ndiagMax)

      COMMON / DIAG_DEFINE /
     &  ndiagt, kdiag, hdiag,
     &  cdiag, tdiag, gdiag, udiag

C--   DIAG_STORE common block:
C       qdiag  :: storage array for 2D/3D diagnostic fields
C       qSdiag :: storage array for (per level) statistics
C       ndiag  :: holds number of times a diagnostic is filled (for time-mean diag)
C       pdiag  :: index of current averaging interval within the averaging-cycle

      _RL qdiag(1-OLx:sNx+Olx,1-Oly:sNy+Oly,numDiags,nSx,nSy)
      _RL qSdiag(0:nStats,0:nRegions,diagSt_size,nSx,nSy)
      INTEGER  ndiag(numDiags,nSx,nSy)
      INTEGER  pdiag(numlists,nSx,nSy)

      COMMON / DIAG_STORE / qdiag, qSdiag, ndiag, pdiag

	
C--   DIAG_SELECT common block:
C     freq        :: frequency (in s) to write output stream # n
C     phase       :: phase     (in s) to write output stream # n
C     averageFreq :: frequency (in s) for periodic averaging interval
C     averagePhase:: phase     (in s) for periodic averaging interval
C     averageCycle:: number of averaging intervals in 1 cycle
C     nfields(n)  :: number of active diagnostics for output stream # n
C     nActive(n)  :: number of active diagnostics (including counters)
C                    for output stream # n
C     nlists      :: effective number of output streams
C     idiag       :: slot number in large diagnostic array
C     mdiag       :: slot number in large diagnostic array for the mate
C     jdiag       :: short-list (active diag.) to long-list (available diag.) pointer
C     flds(:,n)   :: list of field names in output stream # n
C     fnames(n)   :: output file name for output stream # n
C     fflags(n)   :: character string with per-file flags

      _RL freq(numlists), phase(numlists)
      _RL levs (numLevels,numlists)
      _RL averageFreq(numlists), averagePhase(numlists)
      INTEGER averageCycle(numlists)
      INTEGER nlevels(numlists)
      INTEGER nfields(numlists)
      INTEGER nActive(numlists)
      INTEGER nlists
      INTEGER idiag(numperlist,numlists)
      INTEGER mdiag(numperlist,numlists)
      INTEGER jdiag(numperlist,numlists)
      CHARACTER*8 flds (numperlist,numlists)
      CHARACTER*80 fnames(numlists)
      CHARACTER*8 fflags(numlists)
      LOGICAL dumpAtLast, diag_mdsio,  diag_mnc
      LOGICAL diag_pickup_read,        diag_pickup_write
      LOGICAL diag_pickup_read_mdsio,  diag_pickup_write_mdsio
      LOGICAL diag_pickup_read_mnc,    diag_pickup_write_mnc

      COMMON / DIAG_SELECT /
     &     freq, phase, levs,
     &     averageFreq, averagePhase, averageCycle,
     &     nlevels, nfields, nActive, nlists,
     &     idiag, mdiag, jdiag,
     &     dumpAtLast, diag_mdsio, diag_mnc,
     &     diag_pickup_read,        diag_pickup_write,
     &     diag_pickup_read_mdsio,  diag_pickup_write_mdsio,
     &     diag_pickup_read_mnc,    diag_pickup_write_mnc,
     &     flds, fnames, fflags

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   DIAG_STATIS common block:
C     iSdiag :: slot number in large diagnostic array
C     mSdiag :: slot number in large diagnostic array for the mate
C     jSdiag :: short-list (active diag.) to long-list (available diag.)
C               pointer
C     diagSt_region(j,n) :: flag to perform (=1) or not (=0) regional-statistics
C                           over region # j for output stream # n
C     diagSt_freq(n)   :: frequency (in s) to write output stream # n
C     diagSt_phase(n)  :: phase     (in s) to write output stream # n
C     diagSt_nbFlds(n) :: number of active diagnostics for output stream # n
C     diagSt_nbActv(n) :: number of active diagnostics (including counters)
C                         for output stream # n
C     diagSt_nbLists   :: effective number of output streams
C     diagSt_ioUnit(n) :: fortran IO unit for output stream # n (ascii output)
C     diagSt_Flds(:,n) :: list of field names in output stream # n
C     diagSt_Fname(n)  :: output file name for output stream # n

      _RL       diagSt_freq(numlists), diagSt_phase(numlists)
      CHARACTER*8  diagSt_Flds(numperlist,numlists)
      CHARACTER*80 diagSt_Fname(numlists)
      INTEGER   iSdiag(numperlist,numlists)
      INTEGER   jSdiag(numperlist,numlists)
      INTEGER   mSdiag(numperlist,numlists)
      INTEGER   diagSt_region(0:nRegions,numlists)
      INTEGER   diagSt_nbFlds(numlists)
      INTEGER   diagSt_nbActv(numlists)
      INTEGER   diagSt_nbLists
      INTEGER   diagSt_ioUnit(numlists)
      LOGICAL   diagSt_ascii, diagSt_mnc
      COMMON / DIAG_STATIS /
     &     diagSt_freq, diagSt_phase,
     &     iSdiag, jSdiag, mSdiag, diagSt_region,
     &     diagSt_nbFlds, diagSt_nbActv, diagSt_nbLists,
     &     diagSt_ioUnit,
     &     diagSt_Ascii, diagSt_mnc,
     &     diagSt_Flds, diagSt_Fname

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
