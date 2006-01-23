C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/DIAGNOSTICS.h,v 1.12 2006/01/23 22:13:53 jmc Exp $
C $Name:  $

C ======================================================================
C  Common blocks for diagnostics package.
C  - diagarrays contains the master list of diagnostics and parameters
C        ndiagt :: total number of available diagnostics
C         kdiag - number of levels associated with the diagnostic
C         cdiag - character names
C         tdiag - description of field in diagnostic
C         gdiag - parser field with characteristics of the diagnostics
C         udiag - physical units of the diagnostic field
C  - diagnostics contains the large array containing diagnostic fields
C         qdiag - diagnostic fields array
C        qSdiag - storage array for diagnostics of (per level) statistics
C         ndiag - counter for number of times diagnostic is added
C  - diag_choices contains the user-chosen list of fields to store
C         idiag - slot number in large diagnostic array
C         mdiag - slot number in large diagnostic array for the mate
C         jdiag - short-list (active diag.) to long-list (available diag.)
C                 pointer
C  - diag_statis  contains the user-chosen list of statistics to store
C ======================================================================

C diagarrays common

      integer        ndiagt

      integer        kdiag(ndiagMax)
      character*8    cdiag(ndiagMax)
      character*80   tdiag(ndiagMax)
      character*16   gdiag(ndiagMax)
      character*16   udiag(ndiagMax)

      common /diagarrays/ ndiagt
      common /diagarrays/ kdiag
      common /diagarrays/ cdiag
      common /diagarrays/ tdiag
      common /diagarrays/ gdiag
      common /diagarrays/ udiag

C diagnostics common
C      qSdiag  - storage array for (per level) statistics

      _RL qdiag(1-OLx:sNx+Olx,1-Oly:sNy+Oly,numdiags,nSx,nSy)
      _RL qSdiag(0:nStats,0:nRegions,diagSt_size,nSx,nSy)
      integer  ndiag(numdiags,nSx,nSy)

      common /diagnostics/ qdiag, qSdiag, ndiag

	
C diag_choices common
C     freq       :: frequency (in s) to write output stream # n
C     phase      :: phase     (in s) to write output stream # n
C     nfields(n) :: number of active diagnostics for output stream # n
C     nActive(n) :: number of active diagnostics (including counters)
C                   for output stream # n
C     fflags(n)  :: character string with per-file flags

      integer nlists

      _RL freq(numlists), phase(numlists)
      _RL levs (numLevels,numlists)
      integer nlevels(numlists)
      integer nfields(numlists)
      integer nActive(numlists)
      integer idiag(numperlist,numlists)
      integer mdiag(numperlist,numlists)
      integer jdiag(numperlist,numlists)
      character*8 flds (numperlist,numlists)
      character*80 fnames(numlists)
      character*8 fflags(numlists)
      logical dumpatlast, diag_mdsio,  diag_mnc
      logical diag_pickup_read,        diag_pickup_write
      logical diag_pickup_read_mdsio,  diag_pickup_write_mdsio
      logical diag_pickup_read_mnc,    diag_pickup_write_mnc

      common /diag_choices/
     &     freq, phase, levs, nlevels,
     &     nfields, nActive, nlists,
     &     idiag, mdiag, jdiag,
     &     dumpatlast, diag_mdsio, diag_mnc,
     &     diag_pickup_read,        diag_pickup_write,
     &     diag_pickup_read_mdsio,  diag_pickup_write_mdsio,
     &     diag_pickup_read_mnc,    diag_pickup_write_mnc,
     &     flds, fnames, fflags

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C  DIAG_STATIS common block:
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
