#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_READPARMS

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_READPARMS( myThid )

C     !DESCRIPTION:
C     Read Diagnostics Namelists to specify output sequence.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
#include "DIAGNOSTICS_CALC.h"
#include "DIAGSTATS_REGIONS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      CHARACTER*(8) DIAGS_RENAMED
      EXTERNAL DIAGS_RENAMED
#ifdef ALLOW_FIZHI
      _RL      getcon
      EXTERNAL getcon
#endif

C     !LOCAL VARIABLES:
C     ldimLoc :: Max Number of Lists  (in data.diagnostics)
C     kdimLoc :: Max Number of Levels (in data.diagnostics)
C     fdimLoc :: Max Number of Fields (in data.diagnostics)
C     frequency :: Frequency (in s) of Output (ouput every "frequency" second)
C     timePhase :: phase (in s) within the "frequency" period to write output
C     averagingFreq  :: frequency (in s) for periodic averaging interval
C     averagingPhase :: phase     (in s) for periodic averaging interval
C     repeatCycle    :: number of averaging intervals in 1 cycle
C     missing_value  :: missing value for real-type fields in output file
C     missing_value_int :: missing value for integers in output (not used)
C     levels    :: List Output Levels
C     fields    :: List Output Fields
C     fileName  :: List Output Filename
C--   for regional-statistics
C     set_regMask(n) :: region-mask set-index that define the region "n"
C     val_regMask(n) :: corresponding mask value of region "n" in the region-mask
C--   per level statistics output:
C     stat_freq   :: Frequency (in s) of statistics output
C     stat_phase  :: phase (in s) to write statistics output
C     stat_region :: List of statistics output Regions
C     stat_fields :: List of statistics output Fields
C     stat_fName  :: List of statistics output Filename
      INTEGER     ldimLoc, kdimLoc, fdimLoc, rdimLoc
      PARAMETER ( ldimLoc = 2*numLists )
      PARAMETER ( kdimLoc = 2*numLevels )
      PARAMETER ( fdimLoc = 2*numperList )
      PARAMETER ( rdimLoc = nRegions+21 )
      _RL         frequency(ldimLoc), timePhase(ldimLoc)
      _RL         averagingFreq(ldimLoc), averagingPhase(ldimLoc)
      INTEGER     repeatCycle(ldimLoc)
      _RL         missing_value(ldimLoc)
      INTEGER     missing_value_int(ldimLoc)
      _RL         levels(kdimLoc,ldimLoc)
      _RL         stat_freq(ldimLoc), stat_phase(ldimLoc)
      CHARACTER*8 fields(fdimLoc,ldimLoc)
      CHARACTER*8 stat_fields(fdimLoc,ldimLoc)
      CHARACTER*80 fileName(ldimLoc), blkFilName
      CHARACTER*80 stat_fName(ldimLoc)
      CHARACTER*8 fileFlags(ldimLoc)
      CHARACTER*8 blk8c, diagName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) namBuf
      CHARACTER*12 suffix
      INTEGER stat_region(rdimLoc,ldimLoc)
      INTEGER set_regMask(rdimLoc)
      _RS     val_regMask(rdimLoc)
      INTEGER ku, stdUnit
      INTEGER j,k,l,n,m,nf
      INTEGER iLen, regionCount

C--   full level output:
      NAMELIST / DIAGNOSTICS_LIST /
     &     frequency, timePhase,
     &     averagingFreq, averagingPhase, repeatCycle,
     &     missing_value, missing_value_int,
     &     levels, fields, fileName, fileFlags,
     &     dumpAtLast, diag_mnc, useMissingValue,
     &     diagCG_maxIters, diagCG_resTarget,
     &     diagCG_pcOffDFac, diagCG_prtResFrq, xPsi0, yPsi0,
     &     diag_pickup_read,     diag_pickup_write,
     &     diag_pickup_read_mnc, diag_pickup_write_mnc,
     &     diagMdsDir, diagMdsDirCreate

C--   per level statistics output:
      NAMELIST / DIAG_STATIS_PARMS /
     &     stat_freq, stat_phase, stat_region, stat_fields,
     &     stat_fName, diagSt_mnc,
     &     set_regMask, val_regMask,
     &     diagSt_regMaskFile, nSetRegMskFile

      IF ( .NOT.useDiagnostics ) THEN
C-    pkg DIAGNOSTICS is not used
        _BEGIN_MASTER(myThid)
         useDiag4AdjOutp = .FALSE.
C-    Track diagnostics pkg activation status:
         diag_pkgStatus = -1
C     print a (weak) warning if data.diagnostics is found
         CALL PACKAGES_UNUSED_MSG( 'useDiagnostics', ' ', ' ' )
        _END_MASTER(myThid)
        _BARRIER
        RETURN
      ENDIF

C-    Initialize and Read Diagnostics Namelist
      _BEGIN_MASTER(myThid)

      blk8c  = '        '
      DO k=1,LEN(blkFilName)
        blkFilName(k:k) = ' '
      ENDDO

      DO l = 1,ldimLoc
        frequency(l)  = 0.
        timePhase(l)  = UNSET_RL
        averagingFreq(l) = 0.
        averagingPhase(l)= 0.
        repeatCycle(l)   = 0
        fileName(l)   = blkFilName
C-    Cannot use model standard Unset value since this was used previously
C     as defaut missing value that one might want to recover;
C     Use instead the unlikely missing value of One for the Undef-missing-Val
c       missing_value(l)     = UNSET_RL
        missing_value(l)     = oneRL
        missing_value_int(l) = UNSET_I
        fileFlags(l)  = blk8c
        DO k = 1,kdimLoc
          levels(k,l) = UNSET_RL
        ENDDO
        DO m = 1,fdimLoc
          fields(m,l) = blkName
        ENDDO
      ENDDO
      diagLoc_ioUnit = 0
      dumpAtLast   = .FALSE.
      diag_mnc     = useMNC
      useMissingValue = .FALSE.
      diag_pickup_read      = .FALSE.
      diag_pickup_write     = .FALSE.
      diag_pickup_read_mnc  = .FALSE.
      diag_pickup_write_mnc = .FALSE.
      diagMdsDir = ' '
      diagMdsDirCreate = .TRUE.

      prtFirstCall     = .TRUE.
      diagCG_maxIters  = cg2dMaxIters
      diagCG_resTarget = cg2dTargetResidual
      diagCG_prtResFrq = printResidualFreq
      diagCG_pcOffDFac = 1.
      IF ( cg2dpcOffDFac.GT.zeroRL )
     &  diagCG_pcOffDFac = 0.25 _d 0 /( cg2dpcOffDFac*cg2dpcOffDFac )
      xPsi0 = UNSET_RS
      yPsi0 = UNSET_RS

      diagSt_regMaskFile = ' '
      nSetRegMskFile = 0
      DO k = 1,rdimLoc
        set_regMask(k) = 0
        val_regMask(k) = 0.
      ENDDO
      DO l = 1,ldimLoc
        stat_freq(l)  = 0.
        stat_phase(l) = UNSET_RL
        stat_fName(l) = blkFilName
        DO k = 1,rdimLoc
          stat_region(k,l) = UNSET_I
        ENDDO
        DO m = 1,fdimLoc
          stat_fields(m,l) = blkName
        ENDDO
      ENDDO
C     useDiag4AdjOutp will be to set to T if ADJ diags are found in namelist
      useDiag4AdjOutp = .FALSE.
C-    Track diagnostics pkg activation status:
      diag_pkgStatus = 1

      WRITE(msgBuf,'(2A)')
     &     ' DIAGNOSTICS_READPARMS: opening data.diagnostics'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE('data.diagnostics',
     &     'DIAGNOSTICS_READPARMS', ku, myThid )

      WRITE(msgBuf,'(2A)') 'S/R DIAGNOSTICS_READPARMS,',
     &     ' read namelist "diagnostics_list": start'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      READ  (ku,NML=diagnostics_list)
      WRITE(msgBuf,'(2A)') 'S/R DIAGNOSTICS_READPARMS,',
     &     ' read namelist "diagnostics_list": OK'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C-    set default for statistics output according to the main flag
      diag_mnc = diag_mnc .AND. useMNC
      diagSt_mnc = diag_mnc

      WRITE(msgBuf,'(2A)') 'S/R DIAGNOSTICS_READPARMS,',
     &     ' read namelist "DIAG_STATIS_PARMS": start'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      READ  (ku,NML=DIAG_STATIS_PARMS)
      WRITE(msgBuf,'(2A)') 'S/R DIAGNOSTICS_READPARMS,',
     &     ' read namelist "DIAG_STATIS_PARMS": OK'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

#ifdef SINGLE_DISK_IO
      CLOSE(ku)
#else
      CLOSE(ku,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C     Initialise DIAG_SELECT common block (except pointers)
      nlists = 0
      DO n = 1,numLists
        freq(n) = 0.
        phase(n) = 0.
        averageFreq(n)  = 0.
        averagePhase(n) = 0.
        averageCycle(n) = 1
        nlevels(n) = 0
        nfields(n) = 0
        fnames(n) = blkFilName
c       misValFlt(n) = UNSET_RL
c       misValInt(n) = UNSET_I
        misValFlt(n) = -999. _d 0
#ifdef ALLOW_FIZHI
        IF ( useFIZHI ) misValFlt(n) = getcon('UNDEF')
#endif
        DO k = 1,numLevels
          levs(k,n) = 0
        ENDDO
        DO m = 1,numperList
          flds(m,n) = blkName
        ENDDO
        fflags(n)   = blk8c
      ENDDO

C     useMNC is confusing (can be T at this point & turned off later, whereas
C     for all other pkgs, model stops if use${PKG}= T with #undef ALLOW_${PKG})
#ifndef ALLOW_MNC
C     Fix to avoid running without getting any output:
      diag_mnc   = .FALSE.
      diagSt_mnc = .FALSE.
#endif

C     Fill Diagnostics Common Block with Namelist Info
      diagSt_mnc = diagSt_mnc .AND. useMNC
      diag_mdsio = (.NOT. diag_mnc) .OR. outputTypesInclusive
      diag_pickup_read_mnc  = diag_pickup_read_mnc .AND. diag_mnc
      diag_pickup_write_mnc = diag_pickup_write_mnc .AND. diag_mnc
      diag_pickup_read_mdsio  =
     &     diag_pickup_read .AND. (.NOT. diag_pickup_read_mnc)
      diag_pickup_write_mdsio = diag_pickup_write .AND.
     &     ((.NOT. diag_pickup_write_mnc) .OR. outputTypesInclusive)
      diagSt_ascii = (.NOT. diagSt_mnc) .OR. outputTypesInclusive

C     remove trailing "/":
      iLen = ILNBLNK( diagMdsDir )
      IF ( iLen.GE.2 ) THEN
       IF ( diagMdsDir(iLen:iLen).EQ.'/' ) THEN
         namBuf = diagMdsDir
         WRITE(diagMdsDir,'(A)') namBuf(1:iLen-1)
       ENDIF
      ENDIF

      DO l = 1,ldimLoc
       iLen = ILNBLNK(fileName(l))
C-     Only lists with non-empty file name (iLen>0) are considered
       IF ( iLen.GE.1 .AND. nlists.LT.numLists ) THEN
         n = nlists + 1
         freq(n)    = frequency(l)
         IF ( timePhase(l).NE. UNSET_RL ) THEN
           phase(n) = timePhase(l)
         ELSEIF ( frequency(l) .LT. 0. ) THEN
           phase(n) = -0.5 _d 0 * frequency(l)
         ENDIF
         IF ( averagingFreq(l).GT.0. .AND. repeatCycle(l).GT.1 ) THEN
           averageFreq(n)  = averagingFreq(l)
           averagePhase(n) = averagingPhase(l)
           averageCycle(n) = repeatCycle(l)
         ELSEIF (averagingFreq(l).NE.0. .OR. repeatCycle(l).NE.0) THEN
           WRITE(msgBuf,'(2A,F18.6,I4)') 'DIAGNOSTICS_READPARMS: ',
     &       'unvalid Average-Freq & Cycle:',
     &       averagingFreq(l), repeatCycle(l)
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(2A,I4,2A)') 'DIAGNOSTICS_READPARMS: ',
     &         ' for list l=', l, ', fileName: ', fileName(l)
           CALL PRINT_ERROR( msgBuf , myThid )
           STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
         ELSEIF ( frequency(l) .EQ. 0. ) THEN
           averageFreq(n)  = nTimeSteps*deltaTClock
           averagePhase(n) = phase(n)
         ELSEIF ( frequency(l) .GT. 0. ) THEN
           averageFreq(n)  = frequency(l)
           averagePhase(n) = phase(n)
         ENDIF
c        IF ( missing_value(l) .NE. UNSET_RL )
c    &        misValFlt(n) = missing_value(l)
c        IF ( missing_value_int(l) .NE. UNSET_I )
c    &        misValInt(n) = missing_value_int(l)
         IF ( missing_value(l) .NE. oneRL )
     &        misValFlt(n) = missing_value(l)
         fnames(n)  = fileName (l)
         fflags(n)  = fileFlags(l)
         nlevels(n) = 0
         IF ( levels(1,l).NE.UNSET_RL ) THEN
           DO k=1,kdimLoc
             IF ( levels(k,l).NE.UNSET_RL .AND.
     &            nlevels(n).LT.numLevels ) THEN
               nlevels(n) = nlevels(n) + 1
               levs(nlevels(n),n) = levels(k,l)
             ELSEIF ( levels(k,l).NE.UNSET_RL ) THEN
              WRITE(msgBuf,'(2A,I4)') 'DIAGNOSTICS_READPARMS: ',
     &         'Exceed Max.Num. of Levels numLevels=', numLevels
              CALL PRINT_ERROR( msgBuf , myThid )
              WRITE(msgBuf,'(2A,I4,A,F8.0)') 'DIAGNOSTICS_READPARMS: ',
     &         'when trying to add level(k=', k, ' )=', levels(k,l)
              CALL PRINT_ERROR( msgBuf , myThid )
              WRITE(msgBuf,'(2A,I4,2A)') 'DIAGNOSTICS_READPARMS: ',
     &         ' for list l=', l, ', fileName: ', fileName(l)
              CALL PRINT_ERROR( msgBuf , myThid )
              STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
             ENDIF
           ENDDO
         ELSE
C-       will set levels later, once the Nb of levels of each diag is known
           nlevels(n) = -1
         ENDIF
         nfields(n) = 0
         DO m=1,fdimLoc
           diagName = DIAGS_RENAMED( fields(m,l), myThid )
           IF ( diagName.NE.blkName .AND.
     &          nfields(n).LT.numperList ) THEN
             nfields(n) = nfields(n) + 1
             flds(nfields(n),n) = diagName
           ELSEIF ( diagName.NE.blkName ) THEN
             WRITE(msgBuf,'(2A,I4)') 'DIAGNOSTICS_READPARMS: ',
     &        'Exceed Max.Num. of Fields/list numperList=', numperList
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I4,3A,I4,2A)') 'DIAGNOSTICS_READPARMS: ',
     &        'when trying to add field (m=', m, ' ): ', diagName
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I4,2A)') 'DIAGNOSTICS_READPARMS: ',
     &        ' in list l=', l, ', fileName: ', fileName(l)
             CALL PRINT_ERROR( msgBuf , myThid )
             STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
           ENDIF
         ENDDO
         nlists = nlists + 1
c        write(6,*) 'list summary:',n,nfields(n),nlevels(n)
       ELSEIF ( iLen.GE.1 ) THEN
         WRITE(msgBuf,'(2A,I4)') 'DIAGNOSTICS_READPARMS: ',
     &            'Exceed Max.Num. of list numLists=', numLists
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,I4)') 'DIAGNOSTICS_READPARMS: ',
     &    'when trying to add list l=', l
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,F18.6,2A)') 'DIAGNOSTICS_READPARMS: ',
     &    ' Frq=', frequency(l), ', fileName: ', fileName(l)
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
       ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Initialise DIAG_STATS_REGMASK common block (except the mask)
      nSetRegMask = 0
      DO j = 0,nRegions
        diagSt_kRegMsk(j) = 0
        diagSt_vRegMsk(j) = 0.
      ENDDO
C     Global statistics (region # 0)
      diagSt_kRegMsk(0) = 1

C-    Initialise DIAG_STATIS common block (except pointers)
      diagSt_nbLists = 0
      DO n = 1,numLists
        diagSt_freq(n) = 0.
        diagSt_phase(n) = 0.
        diagSt_nbFlds(n) = 0
        diagSt_ioUnit(n) = 0
        diagSt_Fname(n) = blkFilName
        DO j = 0,nRegions
          diagSt_region(j,n) = 0
        ENDDO
        DO m = 1,numperList
          diagSt_Flds(m,n) = blkName
        ENDDO
      ENDDO

C     Fill Diagnostics Common Block with Namelist Info
      diagSt_ascii = (.NOT. diagSt_mnc) .OR. outputTypesInclusive

C-    Region mask correspondence table:
C     note: this table should be build when regions are defined ;
C     for now, simpler just to read it from namelist in data.diagnostics
      j = 0
      DO k = 1,rdimLoc
       IF ( set_regMask(k).NE.0 .OR. val_regMask(k).NE.0. ) THEN
         j = j+1
         IF ( j.LE.nRegions ) THEN
           diagSt_kRegMsk(j) = set_regMask(k)
           diagSt_vRegMsk(j) = val_regMask(k)
         ENDIF
       ENDIF
      ENDDO
      IF ( j.GT.nRegions ) THEN
         WRITE(msgBuf,'(2A,I4,A)') 'DIAGNOSTICS_READPARMS: ',
     &   'set_regMask & val_regMask lists assume at least',j,' regions'
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A)') 'DIAGNOSTICS_READPARMS: ',
     &   'Need to increase "nRegions" in DIAGNOSTICS_SIZE.h'
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
      ENDIF

      DO l = 1,ldimLoc
       iLen = ILNBLNK(stat_fName(l))
C-     Only lists with non-empty file name (iLen>0) are considered
       IF ( iLen.GE.1 .AND. diagSt_nbLists.LT.numLists)THEN
         n = diagSt_nbLists + 1
         diagSt_freq(n) = stat_freq(l)
         IF ( stat_phase(l).NE. UNSET_RL ) THEN
           diagSt_phase(n) = stat_phase(l)
         ELSEIF ( stat_freq(l) .LT. 0. ) THEN
           diagSt_phase(n) = -0.5 _d 0 * stat_freq(l)
         ENDIF
         diagSt_Fname(n)  = stat_fName(l)
         regionCount = 0
         DO k=1,rdimLoc
           j = stat_region(k,l)
           IF ( j.NE.UNSET_I .AND. j.GE.0 .AND. j.LE.nRegions ) THEN
            IF ( diagSt_region(j,n).EQ.0 ) THEN
             diagSt_region(j,n) = 1
             regionCount = regionCount + 1
            ELSE
             WRITE(msgBuf,'(2A,I4,2A)')
     &        'DIAGNOSTICS_READPARMS:',
     &        ' in list l=', l, ', stat_fName: ', stat_fName(l)
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                           SQUEEZE_RIGHT , myThid )
             WRITE(msgBuf,'(A,I4,A)')
     &        'DIAGNOSTICS_READPARMS: region=',j,
     &        ' can only be selected once => ignore 2nd selection'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                           SQUEEZE_RIGHT , myThid )
            ENDIF
           ELSEIF ( j.NE.UNSET_I ) THEN
             WRITE(msgBuf,'(A,I4,A,I4,2A)')
     &       'DIAGNOSTICS_READPARMS: region=',j,
     &         ' in list l=', l, ', stat_fName: ', stat_fName(l)
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I4,A,I4,2A)')
     &       'DIAGNOSTICS_READPARMS: ==> exceed Max.Nb of regions',
     &       '(=',nRegions,' )'
             CALL PRINT_ERROR( msgBuf , myThid )
             STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
           ENDIF
         ENDDO
         IF ( regionCount.EQ.0 ) THEN
C-       no region selected => default is Global statistics (region Id: 0)
           diagSt_region(0,n) = 1
         ENDIF
         diagSt_nbFlds(n) = 0
         DO m=1,fdimLoc
           diagName = DIAGS_RENAMED( stat_fields(m,l), myThid )
           IF ( diagName.NE.blkName .AND.
     &          diagSt_nbFlds(n).LT.numperList ) THEN
             diagSt_nbFlds(n) = diagSt_nbFlds(n) + 1
             diagSt_Flds(diagSt_nbFlds(n),n) = diagName
           ELSEIF ( diagName.NE.blkName ) THEN
             WRITE(msgBuf,'(2A,I4)') 'DIAGNOSTICS_READPARMS: ',
     &        'Exceed Max.Num. of Fields/list numperList=', numperList
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I4,3A,I4,2A)') 'DIAGNOSTICS_READPARMS: ',
     &        'when trying to add stat_field (m=', m, ' ): ', diagName
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I4,2A)') 'DIAGNOSTICS_READPARMS: ',
     &        ' in list l=', l, ', stat_fName: ', stat_fName(l)
             CALL PRINT_ERROR( msgBuf , myThid )
             STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
           ENDIF
         ENDDO
         diagSt_nbLists = diagSt_nbLists + 1
c        write(6,*) 'stat-list summary:',n,diagSt_nbFlds(n),regionCount
       ELSEIF ( iLen.GE.1 ) THEN
         WRITE(msgBuf,'(2A,I4)') 'DIAGNOSTICS_READPARMS: ',
     &            'Exceed Max.Num. of list numLists=', numLists
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,I4)') 'DIAGNOSTICS_READPARMS: ',
     &    'when trying to add stat_list l=', l
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,F18.6,2A)') 'DIAGNOSTICS_READPARMS: ',
     &    ' Frq=', stat_freq(l), ', stat_fName: ', stat_fName(l)
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
       ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Echo History List Data Structure
      stdUnit = standardMessageUnit
      WRITE(msgBuf,'(A)')
     &     ' DIAGNOSTICS_READPARMS: global parameter summary:'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      CALL WRITE_0D_L( dumpAtLast, INDEX_NONE,
     & ' dumpAtLast =',' /* always write time-ave diags at the end */')
      CALL WRITE_0D_L( diag_mnc,   INDEX_NONE,
     & ' diag_mnc =', '   /* write NetCDF output files */')
      IF ( diag_mdsio.AND.(diagMdsDir.NE.' ') ) THEN
       CALL WRITE_0D_C( diagMdsDir, -1, INDEX_NONE,
     & ' diagMdsDir =', ' /* directory for mds diagnostics output */')
       CALL WRITE_0D_L( diagMdsDirCreate, INDEX_NONE,
     & ' diagMdsDirCreate =', ' /* call mkdir to create diagMdsDir */')
      ENDIF
      CALL WRITE_0D_L( useMissingValue, INDEX_NONE,
     & ' useMissingValue =', ' /* put MissingValue where mask = 0 */')
      CALL WRITE_0D_I( diagCG_maxIters, INDEX_NONE,
     & ' diagCG_maxIters =', ' /* max number of iters in diag_cg2d */')
      CALL WRITE_0D_RL( diagCG_resTarget, INDEX_NONE,
     & ' diagCG_resTarget =', ' /* residual target for diag_cg2d */')
      CALL WRITE_0D_RL( diagCG_pcOffDFac, INDEX_NONE,
     & ' diagCG_pcOffDFac =',
     & ' /* preconditioner off-diagonal factor */')
      WRITE(msgBuf,'(A)')
     & '-----------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(A)')
     &     ' DIAGNOSTICS_READPARMS: active diagnostics summary:'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(A)')
     & '-----------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      DO n = 1,nlists
        WRITE(msgBuf,'(2A)') 'Creating Output Stream: ', fnames(n)
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        WRITE(msgBuf,'(2(A,F18.6))') 'Output Frequency:', freq(n),
     &                               ' ; Phase: ', phase(n)
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        WRITE(msgBuf,'(2(A,F18.6),A,I4)')
     &    ' Averaging Freq.:', averageFreq(n),
     &    ' , Phase: ', averagePhase(n), ' , Cycle:', averageCycle(n)
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        IF ( fflags(n).EQ.blk8c ) THEN
c         WRITE(msgBuf,'(A,1PE20.12,A,I12,3A)')
c    &       ' missing value:',  misValFlt(n),
c    &       ' ; for integers:', misValInt(n)
          WRITE(msgBuf,'(A,1PE20.12,3A)')
     &       ' missing value:', misValFlt(n)
        ELSE
c         WRITE(msgBuf,'(A,1PE20.12,A,I12,3A)')
c    &       ' missing value:',  misValFlt(n),
c    &       ' ; for integers:', misValInt(n),
c    &       ' ; F-Flags="', fflags(n),'"'
          WRITE(msgBuf,'(A,1PE20.12,3A)')
     &       ' missing value:', misValFlt(n),
     &       ' ; F-Flags="', fflags(n),'"'
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        IF ( nlevels(n).EQ.-1 .AND. fflags(n)(2:2).EQ.'I' ) THEN
          WRITE(msgBuf,'(A)') ' Cumulate all Levels (to be set later)'
          CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        ELSEIF ( nlevels(n).EQ.-1 ) THEN
          WRITE(msgBuf,'(A,A)') ' Levels:    ','will be set later'
          CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        ELSEIF ( fflags(n)(2:2).EQ.'P' ) THEN
         DO l=1,nlevels(n),10
          m = MIN(nlevels(n),l+9)
          WRITE(msgBuf,'(A,1P10E10.3)')' interp:  ', (levs(k,n),k=l,m)
          CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
         ENDDO
        ELSE
         suffix = ' Levels:    '
         IF ( fflags(n)(2:2).EQ.'I' ) suffix = ' Sum Levels:'
         DO l=1,nlevels(n),20
          m = MIN(nlevels(n),l+19)
          WRITE(msgBuf,'(A,20F5.0)') suffix, (levs(k,n),k=l,m)
          CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
         ENDDO
        ENDIF
        DO nf = 1,nfields(n),10
          m = MIN(nfields(n),nf+9)
          WRITE(msgBuf,'(21A)') ' Fields:   ',(' ',flds(l,n),l=nf,m)
          CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        ENDDO
      ENDDO
      WRITE(msgBuf,'(A)')
     & '-----------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(A)')
     &     ' DIAGNOSTICS_READPARMS: statistics diags. summary:'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      DO n = 1,diagSt_nbLists
        WRITE(msgBuf,'(2A)') 'Creating Stats. Output Stream: ',
     &                       diagSt_Fname(n)
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        WRITE(msgBuf,'(2(A,F18.6))') 'Output Frequency:',
     &               diagSt_freq(n), ' ; Phase: ', diagSt_phase(n)
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        WRITE(msgBuf,'(A)') ' Regions: '
        l = 10
        DO j=0,nRegions
         IF ( diagSt_region(j,n).GE.1 ) THEN
          l = l+3
          IF (l.LE.MAX_LEN_MBUF) WRITE(msgBuf(l-2:l),'(I3)') j
         ENDIF
        ENDDO
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        DO nf = 1,diagSt_nbFlds(n),10
          m = MIN(diagSt_nbFlds(n),nf+9)
          WRITE(msgBuf,'(21A)') ' Fields:   ',
     &                 (' ',diagSt_Flds(l,n),l=nf,m)
          CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        ENDDO
      ENDDO
      WRITE(msgBuf,'(A)')
     & '-----------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(A)')
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
