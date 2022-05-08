#include "MDSIO_OPTIONS.h"

CBOP
C     !ROUTINE: MDS_READ_META
C     !INTERFACE:
      SUBROUTINE MDS_READ_META(
     I               fileName,
     O               simulName,
     O               titleLine,
     O               filePrec,
     U               nDims,   nFlds,   nTimRec,
     O               dimList, fldList, timList,
     O               misVal, nRecords, fileIter,
     I               useCurrentDir,
     I               myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R MDS_READ_META
C     | o Read the content of 1 meta file
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
C     fileName  (string ) :: prefix of meta-file name
C     nDims     (integer) :: max size of array dimList (or =0 if not reading dimList)
C     nFlds     (integer) :: max size of array fldList (or =0 if not reading fldList)
C     nTimRec   (integer) :: max size of array timList (or =0 if not reading timList)
C     useCurrentDir(logic):: always read from the current directory (even if
C                            "mdsioLocalDir" is set)
C     myThid    (integer) :: my Thread Id number
C
C     !OUTPUT PARAMETERS:
C     simulName (string)  :: name of simulation (recorded in file)
C     titleLine (string)  :: title or any descriptive comments (in file)
C     filePrec  (integer) :: number of bits per word in data-file (32 or 64)
C     nDims     (integer) :: number of dimensions
C     dimList   (integer) :: array of dimensions
cC    map2gl    (integer) :: used for mapping tiled file to global file
C     nFlds     (integer) :: number of fields in "fldList"
C     fldList   (string)  :: list of fields (names) stored in file
C     nTimRec   (integer) :: number of time-specification in "timList"
C     timList   (real)    :: array of time-specifications (recorded in file)
C     misVal    (real)    :: missing value
C     nRecords  (integer) :: number of records
C     fileIter  (integer) :: time-step number (recorded in file)
C
      CHARACTER*(*) fileName
      CHARACTER*(*) simulName
      CHARACTER*(*) titleLine
      INTEGER filePrec
      INTEGER nDims
      INTEGER dimList(3,*)
c     INTEGER map2gl(2)
      INTEGER nFlds
      CHARACTER*(8) fldList(*)
      INTEGER nTimRec
      _RL     timList(*)
      _RL     misVal
      INTEGER nRecords
      INTEGER fileIter
      LOGICAL useCurrentDir
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     i, j, ii    :: loop indices
C     iG,jG       :: global tile indices
C     iL,pL,iLm   :: length of character strings (temp. variables)
C     nDimFil     :: number of dimensions (in meta file)
C     nFldFil     :: number of fields in "fldList" (in meta file)
C     nTimFil     :: number of time-specification in "timList" (meta file)
      INTEGER i,j,ii
      INTEGER iG,jG
      INTEGER iL,pL,iLm
      INTEGER mUnit, errIO
      INTEGER nDimFil, nFldFil, nTimFil
      LOGICAL fileExist, globalFile
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_MBUF) lineBuf
      CHARACTER*(MAX_LEN_FNAM) mFileName, pfName

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Initialise output arguments
      simulName = ' '
      titleLine = ' '
      filePrec  = 0
      nRecords  = 0
      fileIter  = 0
c     map2gl(1) = 0
c     map2gl(2) = 1
      DO j=1,nDims
       DO i=1,3
        dimList(i,j) = 0
       ENDDO
      ENDDO
      DO i=1,nFlds
       fldList(i)= ' '
      ENDDO
      DO i=1,nTimRec
       timList(i) = 0.
      ENDDO
      misVal = oneRL
C--   Initialise Temp Var.
      fileExist  = .FALSE.
      globalFile = .FALSE.
      nDimFil   = 0
      nFldFil   = 0
      nTimFil   = 0

C--   Only Master thread check for file, open & read ; others will
C     return null argument ; sharing output needs to be done outside
C     this S/R, using, e.g., common block (+ Master_thread + Barrier)
      _BEGIN_MASTER( myThid )

C     Assign special directory
      iL = ILNBLNK(fileName)
      pL = ILNBLNK( mdsioLocalDir )
      IF ( useCurrentDir .OR. pL.EQ.0 ) THEN
        pfName = fileName
      ELSE
        WRITE(pfName,'(2A)') mdsioLocalDir(1:pL), fileName(1:iL)
      ENDIF
      pL = ILNBLNK( pfName )

C--   Search for meta file:
C-    look for meta-file = {fileName}
      mFileName = fileName(1:iL)
      iLm = iL
c     INQUIRE( FILE=mFileName, EXIST=fileExist )
      IF ( .NOT.fileExist ) THEN
C-    look for meta-file = {fileName}'.meta'
        WRITE(mFileName,'(2A)') fileName(1:iL), '.meta'
        iLm = iL+5
        INQUIRE( FILE=mFileName, EXIST=fileExist )
      ENDIF
      IF ( fileExist ) THEN
        globalFile = .TRUE.
      ELSE
C-    look for meta-file = {fileName}'.{iG}.{jG}.meta'
        iG = 1+(myXGlobalLo-1)/sNx
        jG = 1+(myYGlobalLo-1)/sNy
        WRITE(mFileName,'(2A,I3.3,A,I3.3,A)')
     &             pfName(1:pL),'.',iG,'.',jG,'.meta'
        iLm = pL+8+5
        INQUIRE( FILE=mFileName, EXIST=fileExist )
      ENDIF
      IF ( .NOT.fileExist ) THEN
C-    look for meta-file = {fileName}'.001.001.meta'
        WRITE(mFileName,'(2A,I3.3,A,I3.3,A)')
     &             pfName(1:pL),'.',1,'.',1,'.meta'
        iLm = pL+8+5
        INQUIRE( FILE=mFileName, EXIST=fileExist )
      ENDIF
      IF ( .NOT.fileExist ) THEN
        WRITE(msgBuf,'(4A)') 'WARNING >> MDS_READ_META: file: ',
     &          fileName(1:iL), '.meta , ', mFileName(1:iLm)
c    &               fileName(1:iL), ' , ', mFileName(1:iLm)
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(A)')
     &           'WARNING >> MDS_READ_META: Files DO not exist'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT , myThid )
        nFldFil = -1
      ELSE

C--   File exist
        IF ( debugLevel .GE. debLevB ) THEN
          WRITE(msgBuf,'(2A)') ' MDS_READ_META: opening file: ',
     &                        mFileName(1:iLm)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
        ENDIF

C-    Assign a free unit number as the I/O channel for this subroutine
        CALL MDSFINDUNIT( mUnit, myThid )

C-    Open meta-file
        OPEN( mUnit, FILE=mFileName, STATUS='old', _READONLY_ACTION
     &        FORM='formatted', IOSTAT=errIO )
c       write(0,*) 'errIO=',errIO
        IF ( errIO .NE. 0 ) THEN
          WRITE(msgBuf,'(A,A)') 'MDS_READ_META: Unable to open file: ',
     &                           mFileName(1:iLm)
          CALL PRINT_ERROR( msgBuf , myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_META'
        ENDIF

C-    Read the meta file in the same way as S/R OPEN_COPY_DATA_FILE
C     (which seems to be works on many platforms):
        DO WHILE ( .TRUE. )
         READ( mUnit, FMT='(A)', END=1001 ) lineBuf
C--   Extract information from buffer: "lineBuf"
         iL = ILNBLNK(lineBuf)

C-    Read simulation name (stored in file)
         IF ( iL.GE.22 .AND. lineBuf(1:14).EQ.' simulation = ' ) THEN
          ii = LEN(simulName)
c         IF ( ii.LT.iL-21 )  print 'warning: truncate simulName'
          ii = MIN(ii+17,iL-4)
          simulName = lineBuf(18:ii)
          iL = 0
         ENDIF

C-    Read the number of dimensions
         IF ( nDimFil.EQ.0 .AND.
     &        iL.GE.15 .AND. lineBuf(1:9).EQ.' nDims = ' ) THEN
          READ(lineBuf(12:iL),'(I3)') nDimFil
          IF ( nDimFil.GT.nDims .AND. nDims.GE.1 ) THEN
            WRITE(msgBuf,'(2(A,I3),A)') ' MDS_READ_META: nDims=',
     &                   nDimFil, ' too large ( >', nDims, ' )'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_META'
          ENDIF
          iL = 0
         ENDIF

C-    Read list of dimensions
         IF ( nDims.GE.1 .AND. nDimFil.GE.1 .AND.
     &        iL.GE.11 .AND. lineBuf(1:11).EQ.' dimList = ' ) THEN
C-    For each dimension, read the following:
C     1  global size  (ie. the size of the global dimension of all files)
C     2  global start (ie. the global position of the start of this file)
C     3  global end   (ie. the global position of the end   of this file)
          DO j=1,nDimFil
C-    This is to accomodate with the 2 versions of meta file:
           READ( mUnit, FMT='(A)', END=1001 ) lineBuf
           ii = ILNBLNK(lineBuf)
           IF ( ii.LT.20 ) THEN
C     New version (S/R MDS_WRITE_META, file mdsio_write_meta.F):
C          small-size domain without starting blanks.
            READ(lineBuf, FMT='(3(1X,I5))',    ERR=1002, END=1002 )
     &                  (dimList(i,j),i=1,3)
           ELSEIF ( ii.LT.30 ) THEN
C     Old version (S/R MDSWRITEMETA, file mdsio_writemeta.F):
C          start each line with 10 blanks.
            READ(lineBuf, FMT='(9X,3(1X,I5))', ERR=1002, END=1002 )
     &                  (dimList(i,j),i=1,3)
           ELSE
C     New version (S/R MDS_WRITE_META, file mdsio_write_meta.F):
C          large-size domain without starting blanks.
            READ(lineBuf, FMT='(3(1X,I10))',   ERR=1002, END=1002 )
     &                  (dimList(i,j),i=1,3)
           ENDIF
          ENDDO
          READ(  mUnit, FMT='(A)', END=1001 ) lineBuf
          iL = 0
         ENDIF

C-    only write if different from default:
c     IF ( map2gl(1).NE.0 .OR. map2gl(2).NE.1 ) THEN
c       WRITE(mUnit,'(1X,2(A,I5),A)') 'map2glob = [ ',
c    &                  map2gl(1),',',map2gl(2),' ];'
c     ENDIF

C-    Read the precision of the file
         IF ( iL.GE.20 .AND. lineBuf(1:12).EQ.' dataprec = ' ) THEN
          IF (     lineBuf(16:22).EQ. 'float32' )  THEN
            filePrec = precFloat32
          ELSEIF ( lineBuf(16:22).EQ. 'float64' ) THEN
            filePrec = precFloat64
          ELSE
            WRITE(msgBuf,'(A)') ' MDS_READ_META: invalid dataprec'
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL PRINT_ERROR(lineBuf, myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_META'
          ENDIF
          iL = 0
         ENDIF
C-    Read (old format) precision of the file
         IF ( filePrec.EQ.0 .AND.
     &        iL.GE.18 .AND. lineBuf(1:10).EQ.' format = ' ) THEN
          IF (     lineBuf(14:20).EQ. 'float32' )  THEN
            filePrec = precFloat32
          ELSEIF ( lineBuf(14:20).EQ. 'float64' ) THEN
            filePrec = precFloat64
          ELSE
            WRITE(msgBuf,'(A)') ' MDS_READ_META: invalid dataprec'
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL PRINT_ERROR(lineBuf, myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_META'
          ENDIF
          iL = 0
         ENDIF

C-    Read the number of records
         IF ( nRecords.EQ.0 .AND.
     &        iL.GE.20 .AND. lineBuf(1:12).EQ.' nrecords = ' ) THEN
          IF ( iL.GE.25 ) THEN
            READ(lineBuf(15:iL),'(I10)') nRecords
          ELSE
            READ(lineBuf(15:iL),'(I5)') nRecords
          ENDIF
          iL = 0
         ENDIF

C-    Read recorded iteration number
         IF ( fileIter.EQ.0 .AND. iL.GE.31 .AND.
     &        lineBuf(1:18).EQ.' timeStepNumber = ' ) THEN
          READ(lineBuf(21:iL),'(I10)') fileIter
          iL = 0
         ENDIF

C-    Read list of Time Intervals
         IF ( nTimFil.EQ.0 .AND.
     &        iL.GE.38 .AND. lineBuf(1:16).EQ.' timeInterval = ' ) THEN
C note: format might change once we have a better idea of what will
C       be the time-information to write.
          nTimFil = INT((iL-17-3)/20)
          IF ( nTimRec.GE.1 ) THEN
            IF ( nTimFil.GT.nTimRec ) THEN
             WRITE(msgBuf,'(2(A,I6),A)') ' MDS_READ_META: nTimRec=',
     &                    nTimFil, ' too large ( >', nTimRec, ' )'
             CALL PRINT_ERROR( msgBuf, myThid )
             STOP 'ABNORMAL END: S/R MDS_READ_META'
            ENDIF
            READ(lineBuf(18:iL-3),'(1P20E20.12)',ERR=1003)
     &                            (timList(i),i=1,nTimFil)
          ENDIF
          iL = 0
         ENDIF

         IF ( iL.GE.8 .AND. lineBuf(1:4).EQ.' /* ' ) THEN
          IF ( lineBuf(iL-2:iL).EQ.' */' ) THEN
C-    Read title or comments (ignored by rdmds)
           ii = LEN(titleLine)
c          IF ( ii.LT.iL-7 )  print 'warning: truncate titleLine'
           ii = MIN(ii+4,iL-3)
           titleLine = lineBuf(5:ii)
           iL = 0
          ENDIF
         ENDIF

C-    Read missing value
         IF ( misVal.EQ.oneRL .AND. iL.GE.40 .AND.
     &        lineBuf(1:16).EQ.' missingValue = ' ) THEN
          READ(lineBuf(19:iL),'(1PE21.14)') misVal
          iL = 0
         ENDIF

C-    Read number of Fields
         IF ( nFldFil.EQ.0 .AND.
     &        iL.GE.16 .AND. lineBuf(1:9).EQ.' nFlds = ' ) THEN
          READ(lineBuf(12:iL),'(I4)') nFldFil
          IF ( nFldFil.GT.nFlds .AND. nFlds.GE.1 ) THEN
            WRITE(msgBuf,'(2(A,I6),A)') ' MDS_READ_META: nFlds=',
     &                   nFldFil, ' too large ( >', nFlds, ' )'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_META'
          ENDIF
          iL = 0
         ENDIF

C-    Read list of Fields
         IF ( nFldFil.GE.1 .AND. nFlds.GE.1 .AND.
     &        iL.GE.11 .AND. lineBuf(1:11).EQ.' fldList = ' ) THEN
          DO j=1,nFldFil,20
           READ( mUnit, FMT='(20(2X,A8,1X))', ERR=1004, END=1004 )
     &          (fldList(i),i=j,MIN(nFldFil,j+19))
          ENDDO
          READ(  mUnit, FMT='(A)', END=1001 ) lineBuf
          iL = 0
         ENDIF

C--   End of reading file line per line
        ENDDO
 1004   CONTINUE
        WRITE(msgBuf,'(2(A,I4),A)')
     &    ' MDS_READ_META: error reading Fields: nFlds=',
     &     nFldFil, ' , j=', j
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R MDS_READ_META'
 1003   CONTINUE
        WRITE(msgBuf,'(2(A,I4),A)')
     &    ' MDS_READ_META: error reading Time-Interval: nTimRec=',
     &     nTimFil, ' , iL=', iL
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL PRINT_ERROR(lineBuf, myThid )
        STOP 'ABNORMAL END: S/R MDS_READ_META'
 1002   CONTINUE
        WRITE(msgBuf,'(3(A,I3),A)')
     &    ' MDS_READ_META: error reading Dim-List: nDims=',
     &     nDimFil, ' , j=', j, ' , ii=', ii
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL PRINT_ERROR(lineBuf, myThid )
        STOP 'ABNORMAL END: S/R MDS_READ_META'
 1001   CONTINUE

C-    Close meta-file
        CLOSE(mUnit)

C-    end if block: file exist
      ENDIF

      _END_MASTER( myThid )

C-    Update Arguments with values read from file
      nDims   = nDimFil
      nFlds   = nFldFil
      nTimRec = nTimFil

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
