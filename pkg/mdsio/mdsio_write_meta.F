C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_write_meta.F,v 1.6 2013/01/13 22:42:26 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

CBOP
C     !ROUTINE: MDS_WRITE_META
C     !INTERFACE:
      SUBROUTINE MDS_WRITE_META(
     I    mFileName,
     I    dFileName,
     I    simulName,
     I    titleLine,
     I    filePrec,
     I    nDims,   dimList, map2gl,
     I    nFlds,   fldList,
     I    nTimRec, timList, misVal,
     I    nrecords, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R MDS_WRITE_META
C     | o Write 1 meta file to disk
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     mFileName (string ) :: complete name of meta-file
C     dFileName (string ) :: complete name of data-file
C     simulName (string)  :: name of this simulation
C     titleLine (string)  :: title or any descriptive comments
C     filePrec  (integer) :: number of bits per word in data-file (32 or 64)
C     nDims     (integer) :: number of dimensions
C     dimList   (integer) :: array of dimensions, etc.
C     map2gl    (integer) :: used for mapping tiled file to global file
C     nFlds     (integer) :: number of fields in "fldList"
C     fldList   (string)  :: array of field names to write
C     nTimRec   (integer) :: number of time-specification in "timList"
C     timList   (real)    :: array of time-specifications to write
C     misVal    (real)    :: missing value (ignored if = 1.)
C     nrecords  (integer) :: record number
C     myIter    (integer) :: time-step number
C     myThid    (integer) :: my Thread Id number
C
C     !OUTPUT PARAMETERS:
C
      CHARACTER*(*) mFileName
      CHARACTER*(*) dFileName
      CHARACTER*(*) simulName
      CHARACTER*(*) titleLine
      INTEGER filePrec
      INTEGER nDims
      INTEGER dimList(3,nDims)
      INTEGER map2gl(2)
      INTEGER nFlds
      CHARACTER*(8) fldList(*)
      INTEGER nTimRec
      _RL     timList(*)
      _RL     misVal
      INTEGER nrecords
      INTEGER myIter
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER i,j,ii,iL
      INTEGER mUnit
c     LOGICAL exst
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C We should *read* the met-file IF it exists to check
C  that the information we are writing is consistent
C  with the current contents
c     INQUIRE( file=mFileName, exist=exst )
C However, it is bloody difficult to parse files in fortran so someone
C  else can do this.
C For now, we will assume everything is ok and that the last record
C  is written to the last consecutive record in the file.

C-    Assign a free unit number as the I/O channel for this subroutine
      CALL MDSFINDUNIT( mUnit, myThid )

C-    Open meta-file
      OPEN( mUnit, file=mFileName, status='unknown',
     &      form='formatted' )

C-    Write the simulation name
      iL = ILNBLNK(simulName)
      IF ( iL.GT.0 ) THEN
       WRITE(mUnit,'(3A)') " simulation = { '",simulName(1:iL),"' };"
      ENDIF

C-    Write the number of dimensions
      WRITE(mUnit,'(1X,A,I3,A)') 'nDims = [ ',nDims,' ];'

C-    For each dimension, write the following:
C     1  global size  (ie. the size of the global dimension of all files)
C     2  global start (ie. the global position of the start of this file)
C     3  global end   (ie. the global position of the end   of this file)

      ii = 0
      DO j=1,nDims
       ii = MAX(dimList(1,j),ii)
      ENDDO
      WRITE(mUnit,'(1X,A)') 'dimList = ['
      IF ( ii.LT.10000 ) THEN
C     Small-size domain:
       DO j=1,nDims
        IF (j.LT.nDims) THEN
         WRITE(mUnit,'(1X,3(I5,","))')    (dimList(i,j),i=1,3)
        ELSE
         WRITE(mUnit,'(1X,2(I5,","),I5)') (dimList(i,j),i=1,3)
        ENDIF
       ENDDO
      ELSE
C     Large-size domain:
       DO j=1,nDims
        IF (j.LT.nDims) THEN
         WRITE(mUnit,'(1X,3(I10,","))')     (dimList(i,j),i=1,3)
        ELSE
         WRITE(mUnit,'(1X,2(I10,","),I10)') (dimList(i,j),i=1,3)
        ENDIF
       ENDDO
      ENDIF
      WRITE(mUnit,'(1X,A)') '];'
C-    only write if different from default:
      IF ( map2gl(1).NE.0 .OR. map2gl(2).NE.1 ) THEN
        WRITE(mUnit,'(1X,2(A,I5),A)') 'map2glob = [ ',
     &                  map2gl(1),',',map2gl(2),' ];'
      ENDIF

C-    Record the precision of the file
      IF (filePrec .EQ. precFloat32) THEN
       WRITE(mUnit,'(1X,A)') "dataprec = [ 'float32' ];"
      ELSEIF (filePrec .EQ. precFloat64) THEN
       WRITE(mUnit,'(1X,A)') "dataprec = [ 'float64' ];"
      ELSE
       WRITE(msgBuf,'(A)')
     &   ' MDSWRITEMETA: invalid filePrec'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R MDSWRITEMETA'
      ENDIF

C-    Record the current record number
C     This is a proxy for the actual number of records in the file.
C     If we could read the file then we could do this properly.
      WRITE(mUnit,'(1X,A,I5,A)') 'nrecords = [ ',nrecords,' ];'

C-    Record the file-name for the binary data
Cveto ii=ILNBLNK( dFileName )
Cveto WRITE(mUnit,'(1X,3A)') 'binarydatafile = [ ',dFileName(1:ii),' ];'

C-    Write the integer time (integer iteration number) for later record
C     keeping. If the timestep number is less than 0 then we assume
C     that the information is superfluous and do not write it.
      IF ( myIter.GE.0 )
     & WRITE(mUnit,'(1X,A,I10,A)') 'timeStepNumber = [ ',myIter,' ];'

C-    Write list of Time records
C note: format might change once we have a better idea of what will
C       be the time-information to write.
      IF ( nTimRec.GT.0 ) THEN
       ii = MIN(nTimRec,20)
       WRITE(msgBuf,'(1P20E20.12)') (timList(i),i=1,ii)
       WRITE(mUnit,'(1X,3A)') 'timeInterval = [', msgBuf(1:20*ii),' ];'
      ENDIF

C-    Write missing value
      IF ( misVal.NE.oneRL ) THEN
       WRITE(mUnit,'(1X,A,1PE21.14,A)')
     &                        'missingValue = [ ',misVal,' ];'
      ENDIF

C-    Write list of Fields
      IF ( nFlds.GT.0 ) THEN
        WRITE(mUnit,'(1X,A,I4,A)') 'nFlds = [ ', nFlds, ' ];'
        WRITE(mUnit,'(1X,A)') 'fldList = {'
        WRITE(mUnit,'(20(A2,A8,A1))')
     &          (" '",fldList(i),"'",i=1,nFlds)
        WRITE(mUnit,'(1X,A)') '};'
      ENDIF

C-    Write title or comments (but ignored by rdmds)
      iL = ILNBLNK(titleLine)
      IF ( iL.GT.0 ) THEN
       WRITE(mUnit,'(3A)')' /* ', titleLine(1:iL), ' */'
      ENDIF

C-    Close meta-file
      CLOSE(mUnit)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
