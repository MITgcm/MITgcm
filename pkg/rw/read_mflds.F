C $Header: /u/gcmpack/MITgcm/pkg/rw/read_mflds.F,v 1.10 2014/03/30 16:21:52 jmc Exp $
C $Name:  $

#include "RW_OPTIONS.h"

C--  File read_mflds.F: Routines to handle reading Multi-Fields File (+ meta file)
C--   Contents
C--   o READ_MFLDS_INIT
C--   o READ_MFLDS_SET
C--   o READ_MFLDS_3D_RL
C--   o READ_MFLDS_LEV_RL
C--   o READ_MFLDS_LEV_RS
C--   o READ_MFLDS_CHECK
C--   o READ_MFLDS_RENAME

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_MFLDS_INIT
C     !INTERFACE:
      SUBROUTINE READ_MFLDS_INIT(
     I                            myThid )

C     !DESCRIPTION:
C     Initialise Multi-Fields read variables in common block

C     !USES:
      IMPLICIT NONE
c#include "SIZE.h"
#include "EEPARAMS.h"
#include "RW_MFLDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   :: my Thread Id. number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i        :: loop counter
      INTEGER i
C-    for debug print:
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
C-----

C-    Initialise variables in common block:
      thirdDim = 0
      nFl3D    = 0
      nFlds    = 0
      nMissFld = 0
      mFldsFile = ' '
      DO i=1,sizFldList
        fldList(i) = ' '
        fldMiss(i) = ' '
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_MFLDS_SET
C     !INTERFACE:
      SUBROUTINE READ_MFLDS_SET(
     I                      fName,
     O                      nbFields, filePrec,
     I                      fileDim3, myIter, myThid )

C     !DESCRIPTION:
C     This is the controlling routine for preparing Multi-Fields read
C     by reading the corresponding meta file.
C     the meta-file content is stored in common block (header: RW_MFLDS.h)
C     to be reachable by all threads

C Note: 1) Output arguments should not be shared variables (= not in common block)
C       2) Only master-thread returns a valid filePrec (others just return 0)

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RW_MFLDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     fName    :: current MFLDS file name (prefix) to read
C     nbFields :: Number of fields in current MFLDS file
C     filePrec :: data-precision in current MFLDS file
C     fileDim3 :: 3rd dimension of fields in current MFLDS file
C     myIter   :: Iteration number
C     myThid   :: my Thread Id. number
      CHARACTER*(MAX_LEN_FNAM) fName
      INTEGER nbFields
      INTEGER filePrec
      INTEGER fileDim3
      INTEGER myIter
      INTEGER myThid
CEOP

C !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C-    do change dir. (using mdsioLocalDir):
      LOGICAL  useCurrentDir
C-    output of MDS_READ_META :
      INTEGER  nSizD, nSizT
      PARAMETER( nSizD = 5 , nSizT = 20 )
      CHARACTER*(MAX_LEN_PREC/2) simulName
      CHARACTER*(MAX_LEN_MBUF/2) titleLine
      INTEGER nDims, nTimRec
      INTEGER dimList(3,nSizD)
      _RL     timList(nSizT)
      _RL     misVal
      INTEGER nRecords, fileIter
C-    for debug print:
      INTEGER i, j, ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C-----

C-    Initialise output arguments:
      nbFields = 0
      filePrec = 0

#ifdef RW_SAFE_MFLDS
      i = ILNBLNK(mFldsFile)
      IF ( i.NE.0 ) THEN
        i = MIN(i, MAX_LEN_MBUF-48-34 )
        WRITE(msgBuf,'(4A)') 'READ_MFLDS_SET: ',
     &    'MFLDS file-name already set to: ',mFldsFile(1:i)
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R READ_MFLDS_SET (fileName)'
      ENDIF
      _BARRIER
#endif /* RW_SAFE_MFLDS */
      _BEGIN_MASTER( myThid )

C-    Initialise variables in common block:
      thirdDim = fileDim3
      nFl3D    = 0
      nFlds    = 0
      nMissFld = 0
      mFldsFile = fName
      DO i=1,sizFldList
        fldList(i) = ' '
        fldMiss(i) = ' '
      ENDDO

#ifdef ALLOW_MDSIO
       useCurrentDir = .FALSE.
       nDims = nSizD
       nFlds = sizFldList
       nTimRec = nSizT
       CALL MDS_READ_META(
     I               fName,
     O               simulName,
     O               titleLine,
     O               filePrec,
     U               nDims,   nFlds,   nTimRec,
     O               dimList, fldList, timList,
     O               misVal, nRecords, fileIter,
     I               useCurrentDir, myThid )
#endif /* ALLOW_MDSIO */

C-    evaluate Nb of 3.D fields (used if mix 3-D & 2-D fields in file):
      nFl3D = 0
      IF ( nFlds.GE.1 ) THEN
        IF ( nDims.EQ.2 .AND. thirdDim.GT.1
     &                   .AND. nFlds.LT.nRecords ) THEN
          IF ( MOD( nRecords-nFlds , thirdDim-1 ) .EQ. 0 )
     &       nFl3D = (nRecords-nFlds)/(thirdDim-1)
        ENDIF
        IF ( nFlds.NE.nRecords .AND. nFl3D.EQ.0 ) THEN
C-    here we have a problem
          WRITE(msgBuf,'(A,I5,A,I4,A)')
     &     'READ_MFLDS_SET: Pb with Nb of records=', nRecords,
     &     ' (3rd-Dim=', thirdDim,')'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I5,A,I4,A)')
     &     ' does not match Nb of flds=', nFlds
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( 0 )
          STOP 'ABNORMAL END: S/R READ_MFLDS_SET (Nb-records Pb)'
        ENDIF
c       IF ( nFl3D.EQ.0 ) nFl3D = nFlds
      ENDIF

C-    write to Standard Output
      IF ( debugLevel.GE.debLevA ) THEN
        ioUnit = standardMessageUnit
        i = ILNBLNK(simulName)
        IF ( i.GE.1 ) THEN
         WRITE(msgBuf,'(3A)') ' simulName=>', simulName(1:i), '<'
         CALL PRINT_MESSAGE(msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
        i = ILNBLNK(titleLine)
        IF ( i.GE.1 ) THEN
         WRITE(msgBuf,'(3A)') ' titleLine=>', titleLine(1:i), '<'
         CALL PRINT_MESSAGE(msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
        WRITE(msgBuf,'(2(A,I4),A,I10)')
     &    ' nRecords =', nRecords, ' ; filePrec =', filePrec,
     &    ' ; fileIter =', fileIter
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,I4,A)') '    nDims =', nDims, ' , dims:'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        DO j=1,nDims
         WRITE(msgBuf,'(I4,A,3I4)') j,':',(dimList(i,j),i=1,3)
         CALL PRINT_MESSAGE(msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDDO
        WRITE(msgBuf,'(3(A,I4))')
     &    '    nFlds =', nFlds, ' , nFl3D =', nFl3D, ' , fields:'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        DO j=1,nFlds,20
         WRITE(msgBuf,'(20(A2,A8,A))')
     &     (' >', fldList(i), '<', i=j,MIN(j+19,nFlds) )
         CALL PRINT_MESSAGE(msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDDO
        WRITE(msgBuf,'(A,1PE22.14,A,I4,A)') 'missingVal=', misVal,
     &                           ' ; nTimRec =',nTimRec,' , timeList:'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        IF ( nTimRec.GE.1 ) THEN
         WRITE(msgBuf,'(1P20E20.12)') (timList(i),i=1,nTimRec)
         CALL PRINT_MESSAGE(msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF

      _END_MASTER( myThid )
      _BARRIER

C-    set output arguments:
      nbFields = nFlds

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_MFLDS_3D_RL
C     !INTERFACE:
      SUBROUTINE READ_MFLDS_3D_RL(
     I                fldName,
     O                field,
     U                nj,
     I                fPrec, nNz, myIter, myThid )

C     !DESCRIPTION:
C     Read, from a Multi-Fields binary file, field "fldName" into array "field"
C     record Nb "nj" is search through the field-list (from meta-file) which
C     has been set before (calling READ_MFLDS_SET).
C     In case nFlds is <=0 , by-pass the search and directly read record number "nj"

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RW_MFLDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     fldName :: Name of the field to read
C     field   :: Output array to read in
C     nj (in) :: number of the record (in file) just before the one to read
C     nj (out):: number of the record (from current file) which was read in
C     fPrec   :: File precision (number of bits per word, = 32 or 64)
C     nNz     :: Number of levels to read in
C     myIter  :: Iteration number
C     myThid  :: My Thread Id number
      CHARACTER*(8) fldName
      _RL     field(*)
      INTEGER nj
      INTEGER fPrec
      INTEGER nNz
      INTEGER myIter
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER j, iL, ioUnit
      LOGICAL prtMsg
      LOGICAL useCurrentDir
      _RS     dummyRS(1)
      CHARACTER*(2) fType
      CHARACTER*(MAX_LEN_FNAM) fName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C-----

      iL = ILNBLNK(mFldsFile)
#ifdef RW_SAFE_MFLDS
      IF ( iL.EQ.0 ) THEN
        WRITE(msgBuf,'(2A,I4)') 'READ_MFLDS_3D_RL: ',
     &    'empty MFLDS file-name'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R READ_MFLDS_3D_RL (fileName)'
      ENDIF
#endif /* RW_SAFE_MFLDS */

      ioUnit = standardMessageUnit
      prtMsg = debugLevel.GE.debLevA .AND. myThid.EQ.1
      IF ( nFlds.GE.1 ) THEN
C--   Search for "fldName" in list of field-names:
        nj = 0
        DO j=1,nFlds
          IF ( fldName.EQ.fldList(j) .AND. nj.EQ.0 ) nj = j
        ENDDO
        IF ( nj.EQ.0 ) THEN
C-      record unsuccessful search:
          _BEGIN_MASTER( myThid )
          nMissFld = nMissFld + 1
          j = MIN(nMissFld,sizFldList)
          fldMiss(j) = fldName
          _END_MASTER( myThid )
          IF ( prtMsg ) THEN
            iL = ILNBLNK(mFldsFile)
            iL = MIN(iL,MAX_LEN_MBUF-54-20)
            WRITE(msgBuf,'(5A)') 'READ_MFLDS_3D_RL: ',
     &       'field: "',fldName,'" missing in file: ',mFldsFile(1:iL)
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
        ELSE
C-      convert from field Number to record number (if mix of 3D & 2D flds)
          j = nj
          IF ( nj.GT.nFl3D ) nj = nj + nFl3D*(thirdDim-1)
          IF ( prtMsg ) THEN
            WRITE(msgBuf,'(3A,2(A,I4))') 'READ_MFLDS_3D_RL: ',
     &       'read field: "',fldName,'", #',j,' in fldList, rec=',nj
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
        ENDIF
      ELSEIF ( nj.GE.0 ) THEN
C-      increment record number
        nj = nj + 1
        IF ( prtMsg ) THEN
            WRITE(msgBuf,'(3A,2(A,I4))') 'READ_MFLDS_3D_RL: ',
     &       'no fldList, try to read field "',fldName, '", rec=',nj
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF

      IF ( nj.GE.1 ) THEN
C--   read in array "field"
        fName = mFldsFile
        useCurrentDir = .FALSE.
        fType = 'RL'
#ifdef ALLOW_MDSIO
        CALL MDS_READ_FIELD(
     I                       fName, fPrec, useCurrentDir,
     I                       fType, nNz, 1, nNz,
     O                       field, dummyRS,
     I                       nj, myThid )

#endif
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_MFLDS_LEV_RL
C     !INTERFACE:
      SUBROUTINE READ_MFLDS_LEV_RL(
     I                fldName,
     O                field,
     U                nj,
     I                fPrec, kSiz, kLo, kHi, myIter, myThid )

C     !DESCRIPTION:
C     Read, from a Multi-Fields binary file, field "fldName", a set of
C     consecutive levels (from kLo to kHi) into 3D RL array "field" (size: kSiz)
C     record Nb "nj" is search through the field-list (from meta-file) which
C     has been set before (calling READ_MFLDS_SET).
C     In case nFlds is <=0, by-pass the search and directly read record number "nj"

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RW_MFLDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     fldName :: Name of the field to read
C     field   :: Output array (RL type) to read in
C     nj (in) :: number of the record (in file) just before the one to read
C     nj (out):: number of the record (from current file) which was read in
C     fPrec   :: File precision (number of bits per word, = 32 or 64)
C     kSiz    :: size of third dimension of array "field" to read-in
C     kLo     :: 1rst vertical level (of array "field") to read-in
C     kHi     :: last vertical level (of array "field") to read-in
C     myIter  :: Iteration number
C     myThid  :: My Thread Id number
      CHARACTER*(8) fldName
      _RL     field(*)
      INTEGER nj
      INTEGER fPrec
      INTEGER kSiz, kLo, kHi
      INTEGER myIter
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER j, iL, ioUnit
      LOGICAL prtMsg
      LOGICAL useCurrentDir
      _RS     dummyRS(1)
      CHARACTER*(2) fType
      CHARACTER*(MAX_LEN_FNAM) fName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C-----

      iL = ILNBLNK(mFldsFile)
#ifdef RW_SAFE_MFLDS
      IF ( iL.EQ.0 ) THEN
        WRITE(msgBuf,'(2A,I4)') 'READ_MFLDS_LEV_RL: ',
     &    'empty MFLDS file-name'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R READ_MFLDS_LEV_RL (fileName)'
      ENDIF
#endif /* RW_SAFE_MFLDS */

      ioUnit = standardMessageUnit
      prtMsg = debugLevel.GE.debLevA .AND. myThid.EQ.1
      IF ( nFlds.GE.1 ) THEN
C--   Search for "fldName" in list of field-names:
        nj = 0
        DO j=1,nFlds
          IF ( fldName.EQ.fldList(j) .AND. nj.EQ.0 ) nj = j
        ENDDO
        IF ( nj.EQ.0 ) THEN
C-      record unsuccessful search:
          _BEGIN_MASTER( myThid )
          nMissFld = nMissFld + 1
          j = MIN(nMissFld,sizFldList)
          fldMiss(j) = fldName
          _END_MASTER( myThid )
          IF ( prtMsg ) THEN
            iL = ILNBLNK(mFldsFile)
            iL = MIN(iL,MAX_LEN_MBUF-54-20)
            WRITE(msgBuf,'(5A)') 'READ_MFLDS_LEV_RL: ',
     &       'field: "',fldName,'" missing in file: ',mFldsFile(1:iL)
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
        ELSE
C-      convert from field Number to record number (if mix of 3D & 2D flds)
          j = nj
          IF ( nj.GT.nFl3D ) nj = nj + nFl3D*(thirdDim-1)
          IF ( prtMsg ) THEN
            WRITE(msgBuf,'(3A,2(A,I4))') 'READ_MFLDS_LEV_RL: ',
     &       'read field: "',fldName,'", #',j,' in fldList, rec=',nj
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
        ENDIF
      ELSEIF ( nj.GE.0 ) THEN
C-      increment record number
        nj = nj + 1
        IF ( prtMsg ) THEN
            WRITE(msgBuf,'(3A,2(A,I4))') 'READ_MFLDS_LEV_RL: ',
     &       'no fldList, try to read field "',fldName, '", rec=',nj
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF

      IF ( nj.GE.1 ) THEN
C--   read in array "field"
        fName = mFldsFile
        useCurrentDir = .FALSE.
        fType = 'RL'
#ifdef ALLOW_MDSIO
        CALL MDS_READ_FIELD(
     I                       fName, fPrec, useCurrentDir,
     I                       fType, kSiz, kLo, kHi,
     O                       field, dummyRS,
     I                       nj, myThid )

#endif
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_MFLDS_LEV_RS
C     !INTERFACE:
      SUBROUTINE READ_MFLDS_LEV_RS(
     I                fldName,
     O                field,
     U                nj,
     I                fPrec, kSiz, kLo, kHi, myIter, myThid )

C     !DESCRIPTION:
C     Read, from a Multi-Fields binary file, field "fldName", a set of
C     consecutive levels (from kLo to kHi) into 3D RS array "field" (size: kSiz)
C     record Nb "nj" is search through the field-list (from meta-file) which
C     has been set before (calling READ_MFLDS_SET).
C     In case nFlds is <=0, by-pass the search and directly read record number "nj"

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RW_MFLDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     fldName :: Name of the field to read
C     field   :: Output array (RS type) to read in
C     nj (in) :: number of the record (in file) just before the one to read
C     nj (out):: number of the record (from current file) which was read in
C     fPrec   :: File precision (number of bits per word, = 32 or 64)
C     kSiz    :: size of third dimension of array "field" to read-in
C     kLo     :: 1rst vertical level (of array "field") to read-in
C     kHi     :: last vertical level (of array "field") to read-in
C     myIter  :: Iteration number
C     myThid  :: My Thread Id number
      CHARACTER*(8) fldName
      _RS     field(*)
      INTEGER nj
      INTEGER fPrec
      INTEGER kSiz, kLo, kHi
      INTEGER myIter
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER j, iL, ioUnit
      LOGICAL prtMsg
      LOGICAL useCurrentDir
      _RL     dummyRL(1)
      CHARACTER*(2) fType
      CHARACTER*(MAX_LEN_FNAM) fName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C-----

      iL = ILNBLNK(mFldsFile)
#ifdef RW_SAFE_MFLDS
      IF ( iL.EQ.0 ) THEN
        WRITE(msgBuf,'(2A,I4)') 'READ_MFLDS_LEV_RS: ',
     &    'empty MFLDS file-name'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R READ_MFLDS_LEV_RS (fileName)'
      ENDIF
#endif /* RW_SAFE_MFLDS */

      ioUnit = standardMessageUnit
      prtMsg = debugLevel.GE.debLevA .AND. myThid.EQ.1
      IF ( nFlds.GE.1 ) THEN
C--   Search for "fldName" in list of field-names:
        nj = 0
        DO j=1,nFlds
          IF ( fldName.EQ.fldList(j) .AND. nj.EQ.0 ) nj = j
        ENDDO
        IF ( nj.EQ.0 ) THEN
C-      record unsuccessful search:
          _BEGIN_MASTER( myThid )
          nMissFld = nMissFld + 1
          j = MIN(nMissFld,sizFldList)
          fldMiss(j) = fldName
          _END_MASTER( myThid )
          IF ( prtMsg ) THEN
            iL = ILNBLNK(mFldsFile)
            iL = MIN(iL,MAX_LEN_MBUF-54-20)
            WRITE(msgBuf,'(5A)') 'READ_MFLDS_LEV_RS: ',
     &       'field: "',fldName,'" missing in file: ',mFldsFile(1:iL)
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
        ELSE
C-      convert from field Number to record number (if mix of 3D & 2D flds)
          j = nj
          IF ( nj.GT.nFl3D ) nj = nj + nFl3D*(thirdDim-1)
          IF ( prtMsg ) THEN
            WRITE(msgBuf,'(3A,2(A,I4))') 'READ_MFLDS_LEV_RS: ',
     &       'read field: "',fldName,'", #',j,' in fldList, rec=',nj
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
        ENDIF
      ELSEIF ( nj.GE.0 ) THEN
C-      increment record number
        nj = nj + 1
        IF ( prtMsg ) THEN
            WRITE(msgBuf,'(3A,2(A,I4))') 'READ_MFLDS_LEV_RS: ',
     &       'no fldList, try to read field "',fldName, '", rec=',nj
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF

      IF ( nj.GE.1 ) THEN
C--   read in array "field"
        fName = mFldsFile
        useCurrentDir = .FALSE.
        fType = 'RS'
#ifdef ALLOW_MDSIO
        CALL MDS_READ_FIELD(
     I                       fName, fPrec, useCurrentDir,
     I                       fType, kSiz, kLo, kHi,
     O                       dummyRL, field,
     I                       nj, myThid )

#endif
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_MFLDS_CHECK
C     !INTERFACE:
      SUBROUTINE READ_MFLDS_CHECK(
     O                errList,
     U                nbErr,
     I                myIter, myThid )

C     !DESCRIPTION:
C     After reading a Multi-Fields binary file, check (and report)
C     for missing fields (attempted to read but not found).
C
C     Note: If missing fields, print error msg but take no action (no stop)
C           but return number of missing fields (+ list, if nbErr_inputArg > 0)
C           Depending on the calling context, may choose to stop or to continue

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RW_MFLDS.h"

C     !INPUT PARAMETERS:
C     nbErr   :: max size of array errList
C     myIter  :: Iteration number
C     myThid  :: My Thread Id number
C     !OUTPUT PARAMETERS:
C     errList :: List of missing fields   (attempted to read but not found)
C     nbErr   :: Number of missing fields (attempted to read but not found)
      CHARACTER*(8) errList(*)
      INTEGER nbErr
      INTEGER myIter
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER i, j, nj, iL, ioUnit
      LOGICAL prtMsg
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C-----

      iL = ILNBLNK(mFldsFile)
#ifdef RW_SAFE_MFLDS
      IF ( iL.EQ.0 ) THEN
        WRITE(msgBuf,'(2A,I4)') 'READ_MFLDS_CHECK: ',
     &    'empty MFLDS file-name'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R READ_MFLDS_CHECK (fileName)'
      ENDIF
#endif /* RW_SAFE_MFLDS */

C--   Initialise output arguments
      DO j=1,nbErr
       errList(j) = ' '
      ENDDO

C--   every one waits for master thread to finish the update of
C       missing fields number & list.
      _BARRIER

      IF ( nMissFld.GE.1 ) THEN
C--   Attempted to read some fields that were not in the current MFLDS file
C     => report by printing Error Msg:
         ioUnit = errorMessageUnit
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(3A)') 'READ_MFLDS_CHECK: ',
     &     'reading from file: ', mFldsFile(1:iL)
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A,I4,A)') 'READ_MFLDS_CHECK: ',
     &     'which contains ', nFlds, ' fields :'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         DO j=1,nFlds,20
          WRITE(msgBuf,'(20(A2,A8,A))')
     &     (' >', fldList(i), '<', i=j,MIN(j+19,nFlds) )
          CALL PRINT_MESSAGE(msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         ENDDO
         WRITE(msgBuf,'(A,I4,A)') 'READ_MFLDS_CHECK: ',
     &                  nMissFld, ' field(s) is/are missing :'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         nj = MIN( nMissFld, sizFldList )
         DO j=1,nj,20
          WRITE(msgBuf,'(20(A2,A8,A))')
     &     (' >', fldMiss(i), '<', i=j,MIN(j+19,nj) )
          CALL PRINT_MESSAGE(msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         ENDDO
         _END_MASTER( myThid )

C-       Size problem:
         IF ( nMissFld.GT.sizFldList ) THEN
           WRITE(msgBuf,'(2A,I4)') 'READ_MFLDS_CHECK: ',
     &      'missing fields list has been truncated to', sizFldList
           CALL PRINT_ERROR( msgBuf, myThid )
           CALL ALL_PROC_DIE( myThid )
           STOP 'ABNORMAL END: S/R READ_MFLDS_CHECK (list-size Pb)'
         ENDIF

C-       Fill the error output list (up to the Max size: nbErr)
         nj = MIN( nMissFld, nbErr )
         DO j=1,nj
          errList(j) = fldMiss(j)
         ENDDO
      ELSE
C--   Normal end : print msg before resetting "mFldsFile"
        ioUnit = standardMessageUnit
        prtMsg = debugLevel.GE.debLevA .AND. myThid.EQ.1
        IF ( prtMsg ) THEN
         WRITE(msgBuf,'(3A)') 'READ_MFLDS_CHECK: ',
     &     '- normal end ; reset MFLDS file-name: ', mFldsFile(1:iL)
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDIF

      ENDIF

C-    Return the number of missing fields
      nbErr = nMissFld

#ifdef RW_SAFE_MFLDS
      _BARRIER
      _BEGIN_MASTER( myThid )
C--   Reset MFLDS file name:
       mFldsFile = ' '
      _END_MASTER( myThid )
      _BARRIER
#endif /* RW_SAFE_MFLDS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_MFLDS_RENAME
C     !INTERFACE:
      SUBROUTINE READ_MFLDS_RENAME(
     I                              fldName, newName,
     O                              errCode,
     I                              myThid )

C     !DESCRIPTION:
C     Rename one field in fldList

C     !USES:
      IMPLICIT NONE
c#include "SIZE.h"
#include "EEPARAMS.h"
#include "RW_MFLDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     fldName  :: field name to rename
C     newName  :: new name to replace fldName
C     errCode  :: returned error code:
C                 0 = succesful ; 1 = fldName not found ; > 1 : error
C     myThid   :: my Thread Id. number
      CHARACTER*(8) fldName
      CHARACTER*(8) newName
      INTEGER errCode
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i , j    :: loop counter
      INTEGER i , j
C-    for debug print:
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      errCode = 1

C-    search for fldName in fldList:
      j = 0
      DO i=1,nFlds
        IF ( fldList(i) .EQ. fldName ) THEN
          IF ( j.EQ.0 ) THEN
            errCode = 0
            j = i
          ELSE
C--    fldName appears more than once in fldList (errCode=3):
            errCode = 3
          ENDIF
        ENDIF
      ENDDO

      IF ( errCode.EQ.0 ) THEN
C--   Do not replace if newName is already in the list (errCode=2):
        DO i=1,nFlds
          IF ( fldList(i).EQ.newName ) errCode = 2
        ENDDO
      ENDIF

      IF ( errCode.EQ.0 ) THEN
        _BARRIER
        _BEGIN_MASTER( myThid )
        fldList(j) = newName
        _END_MASTER( myThid )
        _BARRIER
      ENDIF

      RETURN
      END
