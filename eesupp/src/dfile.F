C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/dfile.F,v 1.10 2000/03/15 16:00:52 adcroft Exp $

#include "CPP_EEOPTIONS.h"

C-- File dfile.F: Routines that handle actual I/O
C--               to model "dump" files.
C--               These low-level routines could be replaced
C--               by platform/environment specific C or
C--               MPI-2 routines one day! In some situations the
C--               functionality of these low-level routines is
C--               encompassed by the data handling package. For
C--               example netCDF provides primitive that are
C--               higher level
C-- Contents
C-- DFILE_CLOSE    - Closes dump file
C-- DFILE_INIT     - Initialisation procedure for subsequent DFILE 
C                    data-structures. Only called once per run.
C-- DFILE_OPEN     - Opens dump file
C-- DFILE_READ_R8  - Reads from a dump file
C-- DFILE_READ_R4  - Reads from a dump file
C-- DFILE_SET_RO            - Sets new connections to be read-only
C-- DFILE_SET_RW            - Sets new connections to be read-write
C-- DFILE_SET_STOP_ON_ERROR - Sets new connections to STOP on error
C-- DFILE_SET_CONT_ON_ERROR - Sets new connections to continue 
C                             on error
C-- DFILE_WRITE_R4 - Writes to a dump file
C-- DFILE_WRITE_R8 - Writes to a dump file
C
C   Notes: 
C   ======
C    The default behaviour is for the model to stop if an
C   input errors occur but to continue if output errors occur.
C   However, this policy is not enforced in these low-level routines.
C   Instead these routines are coded to allow either continue
C   on error or stop on error. Which action is taken
C   is controlled via a mode flag which is set from the higher
C   level calls to these routines. A mode flag is also used to 
C   control whether the DFILE_OPEN routine opens a file in
C   read-write or read-only mode. On some systems this is necessary
C   as the default is read-write and will fail for read-only files or
C   file systems. Other systems do not support the OPEN(...='READ_ONLY')
C   so this feature may need to be switched on or off as appropriate.
C    The DFILE_SET routines provide this mechanism. They work by setting
C   a "context" flag which is applied to IO ahndles when the DFILE_OPEN 
C   call is made. IO handles that are already open are not affected by 
C   subsequent calls to DFILE_SET routines.

      SUBROUTINE DFILE_CLOSE(
     I                       fileHandle, myThid)
C     /==========================================================\
C     | SUBROUTINE DFILE_CLOSE                                   |
C     | o Close model "dump" file.                               |
C     |==========================================================|
C     | Controlling routine for doing actual I/O operations.     |
C     | Close the file referred to by handle fielHandle.         |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "DFILE.h"

C     == Routine arguments ==
      INTEGER myThid
      INTEGER fileHandle

#ifdef USE_DFILE

C     == Local variables ==
C     msgBuf - Error message buffer
C     I      - Work variables
C     dUnit    Data unit
C     mUnit    Meta data unit
C     eMode    Error mode
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER I
      INTEGER dUnit
      INTEGER mUnit
      INTEGER eMode

      I = fileHandle

C--   Check that the fileHandle passed in is open
      IF ( unitStatus(I,myThid) .NE. busyUnit ) GOTO 1000

       unitStatus(I,myThid)     = freeUnit
       dUnit = dUnitNumber(I,myThid)
       mUnit = mUnitNumber(I,myThid)
       eMode =   errorMode(I,myThid)
       CLOSE(dUnit,ERR=999)
       CLOSE(mUnit,ERR=999)

 1000 CONTINUE
      RETURN
  999 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_CLOSE'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A)') ' Thread ', myThid,' Close file failed'
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_CLOSE'
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)+1
      GOTO 1000

      END

      SUBROUTINE DFILE_INIT
C     /==========================================================\
C     | SUBROUTINE DFILE_INIT                                    |
C     | o Model "dump" file initialisation procedure             |
C     |==========================================================|
C     | Initalises data structures used by MITgcmUV "dump file"  |
C     | procedures.                                              |
C     | As coded this routine sets the unit number used for      |
C     | dump file IO. Two numbers are used one for data and one  |
C     | for meta data. It is possible to use more unit numbers   |
C     | and/or have different unit numbers per thread. This is   |
C     | not done here.                                           |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DFILE.h"

C     == Local variables ==
C     I, J - Loop counters
      INTEGER I, J

      DO j=1,MAX_NO_THREADS
       DO i=1,ioUnitsPerThread
        mUnitNumber(i,j) = 20+i*2-1
        dUnitNumber(i,j) = 20+i*2
        unitStatus (i,j) = freeUnit
        metaDataStatus(i,j) = metaDataNotWritten
       ENDDO
      ENDDO

C--   Set initial access and error modes
      CALL DFILE_SET_RW
      CALL DFILE_SET_STOP_ON_ERROR

      RETURN 
      END

      SUBROUTINE DFILE_OPEN( 
     I                       fNam, fNamMeta, myThid, 
     O                       fileHandle)
C     /==========================================================\
C     | SUBROUTINE DFILE_OPEN                                    |
C     | o Open model "dump" file.                                |
C     |==========================================================|
C     | Controlling routine for doing actual I/O operations.     |
C     | Routine returns a handle to the caller that can be used  |
C     | in subsequent read and write operations.                 |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "DFILE.h"

      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == Routine arguments ==
      CHARACTER*(*) fNam
      CHARACTER*(*) fNamMeta
      INTEGER myThid
      INTEGER fileHandle

C     == Local variables ==
C     msgBuf - Error message buffer
C     dUnit  - Unit number for data
C     mUnit  - Unit number for meta data
C     eMode  - Error mode
C     aMode  - Access mode
C     I      - Loop counters
      INTEGER dUnit
      INTEGER mUnit
      INTEGER eMode
      INTEGER aMode
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER I
      INTEGER i1Lo, i1Hi,  i2Lo, i2Hi

C--   Get statistics on names
      i1Lo = IFNBLNK(fNam)
      i1Hi = ILNBLNK(fNam)
      i2Lo = IFNBLNK(fNamMeta)
      i2Hi = ILNBLNK(fNamMeta)

C--   Choose a free I/O unit
      fileHandle = -1
      dUnit = 0
      DO I=1, ioUnitsPerThread
       IF ( unitStatus(I,myThid) .EQ. freeUnit ) THEN
        dUnit = dUnitNumber(I,myThid)
        mUnit = mUnitNumber(I,myThid)
        unitStatus(I,myThid) = busyUnit
        errorMode(I,myThid)  = theErrorMode
        accessMode(I,myThid) = theAccessMode
        eMode                = theErrorMode
        aMode                = theAccessMode
        fileHandle = I
        GOTO 10
       ENDIF
      ENDDO
   10 CONTINUE
      IF ( dUnit .EQ. 0 ) GOTO 999

C--   Remove previous meta information if there was any
      metaDataStatus(fileHandle,myThid) = metaDataWritten
      IF ( fNamMeta .NE. ' ' ) THEN
       IF ( aMode .EQ. accessModeRW ) THEN
        OPEN(UNIT=mUnit,FILE=fNamMeta(i2Lo:i2Hi),
     &       STATUS='UNKNOWN',ERR=899)
        CLOSE(mUnit,ERR=899)
        OPEN(UNIT=mUnit,FILE=fNamMeta(i2Lo:i2Hi),
     &       STATUS='UNKNOWN',ERR=899)
        metaDataStatus(fileHandle,myThid) = metaDataNotWritten
        nameOfMFile(fileHandle,myThid) = fNamMeta(i2Lo:i2Hi)
       ENDIF
      ENDIF

C--   Open data file
      nameOfDFile(fileHandle,myThid) = fNam(i1Lo:i1Hi)
      OPEN(UNIT=dUnit,FILE=fNam(i1Lo:i1Hi),STATUS='UNKNOWN', ERR=799,
     &     FORM='UNFORMATTED',ACCESS='SEQUENTIAL')


 1000 CONTINUE
      RETURN

  999 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_OPEN '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A)') ' Too many open files '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,I4,A,A)') ' Thread ', myThid,' trying to open ',
     & fNam(i1Lo:i1Hi)
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_OPEN '
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)  + 1
      GOTO 1000

  899 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_OPEN '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,I4,A,A)') ' Thread ',myThid,' failed open for ',
     & fNamMeta(i2Lo:i2Hi)
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_OPEN '
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)  + 1
      GOTO 1000

  799 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_OPEN '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,I4,A,A)') ' Thread ',myThid,' failed open for ',
     & fNam(i1Lo:i1Hi)
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_OPEN '
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)  + 1
      GOTO 1000
       
      END

      SUBROUTINE DFILE_READ_R4( 
     I                       lBuffer,
     I                       fileHandle, myThid)
C     /==========================================================\
C     | SUBROUTINE DFILE_READ_R4                                 |
C     | o Read record(s) from model dump file.                   |
C     |==========================================================|
C     | Controlling routine for doing actual I/O operations.     |
C     | Routine reads data from binary files formatted for       |
C     | model input. Could do elaborate reads from netCDF or     |
C     | using regular C I/O primitives. For now we use plain     |
C     | F77.                                                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DFILE.h"

      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == Routine arguments ==
C     lBuffer    - Length of buffer data will be read into
C     fileHandle - Handle of already opened file
C     myThid     - Thread id calling this routine
      INTEGER lBuffer
      INTEGER fileHandle
      INTEGER myThid

C     == Local variables ==
C     ioUnit - Unit number associated with fileHandle
C     I      - Loop counter
C     eMode  - fileHandles error mode
      CHARACTER*(MAX_LEN_FNAM) fNam
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ioUnit
      INTEGER I, iLo, iHi
      INTEGER eMode

C--   Get error mode
      eMode = errorMode(fileHandle,myThid)

C--   Check that file is active
      IF ( unitStatus(fileHandle,myThid) .NE. busyUnit ) GOTO 999

       fNam   = nameOfDFile(fileHandle,myThid)
       iLo = IFNBLNK(fNam)
       iHi = ILNBLNK(fNam)
       ioUnit = dUnitNumber(fileHandle,myThid)
       READ(ioUnit,ERR=899) (ioBuf_R4(I),I=1,lBuffer)


 1000 CONTINUE
      RETURN

  999 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_READ_R4 '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,I4,A)') ' Thread ', myThid, ' unit not open '
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_READ_R4'
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)  + 1
      GOTO 1000

  899 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_READ_R4 '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,I4,A)') ' Thread ', myThid,' error reading file'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A,A)') ' "', fNam(iLo:iHi),'"'
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_READ_R4'
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)  + 1
      GOTO 1000

      END
      SUBROUTINE DFILE_READ_R8( 
     I                       lBuffer,
     I                       fileHandle, myThid)
C     /==========================================================\
C     | SUBROUTINE DFILE_READ_R8                                 |
C     | o Read record(s) from model dump file.                   |
C     |==========================================================|
C     | Controlling routine for doing actual I/O operations.     |
C     | Routine reads data from binary files formatted for       |
C     | model input. Could do elaborate reads from netCDF or     |
C     | using regular C I/O primitives. For now we use plain     |
C     | F77.                                                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DFILE.h"

      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == Routine arguments ==
C     lBuffer    - Length of buffer data will be read into
C     fileHandle - Handle of already opened file
C     myThid     - Thread id calling this routine
      INTEGER lBuffer
      INTEGER fileHandle
      INTEGER myThid

C     == Local variables ==
C     ioUnit - Unit number associated with fileHandle
C     I      - Loop counter
C     eMode  - fileHandles error mode
      CHARACTER*(MAX_LEN_FNAM) fNam
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ioUnit
      INTEGER I, iLo, iHi
      INTEGER eMode

C--   Get error mode
      eMode = errorMode(fileHandle,myThid)

C--   Check that file is active
      IF ( unitStatus(fileHandle,myThid) .NE. busyUnit ) GOTO 999

       fNam   = nameOfDFile(fileHandle,myThid)
       iLo = IFNBLNK(fNam)
       iHi = ILNBLNK(fNam)
       ioUnit = dUnitNumber(fileHandle,myThid)
       READ(ioUnit,ERR=899) (ioBuf_R8(I),I=1,lBuffer)


 1000 CONTINUE
      RETURN

  999 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_READ_R8 '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,I4,A)') ' Thread ', myThid, ' unit not open '
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_READ_R8'
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)  + 1
      GOTO 1000

  899 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_READ_R8 '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,I4,A)') ' Thread ', myThid,' error reading file'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A,A)') ' "', fNam(iLo:iHi),'"'
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_READ_R8'
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)  + 1
      GOTO 1000

      END

      SUBROUTINE DFILE_SET_RO            
C     /==========================================================\
C     | SUBROUTINE DFILE_SET_RO                                  |
C     | o Sets new connections to be read-only.                  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DFILE.h"
 
      theAccessMode = accessModeRO

      RETURN
      END

      SUBROUTINE DFILE_SET_RW            
C     /==========================================================\
C     | SUBROUTINE DFILE_SET_RW                                  |
C     | o Sets new connections to be read-write                  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DFILE.h"
 
      theAccessMode = accessModeRW

      RETURN
      END

      SUBROUTINE DFILE_SET_STOP_ON_ERROR 
C     /==========================================================\
C     | SUBROUTINE DFILE_SET_STOP_ON_ERROR                       |
C     | o Sets new connections to STOP on error                  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DFILE.h"
 
      theErrorMode = errorModeSTOP

      RETURN
      END

      SUBROUTINE DFILE_SET_CONT_ON_ERROR 
C     /==========================================================\
C     | SUBROUTINE DFILE_SET_CONT_ON_ERROR                       |
C     | o Sets new connections to continue on error              |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DFILE.h"
 
      theErrorMode = errorModeCONT

      RETURN
      END

      SUBROUTINE DFILE_WRITE_R4( 
     I                       lBuffer,
     I                       nDims, dimList,
     I                       fileHandle, fileId, myThid )
C     /==========================================================\
C     | SUBROUTINE DFILE_WRITE_R4                                |
C     | o Write record(s) to model dump file.                    |
C     |==========================================================|
C     | Controlling routine for doing actual I/O operations.     |
C     | Routine writes data to binary files.                     |
C     | Could do elaborate write to   netCDF or                  |
C     | use C I/O primitives. For now we use plain F77 but the   |
C     | routine does write both data and metadata. Metadata is   |
C     | extra info. which describes the data - in this case it   |
C     | is information indicating the subregion of the global    |
C     | dataset being written out.                               |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DFILE.h"

C     == Routine arguments ==
C     lBuffer - Amount of data written
C     nDims   - Global and subset dimensionality
C     dimList - List of global and subset extents
C     fileHandle - Handle identifying actual IO unit
C     myThid  - Thread number of thread calling this 
C               routine
C     eMode   - error mode for this fileHandle
      INTEGER lBuffer
      INTEGER nDims
      INTEGER dimList(nDims*3)
      INTEGER fileHandle
      INTEGER fileId
      INTEGER myThid

C     == Local variables ==
C     ioUnit - Unit number for I/O
C     msgBuf - Textual printing message buffer
C     eMode  - Error mode for this file handle
      INTEGER ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) fNam
      INTEGER eMode
      INTEGER I

C--   Set error mode
      eMode = errorMode(fileHandle,myThid)

C--   Check that file is active
      IF ( unitStatus(fileHandle,myThid) .NE. busyUnit ) GOTO 999

C--   Write data
      ioUnit = dUnitNumber(fileHandle,myThid)
      fNam   = nameOfDFile(fileHandle,myThid)
      WRITE(ioUnit,ERR=899) (ioBuf_R4(I),I=1,lBuffer)

C--   Now write meta information
      IF ( metaDataStatus(fileHandle,myThid) .EQ. 
     &     metaDataNotWritten ) THEN
       ioUnit = mUnitNumber(fileHandle,myThid)

       WRITE(msgBuf,'(A)') '// START OF META DATA'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

       WRITE(msgBuf,'(A,A)') ' id =[ ','/* Identifier */'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       CALL PRINT_LIST_I( fileId, 1, INDEX_NONE, .FALSE., .TRUE., 
     &                    ioUnit )
       WRITE(msgBuf,'(A)')   '   ];     '
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

       WRITE(msgBuf,'(A,A)') ' nDims =[ ','/* Number of dimensions */'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       CALL PRINT_LIST_I( nDims, 1, INDEX_NONE, .FALSE., .TRUE., 
     &                    ioUnit )
       WRITE(msgBuf,'(A)')   '   ];     '
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

       WRITE(msgBuf,'(A,A)') ' dimList =[ ',
     &  '/* Global1, local min1, local max1, ... */'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       DO I=1,nDims
        CALL PRINT_LIST_I( dimList((I-1)*3+1), 3, INDEX_NONE, 
     &                    I.NE.nDims, .FALSE., ioUnit )
       ENDDO
       WRITE(msgBuf,'(A)')   '   ];     '
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

       WRITE(msgBuf,'(A,A)') ' format =[ ','/* Field format */'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       WRITE(msgBuf,'(16X,A)')   '''float32'''
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       WRITE(msgBuf,'(A)')   '   ];     '
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

       WRITE(msgBuf,'(A)') '// END OF META DATA'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       metaDataStatus(fileHandle,myThid) = metaDataWritten
      ENDIF

 1000 CONTINUE
      RETURN

  999 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_WRITE_R4 '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A)') ' Thread ', myThid, ' unit not open '
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_WRITE_R4'
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)  + 1
      GOTO 1000

  899 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_WRITE_R4 '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A)') ' Thread ', myThid, ' write error '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A)') ' File ', fNam
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_WRITE_R4'
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)  + 1
      GOTO 1000
      
      END

      SUBROUTINE DFILE_WRITE_R8( 
     I                       lBuffer,
     I                       nDims, dimList,
     I                       fileHandle, fileId, myThid )
C     /==========================================================\
C     | SUBROUTINE DFILE_WRITE_R8                                |
C     | o Write record(s) to model dump file.                    |
C     |==========================================================|
C     | Controlling routine for doing actual I/O operations.     |
C     | Routine writes data to binary files.                     |
C     | Could do elaborate write to   netCDF or                  |
C     | use C I/O primitives. For now we use plain F77 but the   |
C     | routine does write both data and metadata. Metadata is   |
C     | extra info. which describes the data - in this case it   |
C     | is information indicating the subregion of the global    |
C     | dataset being written out.                               |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DFILE.h"

C     == Routine arguments ==
C     buffer  - Subset data to write
C     lBuffer - Amount of data written
C     nDims   - Global and subset dimensionality
C     dimList - List of global and subset extents
C     fileHandle - Handle identifying actual IO unit
C     myThid  - Thread number of thread calling this 
C               routine
C     eMode   - error mode for this fileHandle
      INTEGER lBuffer
      INTEGER nDims
      INTEGER dimList(nDims*3)
      INTEGER fileHandle
      INTEGER fileId
      INTEGER myThid

C     == Local variables ==
C     ioUnit - Unit number for I/O
C     msgBuf - Textual printing message buffer
C     eMode  - Error mode for this file handle
      INTEGER ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) fNam
      INTEGER eMode
      INTEGER I

C--   Set error mode
      eMode = errorMode(fileHandle,myThid)

C--   Check that file is active
      IF ( unitStatus(fileHandle,myThid) .NE. busyUnit ) GOTO 999

C--   Write data
      ioUnit = dUnitNumber(fileHandle,myThid)
      fNam   = nameOfDFile(fileHandle,myThid)
      WRITE(ioUnit,ERR=899) (ioBuf_R8(I),I=1,lBuffer)

C--   Now write meta information
      IF ( metaDataStatus(fileHandle,myThid) .EQ. 
     &                    metaDataNotWritten ) THEN
       ioUnit = mUnitNumber(fileHandle,myThid)

       WRITE(msgBuf,'(A)') '// START OF META DATA'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

       WRITE(msgBuf,'(A,A)') ' id =[ ','/* Identifier */'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       CALL PRINT_LIST_I( fileId, 1, INDEX_NONE, .FALSE., .TRUE., 
     &                    ioUnit )
       WRITE(msgBuf,'(A)')   '   ];     '
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

       WRITE(msgBuf,'(A,A)') ' nDims =[ ','/* Number of dimensions */'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       CALL PRINT_LIST_I( nDims, 1, INDEX_NONE, .FALSE., .TRUE., 
     &                    ioUnit )
       WRITE(msgBuf,'(A)')   '   ];     '
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

       WRITE(msgBuf,'(A,A)') ' dimList =[ ',
     &  '/* Global1, local min1, local max1, ... */'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       DO I=1,nDims
        CALL PRINT_LIST_I( dimList((I-1)*3+1), 3, INDEX_NONE, 
     &                    I.NE.nDims, .FALSE., ioUnit )
       ENDDO
       WRITE(msgBuf,'(A)')   '   ];     '
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

       WRITE(msgBuf,'(A,A)') ' format =[ ','/* Field format */'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       WRITE(msgBuf,'(16X,A)')   '''float64'''
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       WRITE(msgBuf,'(A)')   '   ];     '
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)

       WRITE(msgBuf,'(A)') '// END OF META DATA'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT , 1)
       metaDataStatus(fileHandle,myThid) = metaDataWritten
      ENDIF

 1000 CONTINUE
      RETURN

  999 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_WRITE_R8 '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A)') ' Thread ', myThid, ' unit not open '
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_WRITE_R8'
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)  + 1
      GOTO 1000

  899 CONTINUE
       WRITE(msgBuf,'(A)') ' S/R DFILE_WRITE_R8 '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A)') ' Thread ', myThid, ' write error '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A)') ' File ', fNam
       CALL PRINT_ERROR( msgBuf , 1)
       IF ( eMode .EQ. errorModeSTOP ) THEN
        STOP 'ABNORMAL END: S/R DFILE_WRITE_R8'
       ENDIF
       ioErrorCount(myThid) = ioErrorCount(myThid)  + 1
      GOTO 1000

#endif /* USE_DFILE */

      END
