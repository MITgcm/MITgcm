#include "MDSIO_OPTIONS.h"

CBOP
C !ROUTINE: MDS_READVEC_LOC
C !INTERFACE:
      SUBROUTINE MDS_READVEC_LOC(
     I   fName,
     I   filePrec,
     U   ioUnit,
     I   arrType,
     I   nSize,
     I   fldRL, fldRS,
     I   bi, bj,
     I   iRec,
     I   myThid )

C !DESCRIPTION:
C Arguments:
C
C fName    string  :: base name for file to read
C filePrec integer :: number of bits per word in file (32 or 64)
C ioUnit   integer :: fortran file IO unit
C nSize    integer :: number of elements of input array "fldRL/RS" to read
C arrType  char(2) :: which array (fldRL/RS) to read, either "RL" or "RS"
C fldRL    ( RL )  :: array to read if arrType="RL", fldRL(nSize)
C fldRS    ( RS )  :: array to read if arrType="RS", fldRS(nSize)
C bi,bj    integer :: tile indices (if tiled array) or 0,0 if not a tiled array
C iRec     integer :: record number to read
C myThid   integer :: my Thread Id number
C
C MDS_READVEC_LOC : reads a vector (local to tile bi,bj) from binary file:
C according to ioUnit:
C  ioUnit > 0 : assume file "ioUnit" is open, and read from it.
C  ioUnit = 0 : open file, read and close the file (return ioUnit=0).
C  ioUnit =-1 : open file, read and leave it open (return IO unit in ioUnit)
C  ioUnit =-2 : same as -1 except keep ioUnit=-2 (no stop) if missing file
C if bi=bj=0, MDS_READVEC_LOC first check if the file "fName" exists,
C  then if the file "fName.data" exists, and read from the 1rst found.
C if bi,bj >0, read from MDS tiled files of the form "fName.xxx.yyy.data"
C The precision of the file is described by filePrec, set either
C  to floatPrec32 or floatPrec64.
C iRec is the record number to read and must be >=1.

C !USES:
      IMPLICIT NONE

C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_FIZHI
# include "fizhi_SIZE.h"
#endif /* ALLOW_FIZHI */
#include "MDSIO_BUFF_3D.h"

C !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) fName
      INTEGER ioUnit
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER nSize
      _RL     fldRL(*)
      _RS     fldRS(*)
      INTEGER bi,bj
      INTEGER iRec
      INTEGER myThid

C !FUNCTIONS:
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN

C !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) dataFname, pfName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL exst
      LOGICAL fileIsOpen
      INTEGER iG,jG,dUnit,IL,pIL,iLfn
      INTEGER length_of_rec
      INTEGER buffSize
CEOP

C---  Only DO I/O IF I am the master thread
      _BEGIN_MASTER( myThid )

C--   Assume nothing
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )

C--   Record number must be >= 1
      IF ( iRec.LT.1 ) THEN
         WRITE(msgBuf,'(A,I9)')
     &     ' MDS_READVEC_LOC: argument iRec = ',iRec
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &    ' MDS_READVEC_LOC: invalid value for iRec'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R MDS_READVEC_LOC'
      ENDIF

C--   Check buffer size
      buffSize = sNx*sNy*size3dBuf*nSx*nSy
      IF ( nSize.GT.buffSize ) THEN
         WRITE(msgBuf,'(3A)')
     &    ' MDS_READVEC_LOC: reading from file "', fName(1:IL), '":'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,I9)')
     &     ' MDS_READVEC_LOC: dim of array to read=', nSize
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,I9)')
     &     ' MDS_READVEC_LOC: exceeds buffer size=', buffSize
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &    ' increase "size3dBuf" in "MDSIO_BUFF_3D.h" and recompile'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R MDS_READVEC_LOC'
      ENDIF

      IF ( ioUnit.GT.0 ) THEN
C--   Assume file Unit is already open with correct Rec-Length & Precision
         fileIsOpen = .TRUE.
         dUnit = ioUnit
      ELSEIF ( ioUnit.GE.-2 ) THEN
C--   Need to open file IO unit with File-name, Rec-Length & Precision

C-    Assign special directory
        IF ( mdsioLocalDir .NE. ' ' ) THEN
          pIL = ILNBLNK( mdsioLocalDir )
          WRITE(pFname,'(2A)') mdsioLocalDir(1:pIL), fName(1:IL)
          pIL = IL + pIL
        ELSE
          WRITE(pFname,'(A)') fName(1:IL)
          pIL = IL
        ENDIF

C-    Assign a free unit number as the I/O channel for this routine
        CALL MDSFINDUNIT( dUnit, myThid )

C--   Set the file Name:
        IF ( bi.EQ.0 .AND. bj.EQ.0 ) THEN
C-    Check first for global file with simple name (ie. fName)
          WRITE(dataFname,'(2A)') fName(1:IL)
          iLfn = IL
          INQUIRE( file=dataFname, exist=exst )
c         IF (exst) THEN
c           write(0,*) 'found file: ',dataFname(1:iLfn)
c         ENDIF
          IF ( .NOT.exst ) THEN
C-    Check for global file with ".data" suffix
            WRITE(dataFname,'(2A)') fName(1:IL),'.data'
            iLfn = IL+5
            INQUIRE( file=dataFname, exist=exst )
c           IF (exst) THEN
c            write(0,*) 'found file: ',dataFname(1:iLfn)
c           ENDIF
          ENDIF
        ELSE
C-    We are reading a tiled array (bi>0,bj>0):
          iG=bi+(myXGlobalLo-1)/sNx
          jG=bj+(myYGlobalLo-1)/sNy
          WRITE(dataFname,'(2A,I3.3,A,I3.3,A)')
     &          pfName(1:pIL),'.',iG,'.',jG,'.data'
          iLfn= pIL+8+5
          INQUIRE( file=dataFname, exist=exst )
c         IF (exst) THEN
c          write(0,*) 'found file: ',dataFname(1:iLfn)
c         ENDIF
        ENDIF
C--   Open the file:
        IF ( exst ) THEN
          IF ( debugLevel.GE.debLevB ) THEN
            WRITE(msgBuf,'(2A)')
     &      ' MDS_READVEC_LOC: open file: ',dataFname(1:iLfn)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
          ENDIF
          length_of_rec = MDS_RECLEN( filePrec, nSize, myThid )
          OPEN( dUnit, file=dataFname, status=_OLD_STATUS,
     &          _READONLY_ACTION access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
        ELSE
          fileIsOpen=.FALSE.
          WRITE(msgBuf,'(3A)')
     &     'S/R MDS_READVEC_LOC: file=',dataFname(1:iLfn),' not found'
          IF ( ioUnit.GE.-1 ) THEN
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R MDS_READVEC_LOC'
          ELSEIF ( debugLevel.GE.debLevA ) THEN
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
        ENDIF
      ELSE
         WRITE(msgBuf,'(A,I9,A)')  ' MDS_READVEC_LOC:',
     &         ioUnit, ' = invalid value for ioUnit argument'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R MDS_READVEC_LOC'
C--   End if block: File Unit is already open / Need to open it
      ENDIF

C--   Read from file
      IF ( fileIsOpen ) THEN
         IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_RD_REC_RS( fldRS, shared3dBuf_r4, shared3dBuf_r8,
     I                          filePrec, dUnit, iRec, nSize, myThid )
         ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_RD_REC_RL( fldRL, shared3dBuf_r4, shared3dBuf_r8,
     I                          filePrec, dUnit, iRec, nSize, myThid )
         ELSE
            WRITE(msgBuf,'(A)')
     &          ' MDS_READVEC_LOC: illegal value for arrType'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDS_READVEC_LOC'
         ENDIF
      ENDIF

C--   Close file
      IF ( fileIsOpen ) THEN
        IF ( ioUnit.EQ.-2 .OR. ioUnit.EQ.-1 ) THEN
          ioUnit = dUnit
        ELSEIF ( ioUnit.EQ.0 ) THEN
          CLOSE( dUnit )
          fileIsOpen = .FALSE.
        ENDIF
      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
