C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_writevec_loc.F,v 1.9 2013/01/13 22:43:53 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

CBOP
C !ROUTINE: MDS_WRITEVEC_LOC
C !INTERFACE:
      SUBROUTINE MDS_WRITEVEC_LOC(
     I   fName,
     I   filePrec,
     U   ioUnit,
     I   arrType,
     I   nSize,
     I   fldRL, fldRS,
     I   bi, bj,
     I   irecord,
     I   myIter,
     I   myThid )

C !DESCRIPTION:
C Arguments:
C
C fName    string  :: base name for file to written
C filePrec integer :: number of bits per word in file (32 or 64)
C ioUnit   integer :: fortran file IO unit
C nSize    integer :: number of elements from input array "fldRL/RS" to be written
C arrType  char(2) :: which array (fldRL/RS) to write, either "RL" or "RS"
C fldRL    ( RL )  :: array to write if arrType="RL", fldRL(nSize)
C fldRS    ( RS )  :: array to write if arrType="RS", fldRS(nSize)
C bi,bj    integer :: tile indices (if tiled array) or 0,0 if not a tiled array
C irecord  integer :: record number to WRITE =|irecord|
C myIter   integer :: time step number
C myThid   integer :: my Thread Id number
C
C MDS_WRITEVEC_LOC according to ioUnit:
C  ioUnit = 0 : open file, write and close the file (return ioUnit=0).
C  ioUnit =-1 : open file, write and leave it open (return IO unit in ioUnit)
C  ioUnit > 0 : assume file "ioUnit" is open, and write to it.
C MDS_WRITEVEC_LOC writes either to a file of the form "fName.data" and
C "fName.meta" if bi=bj=0. Otherwise it writes to MDS tiled files of the
C form "fName.xxx.yyy.data" and "fName.xxx.yyy.meta".
C If irecord>0, a meta-file is created (skipped if irecord<0).
C The precision of the file is described by filePrec, set either
C to floatPrec32 or floatPrec64.
C |irecord|=iRec is the record number to be written and must be >=1.

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
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

C !FUNCTIONS:
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN

C !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) dataFName, metaFName, pfName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL fileIsOpen
      INTEGER iG,jG,iRec,dUnit,IL,pIL
      INTEGER dimList(3,3), nDims, map2gl(2)
      INTEGER length_of_rec
      INTEGER buffSize
      _RL dummyRL(1)
      CHARACTER*8 blank8c
CEOP

      DATA dummyRL(1) / 0. _d 0 /
      DATA blank8c / '        ' /
      DATA map2gl  / 0, 1 /

C We write a non-tiled array (bi=bj=0) only 1 time (if ProcId=0):
      IF ( myProcId.EQ.0 .OR. bi.NE.0 .OR. bj.NE.0 ) THEN

C Only DO I/O IF I am the master thread
        _BEGIN_MASTER( myThid )

C Assume nothing
        fileIsOpen = .FALSE.
        IL  = ILNBLNK( fName )
        iRec = ABS(irecord)

C Record number must be >= 1
        IF ( iRec.LT.1 ) THEN
          WRITE(msgBuf,'(A,I9)')
     &      ' MDS_WRITEVEC_LOC: argument irecord = ',irecord
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &     ' MDS_WRITEVEC_LOC: invalid value for irecord'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITEVEC_LOC'
        ENDIF

C Check buffer size
        buffSize = sNx*sNy*size3dBuf*nSx*nSy
        IF ( nSize.GT.buffSize ) THEN
          WRITE(msgBuf,'(3A)')
     &     ' MDS_WRITEVEC_LOC: writing to file "', fName(1:IL), '":'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I9)')
     &      ' MDS_WRITEVEC_LOC: dim of array to write=', nSize
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I9)')
     &      ' MDS_WRITEVEC_LOC: exceeds buffer size=', buffSize
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &    ' increase "size3dBuf" in "MDSIO_BUFF_3D.h" and recompile'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITEVEC_LOC'
        ENDIF

C Assign special directory
        IF ( mdsioLocalDir .NE. ' ' ) THEN
         pIL = ILNBLNK( mdsioLocalDir )
         WRITE(pFname,'(2A)') mdsioLocalDir(1:pIL), fName(1:IL)
         pIL = IL + pIL
        ELSE
         WRITE(pFname,'(A)') fName(1:IL)
         pIL = IL
        ENDIF

        IF ( ioUnit.GT.0 ) THEN
C- Assume file Unit is already open with correct Rec-Length & Precision
          fileIsOpen = .TRUE.
          dUnit = ioUnit
        ELSE
C- Need to open file IO unit with File-name, Rec-Length & Precision

C     Assign a free unit number as the I/O channel for this routine
          CALL MDSFINDUNIT( dUnit, myThid )

C--   Set the file Name:
          IF ( bi.EQ.0 .AND. bj.EQ.0 ) THEN
C-    we are writing a non-tiled array (bi=bj=0):
            WRITE(dataFname,'(2A)') fName(1:IL),'.data'
          ELSE
C-    we are writing a tiled array (bi>0,bj>0):
            iG=bi+(myXGlobalLo-1)/sNx
            jG=bj+(myYGlobalLo-1)/sNy
            WRITE(dataFname,'(2A,I3.3,A,I3.3,A)')
     &             pfName(1:pIL),'.',iG,'.',jG,'.data'
          ENDIF

C--   Open the file:
          length_of_rec=MDS_RECLEN( filePrec, nSize, myThid )
          IF (iRec .EQ. 1) THEN
            OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &            access='direct', recl=length_of_rec )
            fileIsOpen=.TRUE.
          ELSE
            OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &            access='direct', recl=length_of_rec )
            fileIsOpen=.TRUE.
          ENDIF
          IF ( debugLevel.GE.debLevC ) THEN
            WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITEVEC_LOC: open file: ',dataFname(1:pIL+13)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT , 1)
          ENDIF
C- End if block: File Unit is already open / Need to open it
        ENDIF

        IF (fileIsOpen) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_WR_REC_RS( fldRS, shared3dBuf_r4, shared3dBuf_r8,
     I                          filePrec, dUnit, iRec, nSize, myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_WR_REC_RL( fldRL, shared3dBuf_r4, shared3dBuf_r8,
     I                          filePrec, dUnit, iRec, nSize, myThid )
          ELSE
            WRITE(msgBuf,'(A)')
     &          ' MDS_WRITEVEC_LOC: illegal value for arrType'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDS_WRITEVEC_LOC'
          ENDIF
        ELSE
          WRITE(msgBuf,'(A)')
     &      ' MDS_WRITEVEC_LOC: should never reach this point'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITEVEC_LOC'
        ENDIF

C If we were writing to a tiled MDS file then we close it here
        IF ( fileIsOpen .AND. ioUnit.EQ.0 ) THEN
          CLOSE( dUnit )
          fileIsOpen = .FALSE.
        ENDIF
        IF ( ioUnit.EQ.-1 ) ioUnit = dUnit

        IF ( irecord.GT.0 ) THEN
C Create meta-file for each tile IF we are tiling
          IF ( bi.EQ.0 .AND. bj.EQ.0 ) THEN
C--   we are writing a non-tiled array (bi=bj=0):
            WRITE(metaFname,'(2A)') fName(1:IL),'.meta'
            dimList(1,1)=1
            dimList(2,1)=1
            dimList(3,1)=1
            dimList(1,2)=1
            dimList(2,2)=1
            dimList(3,2)=1
          ELSE
C--   we are writing a tiled array (bi>0,bj>0):
            iG=bi+(myXGlobalLo-1)/sNx
            jG=bj+(myYGlobalLo-1)/sNy
            WRITE(metaFname,'(2A,I3.3,A,I3.3,A)')
     &             pfName(1:pIL),'.',iG,'.',jG,'.meta'
            dimList(1,1)=nSx*nPx
            dimList(2,1)=iG
            dimList(3,1)=iG
            dimList(1,2)=nSy*nPy
            dimList(2,2)=jG
            dimList(3,2)=jG
          ENDIF
          dimList(1,3)=nSize
          dimList(2,3)=1
          dimList(3,3)=nSize
          nDims=3
          IF ( nSize.EQ.1 ) nDims=2
          CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, blank8c,
     I              0, dummyRL, oneRL, irecord, myIter, myThid )
        ENDIF

        _END_MASTER( myThid )
      ENDIF

      RETURN
      END
