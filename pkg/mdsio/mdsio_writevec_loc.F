C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_writevec_loc.F,v 1.2 2008/12/30 02:13:01 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      SUBROUTINE MDS_WRITEVEC_LOC(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nArr,
     I   arr,
     I   bi, bj,
     I   irecord,
     I   myIter,
     I   myThid )

C Arguments:
C
C fName  string base name for file to written
C filePrec integer :: number of bits per word in file (32 or 64)
C nArr     integer :: number of elements from input array "arr" to be written
C arrType  char(2) :: declaration type of "arr": either "RS" or "RL"
C arr       RS/RL  :: array to WRITE, arr(nArr)
C bi,bj    integer :: tile indices (if tiled array) or 0,0 if not a tiled array
C irecord  integer :: record number to WRITE
C myIter   integer :: time step number
C myThid   integer :: my Thread Id number
C
C MDS_WRITEVEC_LOC creates either a file of the form "fName.data" and
C "fName.meta" IF bi=bj=0. Otherwise it creates MDS tiled files of the
C form "fName.xxx.yyy.data" and "fName.xxx.yyy.meta".
C A meta-file is always created.
C The precision of the file is decsribed by filePrec, set either
C to floatPrec32 or floatPrec64.
C irecord is the record number to be written and must be >= 1.

      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER nArr
      _RL     arr(*)
      INTEGER bi,bj
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN
C Local variables
      CHARACTER*(MAX_LEN_FNAM) dataFName, metaFName, pfName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL fileIsOpen
      INTEGER iG,jG,iRec,k,dUnit,IL,pIL
      INTEGER dimList(3,3), nDims, map2gl(2)
      INTEGER length_of_rec
      INTEGER loc_size
      PARAMETER( loc_size = Nx+Ny+Nr )
      real*4 r4seg(loc_size)
      real*8 r8seg(loc_size)

      DATA map2gl / 0, 1 /

C Only DO I/O IF I am the master thread
      _BEGIN_MASTER( myThid )
C--   we write a non-tiled array (bi=bj=0) only 1 time (if ProcId=0):
      IF ( (myProcId.EQ.0 .AND. bi.EQ.0 .AND. bj.EQ.0)
     &                     .OR. bi.NE.0 .OR.  bj.NE.0 ) THEN

C Record number must be >= 1
        IF (irecord .LT. 1) THEN
          WRITE(msgBuf,'(A,I9)')
     &      ' MDS_WRITEVEC_LOC: argument irecord = ',irecord
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &     ' MDS_WRITEVEC_LOC: invalid value for irecord'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITEVEC_LOC'
        ENDIF

C Assume nothing
        fileIsOpen = .FALSE.
        IL  = ILNBLNK( fName )
        iRec = irecord

C Check buffer size
        IF ( nArr.GT.loc_size ) THEN
          WRITE(msgBuf,'(3A)')
     &     ' MDS_WRITEVEC_LOC: writing to file "', fName(1:IL), '":'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I9)')
     &      ' MDS_WRITEVEC_LOC: dim of arr to write=', nArr
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I9)')
     &      ' MDS_WRITEVEC_LOC: exceeds buffer size=', loc_size
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

C Assign a free unit number as the I/O channel for this routine
        CALL MDSFINDUNIT( dUnit, myThid )

C--     Set the file Name:
        IF ( bi.EQ.0 .AND. bj.EQ.0 ) THEN
C-      we are writing a non-tiled array (bi=bj=0):
          WRITE(dataFname,'(2A)') fName(1:IL),'.data'
        ELSE
C-      we are writing a tiled array (bi>0,bj>0):
          iG=bi+(myXGlobalLo-1)/sNx
          jG=bj+(myYGlobalLo-1)/sNy
          WRITE(dataFname,'(2A,I3.3,A,I3.3,A)')
     &             pfName(1:pIL),'.',iG,'.',jG,'.data'
        ENDIF

        length_of_rec=MDS_RECLEN( filePrec, nArr, myThid )
        IF (irecord .EQ. 1) THEN
          OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &          access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
        ELSE
          OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &          access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
        ENDIF
        IF ( debugLevel.GE.debLevB ) THEN
          WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITEVEC_LOC: open file: ',dataFname(1:pIL+13)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , 1)
        ENDIF

        IF (fileIsOpen) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_WR_REC_RS( arr, r4seg, r8seg,
     I                          filePrec, dUnit, iRec, nArr, myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_WR_REC_RL( arr, r4seg, r8seg,
     I                          filePrec, dUnit, iRec, nArr, myThid )
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
        IF ( fileIsOpen ) THEN
          CLOSE( dUnit )
          fileIsOpen = .FALSE.
        ENDIF

C Create meta-file for each tile IF we are tiling
        IF ( bi.EQ.0 .AND. bj.EQ.0 ) THEN
C--     we are writing a non-tiled array (bi=bj=0):
          WRITE(metaFname,'(2A)') fName(1:IL),'.meta'
          dimList(1,1)=1
          dimList(2,1)=1
          dimList(3,1)=1
          dimList(1,2)=1
          dimList(2,2)=1
          dimList(3,2)=1
        ELSE
C--     we are writing a tiled array (bi>0,bj>0):
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
        dimList(1,3)=nArr
        dimList(2,3)=1
        dimList(3,3)=nArr
        nDims=3
        IF (nArr .EQ. 1) nDims=2
        CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, ' ',
     I              0, UNSET_RL, iRec, myIter, myThid )
c    I              metaFName, dataFName, the_run_name, titleLine,
c    I              filePrec, nDims, dimList, map2gl, nFlds, fldList,
c    I              nTimRec, timList, irecord, myIter, myThid )

      ENDIF
      _END_MASTER( myThid )

      RETURN
      END
