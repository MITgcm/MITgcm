C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_writevec_loc.F,v 1.1 2005/10/30 21:12:20 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      SUBROUTINE MDSWRITEVEC_LOC_RS(
     I   fName,
     I   filePrec,
     I   nArr,
     I   arr,
     I   bi, bj,
     I   irecord,
     I   myIter,
     I   myThid )
C
C Arguments:
C
C fName		string	base name for file to written
C filePrec	integer	number of bits per word in file (32 or 64)
C nArr          number of elements from input array "arr" to be written
C arr		RS/RL	array to WRITE, arr(nArr)
C bi,bj         tile indices (if tiled array) or 0,0 if not a tiled array
C irecord	integer	record number to WRITE
C myIter	integer	time step number
C myThid	integer thread identifier
C
C MDSWRITEVEC_LOC creates either a file of the form "fName.data" and
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
#include "EESUPPORT.h"
#include "PARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      INTEGER nArr
      _RS     arr(*)
      INTEGER bi,bj
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
C Local variables
      CHARACTER*(MAX_LEN_FNAM) dataFName, metaFName, pfName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL fileIsOpen
      INTEGER iG,jG,irec,k,dUnit,IL,pIL
      INTEGER dimList(3,3),ndims
      INTEGER length_of_rec
      INTEGER loc_size
      PARAMETER( loc_size = Nx+Ny+Nr )
      real*4 r4seg(loc_size)
      real*8 r8seg(loc_size)

C Only DO I/O IF I am the master thread
      _BEGIN_MASTER( myThid )

#ifdef ALLOW_USE_MPI
      IF ( (mpiMyId.EQ.0 .AND. bi.EQ.0 .AND. bj.EQ.0)
     &                    .OR. bi.NE.0 .OR.  bj.NE.0 ) THEN
C--   we are writing a non-tiled array (bi=bj=0), only 1 time.
#endif

C Record number must be >= 1
        IF (irecord .LT. 1) THEN
          WRITE(msgBuf,'(A,I9.8)')
     &      ' MDSWRITEVEC_LOC: argument irecord = ',irecord
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &     ' MDSWRITEVEC_LOC: invalid value for irecord'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDSWRITEVEC_LOC'
        ENDIF

C Assume nothing
        fileIsOpen=.FALSE.
        IL  = ILNBLNK( fName )
        irec = irecord

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
     &      ' MDSWRITEVEC_LOC: open file: ',dataFname(1:pIL+13)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , 1)
        ENDIF

        IF (fileIsOpen) THEN
          IF (filePrec .EQ. precFloat32) THEN
            DO k=1,nArr
              r4seg(k) = arr(k)
            ENDDO
#ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR4( nArr, r4seg )
#endif
            WRITE(dUnit,rec=irec) (r4seg(k),k=1,nArr)
          ELSEIF (filePrec .EQ. precFloat64) THEN
            DO k=1,nArr
              r8seg(k) = arr(k)
            ENDDO
#ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR8( nArr, r8seg )
#endif
            WRITE(dUnit,rec=irec) (r8seg(k),k=1,nArr)
          ELSE
            WRITE(msgBuf,'(A)')
     &        ' MDSWRITEVEC_LOC: illegal value for filePrec'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDSWRITEVEC_LOC'
          ENDIF
        ELSE
          WRITE(msgBuf,'(A)')
     &      ' MDSWRITEVEC_LOC: should never reach this point'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDSWRITEVEC_LOC'
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
        ndims=3
        IF (nArr .EQ. 1) ndims=2
        CALL MDSWRITEMETA( metaFName, dataFName,
     &       filePrec, ndims, dimList, irec, myIter, myThid )

#ifdef ALLOW_USE_MPI
      ENDIF
#endif

      _END_MASTER( myThid )

      RETURN
      END
