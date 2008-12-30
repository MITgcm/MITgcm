C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_readvector.F,v 1.13 2008/12/30 02:07:01 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      SUBROUTINE MDSREADVECTOR(
     I   fName,
     I   filePrec,
     I   arrType,
     I   narr,
     O   arr,
     I   bi,
     I   bj,
     I   irecord,
     I   myThid )

C Arguments:
C
C fName    string  :: base name for file to read
C filePrec integer :: number of bits per word in file (32 or 64)
C arrType  char(2) :: declaration of "arr": either "RS" or "RL"
C narr     integer :: size of third dimension: normally either 1 or Nr
C arr       RS/RL  :: array to read into, arr(narr)
c bi       integer :: x tile index
c bj       integer :: y tile index
C irecord  integer :: record number to read
C myThid   integer :: thread identifier
C
C Created: 03/26/99 eckert@mit.edu
C Modified: 03/29/99 adcroft@mit.edu + eckert@mit.edu
C           Fixed to work work with _RS and _RL declarations
C Modified: 07/27/99 eckert@mit.edu
C           Customized  for state estimation (--> active_file_control.F)

      IMPLICIT NONE
C Global variables / COMMON blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER narr
      _RL arr(narr)
      INTEGER bi,bj
      INTEGER irecord
      INTEGER myThid

#if defined(ALLOW_AUTODIFF) || defined(ALLOW_FLT)

C Functions
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
C Local variables
      CHARACTER*(MAX_LEN_FNAM) dataFName,pfName
      INTEGER iG,jG,irec,dUnit,IL,pIL
      LOGICAL exst
      LOGICAL globalFile,fileIsOpen
      INTEGER length_of_rec
      CHARACTER*(MAX_LEN_MBUF) msgBuf

cph(
cph Deal with useSingleCpuIO
cph Not extended here for EXCH2
      INTEGER k,l
      INTEGER vec_size
      Real*4 xy_buffer_r4(narr*nPx*nPy)
      Real*8 xy_buffer_r8(narr*nPx*nPy)
      Real*8 global   (narr*nPx*nPy)
      _RL    local(narr)
cph)
C     ------------------------------------------------------------------

      vec_size = narr*nPx*nPy

C Only DO I/O IF I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      IF (irecord .LT. 1) THEN
       WRITE(msgBuf,'(A,I9.8)')
     &   ' MDSREADVECTOR: argument irecord = ',irecord
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   ' MDSREADVECTOR: invalid value for irecord'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R MDSREADVECTOR'
      ENDIF

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

C Assign special directory
      IF ( mdsioLocalDir .NE. ' ' ) THEN
       WRITE(pfName,'(2A)')
     &  mdsioLocalDir(1:pIL), fName(1:IL)
      ELSE
       pfName= fName
      ENDIF
      pIL=ILNBLNK( pfName )

C Assign a free unit number as the I/O channel for this routine
      CALL MDSFINDUNIT( dUnit, myThid )

      IF ( .not. useSingleCPUIO ) THEN

C Check first for global file with simple name (ie. fName)
      dataFName = fName
      INQUIRE( file=dataFName, exist=exst )
      IF (exst) THEN
       IF ( debugLevel .GE. debLevB ) THEN
        WRITE(msgBuf,'(A,A)')
     &   ' MDSREADVECTOR: opening global file: ',dataFName(1:IL)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
       ENDIF
       globalFile = .TRUE.
      ENDIF

C If negative check for global file with MDS name (ie. fName.data)
      IF (.NOT. globalFile) THEN
       WRITE(dataFName,'(2A)') fName(1:IL),'.data'
       INQUIRE( file=dataFName, exist=exst )
       IF (exst) THEN
        IF ( debugLevel .GE. debLevB ) THEN
         WRITE(msgBuf,'(A,A)')
     &     ' MDSREADVECTOR: opening global file: ',dataFName(1:IL+5)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT , myThid )
        ENDIF
        globalFile = .TRUE.
       ENDIF
      ENDIF

C If we are reading from a global file then we open it here
      IF (globalFile) THEN
       length_of_rec=MDS_RECLEN( filePrec, narr, myThid )
       OPEN( dUnit, file=dataFName, status='old',
     &      access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      ENDIF

C Loop over all tiles
c     DO bj=1,nSy
c      DO bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        IF (.NOT. globalFile) THEN
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         INQUIRE( file=dataFName, exist=exst )
C Of course, we only open the file IF the tile is "active"
C (This is a place-holder for the active/passive mechanism)
         IF (exst) THEN
          IF ( debugLevel .GE. debLevB ) THEN
           WRITE(msgBuf,'(A,A)')
     &      ' MDSREADVECTOR: opening file: ',dataFName(1:pIL+13)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT , myThid )
          ENDIF
          length_of_rec=MDS_RECLEN( filePrec, narr, myThid )
          OPEN( dUnit, file=dataFName, status='old',
     &        access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         ELSE
          fileIsOpen=.FALSE.
          WRITE(msgBuf,'(4A)')
     &      ' MDSREADVECTOR: opening file: ',fName(1:IL),
     &                                 ' , ',dataFName(1:pIL+13)
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &      ' MDSREADVECTOR: un-active tiles not implemented yet'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDSREADVECTOR'
         ENDIF
        ENDIF
        IF (fileIsOpen) THEN
          IF (globalFile) THEN
            iG   = myXGlobalLo-1+(bi-1)*sNx
            jG   = myYGlobalLo-1+(bj-1)*sNy
            irec = 1 + int(iG/sNx) + (jG/sNy)*nSx*nPx +
     &             (irecord-1)*nSx*nPx*nSy*nPy
          ELSE
            iG   = 0
            jG   = 0
            irec = irecord
          ENDIF
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_RD_REC_RS( arr, xy_buffer_r4, xy_buffer_r8,
     I                          filePrec, dUnit, irec, narr, myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_RD_REC_RL( arr, xy_buffer_r4, xy_buffer_r8,
     I                          filePrec, dUnit, irec, narr, myThid )
          ELSE
            WRITE(msgBuf,'(A)')
     &          ' MDSREADVECTOR: illegal value for arrType'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDSREADVECTOR'
          ENDIF
          IF (.NOT. globalFile) THEN
            CLOSE( dUnit )
            fileIsOpen = .FALSE.
          ENDIF
        ENDIF
C End of bi,bj loops
c      ENDDO
c     ENDDO

C If global file was opened then close it
      IF (fileIsOpen .AND. globalFile) THEN
        CLOSE( dUnit )
        fileIsOpen = .FALSE.
      ENDIF

      _END_MASTER( myThid )

      ENDIF
C     end-if ( .not. useSingleCPUIO )


C     ------------------------------------------------------------------


      IF ( useSingleCPUIO ) THEN

C master thread of process 0, only, opens a global file
       _BEGIN_MASTER( myThid )
#ifdef ALLOW_USE_MPI
        IF( mpiMyId .EQ. 0 ) THEN
#else
        IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */

C Check first for global file with simple name (ie. fName)
         dataFName = fName
         INQUIRE( file=dataFName, exist=exst )
         IF (exst) globalFile = .TRUE.

C If negative check for global file with MDS name (ie. fName.data)
         IF (.NOT. globalFile) THEN
          WRITE(dataFName,'(2a)') fName(1:IL),'.data'
          INQUIRE( file=dataFName, exist=exst )
          IF (exst) globalFile = .TRUE.
         ENDIF

C If global file is visible to process 0, then open it here.
C Otherwise stop program.
         IF ( globalFile) THEN
          length_of_rec=MDS_RECLEN( filePrec, vec_size, myThid )
          OPEN( dUnit, file=dataFName, status='old',
     &         access='direct', recl=length_of_rec )
         ELSE
          WRITE(msgBuf,'(2A)')
     &      ' MDSREADVECTOR: filename: ',dataFName(1:IL)
C-jmc: why double print (stdout + stderr) ?
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid )
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &      ' MDSREADVECTOR: File does not exist'
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid )
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDSREADVECTOR'
         ENDIF

        ENDIF
       _END_MASTER( myThid )

       DO k=1,1

        _BEGIN_MASTER( myThid )
#ifdef ALLOW_USE_MPI
         IF( mpiMyId .EQ. 0 ) THEN
#else
         IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */
          irec = irecord
          IF (filePrec .EQ. precFloat32) THEN
           READ(dUnit,rec=irec) xy_buffer_r4
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR4( vec_size, xy_buffer_r4 )
#endif
cph#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
cph#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO L=1,narr*nPx*nPy
            global(L) = xy_buffer_r4(L)
           ENDDO
cph#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          ELSEIF (filePrec .EQ. precFloat64) THEN
           READ(dUnit,rec=irec) xy_buffer_r8
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR8( vec_size, xy_buffer_r8 )
#endif
cph#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
cph#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO L=1,narr*nPx*nPy
            global(L) = xy_buffer_r8(L)
           ENDDO
cph#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          ELSE
           WRITE(msgBuf,'(A)')
     &            ' MDSREADVECTOR: illegal value for filePrec'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R MDSREADVECTOR'
          ENDIF
         ENDIF
        _END_MASTER( myThid )
        CALL SCATTER_VECTOR( narr,global,local,myThid )
        IF ( arrType.EQ.'RS' ) THEN
           CALL MDS_BUFFERtoRS( local, arr, narr, .TRUE., myThid )
        ELSEIF ( arrType.EQ.'RL' ) THEN
           CALL MDS_BUFFERtoRL( local, arr, narr, .TRUE., myThid )
        ELSE
           WRITE(msgBuf,'(A)')
     &          ' MDSREADVECTOR: illegal value for arrType'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R MDSREADVECTOR'
        ENDIF

       ENDDO
C      end-do k=1,1

       _BEGIN_MASTER( myThid )
        CLOSE( dUnit )
       _END_MASTER( myThid )

      ENDIF
C     end-if ( useSingleCPUIO )

#endif /* defined(ALLOW_AUTODIFF) || defined(ALLOW_FLT) */

      RETURN
      END
