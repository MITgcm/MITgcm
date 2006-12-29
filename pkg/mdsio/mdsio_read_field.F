C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_read_field.F,v 1.1 2006/12/29 05:25:15 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

CBOP
C !ROUTINE: MDS_READ_FIELD
C !INTERFACE:
      SUBROUTINE MDS_READ_FIELD(
     I   fName,
     I   filePrec,
     I   useCurrentDir,
     I   arrType,
     I   zSize, nNz,
     O   arr,
     I   irecord,
     I   myThid )

C !DESCRIPTION:
C Arguments:
C
C fName     (string)  :: base name for file to read
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C useCurrentDir(logic):: always read from the current directory (even if
C                        "mdsioLocalDir" is set)
C arrType   (char(2)) :: declaration of "arr": either "RS" or "RL"
C zSize     (integer) :: size of third dimension: normally either 1 or Nr
C nNz       (integer) :: number of vertical levels to read
C arr       ( RS/RL ) :: array to read into, arr(:,:,zSize,:,:)
C irecord   (integer) :: record number to read
C myIter    (integer) :: time step number
C myThid    (integer) :: thread identifier
C
C MDS_READ_FIELD first checks to see IF the file "fName" exists, then
C IF the file "fName.data" exists and finally the tiled files of the
C form "fName.xxx.yyy.data" exist. Currently, the meta-files are not
C read because it is difficult to parse files in fortran.
C The precision of the file is decsribed by filePrec, set either
C to floatPrec32 or floatPrec64. The precision or declaration of
C the array argument must be consistently described by the char*(2)
C string arrType, either "RS" or "RL". nNz allows for both 2-D and
C 3-D arrays to be handled. nNz=1 implies a 2-D model field and
C nNz=Nr implies a 3-D model field. irecord is the record number
C to be read and must be >= 1. The file data is stored in
C arr *but* the overlaps are *not* updated. ie. An exchange must
C be called. This is because the routine is sometimes called from
C within a MASTER_THID region.
C
C Created: 03/16/99 adcroft@mit.edu
CEOP

C !USES:
      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */
#include "MDSIO_SCPU.h"

C !INPUT PARAMETERS:
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL useCurrentDir
      CHARACTER*(2) arrType
      INTEGER zSize
      INTEGER nNz
      INTEGER irecord
      INTEGER myThid
C !OUTPUT PARAMETERS:
      Real arr(*)

C !FUNCTIONS
      INTEGER  ILNBLNK
      INTEGER  MDS_RECLEN
      LOGICAL  MASTER_CPU_IO
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN
      EXTERNAL MASTER_CPU_IO

C !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) dataFName,pfName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL exst
      LOGICAL globalFile, fileIsOpen
      LOGICAL iAmDoingIO
      INTEGER iG,jG,irec,bi,bj,i,j,k,dUnit,IL,pIL
      INTEGER x_size,y_size,length_of_rec
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
      INTEGER iG_IO,jG_IO,npe
      PARAMETER ( x_size = exch2_domain_nxt * sNx )
      PARAMETER ( y_size = exch2_domain_nyt * sNy )
#else
      PARAMETER ( x_size = Nx )
      PARAMETER ( y_size = Ny )
#endif
      Real*4 xy_buffer_r4(x_size,y_size)
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      Real*8 xy_buffer_r8(x_size,y_size)
      Real*8 globalBuf(Nx,Ny)
#ifdef ALLOW_EXCH2
c     INTEGER tGy,tGx,tNy,tNx,tn
      INTEGER tGy,tGx,    tNx,tn
#endif /* ALLOW_EXCH2 */
      INTEGER tNy


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

C Only do I/O if I am the master thread (and mpi process 0 IF useSingleCpuIO):
      iAmDoingIO = MASTER_CPU_IO(myThid)

C Only do I/O if I am the master thread
      IF ( iAmDoingIO ) THEN

C Record number must be >= 1
        IF (irecord .LT. 1) THEN
         WRITE(msgBuf,'(A,I9.8)')
     &     ' MDS_READ_FIELD: argument irecord = ',irecord
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
         WRITE(msgBuf,'(A)')
     &     ' MDS_READ_FIELD: Invalid value for irecord'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
        ENDIF

C Assign special directory
        IF ( useCurrentDir .OR. pIL.EQ.0 ) THEN
         pfName= fName
        ELSE
         WRITE(pfName,'(2a)') mdsioLocalDir(1:pIL), fName(1:IL)
        ENDIF
        pIL=ILNBLNK( pfName )

C Assign a free unit number as the I/O channel for this routine
        CALL MDSFINDUNIT( dUnit, myThid )

C Check first for global file with simple name (ie. fName)
        dataFName = fName
        INQUIRE( file=dataFName, exist=exst )
        IF (exst) THEN
          IF ( debugLevel .GE. debLevA ) THEN
            WRITE(msgBuf,'(A,A)')
     &      ' MDS_READ_FIELD: opening global file: ',dataFName(1:IL)
#ifndef ALLOW_ECCO
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT , myThid)
#endif
          ENDIF
          globalFile = .TRUE.
        ENDIF

C If negative check for global file with MDS name (ie. fName.data)
        IF (.NOT. globalFile) THEN
          WRITE(dataFName,'(2a)') fName(1:IL),'.data'
          INQUIRE( file=dataFName, exist=exst )
          IF (exst) THEN
           IF ( debugLevel .GE. debLevA ) THEN
            WRITE(msgBuf,'(A,A)')
     &      ' MDS_READ_FIELD: opening global file: ',dataFName(1:IL+5)
#ifndef ALLOW_ECCO
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT , myThid)
#endif
           ENDIF
           globalFile = .TRUE.
          ENDIF
        ENDIF

C- endif iAmDoingIO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( useSingleCPUIO ) THEN

C master thread of process 0, only, opens a global file
       IF ( iAmDoingIO ) THEN
C If global file is visible to process 0, then open it here.
C Otherwise stop program.
         IF ( globalFile) THEN
          length_of_rec=MDS_RECLEN( filePrec, x_size*y_size, myThid )
          OPEN( dUnit, file=dataFName, status='old',
     &         access='direct', recl=length_of_rec )
         ELSE
          WRITE(msgBuf,'(2A)')
     &      ' MDS_READ_FIELD: filename: ', dataFName(1:IL+5)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &      ' MDS_READ_FIELD: File does not exist'
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
         ENDIF
C- endif iAmDoingIO
       ENDIF

       DO k=1,nNz

C master thread of process 0, only, read from file
        IF ( iAmDoingIO ) THEN
          irec = k+nNz*(irecord-1)
          IF (filePrec .EQ. precFloat32) THEN
           READ(dUnit,rec=irec) xy_buffer_r4
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR4( x_size*y_size, xy_buffer_r4 )
#endif
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
           bj=1
           DO npe=1,nPx*nPy
            DO bi=1,nSx
             DO J=1,sNy
              DO I=1,sNx
#ifdef ALLOW_USE_MPI
               iG=mpi_myXGlobalLo(npe)-1+(bi-1)*sNx+i
               jG=mpi_myYGlobalLo(npe)-1+(bj-1)*sNy+j
#else
               iG= myXGlobalLo-1+(bi-1)*sNx+i
               jG= myYGlobalLo-1+(bj-1)*sNy+j
#endif /* ALLOW_USE_MPI */
               iG_IO=exch2_txGlobalo(W2_mpi_myTileList(npe,bi))+i-1
               jG_IO=exch2_tyGlobalo(W2_mpi_myTileList(npe,bi))+j-1
               globalBuf(iG,jG) = xy_buffer_r4(iG_IO,jG_IO)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO J=1,Ny
            DO I=1,Nx
             globalBuf(I,J) = xy_buffer_r4(I,J)
            ENDDO
           ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          ELSEIF (filePrec .EQ. precFloat64) THEN
           READ(dUnit,rec=irec) xy_buffer_r8
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR8( x_size*y_size, xy_buffer_r8 )
#endif
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
           bj=1
           DO npe=1,nPx*nPy
            DO bi=1,nSx
             DO J=1,sNy
              DO I=1,sNx
#ifdef ALLOW_USE_MPI
               iG=mpi_myXGlobalLo(npe)-1+(bi-1)*sNx+i
               jG=mpi_myYGlobalLo(npe)-1+(bj-1)*sNy+j
#else
               iG= myXGlobalLo-1+(bi-1)*sNx+i
               jG= myYGlobalLo-1+(bj-1)*sNy+j
#endif /* ALLOW_USE_MPI */
               iG_IO=exch2_txGlobalo(W2_mpi_myTileList(npe,bi))+i-1
               jG_IO=exch2_tyGlobalo(W2_mpi_myTileList(npe,bi))+j-1
               globalBuf(iG,jG) = xy_buffer_r8(iG_IO,jG_IO)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO J=1,Ny
            DO I=1,Nx
             globalBuf(I,J) = xy_buffer_r8(I,J)
            ENDDO
           ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          ELSE
           WRITE(msgBuf,'(A)')
     &            ' MDS_READ_FIELD: illegal value for filePrec'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
          ENDIF
C- endif iAmDoingIO
        ENDIF
        CALL SCATTER_2D(globalBuf,sharedLocalBuf,myThid)
        IF (arrType .EQ. 'RS') THEN
          CALL MDS_PASStoRS( sharedLocalBuf,arr,k,zSize,.TRUE.,myThid )
        ELSEIF (arrType .EQ. 'RL') THEN
          CALL MDS_PASStoRL( sharedLocalBuf,arr,k,zSize,.TRUE.,myThid )
        ELSE
          WRITE(msgBuf,'(A)')
     &          ' MDS_READ_FIELD: illegal value for arrType'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
        ENDIF

       ENDDO
c      ENDDO k=1,nNz

       IF ( iAmDoingIO ) THEN
         CLOSE( dUnit )
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  else .NOT.useSingleCpuIO
      ELSE

C Only do I/O if I am the master thread
       IF ( iAmDoingIO ) THEN

C If we are reading from a global file then we open it here
        IF (globalFile) THEN
         length_of_rec=MDS_RECLEN( filePrec, sNx, myThid )
         OPEN( dUnit, file=dataFName, status='old',
     &        access='direct', recl=length_of_rec )
         fileIsOpen=.TRUE.
        ENDIF

C Loop over all tiles
        DO bj=1,nSy
         DO bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
          IF (.NOT. globalFile) THEN
           iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
           jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
           WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &            pfName(1:pIL),'.',iG,'.',jG,'.data'
           INQUIRE( file=dataFName, exist=exst )
C Of course, we only open the file if the tile is "active"
C (This is a place-holder for the active/passive mechanism
           IF (exst) THEN
            IF ( debugLevel .GE. debLevA ) THEN
             WRITE(msgBuf,'(A,A)')
     &       ' MDS_READ_FIELD: opening file: ',dataFName(1:pIL+13)
             CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
            ENDIF
            length_of_rec=MDS_RECLEN( filePrec, sNx, myThid )
            OPEN( dUnit, file=dataFName, status='old',
     &            access='direct', recl=length_of_rec )
            fileIsOpen=.TRUE.
           ELSE
            fileIsOpen=.FALSE.
            WRITE(msgBuf,'(4A)') ' MDS_READ_FIELD: filename: ',
     &             fName(1:IL),' , ', dataFName(1:pIL+13)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT , myThid)
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A)')
     &      ' MDS_READ_FIELD: Files DO not exist'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT , myThid)
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
           ENDIF
          ENDIF

          IF (fileIsOpen) THEN
           tNy = sNy
#ifdef ALLOW_EXCH2
           tn = W2_myTileList(bi)
           tGy = exch2_tyGlobalo(tn)
           tGx = exch2_txGlobalo(tn)
           tNy = exch2_tNy(tn)
           tNx = exch2_tNx(tn)
#endif /* ALLOW_EXCH2 */
           DO k=1,nNz
            DO j=1,tNy
             IF (globalFile) THEN
#ifdef ALLOW_EXCH2
              irec = 1 + (tGx-1)/tNx
     &                 + ( j-1 + tGy-1 )*exch2_domain_nxt
     &                 + ( k-1 + (irecord-1)*nNz
     &                   )*tNy*exch2_domain_nyt*exch2_domain_nxt
#else /* ALLOW_EXCH2 */
              iG = myXGlobalLo-1 + (bi-1)*sNx
              jG = myYGlobalLo-1 + (bj-1)*sNy
              irec= 1 + INT(iG/sNx) + nSx*nPx*(jG+j-1)
     &                + nSx*nPx*Ny*(k-1)
     &                + nSx*nPx*Ny*nNz*(irecord-1)
#endif /* ALLOW_EXCH2 */
             ELSE
              iG = 0
              jG = 0
              irec=j + sNy*(k-1) + sNy*nNz*(irecord-1)
             ENDIF
             IF (filePrec .EQ. precFloat32) THEN
              READ(dUnit,rec=irec) r4seg
#ifdef _BYTESWAPIO
              CALL MDS_BYTESWAPR4( sNx, r4seg )
#endif
              IF (arrType .EQ. 'RS') THEN
               CALL MDS_SEG4toRS( j,bi,bj,k,zSize, r4seg, .TRUE., arr )
              ELSEIF (arrType .EQ. 'RL') THEN
               CALL MDS_SEG4toRL( j,bi,bj,k,zSize, r4seg, .TRUE., arr )
              ELSE
               WRITE(msgBuf,'(A)')
     &         ' MDS_READ_FIELD: illegal value for arrType'
               CALL PRINT_ERROR( msgBuf, myThid )
               STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
              ENDIF
             ELSEIF (filePrec .EQ. precFloat64) THEN
              READ(dUnit,rec=irec) r8seg
#ifdef _BYTESWAPIO
              CALL MDS_BYTESWAPR8( sNx, r8seg )
#endif
              IF (arrType .EQ. 'RS') THEN
               CALL MDS_SEG8toRS( j,bi,bj,k,zSize, r8seg, .TRUE., arr )
              ELSEIF (arrType .EQ. 'RL') THEN
               CALL MDS_SEG8toRL( j,bi,bj,k,zSize, r8seg, .TRUE., arr )
              ELSE
               WRITE(msgBuf,'(A)')
     &         ' MDS_READ_FIELD: illegal value for arrType'
               CALL PRINT_ERROR( msgBuf, myThid )
               STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
              ENDIF
             ELSE
              WRITE(msgBuf,'(A)')
     &        ' MDS_READ_FIELD: illegal value for filePrec'
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
             ENDIF
C End of j loop
            ENDDO
C End of k loop
           ENDDO
C end if fileIsOpen
          ENDIF
          IF (fileIsOpen .AND. (.NOT. globalFile)) THEN
            CLOSE( dUnit )
            fileIsOpen = .FALSE.
          ENDIF
C End of bi,bj loops
         ENDDO
        ENDDO

C If global file was opened then close it
        IF (fileIsOpen .AND. globalFile) THEN
         CLOSE( dUnit )
         fileIsOpen = .FALSE.
        ENDIF

C- endif iAmDoingIO
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     if useSingleCpuIO / else / end
      ENDIF

      RETURN
      END
