C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_write_field.F,v 1.4 2007/11/13 19:37:44 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

CBOP
C !ROUTINE: MDS_WRITE_FIELD
C !INTERFACE:
      SUBROUTINE MDS_WRITE_FIELD(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   useCurrentDir,
     I   arrType,
     I   kSize,kLo,kHi,
     I   arr,
     I   jrecord,
     I   myIter,
     I   myThid )

C !DESCRIPTION:
C Arguments:
C
C fName     (string)  :: base name for file to write
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C globalFile (logical):: selects between writing a global or tiled file
C useCurrentDir(logic):: always write to the current directory (even if
C                        "mdsioLocalDir" is set)
C arrType   (char(2)) :: declaration of "arr": either "RS" or "RL"
C kSize     (integer) :: size of third dimension: normally either 1 or Nr
C kLo       (integer) :: 1rst vertical level (of array "arr") to write
C kHi       (integer) :: last vertical level (of array "arr") to write
C arr       ( RS/RL ) :: array to write, arr(:,:,kSize,:,:)
C irecord   (integer) :: record number to write
C myIter    (integer) :: time step number
C myThid    (integer) :: thread identifier
C
C MDS_WRITE_FIELD creates either a file of the form "fName.data" and
C  "fName.meta" if the logical flag "globalFile" is set true. Otherwise
C  it creates MDS tiled files of the form "fName.xxx.yyy.data" and
C  "fName.xxx.yyy.meta". If jrecord > 0, a meta-file is created.
C Currently, the meta-files are not read because it is difficult
C  to parse files in fortran. We should read meta information before
C  adding records to an existing multi-record file.
C The precision of the file is decsribed by filePrec, set either
C  to floatPrec32 or floatPrec64. The precision or declaration of
C  the array argument must be consistently described by the char*(2)
C  string arrType, either "RS" or "RL".
C (kSize,kLo,kHi) allows for both 2-D and 3-D arrays to be handled, with
C  the option to only write a sub-set of consecutive vertical levels (from
C  kLo to kHi); (kSize,kLo,kHi)=(1,1,1) implies a 2-D model field and
C  (kSize,kLo,kHi)=(Nr,1,Nr) implies a 3-D model field.
C irecord=|jrecord| is the record number to be written and must be >= 1.
C NOTE: It is currently assumed that the highest record number in the file
C  was the last record written. Nor is there a consistency check between the
C  routine arguments and file, i.e., if you write record 2 after record 4
C  the meta information will record the number of records to be 2. This,
C  again, is because we have read the meta information. To be fixed.
C
C Created: 03/16/99 adcroft@mit.edu
C Changed: 01/06/02 menemenlis@jpl.nasa.gov
C          added useSingleCpuIO hack
C changed:  1/23/04 afe@ocean.mit.edu
C          added exch2 handling -- yes, the globalfile logic is nuts
CEOP

C !USES:
      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */
#include "MDSIO_SCPU.h"

C !INPUT PARAMETERS:
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalFile
      LOGICAL useCurrentDir
      CHARACTER*(2) arrType
      INTEGER kSize, kLo, kHi
cph(
cph      Real arr(*)
      _RL arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,kSize,nSx,nSy)
cph)
      INTEGER jrecord
      INTEGER myIter
      INTEGER myThid
C !OUTPUT PARAMETERS:

C !FUNCTIONS
      INTEGER  ILNBLNK
      INTEGER  MDS_RECLEN
      LOGICAL  MASTER_CPU_IO
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN
      EXTERNAL MASTER_CPU_IO

C !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) dataFName,metaFName,pfName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL fileIsOpen
      LOGICAL iAmDoingIO
      LOGICAL writeMetaF
      INTEGER irecord
      INTEGER iG,jG,bi,bj,i,j,k,nNz
      INTEGER irec,dUnit,IL,pIL
      INTEGER dimList(3,3), nDims, map2gl(2)
      INTEGER iGjLoc, jGjLoc
      INTEGER x_size,y_size,length_of_rec
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
      INTEGER iG_IO,jG_IO,npe, loc_xGlobalLo, loc_yGlobalLo
      PARAMETER ( x_size = exch2_domain_nxt * sNx )
      PARAMETER ( y_size = exch2_domain_nyt * sNy )
#else
      PARAMETER ( x_size = Nx )
      PARAMETER ( y_size = Ny )
#endif
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      Real*4 xy_buffer_r4(x_size,y_size)
      Real*8 xy_buffer_r8(x_size,y_size)
      Real*8 globalBuf(Nx,Ny)
#ifdef ALLOW_EXCH2
c     INTEGER tGy,tGx,tNy,tNx,tN
      INTEGER tGy,tGx,    tNx,tN
#endif /* ALLOW_EXCH2 */
      INTEGER tNy

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    default:
      iGjLoc = 0
      jGjLoc = 1

C Assume nothing
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )
      nNz = 1 + kHi - kLo
      irecord = ABS(jrecord)
      writeMetaF = jrecord.GT.0

C Only do I/O if I am the master thread (and mpi process 0 IF useSingleCpuIO):
      iAmDoingIO = MASTER_CPU_IO(myThid)

C Only do I/O if I am the master thread
      IF ( iAmDoingIO ) THEN

C Record number must be >= 1
        IF (irecord .LT. 1) THEN
         WRITE(msgBuf,'(A,I9.8)')
     &     ' MDS_WRITE_FIELD: argument irecord = ',irecord
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
         WRITE(msgBuf,'(A)')
     &     ' MDS_WRITE_FIELD: invalid value for irecord'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
        ENDIF
C check for valid sub-set of levels:
        IF ( kLo.LT.1 .OR. kHi.GT.kSize ) THEN
         WRITE(msgBuf,'(3(A,I6))')
     &     ' MDS_WRITE_FIELD: arguments kSize=', kSize,
     &     ' , kLo=', kLo, ' , kHi=', kHi
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
         WRITE(msgBuf,'(A)')
     &     ' MDS_WRITE_FIELD: invalid sub-set of levels'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
        ENDIF

C Assign special directory
        IF ( useCurrentDir .OR. pIL.EQ.0 ) THEN
         pfName = fName
        ELSE
         WRITE(pfName,'(2A)') mdsioLocalDir(1:pIL), fName(1:IL)
        ENDIF
        pIL=ILNBLNK( pfName )

C Assign a free unit number as the I/O channel for this routine
        CALL MDSFINDUNIT( dUnit, myThid )

C- endif iAmDoingIO
      ENDIF

C If option globalFile is desired but does not work or if
C globalFile is too slow, then try using single-CPU I/O.
      IF (useSingleCpuIO) THEN

C Master thread of process 0, only, opens a global file
       IF ( iAmDoingIO ) THEN
         WRITE(dataFName,'(2a)') fName(1:IL),'.data'
         length_of_rec=MDS_RECLEN(filePrec,x_size*y_size,myThid)
         IF (irecord .EQ. 1) THEN
          OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &        access='direct', recl=length_of_rec )
         ELSE
          OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &        access='direct', recl=length_of_rec )
         ENDIF
       ENDIF

C Gather array and WRITE it to file, one vertical level at a time
       DO k=kLo,kHi
C-      copy from arr(level=k) to 2-D "local":
        IF ( arrType.EQ.'RS' ) THEN
          CALL MDS_PASStoRS(sharedLocalBuf,arr,k,kSize,.FALSE.,myThid)
        ELSEIF ( arrType.EQ.'RL' ) THEN
          CALL MDS_PASStoRL(sharedLocalBuf,arr,k,kSize,.FALSE.,myThid)
        ELSE
          WRITE(msgBuf,'(A)')
     &         ' MDS_WRITE_FIELD: illegal value for arrType'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
        ENDIF
        CALL GATHER_2D( globalBuf, sharedLocalBuf, myThid )

        IF ( iAmDoingIO ) THEN
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
          IF (filePrec .EQ. precFloat32) THEN
           DO J=1,y_size
            DO I=1,x_size
             xy_buffer_r4(I,J) = 0.0
            ENDDO
           ENDDO
          ELSEIF (filePrec .EQ. precFloat64) THEN
           DO J=1,y_size
            DO I=1,x_size
             xy_buffer_r8(I,J) = 0.0
            ENDDO
           ENDDO
          ENDIF

          bj=1
          DO npe=1,nPx*nPy
           DO bi=1,nSx
#ifdef ALLOW_USE_MPI
            loc_xGlobalLo = mpi_myXGlobalLo(npe)
            loc_yGlobalLo = mpi_myYGlobalLo(npe)
#else  /* ALLOW_USE_MPI */
            loc_xGlobalLo = myXGlobalLo
            loc_yGlobalLo = myYGlobalLo
#endif /* ALLOW_USE_MPI */
            tN = W2_mpi_myTileList(npe,bi)
            IF   ( exch2_mydNx(tN) .GT. x_size ) THEN
C-          face x-size larger than glob-size : fold it
              iGjLoc = 0
              jGjLoc = exch2_mydNx(tN) / x_size
            ELSEIF ( exch2_tNy(tN) .GT. y_size ) THEN
C-          tile y-size larger than glob-size : make a long line
              iGjLoc = exch2_mydNx(tN)
              jGjLoc = 0
            ELSE
C-          default (face fit into global-IO-array)
              iGjLoc = 0
              jGjLoc = 1
            ENDIF

            IF (filePrec .EQ. precFloat32) THEN
             DO J=1,sNy
              DO I=1,sNx
               iG = loc_xGlobalLo-1+(bi-1)*sNx+i
               jG = loc_yGlobalLo-1+(bj-1)*sNy+j
               iG_IO=exch2_txGlobalo(tN)+iGjLoc*(j-1)+i-1
               jG_IO=exch2_tyGlobalo(tN)+jGjLoc*(j-1)
               xy_buffer_r4(iG_IO,jG_IO) = globalBuf(iG,jG)
              ENDDO
             ENDDO
            ELSEIF (filePrec .EQ. precFloat64) THEN
             DO J=1,sNy
              DO I=1,sNx
               iG = loc_xGlobalLo-1+(bi-1)*sNx+i
               jG = loc_yGlobalLo-1+(bj-1)*sNy+j
               iG_IO=exch2_txGlobalo(tN)+iGjLoc*(j-1)+i-1
               jG_IO=exch2_tyGlobalo(tN)+jGjLoc*(j-1)
               xy_buffer_r8(iG_IO,jG_IO) = globalBuf(iG,jG)
              ENDDO
             ENDDO
            ENDIF

C--    end of npe & bi loops
           ENDDO
          ENDDO
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          IF (filePrec .EQ. precFloat32) THEN
           DO J=1,Ny
            DO I=1,Nx
             xy_buffer_r4(I,J) = globalBuf(I,J)
            ENDDO
           ENDDO
          ELSEIF (filePrec .EQ. precFloat64) THEN
           DO J=1,Ny
            DO I=1,Nx
             xy_buffer_r8(I,J) = globalBuf(I,J)
            ENDDO
           ENDDO
          ENDIF
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */

          irec=k+1-kLo+nNz*(irecord-1)
          IF (filePrec .EQ. precFloat32) THEN
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR4( x_size*y_size, xy_buffer_r4 )
#endif
           WRITE(dUnit,rec=irec) xy_buffer_r4
          ELSEIF (filePrec .EQ. precFloat64) THEN
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR8( x_size*y_size, xy_buffer_r8 )
#endif
           WRITE(dUnit,rec=irec) xy_buffer_r8
          ELSE
           WRITE(msgBuf,'(A)')
     &       ' MDS_WRITE_FIELD: illegal value for filePrec'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
          ENDIF
C-      end if iAmDoingIO
        ENDIF
C-     end of k loop
       ENDDO

C Close data-file
       IF ( iAmDoingIO ) THEN
         CLOSE( dUnit )
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  else .NOT.useSingleCpuIO
      ELSE

C Only do I/O if I am the master thread
       IF ( iAmDoingIO ) THEN

C If we are writing to a global file then we open it here
        IF (globalFile) THEN
         WRITE(dataFName,'(2a)') fName(1:IL),'.data'
         IF (irecord .EQ. 1) THEN
          length_of_rec=MDS_RECLEN( filePrec, sNx, myThid )
          OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &            access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         ELSE
          length_of_rec=MDS_RECLEN( filePrec, sNx, myThid )
          OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &            access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         ENDIF
        ENDIF

C Loop over all tiles
        DO bj=1,nSy
         DO bi=1,nSx
C If we are writing to a tiled MDS file then we open each one here
          IF (.NOT. globalFile) THEN
           iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
           jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
           WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
           IF (irecord .EQ. 1) THEN
            length_of_rec=MDS_RECLEN( filePrec, sNx, myThid )
            OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &            access='direct', recl=length_of_rec )
            fileIsOpen=.TRUE.
           ELSE
            length_of_rec=MDS_RECLEN( filePrec, sNx, myThid )
            OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &            access='direct', recl=length_of_rec )
            fileIsOpen=.TRUE.
           ENDIF
          ENDIF

          IF (fileIsOpen) THEN
           tNy = sNy
#ifdef ALLOW_EXCH2
           tN = W2_myTileList(bi)
           tGy = exch2_tyGlobalo(tN)
           tGx = exch2_txGlobalo(tN)
           tNy = exch2_tNy(tN)
           tNx = exch2_tNx(tN)
           IF   ( exch2_mydNx(tN) .GT. x_size ) THEN
C-         face x-size larger than glob-size : fold it
             iGjLoc = 0
             jGjLoc = exch2_mydNx(tN) / x_size
           ELSEIF ( exch2_tNy(tN) .GT. y_size ) THEN
C-         tile y-size larger than glob-size : make a long line
             iGjLoc = exch2_mydNx(tN)
             jGjLoc = 0
           ELSE
C-         default (face fit into global-IO-array)
             iGjLoc = 0
             jGjLoc = 1
           ENDIF
#endif /* ALLOW_EXCH2 */
           DO k=1,nNz
            DO j=1,tNy
             IF (globalFile) THEN
#ifdef ALLOW_EXCH2
              irec = 1 + ( tGx-1 + (j-1)*iGjLoc )/tNx
     &                 + ( tGy-1 + (j-1)*jGjLoc )*exch2_domain_nxt
     &                 + ( k-kLo + (irecord-1)*nNz
     &                   )*y_size*exch2_domain_nxt
#else /* ALLOW_EXCH2 */
              iG = myXGlobalLo-1 + (bi-1)*sNx
              jG = myYGlobalLo-1 + (bj-1)*sNy
              irec= 1 + INT(iG/sNx) + nSx*nPx*(jG+j-1)
     &                + nSx*nPx*Ny*(k-kLo)
     &                + nSx*nPx*Ny*nNz*(irecord-1)
#endif /* ALLOW_EXCH2 */
             ELSE
              irec=j + sNy*(k-kLo) + sNy*nNz*(irecord-1)
             ENDIF
             IF (filePrec .EQ. precFloat32) THEN
              IF (arrType .EQ. 'RS') THEN
               CALL MDS_SEG4toRS( j,bi,bj,k,kSize, r4seg,.FALSE.,arr )
              ELSEIF (arrType .EQ. 'RL') THEN
               CALL MDS_SEG4toRL( j,bi,bj,k,kSize, r4seg,.FALSE.,arr )
              ELSE
               WRITE(msgBuf,'(A)')
     &           ' MDS_WRITE_FIELD: illegal value for arrType'
               CALL PRINT_ERROR( msgBuf, myThid )
               STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
              ENDIF
#ifdef _BYTESWAPIO
              CALL MDS_BYTESWAPR4( sNx, r4seg )
#endif
              WRITE(dUnit,rec=irec) r4seg
             ELSEIF (filePrec .EQ. precFloat64) THEN
              IF (arrType .EQ. 'RS') THEN
               CALL MDS_SEG8toRS( j,bi,bj,k,kSize, r8seg,.FALSE.,arr )
              ELSEIF (arrType .EQ. 'RL') THEN
               CALL MDS_SEG8toRL( j,bi,bj,k,kSize, r8seg,.FALSE.,arr )
              ELSE
               WRITE(msgBuf,'(A)')
     &           ' MDS_WRITE_FIELD: illegal value for arrType'
               CALL PRINT_ERROR( msgBuf, myThid )
               STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
              ENDIF
#ifdef _BYTESWAPIO
              CALL MDS_BYTESWAPR8( sNx, r8seg )
#endif
              WRITE(dUnit,rec=irec) r8seg
             ELSE
              WRITE(msgBuf,'(A)')
     &          ' MDS_WRITE_FIELD: illegal value for filePrec'
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
             ENDIF
C End of j loop
            ENDDO
C End of k loop
           ENDDO
          ELSE
C fileIsOpen=F
           WRITE(msgBuf,'(A)')
     &       ' MDS_WRITE_FIELD: I should never get to this point'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
          ENDIF
C If we were writing to a tiled MDS file then we close it here
          IF (fileIsOpen .AND. (.NOT. globalFile)) THEN
           CLOSE( dUnit )
           fileIsOpen = .FALSE.
          ENDIF
C Create meta-file for each tile if we are tiling
          IF ( .NOT.globalFile .AND. writeMetaF ) THEN
           iG=bi+(myXGlobalLo-1)/sNx
           jG=bj+(myYGlobalLo-1)/sNy
           WRITE(metaFname,'(2A,I3.3,A,I3.3,A)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.meta'
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
           tN = W2_myTileList(bi)
           dimList(1,1)=x_size
           dimList(2,1)=exch2_txGlobalo(tN)
           dimList(3,1)=exch2_txGlobalo(tN)+sNx-1
           dimList(1,2)=y_size
           dimList(2,2)=exch2_tyGlobalo(tN)
           dimList(3,2)=exch2_tyGlobalo(tN)+sNy-1
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
C- jmc: if MISSING_TILE_IO, keep meta files unchanged
C       to stay consistent with global file structure
           dimList(1,1)=Nx
           dimList(2,1)=myXGlobalLo+(bi-1)*sNx
           dimList(3,1)=myXGlobalLo+bi*sNx-1
           dimList(1,2)=Ny
           dimList(2,2)=myYGlobalLo+(bj-1)*sNy
           dimList(3,2)=myYGlobalLo+bj*sNy-1
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           dimList(1,3)=nNz
           dimList(2,3)=1
           dimList(3,3)=nNz
           nDims=3
           IF ( nNz.EQ.1 ) nDims=2
           map2gl(1) = iGjLoc
           map2gl(2) = jGjLoc
           CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims,dimList,map2gl, 0,  ' ',
     I              0, UNSET_RL, irecord, myIter, myThid )
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

C     if useSingleCpuIO / else / end
      ENDIF

C Create meta-file for the global-file (also if useSingleCpuIO)
      IF ( writeMetaF .AND. iAmDoingIO .AND.
     &    (globalFile .OR. useSingleCpuIO) ) THEN
         WRITE(metaFName,'(2A)') fName(1:IL),'.meta'
         dimList(1,1)=x_size
         dimList(2,1)=1
         dimList(3,1)=x_size
         dimList(1,2)=y_size
         dimList(2,2)=1
         dimList(3,2)=y_size
         dimList(1,3)=nNz
         dimList(2,3)=1
         dimList(3,3)=nNz
         nDims=3
         IF ( nNz.EQ.1 ) nDims=2
         map2gl(1) = 0
         map2gl(2) = 1
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims,dimList,map2gl, 0,  ' ',
     I              0, UNSET_RL, irecord, myIter, myThid )
c    I              metaFName, dataFName, the_run_name, titleLine,
c    I              filePrec, nDims, dimList, map2gl, nFlds,  fldList,
c    I              nTimRec, timList, irecord, myIter, myThid )
      ENDIF

C To be safe, make other processes wait for I/O completion
      _BARRIER

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      RETURN
      END
