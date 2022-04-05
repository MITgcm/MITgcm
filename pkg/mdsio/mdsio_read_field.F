#include "MDSIO_OPTIONS.h"

CBOP
C !ROUTINE: MDS_READ_FIELD
C !INTERFACE:
      SUBROUTINE MDS_READ_FIELD(
     I   fName,
     I   filePrec,
     I   useCurrentDir,
     I   arrType,
     I   kSize,kLo,kHi,
     O   fldRL, fldRS,
     I   irecord,
     I   myThid )

C !DESCRIPTION:
C Arguments:
C
C fName     (string)  :: base name for file to read
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C useCurrentDir(logic):: always read from the current directory (even if
C                        "mdsioLocalDir" is set)
C arrType   (char(2)) :: which array (fldRL/RS) to read into, either "RL" or "RS"
C kSize     (integer) :: size of third dimension: normally either 1 or Nr
C kLo       (integer) :: 1rst vertical level (of array fldRL/RS) to read-in
C kHi       (integer) :: last vertical level (of array fldRL/RS) to read-in
C fldRL       ( RL )  :: array to read into if arrType="RL", fldRL(:,:,kSize,:,:)
C fldRS       ( RS )  :: array to read into if arrType="RS", fldRS(:,:,kSize,:,:)
C irecord   (integer) :: record number to read
C myIter    (integer) :: time step number
C myThid    (integer) :: thread identifier
C
C MDS_READ_FIELD first checks to see IF the file "fName" exists, then
C  IF the file "fName.data" exists and finally the tiled files of the
C  form "fName.xxx.yyy.data" exist. Currently, the meta-files are not
C  read because it is difficult to parse files in fortran.
C The precision of the file is decsribed by filePrec, set either
C  to floatPrec32 or floatPrec64. The char*(2) string arrType, either "RL"
C  or "RS", selects which array is filled in, either fldRL or fldRS.
C (kSize,kLo,kHi) allows for both 2-D and 3-D arrays to be handled, with
C  the option to only read and fill-in a sub-set of consecutive vertical
C  levels (from kLo to kHi) ; (kSize,kLo,kHi)=(1,1,1) implies a 2-D model
C  field and (kSize,kLo,kHi)=(Nr,1,Nr) implies a 3-D model field.
C irecord is the record number to be read and must be >= 1.
C The file data is stored in fldRL/RS *but* the overlaps are *not* updated,
C  i.e., an exchange must be called.
C
C- Multi-threaded: Only Master thread does IO (and MPI calls) and put data
C   to a shared buffer that any thread can get access to.
C- Convention regarding thread synchronisation (BARRIER):
C  A per-thread (or per tile) partition of the 2-D shared-buffer (sharedLocBuf_r4/r8)
C   is readily available => any access (e.g., by master-thread) to a portion
C   owned by an other thread is put between BARRIER (protected).
C  No thread partition exist for the 3-D shared buffer (shared3dBuf_r4/r8).
C   Therefore, the 3-D buffer is considered to be owned by master-thread and
C   any access by other than master thread is put between BARRIER (protected).
C
C Created: 03/16/99 adcroft@mit.edu
CEOP

C !USES:
      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */
#include "EEBUFF_SCPU.h"
#ifdef ALLOW_FIZHI
# include "fizhi_SIZE.h"
#endif /* ALLOW_FIZHI */
#include "MDSIO_BUFF_3D.h"

C !INPUT PARAMETERS:
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL useCurrentDir
      CHARACTER*(2) arrType
      INTEGER kSize, kLo, kHi
      INTEGER irecord
      INTEGER myThid
C !OUTPUT PARAMETERS:
      _RL  fldRL(*)
      _RS  fldRS(*)

C !FUNCTIONS
      INTEGER  ILNBLNK
      INTEGER  MDS_RECLEN
      LOGICAL  MASTER_CPU_IO
      EXTERNAL ILNBLNK
      EXTERNAL MDS_RECLEN
      EXTERNAL MASTER_CPU_IO

C !LOCAL VARIABLES:
C     bBij  :: base shift in Buffer index for tile bi,bj
      CHARACTER*(MAX_LEN_FNAM) dataFName,pfName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL exst
      LOGICAL globalFile, fileIsOpen
      LOGICAL iAmDoingIO
      LOGICAL useExch2ioLayOut
      INTEGER xSize, ySize
      INTEGER iG,jG,bi,bj
      INTEGER i1,i2,i,j,k,nNz
      INTEGER irec,dUnit,IL,pIL
      INTEGER length_of_rec
      INTEGER bBij
      INTEGER tNx, tNy, global_nTx
      INTEGER tBx, tBy, iGjLoc, jGjLoc
#ifdef ALLOW_EXCH2
      INTEGER tN
#endif /* ALLOW_EXCH2 */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C Set dimensions:
      xSize = Nx
      ySize = Ny
      useExch2ioLayOut = .FALSE.
#ifdef ALLOW_EXCH2
      IF ( W2_useE2ioLayOut ) THEN
        xSize = exch2_global_Nx
        ySize = exch2_global_Ny
        useExch2ioLayOut = .TRUE.
      ENDIF
#endif /* ALLOW_EXCH2 */

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )
      nNz = 1 + kHi - kLo

C Only do I/O if I am the master thread (and mpi process 0 IF useSingleCpuIO):
      iAmDoingIO = MASTER_CPU_IO(myThid)

C File name should not be too long:
C    IL(+pIL if not useCurrentDir)(+5: '.data')(+8: bi,bj) =< MAX_LEN_FNAM
C    and shorter enough to be written to msgBuf with other informations
      IF ( useCurrentDir .AND. (90+IL).GT.MAX_LEN_MBUF ) THEN
        WRITE(msgBuf,'(2A,2(I4,A))') 'MDS_READ_FIELD: ',
     &   'Too long (IL=',IL,') file name:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(errorMessageUnit,'(3A)')'file: >',fName(1:IL),'<'
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
      ELSEIF ( (90+IL+pIL).GT.MAX_LEN_MBUF ) THEN
        WRITE(msgBuf,'(2A,2(I4,A))') 'MDS_READ_FIELD: ',
     &   'Too long (pIL=',pIL,', IL=',IL,') pfix + file name:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(errorMessageUnit,'(3A)')'pfix: >',mdsioLocalDir(1:pIL),'<'
        WRITE(errorMessageUnit,'(3A)')'file: >',fName(1:IL),'<'
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
      ENDIF
C Record number must be >= 1
      IF (irecord .LT. 1) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_READ_FIELD: file="', fName(1:IL), '"'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,I9.8)')
     &    ' MDS_READ_FIELD: argument irecord = ',irecord
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_READ_FIELD: Invalid value for irecord'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
      ENDIF
C check for valid sub-set of levels:
      IF ( kLo.LT.1 .OR. kHi.GT.kSize ) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_READ_FIELD: file="', fName(1:IL), '"'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(3(A,I6))')
     &    ' MDS_READ_FIELD: arguments kSize=', kSize,
     &    ' , kLo=', kLo, ' , kHi=', kHi
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_READ_FIELD: invalid sub-set of levels'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
      ENDIF
C check for 3-D Buffer size:
      IF ( .NOT.useSingleCpuIO .AND. nNz.GT.size3dBuf ) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_READ_FIELD: file="', fName(1:IL), '"'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(3(A,I6))')
     &    ' MDS_READ_FIELD: Nb Lev to read =', nNz,
     &    ' >', size3dBuf, ' = buffer 3rd Dim'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_READ_FIELD: buffer 3rd Dim. too small'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &    ' increase "size3dBuf" in "MDSIO_BUFF_3D.h" and recompile'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
      ENDIF

C Only do I/O if I am the master thread
      IF ( iAmDoingIO ) THEN

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
          IF ( debugLevel .GE. debLevB ) THEN
            WRITE(msgBuf,'(A,A)')
     &      ' MDS_READ_FIELD: opening global file: ',dataFName(1:IL)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
          ENDIF
          globalFile = .TRUE.
        ENDIF

C If negative check for global file with MDS name (ie. fName.data)
        IF (.NOT. globalFile) THEN
          WRITE(dataFName,'(2a)') fName(1:IL),'.data'
          INQUIRE( file=dataFName, exist=exst )
          IF (exst) THEN
           IF ( debugLevel .GE. debLevB ) THEN
            WRITE(msgBuf,'(A,A)')
     &      ' MDS_READ_FIELD: opening global file: ',dataFName(1:IL+5)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
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
          length_of_rec = MDS_RECLEN( filePrec, xSize*ySize, myThid )
          OPEN( dUnit, file=dataFName, status='old', _READONLY_ACTION
     &         access='direct', recl=length_of_rec )
         ELSE
          WRITE(msgBuf,'(2A)')
     &      ' MDS_READ_FIELD: filename: ', dataFName(1:IL+5)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid)
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &      ' MDS_READ_FIELD: File does not exist'
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid)
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
         ENDIF
C- endif iAmDoingIO
       ENDIF

       DO k=kLo,kHi

C master thread of process 0, only, read from file
        IF ( iAmDoingIO ) THEN
          irec = 1 + k-kLo + (irecord-1)*nNz
          IF (filePrec .EQ. precFloat32) THEN
           READ(dUnit,rec=irec) ( xy_buffer_r4(i),i=1,xSize*ySize )
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR4( xSize*ySize, xy_buffer_r4 )
#endif
          ELSE
           READ(dUnit,rec=irec) ( xy_buffer_r8(i),i=1,xSize*ySize )
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR8( xSize*ySize, xy_buffer_r8 )
#endif
          ENDIF
C- endif iAmDoingIO
        ENDIF

C Wait for all thread to finish. This prevents other threads to continue
C  to acces shared buffer while master thread is loading data into
        CALL BAR2( myThid )

        IF ( filePrec.EQ.precFloat32 ) THEN
          CALL SCATTER_2D_R4(
     U                        xy_buffer_r4,
     O                        sharedLocBuf_r4,
     I                        xSize, ySize,
     I                        useExch2ioLayOut, .FALSE., myThid )
C All threads wait for Master to finish loading into shared buffer
          CALL BAR2( myThid )
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_R4toRS( sharedLocBuf_r4, fldRS,
     I                  0, 0, 1, k, kSize, 0, 0, .TRUE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_R4toRL( sharedLocBuf_r4, fldRL,
     I                  0, 0, 1, k, kSize, 0, 0, .TRUE., myThid )
          ELSE
            WRITE(msgBuf,'(A)')
     &          ' MDS_READ_FIELD: illegal value for arrType'
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
          ENDIF
        ELSEIF ( filePrec.EQ.precFloat64 ) THEN
          CALL SCATTER_2D_R8(
     U                        xy_buffer_r8,
     O                        sharedLocBuf_r8,
     I                        xSize, ySize,
     I                        useExch2ioLayOut, .FALSE., myThid )
C All threads wait for Master to finish loading into shared buffer
          CALL BAR2( myThid )
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_R8toRS( sharedLocBuf_r8, fldRS,
     I                  0, 0, 1, k, kSize, 0, 0, .TRUE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_R8toRL( sharedLocBuf_r8, fldRL,
     I                  0, 0, 1, k, kSize, 0, 0, .TRUE., myThid )
          ELSE
            WRITE(msgBuf,'(A)')
     &          ' MDS_READ_FIELD: illegal value for arrType'
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
          ENDIF
        ELSE
          WRITE(msgBuf,'(A)')
     &            ' MDS_READ_FIELD: illegal value for filePrec'
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
        ENDIF

       ENDDO
c      ENDDO k=kLo,kHi

       IF ( iAmDoingIO ) THEN
         CLOSE( dUnit )
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  else .NOT.useSingleCpuIO
      ELSE

C Wait for all thread to finish. This prevents other threads to continue
C  to acces 3-D buffer while master thread is reading
c      CALL BAR2( myThid )

C Only do I/O if I am the master thread
       IF ( iAmDoingIO ) THEN

C If we are reading from a global file then we open it here
        IF (globalFile) THEN
         length_of_rec = MDS_RECLEN( filePrec, sNx, myThid )
         OPEN( dUnit, file=dataFName, status='old', _READONLY_ACTION
     &        access='direct', recl=length_of_rec )
         fileIsOpen=.TRUE.
        ENDIF

C Loop over all tiles
        DO bj=1,nSy
         DO bi=1,nSx
          bBij = sNx*sNy*nNz*( bi-1 + (bj-1)*nSx )

          IF (globalFile) THEN
C--- Case of 1 Global file:

c         IF (fileIsOpen) THEN
           tNx = sNx
           tNy = sNy
           global_nTx = xSize/sNx
           tBx = myXGlobalLo-1 + (bi-1)*sNx
           tBy = myYGlobalLo-1 + (bj-1)*sNy
           iGjLoc = 0
           jGjLoc = 1
#ifdef ALLOW_EXCH2
           IF ( useExch2ioLayOut ) THEN
             tN = W2_myTileList(bi,bj)
c            tNx = exch2_tNx(tN)
c            tNy = exch2_tNy(tN)
c            global_nTx = exch2_global_Nx/tNx
             tBx = exch2_txGlobalo(tN) - 1
             tBy = exch2_tyGlobalo(tN) - 1
             IF   ( exch2_mydNx(tN) .GT. xSize ) THEN
C-           face x-size larger than glob-size : fold it
               iGjLoc = 0
               jGjLoc = exch2_mydNx(tN) / xSize
             ELSEIF ( exch2_tNy(tN) .GT. ySize ) THEN
C-           tile y-size larger than glob-size : make a long line
               iGjLoc = exch2_mydNx(tN)
               jGjLoc = 0
             ELSE
C-           default (face fit into global-IO-array)
               iGjLoc = 0
               jGjLoc = 1
             ENDIF
           ENDIF
#endif /* ALLOW_EXCH2 */

           DO k=kLo,kHi
            DO j=1,tNy
             irec = 1 + ( tBx + (j-1)*iGjLoc )/sNx
     &                + ( tBy + (j-1)*jGjLoc )*global_nTx
     &            +( k-kLo + (irecord-1)*nNz )*global_nTx*ySize
             i1 = bBij + 1 + (j-1)*sNx + (k-kLo)*sNx*sNy
             i2 = bBij +         j*sNx + (k-kLo)*sNx*sNy
             IF ( filePrec.EQ.precFloat32 ) THEN
              READ(dUnit,rec=irec) (shared3dBuf_r4(i),i=i1,i2)
             ELSE
              READ(dUnit,rec=irec) (shared3dBuf_r8(i),i=i1,i2)
             ENDIF
C End of j,k loops
            ENDDO
           ENDDO

C end if fileIsOpen
c         ENDIF

          ELSE
C--- Case of 1 file per tile (globalFile=F):

C If we are reading from a tiled MDS file then we open each one here
           iG=bi+(myXGlobalLo-1)/sNx
           jG=bj+(myYGlobalLo-1)/sNy
           WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &            pfName(1:pIL),'.',iG,'.',jG,'.data'
           INQUIRE( file=dataFName, exist=exst )
C Of course, we only open the file if the tile is "active"
C (This is a place-holder for the active/passive mechanism
           IF (exst) THEN
            IF ( debugLevel .GE. debLevB ) THEN
             WRITE(msgBuf,'(A,A)')
     &       ' MDS_READ_FIELD: opening file: ',dataFName(1:pIL+13)
             CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid)
            ENDIF
            length_of_rec = MDS_RECLEN( filePrec, sNx*sNy*nNz, myThid )
            OPEN( dUnit, file=dataFName, status='old', _READONLY_ACTION
     &            access='direct', recl=length_of_rec )
            fileIsOpen=.TRUE.
           ELSE
            fileIsOpen=.FALSE.
            WRITE(msgBuf,'(4A)') ' MDS_READ_FIELD: filename: ',
     &             fName(1:IL),' , ', dataFName(1:pIL+13)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A)')
     &      ' MDS_READ_FIELD: Files DO not exist'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
           ENDIF

           irec = irecord
           i1 = bBij + 1
           i2 = bBij + sNx*sNy*nNz
           IF ( filePrec.EQ.precFloat32 ) THEN
             READ(dUnit,rec=irec) (shared3dBuf_r4(i),i=i1,i2)
           ELSE
             READ(dUnit,rec=irec) (shared3dBuf_r8(i),i=i1,i2)
           ENDIF

C here We close the tiled MDS file
           IF ( fileIsOpen ) THEN
             CLOSE( dUnit )
             fileIsOpen = .FALSE.
           ENDIF

C--- End Global File / tile-file cases
          ENDIF

C End of bi,bj loops
         ENDDO
        ENDDO

C If global file was opened then close it
        IF (fileIsOpen .AND. globalFile) THEN
          CLOSE( dUnit )
          fileIsOpen = .FALSE.
        ENDIF

#ifdef _BYTESWAPIO
        IF ( filePrec.EQ.precFloat32 ) THEN
          CALL MDS_BYTESWAPR4( sNx*sNy*nNz*nSx*nSy, shared3dBuf_r4 )
        ELSE
          CALL MDS_BYTESWAPR8( sNx*sNy*nNz*nSx*nSy, shared3dBuf_r8 )
        ENDIF
#endif

C- endif iAmDoingIO
       ENDIF

C All threads wait for Master to finish reading into shared buffer
       CALL BAR2( myThid )

C---    Copy from 3-D buffer to fldRL/RS (multi-threads):
        IF ( filePrec.EQ.precFloat32 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_R4toRS( shared3dBuf_r4, fldRS,
     I              0, 0, nNz, kLo, kSize, 0, 0, .TRUE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_R4toRL( shared3dBuf_r4, fldRL,
     I              0, 0, nNz, kLo, kSize, 0, 0, .TRUE., myThid )
          ELSE
            WRITE(msgBuf,'(A)')
     &         ' MDS_READ_FIELD: illegal value for arrType'
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
          ENDIF
        ELSEIF ( filePrec.EQ.precFloat64 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_R8toRS( shared3dBuf_r8, fldRS,
     I              0, 0, nNz, kLo, kSize, 0, 0, .TRUE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_R8toRL( shared3dBuf_r8, fldRL,
     I              0, 0, nNz, kLo, kSize, 0, 0, .TRUE., myThid )
          ELSE
            WRITE(msgBuf,'(A)')
     &         ' MDS_READ_FIELD: illegal value for arrType'
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
          ENDIF
        ELSE
          WRITE(msgBuf,'(A)')
     &         ' MDS_READ_FIELD: illegal value for filePrec'
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_FIELD'
        ENDIF

C Wait for all threads to finish getting data from 3-D shared buffer.
C  This prevents the master-thread to change the buffer content before
C  every one got his data.
       CALL BAR2( myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     if useSingleCpuIO / else / end
      ENDIF

      RETURN
      END
