C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_writelocal.F,v 1.22 2013/01/13 22:43:53 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

CBOP
C !ROUTINE: MDS_WRITELOCAL
C !INTERFACE:
      SUBROUTINE MDS_WRITELOCAL(
     I   fName,
     I   filePrec,
     I   globFile,
     I   arrType,
     I   nNz,
     I   fldRL, fldRS,
     I   biArg, bjArg,
     I   irecord,
     I   myIter,
     I   myThArg )

C !DESCRIPTION:
C Arguments:
C
C fName     (string)  :: base name for file to write
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C globalFile (logical):: selects between writing a global or tiled file
C arrType   (char(2)) :: which array (fldRL/RS) to write, either "RL" or "RS"
C nNz       (integer) :: size of third dimension: normally either 1 or Nr
C fldRL       ( RL )  :: array to write if arrType="RL", fldRL(:,:,nNz)
C fldRS       ( RS )  :: array to write if arrType="RS", fldRS(:,:,nNz)
C biArg     (integer) :: tile X-index argument
C bjArg     (integer) :: tile Y-index argument
C irecord   (integer) :: record number to write
C myIter    (integer) :: time step number
C myThArg   (integer) :: thread argument (= my Thread Id or = 0 to simply
C                        write 1 tile without thread synchronisation)
C
C MDS_WRITELOCAL write a local-tile array corresponding to tile biArg,bjArg
C  of this Process. Threading: with myThArg=0 or when LOCBIN_IO_THREAD_SAFE
C  is defined, go for a strait writing of this tile ; otherwise, use the
C  shared buffer IO to store data from all threads, then synchronise and
C  let the master thread write nThreads tiles. If multiple tiles per thread,
C  will repeat this sequence each time this S/R is called by the master thread
C  with a different biArg,biArg. IMPORTANT: 2nd case requires that all threads
C  call this S/R and assumes symmetry in tiles per thread treatment.
C Convention regarding thread synchronisation (BARRIER): see mdsio_write_field.F
C MDS_WRITELOCAL creates either a file of the form "fName.data" and
C  "fName.meta" if the logical flag "globalFile" is set true. Otherwise
C  it creates MDS tiled files of the form "fName.xxx.yyy.data" and
C  "fName.xxx.yyy.meta". A meta-file is always created.
C Currently, the meta-files are not read because it is difficult
C  to parse files in fortran. We should read meta information before
C  adding records to an existing multi-record file.
C The precision of the file is decsribed by filePrec, set either
C  to floatPrec32 or floatPrec64. The char*(2) string arrType, either
C  "RL" or "RS", selects which array is written, either fldRL or fldRS.
C nNz allows for both 2-D and 3-D arrays to be handled. nNz=1 implies
C  a 2-D model field and nNz=Nr implies a 3-D model field.
C irecord is the record number to be written and must be >= 1.
C NOTE: It is currently assumed that the highest record number in the file
C  was the last record written. Nor is there a consistency check between the
C  routine arguments and file, i.e., if you write record 2 after record 4
C  the meta information will record the number of records to be 2. This,
C  again, is because we have read the meta information. To be fixed.
C
C Created: 03/16/99 adcroft@mit.edu
C Changed: 05/31/00 heimbach@mit.edu
C          open(dUnit, ..., status='old', ... -> status='unknown'
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
#ifdef ALLOW_FIZHI
# include "fizhi_SIZE.h"
#endif /* ALLOW_FIZHI */
#include "MDSIO_BUFF_3D.h"

C !INPUT PARAMETERS:
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globFile
      CHARACTER*(2) arrType
      INTEGER nNz
      _RL fldRL(*)
      _RS fldRS(*)
      INTEGER biArg, bjArg
      INTEGER irecord
      INTEGER myIter
      INTEGER myThArg
C !OUTPUT PARAMETERS:

C !FUNCTIONS
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL ILNBLNK, MDS_RECLEN

C !LOCAL VARIABLES:
C     bBij  :: base shift in Buffer index for tile bi,bj
      CHARACTER*(MAX_LEN_FNAM) dataFName,metaFName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL fileIsOpen
      LOGICAL globalFile
      LOGICAL iAmDoingIO
      INTEGER xSize, ySize
      INTEGER iG,jG
      INTEGER i1,i2,i,j,k
      INTEGER irec,dUnit,IL
      INTEGER dimList(3,3),nDims, map2gl(2)
      INTEGER length_of_rec
      INTEGER bBij
      INTEGER bi, bj
      INTEGER myThid, ith, nthLoop
      INTEGER tNx, tNy, global_nTx
      INTEGER tBx, tBy, iGjLoc, jGjLoc
#ifdef ALLOW_EXCH2
      INTEGER tN
#endif /* ALLOW_EXCH2 */
      _RL dummyRL(1)
      CHARACTER*8 blank8c

      DATA dummyRL(1) / 0. _d 0 /
      DATA blank8c / '        ' /

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C Set dimensions:
      xSize = Nx
      ySize = Ny
#ifdef ALLOW_EXCH2
      IF ( W2_useE2ioLayOut ) THEN
        xSize = exch2_global_Nx
        ySize = exch2_global_Ny
      ENDIF
#endif /* ALLOW_EXCH2 */

C-    default:
      iGjLoc = 0
      jGjLoc = 1

      IL = ILNBLNK( fName )
      globalFile = globFile
      myThid = MAX(myThArg,1)
#ifdef LOCBIN_IO_THREAD_SAFE
      nthLoop = 1
      iAmDoingIO = .TRUE.
#else /* LOCBIN_IO_THREAD_SAFE */
      nthLoop = nThreads
      IF ( myThArg.EQ.0 ) nthLoop = 1
      iAmDoingIO = .FALSE.
      IF ( myThid.EQ.1 ) iAmDoingIO = .TRUE.
#endif /* LOCBIN_IO_THREAD_SAFE */

      IF ( nThreads.GT.1 .AND. globFile ) THEN
C-    do not assume safe Muti-Threaded Binary IO to a single global file
C      => switch to tiled file
        globalFile = .FALSE.
        IF ( debugLevel.GE.debLevA .AND. IL.GT.0 ) THEN
         WRITE(msgBuf,'(A,I10,A,2I5,A)')
     &    'MDS_WRITELOCAL (it=', myIter, ' ; bi,bj=', biArg,bjArg,
     &    ' ): No global-file multi-threaded IO'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid )
         WRITE(msgBuf,'(2A)')
     &    'MDS_WRITELOCAL: => write tiled file: ', fName(1:IL)
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid )
        ENDIF
      ENDIF

C Record number must be >= 1
      IF (irecord .LT. 1) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_WRITELOCAL: file="', fName(1:IL), '" , iter=', myIter
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(A,I9.8)')
     &    ' MDS_WRITELOCAL: argument irecord = ',irecord
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_WRITELOCAL: invalid value for irecord'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThArg )
        STOP 'ABNORMAL END: S/R MDS_WRITELOCAL'
      ENDIF
C check for 3-D Buffer size:
      IF ( .NOT.useSingleCpuIO .AND. nNz.GT.size3dBuf ) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_WRITELOCAL: file="', fName(1:IL), '" , iter=', myIter
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(3(A,I6))')
     &    ' MDS_WRITELOCAL: Nb Lev to write =', nNz,
     &    ' >', size3dBuf, ' = buffer 3rd Dim'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_WRITELOCAL: buffer 3rd Dim. too small'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &    ' increase "size3dBuf" in "MDSIO_BUFF_3D.h" and recompile'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        CALL ALL_PROC_DIE( myThArg )
        STOP 'ABNORMAL END: S/R MDS_WRITELOCAL'
      ENDIF

C Wait for all thread to finish. This prevents other threads (e.g., master)
C  to continue to acces 3-D buffer while this thread is filling it.
      IF ( nthLoop.GT.1 ) CALL BAR2( myThid )

C-------------------------------------------------
C---    Copy from fldRL/RS to 3-D buffer (multi-threads):
      IF ( filePrec.EQ.precFloat32 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_R4toRS( shared3dBuf_r4, fldRS,
     I           0, 0, nNz, 1, nNz, biArg, bjArg, .FALSE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_R4toRL( shared3dBuf_r4, fldRL,
     I           0, 0, nNz, 1, nNz, biArg, bjArg, .FALSE., myThid )
          ELSE
            WRITE(msgBuf,'(A)')
     &         ' MDS_WRITELOCAL: illegal value for arrType'
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThArg )
            STOP 'ABNORMAL END: S/R MDS_WRITELOCAL'
          ENDIF
      ELSEIF ( filePrec.EQ.precFloat64 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_R8toRS( shared3dBuf_r8, fldRS,
     I           0, 0, nNz, 1, nNz, biArg, bjArg, .FALSE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_R8toRL( shared3dBuf_r8, fldRL,
     I           0, 0, nNz, 1, nNz, biArg, bjArg, .FALSE., myThid )
          ELSE
            WRITE(msgBuf,'(A)')
     &         ' MDS_WRITELOCAL: illegal value for arrType'
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThArg )
            STOP 'ABNORMAL END: S/R MDS_WRITELOCAL'
          ENDIF
      ELSE
          WRITE(msgBuf,'(A)')
     &         ' MDS_WRITELOCAL: illegal value for filePrec'
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThArg )
          STOP 'ABNORMAL END: S/R MDS_WRITELOCAL'
      ENDIF
C-------------------------------------------------

C Wait for all threads to finish filling shared buffer
      IF ( nthLoop.GT.1 ) CALL BAR2( myThid )

C Only do I/O if I am the master thread
      IF ( iAmDoingIO ) THEN

C Assume nothing
        fileIsOpen=.FALSE.

C Assign a free unit number as the I/O channel for this routine
        CALL MDSFINDUNIT( dUnit, myThid )

C If we are writing to a global file then we open it here
        IF (globalFile) THEN
          WRITE(dataFName,'(2A)') fName(1:IL),'.data'
          length_of_rec = MDS_RECLEN( filePrec, sNx, myThid )
          IF (irecord .EQ. 1) THEN
           OPEN( dUnit, file=dataFName, status='unknown',
     &           access='direct', recl=length_of_rec )
          ELSE
           OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &           access='direct', recl=length_of_rec )
          ENDIF
          fileIsOpen=.TRUE.
        ENDIF

C Loop over tiles
        DO ith=1,nthLoop
          bi = biArg + myBxLo(ith) - 1
          bj = bjArg + myByLo(ith) - 1

          bBij = sNx*sNy*nNz*( bi-1 + (bj-1)*nSx )
          i1 = bBij + 1
#ifdef _BYTESWAPIO
          IF ( filePrec.EQ.precFloat32 ) THEN
            CALL MDS_BYTESWAPR4( sNx*sNy*nNz, shared3dBuf_r4(i1) )
          ELSE
            CALL MDS_BYTESWAPR8( sNx*sNy*nNz, shared3dBuf_r8(i1) )
          ENDIF
#endif

          tNx = sNx
          tNy = sNy
          global_nTx = xSize/sNx
          tBx = myXGlobalLo-1 + (bi-1)*sNx
          tBy = myYGlobalLo-1 + (bj-1)*sNy
#ifdef ALLOW_EXCH2
          IF ( W2_useE2ioLayOut ) THEN
            tN = W2_myTileList(bi,bj)
c           global_nTx = exch2_global_Nx/sNx
            tBx = exch2_txGlobalo(tN) - 1
            tBy = exch2_tyGlobalo(tN) - 1
            IF   ( exch2_mydNx(tN) .GT. xSize ) THEN
C-          face x-size larger than glob-size : fold it
              iGjLoc = 0
              jGjLoc = exch2_mydNx(tN) / xSize
            ELSEIF ( exch2_tNy(tN) .GT. ySize ) THEN
C-          tile y-size larger than glob-size : make a long line
              iGjLoc = exch2_mydNx(tN)
              jGjLoc = 0
            ELSE
C-          default (face fit into global-IO-array)
              iGjLoc = 0
              jGjLoc = 1
            ENDIF
          ENDIF
#endif /* ALLOW_EXCH2 */

          IF (globalFile) THEN
C--- Case of 1 Global file:

           DO k=1,nNz
            DO j=1,sNy
C-       compute record number:
             irec = 1 + ( tBx + (j-1)*iGjLoc )/tNx
     &                + ( tBy + (j-1)*jGjLoc )*global_nTx
     &            +  ( k-1 + (irecord-1)*nNz )*global_nTx*ySize
             i1 = bBij + 1 + (j-1)*sNx + (k-1)*sNx*sNy
             i2 = bBij +         j*sNx + (k-1)*sNx*sNy
             IF ( filePrec.EQ.precFloat32 ) THEN
               WRITE(dUnit,rec=irec) (shared3dBuf_r4(i),i=i1,i2)
             ELSE
               WRITE(dUnit,rec=irec) (shared3dBuf_r8(i),i=i1,i2)
             ENDIF
C End of j,k loops
            ENDDO
           ENDDO

          ELSE
C--- Case of 1 file per tile (globalFile=F):

C If we are writing to a tiled MDS file then we open each one here
            iG=bi+(myXGlobalLo-1)/sNx
            jG=bj+(myYGlobalLo-1)/sNy
            WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &               fName(1:IL),'.',iG,'.',jG,'.data'
            length_of_rec = MDS_RECLEN( filePrec, sNx*sNy*nNz, myThid )
            IF (irecord .EQ. 1) THEN
             OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &             access='direct', recl=length_of_rec )
            ELSE
             OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &             access='direct', recl=length_of_rec )
            ENDIF
            fileIsOpen=.TRUE.

            irec = irecord
            i1 = bBij + 1
            i2 = bBij + sNx*sNy*nNz
            IF ( filePrec.EQ.precFloat32 ) THEN
              WRITE(dUnit,rec=irec) (shared3dBuf_r4(i),i=i1,i2)
            ELSE
              WRITE(dUnit,rec=irec) (shared3dBuf_r8(i),i=i1,i2)
            ENDIF

C If we were writing to a tiled MDS file then we close it here
            IF ( fileIsOpen ) THEN
              CLOSE( dUnit )
              fileIsOpen = .FALSE.
            ENDIF

C--- End Global File / tile-file cases
          ENDIF

C Create meta-file for each tile if we are tiling
          IF ( .NOT.globalFile ) THEN
           iG=bi+(myXGlobalLo-1)/sNx
           jG=bj+(myYGlobalLo-1)/sNy
           WRITE(metaFname,'(2A,I3.3,A,I3.3,A)')
     &                fName(1:IL),'.',iG,'.',jG,'.meta'
           dimList(1,1) = xSize
           dimList(2,1) = tBx + 1
           dimList(3,1) = tBx + tNx
           dimList(1,2) = ySize
           dimList(2,2) = tBy + 1
           dimList(3,2) = tBy + tNy
           dimList(1,3) = Nr
           dimList(2,3) = 1
           dimList(3,3) = Nr
           nDims = 3
           IF ( nNz.EQ.1 ) nDims = 2
           map2gl(1) = iGjLoc
           map2gl(2) = jGjLoc
           CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, blank8c,
     I              0, dummyRL, oneRL, irecord, myIter, myThid )
          ENDIF

C End of ith loop
        ENDDO

C If global file was opened then close it
        IF (fileIsOpen .AND. globalFile) THEN
          CLOSE( dUnit )
          fileIsOpen = .FALSE.
        ENDIF

C Create meta-file for the global-file
        IF (globalFile) THEN
         WRITE(metaFName,'(2A)') fName(1:IL),'.meta'
         dimList(1,1) = xSize
         dimList(2,1) = 1
         dimList(3,1) = xSize
         dimList(1,2) = ySize
         dimList(2,2) = 1
         dimList(3,2) = ySize
         dimList(1,3) = Nr
         dimList(2,3) = 1
         dimList(3,3) = Nr
         nDims = 3
         IF ( nNz.EQ.1 ) nDims = 2
         map2gl(1) = 0
         map2gl(2) = 1
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, blank8c,
     I              0, dummyRL, oneRL, irecord, myIter, myThid )
        ENDIF

C-    end if iAmDoingIO
      ENDIF

C Make other threads wait for I/O completion so that after this,
C  3-D buffer can again be modified by any thread
c     IF ( nthLoop.GT.1 ) CALL BAR2( myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      RETURN
      END
