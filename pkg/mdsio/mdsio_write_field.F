C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_write_field.F,v 1.20 2014/08/12 17:38:11 jmc Exp $
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
     I   fldRL, fldRS,
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
C arrType   (char(2)) :: which array (fldRL/RS) to write, either "RL" or "RS"
C kSize     (integer) :: size of third dimension: normally either 1 or Nr
C kLo       (integer) :: 1rst vertical level (of array fldRL/RS) to write
C kHi       (integer) :: last vertical level (of array fldRL/RS) to write
C fldRL       ( RL )  :: array to write if arrType="RL", fldRL(:,:,kSize,:,:)
C fldRS       ( RS )  :: array to write if arrType="RS", fldRS(:,:,kSize,:,:)
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
C The precision of the file is described by filePrec, set either
C  to floatPrec32 or floatPrec64. The char*(2) string arrType, either
C  "RL" or "RS", selects which array is written, either fldRL or fldRS.
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
C- Multi-threaded: Only Master thread does IO (and MPI calls) and get data
C   from a shared buffer that any thread can copy to.
C- Convention regarding thread synchronisation (BARRIER):
C  A per-thread (or per tile) partition of the 2-D shared-buffer (sharedLocBuf_r4/r8)
C   is readily available => any access (e.g., by master-thread) to a portion
C   owned by an other thread is put between BARRIER (protected).
C  No thread partition exist for the 3-D shared buffer (shared3dBuf_r4/r8);
C   Therefore, the 3-D buffer is considered to be owned by master-thread and
C   any access by other than master thread is put between BARRIER (protected).
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
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
# include "W2_EXCH2_TOPOLOGY.h"
# include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */
#include "EEBUFF_SCPU.h"
#ifdef ALLOW_FIZHI
# include "fizhi_SIZE.h"
#endif /* ALLOW_FIZHI */
#include "MDSIO_BUFF_3D.h"

C !INPUT PARAMETERS:
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalFile
      LOGICAL useCurrentDir
      CHARACTER*(2) arrType
      INTEGER kSize, kLo, kHi
      _RL fldRL(*)
      _RS fldRS(*)
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
C     bBij  :: base shift in Buffer index for tile bi,bj
      CHARACTER*(MAX_LEN_FNAM) dataFName,metaFName,pfName
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL fileIsOpen
      LOGICAL iAmDoingIO
      LOGICAL writeMetaF
      LOGICAL useExch2ioLayOut
      LOGICAL zeroBuff
      INTEGER xSize, ySize
      INTEGER irecord
      INTEGER iG,jG,bi,bj
      INTEGER i1,i2,i,j,k,nNz
      INTEGER irec,dUnit,IL,pIL
      INTEGER dimList(3,3), nDims, map2gl(2)
      INTEGER length_of_rec
      INTEGER bBij
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
      useExch2ioLayOut = .FALSE.
#ifdef ALLOW_EXCH2
      IF ( W2_useE2ioLayOut ) THEN
        xSize = exch2_global_Nx
        ySize = exch2_global_Ny
        useExch2ioLayOut = .TRUE.
      ENDIF
#endif /* ALLOW_EXCH2 */

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

C File name should not be too long:
C    IL(+pIL if not useCurrentDir)(+5: '.data')(+8: bi,bj) =< MAX_LEN_FNAM
C    and shorter enough to be written to msgBuf with other informations
      IF ( useCurrentDir .AND. (90+IL).GT.MAX_LEN_MBUF ) THEN
        WRITE(msgBuf,'(2A,2(I4,A))') 'MDS_WRITE_FIELD: ',
     &   'Too long (IL=',IL,') file name:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(errorMessageUnit,'(3A)')'file: >',fName(1:IL),'<'
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
      ELSEIF ( (90+IL+pIL).GT.MAX_LEN_MBUF ) THEN
        WRITE(msgBuf,'(2A,2(I4,A))') 'MDS_WRITE_FIELD: ',
     &   'Too long (pIL=',pIL,', IL=',IL,') pfix + file name:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(errorMessageUnit,'(3A)')'pfix: >',mdsioLocalDir(1:pIL),'<'
        WRITE(errorMessageUnit,'(3A)')'file: >',fName(1:IL),'<'
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
      ENDIF
C Record number must be >= 1
      IF (irecord .LT. 1) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_WRITE_FIELD: file="', fName(1:IL), '" , iter=', myIter
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,I9.8)')
     &    ' MDS_WRITE_FIELD: argument irecord = ',irecord
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_WRITE_FIELD: invalid value for irecord'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
      ENDIF
C check for valid sub-set of levels:
      IF ( kLo.LT.1 .OR. kHi.GT.kSize ) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_WRITE_FIELD: file="', fName(1:IL), '" , iter=', myIter
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(3(A,I6))')
     &    ' MDS_WRITE_FIELD: arguments kSize=', kSize,
     &    ' , kLo=', kLo, ' , kHi=', kHi
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_WRITE_FIELD: invalid sub-set of levels'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
      ENDIF
C check for 3-D Buffer size:
      IF ( .NOT.useSingleCpuIO .AND. nNz.GT.size3dBuf ) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_WRITE_FIELD: file="', fName(1:IL), '" , iter=', myIter
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(3(A,I6))')
     &    ' MDS_WRITE_FIELD: Nb Lev to write =', nNz,
     &    ' >', size3dBuf, ' = buffer 3rd Dim'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_WRITE_FIELD: buffer 3rd Dim. too small'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &    ' increase "size3dBuf" in "MDSIO_BUFF_3D.h" and recompile'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid)
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
      ENDIF

C Only do I/O if I am the master thread
      IF ( iAmDoingIO ) THEN

C Assign special directory
        IF ( useCurrentDir .OR. pIL.EQ.0 ) THEN
         pfName = fName
        ELSE
         WRITE(pfName,'(2A)') mdsioLocalDir(1:pIL), fName(1:IL)
        ENDIF
        pIL=ILNBLNK( pfName )
        IF ( debugLevel .GE. debLevC ) THEN
          WRITE(msgBuf,'(A,I8,I6,3I4,2A)')
     &      ' MDS_WRITE_FIELD: it,rec,kS,kL,kH=', myIter, jrecord,
     &      kSize, kLo, kHi, ' file=', pfName(1:pIL)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF

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
         length_of_rec = MDS_RECLEN( filePrec, xSize*ySize, myThid )
         IF (irecord .EQ. 1) THEN
          OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &        access='direct', recl=length_of_rec )
         ELSE
          OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &        access='direct', recl=length_of_rec )
         ENDIF
       ENDIF

C Gather array and write it to file, one vertical level at a time
       DO k=kLo,kHi
        zeroBuff = k.EQ.kLo
C-      copy from fldRL/RS(level=k) to 2-D "local":
        IF ( filePrec.EQ.precFloat32 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_R4toRS( sharedLocBuf_r4, fldRS,
     I                 0, 0, 1, k, kSize, 0, 0, .FALSE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_R4toRL( sharedLocBuf_r4, fldRL,
     I                 0, 0, 1, k, kSize, 0, 0, .FALSE., myThid )
          ELSE
            WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITE_FIELD: illegal value for arrType=', arrType
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
          ENDIF
C Wait for all threads to finish filling shared buffer
          CALL BAR2( myThid )
          CALL GATHER_2D_R4(
     O                       xy_buffer_r4,
     I                       sharedLocBuf_r4,
     I                       xSize, ySize,
     I                       useExch2ioLayOut, zeroBuff, myThid )
        ELSEIF ( filePrec.EQ.precFloat64 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_R8toRS( sharedLocBuf_r8, fldRS,
     I                 0, 0, 1, k, kSize, 0, 0, .FALSE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_R8toRL( sharedLocBuf_r8, fldRL,
     I                 0, 0, 1, k, kSize, 0, 0, .FALSE., myThid )
          ELSE
            WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITE_FIELD: illegal value for arrType=', arrType
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
          ENDIF
C Wait for all threads to finish filling shared buffer
          CALL BAR2( myThid )
          CALL GATHER_2D_R8(
     O                       xy_buffer_r8,
     I                       sharedLocBuf_r8,
     I                       xSize, ySize,
     I                       useExch2ioLayOut, zeroBuff, myThid )
        ELSE
          WRITE(msgBuf,'(A,I6)')
     &      ' MDS_WRITE_FIELD: illegal value for filePrec=',filePrec
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
        ENDIF
C Make other threads wait for "gather" completion so that after this,
C  shared buffer can again be modified by any thread
        CALL BAR2( myThid )

        IF ( iAmDoingIO ) THEN
          irec = 1 + k-kLo + (irecord-1)*nNz
          IF ( filePrec.EQ.precFloat32 ) THEN
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR4( xSize*ySize, xy_buffer_r4 )
#endif
           WRITE(dUnit,rec=irec) ( xy_buffer_r4(i),i=1,xSize*ySize )
          ELSE
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR8( xSize*ySize, xy_buffer_r8 )
#endif
           WRITE(dUnit,rec=irec) ( xy_buffer_r8(i),i=1,xSize*ySize )
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

C Wait for all thread to finish. This prevents other threads (e.g., master)
C  to continue to acces 3-D buffer while this thread is filling it.
        CALL BAR2( myThid )

C---    Copy from fldRL/RS to 3-D buffer (multi-threads):
        IF ( filePrec.EQ.precFloat32 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_R4toRS( shared3dBuf_r4, fldRS,
     I              0, 0, nNz, kLo, kSize, 0,0, .FALSE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_R4toRL( shared3dBuf_r4, fldRL,
     I              0, 0, nNz, kLo, kSize, 0,0, .FALSE., myThid )
          ELSE
            WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITE_FIELD: illegal value for arrType=', arrType
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
          ENDIF
        ELSEIF ( filePrec.EQ.precFloat64 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_R8toRS( shared3dBuf_r8, fldRS,
     I              0, 0, nNz, kLo, kSize, 0,0, .FALSE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_R8toRL( shared3dBuf_r8, fldRL,
     I              0, 0, nNz, kLo, kSize, 0,0, .FALSE., myThid )
          ELSE
            WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITE_FIELD: illegal value for arrType=', arrType
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
          ENDIF
        ELSE
          WRITE(msgBuf,'(A,I6)')
     &      ' MDS_WRITE_FIELD: illegal value for filePrec=',filePrec
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITE_FIELD'
        ENDIF

C Wait for all threads to finish filling shared buffer
       CALL BAR2( myThid )

C Only do I/O if I am the master thread
       IF ( iAmDoingIO ) THEN

#ifdef _BYTESWAPIO
        IF ( filePrec.EQ.precFloat32 ) THEN
          CALL MDS_BYTESWAPR4( sNx*sNy*nNz*nSx*nSy, shared3dBuf_r4 )
        ELSE
          CALL MDS_BYTESWAPR8( sNx*sNy*nNz*nSx*nSy, shared3dBuf_r8 )
        ENDIF
#endif

C If we are writing to a global file then we open it here
        IF (globalFile) THEN
          WRITE(dataFName,'(2a)') fName(1:IL),'.data'
          length_of_rec = MDS_RECLEN( filePrec, sNx, myThid )
          IF (irecord .EQ. 1) THEN
           OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &             access='direct', recl=length_of_rec )
          ELSE
           OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &             access='direct', recl=length_of_rec )
          ENDIF
          fileIsOpen=.TRUE.
        ENDIF

C Loop over all tiles
        DO bj=1,nSy
         DO bi=1,nSx
          bBij = sNx*sNy*nNz*( bi-1 + (bj-1)*nSx )

          tNx = sNx
          tNy = sNy
          global_nTx = xSize/sNx
          tBx = myXGlobalLo-1 + (bi-1)*sNx
          tBy = myYGlobalLo-1 + (bj-1)*sNy
#ifdef ALLOW_EXCH2
          IF ( useExch2ioLayOut ) THEN
            tN = W2_myTileList(bi,bj)
c           tNx = exch2_tNx(tN)
c           tNy = exch2_tNy(tN)
c           global_nTx = exch2_global_Nx/tNx
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

           DO k=kLo,kHi
            DO j=1,tNy
             irec = 1 + ( tBx + (j-1)*iGjLoc )/tNx
     &                + ( tBy + (j-1)*jGjLoc )*global_nTx
     &            +( k-kLo + (irecord-1)*nNz )*global_nTx*ySize
             i1 = bBij + 1 + (j-1)*sNx + (k-kLo)*sNx*sNy
             i2 = bBij +         j*sNx + (k-kLo)*sNx*sNy
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
     &            pfName(1:pIL),'.',iG,'.',jG,'.data'
           length_of_rec = MDS_RECLEN( filePrec, sNx*sNy*nNz, myThid )
           IF (irecord .EQ. 1) THEN
            OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &            access='direct', recl=length_of_rec )
           ELSE
            OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &            access='direct', recl=length_of_rec )
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

C here We close the tiled MDS file
           IF ( fileIsOpen ) THEN
             CLOSE( dUnit )
             fileIsOpen = .FALSE.
           ENDIF

C--- End Global File / tile-file cases
          ENDIF

C Create meta-file for each tile if we are tiling
          IF ( .NOT.globalFile .AND. writeMetaF ) THEN
           iG=bi+(myXGlobalLo-1)/sNx
           jG=bj+(myYGlobalLo-1)/sNy
           WRITE(metaFname,'(2A,I3.3,A,I3.3,A)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.meta'
           dimList(1,1) = xSize
           dimList(2,1) = tBx + 1
           dimList(3,1) = tBx + tNx
           dimList(1,2) = ySize
           dimList(2,2) = tBy + 1
           dimList(3,2) = tBy + tNy
           dimList(1,3) = nNz
           dimList(2,3) = 1
           dimList(3,3) = nNz
c          dimList(1,3) = kSize
c          dimList(2,3) = kLo
c          dimList(3,3) = kHi
           nDims = 3
           IF ( nNz.EQ.1 ) nDims = 2
           map2gl(1) = iGjLoc
           map2gl(2) = jGjLoc
           CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, blank8c,
     I              0, dummyRL, oneRL, irecord, myIter, myThid )
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

C Make other threads wait for I/O completion so that after this,
C  3-D buffer can again be modified by any thread
c      CALL BAR2( myThid )

C     if useSingleCpuIO / else / end
      ENDIF

C Create meta-file for the global-file (also if useSingleCpuIO)
      IF ( writeMetaF .AND. iAmDoingIO .AND.
     &    (globalFile .OR. useSingleCpuIO) ) THEN
         WRITE(metaFName,'(2A)') fName(1:IL),'.meta'
         dimList(1,1) = xSize
         dimList(2,1) = 1
         dimList(3,1) = xSize
         dimList(1,2) = ySize
         dimList(2,2) = 1
         dimList(3,2) = ySize
         dimList(1,3) = nNz
         dimList(2,3) = 1
         dimList(3,3) = nNz
c        dimList(1,3) = kSize
c        dimList(2,3) = kLo
c        dimList(3,3) = kHi
         nDims = 3
         IF ( nNz.EQ.1 ) nDims = 2
         map2gl(1) = 0
         map2gl(2) = 1
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, ' ',
     I              filePrec, nDims, dimList, map2gl, 0, blank8c,
     I              0, dummyRL, oneRL, irecord, myIter, myThid )
c    I              metaFName, dataFName, the_run_name, titleLine,
c    I              filePrec, nDims, dimList, map2gl, nFlds,  fldList,
c    I              nTimRec, timList, misVal, irecord, myIter, myThid )
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      RETURN
      END
