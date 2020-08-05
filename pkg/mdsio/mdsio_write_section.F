#include "MDSIO_OPTIONS.h"

C--  File mdsio_read_section.F: Routines to handle mid-level I/O interface.
C--   Contents
C--   o MDS_WRITE_SEC_XZ
C--   o MDS_WRITE_SEC_YZ

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: MDS_WRITE_SEC_XZ
C !INTERFACE:
      SUBROUTINE MDS_WRITE_SEC_XZ(
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
C MDS_WRITE_SEC_XZ creates either a file of the form "fName.data" and
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
C Modified: 06/02/00 spk@ocean.mit.edu
C Modified: 08/03/20 - style copied from mdsio_write_field timsmith204@utexas.edu 
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
      INTEGER dimList(3,2), nDims, map2gl(2)
      INTEGER length_of_rec
      INTEGER tNx, global_nTx
      INTEGER tBx, iGjLoc, jGjLoc
      REAL*4 r4seg(sNx)
      REAL*8 r8seg(sNx)
      REAL*4 x_layer_buffer_r4(Nx)
      REAL*8 x_layer_buffer_r8(Nx)
#ifdef ALLOW_EXCH2
      INTEGER tN
      REAL*4 x_layer_buffer_exch2_r4(exch2_global_Nx)
      REAL*8 x_layer_buffer_exch2_r8(exch2_global_Nx)
#endif /* ALLOW_EXCH2 */
      _RL dummyRL(1)
      CHARACTER*8 blank8c

      DATA dummyRL(1) / 0. _d 0 /
      DATA blank8c / '        ' /

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C Set dimensions:
      xSize = Nx
      ySize = 1
      useExch2ioLayOut = .FALSE.
#ifdef ALLOW_EXCH2
      IF ( W2_useE2ioLayOut ) THEN
        xSize = exch2_global_Nx
        ySize = 1
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
        WRITE(msgBuf,'(2A,2(I4,A))') 'MDS_WRITE_SEC_XZ: ',
     &   'Too long (IL=',IL,') file name:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(errorMessageUnit,'(3A)')'file: >',fName(1:IL),'<'
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_XZ'
      ELSEIF ( (90+IL+pIL).GT.MAX_LEN_MBUF ) THEN
        WRITE(msgBuf,'(2A,2(I4,A))') 'MDS_WRITE_SEC_XZ: ',
     &   'Too long (pIL=',pIL,', IL=',IL,') pfix + file name:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(errorMessageUnit,'(3A)')'pfix: >',mdsioLocalDir(1:pIL),'<'
        WRITE(errorMessageUnit,'(3A)')'file: >',fName(1:IL),'<'
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_XZ'
      ENDIF
C Record number must be >= 1
      IF (irecord .LT. 1) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_WRITE_SEC_XZ: file="', fName(1:IL), '" , iter=', myIter
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,I9.8)')
     &    ' MDS_WRITE_SEC_XZ: argument irecord = ',irecord
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_WRITE_SEC_XZ: invalid value for irecord'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_XZ'
      ENDIF
C check for valid sub-set of levels:
      IF ( kLo.LT.1 .OR. kHi.GT.kSize ) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_WRITE_SEC_XZ: file="', fName(1:IL), '" , iter=', myIter
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(3(A,I6))')
     &    ' MDS_WRITE_SEC_XZ: arguments kSize=', kSize,
     &    ' , kLo=', kLo, ' , kHi=', kHi
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_WRITE_SEC_XZ: invalid sub-set of levels'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_XZ'
      ENDIF
C check for 3-D Buffer size:
      IF ( .NOT.useSingleCpuIO .AND. nNz.GT.size3dBuf ) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_WRITE_SEC_XZ: file="', fName(1:IL), '" , iter=', myIter
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(3(A,I6))')
     &    ' MDS_WRITE_SEC_XZ: Nb Lev to write =', nNz,
     &    ' >', size3dBuf, ' = buffer 3rd Dim'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_WRITE_SEC_XZ: buffer 3rd Dim. too small'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &    ' increase "size3dBuf" in "MDSIO_BUFF_3D.h" and recompile'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid)
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_XZ'
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
     &      ' MDS_WRITE_SEC_XZ: it,rec,kS,kL,kH=', myIter, jrecord,
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
         length_of_rec = MDS_RECLEN( filePrec, sNx, myThid )
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
C-     copy from fldRL/RS(level=k) to 2-D "local":
       IF ( filePrec.EQ.precFloat32 ) THEN
         DO bj=1,nSy
         DO bi=1,nSx
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_SEG4toRS_2D( sNx, oLx, kSize, bi, bj, k,
     &                            .FALSE., r4seg, fldRS )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_SEG4toRS_2D( sNx, oLx, kSize, bi, bj, k,
     &                            .FALSE., r4seg, fldRL )
          ELSE
            WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITE_SEC_XZ: illegal value for arrType=', arrType
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_XZ'
          ENDIF
         ENDDO
         ENDDO
C Wait for all threads to finish filling shared buffer
          CALL BAR2( myThid )
#ifdef ALLOW_EXCH2
          IF ( .not.W2_useE2ioLayOut ) THEN
#endif
            CALL GATHER_VEC_R4( 
     O                         x_layer_buffer_r4,
     I                         r4seg, 
     I                         sNx, myThid ) 
#ifdef ALLOW_EXCH2
          ELSE

            CALL GATHER_VEC_R4( 
     O                         x_layer_buffer_exch2_r4,
     I                         r4seg, 
     I                         sNx, myThid ) 
          ENDIF
#endif

        ELSEIF ( filePrec.EQ.precFloat64 ) THEN
         DO bj=1,nSy
         DO bi=1,nSx
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_SEG8toRS_2D( sNx, oLx, kSize, bi, bj, k,
     I                         .FALSE., r8seg, fldRS )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_SEG8toRS_2D( sNx, oLx, kSize, bi, bj, k,
     I                         .FALSE., r8seg, fldRL )
          ELSE
            WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITE_SEC_XZ: illegal value for arrType=', arrType
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_XZ'
          ENDIF
         ENDDO
         ENDDO
C Wait for all threads to finish filling shared buffer
          CALL BAR2( myThid )
#ifdef ALLOW_EXCH2
          IF ( .not.W2_useE2ioLayOut ) THEN
#endif
            CALL GATHER_VEC_R8( 
     O                         x_layer_buffer_r8,
     I                         r8seg, 
     I                         sNx, myThid ) 
#ifdef ALLOW_EXCH2
          ELSE

            CALL GATHER_VEC_R8( 
     O                          x_layer_buffer_exch2_r8,
     I                          r8seg, 
     I                          sNx, myThid ) 
          ENDIF
#endif
        ELSE
          WRITE(msgBuf,'(A,I6)')
     &      ' MDS_WRITE_SEC_XZ: illegal value for filePrec=',filePrec
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_XZ'
        ENDIF
C Make other threads wait for "gather" completion so that after this,
C  shared buffer can again be modified by any thread
        CALL BAR2( myThid )

        IF ( iAmDoingIO ) THEN
          irec = 1 + k-kLo + (irecord-1)*kSize
          IF ( filePrec.EQ.precFloat32 ) THEN

#ifdef ALLOW_EXCH2
           IF ( .not.W2_useE2ioLayOut ) THEN
#endif
# ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR4( xSize, x_layer_buffer_r4 )
# endif
            WRITE(dUnit,rec=irec) x_layer_buffer_r4
#ifdef ALLOW_EXCH2
           ELSE
# ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR4( xSize, x_layer_buffer_exch2_r4 )
# endif
            WRITE(dUnit,rec=irec) x_layer_buffer_exch2_r4
           ENDIF
#endif
          ELSE

#ifdef ALLOW_EXCH2
           IF ( .not.W2_useE2ioLayOut ) THEN
#endif
# ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR8( xSize, x_layer_buffer_r8 )
# endif
            WRITE(dUnit,rec=irec) x_layer_buffer_r8
#ifdef ALLOW_EXCH2
           ELSE
# ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR8( xSize, x_layer_buffer_exch2_r8 )
# endif
            WRITE(dUnit,rec=irec) x_layer_buffer_exch2_r8
           ENDIF
#endif
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
       DO k=kLo,kHi
        zeroBuff = k.EQ.kLo
        DO bj=1,nSy
        DO bi=1,nSx
C-      copy from fldRL/RS(level=k) to 2-D "local":
        IF ( filePrec.EQ.precFloat32 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_SEG4toRS_2D( sNx, oLx, kSize, bi, bj, k,
     &                            .FALSE., r4seg, fldRS )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_SEG4toRS_2D( sNx, oLx, kSize, bi, bj, k,
     &                            .FALSE., r4seg, fldRL )
          ELSE
            WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITE_SEC_XZ: illegal value for arrType=', arrType
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_XZ'
          ENDIF

        ELSEIF ( filePrec.EQ.precFloat64 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_SEG8toRS_2D( sNx, oLx, kSize, bi, bj, k,
     I                         .FALSE., r8seg, fldRS )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_SEG8toRS_2D( sNx, oLx, kSize, bi, bj, k,
     I                         .FALSE., r8seg, fldRL )
          ELSE
            WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITE_SEC_XZ: illegal value for arrType=', arrType
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_XZ'
          ENDIF

        ELSE
          WRITE(msgBuf,'(A,I6)')
     &      ' MDS_WRITE_SEC_XZ: illegal value for filePrec=',filePrec
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_XZ'
        ENDIF
        ENDDO
        ENDDO
C-     end of k loop
       ENDDO

C Wait for all threads to finish filling shared buffer
       CALL BAR2( myThid )

C Only do I/O if I am the master thread
       IF ( iAmDoingIO ) THEN

#ifdef _BYTESWAPIO
        IF ( filePrec.EQ.precFloat32 ) THEN
          CALL MDS_BYTESWAPR4( sNx, r4seg )
        ELSE
          CALL MDS_BYTESWAPR8( sNx, r8seg )
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

          tNx = sNx
          global_nTx = xSize/sNx
          tBx = myXGlobalLo-1 + (bi-1)*sNx
#ifdef ALLOW_EXCH2
          IF ( useExch2ioLayOut ) THEN
            tN = W2_myTileList(bi,bj)
c           tNx = exch2_tNx(tN)
c           tNy = exch2_tNy(tN)
c           global_nTx = exch2_global_Nx/tNx
            tBx = exch2_txGlobalo(tN) - 1
            IF   ( exch2_mydNx(tN) .GT. xSize ) THEN
C-          face x-size larger than glob-size : fold it
              iGjLoc = 0
              jGjLoc = exch2_mydNx(tN) / xSize
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
            irec = 1 + tBx/tNx
     &           +( k-kLo + (irecord-1)*kSize )*global_nTx
            IF ( filePrec.EQ.precFloat32 ) THEN
             WRITE(dUnit,rec=irec) r4seg
            ELSE
             WRITE(dUnit,rec=irec) r8seg
            ENDIF
C End of k loop
           ENDDO

          ELSE
C--- Case of 1 file per tile (globalFile=F):

C If we are writing to a tiled MDS file then we open each one here
           iG=bi+(myXGlobalLo-1)/sNx
           jG=bj+(myYGlobalLo-1)/sNy
           WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &            pfName(1:pIL),'.',iG,'.',jG,'.data'
           length_of_rec = MDS_RECLEN( filePrec, sNx, myThid )
           IF (irecord .EQ. 1) THEN
            OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &            access='direct', recl=length_of_rec )
           ELSE
            OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &            access='direct', recl=length_of_rec )
           ENDIF
           fileIsOpen=.TRUE.

           DO k=kLo,kHi
            irec = k+kSize*(irecord-1)
            IF ( filePrec.EQ.precFloat32 ) THEN
              WRITE(dUnit,rec=irec) r4seg
            ELSE
              WRITE(dUnit,rec=irec) r8seg
            ENDIF
C End of k loop
           ENDDO

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
           dimList(1,2) = nNz
           dimList(2,2) = 1
           dimList(3,2) = nNz
           nDims = 2
           IF ( nNz.EQ.1 ) nDims = 1
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
         dimList(1,2) = nNz
         dimList(2,2) = 1
         dimList(3,2) = nNz
         nDims = 2
         IF ( nNz.EQ.1 ) nDims = 1
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: MDS_WRITE_SEC_YZ
C !INTERFACE:
      SUBROUTINE MDS_WRITE_SEC_YZ(
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
C MDS_WRITE_SEC_YZ creates either a file of the form "fName.data" and
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
C Modified: 06/02/00 spk@ocean.mit.edu
C Modified: 08/03/20 - style copied from mdsio_write_field timsmith204@utexas.edu 
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
      INTEGER i1,i2,i,j,k,nNz,np
      INTEGER irec,dUnit,IL,pIL
      INTEGER dimList(3,2), nDims, map2gl(2)
      INTEGER length_of_rec
      INTEGER bBij
      INTEGER tNy, global_nTy
      INTEGER tBy, iGjLoc, jGjLoc
      _R4 r4seg(sNy,nSx,nSy)
      _R8 r8seg(sNy,nSx,nSy)
      _R4 y_layer_buffer_r4( Ny )
      _R8 y_layer_buffer_r8( Ny )
      _R4 yz_buffer_r4( sNy*Nr*nSx*nSy )
      _R8 yz_buffer_r8( sNy*Nr*nSx*nSy )
#ifdef ALLOW_EXCH2
      INTEGER tN
      REAL*4 y_layer_buffer_exch2_r4(exch2_global_Ny)
      REAL*8 y_layer_buffer_exch2_r8(exch2_global_Ny)
#endif /* ALLOW_EXCH2 */
      _RL dummyRL(1)
      CHARACTER*8 blank8c

      DATA dummyRL(1) / 0. _d 0 /
      DATA blank8c / '        ' /

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C Set dimensions:
      xSize = 1
      ySize = Ny
      useExch2ioLayOut = .FALSE.
#ifdef ALLOW_EXCH2
      IF ( W2_useE2ioLayOut ) THEN
        xSize = 1
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
        WRITE(msgBuf,'(2A,2(I4,A))') 'MDS_WRITE_SEC_YZ: ',
     &   'Too long (IL=',IL,') file name:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(errorMessageUnit,'(3A)')'file: >',fName(1:IL),'<'
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_YZ'
      ELSEIF ( (90+IL+pIL).GT.MAX_LEN_MBUF ) THEN
        WRITE(msgBuf,'(2A,2(I4,A))') 'MDS_WRITE_SEC_YZ: ',
     &   'Too long (pIL=',pIL,', IL=',IL,') pfix + file name:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(errorMessageUnit,'(3A)')'pfix: >',mdsioLocalDir(1:pIL),'<'
        WRITE(errorMessageUnit,'(3A)')'file: >',fName(1:IL),'<'
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_YZ'
      ENDIF
C Record number must be >= 1
      IF (irecord .LT. 1) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_WRITE_SEC_YZ: file="', fName(1:IL), '" , iter=', myIter
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,I9.8)')
     &    ' MDS_WRITE_SEC_YZ: argument irecord = ',irecord
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_WRITE_SEC_YZ: invalid value for irecord'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_YZ'
      ENDIF
C check for valid sub-set of levels:
      IF ( kLo.LT.1 .OR. kHi.GT.kSize ) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_WRITE_SEC_YZ: file="', fName(1:IL), '" , iter=', myIter
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(3(A,I6))')
     &    ' MDS_WRITE_SEC_YZ: arguments kSize=', kSize,
     &    ' , kLo=', kLo, ' , kHi=', kHi
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_WRITE_SEC_YZ: invalid sub-set of levels'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_YZ'
      ENDIF
C check for 3-D Buffer size:
      IF ( .NOT.useSingleCpuIO .AND. nNz.GT.size3dBuf ) THEN
        WRITE(msgBuf,'(3A,I10)')
     &    ' MDS_WRITE_SEC_YZ: file="', fName(1:IL), '" , iter=', myIter
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(3(A,I6))')
     &    ' MDS_WRITE_SEC_YZ: Nb Lev to write =', nNz,
     &    ' >', size3dBuf, ' = buffer 3rd Dim'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)')
     &    ' MDS_WRITE_SEC_YZ: buffer 3rd Dim. too small'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &    ' increase "size3dBuf" in "MDSIO_BUFF_3D.h" and recompile'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid)
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_YZ'
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
     &      ' MDS_WRITE_SEC_YZ: it,rec,kS,kL,kH=', myIter, jrecord,
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
         length_of_rec = MDS_RECLEN( filePrec, ySize, myThid )
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
C Initialize buffer
       DO j = 1,ySize
        y_layer_buffer_r4(j)=0.
        y_layer_buffer_r8(j)=0.
       ENDDO
       zeroBuff = k.EQ.kLo
C-     copy from fldRL/RS(level=k) to 2-D "local":
       IF ( filePrec.EQ.precFloat32 ) THEN
         IF ( arrType.EQ.'RS' ) THEN
           CALL MDS_PASS_YZ_R4toRS( r4seg, fldRS, 
     I             0, 1, k, kSize, 0, 0, .FALSE., myThid )
         ELSEIF ( arrType.EQ.'RL' ) THEN
           CALL MDS_PASS_YZ_R4toRL( r4seg, fldRL, 
     I             0, 1, k, kSize, 0, 0, .FALSE., myThid )
         ELSE
           WRITE(msgBuf,'(2A)')
     &     ' MDS_WRITE_SEC_YZ: illegal value for arrType=', arrType
           CALL PRINT_ERROR( msgBuf, myThid )
           CALL ALL_PROC_DIE( myThid )
           STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_YZ'
         ENDIF
C Wait for all threads to finish filling shared buffer
          CALL BAR2( myThid )
#ifdef ALLOW_EXCH2
          IF ( .not.W2_useE2ioLayOut ) THEN
#endif
            CALL GATHER_YZ_R4(
     O                        y_layer_buffer_r4,
     I                        r4seg,
     I                        ySize,
     I                        useExch2ioLayout,
     I                        zeroBuff,
     I                        myThid )
#ifdef ALLOW_EXCH2
          ELSE
            CALL GATHER_YZ_R4(
     O                        y_layer_buffer_exch2_r4,
     I                        r4seg,
     I                        ySize,
     I                        useExch2ioLayout,
     I                        zeroBuff,
     I                        myThid )
          ENDIF
#endif

       ELSEIF ( filePrec.EQ.precFloat64 ) THEN
         IF ( arrType.EQ.'RS' ) THEN
           CALL MDS_PASS_YZ_R8toRS( r8seg, fldRS,
     I            0, 1, k, kSize, 0, 0, .FALSE., myThid )
         ELSEIF ( arrType.EQ.'RL' ) THEN
           CALL MDS_PASS_YZ_R8toRL( r8seg, fldRL,
     I            0, 1, k, kSize, 0, 0, .FALSE., myThid )
         ELSE
           WRITE(msgBuf,'(2A)')
     &     ' MDS_WRITE_SEC_YZ: illegal value for arrType=', arrType
           CALL PRINT_ERROR( msgBuf, myThid )
           CALL ALL_PROC_DIE( myThid )
           STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_YZ'
         ENDIF
         IF ( debugLevel .GE. debLevC ) THEN
           WRITE(msgBuf,'(A,I8)')
     &       ' MDS_WRITE_SEC_YZ: SIZE(ylayerbuffer_r8): ', 
     &          SIZE(y_layer_buffer_r8)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
         ENDIF
C Wait for all threads to finish filling shared buffer
         CALL BAR2( myThid )
#ifdef ALLOW_EXCH2
          IF ( .not.W2_useE2ioLayOut ) THEN
#endif
            CALL GATHER_YZ_R8(
     O                        y_layer_buffer_r8,
     I                        r8seg,
     I                        ySize,
     I                        useExch2ioLayout,
     I                        zeroBuff,
     I                        myThid )
#ifdef ALLOW_EXCH2
          ELSE
            CALL GATHER_YZ_R8(
     O                        y_layer_buffer_exch2_r8,
     I                        r8seg,
     I                        ySize,
     I                        useExch2ioLayout,
     I                        zeroBuff,
     I                        myThid )
          ENDIF
#endif
       ELSE
         WRITE(msgBuf,'(A,I6)')
     &     ' MDS_WRITE_SEC_YZ: illegal value for filePrec=',filePrec
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( myThid )
         STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_YZ'
       ENDIF
C Make other threads wait for "gather" completion so that after this,
C  shared buffer can again be modified by any thread
        CALL BAR2( myThid )

       IF ( iAmDoingIO ) THEN
         irec = 1 + (k-kLo) + (irecord-1)*nNz
         IF ( debugLevel .GE. debLevC ) THEN
           WRITE(msgBuf,'(A,I8,A,I8)')
     &       ' MDS_WRITE_SEC_YZ: irec: ', irec,
     &       ' SIZE(ylayerbuffer_r8): ', SIZE(y_layer_buffer_r8)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
         ENDIF
         IF ( filePrec.EQ.precFloat32 ) THEN
#ifdef ALLOW_EXCH2
           IF ( .not.W2_useE2ioLayOut ) THEN
#endif
# ifdef _BYTESWAPIO
             CALL MDS_BYTESWAPR4( ySize, y_layer_buffer_r4 )
# endif
             WRITE(dUnit,rec=irec) y_layer_buffer_r4
#ifdef ALLOW_EXCH2
           ELSE
# ifdef _BYTESWAPIO
             CALL MDS_BYTESWAPR4( ySize, y_layer_buffer_exch2_r4 )
# endif
             WRITE(dUnit,rec=irec) y_layer_buffer_exch2_r4
           ENDIF
#endif
         ELSE

#ifdef ALLOW_EXCH2
           IF ( .not.W2_useE2ioLayOut ) THEN
#endif
# ifdef _BYTESWAPIO
             CALL MDS_BYTESWAPR8( ySize, y_layer_buffer_r8 )
# endif
             WRITE(dUnit,rec=irec) y_layer_buffer_r8
#ifdef ALLOW_EXCH2
           ELSE
# ifdef _BYTESWAPIO
             CALL MDS_BYTESWAPR8( ySize, y_layer_buffer_exch2_r8 )
# endif
             WRITE(dUnit,rec=irec) y_layer_buffer_exch2_r8
           ENDIF
#endif
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

C---    Copy from fldRL/RS to 2-D buffer (multi-threads):
        IF ( filePrec.EQ.precFloat32 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_YZ_R4toRS( yz_buffer_r4, fldRS,
     I              0, nNz, kLo, kSize, 0, 0, .FALSE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_YZ_R4toRL( yz_buffer_r4, fldRL,
     I              0, nNz, kLo, kSize, 0, 0, .FALSE., myThid )
          ELSE
            WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITE_SEC_YZ: illegal value for arrType=', arrType
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_YZ'
          ENDIF

        ELSEIF ( filePrec.EQ.precFloat64 ) THEN
          IF ( arrType.EQ.'RS' ) THEN
            CALL MDS_PASS_YZ_R8toRS( yz_buffer_r8, fldRS,
     I              0, nNz, kLo, kSize, 0, 0, .FALSE., myThid )
          ELSEIF ( arrType.EQ.'RL' ) THEN
            CALL MDS_PASS_YZ_R8toRL( yz_buffer_r8, fldRL,
     I              0, nNz, kLo, kSize, 0, 0, .FALSE., myThid )
          ELSE
            WRITE(msgBuf,'(2A)')
     &      ' MDS_WRITE_SEC_YZ: illegal value for arrType=', arrType
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_YZ'
          ENDIF

        ELSE
          WRITE(msgBuf,'(A,I6)')
     &      ' MDS_WRITE_SEC_YZ: illegal value for filePrec=',filePrec
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R MDS_WRITE_SEC_YZ'
        ENDIF

C Wait for all threads to finish filling shared buffer
       CALL BAR2( myThid )

C Only do I/O if I am the master thread
       IF ( iAmDoingIO ) THEN

#ifdef _BYTESWAPIO
        IF ( filePrec.EQ.precFloat32 ) THEN
          CALL MDS_BYTESWAPR4( sNy*nNz*nSx, yz_buffer_r4 )
        ELSE
          CALL MDS_BYTESWAPR8( sNy*nNz*nSx, yz_buffer_r8 )
        ENDIF
#endif

C If we are writing to a global file then we open it here
        IF (globalFile) THEN
          WRITE(dataFName,'(2a)') fName(1:IL),'.data'
          length_of_rec = MDS_RECLEN( filePrec, sNy, myThid )
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
          bBij = sNy*nNz*( bi-1 + (bj-1)*nSx )

          tNy = sNy
          global_nTy = ySize/sNy
          tBy = myYGlobalLo-1 + (bj-1)*sNy
#ifdef ALLOW_EXCH2
          IF ( useExch2ioLayOut ) THEN
            tN = W2_myTileList(bi,bj)
c           tNx = exch2_tNx(tN)
c           tNy = exch2_tNy(tN)
c           global_nTx = exch2_global_Nx/tNx
            tBy = exch2_tyGlobalo(tN) - 1
            IF   ( exch2_mydNy(tN) .GT. ySize ) THEN
C-          face x-size larger than glob-size : fold it
              iGjLoc = 0
              jGjLoc = exch2_mydNy(tN) / ySize
            ELSE
C-          default (face fit into global-IO-array)
              iGjLoc = 0
              jGjLoc = 1
            ENDIF
          ENDIF
#endif /* ALLOW_EXCH2 */

          IF (globalFile) THEN
C--- Case of 1 Global file:

Cts --- TODO: check this!
           DO k=kLo,kHi
            irec = 1 + tBy/tNy
     &           +( k-kLo + (irecord-1)*kSize )*global_nTy
            i1 = bBij + 1 + (k-kLo)*sNx*sNy
            i2 = bBij +           k*sNx*sNy
            IF ( filePrec.EQ.precFloat32 ) THEN
             WRITE(dUnit,rec=irec) (yz_buffer_r4(i),i=i1,i2)
            ELSE
             WRITE(dUnit,rec=irec) (yz_buffer_r8(i),i=i1,i2)
            ENDIF
C End of k loop
           ENDDO

          ELSE
C--- Case of 1 file per tile (globalFile=F):

C If we are writing to a tiled MDS file then we open each one here
           iG=bi+(myXGlobalLo-1)/sNx
           jG=bj+(myYGlobalLo-1)/sNy
           WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &            pfName(1:pIL),'.',iG,'.',jG,'.data'
           length_of_rec = MDS_RECLEN( filePrec, sNy*nNz, myThid )
           IF (irecord .EQ. 1) THEN
            OPEN( dUnit, file=dataFName, status=_NEW_STATUS,
     &            access='direct', recl=length_of_rec )
           ELSE
            OPEN( dUnit, file=dataFName, status=_OLD_STATUS,
     &            access='direct', recl=length_of_rec )
           ENDIF
           fileIsOpen=.TRUE.

           irec = irecord
           IF ( filePrec.EQ.precFloat32 ) THEN
             WRITE(dUnit,rec=irec) yz_buffer_r4
           ELSE
             WRITE(dUnit,rec=irec) yz_buffer_r8
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
           dimList(1,1) = Ny
           dimList(2,1) = tBy + 1
           dimList(3,1) = tBy + tNy
           dimList(1,2) = nNz
           dimList(2,2) = 1
           dimList(3,2) = nNz
           nDims = 2
           IF ( nNz.EQ.1 ) nDims = 1
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
         dimList(1,1) = Ny
         dimList(2,1) = 1
         dimList(3,1) = Ny
         ! dimList(1,2) = nPx
         ! dimList(2,2) = 1
         ! dimList(3,2) = nPx
         dimList(1,2) = nNz
         dimList(2,2) = 1
         dimList(3,2) = nNz
         nDims = 2
         IF ( nNz.EQ.1 ) nDims = 1
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
