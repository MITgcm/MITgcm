#include "MDSIO_OPTIONS.h"

C--  File mdsio_read_section.F: Routines to handle mid-level I/O interface.
C--   Contents
C--   o MDS_READ_SEC_XZ
C--   o MDS_READ_SEC_YZ

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: MDS_READ_SEC_XZ
C !INTERFACE:
      SUBROUTINE MDS_READ_SEC_XZ(
     I   fName,
     I   filePrec,
     I   useCurrentDir,
     I   arrType,
     I   kSize,
     O   fldRL, fldRS,
     I   irecord,
     I   myThid )

C !DESCRIPTION
C Arguments:
C
C fName       string  :: base name for file to read
C filePrec    integer :: number of bits per word in file (32 or 64)
C useCurrentDir(logic):: always read from the current directory (even if
C                        "mdsioLocalDir" is set)
C arrType     char(2) :: which array (fldRL/RS) to read into, either "RL" or "RS"
C kSize       integer :: size of third dimension, normally either 1 or Nr
C fldRL         RL    :: array to read into if arrType="RL", fldRL(:,kSize,:,:)
C fldRS         RS    :: array to read into if arrType="RS", fldRS(:,kSize,:,:)
C irecord     integer :: record number to read
C myThid      integer :: thread identifier
C
C MDS_READ_SEC_XZ first checks to see IF the file "fName" exists, then
C if the file "fName.data" exists and finally the tiled files of the
C form "fName.xxx.yyy.data" exist.
C The precision of the file is decsribed by filePrec, set either
C  to floatPrec32 or floatPrec64. The char*(2) string arrType, either "RL"
C  or "RS", selects which array is filled in, either fldRL or fldRS.
C This routine reads vertical slices (X-Z) including the overlap region.
C irecord is the record number to be read and must be >= 1.
C The file data is stored in fldRL/RS *but* the overlaps are *not* updated.
C
C Created: 06/03/00 spk@ocean.mit.edu
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

C !INPUT PARAMETERS:
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL useCurrentDir
      CHARACTER*(2) arrType
      INTEGER kSize
      INTEGER irecord
      INTEGER myThid
C !OUTPUT PARAMETERS:
      _RL  fldRL(*)
      _RS  fldRS(*)

C !FUNCTIONS:
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL ILNBLNK, MDS_RECLEN

C !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) dataFName,pfName
      INTEGER iG,jG,irec,bi,bj,k,dUnit,IL,pIL
      LOGICAL exst
      Real*4 r4seg(sNx)
      Real*8 r8seg(sNx)
      LOGICAL globalFile,fileIsOpen
      INTEGER length_of_rec
      CHARACTER*(max_len_mbuf) msgBuf
#ifdef ALLOW_EXCH2
      INTEGER tGx,tNx,tN
#endif /* ALLOW_EXCH2 */
C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      IF (irecord .LT. 1) THEN
       WRITE(msgBuf,'(A,I9.8)')
     &   ' MDS_READ_SEC_XZ: argument irecord = ',irecord
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &   ' MDS_READ_SEC_XZ: Invalid value for irecord'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R MDS_READ_SEC_XZ'
      ENDIF

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

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
     &   ' MDS_READ_SEC_XZ: opening global file: ',dataFName(1:IL)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
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
     &    ' MDS_READ_SEC_XZ: opening global file: ',dataFName(1:IL+5)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
        globalFile = .TRUE.
       ENDIF
      ENDIF

C If we are reading from a global file then we open it here
      IF (globalFile) THEN
       length_of_rec = MDS_RECLEN( filePrec, sNx, myThid )
       OPEN( dUnit, file=dataFName, status='old', _READONLY_ACTION
     &       access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      ENDIF

C Loop over all tiles
      DO bj=1,nSy
       DO bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        IF (.NOT. globalFile) THEN
         iG=bi+(myXGlobalLo-1)/sNx
         jG=bj+(myYGlobalLo-1)/sNy
         WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         INQUIRE( file=dataFName, exist=exst )
C Of course, we only open the file IF the tile is "active"
C (This is a place-holder for the active/passive mechanism
         IF (exst) THEN
          IF ( debugLevel .GE. debLevB ) THEN
           WRITE(msgBuf,'(A,A)')
     &      ' MDS_READ_SEC_XZ: opening file: ',dataFName(1:pIL+13)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
          length_of_rec = MDS_RECLEN( filePrec, sNx, myThid )
          OPEN( dUnit, file=dataFName, status='old', _READONLY_ACTION
     &          access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         ELSE
          fileIsOpen=.FALSE.
          WRITE(msgBuf,'(4A)') ' MDS_READ_SEC_XZ: filename: ',
     &             fName(1:IL),' , ', dataFName(1:pIL+13)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A)')
     &      ' MDS_READ_SEC_XZ: Files DO not exist'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_SEC_XZ'
         ENDIF
        ENDIF

        IF (fileIsOpen) THEN
#ifdef ALLOW_EXCH2
C layout of global x-z section files is "xStack"
         tN = W2_myTileList(bi,bj)
         tGx = exch2_txXStackLo(tN)
         tNx = exch2_tNx(tN)
#endif /* ALLOW_EXCH2 */
         DO k=1,kSize
           IF (globalFile) THEN
#ifdef ALLOW_EXCH2
C record length is sNx==tNx
            irec = 1 + ( tGx-1
     &                   + ( k-1 + (irecord-1)*kSize )*exch2_xStack_Nx
     &                 )/tNx
#else /* ALLOW_EXCH2 */
            iG = myXGlobalLo-1 + (bi-1)*sNx
            jG = (myYGlobalLo-1)/sNy + (bj-1)
            irec=1 + INT(iG/sNx) + nSx*nPx*(k-1)
     &           + nSx*nPx*kSize*(irecord-1)
#endif /* ALLOW_EXCH2 */
           ELSE
            iG = 0
            jG = 0
            irec=k + kSize*(irecord-1)
           ENDIF
           IF (filePrec .EQ. precFloat32) THEN
            READ(dUnit,rec=irec) r4seg
#ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR4(sNx,r4seg)
#endif
            IF (arrType .EQ. 'RS') THEN
             CALL MDS_SEG4toRS_2D( sNx,oLx,kSize,bi,bj,k,.TRUE.,
     &                             r4seg,fldRS )
            ELSEIF (arrType .EQ. 'RL') THEN
             CALL MDS_SEG4toRL_2D( sNx,oLx,kSize,bi,bj,k,.TRUE.,
     &                             r4seg,fldRL )
            ELSE
             WRITE(msgBuf,'(A)')
     &         ' MDS_READ_SEC_XZ: illegal value for arrType'
             CALL PRINT_ERROR( msgBuf, myThid )
             STOP 'ABNORMAL END: S/R MDS_READ_SEC_XZ'
            ENDIF
           ELSEIF (filePrec .EQ. precFloat64) THEN
            READ(dUnit,rec=irec) r8seg
#ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR8( sNx, r8seg )
#endif
            IF (arrType .EQ. 'RS') THEN
             CALL MDS_SEG8toRS_2D(sNx,oLx,kSize,bi,bj,k,.TRUE.,
     &                             r8seg,fldRS )
            ELSEIF (arrType .EQ. 'RL') THEN
             CALL MDS_SEG8toRL_2D(sNx,oLx,kSize,bi,bj,k,.TRUE.,
     &                             r8seg,fldRL )
            ELSE
             WRITE(msgBuf,'(A)')
     &         ' MDS_READ_SEC_XZ: illegal value for arrType'
             CALL PRINT_ERROR( msgBuf, myThid )
             STOP 'ABNORMAL END: S/R MDS_READ_SEC_XZ'
            ENDIF
           ELSE
            WRITE(msgBuf,'(A)')
     &        ' MDS_READ_SEC_XZ: illegal value for filePrec'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_SEC_XZ'
           ENDIF
C End of k loop
         ENDDO
         IF (.NOT. globalFile) THEN
          CLOSE( dUnit )
          fileIsOpen = .FALSE.
         ENDIF
        ENDIF
C End of bi,bj loops
       ENDDO
      ENDDO

C If global file was opened then close it
      IF (fileIsOpen .AND. globalFile) THEN
       CLOSE( dUnit )
       fileIsOpen = .FALSE.
      ENDIF

      _END_MASTER( myThid )

C     ------------------------------------------------------------------
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: MDS_READ_SEC_YZ
C !INTERFACE:
      SUBROUTINE MDS_READ_SEC_YZ(
     I   fName,
     I   filePrec,
     I   useCurrentDir,
     I   arrType,
     I   kSize,
     O   fldRL, fldRS,
     I   irecord,
     I   myThid )

C !DESCRIPTION
C Arguments:
C
C fName       string  :: base name for file to read
C filePrec    integer :: number of bits per word in file (32 or 64)
C useCurrentDir(logic):: always read from the current directory (even if
C                        "mdsioLocalDir" is set)
C arrType     char(2) :: which array (fldRL/RS) to read into, either "RL" or "RS"
C kSize       integer :: size of third dimension, normally either 1 or Nr
C fldRL         RL    :: array to read into if arrType="RL", fldRL(:,kSize,:,:)
C fldRS         RS    :: array to read into if arrType="RS", fldRS(:,kSize,:,:)
C irecord     integer :: record number to read
C myThid      integer :: thread identifier
C
C MDS_READ_SEC_YZ first checks to see IF the file "fName" exists, then
C if the file "fName.data" exists and finally the tiled files of the
C form "fName.xxx.yyy.data" exist.
C The precision of the file is decsribed by filePrec, set either
C  to floatPrec32 or floatPrec64. The char*(2) string arrType, either "RL"
C  or "RS", selects which array is filled in, either fldRL or fldRS.
C This routine reads vertical slices (Y-Z) including the overlap region.
C irecord is the record number to be read and must be >= 1.
C The file data is stored in fldRL/RS *but* the overlaps are *not* updated.
C
C Created: 06/03/00 spk@ocean.mit.edu
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

C !INPUT PARAMETERS:
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL useCurrentDir
      CHARACTER*(2) arrType
      INTEGER kSize
      INTEGER irecord
      INTEGER myThid
C !OUTPUT PARAMETERS:
      _RL  fldRL(*)
      _RS  fldRS(*)

C !FUNCTIONS:
      INTEGER ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL ILNBLNK, MDS_RECLEN

C !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) dataFName,pfName
      INTEGER iG,jG,irec,bi,bj,k,dUnit,IL,pIL
      LOGICAL exst
      Real*4 r4seg(sNy)
      Real*8 r8seg(sNy)
      LOGICAL globalFile,fileIsOpen
      INTEGER length_of_rec
      CHARACTER*(max_len_mbuf) msgBuf
#ifdef ALLOW_EXCH2
      INTEGER tGy,tNy,tN
#endif /* ALLOW_EXCH2 */

C     ------------------------------------------------------------------

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      IF (irecord .LT. 1) THEN
       WRITE(msgBuf,'(A,I9.8)')
     &   ' MDS_READ_SEC_YZ: argument irecord = ',irecord
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &   ' MDS_READ_SEC_YZ: Invalid value for irecord'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R MDS_READ_SEC_YZ'
      ENDIF

C Assume nothing
      globalFile = .FALSE.
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

C Assign special directory
      IF ( useCurrentDir .OR. pIL.EQ.0 ) THEN
       pfName= fName
      ELSE
       WRITE(pfName,'(2A)') mdsioLocalDir(1:pIL), fName(1:IL)
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
     &   ' MDS_READ_SEC_YZ: opening global file: ',dataFName(1:IL)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
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
     &    ' MDS_READ_SEC_YZ: opening global file: ',dataFName(1:IL+5)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
        globalFile = .TRUE.
       ENDIF
      ENDIF

C If we are reading from a global file then we open it here
      IF (globalFile) THEN
       length_of_rec = MDS_RECLEN( filePrec, sNy, myThid )
       OPEN( dUnit, file=dataFName, status='old', _READONLY_ACTION
     &       access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      ENDIF

C Loop over all tiles
      DO bj=1,nSy
       DO bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        IF (.NOT. globalFile) THEN
         iG=bi+(myXGlobalLo-1)/sNx
         jG=bj+(myYGlobalLo-1)/sNy
         WRITE(dataFName,'(2A,I3.3,A,I3.3,A)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         INQUIRE( file=dataFName, exist=exst )
C Of course, we only open the file IF the tile is "active"
C (This is a place-holder for the active/passive mechanism
         IF (exst) THEN
          IF ( debugLevel .GE. debLevB ) THEN
           WRITE(msgBuf,'(A,A)')
     &      ' MDS_READ_SEC_YZ: opening file: ',dataFName(1:pIL+13)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          ENDIF
          length_of_rec = MDS_RECLEN( filePrec, sNy, myThid )
          OPEN( dUnit, file=dataFName, status='old', _READONLY_ACTION
     &          access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         ELSE
          fileIsOpen=.FALSE.
          WRITE(msgBuf,'(4A)') ' MDS_READ_SEC_YZ: filename: ',
     &             fName(1:IL),' , ', dataFName(1:pIL+13)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A)')
     &      ' MDS_READ_SEC_YZ: Files DO not exist'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R MDS_READ_SEC_YZ'
         ENDIF
        ENDIF

        IF (fileIsOpen) THEN
#ifdef ALLOW_EXCH2
C layout of global y-z section files is "yStack"
         tN = W2_myTileList(bi,bj)
         tGy = exch2_tyYStackLo(tN)
         tNy = exch2_tNy(tN)
#endif /* ALLOW_EXCH2 */
         DO k=1,kSize
           IF (globalFile) THEN
#ifdef ALLOW_EXCH2
C record length is sNy==tNy
            irec = 1 + ( tGy-1
     &                   + ( k-1 + (irecord-1)*kSize )*exch2_yStack_Ny
     &                 )/tNy
#else /* ALLOW_EXCH2 */
            iG = (myXGlobalLo-1)/sNx + (bi-1)
            jG = myYGlobalLo-1 + (bj-1)*sNy
            irec=1 + INT(jG/sNy) + nSy*nPy*(k-1)
     &           + nSy*nPy*kSize*(irecord-1)
#endif /* ALLOW_EXCH2 */
           ELSE
            iG = 0
            jG = 0
            irec=k + kSize*(irecord-1)
           ENDIF
           IF (filePrec .EQ. precFloat32) THEN
            READ(dUnit,rec=irec) r4seg
#ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR4(sNy,r4seg)
#endif
            IF (arrType .EQ. 'RS') THEN
             CALL MDS_SEG4toRS_2D( sNy,oLy,kSize,bi,bj,k,.TRUE.,
     &                             r4seg,fldRS )
            ELSEIF (arrType .EQ. 'RL') THEN
             CALL MDS_SEG4toRL_2D( sNy,oLy,kSize,bi,bj,k,.TRUE.,
     &                             r4seg,fldRL )
            ELSE
             WRITE(msgBuf,'(A)')
     &         ' MDS_READ_SEC_YZ: illegal value for arrType'
             CALL PRINT_ERROR( msgBuf, myThid )
             STOP 'ABNORMAL END: S/R MDS_READ_SEC_YZ'
            ENDIF
           ELSEIF (filePrec .EQ. precFloat64) THEN
            READ(dUnit,rec=irec) r8seg
#ifdef _BYTESWAPIO
            CALL MDS_BYTESWAPR8( sNy, r8seg )
#endif
            IF (arrType .EQ. 'RS') THEN
             CALL MDS_SEG8toRS_2D( sNy,oLy,kSize,bi,bj,k,.TRUE.,
     &                             r8seg,fldRS )
            ELSEIF (arrType .EQ. 'RL') THEN
             CALL MDS_SEG8toRL_2D( sNy,oLy,kSize,bi,bj,k,.TRUE.,
     &                             r8seg,fldRL )
            ELSE
             WRITE(msgBuf,'(A)')
     &         ' MDS_READ_SEC_YZ: illegal value for arrType'
             CALL PRINT_ERROR( msgBuf, myThid )
             STOP 'ABNORMAL END: S/R MDS_READ_SEC_YZ'
            ENDIF
           ELSE
            WRITE(msgBuf,'(A)')
     &        ' MDS_READ_SEC_YZ: illegal value for filePrec'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R MDS_READ_SEC_YZ'
           ENDIF
C End of k loop
         ENDDO
         IF (.NOT. globalFile) THEN
          CLOSE( dUnit )
          fileIsOpen = .FALSE.
         ENDIF
        ENDIF
C End of bi,bj loops
       ENDDO
      ENDDO

C If global file was opened then close it
      IF (fileIsOpen .AND. globalFile) THEN
       CLOSE( dUnit )
       fileIsOpen = .FALSE.
      ENDIF

      _END_MASTER( myThid )

C     ------------------------------------------------------------------
      RETURN
      END
