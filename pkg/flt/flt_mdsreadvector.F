C $Header: /u/gcmpack/MITgcm/pkg/flt/Attic/flt_mdsreadvector.F,v 1.3 2008/12/03 02:34:10 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"


#undef  SAFE_IO

#ifdef SAFE_IO
#define _NEW_STATUS 'new'
#else
#define _NEW_STATUS 'unknown'
#endif

      SUBROUTINE FLT_MDSREADVECTOR(
     I   fName,
     O   globalFile,
     I   filePrec,
     I   arrType,
     I   narr,
     O   arr,
     I   bi,
     I   bj,
     I   irecord,
     I   myThid )
C
C Arguments:
C
C fName		string	base name for file to read
C filePrec	integer	number of bits per word in file (32 or 64)
C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C narr		integer	size of third dimension: normally either 1 or Nr
C arr		RS/RL	array to read into, arr(narr)
ce bi           integer x tile index
ce bj           integer y tile index
C irecord	integer	record number to read
C myThid	integer thread identifier
C
C Created: 03/26/99 eckert@mit.edu
C Modified: 03/29/99 adcroft@mit.edu + eckert@mit.edu
C           Fixed to work work with _RS and _RL declarations
C Modified: 07/27/99 eckert@mit.edu
C           Customized  for state estimation (--> active_file_control.F)
c Modified: 09/29/00 abiastoch@ucsd.edu
c           based on mdsreadvector
c           Checks first for local files and then for global

      IMPLICIT NONE
C Global variables / COMMON blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER narr
c     Real arr(narr)
      _RL arr(narr)
      INTEGER irecord
      INTEGER myThid
      INTEGER bi,bj

C Functions
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER  MDS_RECLEN
      EXTERNAL MDS_RECLEN
C Local variables
      CHARACTER*(MAX_LEN_FNAM) dataFName
      INTEGER i,iG,jG,irec,dUnit,IL,iLfn
      LOGICAL exst
      LOGICAL globalFile,fileIsOpen
      INTEGER length_of_rec
      CHARACTER*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------

C Only DO I/O IF I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      IF (irecord .LT. 1) THEN
       WRITE(msgbuf,'(A,I9.8)')
     &   ' FLT_MDSREADVECTOR: argument irecord = ',irecord
       CALL PRINT_MESSAGE( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , myThid)
       WRITE(msgbuf,'(A)')
     &   ' FLT_MDSREADVECTOR: invalid value for irecord'
       CALL PRINT_ERROR( msgbuf, myThid )
       STOP 'ABNORMAL END: S/R FLT_MDSREADVECTOR'
      ENDIF
      IF ( arrType.NE.'RL' ) THEN
       WRITE(msgbuf,'(3A)')
     &   ' FLT_MDSREADVECTOR: not yet coded for arrType="',arrType,'"'
       CALL PRINT_ERROR( msgbuf, myThid )
       STOP 'ABNORMAL END: S/R FLT_MDSREADVECTOR'
      ENDIF

C Assume nothing
      globalFile = .TRUE.
      fileIsOpen = .FALSE.
      IL=ILNBLNK( fName )

C Assign a free unit number as the I/O channel for this routine
      CALL MDSFINDUNIT( dUnit, myThid )

C Check first for local file
      iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
      jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
      WRITE(dataFname,'(2A,I3.3,A,I3.3,A)')
     &          fName(1:IL),'.',iG,'.',jG,'.data'
      INQUIRE( file=dataFname, exist=exst )
C Of course, we only open the file IF the tile is "active"
C (This is a place-holder for the active/passive mechanism)
      IF (exst) THEN
        globalFile = .FALSE.
      ENDIF

C If no local file is available check for global files
      IF (globalFile) THEN
C Check first for global file with simple name (ie. fName)
       WRITE(dataFname,'(2A)') fName(1:IL)
       iLfn = IL
       INQUIRE( file=dataFname, exist=exst )
c      IF (exst) THEN
c        write(0,*) 'found file: ',dataFname(1:iLfn)
c      ENDIF
       IF ( .NOT.exst) THEN
        WRITE(dataFname,'(2A)') fName(1:IL),'.data'
        iLfn = IL+5
        INQUIRE( file=dataFname, exist=exst )
c       IF (exst) THEN
c        write(0,*) 'found file: ',dataFname(1:iLfn)
c       ENDIF
       ENDIF
      ENDIF

C If we are reading from a global file then we open it here
      IF (globalFile) THEN
       IF ( debugLevel.GE.debLevB ) THEN
        WRITE(msgbuf,'(A,A)')
     &   ' FLT_MDSREADVECTOR: opening global file: ',dataFName(1:iLfn)
        CALL PRINT_MESSAGE( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , myThid)
       ENDIF
       length_of_rec=MDS_RECLEN( filePrec, narr, myThid )
       OPEN( dUnit, file=dataFName, status='old',
     &      access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      ENDIF

C Loop over all tiles
ce      DO bj=1,nSy
ce       DO bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        IF (.NOT. globalFile) THEN
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         WRITE(dataFname,'(2A,I3.3,A,I3.3,A)')
     &              fName(1:IL),'.',iG,'.',jG,'.data'
         iLfn= IL+8+5
         INQUIRE( file=dataFname, exist=exst )
C Of course, we only open the file IF the tile is "active"
C (This is a place-holder for the active/passive mechanism)
         IF (exst) THEN
          IF ( debugLevel.GE.debLevB ) THEN
           WRITE(msgbuf,'(A,A)')
     &      ' FLT_MDSREADVECTOR: opening file: ',dataFName(1:iLfn)
           CALL PRINT_MESSAGE( msgbuf, standardmessageunit,
     &                         SQUEEZE_RIGHT , myThid)
          ENDIF
          length_of_rec=MDS_RECLEN( filePrec, narr, myThid )
          OPEN( dUnit, file=dataFName, status='old',
     &        access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         ELSE
          fileIsOpen=.FALSE.
          WRITE(msgbuf,'(A)')
     &      ' FLT_MDSREADVECTOR: un-active tiles not implemented yet'
          CALL PRINT_ERROR( msgbuf, myThid )
          STOP 'ABNORMAL END: S/R FLT_MDSREADVECTOR'
         ENDIF
        ENDIF

        IF (fileIsOpen) THEN
          irec = irecord
          IF (filePrec .EQ. precFloat32) THEN
C-    wrong S/R call: should be MDS_READ_R4_VEC_RL (if arrType=RL)
C-                           or MDS_READ_R4_VEC_RS (if arrType=RS)
c          CALL MDS_READ_RS_VEC( dUnit, irec, narr, arr, myThid )
           WRITE(msgbuf,'(A,I8)')
     &     ' FLT_MDSREADVECTOR: not yet coded for filePrec=',filePrec
           CALL PRINT_ERROR( msgbuf, myThid )
           STOP 'ABNORMAL END: S/R FLT_MDSREADVECTOR'
          ELSEIF (filePrec .EQ. precFloat64) THEN
C-    wrong S/R call: should be MDS_READ_R8_VEC_RL (if arrType=RL)
C-                           or MDS_READ_R8_VEC_RS (if arrType=RS)
C-    + byte-swapp should be inside MDS_READ_RL_VEC
c          CALL MDS_READ_RL_VEC( dUnit, irec, narr, arr, myThid )
           READ( dUnit, rec=irec ) ( arr(i),i=1,narr )
#ifdef _BYTESWAPIO
           CALL MDS_BYTESWAPR8( narr, arr )
#endif
          ELSE
            WRITE(msgbuf,'(A)')
     &        ' FLT_MDSREADVECTOR: illegal value for filePrec'
            CALL PRINT_ERROR( msgbuf, myThid )
            STOP 'ABNORMAL END: S/R FLT_MDSREADVECTOR'
          ENDIF
          IF (.NOT. globalFile) THEN
            CLOSE( dUnit )
            fileIsOpen = .FALSE.
          ENDIF
        ENDIF
C End of bi,bj loops
ce       ENDDO
ce      ENDDO

C If global file was opened then close it
      IF (fileIsOpen .AND. globalFile) THEN
        CLOSE( dUnit )
        fileIsOpen = .FALSE.
      ENDIF

      _END_MASTER( myThid )

C     ------------------------------------------------------------------
      RETURN
      END
