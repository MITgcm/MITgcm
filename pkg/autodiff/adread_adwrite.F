#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C     ==================================================================
C
C     adread_adwrite.F: routines to handle the I/O of the TAF
C                       generated code. All files are direct access files.
C     Routines:
C     o  ADREAD    - Read  RL/RS data from file.
C     o  ADWRITE   - Write RL/RS data to   file.
C
C     Note on short real variables (when _RS expands to real*4):
C     ADREAD/ADWRITE handles this case properly as in F77 only addresses
C     are passed to the subroutine. The size-parameter determines
C     correctly how the passed array var is passed to
C     MDS_READ/WRITE_TAPE. Some compiler/flag combinations that check
C     for subroutine argument consistency will complain about a real*4
C     array being passed to a real*8 array, but this can be avoided by
C     adding this file to the list of NOOPTFILES with appropriate
C     NOOPTFLAGS in the build-options file.
C
C     The following input variables are used throughout in the argument
C     lists:
C
C     name   -  character
C                 On entry, name is the extended tape name.
C     len    -  integer
C                 On entry, len is the number of characters in name.
C     tid    -  integer
C                 On entry, tid identifies the tape.
C     vid    -  integer
C                 On entry, vid identifies the variable to be stored on
C                 the tape.
C     var    -  real array of dimension length
C                 On entry, var contains the values to be stored.
C                           var must not be changed.
C     size   -  integer
C                 On entry, size is the size in bytes of the type of
C                           variable var.
C     length -  integer
C                 On entry, length is the dimension of the variable
C                           stored on the tape.
C     irec   -  integer
C                 On entry, irec is the record number to be written.
C     myThid -  integer
C                 On entry, myThid is the number of the thread or
C                           instance of the program.
C
C     TAF does not provide the leading argument myThid when compiling
C     the MITgcmUV code. Instead there is a sed script available that
C     changes the TAF-generated adjoint code.
C
C     Only the master thread is allowed to write data and only gobal
C     model arrays are allowed to be written be the subsequent routines.
C     Tiled data are to be stored in common blocks. This implies that at
C     least a two level checkpointing for the adjoint code has to be
C     available.
C
C     ==================================================================

CBOP
C     !ROUTINE: ADREAD
C     !INTERFACE:
      SUBROUTINE ADREAD(
     I                   myThid,
     I                   name,
     I                   len,
     I                   tid,
     I                   vid,
     O                   var,
     I                   size,
     I                   length,
     I                   irec )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adread
C     ==================================================================
C     o Read direct access file.
C     A call to this routine implies an open-read-close sequence
C     since it uses the MITgcmUV i/o routine MDS_READ_TAPE. Only
C     the master thread reads the data. Otherwise each thread would
C     read from file.
C     started: Christian Eckert eckert@mit.edu 30-Jun-1999
C     ==================================================================
C     SUBROUTINE adread
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "OPTIMCYCLE.h"
#endif
C- OpenAD: this header is not converted to a module --> go after the others
#include "AUTODIFF.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: number of the thread or instance of the program.
C     name   :: extended tape name.
C     len    :: number of characters in name.
C     tid    :: tape identifier.
C     vid    :: identifies the variable to be stored on tape.
C     var    :: values to be stored.
C     size   :: size in bytes of the type of variable var.
C     length :: dimension of the variable stored on the tape.
C     irec   :: record number to be written.
      INTEGER myThid
      CHARACTER*(*) name
      INTEGER len
      INTEGER tid
      INTEGER vid
      real*8  var(*)
      INTEGER size
      INTEGER length
      INTEGER irec

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) fname
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER filePrec
      INTEGER il, jl, lenLoc
      real*8  dummyR8(1)
      real*4  dummyR4(1)
      LOGICAL useWHTapeIO
#ifdef ALLOW_AUTODIFF_WHTAPEIO
      INTEGER n2d,length2d, jrec, i2d, j2d
#endif
CEOP

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_ENTER('ADREAD',myThid)
#endif

C--   default is to write tape-files of same precision as array:
C     convert bytes to file-prec
      filePrec = 8*size
      IF ( doSinglePrecTapelev ) THEN
       filePrec = precFloat32
      ENDIF

      useWHTapeIO = .FALSE.
#ifdef ALLOW_AUTODIFF_WHTAPEIO
C    determine number of 2d fields
      length2d = (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy
      n2d = INT(length/length2d)
      IF ( size.EQ.8 .AND. n2d*length2d.EQ.length ) THEN
C-    only use "WHTAPEIO" when type and length match
        useWHTapeIO = .TRUE.
      ENDIF
#endif /* ALLOW_AUTODIFF_WHTAPEIO */

      il = ILNBLNK( name )
      jl = ILNBLNK( adTapeDir )
      IF ( useWHTapeIO ) THEN
        lenLoc = il+jl
        WRITE(fname,'(2A)') adTapeDir(1:jl),name(1:il)
      ELSE
        lenLoc = il+jl+7
        WRITE(fname,'(3A,I4.4)')
     &     adTapeDir(1:jl),name(1:il),'.it',optimcycle
      ENDIF
#ifdef ALLOW_DEBUG
      IF ( debugLevel.GE.debLevC ) THEN
        WRITE(msgBuf,'(2A,3I6,I9,2I3,2A)') 'ADREAD: ',
     &    ' tid,vid, irec, length, prec(x2)=', tid, vid, irec,
     &      length, size, filePrec, ' fname=', fname(1:lenLoc)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
#endif

#ifdef ALLOW_AUTODIFF_WHTAPEIO
      IF ( useWHTapeIO ) THEN

cc      IF (n2d*length2d.EQ.length) THEN
        DO i2d=1,n2d
          IF (tapeFileUnit.EQ.0) THEN
            jrec=irec
          ELSE
            tapeFileCounter=tapeFileCounter+1
            jrec=tapeFileCounter+tapeMaxCounter*(irec-1)
            IF (tapeFileCounter.GT.tapeMaxCounter) THEN
              WRITE(msgBuf,'(A,2I5)')
     &              'ADREAD: tapeFileCounter > tapeMaxCounter ',
     &              tapeFileCounter, tapeMaxCounter
              CALL PRINT_ERROR( msgBuf, myThid )
              WRITE(msgBuf,'(2A)') 'for file ', fname(1:lenLoc)
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R ADREAD'
            ENDIF
          ENDIF
          j2d=(i2d-1)*length2d+1
          CALL MDS_READ_WHALOS(fname,lenLoc,filePrec,tapeFileUnit,
     &      1,var(j2d),jrec,tapeSingleCpuIO,tapeBufferIO,myThid)
        ENDDO
cc      ELSE
C     The other case actually does not (and should not) occur within the
C     main loop, where we only store global arrays (i.e. with i,j,bi,bj
C     indices) to disk. At init and final time it is always possible to
C     recompute or store in memory without much trouble or computational
C     cost.
cc         WRITE(msgBuf,'(3A)')
cc     &        'ADREAD: ', name, ' was not saved to tape.'
cc         CALL PRINT_ERROR( msgBuf, myThid )
cc         STOP 'ABNORMAL END: S/R ADREAD'
cc      ENDIF

      ELSE
#else
      IF ( .TRUE. ) THEN
#endif /* ALLOW_AUTODIFF_WHTAPEIO */

        _BEGIN_MASTER( myThid )
         IF ( size.EQ.4 ) THEN
c          CALL MDSREADVECTOR( fname, filePrec, 'RS',
c    &                         length, var, 1, 1, irec, myThid )
           CALL MDS_READ_TAPE( fname, filePrec, 'R4',
     &                         length, dummyR8, var,
     &                         useSingleCpuIO, irec, myThid )
         ELSE
c          CALL MDSREADVECTOR( fname, filePrec, 'RL',
c    &                         length, var, 1, 1, irec, myThid )
           CALL MDS_READ_TAPE( fname, filePrec, 'R8',
     &                         length,  var, dummyR4,
     &                         useSingleCpuIO, irec, myThid )
         ENDIF
        _END_MASTER( myThid )

C     end if useWHTapeIO / else
      ENDIF

C     Everyone must wait for the read operation to be completed.
c     _BARRIER

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_LEAVE('ADREAD',myThid)
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADWRITE
C     !INTERFACE:
      SUBROUTINE ADWRITE(
     I                    myThid,
     I                    name,
     I                    len,
     I                    tid,
     I                    vid,
     I                    var,
     I                    size,
     I                    length,
     I                    irec )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adwrite
C     ==================================================================
C     o Write to direct access file.
C     A call to this routine implies an open-read-close sequence
C     since it uses the MITgcmUV i/o routine MDS_WRITE_TAPE. Only
C     the master thread writes the data. Otherwise each thread would
C     write to file. This would result in an excessive waste of
C     disk space.
C     started: Christian Eckert eckert@mit.edu 30-Jun-1999
C     ==================================================================
C     SUBROUTINE adwrite
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "OPTIMCYCLE.h"
#endif
C- OpenAD: this header is not converted to a module --> go after the others
#include "AUTODIFF.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: number of the thread or instance of the program.
C     name   :: extended tape name.
C     len    :: number of characters in name.
C     tid    :: tape identifier.
C     vid    :: identifies the variable to be stored on tape.
C     var    :: values to be stored.
C     size   :: size in bytes of the type of variable var.
C     length :: dimension of the variable stored on the tape.
C     irec   :: record number to be written.
      INTEGER myThid
      CHARACTER*(*) name
      INTEGER len
      INTEGER tid
      INTEGER vid
      real*8  var(*)
      INTEGER size
      INTEGER length
      INTEGER irec

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) fname
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER filePrec
      INTEGER il,jl,lenLoc
      real*8  dummyR8(1)
      real*4  dummyR4(1)
      LOGICAL useWHTapeIO
      LOGICAL globalfile
#ifdef ALLOW_AUTODIFF_WHTAPEIO
      INTEGER n2d,length2d, jrec, i2d, j2d
#endif
CEOP

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_ENTER('ADWRITE',myThid)
#endif

C--   default is to write tape-files of same precision as array:
C     convert bytes to file-prec
      filePrec = 8*size
      IF ( doSinglePrecTapelev ) THEN
       filePrec = precFloat32
      ENDIF

      useWHTapeIO = .FALSE.
#ifdef ALLOW_AUTODIFF_WHTAPEIO
C    determine number of 2d fields
      length2d = (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy
      n2d = INT(length/length2d)
      IF ( size.EQ.8 .AND. n2d*length2d.EQ.length ) THEN
C-    only use "WHTAPEIO" when type and length match
        useWHTapeIO = .TRUE.
      ENDIF
#endif /* ALLOW_AUTODIFF_WHTAPEIO */

      il = ILNBLNK( name )
      jl = ILNBLNK( adTapeDir )
      IF ( useWHTapeIO ) THEN
        lenLoc = il+jl
        WRITE(fname,'(2A)') adTapeDir(1:jl),name(1:il)
      ELSE
        lenLoc = il+jl+7
        WRITE(fname,'(3A,I4.4)')
     &     adTapeDir(1:jl),name(1:il),'.it',optimcycle
      ENDIF
#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevC ) THEN
        WRITE(msgBuf,'(2A,3I6,I9,2I3,2A)') 'ADWRITE:',
     &    ' tid,vid, irec, length, prec(x2)=', tid, vid, irec,
     &      length, size, filePrec, ' fname=', fname(1:lenLoc)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
#endif

#ifdef ALLOW_AUTODIFF_WHTAPEIO
      IF ( useWHTapeIO ) THEN

cc      IF (n2d*length2d.EQ.length) THEN
        DO i2d=1,n2d
          IF (tapeFileUnit.EQ.0) THEN
            jrec=irec
          ELSE
            tapeFileCounter = tapeFileCounter+1
            jrec = tapeFileCounter+tapeMaxCounter*(irec-1)
            IF ( tapeFileCounter.GT.tapeMaxCounter ) THEN
              WRITE(msgBuf,'(2A,I6,A,I6,A)') 'ADWRITE: ',
     &            'tapeFileCounter (=',tapeFileCounter,
     &            ') > tapeMaxCounter (= nWh =', tapeMaxCounter, ')'
              CALL PRINT_ERROR( msgBuf, myThid )
              WRITE(msgBuf,'(3A)') 'ADWRITE: ',
     &            ' for file: ', fname(1:lenLoc)
              CALL PRINT_ERROR( msgBuf, myThid )
C     Here, we cannot determine how many tapeFiles we need, so we print
C     this rather unspecific instruction.
              WRITE(msgBuf,'(2A)') 'ADWRITE: ',
     &             '==> Need to increase "nWh" in: MDSIO_BUFF_WH.h'
              CALL PRINT_ERROR( msgBuf, myThid )
              WRITE(msgBuf,'(2A)') 'ADWRITE: Tip to find lowest',
     &             ' allowed "nWh": setting debugLevel >= 3'
              CALL PRINT_ERROR( msgBuf , myThid )
              WRITE(msgBuf,'(2A)') 'ADWRITE:  will report ',
     &             '"tapeFileCounter" to STDOUT (can grep for)'
              CALL PRINT_ERROR( msgBuf , myThid )
              STOP 'ABNORMAL END: S/R ADWRITE'
            ENDIF
#ifdef ALLOW_DEBUG
            IF ( debugLevel.GE.debLevC ) THEN
C     Print tapeFileCounter to help find minimum tapeMaxCounter=nWh value
              WRITE(msgBuf,'(A,I6)') 'ADWRITE:  tapeFileCounter =',
     &            tapeFileCounter
              CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
            ENDIF
#endif
          ENDIF
          j2d=(i2d-1)*length2d+1
          CALL MDS_WRITE_WHALOS(fname,lenLoc,filePrec,tapeFileUnit,
     &      1,var(j2d),jrec,tapeSingleCpuIO,tapeBufferIO,myThid)
        ENDDO
cc      ELSE
cc       WRITE(msgBuf,'(3A)')
cc     &      'ADWRITE: ',fname(1:lenLoc),'was not read from tape.'
cc       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
cc     &                     SQUEEZE_RIGHT , myThid)
cc      ENDIF

      ELSE
#else
      IF ( .TRUE. ) THEN
#endif /* ALLOW_AUTODIFF_WHTAPEIO */

        globalfile = globalFiles
c       globalfile = .FALSE.

        _BEGIN_MASTER( myThid )
        IF ( size.EQ.4 ) THEN
c         CALL MDSWRITEVECTOR( fname, filePrec, globalfile, 'RS',
c    &                         length, var, 1, 1, irec, 0, myThid )
          CALL MDS_WRITE_TAPE( fname, filePrec, globalfile, 'R4',
     &                         length, dummyR8, var,
     &                         useSingleCpuIO, irec, 0, myThid )
        ELSE
c         CALL MDSWRITEVECTOR( fname, filePrec, globalfile, 'RL',
c    &                         length, var, 1, 1, irec, 0, myThid )
          CALL MDS_WRITE_TAPE( fname, filePrec, globalfile, 'R8',
     &                         length, var, dummyR4,
     &                         useSingleCpuIO, irec, 0, myThid )
        ENDIF
        _END_MASTER( myThid )

C     end if useWHTapeIO / else
      ENDIF

C     Everyone must wait for the write operation to be completed.
c     _BARRIER

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_LEAVE('ADWRITE',myThid)
#endif

      RETURN
      END
