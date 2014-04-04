C $Header: /u/gcmpack/MITgcm/pkg/autodiff/adread_adwrite.F,v 1.25 2014/04/04 23:03:59 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C     ==================================================================
C
C     ad_read_write.F: routines to handle the I/O of the TAMC generated
C                      code. All files are direct access files.
C     Routines:
C     o  ADREAD  - Read  data from file.
C     o  ADWRITE - Write data to   file.
C
C
C     The following input veriables are used throughout in the argument
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
C     For further details on this see the TAMC Users Manual, Appendix B,
C     User defined Storage Subroutines.
C
C     TAMC does not provide the two leading arguments myThid and myIter
C     when compiling the MITgcmUV code. Instead the is a sed script avail-
C     able that does change the TAMC-generated adjoint code.
C
C     Only the master thread is allowed to write data and only gobal
C     model arrays are allowed to be written be the subsequent routines.
C     Tiled data are to be stored in common blocks. This implies that at
C     least a two level checkpointing for the adjoint code has to be
C     available.
C
C     ==================================================================

CBOP
C     !ROUTINE: adread
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
     I                   irec
     &                 )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adread
C     ==================================================================
C     o Read direct access file.
C     A call to this routine implies an open-read-close sequence
C     since it uses the MITgcmUV i/o routine MDSREADVECTOR. Only
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
#include "ctrl.h"
#include "optim.h"
#include "AUTODIFF.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
C     name   :: extended tape name.
C     len    :: number of characters in name.
C     tid    :: tape identifier.
C     vid    :: identifies the variable to be stored on tape.
C     var    :: values to be stored.
C     size   :: size in bytes of the type of variable var.
C     length :: dimension of the variable stored on the tape.
C     myThid :: number of the thread or instance of the program.
C     irec   :: record number to be written.

      INTEGER myThid
      CHARACTER*(*) name
      INTEGER len
      INTEGER tid
      INTEGER vid
      INTEGER size
      INTEGER length
      INTEGER irec
      real*8  var(*)

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == local variables ==
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

cc      if (n2d*length2d.EQ.length) then
        DO i2d=1,n2d
          if (tapeFileUnit.EQ.0) THEN
            jrec=irec
          else
            tapeFileCounter=tapeFileCounter+1
            jrec=tapeFileCounter+tapeMaxCounter*(irec-1)
            if (tapeFileCounter.GT.tapeMaxCounter) stop
          endif
          j2d=(i2d-1)*length2d+1
          call mds_read_whalos(fname,lenLoc,filePrec,tapeFileUnit,
     &      1,var(j2d),jrec,tapeSingleCpuIO,tapeBufferIO,myThid)
        ENDDO
cc      else
C   The other case actually does not (and should not) occur within the main loop,
C   where we only store global arrays (i.e. with i,j,bi,bj indices) to disk.
C     At init and final time it is always be possible to recompute or store in
C   memory without much trouble or computational cost.
C
C    Presently there are three instances where non-global arrays are stored to disk:
C   (a) the one instance when onetape is used, to store myTime, which is of no effect.
C   In the_main_loop, we switch onetape to memory tape if ALLOW_AUTODIFF_WHTAPEIO
C   (b) the two instances when tapelev_ini_bibj_k is used (in convective
C   _adjustment_ini.F and cost_drifter.F) are disabled at compile time if
C   ALLOW_AUTODIFF_WHTAPEIO. So is the definition of tapelev_ini_bibj_k,
C   which is not supported with ALLOW_AUTODIFF_WHTAPEIO (side-note:
C   tapelev_ini_bibj_k is likely unsafe with mdsreadvector/mdsreadvector)
C
C   The issue could be revisited if needed.
C   But for now we stop if any otehr instance is met.
cc         WRITE(msgBuf,'(3A)')
cc     &        'ADWRITE: ',name,'was not saved to tape.'
cc         CALL PRINT_ERROR( msgBuf, myThid )
cc         STOP 'ABNORMAL END: S/R ADWRITE'
cc      endif

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
C     !ROUTINE: adwrite
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
     I                    irec
     &                  )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adwrite
C     ==================================================================
C     o Write to direct access file.
C     A call to this routine implies an open-read-close sequence
C     since it uses the MITgcmUV i/o routine MDSREADVECTOR. Only
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
#include "ctrl.h"
#include "optim.h"
#include "AUTODIFF.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
C     name   :: extended tape name.
C     len    :: number of characters in name.
C     tid    :: tape identifier.
C     vid    :: identifies the variable to be stored on tape.
C     var    :: values to be stored.
C     size   :: size in bytes of the type of variable var.
C     length :: dimension of the variable stored on the tape.
C     myThid :: number of the thread or instance of the program.
C     irec   :: record number to be written.

      INTEGER myThid
      CHARACTER*(*) name
      INTEGER len
      INTEGER tid
      INTEGER vid
      INTEGER size
      INTEGER length
      INTEGER irec
      real*8  var(*)

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == local variables ==
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

cc      if (n2d*length2d.EQ.length) then
        DO i2d=1,n2d
          if (tapeFileUnit.EQ.0) THEN
            jrec=irec
          else
            tapeFileCounter=tapeFileCounter+1
            jrec=tapeFileCounter+tapeMaxCounter*(irec-1)
            if (tapeFileCounter.GT.tapeMaxCounter) then
               write(msgBuf,'(A,2I5)')
     &              'ADWRITE: tapeFileCounter > tapeMaxCounter ',
     &              tapeFileCounter, tapeMaxCounter
               CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                             SQUEEZE_RIGHT, myThid )
               write(msgBuf,'(2A)') 'for file ', fname(1:lenLoc)
               CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                             SQUEEZE_RIGHT, myThid )
               STOP 'in S/R ADWRITE'
            endif
          endif
          j2d=(i2d-1)*length2d+1
          call mds_write_whalos(fname,lenLoc,filePrec,tapeFileUnit,
     &      1,var(j2d),jrec,tapeSingleCpuIO,tapeBufferIO,myThid)
        ENDDO
cc      else
cc       write(msgBuf,'(3A)')
cc     &      'ADWRITE: ',fname(1:lenLoc),'was not read from tape.'
cc       call print_message( msgBuf, errorMessageUnit,
cc     &                     SQUEEZE_RIGHT , myThid)
cc      endif

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
