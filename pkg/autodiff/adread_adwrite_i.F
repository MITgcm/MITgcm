#include "AUTODIFF_OPTIONS.h"

C     ==================================================================
C
C     adread_adwrite_i.F: routines to handle the I/O of the TAF
C                         code for integer fields. All files are direct
C                         generated code or integer fields. All files
C                         are direct access files.
C     Routines:
C     o  ADREAD_I  - Read  INT data from file.
C     o  ADWRITE_I - Write INT data to   file.
C
C     see adread_adwrite.F for more explanation.
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
C     !ROUTINE: ADREAD_I
C     !INTERFACE:
      SUBROUTINE ADREAD_I(
     I                   myThid,
     I                   name,
     I                   len,
     I                   tid,
     I                   vid,
     O                   ivar,
     I                   size,
     I                   length,
     I                   irec )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ADREAD_I
C     ==================================================================
C     o read from direct access file, INTEGER version.
C     The strategy is to read an real*8 field with ADREAD resl*8 version
C     and convert to INTEGER
C     ==================================================================
C     SUBROUTINE ADREAD_I
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: number of the thread or instance of the program.
C     name   :: extended tape name.
C     len    :: number of characters in name.
C     tid    :: tape identifier.
C     vid    :: identifies the variable to be stored on tape.
C     ivar   :: values to be stored.
C     size   :: size in bytes of the type of variable var.
C     length :: dimension of the variable stored on the tape.
C     irec   :: record number to be written.
      INTEGER myThid
      CHARACTER*(*) name
      INTEGER len
      INTEGER tid
      INTEGER vid
      INTEGER ivar(*)
      INTEGER size
      INTEGER length
      INTEGER irec

C     !LOCAL VARIABLES:
      INTEGER k, lsize, length2d
C     2D fields only
      PARAMETER ( length2d = (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy )
      real*8  var(length2d)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_ENTER('ADREAD_I',myThid)
#endif

      IF ( length .GT. length2d ) THEN
       WRITE(msgBuf,'(A,I9,A,I9)')
     &      'ADREAD_I: length = ', length,' > length2d = ', length2d
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL ALL_PROC_DIE( myThid )
       STOP 'ABNORMAL END: S/R ADREAD_I'
      ENDIF
      lsize = 8
      CALL ADREAD(
     I            myThid,
     I            name,
     I            len,
     I            tid,
     I            vid,
     O            var,
     I            lsize,
     I            length,
     I            irec )

      DO k = 1,length
       ivar(k) = NINT(var(k))
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_LEAVE('ADREAD_I',myThid)
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADWRITE_I
C     !INTERFACE:
      SUBROUTINE ADWRITE_I(
     I                    myThid,
     I                    name,
     I                    len,
     I                    tid,
     I                    vid,
     I                    ivar,
     I                    size,
     I                    length,
     I                    irec )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ADWRITE_I
C     ==================================================================
C     o Write to direct access file, INTEGER version.
C     The strategy is to convert field to real*8 and use the
C     ADWRITE real*8 version.
C     ==================================================================
C     SUBROUTINE ADWRITE_I
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: number of the thread or instance of the program.
C     name   :: extended tape name.
C     len    :: number of characters in name.
C     tid    :: tape identifier.
C     vid    :: identifies the variable to be stored on tape.
C     ivar   :: values to be stored.
C     size   :: size in bytes of the type of variable var.
C     length :: dimension of the variable stored on the tape.
C     irec   :: record number to be written.
      INTEGER myThid
      CHARACTER*(*) name
      INTEGER len
      INTEGER tid
      INTEGER vid
      INTEGER ivar(*)
      INTEGER size
      INTEGER length
      INTEGER irec

C     !LOCAL VARIABLES:
      INTEGER k, lsize, length2d
      PARAMETER ( length2d = (sNx+2*OLx)*(sNy+2*OLy)*nSx*nSy )
      real*8  var(length2d)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_ENTER('ADWRITE_I',myThid)
#endif

      IF ( length .GT. length2d ) THEN
       WRITE(msgBuf,'(A,I9,A,I9)')
     &      'ADWRITE_I: length = ',length,' > length2d = ',length2d
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL ALL_PROC_DIE( myThid )
       STOP 'ABNORMAL END: S/R ADWRITE_I'
      ENDIF

      DO k = 1,length
C     no automatic conversion
c      var(k) = DFLOAT(ivar(k))
C     let compiler do the type conversion
       var(k) = ivar(k)
      ENDDO

      lsize = 8
      CALL ADWRITE(
     I             myThid,
     I             name,
     I             len,
     I             tid,
     I             vid,
     I             var,
     I             lsize,
     I             length,
     I             irec )

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_LEAVE('ADWRITE_I',myThid)
#endif

      RETURN
      END
