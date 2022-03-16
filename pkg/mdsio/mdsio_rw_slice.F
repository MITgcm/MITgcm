#undef USE_OBSOLETE_MDS_RW_SLICE
#include "MDSIO_OPTIONS.h"

C--  File mdsio_rw_slice.F: old version of MDS_READ/WRITE_SEC_XZ/YZ S/R with
C    fewer arguments (kept for backward compatibility): call new MDSIO S/R
C    with fixed additional arguments
C--   Contents
C--   o MDSREADFIELDXZ
C--   o MDSREADFIELDYZ
C--   o MDSREADFIELDXZ_LOC
C--   o MDSREADFIELDYZ_LOC
C--   o MDSWRITEFIELDXZ
C--   o MDSWRITEFIELDYZ
C--   o MDSWRITEFIELDXZ_LOC
C--   o MDSWRITEFIELDYZ_LOC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELDXZ(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     |   arr,
     I   irecord,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to written
C filePrec   integer :: number of bits per word in file (32 or 64)
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNz,:,:)
C irecord    integer :: record number to read
C myThid     integer :: thread identifier
C
C Routine now calls MDS_READ_SEC_XZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.FALSE.) allows to read files from
C the "mdsioLocalDir" directory (if it is set).
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / COMMON blocks
#include "SIZE.h"
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER nNz
      _RL     arr(*)
      INTEGER irecord
      INTEGER myThid

#ifdef USE_OBSOLETE_MDS_RW_SLICE
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_READ_SEC_XZ(
     I                fName, filePrec, .FALSE., arrType, nNz,
     O                arr, dummyRS,
     I                irecord, myThid )
      ELSE
        CALL MDS_READ_SEC_XZ(
     I                fName, filePrec, .FALSE., arrType, nNz,
     O                dummyRL, arr,
     I                irecord, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_SLICE */
      STOP 'ABNORMAL END: S/R MDSREADFIELDXZ is retired'
#endif /* USE_OBSOLETE_MDS_RW_SLICE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELDYZ(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     |   arr,
     I   irecord,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to written
C filePrec   integer :: number of bits per word in file (32 or 64)
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNz,:,:)
C irecord    integer :: record number to read
C myThid     integer :: thread identifier
C
C Routine now calls MDS_READ_SEC_YZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.FALSE.) allows to read files from
C the "mdsioLocalDir" directory (if it is set).
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / COMMON blocks
#include "SIZE.h"
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER nNz
      _RL     arr(*)
      INTEGER irecord
      INTEGER myThid

#ifdef USE_OBSOLETE_MDS_RW_SLICE
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_READ_SEC_YZ(
     I                fName, filePrec, .FALSE., arrType, nNz,
     O                arr, dummyRS,
     I                irecord, myThid )
      ELSE
        CALL MDS_READ_SEC_YZ(
     I                fName, filePrec, .FALSE., arrType, nNz,
     O                dummyRL, arr,
     I                irecord, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_SLICE */
      STOP 'ABNORMAL END: S/R MDSREADFIELDYZ is retired'
#endif /* USE_OBSOLETE_MDS_RW_SLICE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELDXZ_LOC(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     |   arr,
     I   irecord,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to written
C filePrec   integer :: number of bits per word in file (32 or 64)
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNz,:,:)
C irecord    integer :: record number to read
C myThid     integer :: thread identifier
C
C Routine now calls MDS_READ_SEC_XZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.TRUE.) forces to ignore the
C "mdsioLocalDir" parameter and to always write to the current directory.
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / COMMON blocks
#include "SIZE.h"
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER nNz
      _RL     arr(*)
      INTEGER irecord
      INTEGER myThid

#ifdef USE_OBSOLETE_MDS_RW_SLICE
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_READ_SEC_XZ(
     I                fName, filePrec, .TRUE., arrType, nNz,
     O                arr, dummyRS,
     I                irecord, myThid )
      ELSE
        CALL MDS_READ_SEC_XZ(
     I                fName, filePrec, .TRUE., arrType, nNz,
     O                dummyRL, arr,
     I                irecord, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_SLICE */
      STOP 'ABNORMAL END: S/R MDSREADFIELDXZ_LOC is empty'
#endif /* USE_OBSOLETE_MDS_RW_SLICE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELDYZ_LOC(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     |   arr,
     I   irecord,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to written
C filePrec   integer :: number of bits per word in file (32 or 64)
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNz,:,:)
C irecord    integer :: record number to read
C myThid     integer :: thread identifier
C
C Routine now calls MDS_READ_SEC_YZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.TRUE.) forces to ignore the
C "mdsioLocalDir" parameter and to always write to the current directory.
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / COMMON blocks
#include "SIZE.h"
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      CHARACTER*(2) arrType
      INTEGER nNz
      _RL     arr(*)
      INTEGER irecord
      INTEGER myThid

#ifdef USE_OBSOLETE_MDS_RW_SLICE
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_READ_SEC_YZ(
     I                fName, filePrec, .TRUE., arrType, nNz,
     O                arr, dummyRS,
     I                irecord, myThid )
      ELSE
        CALL MDS_READ_SEC_YZ(
     I                fName, filePrec, .TRUE., arrType, nNz,
     O                dummyRL, arr,
     I                irecord, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_SLICE */
      STOP 'ABNORMAL END: S/R MDSREADFIELDYZ_LOC is empty'
#endif /* USE_OBSOLETE_MDS_RW_SLICE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELDXZ(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to write
C filePrec   integer :: number of bits per word in file (32 or 64)
C globalFile logical :: selects between writing a global or tiled file
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNzdim,:,:)
C irecord    integer :: record number to write
C myIter     integer :: time step number
C myThid     integer :: thread identifier
C
C Routine now calls MDS_WRITE_REC_XZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.FALSE.) allows to write files to
C the "mdsioLocalDir" directory (if it is set).
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalFile
      CHARACTER*(2) arrType
      INTEGER nNz
      _RL     arr(*)
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

#ifdef USE_OBSOLETE_MDS_RW_SLICE
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_WRITE_SEC_XZ(
     I                 fName, filePrec, globalFile, .FALSE.,
     I                 arrType, nNz, arr, dummyRS,
     I                 irecord, myIter, myThid )
      ELSE
        CALL MDS_WRITE_SEC_XZ(
     I                 fName, filePrec, globalFile, .FALSE.,
     I                 arrType, nNz, dummyRL, arr,
     I                 irecord, myIter, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_SLICE */
      STOP 'ABNORMAL END: S/R MDSWRITEFIELDXZ is retired'
#endif /* USE_OBSOLETE_MDS_RW_SLICE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELDYZ(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to write
C filePrec   integer :: number of bits per word in file (32 or 64)
C globalFile logical :: selects between writing a global or tiled file
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNzdim,:,:)
C irecord    integer :: record number to write
C myIter     integer :: time step number
C myThid     integer :: thread identifier
C
C Routine now calls MDS_WRITE_REC_YZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.FALSE.) allows to write files to
C the "mdsioLocalDir" directory (if it is set).
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalFile
      CHARACTER*(2) arrType
      INTEGER nNz
      _RL     arr(*)
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

#ifdef USE_OBSOLETE_MDS_RW_SLICE
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_WRITE_SEC_YZ(
     I                 fName, filePrec, globalFile, .FALSE.,
     I                 arrType, nNz, arr, dummyRS,
     I                 irecord, myIter, myThid )
      ELSE
        CALL MDS_WRITE_SEC_YZ(
     I                 fName, filePrec, globalFile, .FALSE.,
     I                 arrType, nNz, dummyRL, arr,
     I                 irecord, myIter, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_SLICE */
      STOP 'ABNORMAL END: S/R MDSWRITEFIELDYZ is retired'
#endif /* USE_OBSOLETE_MDS_RW_SLICE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELDXZ_LOC(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to write
C filePrec   integer :: number of bits per word in file (32 or 64)
C globalFile logical :: selects between writing a global or tiled file
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNzdim,:,:)
C irecord    integer :: record number to write
C myIter     integer :: time step number
C myThid     integer :: thread identifier
C
C Routine now calls MDS_WRITE_REC_XZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.TRUE.) forces to ignore the
C "mdsioLocalDir" parameter and to always write to the current directory.
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalFile
      CHARACTER*(2) arrType
      INTEGER nNz
      _RL     arr(*)
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

#ifdef USE_OBSOLETE_MDS_RW_SLICE
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_WRITE_SEC_XZ(
     I                 fName, filePrec, globalFile, .TRUE.,
     I                 arrType, nNz, arr, dummyRS,
     I                 irecord, myIter, myThid )
      ELSE
        CALL MDS_WRITE_SEC_XZ(
     I                 fName, filePrec, globalFile, .TRUE.,
     I                 arrType, nNz, dummyRL, arr,
     I                 irecord, myIter, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_SLICE */
      STOP 'ABNORMAL END: S/R MDSWRITEFIELDXZ_LOC is empty'
#endif /* USE_OBSOLETE_MDS_RW_SLICE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELDYZ_LOC(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )

C Arguments:
C
C fName      string  :: base name for file to write
C filePrec   integer :: number of bits per word in file (32 or 64)
C globalFile logical :: selects between writing a global or tiled file
C arrType    char(2) :: declaration of "arr": either "RS" or "RL"
C nNz        integer :: size of third dimension: normally either 1 or Nr
C arr         RS/RL  :: array to write, arr(:,:,nNzdim,:,:)
C irecord    integer :: record number to write
C myIter     integer :: time step number
C myThid     integer :: thread identifier
C
C Routine now calls MDS_WRITE_REC_YZ, just a way to add extra arguments
C to the argument list.
C The 1rst new argument (useCurrentDir=.TRUE.) forces to ignore the
C "mdsioLocalDir" parameter and to always write to the current directory.
C The 2nd new argument avoid argument array of undefined type (RL/RS).

      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"
c #include "EEPARAMS.h"

C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalFile
      CHARACTER*(2) arrType
      INTEGER nNz
      _RL     arr(*)
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

#ifdef USE_OBSOLETE_MDS_RW_SLICE
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_WRITE_SEC_YZ(
     I                 fName, filePrec, globalFile, .TRUE.,
     I                 arrType, nNz, arr, dummyRS,
     I                 irecord, myIter, myThid )
      ELSE
        CALL MDS_WRITE_SEC_YZ(
     I                 fName, filePrec, globalFile, .TRUE.,
     I                 arrType, nNz, dummyRL, arr,
     I                 irecord, myIter, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_SLICE */
      STOP 'ABNORMAL END: S/R MDSWRITEFIELDYZ_LOC is empty'
#endif /* USE_OBSOLETE_MDS_RW_SLICE */

      RETURN
      END
