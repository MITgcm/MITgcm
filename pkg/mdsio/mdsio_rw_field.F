#undef USE_OBSOLETE_MDS_RW_FIELD
#include "MDSIO_OPTIONS.h"

C--  File mdsio_rw_field.F: old version of MDSIO READ/WRITE FIELD S/R with
C    fewer arguments (kept for backward compatibility): call new MDSIO S/R
C    with fixed additional arguments
C--   Contents
C--   o MDSREADFIELD
C--   o MDSREADFIELD_LOC
C--   o MDSWRITEFIELD
C--   o MDSWRITEFIELD_LOC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELD(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     O   arr,
     I   irecord,
     I   myThid )
C
C Arguments:
C
C fName     (string)  :: base name for file to written
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C arrType   (char(2)) :: declaration of "arr": either "RS" or "RL"
C nNz       (integer) :: size of third dimension: normally either 1 or Nr
C arr       ( RS/RL ) :: array to write, arr(:,:,nNz,:,:)
C irecord   (integer) :: record number to read
C myThid    (integer) :: thread identifier
C
C Routine now calls MDS_READ_FIELD, just a way to add 2 extra arguments
C to the argument list. The 1rst new argument is to make the difference between
C the vertical dimension (3rd dimension) of the output array and the number
C of levels to read in. This routine assumes they are the same.
C The 2nd new argument (useCurrentDir=.FALSE.) allows to read files from
C the "mdsioLocalDir" directory (if it is set).

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

#ifdef USE_OBSOLETE_MDS_RW_FIELD
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_READ_FIELD(
     I                fName, filePrec, .FALSE., arrType, nNz, 1, nNz,
     O                arr, dummyRS,
     I                irecord, myThid )
      ELSE
        CALL MDS_READ_FIELD(
     I                fName, filePrec, .FALSE., arrType, nNz, 1, nNz,
     O                dummyRL, arr,
     I                irecord, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_FIELD */
      STOP 'ABNORMAL END: S/R MDSREADFIELD is retired'
#endif /* USE_OBSOLETE_MDS_RW_FIELD */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSREADFIELD_LOC(
     I   fName,
     I   filePrec,
     I   arrType,
     I   nNz,
     O   arr,
     I   irecord,
     I   myThid )
C
C Arguments:
C
C fName     (string)  :: base name for file to write
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C arrType   (char(2)) :: declaration of "arr": either "RS" or "RL"
C nNz       (integer) :: size of third dimension: normally either 1 or Nr
C arr       ( RS/RL ) :: array to write, arr(:,:,nNz,:,:)
C irecord   (integer) :: record number to read
C myThid    (integer) :: thread identifier
C
C Routine now calls MDS_READ_FIELD, just a way to add 2 extra arguments
C to the argument list. The 1rst new argument is to make the difference between
C the vertical dimension (3rd dimension) of the output array and the number
C of levels to read in. This routine assumes they are the same.
C The 2nd new argument (useCurrentDir=.FALSE.) forces to ignore the
C "mdsioLocalDir" parameter and to always read from the current directory.

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

#ifdef USE_OBSOLETE_MDS_RW_FIELD
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_READ_FIELD(
     I                fName, filePrec, .TRUE., arrType, nNz, 1, nNz,
     O                arr, dummyRS,
     I                irecord, myThid )
      ELSE
        CALL MDS_READ_FIELD(
     I                fName, filePrec, .TRUE., arrType, nNz, 1, nNz,
     O                dummyRL, arr,
     I                irecord, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_FIELD */
      STOP 'ABNORMAL END: S/R MDSREADFIELD_LOC is empty'
#endif /* USE_OBSOLETE_MDS_RW_FIELD */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELD(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )
C
C Arguments:
C
C fName     (string)  :: base name for file to write
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C globalFile (logical):: selects between writing a global or tiled file
C arrType   (char(2)) :: declaration of "arr": either "RS" or "RL"
C nNz       (integer) :: size of third dimension: normally either 1 or Nr
C arr       ( RS/RL ) :: array to write, arr(:,:,nNzdim,:,:)
C irecord   (integer) :: record number to write
C myIter    (integer) :: time step number
C myThid    (integer) :: thread identifier
C
C Routine now calls MDS_WRITE_FIELD, just a way to add 2 extra arguments
C to the argument list. The 1rst new argument is to make the difference between
C the vertical dimension (3rd dimension) of an array and the number of levels
C the output routine should process. This routine assumes they are the same.
C The 2nd new argument (useCurrentDir=.FALSE.) allows to write files to
C the "mdsioLocalDir" directory (if it is set).

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
      _RL     arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz,nSx,nSy)
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

#ifdef USE_OBSOLETE_MDS_RW_FIELD
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_WRITE_FIELD(
     I                 fName, filePrec, globalFile, .FALSE.,
     I                 arrType, nNz, 1, nNz, arr, dummyRS,
     I                 irecord, myIter, myThid )
      ELSE
        CALL MDS_WRITE_FIELD(
     I                 fName, filePrec, globalFile, .FALSE.,
     I                 arrType, nNz, 1, nNz, dummyRL, arr,
     I                 irecord, myIter, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_FIELD */
      STOP 'ABNORMAL END: S/R MDSWRITEFIELD is retired'
#endif /* USE_OBSOLETE_MDS_RW_FIELD */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDSWRITEFIELD_LOC(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )
C
C Arguments:
C
C fName     (string)  :: base name for file to write
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C globalFile (logical):: selects between writing a global or tiled file
C arrType   (char(2)) :: declaration of "arr": either "RS" or "RL"
C nNz       (integer) :: size of third dimension: normally either 1 or Nr
C arr       ( RS/RL ) :: array to write, arr(:,:,nNzdim,:,:)
C irecord   (integer) :: record number to write
C myIter    (integer) :: time step number
C myThid    (integer) :: thread identifier
C
C Routine now calls mdswritefield_new, just a way to add 2 extra arguments
C to the argument list. The 1rst new argument is to make the difference between
C the vertical dimension (3rd dimension) of an array and the number of levels
C the output routine should process. This routine assumes they are the same.
C The 2nd new argument (useCurrentDir=.TRUE.) forces to ignore the
C "mdsioLocalDir" parameter and to always write to the current directory.

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
      _RL     arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz,nSx,nSy)
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid

#ifdef USE_OBSOLETE_MDS_RW_FIELD
C Local variables
      _RL dummyRL(1)
      _RS dummyRS(1)

      IF ( arrType.EQ.'RL' ) THEN
        CALL MDS_WRITE_FIELD(
     I                 fName, filePrec, globalFile, .TRUE.,
     I                 arrType, nNz, 1, nNz, arr, dummyRS,
     I                 irecord, myIter, myThid )
      ELSE
        CALL MDS_WRITE_FIELD(
     I                 fName, filePrec, globalFile, .TRUE.,
     I                 arrType, nNz, 1, nNz, dummyRL, arr,
     I                 irecord, myIter, myThid )
      ENDIF

#else /* USE_OBSOLETE_MDS_RW_FIELD */
      STOP 'ABNORMAL END: S/R MDSWRITEFIELD_LOC is empty'
#endif /* USE_OBSOLETE_MDS_RW_FIELD */

      RETURN
      END
