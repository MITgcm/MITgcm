C $Header: /u/gcmpack/MITgcm/pkg/autodiff/active_file_gen_ad.F,v 1.2 2012/08/06 19:27:13 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

C     ==================================================================
C     active_file_gen_ad.F: Routines to handle the I/O of the active file
C                           for the adjoint calculations.
C     Routines
C    o  adACTIVE_READ_GEN_RL    - Adjoint of ACTIVE_READ_GEN_RL
C    o  adACTIVE_READ_GEN_RS    - Adjoint of ACTIVE_READ_GEN_RS
C
C    o  adACTIVE_WRITE_GEN_RL   - Adjoint of ACTIVE_WRITE_GEN_RL
C    o  adACTIVE_WRITE_GEN_RS   - Adjoint of ACTIVE_WRITE_GEN_RS
C
C     Generic version for each array type (_RL or _RS).
C     Note: the _RL version can replace the 4 x 2 shape-specific
C           (_xy,_xyz,_xz,_yz) and standard or "_loc" routines.
C     ==================================================================

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adACTIVE_READ_GEN_RL
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE adACTIVE_READ_GEN_RL(
     I                               activeVar_file,
     I                               arrShape, myNr,
     I                               iRec,
     I                               globalFile,
     I                               useCurrentDir,
     I                               lAdInit,
     I                               myIter,
     I                               myThid,
     O                               adactive_var
     I                               )
#else
      SUBROUTINE adACTIVE_READ_GEN_RL(
     I                               activeVar_file,
     O                               adactive_var,
     I                               arrShape, myNr,
     I                               iRec,
     I                               globalFile,
     I                               useCurrentDir,
     I                               lAdInit,
     I                               myIter,
     I                               myThid,
     I                               dummy,
     O                               addummy )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adACTIVE_READ_GEN_RL
C     ==================================================================
C     o Adjoint of ACTIVE_READ_GEN_RL.
C     Accept active variable of various (XY,XZ,YZ) shape and level
C     number, according to arguments arrShape and myNr.
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     adactive_var   :: adoint variable array
C     arrShape       :: shape of active-var array ('XY','XZ' or 'YZ')
C     myNr           :: number of vertical-levels in active-var array
C     iRec           :: record number in file
C     globalFile     :: used for writing global (if =T) or tiled files
C     useCurrentDir  :: always read from the current directory
C                        (even if "mdsioLocalDir" is set)
C     lAdInit        :: initialisation of corresponding adjoint variable
C                        and write to active file
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: my Thread Id number
C     dummy          :: (needed for 2nd derivative code)
      CHARACTER*(*) activeVar_file
      _RL     adactive_var(*)
      CHARACTER*(2) arrShape
      INTEGER myNr
      INTEGER iRec
      LOGICAL globalFile
      LOGICAL useCurrentDir
      LOGICAL lAdInit
      INTEGER myIter
      INTEGER myThid
      _RL     dummy, addummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2)  pref
      CHARACTER*(80) fName
      INTEGER il
CEOP

      pref = 'ad'
      il   = ILNBLNK( activeVar_file )
      WRITE(fName(1:80),'(A)') ' '
      WRITE(fName(1:2+il),'(2A)') pref, activeVar_file(1:il)

      IF ( arrShape.EQ.'XY' ) THEN

        CALL ACTIVE_READ_3D_RL(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSEIF ( arrShape.EQ.'XZ' ) THEN

        CALL ACTIVE_READ_XZ_RL(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSEIF ( arrShape.EQ.'YZ' ) THEN

        CALL ACTIVE_READ_YZ_RL(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSE
        STOP
     &   'ABNORMAL END: S/R adACTIVE_READ_GEN_RL: invalid arrShape'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adACTIVE_READ_GEN_RS
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE adACTIVE_READ_GEN_RS(
     I                               activeVar_file,
     I                               arrShape, myNr,
     I                               iRec,
     I                               globalFile,
     I                               useCurrentDir,
     I                               lAdInit,
     I                               myIter,
     I                               myThid,
     O                               adactive_var
     I                               )
#else
      SUBROUTINE adACTIVE_READ_GEN_RS(
     I                               activeVar_file,
     O                               adactive_var,
     I                               arrShape, myNr,
     I                               iRec,
     I                               globalFile,
     I                               useCurrentDir,
     I                               lAdInit,
     I                               myIter,
     I                               myThid,
     I                               dummy,
     O                               addummy )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adACTIVE_READ_GEN_RS
C     ==================================================================
C     o Adjoint of ACTIVE_READ_GEN_RS.
C     Accept active variable of various (XY,XZ,YZ) shape and level
C     number, according to arguments arrShape and myNr.
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     adactive_var   :: adoint variable array
C     arrShape       :: shape of active-var array ('XY','XZ' or 'YZ')
C     myNr           :: number of vertical-levels in active-var array
C     iRec           :: record number in file
C     globalFile     :: used for writing global (if =T) or tiled files
C     useCurrentDir  :: always read from the current directory
C                        (even if "mdsioLocalDir" is set)
C     lAdInit        :: initialisation of corresponding adjoint variable
C                        and write to active file
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: my Thread Id number
C     dummy          :: (needed for 2nd derivative code)
      CHARACTER*(*) activeVar_file
      _RS     adactive_var(*)
      CHARACTER*(2) arrShape
      INTEGER myNr
      INTEGER iRec
      LOGICAL globalFile
      LOGICAL useCurrentDir
      LOGICAL lAdInit
      INTEGER myIter
      INTEGER myThid
      _RS     dummy, addummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2)  pref
      CHARACTER*(80) fName
      INTEGER il
CEOP

      pref = 'ad'
      il   = ILNBLNK( activeVar_file )
      WRITE(fName(1:80),'(A)') ' '
      WRITE(fName(1:2+il),'(2A)') pref, activeVar_file(1:il)

      IF ( arrShape.EQ.'XY' ) THEN

        CALL ACTIVE_READ_3D_RS(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSEIF ( arrShape.EQ.'XZ' ) THEN

        CALL ACTIVE_READ_XZ_RS(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSEIF ( arrShape.EQ.'YZ' ) THEN

        CALL ACTIVE_READ_YZ_RS(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSE
        STOP
     &   'ABNORMAL END: S/R adACTIVE_READ_GEN_RS: invalid arrShape'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adACTIVE_WRITE_GEN_RL
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE adACTIVE_WRITE_GEN_RL(
     I                               activeVar_file,
     I                               arrShape, myNr,
     I                               iRec,
     I                               useCurrentDir,
     I                               myIter,
     I                               myThid,
     U                               adactive_var,
     I                               dummy )
#else
      SUBROUTINE adACTIVE_WRITE_GEN_RL(
     I                               activeVar_file,
     U                               adactive_var,
     I                               arrShape, myNr,
     I                               iRec,
     I                               useCurrentDir,
     I                               myIter,
     I                               myThid,
     I                               dummy )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adACTIVE_WRITE_GEN_RL
C     ==================================================================
C     o Adjoint of ACTIVE_WRITE_GEN_RL.
C     Accept active variable of various (XY,XZ,YZ) shape and level
C     number, according to arguments arrShape and myNr.
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     active_var     :: active variable array
C     arrShape       :: shape of active-var array ('XY','XZ' or 'YZ')
C     myNr           :: number of vertical-levels in active-var array
C     iRec           :: record number in file
C     useCurrentDir  :: always write to the current directory
C                        (even if "mdsioLocalDir" is set)
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: my Thread Id number
C     dummy          :: (needed for 2nd derivative code)
      CHARACTER*(*) activeVar_file
      _RL     adactive_var(*)
      CHARACTER*(2) arrShape
      INTEGER myNr
      INTEGER iRec
      LOGICAL useCurrentDir
      INTEGER myIter
      INTEGER myThid
      _RL     dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     globalFile     :: used for writing global (if =T) or tiled files
      CHARACTER*(2)  pref
      CHARACTER*(80) fName
      INTEGER il
      LOGICAL globalFile
CEOP

      pref = 'ad'
      il   = ILNBLNK( activeVar_file )
      WRITE(fName(1:80),'(A)') ' '
      WRITE(fName(1:2+il),'(2A)') pref, activeVar_file(1:il)
      globalFile = .FALSE.

      IF ( arrShape.EQ.'XY' ) THEN

        CALL ACTIVE_WRITE_3D_RL(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSEIF ( arrShape.EQ.'XZ' ) THEN

        CALL ACTIVE_WRITE_XZ_RL(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSEIF ( arrShape.EQ.'YZ' ) THEN

        CALL ACTIVE_WRITE_YZ_RL(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSE
        STOP
     &   'ABNORMAL END: S/R adACTIVE_WRITE_GEN_RL: invalid arrShape'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adACTIVE_WRITE_GEN_RS
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE adACTIVE_WRITE_GEN_RS(
     I                               activeVar_file,
     I                               arrShape, myNr,
     I                               iRec,
     I                               useCurrentDir,
     I                               myIter,
     I                               myThid,
     U                               adactive_var,
     I                               dummy )
#else
      SUBROUTINE adACTIVE_WRITE_GEN_RS(
     I                               activeVar_file,
     U                               adactive_var,
     I                               arrShape, myNr,
     I                               iRec,
     I                               useCurrentDir,
     I                               myIter,
     I                               myThid,
     I                               dummy )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adACTIVE_WRITE_GEN_RS
C     ==================================================================
C     o Adjoint of ACTIVE_WRITE_GEN_RS.
C     Accept active variable of various (XY,XZ,YZ) shape and level
C     number, according to arguments arrShape and myNr.
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     active_var     :: active variable array
C     arrShape       :: shape of active-var array ('XY','XZ' or 'YZ')
C     myNr           :: number of vertical-levels in active-var array
C     iRec           :: record number in file
C     useCurrentDir  :: always write to the current directory
C                        (even if "mdsioLocalDir" is set)
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: my Thread Id number
C     dummy          :: (needed for 2nd derivative code)
      CHARACTER*(*) activeVar_file
      _RS     adactive_var(*)
      CHARACTER*(2) arrShape
      INTEGER myNr
      INTEGER iRec
      LOGICAL useCurrentDir
      INTEGER myIter
      INTEGER myThid
      _RS     dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     globalFile     :: used for writing global (if =T) or tiled files
      CHARACTER*(2)  pref
      CHARACTER*(80) fName
      INTEGER il
      LOGICAL globalFile
CEOP

      pref = 'ad'
      il   = ILNBLNK( activeVar_file )
      WRITE(fName(1:80),'(A)') ' '
      WRITE(fName(1:2+il),'(2A)') pref, activeVar_file(1:il)
      globalFile = .FALSE.

      IF ( arrShape.EQ.'XY' ) THEN

        CALL ACTIVE_WRITE_3D_RS(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSEIF ( arrShape.EQ.'XZ' ) THEN

        CALL ACTIVE_WRITE_XZ_RS(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSEIF ( arrShape.EQ.'YZ' ) THEN

        CALL ACTIVE_WRITE_YZ_RS(
     &                 fName, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      ELSE
        STOP
     &   'ABNORMAL END: S/R adACTIVE_WRITE_GEN_RS: invalid arrShape'
      ENDIF

      RETURN
      END
