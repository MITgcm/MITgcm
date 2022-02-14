#include "AUTODIFF_OPTIONS.h"

C     ==================================================================
C     active_files_ad.F: Routines to handle the I/O of the active file
C                        for the adjoint calculations. All files are
C                        direct access files.
C     Routines
C     o  adactive_read_xy         - Adjoint of active_read_xy
C     o  adactive_read_xyz        - Adjoint of active_read_xyz
C     o  adactive_read_xz         - Adjoint of active_read_xz
C     o  adactive_read_yz         - Adjoint of active_read_yz
C     o  adactive_read_1d         - Adjoint of active_read_1d
C
C     o  adactive_write_xy        - Adjoint of active_write_xy
C     o  adactive_write_xyz       - Adjoint of active_write_xyz
C     o  adactive_write_xz        - Adjoint of active_write_xz
C     o  adactive_write_yz        - Adjoint of active_write_yz
C     o  adactive_write_1d        - Adjoint of active_write_1d
C
C        changed: Christian Eckert eckert@mit.edu 24-Apr-2000
C                 - Added routines that do active writes on tiles
C                   instead of a whole thread.
C        changed: Patrick Heimbach heimbach@mit.edu 27-May-2000
C                 - changed suboutine argument list:
C                   dropped mycurrentiter, mycurrenttime
C        changed: heimbach@mit.edu 25-Mar-2002
C                 - added active file handling of xz-/yz-arrays
C        changed: tsmith@oden.utexas.edu 22-Oct-2019
C                 - added 1D vectors
C     ==================================================================

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_read_xy
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_read_xy(
     I                             active_var_file,
     I                             iRec,
     I                             doglobalread,
     I                             lAdInit,
     I                             myIter,
     I                             myThid,
     I                             adactive_var
     &                           )
#else
      subroutine adactive_read_xy(
     I                             active_var_file,
     I                             adactive_var,
     I                             iRec,
     I                             doglobalread,
     I                             lAdInit,
     I                             myIter,
     I                             myThid,
     I                             dummy,
     O                             addummy
     &                           )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adactive_read_xy
C     ==================================================================
C     o Adjoint of active_read_xy.
C     started: Christian Eckert eckert@mit.edu 24-May-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     adactive_var   :: array
C     iRec           :: record number
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
C     doglobalread   :: flag for global or local read/write
C                       (default: .false.)
C     lAdInit        :: initialisation of corresponding adjoint
C                       variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL  dummy, addummy ! Tags for IO: ctrl input and adjoint (gradient) output

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') adpref, active_var_file(1:il)
      myNr = 1
      useCurrentDir = .FALSE.

      CALL ACTIVE_READ_3D_RL(
     &                 fname, adactive_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_read_xyz
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_read_xyz(
     I                              active_var_file,
     I                              iRec,
     I                              doglobalread,
     I                              lAdInit,
     I                              myIter,
     I                              myThid,
     I                              adactive_var
     &                            )
#else
      subroutine adactive_read_xyz(
     I                              active_var_file,
     I                              adactive_var,
     I                              iRec,
     I                              doglobalread,
     I                              lAdInit,
     I                              myIter,
     I                              myThid,
     I                              dummy,
     I                              addummy
     &                            )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adactive_read_xyz
C     ==================================================================
C     o Adjoint of active_read_xyz.
C     started: Christian Eckert eckert@mit.edu 24-May-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     adactive_var   :: array
C     iRec           :: record number
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
C     doglobalread   :: flag for global or local read/write
C                       (default: .false.)
C     lAdInit        :: initialisation of corresponding adjoint
C                       variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL  dummy, addummy ! Tags for IO: ctrl input and adjoint (gradient) output

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') adpref, active_var_file(1:il)
      myNr = Nr
      useCurrentDir = .FALSE.

      CALL ACTIVE_READ_3D_RL(
     &                 fname, adactive_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_read_xz
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_read_xz(
     I                              active_var_file,
     I                              iRec,
     I                              doglobalread,
     I                              lAdInit,
     I                              myIter,
     I                              myThid,
     I                              adactive_var
     &                            )
#else
      subroutine adactive_read_xz(
     I                              active_var_file,
     I                              adactive_var,
     I                              iRec,
     I                              doglobalread,
     I                              lAdInit,
     I                              myIter,
     I                              myThid,
     I                              dummy,
     O                              addummy
     &                            )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adactive_read_xz
C     ==================================================================
C     o Adjoint of active_read_xz.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     adactive_var   :: array
C     iRec           :: record number
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
C     doglobalread   :: flag for global or local read/write
C                       (default: .false.)
C     lAdInit        :: initialisation of corresponding adjoint
C                       variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL  dummy, addummy ! Tags for IO: ctrl input and adjoint (gradient) output

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') adpref, active_var_file(1:il)
      myNr = Nr
      useCurrentDir = .FALSE.

      CALL ACTIVE_READ_XZ_RL(
     &                 fname, adactive_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_read_yz
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_read_yz(
     I                              active_var_file,
     I                              iRec,
     I                              doglobalread,
     I                              lAdInit,
     I                              myIter,
     I                              myThid,
     I                              adactive_var
     &                            )
#else
      subroutine adactive_read_yz(
     I                              active_var_file,
     I                              adactive_var,
     I                              iRec,
     I                              doglobalread,
     I                              lAdInit,
     I                              myIter,
     I                              myThid,
     I                              dummy,
     O                              addummy
     &                            )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adactive_read_yz
C     ==================================================================
C     o Adjoint of active_read_yz.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     adactive_var   :: array
C     iRec           :: record number
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
C     doglobalread   :: flag for global or local read/write
C                       (default: .false.)
C     lAdInit        :: initialisation of corresponding adjoint
C                       variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL  dummy, addummy ! Tags for IO: ctrl input and adjoint (gradient) output

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') adpref, active_var_file(1:il)
      myNr = Nr
      useCurrentDir = .FALSE.

      CALL ACTIVE_READ_YZ_RL(
     &                 fname, adactive_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_read_1d
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_read_1d(
     I                             active_var_file,
     I                             active_var_length,
     I                             iRec,
     I                             lAdInit,
     I                             myIter,
     I                             myThid,
     I                             adactive_var
     &                           )
#else
      subroutine adactive_read_1d(
     I                             active_var_file,
     I                             adactive_var,
     I                             active_var_length,
     I                             iRec,
     I                             lAdInit,
     I                             myIter,
     I                             myThid,
     I                             dummy,
     O                             addummy
     &                           )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adactive_read_1d
C     ==================================================================
C     o Adjoint of active_read_1d.
C     started: Tim Smith tsmith@oden.utexas.edu 22-Oct-2019
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     adactive_var:  :: array
C     active_var_length :: length of array
C     iRec           :: record number
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
C     lAdInit        :: initialisation of corresponding adjoint
C                       variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     adactive_var(*)
      INTEGER active_var_length
      INTEGER iRec
      INTEGER myIter, myThid
      LOGICAL lAdInit
      _RL  dummy, addummy ! Tags for IO: ctrl input and adjoint (gradient) output

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
CEOP

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') adpref, active_var_file(1:il)

      CALL ACTIVE_READ_1D_RL(
     &                 fname, adactive_var, active_var_length,
     &                 lAdInit, iRec,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_write_xy
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_write_xy(
     I                              active_var_file,
     I                              iRec,
     I                              myIter,
     I                              myThid,
     U                              adactive_var,
     I                              dummy
     &                            )
#else
      subroutine adactive_write_xy(
     I                              active_var_file,
     U                              adactive_var,
     I                              iRec,
     I                              myIter,
     I                              myThid,
     I                              dummy
     &                            )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adactive_write_xy
C     ==================================================================
C     o Adjoint of active_write_xy.
C     started: Christian Eckert eckert@mit.edu 24-May-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     adactive_var   :: array
C     iRec           :: record number
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      _RL     dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') adpref, active_var_file(1:il)
      myNr = 1
      globalFile = .FALSE.
      useCurrentDir = .FALSE.

      CALL ACTIVE_WRITE_3D_RL(
     &                 fname, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_write_xyz
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_write_xyz(
     I                               active_var_file,
     I                               iRec,
     I                               myIter,
     I                               myThid,
     U                               adactive_var,
     I                               dummy
     &                             )
#else
      subroutine adactive_write_xyz(
     I                               active_var_file,
     U                               adactive_var,
     I                               iRec,
     I                               myIter,
     I                               myThid,
     I                               dummy
     &                             )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adactive_write_xyz
C     ==================================================================
C     o Adjoint of active_write_xyz.
C     started: Christian Eckert eckert@mit.edu 24-May-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     adactive_var   :: array
C     iRec           :: record number
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      _RL dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') adpref, active_var_file(1:il)
      myNr = Nr
      globalFile = .FALSE.
      useCurrentDir = .FALSE.

      CALL ACTIVE_WRITE_3D_RL(
     &                 fname, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_write_xz
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_write_xz(
     I                               active_var_file,
     I                               iRec,
     I                               myIter,
     I                               myThid,
     U                               adactive_var,
     I                               dummy
     &                             )
#else
      subroutine adactive_write_xz(
     I                               active_var_file,
     U                               adactive_var,
     I                               iRec,
     I                               myIter,
     I                               myThid,
     I                               dummy
     &                             )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adactive_write_xz
C     ==================================================================
C     o Adjoint of active_write_xz.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     adactive_var   :: array
C     iRec           :: record number
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      _RL dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') adpref, active_var_file(1:il)
      myNr = Nr
      globalFile = .FALSE.
      useCurrentDir = .FALSE.

      CALL ACTIVE_WRITE_XZ_RL(
     &                 fname, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_write_yz
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_write_yz(
     I                               active_var_file,
     I                               iRec,
     I                               myIter,
     I                               myThid,
     U                               adactive_var,
     I                               dummy
     &                             )
#else
      subroutine adactive_write_yz(
     I                               active_var_file,
     U                               adactive_var,
     I                               iRec,
     I                               myIter,
     I                               myThid,
     I                               dummy
     &                             )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adactive_write_yz
C     ==================================================================
C     o Adjoint of active_write_yz.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     adactive_var   :: array
C     iRec           :: record number
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      _RL dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') adpref, active_var_file(1:il)
      myNr = Nr
      globalFile = .FALSE.
      useCurrentDir = .FALSE.

      CALL ACTIVE_WRITE_YZ_RL(
     &                 fname, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_write_1d
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_write_1d(
     I                               active_var_file,
     I                               active_var_length,
     I                               iRec,
     I                               myIter,
     I                               myThid,
     U                               adactive_var,
     I                               dummy
     &                             )
#else
      subroutine adactive_write_1d(
     I                               active_var_file,
     U                               adactive_var,
     U                               active_var_length,
     I                               iRec,
     I                               myIter,
     I                               myThid,
     I                               dummy
     &                             )
#endif

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE adactive_write_1d
C     ==================================================================
C     o Adjoint of active_write_1d.
C     started: tsmith@oden.utexas.edu 22-Oct-2019
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     adactive_var   :: array
C     active_var_length :: length of array
C     iRec           :: record number
C     myIter         :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     adactive_var(*)
      INTEGER active_var_length
      INTEGER iRec
      INTEGER myIter, myThid
      _RL dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
CEOP

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') adpref, active_var_file(1:il)

      CALL ACTIVE_WRITE_1D_RL(
     &                 fname, adactive_var, active_var_length,
     &                 iRec, REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END
