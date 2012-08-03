C $Header: /u/gcmpack/MITgcm/pkg/autodiff/active_file_loc_ad.F,v 1.8 2012/08/03 18:49:34 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

C     ==================================================================
C     active_files_ad.F: Routines to handle the I/O of the active file
C                        for the adjoint calculations. All files are
C                        direct access files.
C     Routines
C     o  adactive_read_xy_loc     - Adjoint of active_read_xy_loc
C     o  adactive_read_xyz_loc    - Adjoint of active_read_xyz_loc
C     o  adactive_read_xz_loc     - Adjoint of active_read_xz_loc
C     o  adactive_read_yz_loc     - Adjoint of active_read_yz_loc
C
C     o  adactive_write_xy_loc    - Adjoint of active_write_xy_loc
C     o  adactive_write_xyz_loc   - Adjoint of active_write_xyz_loc
C     o  adactive_write_xz_loc    - Adjoint of active_write_xz_loc
C     o  adactive_write_yz_loc    - Adjoint of active_write_yz_loc
C
C        changed: Christian Eckert eckert@mit.edu 24-Apr-2000
C                 - Added routines that do active writes on tiles
C                   instead of a whole thread.
C        changed: Patrick Heimbach heimbach@mit.edu 27-May-2000
C                 - changed suboutine argument list:
C                   dropped mycurrentiter, mycurrenttime
C        changed: heimbach@mit.edu 25-Mar-2002
C                 - added active file handling of xz-/yz-arrays
C     ==================================================================

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_read_xy_loc
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_read_xy_loc(
     I                             active_var_file,
     I                             iRec,
     I                             doglobalread,
     I                             lAdInit,
     I                             myIter,
     I                             myThid,
     I                             adactive_var
     &                           )
#else
      subroutine adactive_read_xy_loc(
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
C     SUBROUTINE adactive_read_xy_loc
C     ==================================================================
C     o Adjoint of active_read_xy_loc.
C     started: Christian Eckert eckert@mit.edu 24-May-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     adactive_var:    array
C     iRec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myIter,myThid
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
      useCurrentDir = .TRUE.

      CALL ACTIVE_READ_3D_RL(
     &                 fname, adactive_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_read_xyz_loc
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_read_xyz_loc(
     I                              active_var_file,
     I                              iRec,
     I                              doglobalread,
     I                              lAdInit,
     I                              myIter,
     I                              myThid,
     I                              adactive_var
     &                            )
#else
      subroutine adactive_read_xyz_loc(
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
C     SUBROUTINE adactive_read_xyz_loc
C     ==================================================================
C     o Adjoint of active_read_xyz_loc.
C     started: Christian Eckert eckert@mit.edu 24-May-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     adactive_var:    array
C     iRec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter,myThid
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
      useCurrentDir = .TRUE.

      CALL ACTIVE_READ_3D_RL(
     &                 fname, adactive_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_read_xz_loc
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_read_xz_loc(
     I                              active_var_file,
     I                              iRec,
     I                              doglobalread,
     I                              lAdInit,
     I                              myIter,
     I                              myThid,
     I                              adactive_var
     &                            )
#else
      subroutine adactive_read_xz_loc(
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
C     SUBROUTINE adactive_read_xz_loc
C     ==================================================================
C     o Adjoint of active_read_xz_loc.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     adactive_var:    array
C     iRec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter,myThid
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
      useCurrentDir = .TRUE.

      CALL ACTIVE_READ_XZ_RL(
     &                 fname, adactive_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_read_yz_loc
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_read_yz_loc(
     I                              active_var_file,
     I                              iRec,
     I                              doglobalread,
     I                              lAdInit,
     I                              myIter,
     I                              myThid,
     I                              adactive_var
     &                            )
#else
      subroutine adactive_read_yz_loc(
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
C     SUBROUTINE adactive_read_yz_loc
C     ==================================================================
C     o Adjoint of active_read_yz_loc.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     adactive_var:    array
C     iRec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter,myThid
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
      useCurrentDir = .TRUE.

      CALL ACTIVE_READ_YZ_RL(
     &                 fname, adactive_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_write_xy_loc
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_write_xy_loc(
     I                              active_var_file,
     I                              iRec,
     I                              myIter,
     I                              myThid,
     U                              adactive_var,
     I                              dummy
     &                            )
#else
      subroutine adactive_write_xy_loc(
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
C     SUBROUTINE adactive_write_xy_loc
C     ==================================================================
C     o Adjoint of active_write_xy_loc.
C     started: Christian Eckert eckert@mit.edu 24-May-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     adactive_var:    array
C     iRec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myIter,myThid
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
      useCurrentDir = .TRUE.

      CALL ACTIVE_WRITE_3D_RL(
     &                 fname, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_write_xyz_loc
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_write_xyz_loc(
     I                               active_var_file,
     I                               iRec,
     I                               myIter,
     I                               myThid,
     U                               adactive_var,
     I                               dummy
     &                             )
#else
      subroutine adactive_write_xyz_loc(
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
C     SUBROUTINE adactive_write_xyz_loc
C     ==================================================================
C     o Adjoint of active_write_xyz_loc.
C     started: Christian Eckert eckert@mit.edu 24-May-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     adactive_var:    array
C     iRec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter,myThid
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
      useCurrentDir = .TRUE.

      CALL ACTIVE_WRITE_3D_RL(
     &                 fname, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_write_xz_loc
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_write_xz_loc(
     I                               active_var_file,
     I                               iRec,
     I                               myIter,
     I                               myThid,
     U                               adactive_var,
     I                               dummy
     &                             )
#else
      subroutine adactive_write_xz_loc(
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
C     SUBROUTINE adactive_write_xz_loc
C     ==================================================================
C     o Adjoint of active_write_xz_loc.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     adactive_var:    array
C     iRec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLx:sNx+OLx,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter,myThid
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
      useCurrentDir = .TRUE.

      CALL ACTIVE_WRITE_XZ_RL(
     &                 fname, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: adactive_write_yz_loc
C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      subroutine adactive_write_yz_loc(
     I                               active_var_file,
     I                               iRec,
     I                               myIter,
     I                               myThid,
     U                               adactive_var,
     I                               dummy
     &                             )
#else
      subroutine adactive_write_yz_loc(
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
C     SUBROUTINE adactive_write_yz_loc
C     ==================================================================
C     o Adjoint of active_write_yz_loc.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     adactive_var:    array
C     iRec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     adactive_var(1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter,myThid
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
      useCurrentDir = .TRUE.

      CALL ACTIVE_WRITE_YZ_RL(
     &                 fname, adactive_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END
