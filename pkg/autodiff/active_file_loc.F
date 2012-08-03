C $Header: /u/gcmpack/MITgcm/pkg/autodiff/active_file_loc.F,v 1.6 2012/08/03 18:49:34 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

C     ==================================================================
C     active_file.F: Routines to handle the I/O of the active file for
C                    the adjoint calculations. All files are direct
C                    access files.
C     Routines
C    o  active_read_xy_loc     - Read  an active 2D variable from file.
C    o  active_read_xyz_loc    - Read  an active 3D variable from file.
C    o  active_read_xz_loc     - Read  an active 2D xz-slice from file.
C    o  active_read_yz_loc     - Read  an active 2D yz-slice from file.
C
C    o  active_write_xy_loc    - Write an active 2D variable to a file.
C    o  active_write_xyz_loc   - Write an active 3D variable to a file.
C    o  active_write_xz_loc    - Write an active 2D xz-slice to a file.
C    o  active_write_yz_loc    - Write an active 2D yz-slice to a file.
C
C        changed: Christian Eckert eckert@mit.edu 24-Apr-2000
C                 - Added routines that do active writes on tiles
C                   instead of a whole thread.
C        changed: heimbach@mit.edu 05-Mar-2001
C                 - added active file handling of xz-/yz-arrays
C     ==================================================================

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: active_read_xy_loc
C     !INTERFACE:
      subroutine active_read_xy_loc(
     I                           active_var_file,
     O                           active_var,
     I                           iRec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           myThid
     I                         , dummy
     &                         )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_read_xy_loc
C     ==================================================================
C     o Read an active 2D (XY) variable from file.
C     started: Christian Eckert eckert@mit.edu 30-Jun-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     active_var:      array
C     iRec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     dummy

C     !LOCAL VARIABLES:
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      myNr = 1
      useCurrentDir = .TRUE.
      CALL ACTIVE_READ_3D_RL(
     &                 active_var_file, active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: active_read_xyz_loc
C     !INTERFACE:
      subroutine active_read_xyz_loc(
     I                            active_var_file,
     O                            active_var,
     I                            iRec,
     I                            doglobalread,
     I                            lAdInit,
     I                            myOptimIter,
     I                            myThid
     I                         , dummy
     &                           )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_read_xyz_loc
C     ==================================================================
C     o Read an active 3D variable from file.
C     started: Christian Eckert eckert@mit.edu 30-Jun-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     active_var:      array
C     iRec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      CHARACTER*(*) active_var_file
      _RL active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     dummy

C     !LOCAL VARIABLES:
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      myNr = Nr
      useCurrentDir = .TRUE.
      CALL ACTIVE_READ_3D_RL(
     &                 active_var_file, active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: active_read_xz_loc
C     !INTERFACE:
      subroutine active_read_xz_loc(
     I                           active_var_file,
     O                           active_var,
     I                           iRec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           myThid
     I                         , dummy
     &                         )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_read_xz_loc
C     ==================================================================
C     o Read an active 2D xz-slice from file.
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
C     active_var:      array
C     iRec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     dummy

C     !LOCAL VARIABLES:
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      myNr = Nr
      useCurrentDir = .TRUE.
      CALL ACTIVE_READ_XZ_RL(
     &                 active_var_file, active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: active_read_yz_loc
C     !INTERFACE:
       subroutine active_read_yz_loc(
     I                           active_var_file,
     O                           active_var,
     I                           iRec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           myThid
     I                         , dummy
     &                         )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_read_yz_loc
C     ==================================================================
C     o Read an active 2D yz-slice from file.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     \ev

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     active_var:      array
C     iRec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     dummy

C     !LOCAL VARIABLES:
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      myNr = Nr
      useCurrentDir = .TRUE.
      CALL ACTIVE_READ_YZ_RL(
     &                 active_var_file, active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: active_write_xy_loc
C     !INTERFACE:
      subroutine active_write_xy_loc(
     I                            active_var_file,
     I                            active_var,
     I                            iRec,
     I                            myOptimIter,
     I                            myThid
     I                         , dummy
     &                          )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_write_xy_loc
C     ==================================================================
C     o Write an active 2D variable to a file.
C     started: Christian Eckert eckert@mit.edu 30-Jun-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     active_var:      array
C     iRec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      _RL     dummy

C     !LOCAL VARIABLES:
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      myNr = 1
      globalFile = .FALSE.
      useCurrentDir = .TRUE.
      CALL ACTIVE_WRITE_3D_RL(
     &                 active_var_file, active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: active_write_xyz_loc
C     !INTERFACE:
      subroutine active_write_xyz_loc(
     I                             active_var_file,
     I                             active_var,
     I                             iRec,
     I                             myOptimIter,
     I                             myThid
     I                         , dummy
     &                           )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_write_xyz_loc
C     ==================================================================
C     o Write an active 3D variable to a file.
C     started: Christian Eckert eckert@mit.edu 30-Jun-1999
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     active_var:      array
C     iRec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      CHARACTER*(*) active_var_file
      _RL active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      _RL     dummy

C     !LOCAL VARIABLES:
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      myNr = Nr
      globalFile = .FALSE.
      useCurrentDir = .TRUE.
      CALL ACTIVE_WRITE_3D_RL(
     &                 active_var_file, active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: active_write_xz_loc
C     !INTERFACE:
      subroutine active_write_xz_loc(
     I                            active_var_file,
     I                            active_var,
     I                            iRec,
     I                            myOptimIter,
     I                            myThid
     I                         , dummy
     &                          )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_write_xz_loc
C     ==================================================================
C     o Write an active 2D xz-slice to a file.
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
C     active_var:      array
C     iRec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      _RL     dummy

C     !LOCAL VARIABLES:
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      myNr = Nr
      globalFile = .FALSE.
      useCurrentDir = .TRUE.
      CALL ACTIVE_WRITE_XZ_RL(
     &                 active_var_file, active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: active_write_yz_loc
C     !INTERFACE:
      subroutine active_write_yz_loc(
     I                            active_var_file,
     I                            active_var,
     I                            iRec,
     I                            myOptimIter,
     I                            myThid
     I                         , dummy
     &                          )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_write_yz_loc
C     ==================================================================
C     o Write an active 2D variable to a file.
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
C     active_var:      array
C     iRec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      _RL     dummy

C     !LOCAL VARIABLES:
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      myNr = Nr
      globalFile = .FALSE.
      useCurrentDir = .TRUE.
      CALL ACTIVE_WRITE_YZ_RL(
     &                 active_var_file, active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      RETURN
      END
