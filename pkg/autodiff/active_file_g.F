C $Header: /u/gcmpack/MITgcm/pkg/autodiff/active_file_g.F,v 1.10 2012/08/02 22:10:31 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

C     ==================================================================
C     active_file_g.F: Routines to handle the I/O of the active file for
C                      the tangent linear calculations. All files are
C                      direct access files.
C     Routines
C    o  g_active_read_xy         - Read  an active 2D variable from file.
C    o  g_active_read_xyz        - Read  an active 3D variable from file.
C    o  g_active_read_xz         - Read  an active 2D xz-slice from file.
C    o  g_active_read_yz         - Read  an active 2D yz-slice from file.
C
C    o  g_active_write_xy        - Write an active 2D variable to a file.
C    o  g_active_write_xyz       - Write an active 3D variable to a file.
C    o  g_active_write_xz        - Write an active 2D xz-slice to a file.
C    o  g_active_write_yz        - Write an active 2D yz-slice to a file.
C
C        changed: Christian Eckert eckert@mit.edu 24-Apr-2000
C                 - Added routines that do active writes on tiles
C                   instead of a whole thread.
C        changed: heimbach@mit.edu 05-Mar-2001
C                 - added active file handling of xz-/yz-arrays
C     ==================================================================

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: g_active_read_xy
C     !INTERFACE:
      subroutine g_active_read_xy(
     I                           active_var_file,
     O                           active_var,
     I                           g_active_var,
     I                           iRec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           myThid,
     I                           dummy,
     I                           g_dummy
     &                         )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE g_active_read_xy
C     ==================================================================
C     o Read an active 2D (XY) variable from file.
C     started: Christian Eckert eckert@mit.edu 30-Jun-1999
C     ==================================================================
C     SUBROUTINE g_active_read_xy
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
      _RL     g_active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     dummy, g_dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) pref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      pref = 'g_'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') pref, active_var_file(1:il)
      myNr = 1
      useCurrentDir = .FALSE.

      CALL ACTIVE_READ_3D_RL(
     &                 active_var_file, active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      CALL ACTIVE_READ_3D_RL(
     &                 fname, g_active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 TANGENT_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: g_active_read_xyz
C     !INTERFACE:
      subroutine g_active_read_xyz(
     I                            active_var_file,
     O                            active_var,
     I                            g_active_var,
     I                            iRec,
     I                            doglobalread,
     I                            lAdInit,
     I                            myOptimIter,
     I                            myThid,
     I                            dummy,
     I                            g_dummy
     &                           )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE g_active_read_xyz
C     ==================================================================
C     o Read an active 3D variable from file.
C     started: Christian Eckert eckert@mit.edu 30-Jun-1999
C     ==================================================================
C     SUBROUTINE g_active_read_xyz
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
      _RL g_active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     dummy, g_dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) pref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      pref = 'g_'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') pref, active_var_file(1:il)
      myNr = Nr
      useCurrentDir = .FALSE.

      CALL ACTIVE_READ_3D_RL(
     &                 active_var_file, active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      CALL ACTIVE_READ_3D_RL(
     &                 fname, g_active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 TANGENT_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: g_active_read_xz
C     !INTERFACE:
      subroutine g_active_read_xz(
     I                           active_var_file,
     O                           active_var,
     I                           g_active_var,
     I                           iRec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           myThid,
     I                           dummy,
     I                           g_dummy
     &                         )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE g_active_read_xz
C     ==================================================================
C     o Read an active 2D xz-slice from file.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE g_active_read_xz
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
      _RL     g_active_var(1-OLx:sNx+OLx,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     dummy, g_dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) pref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      pref = 'g_'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') pref, active_var_file(1:il)
      myNr = Nr
      useCurrentDir = .FALSE.

      CALL ACTIVE_READ_XZ_RL(
     &                 active_var_file, active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      CALL ACTIVE_READ_XZ_RL(
     &                 fname, g_active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 TANGENT_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: g_active_read_yz
C     !INTERFACE:
       subroutine g_active_read_yz(
     I                           active_var_file,
     O                           active_var,
     I                           g_active_var,
     I                           iRec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           myThid,
     I                           dummy,
     I                           g_dummy
     &                         )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE g_active_read_yz
C     ==================================================================
C     o Read an active 2D yz-slice from file.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE g_active_read_yz
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
      _RL     active_var(1-OLy:sNy+OLy,nSx,nSy)
      _RL     g_active_var(1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      _RL     dummy, g_dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) pref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      pref = 'g_'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') pref, active_var_file(1:il)
      myNr = Nr
      useCurrentDir = .FALSE.

      CALL ACTIVE_READ_YZ_RL(
     &                 active_var_file, active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      CALL ACTIVE_READ_YZ_RL(
     &                 fname, g_active_var, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 TANGENT_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: g_active_write_xy
C     !INTERFACE:
      subroutine g_active_write_xy(
     I                            active_var_file,
     I                            active_var,
     I                            g_active_var,
     I                            iRec,
     I                            myOptimIter,
     I                            myThid,
     I                            dummy,
     I                            g_dummy
     &                          )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE g_active_write_xy
C     ==================================================================
C     o Write an active 2D variable to a file.
C     started: Christian Eckert eckert@mit.edu 30-Jun-1999
C     ==================================================================
C     SUBROUTINE g_active_write_xy
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
      _RL     g_active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      _RL     dummy
      _RL     g_dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) pref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      pref = 'g_'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') pref, active_var_file(1:il)
      myNr = 1
      globalFile = .FALSE.
      useCurrentDir = .FALSE.

      CALL ACTIVE_WRITE_3D_RL(
     &                 active_var_file, active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      CALL ACTIVE_WRITE_3D_RL(
     &                 fname, g_active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 TANGENT_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: g_active_write_xyz
C     !INTERFACE:
      subroutine g_active_write_xyz(
     I                             active_var_file,
     I                             active_var,
     I                             g_active_var,
     I                             iRec,
     I                             myOptimIter,
     I                             myThid,
     I                             dummy,
     I                             g_dummy
     &                           )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE g_active_write_xyz
C     ==================================================================
C     o Write an active 3D variable to a file.
C     started: Christian Eckert eckert@mit.edu 30-Jun-1999
C     ==================================================================
C     SUBROUTINE g_active_write_xyz
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
      _RL g_active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      _RL     dummy
      _RL     g_dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) pref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      pref = 'g_'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') pref, active_var_file(1:il)
      myNr = Nr
      globalFile = .FALSE.
      useCurrentDir = .FALSE.

      CALL ACTIVE_WRITE_3D_RL(
     &                 active_var_file, active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      CALL ACTIVE_WRITE_3D_RL(
     &                 fname, g_active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 TANGENT_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: g_active_write_xz
C     !INTERFACE:
      subroutine g_active_write_xz(
     I                            active_var_file,
     I                            active_var,
     I                            g_active_var,
     I                            iRec,
     I                            myOptimIter,
     I                            myThid,
     I                            dummy,
     I                            g_dummy
     &                          )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE g_active_write_xz
C     ==================================================================
C     o Write an active 2D xz-slice to a file.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE g_active_write_xz
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
      _RL     g_active_var(1-OLx:sNx+OLx,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      _RL     dummy
      _RL     g_dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) pref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      pref = 'g_'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') pref, active_var_file(1:il)
      myNr = Nr
      globalFile = .FALSE.
      useCurrentDir = .FALSE.

      CALL ACTIVE_WRITE_XZ_RL(
     &                 active_var_file, active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      CALL ACTIVE_WRITE_XZ_RL(
     &                 fname, g_active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 TANGENT_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: g_active_write_yz
C     !INTERFACE:
      subroutine g_active_write_yz(
     I                            active_var_file,
     I                            active_var,
     I                            g_active_var,
     I                            iRec,
     I                            myOptimIter,
     I                            myThid,
     I                            dummy,
     I                            g_dummy
     &                          )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE g_active_write_yz
C     ==================================================================
C     o Write an active 2D variable to a file.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE g_active_write_yz
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
      _RL     g_active_var(1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      _RL     dummy
      _RL     g_dummy

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) pref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      pref = 'g_'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:2+il),'(2A)') pref, active_var_file(1:il)
      myNr = Nr
      globalFile = .FALSE.
      useCurrentDir = .FALSE.

      CALL ACTIVE_WRITE_YZ_RL(
     &                 active_var_file, active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 FORWARD_SIMULATION, myOptimIter, myThid )

      CALL ACTIVE_WRITE_YZ_RL(
     &                 fname, g_active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 TANGENT_SIMULATION, myOptimIter, myThid )

      RETURN
      END
