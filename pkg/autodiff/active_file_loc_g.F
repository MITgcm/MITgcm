
#include "CPP_OPTIONS.h"

c     ==================================================================
c
c     active_file_g.F: Routines to handle the I/O of the active file for
c                      the tangent linear calculations. All files are 
c                      direct access files.
c
c     Routines
c
c    o  g_active_read_xy         - Read  an active 2D variable from file.
c    o  g_active_read_xyz        - Read  an active 3D variable from file.
c    o  g_active_read_xz         - Read  an active 2D xz-slice from file.
c    o  g_active_read_yz         - Read  an active 2D yz-slice from file.
c
c    o  g_active_write_xy        - Write an active 2D variable to a file.
c    o  g_active_write_xyz       - Write an active 3D variable to a file.
c    o  g_active_write_xz        - Write an active 2D xz-slice to a file.
c    o  g_active_write_yz        - Write an active 2D yz-slice to a file.
c
c    o  g_active_read_tile_xy    - Read  an active 2D variable from file.
c    o  g_active_read_tile_xyz   - Read  an active 3D variable from file.
c
c    o  g_active_write_tile_xy   - Write an active 2D variable to a file.
c    o  g_active_write_tile_xyz  - Write an active 3D variable to a file.
c
c
c        changed: Christian Eckert eckert@mit.edu 24-Apr-2000
c                 - Added routines that do active writes on tiles
c                   instead of a whole thread.
c        changed: heimbach@mit.edu 05-Mar-2001
c                 - added active file handling of xz-/yz-arrays
c
c     ==================================================================


CBOP
C     !ROUTINE: g_active_read_xy_loc
C     !INTERFACE:
      subroutine g_active_read_xy_loc(
     I                           active_var_file,
     O                           active_var,
     I                           irec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           mythid,
     I                           dummy,
     I                           g_active_var
     &                         )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE g_active_read_xy_loc
c     ==================================================================
c     o Read an active 2D (XY) variable from file.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
c     doglobalread:    flag for global or local read/write
c                      (default: .false.)
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      character*(*) active_var_file
      _RL     active_var(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     g_active_var(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer irec
      integer myOptimIter
      integer mythid
      logical doglobalread
      logical lAdInit
      _RL     dummy

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      integer il
      integer ilnblnk
      character*(2) pref
      character*(80) fname

c     == end of interface ==
CEOP

      mynr = 1
      pref = 'g_'
      write(fname(1:80),'(a)') ' '
      il     = ilnblnk(active_var_file)
      write(fname(1:2),'(a)') pref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_read_rl_loc( active_var_file, active_var,
     &                     doglobalread, lAdInit, irec, mynr,
     &                     FORWARD_SIMULATION, myOptimIter, mythid)

      call active_read_rl_loc( fname, g_active_var, doglobalread,
     &                     doglobalread, lAdInit, irec, mynr,
     &                     TANGENT_SIMULATION, myOptimIter, mythid)

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: g_active_read_xyz_loc
C     !INTERFACE:
      subroutine g_active_read_xyz_loc(
     I                            active_var_file,
     O                            active_var,
     I                            irec,
     I                            doglobalread,
     I                            lAdInit,
     I                            myOptimIter,
     I                            mythid,
     I                            dummy,
     I                            g_active_var
     &                           )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE g_active_read_xyz_loc
c     ==================================================================
c     o Read an active 3D variable from file.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
c     doglobalread:    flag for global or local read/write
c                      (default: .false.)
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      character*(*) active_var_file
      _RL active_var(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_active_var(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      integer irec
      integer myOptimIter
      integer mythid
      logical doglobalread
      logical lAdInit
      _RL     dummy

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      integer il
      integer ilnblnk
      character*(2) pref
      character*(80) fname

c     == end of interface ==
CEOP

      mynr = nr
      pref = 'g_'
      write(fname(1:80),'(a)') ' '
      il     = ilnblnk(active_var_file)
      write(fname(1:2),'(a)') pref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_read_rl_loc( active_var_file, active_var,
     &                     doglobalread, lAdInit, irec, mynr,
     &                     FORWARD_SIMULATION, myOptimIter, mythid)

      call active_read_rl_loc( fname, g_active_var,
     &                     doglobalread, lAdInit, irec, mynr,
     &                     TANGENT_SIMULATION, myOptimIter, mythid)

      return
      end

c     ==================================================================


CBOP
C     !ROUTINE: g_active_read_xz
C     !INTERFACE:
      subroutine g_active_read_xz_loc(
     I                           active_var_file,
     O                           active_var,
     I                           irec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           mythid,
     I                           dummy,
     I                           g_active_var
     &                         )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE g_active_read_xz_loc
c     ==================================================================
c     o Read an active 2D xz-slice from file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
c     doglobalread:    flag for global or local read/write
c                      (default: .false.)
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      character*(*) active_var_file
      _RL     active_var(1-olx:snx+olx,nsx,nsy)
      _RL     g_active_var(1-olx:snx+olx,nsx,nsy)
      integer irec
      integer myOptimIter
      integer mythid
      logical doglobalread
      logical lAdInit
      _RL     dummy

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      integer il
      integer ilnblnk
      character*(2) pref
      character*(80) fname

c     == end of interface ==
CEOP

      mynr = nr
      pref = 'g_'
      write(fname(1:80),'(a)') ' '
      il     = ilnblnk(active_var_file)
      write(fname(1:2),'(a)') pref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_read_xz_rl_loc( active_var_file, active_var,
     &                     doglobalread, lAdInit, irec, mynr,
     &                     FORWARD_SIMULATION, myOptimIter, mythid)

      call active_read_xz_rl_loc( fname, g_active_var,
     &                     doglobalread, lAdInit, irec, mynr,
     &                     TANGENT_SIMULATION, myOptimIter, mythid)

      return
      end

c     ==================================================================


CBOP
C     !ROUTINE: g_active_read_yz_loc
C     !INTERFACE:
       subroutine g_active_read_yz_loc(
     I                           active_var_file,
     O                           active_var,
     I                           irec,
     I                           doglobalread,
     I                           lAdInit,
     I                           myOptimIter,
     I                           mythid,
     I                           dummy,
     I                           g_active_var
     &                         )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE g_active_read_yz_loc
c     ==================================================================
c     o Read an active 2D yz-slice from file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
c     doglobalread:    flag for global or local read/write
c                      (default: .false.)
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      character*(*) active_var_file
      _RL     active_var(1-oly:sny+oly,nsx,nsy)
      _RL     g_active_var(1-oly:sny+oly,nsx,nsy)
      integer irec
      integer myOptimIter
      integer mythid
      logical doglobalread
      logical lAdInit
      _RL     dummy

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      integer il
      integer ilnblnk
      character*(2) pref
      character*(80) fname

c     == end of interface ==
CEOP

      mynr = nr
      pref = 'g_'
      write(fname(1:80),'(a)') ' '
      il     = ilnblnk(active_var_file)
      write(fname(1:2),'(a)') pref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_read_yz_rl_loc( active_var_file, active_var,
     &                     doglobalread, lAdInit, irec, mynr,
     &                     FORWARD_SIMULATION, myOptimIter, mythid)

      call active_read_yz_rl_loc( fname, g_active_var,
     &                     doglobalread, lAdInit, irec, mynr,
     &                     TANGENT_SIMULATION, myOptimIter, mythid)

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: g_active_write_xy_loc
C     !INTERFACE:
      subroutine g_active_write_xy_loc(
     I                            active_var_file,
     I                            active_var,
     I                            irec,
     I                            myOptimIter,
     I                            mythid,
     I                            dummy,
     I                            g_active_var,
     I                            g_dummy
     &                          )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE g_active_write_xy_loc
c     ==================================================================
c     o Write an active 2D variable to a file.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
      character*(*) active_var_file
      _RL     active_var(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     g_active_var(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer irec
      integer myOptimIter
      integer mythid
      _RL     dummy
      _RL     g_dummy

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      logical globalfile
      integer il
      integer ilnblnk
      character*(2) pref
      character*(80) fname

c     == end of interface ==
CEOP

      mynr = 1
      globalfile = .false.
      pref = 'g_'
      write(fname(1:80),'(a)') ' '
      il     = ilnblnk(active_var_file)
      write(fname(1:2),'(a)') pref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_write_rl_loc( active_var_file, active_var,
     &                      globalfile, irec, mynr,
     &                      FORWARD_SIMULATION, myOptimIter, mythid )

      call active_write_rl_loc( fname, g_active_var,
     &                      globalfile, irec, mynr,
     &                      TANGENT_SIMULATION, myOptimIter, mythid )

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: g_active_write_xyz_loc
C     !INTERFACE:
      subroutine g_active_write_xyz_loc(
     I                             active_var_file,
     I                             active_var,
     I                             irec,
     I                             myOptimIter,
     I                             mythid,
     I                             dummy,
     I                             g_active_var,
     I                             g_dummy
     &                           )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE g_active_write_xyz_loc
c     ==================================================================
c     o Write an active 3D variable to a file.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
      character*(*) active_var_file
      _RL active_var(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_active_var(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      integer irec
      integer myOptimIter
      integer mythid
      _RL     dummy
      _RL     g_dummy

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      logical globalfile
      integer il
      integer ilnblnk
      character*(2) pref
      character*(80) fname

c     == end of interface ==
CEOP

      mynr = nr
      globalfile = .false.
      pref = 'g_'
      write(fname(1:80),'(a)') ' '
      il     = ilnblnk(active_var_file)
      write(fname(1:2),'(a)') pref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_write_rl_loc( active_var_file, active_var,
     &                     globalfile, irec, mynr,
     &                     FORWARD_SIMULATION, myOptimIter, mythid)

      call active_write_rl_loc( fname, g_active_var,
     &                     globalfile, irec, mynr,
     &                     TANGENT_SIMULATION, myOptimIter, mythid)

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: g_active_write_xz_loc
C     !INTERFACE:
      subroutine g_active_write_xz_loc(
     I                            active_var_file,
     I                            active_var,
     I                            irec,
     I                            myOptimIter,
     I                            mythid,
     I                            dummy,
     I                            g_active_var,
     I                            g_dummy
     &                          )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE g_active_write_xz_loc
c     ==================================================================
c     o Write an active 2D xz-slice to a file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
      character*(*) active_var_file
      _RL     active_var(1-olx:snx+olx,nsx,nsy)
      _RL     g_active_var(1-olx:snx+olx,nsx,nsy)
      integer irec
      integer myOptimIter
      integer mythid
      _RL     dummy
      _RL     g_dummy

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      logical globalfile
      integer il
      integer ilnblnk
      character*(2) pref
      character*(80) fname

c     == end of interface ==
CEOP

      mynr = nr
      globalfile = .false.
      pref = 'g_'
      write(fname(1:80),'(a)') ' '
      il     = ilnblnk(active_var_file)
      write(fname(1:2),'(a)') pref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_write_xz_rl_loc( active_var_file, active_var,
     &                      globalfile, irec, mynr,
     &                      FORWARD_SIMULATION, myOptimIter, mythid )

      call active_write_xz_rl_loc( fname, g_active_var,
     &                      globalfile, irec, mynr,
     &                      TANGENT_SIMULATION, myOptimIter, mythid )

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: g_active_write_yz_loc
C     !INTERFACE:
      subroutine g_active_write_yz_loc(
     I                            active_var_file,
     I                            active_var,
     I                            irec,
     I                            myOptimIter,
     I                            mythid,
     I                            dummy,
     I                            g_active_var,
     I                            g_dummy
     &                          )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE g_active_write_yz_loc
c     ==================================================================
c     o Write an active 2D variable to a file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
      character*(*) active_var_file
      _RL     active_var(1-oly:sny+oly,nsx,nsy)
      _RL     g_active_var(1-oly:sny+oly,nsx,nsy)
      integer irec
      integer myOptimIter
      integer mythid
      _RL     dummy
      _RL     g_dummy

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      logical globalfile
      integer il
      integer ilnblnk
      character*(2) pref
      character*(80) fname

c     == end of interface ==
CEOP

      mynr = nr
      globalfile = .false.
      pref = 'g_'
      write(fname(1:80),'(a)') ' '
      il     = ilnblnk(active_var_file)
      write(fname(1:2),'(a)') pref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_write_yz_rl_loc( active_var_file, active_var,
     &                      globalfile, irec, mynr,
     &                      FORWARD_SIMULATION, myOptimIter, mythid )

      call active_write_yz_rl_loc( fname, g_active_var,
     &                      globalfile, irec, mynr,
     &                      TANGENT_SIMULATION, myOptimIter, mythid )

      return
      end

