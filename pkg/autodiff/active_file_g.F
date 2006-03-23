
#include "AUTODIFF_OPTIONS.h"

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
c    o  g_active_read_profile      - Read  an active 1D variable from file.
c    o  g_active_write_profile     - Write an active 1D variable to a file.
c
c        changed: Christian Eckert eckert@mit.edu 24-Apr-2000
c                 - Added routines that do active writes on tiles
c                   instead of a whole thread.
c        changed: heimbach@mit.edu 05-Mar-2001
c                 - added active file handling of xz-/yz-arrays
c
c     ==================================================================


CBOP
C     !ROUTINE: g_active_read_xy
C     !INTERFACE:
      subroutine g_active_read_xy(
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
c     SUBROUTINE g_active_read_xy
c     ==================================================================
c     o Read an active 2D (XY) variable from file.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
c     SUBROUTINE g_active_read_xy
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

      call active_read_rl( active_var_file, active_var, doglobalread,
     &                     lAdInit, irec, mynr,
     &                     FORWARD_SIMULATION, myOptimIter, mythid)

      call active_read_rl( fname, g_active_var, doglobalread,
     &                     lAdInit, irec, mynr,
     &                     TANGENT_SIMULATION, myOptimIter, mythid)

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: g_active_read_xyz
C     !INTERFACE:
      subroutine g_active_read_xyz(
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
c     SUBROUTINE g_active_read_xyz
c     ==================================================================
c     o Read an active 3D variable from file.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
c     SUBROUTINE g_active_read_xyz
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

      call active_read_rl( active_var_file, active_var, doglobalread,
     &                     lAdInit, irec, mynr,
     &                     FORWARD_SIMULATION, myOptimIter, mythid)

      call active_read_rl( fname, g_active_var, doglobalread,
     &                     lAdInit, irec, mynr,
     &                     TANGENT_SIMULATION, myOptimIter, mythid)

      return
      end

c     ==================================================================


CBOP
C     !ROUTINE: g_active_read_xz
C     !INTERFACE:
      subroutine g_active_read_xz(
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
c     SUBROUTINE g_active_read_xz
c     ==================================================================
c     o Read an active 2D xz-slice from file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
c     SUBROUTINE g_active_read_xz
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

      call active_read_xz_rl( active_var_file, active_var, doglobalread,
     &                     lAdInit, irec, mynr,
     &                     FORWARD_SIMULATION, myOptimIter, mythid)

      call active_read_xz_rl( fname, g_active_var, doglobalread,
     &                     lAdInit, irec, mynr,
     &                     TANGENT_SIMULATION, myOptimIter, mythid)

      return
      end

c     ==================================================================


CBOP
C     !ROUTINE: g_active_read_yz
C     !INTERFACE:
       subroutine g_active_read_yz(
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
c     SUBROUTINE g_active_read_yz
c     ==================================================================
c     o Read an active 2D yz-slice from file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
c     SUBROUTINE g_active_read_yz
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

      call active_read_yz_rl( active_var_file, active_var, doglobalread,
     &                     lAdInit, irec, mynr,
     &                     FORWARD_SIMULATION, myOptimIter, mythid)

      call active_read_yz_rl( fname, g_active_var, doglobalread,
     &                     lAdInit, irec, mynr,
     &                     TANGENT_SIMULATION, myOptimIter, mythid)

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: g_active_write_xy
C     !INTERFACE:
      subroutine g_active_write_xy(
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
c     SUBROUTINE g_active_write_xy
c     ==================================================================
c     o Write an active 2D variable to a file.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
c     SUBROUTINE g_active_write_xy
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

      call active_write_rl( active_var_file, active_var, globalfile,
     &                      irec, mynr,
     &                      FORWARD_SIMULATION, myOptimIter, mythid )

      call active_write_rl( fname, g_active_var, globalfile,
     &                      irec, mynr,
     &                      TANGENT_SIMULATION, myOptimIter, mythid )

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: g_active_write_xyz
C     !INTERFACE:
      subroutine g_active_write_xyz(
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
c     SUBROUTINE g_active_write_xyz
c     ==================================================================
c     o Write an active 3D variable to a file.
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     ==================================================================
c     SUBROUTINE g_active_write_xyz
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

      call active_write_rl( active_var_file, active_var, globalfile,
     &                     irec, mynr,
     &                     FORWARD_SIMULATION, myOptimIter, mythid)

      call active_write_rl( fname, g_active_var, globalfile,
     &                     irec, mynr,
     &                     TANGENT_SIMULATION, myOptimIter, mythid)

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: g_active_write_xz
C     !INTERFACE:
      subroutine g_active_write_xz(
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
c     SUBROUTINE g_active_write_xz
c     ==================================================================
c     o Write an active 2D xz-slice to a file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
c     SUBROUTINE g_active_write_xz
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

      call active_write_xz_rl( active_var_file, active_var, globalfile,
     &                      irec, mynr,
     &                      FORWARD_SIMULATION, myOptimIter, mythid )

      call active_write_xz_rl( fname, g_active_var, globalfile,
     &                      irec, mynr,
     &                      TANGENT_SIMULATION, myOptimIter, mythid )

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: g_active_write_yz
C     !INTERFACE:
      subroutine g_active_write_yz(
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
c     SUBROUTINE g_active_write_yz
c     ==================================================================
c     o Write an active 2D variable to a file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
c     SUBROUTINE g_active_write_yz
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

      call active_write_yz_rl( active_var_file, active_var, globalfile,
     &                      irec, mynr,
     &                      FORWARD_SIMULATION, myOptimIter, mythid )

      call active_write_yz_rl( fname, g_active_var, globalfile,
     &                      irec, mynr,
     &                      TANGENT_SIMULATION, myOptimIter, mythid )

      return
      end

c     ==================================================================

      subroutine g_active_read_tile_xy(
     I                                active_var_file,
     O                                active_var,
     I                                irec,
     I                                doglobalread,
     I                                bi,
     I                                bj,
     I                                lAdInit,
     I                                myOptimIter,
     I                                mythid
     &                              )

c     ==================================================================
c     SUBROUTINE g_active_read_tile_xy
c     ==================================================================
c
c     o Read an active 2D variable from file.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 11-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.c
c
c     changed: Christian Eckert eckert@mit.edu 24-Apr-2000
c
c              - Added routines that do active writes on tiles
c                instead of a whole thread.
c
c     ==================================================================
c     SUBROUTINE g_active_read_tile_xy
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      character*(*) active_var_file
      _RL     active_var(1-olx:snx+olx,1-oly:sny+oly)
      integer irec
      integer myOptimIter
      integer mythid
      integer bi
      integer bj
      logical doglobalread
      logical lAdInit

c     == local variables ==

      integer mynr

c     == end of interface ==

      mynr = 1
      call active_read_tile_rl(
     I                          active_var_file,
     I                          active_var,
     I                          doglobalread,
     I                          lAdInit,
     I                          irec,
     I                          mynr,
     I                          bi,
     I                          bj,
     I                          TANGENT_SIMULATION,
     I                          myOptimIter,
     I                          mythid
     &                        )

      return
      end


      subroutine g_active_read_tile_xyz(
     I                                 active_var_file,
     O                                 active_var,
     I                                 irec,
     I                                 doglobalread,
     I                                 bi,
     I                                 bj,
     I                                 lAdInit,
     I                                 myOptimIter,
     I                                 mythid
     &                               )

c     ==================================================================
c     SUBROUTINE g_active_read_tile_xyz
c     ==================================================================
c
c     o Read an active 3D variable from file.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 11-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.c
c
c     changed: Christian Eckert eckert@mit.edu 24-Apr-2000
c
c              - Added routines that do active writes on tiles
c                instead of a whole thread.
c
c     ==================================================================
c     SUBROUTINE g_active_read_tile_xyz
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      character*(*) active_var_file
      _RL active_var(1-olx:snx+olx,1-oly:sny+oly,nr)
      integer irec
      integer myOptimIter
      integer mythid
      logical doglobalread
      integer bi
      integer bj
      logical lAdInit

c     == local variables ==

      integer mynr

c     == end of interface ==

      mynr = nr

      call active_read_tile_rl(
     I                          active_var_file,
     I                          active_var,
     I                          doglobalread,
     I                          lAdInit,
     I                          irec,
     I                          mynr,
     I                          bi,
     I                          bj,
     I                          TANGENT_SIMULATION,
     I                          myOptimIter,
     I                          mythid
     &                        )

      return
      end


      subroutine g_active_write_tile_xy(
     I                                 active_var_file,
     I                                 active_var,
     I                                 irec,
     I                                 bi,
     I                                 bj,
     I                                 myOptimIter,
     I                                 mythid
     &                               )

c     ==================================================================
c     SUBROUTINE g_active_write_tile_xy
c     ==================================================================
c
c     o Write an active 2D variable to a file.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 11-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.c
c
c     changed: Christian Eckert eckert@mit.edu 24-Apr-2000
c
c              - Added routines that do active writes on tiles
c                instead of a whole thread.
c
c     ==================================================================
c     SUBROUTINE g_active_write_tile_xy
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      character*(*) active_var_file
      _RL     active_var(1-olx:snx+olx,1-oly:sny+oly)
      integer irec
      integer bi
      integer bj
      integer myOptimIter
      integer mythid

c     == local variables ==

      integer mynr
      logical globalfile

c     == end of interface ==

      mynr = 1
      globalfile = .false.

      call active_write_tile_rl(
     I                           active_var_file,
     I                           active_var,
     I                           globalfile,
     I                           irec,
     I                           mynr,
     I                           bi,
     I                           bj,
     I                           TANGENT_SIMULATION,
     I                           myOptimIter,
     I                           mythid
     &                         )

      return
      end


      subroutine g_active_write_tile_xyz(
     I                                  active_var_file,
     I                                  active_var,
     I                                  irec,
     I                                  bi,
     I                                  bj,
     I                                  myOptimIter,
     I                                  mythid
     &                                )

c     ==================================================================
c     SUBROUTINE g_active_write_tile_xyz
c     ==================================================================
c
c     o Write an active 3D variable to a file.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 11-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.c
c
c     changed: Christian Eckert eckert@mit.edu 24-Apr-2000
c
c              - Added routines that do active writes on tiles
c                instead of a whole thread.
c
c     ==================================================================
c     SUBROUTINE g_active_write_tile_xyz
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      character*(*) active_var_file
      _RL active_var(1-olx:snx+olx,1-oly:sny+oly,nr)
      integer irec
      integer bi
      integer bj
      integer myOptimIter
      integer mythid

c     == local variables ==

      integer mynr
      logical globalfile

c     == end of interface ==

      mynr = nr
      globalfile = .false.
      call active_write_tile_rl(
     I                           active_var_file,
     I                           active_var,
     I                           globalfile,
     I                           irec,
     I                           mynr,
     I                           bi,
     I                           bj,
     I                           TANGENT_SIMULATION,
     I                           myOptimIter,
     I                           mythid
     &                         )

      return
      end

c     ==================================================================
c     ==================================================================


      subroutine g_active_read_profile(
     I                           active_num_file,
     I                           nactive_var,
     O                           active_var,
     I                           active_varnum,
     I                           irec,
     I                           lAdInit,
     I                           myOptimIter,
     I                           mythid,
     I                           dummy,
     I                           g_active_var
     &                         )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE g_active_read_profile
c     ==================================================================
c     o Read an active 1D record from an profile data file.
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "profiles.h"
c     == routine arguments ==
c     active_var_file: filename
c     nactive_var:     integer size of active_var
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
c     doglobalread:    flag for global or local read/write
c                      (default: .false.)
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      integer nactive_var
      _RL  active_var(nactive_var)
      _RL  g_active_var(nactive_var)
      integer irec,active_varnum,active_num_file
      integer myOptimIter
      integer mythid
      logical lAdInit
      _RL     dummy

      call active_read_profile_rl( fidforward(active_num_file),
     &     active_num_file,
     & nactive_var,  active_var, active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec),
     & TANGENT_SIMULATION, myOptimIter, mythid)

      call active_read_profile_rl( fidtangent(active_num_file),
     &     active_num_file,
     & nactive_var, g_active_var,active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec),
     & TANGENT_SIMULATION, myOptimIter, mythid)


      return
      end


c     ==================================================================
c     ==================================================================
c     ==================================================================

      subroutine g_active_write_profile(
     I                           active_num_file,
     I                           nactive_var,
     I                           active_var,
     I                           active_varnum,
     I                           irec,
     I                           myOptimIter,
     I                           mythid,
     I                           dummy,
     I                           g_active_var,
     I                           g_dummy
     &                                )

c     ==================================================================
c     SUBROUTINE g_active_write_profile
c     ==================================================================
c
c     o Write an active 1D variable to a file.
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "profiles.h"

c     == routine arguments ==
c     active_var_file: filename
c     nactive_var:     integer size of active_var
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
      integer nactive_var,active_num_file
      _RL  active_var(nactive_var)
      _RL  g_active_var(nactive_var)
      integer irec,active_varnum
      integer myOptimIter
      integer mythid
      _RL     dummy
      _RL     g_dummy


      call active_write_profile_rl( fidforward(active_num_file) ,
     &     active_num_file,
     & nactive_var, active_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec),
     & TANGENT_SIMULATION, myOptimIter, mythid)

      call active_write_profile_rl( fidtangent(active_num_file) ,
     &     active_num_file,
     & nactive_var, g_active_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec),
     & TANGENT_SIMULATION, myOptimIter, mythid)



      return
      end

c     ==================================================================



