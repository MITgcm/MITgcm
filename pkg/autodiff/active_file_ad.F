
#include "CPP_OPTIONS.h"

c     ==================================================================
c
c     active_files_ad.F: Routines to handle the I/O of the active file
c                        for the adjoint calculations. All files are
c                        direct access files.
c
c     Routines
c
c     o  adactive_read_xy         - Adjoint of active_read_xy
c     o  adactive_read_xyz        - Adjoint of active_read_xyz
c     o  adactive_write_xy        - Adjoint of active_write_xy
c     o  adactive_write_xyz       - Adjoint of active_write_xyz
c
c     o  adactive_read_tile_xy    - Adjoint of active_read_tile_xy
c     o  adactive_read_tile_xyz   - Adjoint of active_read_tile_xyz
c     o  adactive_write_tile_xy   - Adjoint of active_write_tile_xy
c     o  adactive_write_tile_xyz  - Adjoint of active_write_tile_xyz
c
c
c        changed: Christian Eckert eckert@mit.edu 24-Apr-2000
c                 - Added routines that do active writes on tiles
c                   instead of a whole thread.
c        changed: Patrick Heimbach heimbach@mit.edu 27-May-2000
c                 - changed suboutine argument list: 
c                   dropped mycurrentiter, mycurrenttime
c
c     ==================================================================

CBOP
C     !ROUTINE: adactive_read_xy
C     !INTERFACE:
      subroutine adactive_read_xy(
     I                             active_var_file,
     I                             irec,
     I                             doglobalread,
     I                             lAdInit,
     I                             myiter,
     I                             mythid,
     I                             adactive_var
     &                           )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE adactive_read_xy
c     ==================================================================
c     o Adjoint of active_read_xy.
c     started: Christian Eckert eckert@mit.edu 24-May-1999
c     ==================================================================
c     SUBROUTINE adactive_read_xy
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
c     adactive_var:    array
c     irec:            record number
c     myIter:          number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
c     doglobalread:    flag for global or local read/write
c                      (default: .false.)
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      character*(*) active_var_file
      _RL     adactive_var(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer irec
      integer myiter,mythid
      logical doglobalread
      logical lAdInit

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      integer il
      integer ilnblnk
      character*(2) adpref
      character*(80) fname

c     == functions ==
      external ilnblnk

c     == end of interface ==
CEOP

      mynr   = 1
      adpref = 'ad'
      write(fname(1:80),'(a)') ' '

      il     = ilnblnk(active_var_file)

      write(fname(1:2),'(a)') adpref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_read_rl( fname, adactive_var,
     &                     doglobalread, lAdInit, irec, mynr,
     &                     REVERSE_SIMULATION, myiter, mythid )

      end

CBOP
C     !ROUTINE: adactive_read_xyz
C     !INTERFACE:
      subroutine adactive_read_xyz(
     I                              active_var_file,
     I                              irec,
     I                              doglobalread,
     I                              lAdInit,
     I                              myiter,
     I                              mythid,
     I                              adactive_var
     &                            )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE adactive_read_xyz
c     ==================================================================
c     o Adjoint of active_read_xyz.
c     started: Christian Eckert eckert@mit.edu 24-May-1999
c     ==================================================================
c     SUBROUTINE adactive_read_xyz
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==
c     active_var_file: filename
c     adactive_var:    array
c     irec:            record number
c     myIter:          number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
c     doglobalread:    flag for global or local read/write
c                      (default: .false.)
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      character*(*) active_var_file
      _RL     adactive_var(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      integer irec
      integer myiter,mythid
      logical doglobalread
      logical lAdInit

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      integer il
      integer ilnblnk
      character*(2) adpref
      character*(80) fname

c     == functions ==
      external ilnblnk

c     == end of interface ==
CEOP

      mynr   = nr
      adpref = 'ad'
      write(fname(1:80),'(a)') ' '

      il     = ilnblnk(active_var_file)

      write(fname(1:2),'(a)') adpref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_read_rl( fname, adactive_var,
     &                     doglobalread, lAdInit, irec, mynr,
     &                     REVERSE_SIMULATION, myiter, mythid )

      end

CBOP
C     !ROUTINE: adactive_read_xy
C     !INTERFACE:
      subroutine adactive_write_xy(
     I                              active_var_file,
     I                              irec,
     I                              myiter,
     I                              mythid,
     U                              adactive_var,
     I                              dummy
     &                            )


C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE adactive_write_xy
c     ==================================================================
c     o Adjoint of active_write_xy.
c     started: Christian Eckert eckert@mit.edu 24-May-1999
c     ==================================================================
c     SUBROUTINE adactive_write_xy
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==
c     active_var_file: filename
c     adactive_var:    array
c     irec:            record number
c     myIter:          number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
      character*(*) active_var_file
      _RL     adactive_var(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer irec
      integer myiter,mythid
      _RL     dummy

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      integer il
      integer ilnblnk
      character*(2) adpref
      character*(80) fname
      logical globalfile

c     == functions ==
      external ilnblnk

c     == end of interface ==
CEOP

      mynr   = 1
      adpref = 'ad'
      write(fname(1:80),'(a)') ' '

      il     = ilnblnk(active_var_file)

      write(fname(1:2),'(a)') adpref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      globalfile = .false.

      call active_write_rl( fname, adactive_var,
     &                      globalfile, irec, mynr,
     &                      REVERSE_SIMULATION, myiter, mythid )

      end


CBOP
C     !ROUTINE: adactive_read_xyz
C     !INTERFACE:
      subroutine adactive_write_xyz(
     I                               active_var_file,
     I                               irec,
     I                               myiter,
     I                               mythid,
     U                               adactive_var,
     I                               dummy
     &                             )


C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE adactive_write_xyz
c     ==================================================================
c     o Adjoint of active_write_xyz.
c     started: Christian Eckert eckert@mit.edu 24-May-1999
c     ==================================================================
c     SUBROUTINE adactive_write_xyz
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==
c     active_var_file: filename
c     adactive_var:    array
c     irec:            record number
c     myIter:          number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
      character*(*) active_var_file
      _RL     adactive_var(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      integer irec
      integer myiter,mythid
      _RL dummy

C     !LOCAL VARIABLES:
c     == local variables ==
      integer mynr
      integer il
      integer ilnblnk
      character*(2) adpref
      character*(80) fname
      logical globalfile

c     == functions ==
      external ilnblnk

c     == end of interface ==
CEOP

      mynr   = nr
      adpref = 'ad'
      write(fname(1:80),'(a)') ' '

      il     = ilnblnk(active_var_file)

      write(fname(1:2),'(a)') adpref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      globalfile = .false.

      call active_write_rl( fname, adactive_var,
     &                      globalfile, irec, mynr,
     &                      REVERSE_SIMULATION, myiter, mythid )

      end



      subroutine adactive_read_tile_xy(
     I                                  active_var_file,
     I                                  irec,
     I                                  doglobalread,
     I                                  bi,
     I                                  bj,
     I                                  lAdInit,
     I                                  myOptimIter,
     I                                  mythid,
     U                                  adactive_var
     &                                )

c     ==================================================================
c     SUBROUTINE active_read_tile_xy
c     ==================================================================
c
c     o Adjoint of active_read_tile_xy.
c
c     started: Christian Eckert eckert@mit.edu 24-May-1999
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
c     changed: Patrick Heimbach heimbach@mit.edu 27-May-2000
c
c              - changed suboutine argument list: 
c                dropped mycurrentiter, mycurrenttime
c
c     ==================================================================
c     SUBROUTINE active_read_tile_xy
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      character*(*) active_var_file
      _RL     adactive_var(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer irec
      integer myOptimIter
      integer mythid
      integer bi
      integer bj
      logical doglobalread
      logical lAdInit

c     == local variables ==

      integer mynr
      integer il
      integer ilnblnk
      character*(2) adpref
      character*(80) fname

c     == functions ==

      external ilnblnk

c     == end of interface ==

      mynr   = 1
      adpref = 'ad'
      write(fname(1:80),'(a)') ' '

      il     = ilnblnk(active_var_file)

      write(fname(1:2),   '(a)') adpref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_read_tile_rl(
     I                          fname,
     U                          adactive_var,
     I                          doglobalread,
     I                          lAdInit,
     I                          irec,
     I                          mynr,
     I                          bi,
     I                          bj,
     I                          REVERSE_SIMULATION,
     I                          myOptimIter,
     I                          mythid
     &                        )

      end


      subroutine adactive_read_tile_xyz(
     I                                   active_var_file,
     I                                   irec,
     I                                   doglobalread,
     I                                   bi,
     I                                   bj,
     I                                   lAdInit,
     I                                   myOptimIter,
     I                                   mythid,
     U                                   adactive_var
     &                                 )

c     ==================================================================
c     SUBROUTINE adactive_read_tile_xyz
c     ==================================================================
c
c     o Adjoint of active_read_tile_xyz.
c
c     started: Christian Eckert eckert@mit.edu 24-May-1999
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
c     changed: Patrick Heimbach heimbach@mit.edu 27-May-2000
c
c              - changed suboutine argument list: 
c                dropped mycurrentiter, mycurrenttime
c
c     ==================================================================
c     SUBROUTINE adactive_read_tile_xyz
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      character*(*) active_var_file
      _RL     adactive_var(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      integer irec
      integer myOptimIter
      integer mythid
      integer bi
      integer bj
      logical doglobalread
      logical lAdInit

c     == local variables ==

      integer mynr
      integer il
      integer ilnblnk
      character*(2) adpref
      character*(80) fname

c     == functions ==

      external ilnblnk

c     == end of interface ==

      mynr   = nr
      adpref = 'ad'
      write(fname(1:80),'(a)') ' '

      il     = ilnblnk(active_var_file)

      write(fname(1:2),   '(a)') adpref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      call active_read_tile_rl(
     I                          fname,
     U                          adactive_var,
     I                          doglobalread,
     I                          lAdInit,
     I                          irec,
     I                          mynr,
     I                          bi,
     I                          bj,
     I                          REVERSE_SIMULATION,
     I                          myOptimIter,
     I                          mythid
     &                        )

      end


      subroutine adactive_write_tile_xy(
     I                                   active_var_file,
     I                                   irec,
     I                                   bi,
     I                                   bj,
     I                                   myOptimIter,
     I                                   mythid,
     U                                   adactive_var
     &                                 )


c     ==================================================================
c     SUBROUTINE adactive_write_tile_xy
c     ==================================================================
c
c     o Adjoint of active_write_tile_xy.
c
c     started: Christian Eckert eckert@mit.edu 24-May-1999
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
c     changed: Patrick Heimbach heimbach@mit.edu 27-May-2000
c
c              - changed suboutine argument list: 
c                dropped mycurrentiter, mycurrenttime
c
c     ==================================================================
c     SUBROUTINE adactive_write_tile_xy
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      character*(*) active_var_file
      _RL     adactive_var(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer irec
      integer bi
      integer bj
      integer myOptimIter
      integer mythid

c     == local variables ==

      integer mynr
      integer il
      integer ilnblnk
      character*(2) adpref
      character*(80) fname
      logical globalfile

c     == functions ==

      external ilnblnk

c     == end of interface ==

      mynr   = 1
      adpref = 'ad'
      write(fname(1:80),'(a)') ' '

      il     = ilnblnk(active_var_file)

      write(fname(1:2),   '(a)') adpref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      globalfile = .false.

      call active_write_tile_rl(
     I                           fname,
     U                           adactive_var,
     I                           globalfile,
     I                           irec,
     I                           mynr,
     I                           bi,
     I                           bj,
     I                           REVERSE_SIMULATION,
     I                           myOptimIter,
     I                           mythid
     &                         )

      end


      subroutine adactive_write_tile_xyz(
     I                                    active_var_file,
     I                                    irec,
     I                                    bi,
     I                                    bj,
     I                                    myOptimIter,
     I                                    mythid,
     U                                    adactive_var
     &                                  )


c     ==================================================================
c     SUBROUTINE adactive_write_tile_xyz
c     ==================================================================
c
c     o Adjoint of active_write_tile_xyz.
c
c     started: Christian Eckert eckert@mit.edu 24-May-1999
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
c     changed: Patrick Heimbach heimbach@mit.edu 27-May-2000
c
c              - changed suboutine argument list: 
c                dropped mycurrentiter, mycurrenttime
c
c     ==================================================================
c     SUBROUTINE adactive_write_tile_xyz
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

c     == routine arguments ==

      character*(*) active_var_file
      _RL     adactive_var(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      integer irec
      integer bi
      integer bj
      integer myOptimIter
      integer mythid

c     == local variables ==

      integer mynr
      integer il
      integer ilnblnk
      character*(2) adpref
      character*(80) fname
      logical globalfile

c     == functions ==

      external ilnblnk

c     == end of interface ==

      mynr   = nr
      adpref = 'ad'
      write(fname(1:80),'(a)') ' '

      il     = ilnblnk(active_var_file)

      write(fname(1:2),'(a)') adpref
      write(fname(3:2+il),'(a)') active_var_file(1:il)

      globalfile = .false.

      call active_write_tile_rl(
     I                           fname,
     U                           adactive_var,
     I                           globalfile,
     I                           irec,
     I                           mynr,
     I                           bi,
     I                           bj,
     I                           REVERSE_SIMULATION,
     I                           myOptimIter,
     I                           mythid
     &                         )

      end
