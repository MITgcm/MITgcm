
#include "CPP_OPTIONS.h"

c     ==================================================================
c
c     active_file_control.F: Routines to handle the i/o of active vari-
c                            ables for the adjoint calculations. All
c                            files are direct access files.
c
c     Routines:
c
c     o  active_read_xz_rl      - Basic routine to handle active read
c                                 operations.
c     o  active_write_xz_rl     - Basic routine to handle active write
c                                 operations.
c     o  active_read_yz_rl      - Basic routine to handle active read
c                                 operations.
c     o  active_write_yz_rl     - Basic routine to handle active write
c                                 operations.
c
c     ==================================================================


CBOP
C     !ROUTINE: active_read_xz_rl
C     !INTERFACE:
      subroutine active_read_xz_rl(
     I                           active_var_file,
     O                           active_var,
     I                           globalfile,
     I                           lAdInit,
     I                           irec,
     I                           mynr,
     I                           theSimulationMode,
     I                           myOptimIter,
     I                           mythid
     &                         )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE active_read_xz_rl
c     ==================================================================
c     o Read an active sliced xz _RL variable from file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
c     SUBROUTINE active_read_xz_rl
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

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
c     mynr:            vertical array dimension
c     theSimulationMode: forward mode or reverse mode simulation
      character*(*) active_var_file
      logical  globalfile
      logical  lAdInit
      integer  irec
      integer  mynr
      integer  theSimulationMode
      integer  myOptimIter
      integer  mythid
      _RL     active_var(1-olx:snx+olx,mynr,nsx,nsy)

C     !LOCAL VARIABLES:
c     == local variables ==
      character*(2)  adpref
      character*(80) adfname
      integer bi,bj
      integer i,j,k
      integer oldprec
      integer prec
      integer il
      integer ilnblnk
      logical writeglobalfile
      _RL  active_data_t(1-olx:snx+olx,nsx,nsy)

c     == functions ==
      external ilnblnk

c     == end of interface ==
CEOP

c     force 64-bit io
      oldPrec        = readBinaryPrec
      readBinaryPrec = precFloat64
      prec           = precFloat64

      write(adfname(1:80),'(80a)') ' '
      adpref = 'ad'
      il = ilnblnk( active_var_file )

      write(adfname(1:2),'(a)') adpref
      write(adfname(3:il+2),'(a)') active_var_file(1:il)

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. FORWARD_SIMULATION) then

        _BEGIN_MASTER( mythid )

c       Read the active variable from file.

        call mdsreadfieldxz( 
     &                     active_var_file, 
     &                     prec, 
     &                     'RL',
     &                     mynr,      
     &                     active_var,
     &                     irec, 
     &                     mythid )

        if (lAdInit) then
c         Initialise the corresponding adjoint variable on the
c         adjoint variable's file. These files are tiled.

          writeglobalfile = .false.
          do bj = 1,nsy
             do bi = 1,nsx
                do i = 1,snx
                   active_data_t(i,bi,bj)= 0. _d 0
                enddo
             enddo
          enddo

          do k = 1,mynr
             call mdswritefieldxz( 
     &                           adfname, 
     &                           prec, 
     &                           globalfile,
     &                           'RL', 
     &                           1, 
     &                           active_data_t,
     &                           (irec-1)*mynr+k,
     &                           myOptimIter,
     &                           mythid )
          enddo
        endif

        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. REVERSE_SIMULATION) then

        _BEGIN_MASTER( mythid )

        writeglobalfile = .false.
        do k=1,mynr
c             Read data from file layer by layer.
           call mdsreadfieldxz( 
     &                        active_var_file, 
     &                        prec, 
     &                        'RL',
     &                        1, 
     &                        active_data_t, 
     &                        (irec-1)*mynr+k, 
     &                        mythid )

c             Add active_var from appropriate location to data.
           do bj = 1,nsy
              do bi = 1,nsx
                 do i = 1,snx
                    active_data_t(i,bi,bj) = active_data_t(i,bi,bj) +
     &                   active_var(i,k,bi,bj)
                 enddo
              enddo
           enddo

c             Store the result on disk.
           call mdswritefieldxz( 
     &                         active_var_file, 
     &                         prec,
     &                         writeglobalfile, 
     &                         'RL',
     &                         1, 
     &                         active_data_t, 
     &                         (irec-1)*mynr+k, 
     &                         myOptimIter,
     &                         mythid )
        enddo


c       Set active_var to zero.
        do k=1,mynr
           do bj = 1,nsy
              do bi = 1,nsx
                 do i = 1,snx
                    active_var(i,k,bi,bj) = 0. _d 0
                 enddo
              enddo
           enddo
        enddo

        _END_MASTER( mythid )
      endif

c     Reset default io precision.
      readBinaryPrec = oldPrec

      _BARRIER

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: active_write_xz_rl
C     !INTERFACE:
      subroutine active_write_xz_rl(
     I                            active_var_file,
     I                            active_var,
     I                            globalfile,
     I                            irec,
     I                            mynr,
     I                            theSimulationMode,
     I                            myOptimIter,
     I                            mythid
     &                          )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE active_write_xz_rl
c     ==================================================================
c     o Write an active xz _RL variable to a file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
c     SUBROUTINE active_write_xz_rl
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

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
c     mynr:            vertical array dimension
c     theSimulationMode: forward mode or reverse mode simulation
      character*(*) active_var_file
      integer  mynr
      logical  globalfile
      integer  irec
      integer  theSimulationMode
      integer  myOptimIter
      integer  mythid
      _RL     active_var(1-olx:snx+olx,mynr,nsx,nsy)

C     !LOCAL VARIABLES:
c     == local variables ==
      integer  i,j,k
      integer  bi,bj
      _RL  active_data_t(1-olx:snx+olx,nsx,nsy)
      integer  oldprec
      integer  prec

c     == end of interface ==
CEOP

c     force 64-bit io
      oldPrec        = readBinaryPrec
      readBinaryPrec = precFloat64
      prec           = precFloat64

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. FORWARD_SIMULATION) then

        _BEGIN_MASTER( mythid )

          call mdswritefieldxz( 
     &                        active_var_file, 
     &                        prec, 
     &                        globalfile,
     &                        'RL', 
     &                        mynr, 
     &                        active_var, 
     &                        irec, 
     &                        myOptimIter,
     &                        mythid )

        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. REVERSE_SIMULATION) then

        _BEGIN_MASTER( mythid )

            do k=1,mynr
c             Read data from file layer by layer.
              call mdsreadfieldxz( 
     &                           active_var_file, 
     &                           prec, 
     &                           'RL',
     &                            1,
     &                            active_data_t, 
     &                            (irec-1)*mynr+k, 
     &                            mythid )

c             Add active_var from appropriate location to data.
              do bj = 1,nsy
                 do bi = 1,nsx
                    do i = 1,snx
                       active_var(i,k,bi,bj) = 
     &                      active_var(i,k,bi,bj) +
     &                      active_data_t(i,bi,bj)
                       active_data_t(i,bi,bj) = 0. _d 0
                    enddo
                 enddo
              enddo
              call mdswritefieldxz( 
     &                            active_var_file, 
     &                            prec, 
     &                            globalfile,
     &                            'RL', 
     &                            1, 
     &                            active_data_t, 
     &                            (irec-1)*mynr+k, 
     &                            myOptimIter,
     &                            mythid )
        enddo

        _END_MASTER( mythid )

      endif

c     Reset default io precision.
      readBinaryPrec = oldPrec

      _BARRIER

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: active_read_yz_rl
C     !INTERFACE:
      subroutine active_read_yz_rl(
     I                           active_var_file,
     O                           active_var,
     I                           globalfile,
     I                           lAdInit,
     I                           irec,
     I                           mynr,
     I                           theSimulationMode,
     I                           myOptimIter,
     I                           mythid
     &                         )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE active_read_yz_rl
c     ==================================================================
c     o Read an active sliced yz _RL variable from file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
c     SUBROUTINE active_read_yz_rl
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

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
c     mynr:            vertical array dimension
c     theSimulationMode: forward mode or reverse mode simulation
      character*(*) active_var_file
      logical  globalfile
      logical  lAdInit
      integer  irec
      integer  mynr
      integer  theSimulationMode
      integer  myOptimIter
      integer  mythid
      _RL     active_var(1-oly:sny+oly,mynr,nsx,nsy)

C     !LOCAL VARIABLES:
c     == local variables ==
      character*(2)  adpref
      character*(80) adfname
      integer bi,bj
      integer i,j,k
      integer oldprec
      integer prec
      integer il
      integer ilnblnk
      logical writeglobalfile
      _RL  active_data_t(1-oly:sny+oly,nsx,nsy)

c     == functions ==
      external ilnblnk

c     == end of interface ==
CEOP

c     force 64-bit io
      oldPrec        = readBinaryPrec
      readBinaryPrec = precFloat64
      prec           = precFloat64

      write(adfname(1:80),'(80a)') ' '
      adpref = 'ad'
      il = ilnblnk( active_var_file )

      write(adfname(1:2),'(a)') adpref
      write(adfname(3:il+2),'(a)') active_var_file(1:il)

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. FORWARD_SIMULATION) then

        _BEGIN_MASTER( mythid )

c       Read the active variable from file.

        call mdsreadfieldyz( 
     &                     active_var_file, 
     &                     prec, 
     &                     'RL',
     &                     mynr,      
     &                     active_var,
     &                     irec, 
     &                     mythid )

        if (lAdInit) then
c         Initialise the corresponding adjoint variable on the
c         adjoint variable's file. These files are tiled.

          writeglobalfile = .false.
          do bj = 1,nsy
             do bi = 1,nsx
                do j = 1,sny
                   active_data_t(j,bi,bj)= 0. _d 0
                enddo
             enddo
          enddo

          do k = 1,mynr
             call mdswritefieldyz( 
     &                           adfname, 
     &                           prec, 
     &                           globalfile,
     &                           'RL', 
     &                           1, 
     &                           active_data_t,
     &                           (irec-1)*mynr+k,
     &                           myOptimIter,
     &                           mythid )
          enddo
        endif

        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. REVERSE_SIMULATION) then

        _BEGIN_MASTER( mythid )

        writeglobalfile = .false.
        do k=1,mynr
c             Read data from file layer by layer.
           call mdsreadfieldyz( 
     &                        active_var_file, 
     &                        prec, 
     &                        'RL',
     &                        1, 
     &                        active_data_t, 
     &                        (irec-1)*mynr+k, 
     &                        mythid )

c             Add active_var from appropriate location to data.
           do bj = 1,nsy
              do bi = 1,nsx
                 do j = 1,sny
                    active_data_t(j,bi,bj) = active_data_t(j,bi,bj) +
     &                   active_var(j,k,bi,bj)
                 enddo
              enddo
           enddo

c             Store the result on disk.
           call mdswritefieldyz( 
     &                         active_var_file, 
     &                         prec,
     &                         writeglobalfile, 
     &                         'RL',
     &                         1, 
     &                         active_data_t, 
     &                         (irec-1)*mynr+k, 
     &                         myOptimIter,
     &                         mythid )
        enddo


c       Set active_var to zero.
        do k=1,mynr
           do bj = 1,nsy
              do bi = 1,nsx
                 do j = 1,sny
                    active_var(j,k,bi,bj) = 0. _d 0
                 enddo
              enddo
           enddo
        enddo

        _END_MASTER( mythid )
      endif

c     Reset default io precision.
      readBinaryPrec = oldPrec

      _BARRIER

      return
      end

c     ==================================================================

CBOP
C     !ROUTINE: active_write_yz_rl
C     !INTERFACE:
      subroutine active_write_yz_rl(
     I                            active_var_file,
     I                            active_var,
     I                            globalfile,
     I                            irec,
     I                            mynr,
     I                            theSimulationMode,
     I                            myOptimIter,
     I                            mythid
     &                          )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE active_write_yz_rl
c     ==================================================================
c     o Write an active yz _RL variable to a file.
c     started: heimbach@mit.edu 05-Mar-2001
c     ==================================================================
c     SUBROUTINE active_write_yz_rl
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

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
c     mynr:            vertical array dimension
c     theSimulationMode: forward mode or reverse mode simulation
      character*(*) active_var_file
      integer  mynr
      logical  globalfile
      integer  irec
      integer  theSimulationMode
      integer  myOptimIter
      integer  mythid
      _RL     active_var(1-oly:sny+oly,mynr,nsx,nsy)

C     !LOCAL VARIABLES:
c     == local variables ==
      integer  i,j,k
      integer  bi,bj
      _RL  active_data_t(1-oly:sny+oly,nsx,nsy)
      integer  oldprec
      integer  prec

c     == end of interface ==
CEOP

c     force 64-bit io
      oldPrec        = readBinaryPrec
      readBinaryPrec = precFloat64
      prec           = precFloat64

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. FORWARD_SIMULATION) then

        _BEGIN_MASTER( mythid )

          call mdswritefieldyz( 
     &                        active_var_file, 
     &                        prec, 
     &                        globalfile,
     &                        'RL', 
     &                        mynr, 
     &                        active_var, 
     &                        irec, 
     &                        myOptimIter,
     &                        mythid )

        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. REVERSE_SIMULATION) then

        _BEGIN_MASTER( mythid )

            do k=1,mynr
c             Read data from file layer by layer.
              call mdsreadfieldyz( 
     &                           active_var_file, 
     &                           prec, 
     &                           'RL',
     &                            1,
     &                            active_data_t, 
     &                            (irec-1)*mynr+k, 
     &                            mythid )

c             Add active_var from appropriate location to data.
              do bj = 1,nsy
                 do bi = 1,nsx
                    do j = 1,sny
                       active_var(j,k,bi,bj) = 
     &                      active_var(j,k,bi,bj) +
     &                      active_data_t(j,bi,bj)
                       active_data_t(j,bi,bj) = 0. _d 0
                    enddo
                 enddo
              enddo
              call mdswritefieldyz( 
     &                            active_var_file, 
     &                            prec, 
     &                            globalfile,
     &                            'RL', 
     &                            1, 
     &                            active_data_t, 
     &                            (irec-1)*mynr+k, 
     &                            myOptimIter,
     &                            mythid )
        enddo

        _END_MASTER( mythid )

      endif

c     Reset default io precision.
      readBinaryPrec = oldPrec

      _BARRIER

      return
      end

