
#include "AUTODIFF_OPTIONS.h"

c     ==================================================================
c
c     active_file_control.F: Routines to handle the i/o of active vari-
c                            ables for the adjoint calculations. All
c                            files are direct access files.
c
c     Routines:
c
c     o  active_read_rl         - Basic routine to handle active read
c                                 operations.
c     o  active_write_rl        - Basic routine to handle active write
c                                 operations.
c
c     o  active_read_tile_rl    - Basic routine to handle active read
c                                 operations.
c     o  active_write_tile_rl   - Basic routine to handle active write
c                                 operations.
c
c
c        changed: Christian Eckert eckert@mit.edu 24-Apr-2000
c                 - Added routines that do active writes on tiles
c                   instead of a whole thread.
c
c     ==================================================================


CBOP
C     !ROUTINE: active_read_rl
C     !INTERFACE:
      subroutine active_read_rl(
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
c     SUBROUTINE active_read_rl
c     ==================================================================
c     o Read an active _RL variable from file.
c     The variable *globalfile* can be used as a switch, which allows
c     to read from a global file. The adjoint files are, however, always
c     treated as tiled files.
c     started: Christian Eckert eckert@mit.edu    Jan-1999
c     ==================================================================
c     SUBROUTINE active_read_rl
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

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
      _RL     active_var(1-olx:snx+olx,1-oly:sny+oly,mynr,nsx,nsy)

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
      _RL  active_data_t(1-olx:snx+olx,1-oly:sny+oly,mynr,nsx,nsy)

c     == functions ==
      external ilnblnk

c     == end of interface ==
CEOP

c     force 64-bit io
      oldPrec        = readBinaryPrec
      readBinaryPrec = ctrlprec
      prec           = ctrlprec

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

        call mdsreadfield( 
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
                do k = 1,mynr
                   do j=1,sny
                      do i=1,snx
                         active_data_t(i,j,k,bi,bj)= 0. _d 0
                      enddo
                   enddo
                enddo
             enddo
          enddo

          call mdswritefield( 
     &                           adfname, 
     &                           prec, 
     &                           globalfile,
     &                           'RL', 
     &                           mynr, 
     &                           active_data_t,
     &                           irec,
     &                           myOptimIter,
     &                           mythid )
        endif

        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. REVERSE_SIMULATION) then

        _BEGIN_MASTER( mythid )

        writeglobalfile = .false.
           call mdsreadfield( 
     &                        active_var_file, 
     &                        prec, 
     &                        'RL',
     &                        mynr, 
     &                        active_data_t, 
     &                        irec, 
     &                        mythid )

c             Add active_var from appropriate location to data.
           do bj = 1,nsy
              do bi = 1,nsx
                 do k = 1,mynr
                    do j=1,sny
                       do i=1,snx
                          active_data_t(i,j,k,bi,bj) = 
     &                         active_data_t(i,j,k,bi,bj) +
     &                         active_var(i,j,k,bi,bj)
                       enddo
                    enddo
                 enddo
              enddo
           enddo
              
c             Store the result on disk.
           call mdswritefield( 
     &                         active_var_file, 
     &                         prec,
     &                         writeglobalfile, 
     &                         'RL',
     &                         mynr, 
     &                         active_data_t, 
     &                         irec, 
     &                         myOptimIter,
     &                         mythid )

c       Set active_var to zero.
        do bj = 1,nsy
           do bi = 1,nsx
              do k=1,mynr
                 do j=1,sny
                    do i=1,snx
                       active_var(i,j,k,bi,bj) = 0. _d 0
                    enddo
                 enddo
              enddo
           enddo
        enddo

        _END_MASTER( mythid )
      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. TANGENT_SIMULATION) then

        _BEGIN_MASTER( mythid )

c       Read the active variable from file.

        call mdsreadfield( 
     &                     active_var_file, 
     &                     prec, 
     &                     'RL',
     &                     mynr,      
     &                     active_var,
     &                     irec, 
     &                     mythid )

        _END_MASTER( mythid )
      endif

c     Reset default io precision.
      readBinaryPrec = oldPrec

      _BARRIER

      return
      end


CBOP
C     !ROUTINE: active_write_rl
C     !INTERFACE:
      subroutine active_write_rl(
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
c     SUBROUTINE active_write_rl
c     ==================================================================
c     o Write an active _RL variable to a file.
c     started: Christian Eckert eckert@mit.edu    Jan-1999
c     ==================================================================
c     SUBROUTINE active_write_rl
c     ==================================================================
C     \ev

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

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
      _RL     active_var(1-olx:snx+olx,1-oly:sny+oly,mynr,nsx,nsy)

C     !LOCAL VARIABLES:
c     == local variables ==
      integer  i,j,k
      integer  bi,bj
      _RL  active_data_t(1-olx:snx+olx,1-oly:sny+oly,mynr,nsx,nsy)
      integer  oldprec
      integer  prec

c     == end of interface ==
CEOP

c     force 64-bit io
      oldPrec        = readBinaryPrec
      readBinaryPrec = ctrlprec
      prec           = ctrlprec

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. FORWARD_SIMULATION) then

        _BEGIN_MASTER( mythid )

          call mdswritefield( 
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

        call mdsreadfield( 
     &                           active_var_file, 
     &                           prec, 
     &                           'RL',
     &                            mynr,
     &                            active_data_t, 
     &                            irec, 
     &                            mythid )

c             Add active_var from appropriate location to data.
        do bj = 1,nsy
           do bi = 1,nsx
              do k = 1, mynr
                 do j=1,sny
                    do i=1,snx
                       active_var(i,j,k,bi,bj) = 
     &                      active_var(i,j,k,bi,bj) +
     &                      active_data_t(i,j,k,bi,bj)
                       active_data_t(i,j,k,bi,bj) = 0. _d 0
                    enddo
                 enddo
              enddo
           enddo
        enddo
        call mdswritefield( 
     &                            active_var_file, 
     &                            prec, 
     &                            globalfile,
     &                            'RL', 
     &                            mynr, 
     &                            active_data_t, 
     &                            irec, 
     &                            myOptimIter,
     &                            mythid )

        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. TANGENT_SIMULATION) then

        _BEGIN_MASTER( mythid )

          call mdswritefield( 
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

c     Reset default io precision.
      readBinaryPrec = oldPrec

      _BARRIER

      return
      end


      subroutine active_read_tile_rl(
     I                                active_var_file,
     O                                active_var,
     I                                globalfile,
     I                                lAdInit,
     I                                irec,
     I                                mynr,
     I                                itile,
     I                                jtile,
     I                                theSimulationMode,
     I                                myOptimIter,
     I                                mythid
     &                              )

c     ==================================================================
c     SUBROUTINE active_read_tile_rl
c     ==================================================================
c
c     o Read an active variable from file.
c
c     The variable *globalfile* can be used as a switch, which allows
c     to read from a global file. The adjoint files are, however, always
c     treated as tiled files.
c
c     started: Christian Eckert eckert@mit.edu    Jan-1999
c
c     changed: Christian Eckert eckert@mit.edu 23-Nov-1999
c
c              - interchanged the i,j loops
c
c              Christian Eckert eckert@mit.edu 11-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu 31-Mar-2000
c
c              - Added BEGIN/END_MASTER and BARRIER for multiple
c                threads and synchronisation, respectively. Added
c                logical lAdInit.
c
c     changed: Christian Eckert eckert@mit.edu 24-Apr-2000
c
c              - Added routines that do active writes on tiles
c                instead of a whole thread.
c
c     ==================================================================
c     SUBROUTINE active_read_tile_rl
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

c     == routine arguments ==

      character*(*) active_var_file

      logical  globalfile
      logical  lAdInit
      integer  irec
      integer  mynr
      integer  itile
      integer  jtile
      integer  theSimulationMode
      integer  myOptimIter
      integer  mythid
      _RL      active_var((snx+2*olx)*(sny+2*oly)*mynr*nsx*nsy)

c     == local variables ==

      character*(2)  adpref
      character*(80) adfname

      integer bi,bj
      integer i,j,k
      integer index_t
      integer index_var
      integer oldprec
      integer prec
      integer il
      integer ilnblnk

      logical writeglobalfile

      _RL  active_data_t((snx+2*olx)*(sny+2*oly))

c     == functions ==

      external ilnblnk

c     == end of interface ==

c     force 64-bit io
      oldPrec        = readBinaryPrec
      readBinaryPrec = ctrlprec
      prec           = ctrlprec

      write(adfname(1:80),'(80a)') ' '
      adpref = 'ad'
      il = ilnblnk( active_var_file )

      write(adfname(1:2)   ,'(a)') adpref
      write(adfname(3:il+2),'(a)') active_var_file(1:il)

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. FORWARD_SIMULATION) then

        _BEGIN_MASTER( mythid )

c       Read the active variable from file.
        if ( .not. globalfile ) then
          bj = jtile
          bi = itile
c         Read the layers individually.
          do k=1,mynr
            call mdsreadvector( active_var_file, prec, 'RL',
     &                          (snx+2*olx)*(sny+2*oly),
     &                          active_data_t, bi, bj,
     &                          (irec-1)*mynr+k, mythid )

c           Copy data to appropriate location in active_var.
            index_t = 0
            do j=1-oly,sny+oly
              do i=1-olx,snx+olx
                index_t   = index_t + 1
                index_var = 1 +
     &                     (i-1+olx) +
     &                     (j-1+oly)*(snx+2*olx) +
     &                     (  k-1  )*(snx+2*olx)*(sny+2*oly) +
     &                     (  bi-1 )*(snx+2*olx)*(sny+2*oly)*nr +
     &                     (  bj-1 )*(snx+2*olx)*(sny+2*oly)*nr*nsx

                active_var(index_var) = active_data_t(index_t)
              enddo
            enddo
          enddo

        else if ( globalfile ) then
          do i=1,(snx+2*olx)*(sny+2*oly)*mynr*nsx*nsy
            active_var(i) = 0. _d 0
          enddo
          call mdsreadfield( active_var_file, prec, 'RL',
     &                       mynr, active_var, irec, mythid )
        endif

        if (lAdInit) then
c         Initialise the corresponding adjoint variable on the
c         adjoint variable's file.

          writeglobalfile = .false.
          do i=1,(snx+2*olx)*(sny+2*oly)
            active_data_t(i) = 0. _d 0
          enddo
          bj = jtile
          bi = itile
          do k = 1,mynr
            call mdswritevector( adfname, prec, writeglobalfile,
     &                           'RL', (snx+2*olx)*(sny+2*oly), 
     &                           active_data_t, bi, bj,
     &                           (irec-1)*mynr+k, myOptimIter,
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
        bj = jtile
        bi = itile
        do k=1,mynr
c         Read data from file layer by layer.
          call mdsreadvector( active_var_file, prec, 'RL',
     &                        (snx+2*olx)*(sny+2*oly),
     &                        active_data_t, bi, bj,
     &                        (irec-1)*mynr+k, mythid )

c         Add active_var from appropriate location to data.
          index_t = 0
          do j=1-oly,sny+oly
            do i=1-olx,snx+olx
              index_t   = index_t + 1
              index_var = 1 +
     &                   (i-1+olx) +
     &                   (j-1+oly)*(snx+2*olx) +
     &                   (  k-1  )*(snx+2*olx)*(sny+2*oly) +
     &                   (  bi-1 )*(snx+2*olx)*(sny+2*oly)*nr +
     &                   (  bj-1 )*(snx+2*olx)*(sny+2*oly)*nr*nsx

              active_data_t(index_t) = active_data_t(index_t) +
     &                                 active_var(index_var)
            enddo
          enddo

c         Store the result on disk.
          call mdswritevector( active_var_file, prec,
     &                         writeglobalfile, 'RL',
     &                         (snx+2*olx)*(sny+2*oly),
     &                         active_data_t, bi, bj,
     &                         (irec-1)*mynr+k, myOptimIter,
     &                         mythid )
        enddo


c       Set active_var to zero. Use the standard FORTRAN index
c       mapping.
        bj = jtile
        bi = itile
        do k = 1,nr
          do j = 1-oly,sny+oly
            do i = 1-olx,snx+olx
              index_var = 1 +
     &                   (i-1+olx) +
     &                   (j-1+oly)*(snx+2*olx) +
     &                   (  k-1  )*(snx+2*olx)*(sny+2*oly) +
     &                   (  bi-1 )*(snx+2*olx)*(sny+2*oly)*nr +
     &                   (  bj-1 )*(snx+2*olx)*(sny+2*oly)*nr*nsx

              active_var(index_var) = 0. _d 0
            enddo
          enddo
        enddo

        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. TANGENT_SIMULATION) then

        _BEGIN_MASTER( mythid )

c       Read the active variable from file.
        if ( .not. globalfile ) then
          bj = jtile
          bi = itile
c         Read the layers individually.
          do k=1,mynr
            call mdsreadvector( active_var_file, prec, 'RL',
     &                          (snx+2*olx)*(sny+2*oly),
     &                          active_data_t, bi, bj,
     &                          (irec-1)*mynr+k, mythid )

c           Copy data to appropriate location in active_var.
            index_t = 0
            do j=1-oly,sny+oly
              do i=1-olx,snx+olx
                index_t   = index_t + 1
                index_var = 1 +
     &                     (i-1+olx) +
     &                     (j-1+oly)*(snx+2*olx) +
     &                     (  k-1  )*(snx+2*olx)*(sny+2*oly) +
     &                     (  bi-1 )*(snx+2*olx)*(sny+2*oly)*nr +
     &                     (  bj-1 )*(snx+2*olx)*(sny+2*oly)*nr*nsx

                active_var(index_var) = active_data_t(index_t)
              enddo
            enddo
          enddo

        else if ( globalfile ) then
          do i=1,(snx+2*olx)*(sny+2*oly)*mynr*nsx*nsy
            active_var(i) = 0. _d 0
          enddo
          call mdsreadfield( active_var_file, prec, 'RL',
     &                       mynr, active_var, irec, mythid )
        endif

        _END_MASTER( mythid )

      endif

c     Reset default io precision.
      readBinaryPrec = oldPrec

      _BARRIER

      return
      end


      subroutine active_write_tile_rl(
     I                                 active_var_file,
     I                                 active_var,
     I                                 globalfile,
     I                                 irec,
     I                                 mynr,
     I                                 itile,
     I                                 jtile,
     I                                 theSimulationMode,
     I                                 myOptimIter,
     I                                 mythid
     &                               )

c     ==================================================================
c     SUBROUTINE active_write_tile_rl
c     ==================================================================
c
c     o Write an active variable to a file.
c
c     started: Christian Eckert eckert@mit.edu    Jan-1999
c
c     changed: Christian Eckert eckert@mit.edu 23-Nov-1999
c
c              - interchanged the i,j loops
c
c              Christian Eckert eckert@mit.edu 11-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu 31-Mar-2000
c
c              - Added BEGIN/END_MASTER and BARRIER for multiple
c                threads and synchronisation, respectively.c
c
c     changed: Christian Eckert eckert@mit.edu 24-Apr-2000
c
c              - Added routines that do active writes on tiles
c                instead of a whole thread.
c
c     ==================================================================
c     SUBROUTINE active_write_tile_rl
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

c     == routine arguments ==

      character*(*) active_var_file

      integer  mynr
      logical  globalfile
      integer  irec
      integer  itile
      integer  jtile
      integer  theSimulationMode
      integer  myOptimIter
      integer  mythid
      _RL      active_var((snx+2*olx)*(sny+2*oly)*mynr*nsx*nsy)

c     == local variables ==

      integer  i,j,k
      integer  bi,bj
      _RL      active_data_t((snx+2*olx)*(sny+2*oly))
      integer  oldprec
      integer  prec
      integer  index_t
      integer  index_var

c     == end of interface ==

c     force 64-bit io
      oldPrec        = readBinaryPrec
      readBinaryPrec = ctrlprec
      prec           = ctrlprec

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. FORWARD_SIMULATION) then

        _BEGIN_MASTER( mythid )

        bj = jtile
        bi = itile
        do k=1,mynr
          index_t = 0
          do j=1-oly,sny+oly
            do i=1-olx,snx+olx
              index_t   = index_t + 1
              index_var = 1 +
     &                   (i-1+olx) +
     &                   (j-1+oly)*(snx+2*olx) +
     &                   (  k-1  )*(snx+2*olx)*(sny+2*oly) +
     &                   (  bi-1 )*(snx+2*olx)*(sny+2*oly)*nr +
     &                   (  bj-1 )*(snx+2*olx)*(sny+2*oly)*nr*nsx

              active_data_t(index_t) = active_var(index_var)
            enddo
          enddo
          call mdswritevector( active_var_file, prec, globalfile,
     &                         'RL', (snx+2*olx)*(sny+2*oly),
     &                         active_data_t, bi, bj,
     &                         (irec-1)*mynr+k, myOptimIter,
     &                          mythid )

        enddo

        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. REVERSE_SIMULATION) then

        _BEGIN_MASTER( mythid )

        bj = jtile
        bi = itile
        do k=1,mynr
c         Read data from file layer by layer.
          call mdsreadvector( active_var_file, prec, 'RL',
     &                        (snx+2*olx)*(sny+2*oly),
     &                        active_data_t, bi, bj,
     &                        (irec-1)*mynr+k, mythid )

c         Add active_var from appropriate location to data.
          index_t = 0
          do j=1-oly,sny+oly
            do i=1-olx,snx+olx
              index_t   = index_t + 1
              index_var = 1 +
     &                   (i-1+olx) +
     &                   (j-1+oly)*(snx+2*olx) +
     &                   (  k-1  )*(snx+2*olx)*(sny+2*oly) +
     &                   (  bi-1 )*(snx+2*olx)*(sny+2*oly)*nr +
     &                   (  bj-1 )*(snx+2*olx)*(sny+2*oly)*nr*nsx

              active_var(index_var) = active_var(index_var) +
     &                                active_data_t(index_t)
              active_data_t(index_t) = 0. _d 0
            enddo
          enddo
          call mdswritevector( active_var_file, prec, globalfile,
     &                         'RL', (snx+2*olx)*(sny+2*oly),
     &                         active_data_t, bi, bj,
     &                         (irec-1)*mynr+k, myOptimIter,
     &                         mythid )
        enddo

        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. TANGENT_SIMULATION) then

        _BEGIN_MASTER( mythid )

        bj = jtile
        bi = itile
        do k=1,mynr
          index_t = 0
          do j=1-oly,sny+oly
            do i=1-olx,snx+olx
              index_t   = index_t + 1
              index_var = 1 +
     &                   (i-1+olx) +
     &                   (j-1+oly)*(snx+2*olx) +
     &                   (  k-1  )*(snx+2*olx)*(sny+2*oly) +
     &                   (  bi-1 )*(snx+2*olx)*(sny+2*oly)*nr +
     &                   (  bj-1 )*(snx+2*olx)*(sny+2*oly)*nr*nsx

              active_data_t(index_t) = active_var(index_var)
            enddo
          enddo
          call mdswritevector( active_var_file, prec, globalfile,
     &                         'RL', (snx+2*olx)*(sny+2*oly),
     &                         active_data_t, bi, bj,
     &                         (irec-1)*mynr+k, myOptimIter,
     &                          mythid )

        enddo

        _END_MASTER( mythid )

      endif

c     Reset default io precision.
      readBinaryPrec = oldPrec

      _BARRIER

      return
      end
