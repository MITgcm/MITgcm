C $Header: /u/gcmpack/MITgcm/pkg/ctrl/optim_readparms.F,v 1.1 2001/07/13 13:40:17 heimbach Exp $

#include "CTRL_CPPOPTIONS.h"


      subroutine optim_readparms(
     I                            mythid
     &                          )

c     ==================================================================
c     SUBROUTINE optim_readparms
c     ==================================================================
c
c     o Initialise the optimization part of the ECCO release.
c
c     started: Christian Eckert eckert@mit.edu 03-Mar-2000
c
c     changed: 
c
c     ==================================================================
c     SUBROUTINE optim_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"

#include "optim.h"

c     == routine arguments ==

      integer mythid 

c     == local variables ==

      integer errio
      integer il

      character*(max_len_prec) record

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Optimization parameters.
      namelist /optim/
     &                 optimcycle, nvars,
     &                 nondimcontrol,
     &                 numiter, nfunc, fmin, iprint,
     &                 epsf, epsx, epsg,
     &                 nupdate, eps

      _BEGIN_MASTER( mythid )

c--     Set default values.
        optimcycle    =   0
        nvars         =  10
        nondimcontrol =  .false.
        numiter       =   0
        nfunc         =   1
        fmin          =   0.0 _d 0
        iprint        =  10
        epsx          =   1.e-6
        epsg          =   1.e-6
        eps           =  -1.e-6
        nupdate       =   0

c--     Check versions.

        open(unit=scrunit1,status='scratch')

c--     Next, read the forcing data file.
        open(unit = modeldataunit,file = 'data.optim',
     &       status = 'old', iostat = errio)
        if ( errio .lt. 0 ) then
          stop ' stopped in optim_readparms.'
        endif

        do while ( .true. )
          read(modeldataunit, fmt='(a)', end=1001) record
          il = max(ilnblnk(record),1)
          if ( record(1:1) .ne. commentcharacter )
     &        write(unit=scrunit1, fmt='(a)') record(:il)
        enddo
 1001   continue
        close( modeldataunit )

        rewind( scrunit1 )
        read(unit = scrunit1, nml = optim)
        close( scrunit1 )

      _END_MASTER( mythid )

      _BARRIER

      return
      end

