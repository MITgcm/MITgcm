C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_readparms.F,v 1.2 2001/07/13 14:50:46 heimbach Exp $

#include "CPP_OPTIONS.h"


      subroutine grdchk_readparms(
     I                        mythid
     &                      )

c     ==================================================================
c     SUBROUTINE grdchk_readparms
c     ==================================================================
c
c     o Initialize the ECCO gradient check.
c
c     started: Christian Eckert eckert@mit.edu 03-Mar-2000
c     continued: heimbach@mit.edu: 13-Jun-2001
c
c     ==================================================================
c     SUBROUTINE grdchk_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "ctrl.h"
#include "grdchk.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_GRADIENT_CHECK
c     == local variables ==

      integer i,j,k
      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer errio
      integer il

      character*(max_len_prec) record

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Optimization parameters.
      namelist /grdchk_nml/
     &                  grdchk_eps,
     &                  nbeg,
     &                  nstep,
     &                  nend,
     &                  grdchkvarindex

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

      _BEGIN_MASTER( mythid )

c--     Set default values.
        grdchk_eps = 1. _d 0
        nbeg       = 1
        nend       = 1
        nstep      = 1

c--     Check versions.

        open(unit=scrunit1,status='scratch')

c--     Next, read the forcing data file.
        open(unit = modeldataunit,file = 'data.grdchk',
     &       status = 'old', iostat = errio)
        if ( errio .lt. 0 ) then
          stop ' ... stopped in grdchk_readparms.'
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
        read(unit = scrunit1, nml = grdchk_nml)
        close( scrunit1 )

c--     Summarize the gradient check's setup.
        call grdchk_Summary( mythid )
      _END_MASTER( mythid )

      _BARRIER

#endif /* ALLOW_GRADIENT_CHECK */

      end


