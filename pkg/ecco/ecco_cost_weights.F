#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine ecco_cost_weights( myThid )

c     ==================================================================
c     SUBROUTINE ecco_cost_weights
c     ==================================================================
c
c     o Read the weights used for the cost function evaluation.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu 02-May-2000
c
c              - corrected typo in mdsreadfield( sflux_errfile );
c                wp --> wsflux. Spotted by Patrick Heimbach.
c
c     ==================================================================
c     SUBROUTINE ecco_cost_weights
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"
#ifdef ALLOW_CTRL
# include "CTRL_OBCS.h"
#endif

c     == routine arguments ==

      integer  myThid

c     == local variables ==

#ifdef ALLOW_OBCS
      integer k
      integer gwUnit
      integer ilo,ihi
      integer iobcs
      _RL ratio
      _RL wti(nr)
      _RL wsi(nr)
      _RL wui(nr)
      _RL wvi(nr)
      _RL dummy
      logical  exst
#endif

c     == external ==

      integer  ifnblnk
      external ifnblnk
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Initialize variance (weight) fields.
#ifdef ALLOW_OBCS
      do k = 1,nr
         wti(k) = 0. _d 0
         wsi(k) = 0. _d 0
         wui(k) = 0. _d 0
         wvi(k) = 0. _d 0
      enddo

#if (defined (ALLOW_OBCS_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_CONTROL))
      do iobcs = 1,nobcs
        do k = 1,Nr
#if (defined (ALLOW_OBCSN_CONTROL) || \
     defined (ALLOW_OBCSN_COST_CONTRIBUTION))
          wobcsn(k,iobcs) = 0. _d 0
#endif
#if (defined (ALLOW_OBCSS_CONTROL) || \
     defined (ALLOW_OBCSS_COST_CONTRIBUTION))
          wobcss(k,iobcs) = 0. _d 0
#endif
#if (defined (ALLOW_OBCSW_CONTROL) || \
     defined (ALLOW_OBCSW_COST_CONTRIBUTION))
          wobcsw(k,iobcs) = 0. _d 0
#endif
#if (defined (ALLOW_OBCSE_CONTROL) || \
     defined (ALLOW_OBCSE_COST_CONTRIBUTION))
          wobcse(k,iobcs) = 0. _d 0
#endif
        enddo
      enddo
#endif

c--   Read error information and set up weight matrices.
      _BEGIN_MASTER(myThid)
      ilo = ifnblnk(data_errfile)
      ihi = ilnblnk(data_errfile)

      inquire( file=data_errfile, exist=exst )
      if (exst) then
        CALL OPEN_COPY_DATA_FILE(
     I                          data_errfile(ilo:ihi),
     I                          'ECCO_COST_WEIGHTS',
     O                          gwUnit,
     I                          myThid )

        read(gwUnit,*) ratio
#if (defined (ALLOW_OBCS_COST_CONTRIBUTION) || defined (ALLOW_OBCS_CONTROL))
     &       , dummy
#endif
        do k = 1,nr
          read(gwUnit,*) wti(k), wsi(k)
#if (defined (ALLOW_OBCS_COST_CONTRIBUTION) || defined (ALLOW_OBCS_CONTROL))
     &               , wvi(k)
#endif
        end do
#ifdef SINGLE_DISK_IO
        CLOSE(gwUnit)
#else
        CLOSE(gwUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */
      endif

      _END_MASTER(myThid)

      _BARRIER

      do k = 1,nr
# ifdef ALLOW_OBCSN_COST_CONTRIBUTION
       wobcsn(k,1) = wti(k)
       wobcsn(k,2) = wsi(k)
       wobcsn(k,3) = wvi(k)
       wobcsn(k,4) = wvi(k)
# endif
# ifdef ALLOW_OBCSS_COST_CONTRIBUTION
       wobcss(k,1) = wti(k)
       wobcss(k,2) = wsi(k)
       wobcss(k,3) = wvi(k)
       wobcss(k,4) = wvi(k)
# endif
# ifdef ALLOW_OBCSW_COST_CONTRIBUTION
       wobcsw(k,1) = wti(k)
       wobcsw(k,2) = wsi(k)
       wobcsw(k,3) = wvi(k)
       wobcsw(k,4) = wvi(k)
# endif
# ifdef ALLOW_OBCSE_COST_CONTRIBUTION
       wobcse(k,1) = wti(k)
       wobcse(k,2) = wsi(k)
       wobcse(k,3) = wvi(k)
       wobcse(k,4) = wvi(k)
# endif
      enddo

# ifdef ALLOW_OBCS_COST_CONTRIBUTION
      do iobcs = 1,nobcs
       do k = 1,nr
#  ifdef ALLOW_OBCSN_COST_CONTRIBUTION
        if (wobcsn(k,iobcs) .ne. 0.) then
         wobcsn(k,iobcs) =
     &        ratio/wobcsn(k,iobcs)/wobcsn(k,iobcs)
        else
         wobcsn(k,iobcs) = 0.0 _d 0
        endif
#  endif
#  ifdef ALLOW_OBCSS_COST_CONTRIBUTION
        if (wobcss(k,iobcs) .ne. 0.) then
         wobcss(k,iobcs) =
     &        ratio/wobcss(k,iobcs)/wobcss(k,iobcs)
        else
         wobcss(k,iobcs) = 0.0 _d 0
        endif
#  endif
#  ifdef ALLOW_OBCSW_COST_CONTRIBUTION
        if (wobcsw(k,iobcs) .ne. 0.) then
         wobcsw(k,iobcs) =
     &        ratio/wobcsw(k,iobcs)/wobcsw(k,iobcs)
        else
         wobcsw(k,iobcs) = 0.0 _d 0
        endif
#  endif
#  ifdef ALLOW_OBCSE_COST_CONTRIBUTION
        if (wobcse(k,iobcs) .ne. 0.) then
         wobcse(k,iobcs) =
     &        ratio/wobcse(k,iobcs)/wobcse(k,iobcs)
        else
         wobcse(k,iobcs) = 0.0 _d 0
        endif
#  endif
       enddo
      enddo
# endif /* ALLOW_OBCS_COST_CONTRIBUTION */
#endif /* ALLOW_OBCS */

      RETURN
      END
