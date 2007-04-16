c $Header: /u/gcmpack/MITgcm/verification/lab_sea/code_ad_seaice_only/Attic/exf_getclim.F,v 1.2 2007/04/16 23:41:49 jmc Exp $

#include "EXF_OPTIONS.h"


      subroutine exf_getclim(
     I                        mytime,
     I                        myiter,
     I                        mythid
     &                      )

c     ==================================================================
c     SUBROUTINE exf_getclim
c     ==================================================================
c
c     o Get the climatogy fields for the current time step. The switches
c       for the inclusion of the individual forcing components have to
c       be set in EXF_OPTIONS.h .
c
c       A note on surface fluxes:
c
c       The MITgcmUV's vertical coordinate z is positive upward.
c       This implies that a positive flux is out of the ocean
c       model. However, the wind stress forcing is not treated
c       this way. A positive zonal wind stress accelerates the
c       model ocean towards the east.
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_getclim
c     ==================================================================

      implicit none

c     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EXF_FIELDS.h"
#include "EXF_CLIM_FIELDS.h"
#ifdef ALLOW_AUTODIFF
# include "ctrl.h"
# include "ctrl_dummy.h"
#endif

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

      integer i, j, k
      integer bi, bj

c     == end of interface ==

#ifdef ALLOW_CLIMTEMP_RELAXATION
c     Get values of climatological temperature fields.
      call exf_set_climtemp ( mytime, myiter, mythid )
c--   Update the tile edges.
      _EXCH_XY_R8(climtemp, mythid)
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
c     Get values of climatological salinity fields.
      call exf_set_climsalt ( mytime, myiter, mythid )
c--   Update the tile edges.
      _EXCH_XY_R8(climsalt, mythid)
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
c     Get values of climatological sst fields.
      call exf_set_climsst ( mytime, myiter, mythid )
c--   Update the tile edges.
      _EXCH_XY_R8(climsst, mythid)
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
c     Get values of climatological sst fields.
      call exf_set_climsss ( mytime, myiter, mythid )
c--   Update the tile edges.
      _EXCH_XY_R8(climsss, mythid)
#endif

#ifdef ALLOW_SST_CONTROL
      call ctrl_get_gen (
     &     xx_sst_file, xx_sststartdate, xx_sstperiod,
     &     maskc, climsst, xx_sst0, xx_sst1, xx_sst_dummy,
     &     mytime, myiter, mythid )
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
cph        DO k=1,Nr
         DO j=1-oLy,sNy+oLy
          DO i=1-oLx,sNx+oLx
           theta(i,j,1,bi,bj) = climsst(i,j,bi,bj)
           if ( myiter .EQ. niter0 ) then
              if ( maskC(i,j,1,bi,bj) .NE. 0. .AND.
     &             theta(i,j,1,bi,bj) .EQ. 0. ) then
                print *, 'ph-warn-exf-clim ', i, j, theta(i,j,1,bi,bj)
cph                STOP 'in exf_getclim'
              endif
           endif
          ENDDO
         ENDDO
cph        ENDDO
       ENDDO
      ENDDO


      end
