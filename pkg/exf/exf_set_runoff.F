C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_set_runoff.F,v 1.2 2009/08/27 18:00:01 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_set_runoff(
     &     genfile, genstartdate, genperiod,
     &     exf_inscal_gen, genremove_intercept, genremove_slope,
     &     genfld, gen0, gen1, genmask,
     &     mytime, myiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_set_runoff
c     ==================================================================
c
c     o set external forcing runoff
c       this is copy of exf_set_gen, but with the spatial interpolation
c       capability removed, so that this routine always expects
c       the runoff file to contain pre-interpolated data
c
c     started: Martin.Losch@awi.de 12-Feb-2009

c     ==================================================================
c     SUBROUTINE exf_set_runoff
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

c     == routine arguments ==

      _RL genstartdate, genperiod
      _RL exf_inscal_gen
      _RL genremove_intercept, genremove_slope
      _RL genfld(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL gen0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL gen1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      character*1 genmask
      character*(128) genfile
      _RL     mytime
      integer myiter
      integer mythid

c     == local variables ==

      logical first, changed
      integer count0, count1
      integer year0, year1
      integer bi, bj, i, j, il
      _RL     fac
      character*(128) genfile0, genfile1

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      if ( genfile .NE. ' ' .and. genperiod .ne. 0 ) then

cph(
cph-exf-print         if (genfile .EQ. hfluxfile)  year0 = 3000
cph)

         if ( genperiod .eq. -12 ) then
c     genperiod=-12 means input file contains 12 monthly means
c     record numbers are assumed 1 to 12 corresponding to
c     Jan. through Dec.
            call cal_GetMonthsRec(
     O           fac, first, changed,
     O           count0, count1,
     I           mytime, myiter, mythid
     &           )

         elseif ( genperiod .lt. 0 ) then
            print *, 'genperiod is out of range'
            STOP 'ABNORMAL END: S/R EXF_SET_RUNOFF'

         else
c     get record numbers and interpolation factor for gen
            call exf_GetFFieldRec(
     I           genstartdate, genperiod
     I           , useExfYearlyFields
     O           , fac, first, changed
     O           , count0, count1, year0, year1
     I           , mytime, myiter, mythid
     &           )

         endif

         if ( first ) then
            call exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, genperiod, year0,
     I         genfile,
     O         genfile0,
     I         mytime, myiter, mythid )

            if (exf_yftype .eq. 'RL') then
               CALL READ_REC_3D_RL( genfile0, exf_iprec, 1,
     &                              gen1, count0, myIter, myThid )
               call exf_filter_rl( gen1, genmask, mythid )
            else
c              CALL READ_REC_3D_RS( genfile0, exf_iprec, 1,
c    &                              gen1, count0, myIter, myThid )
c              call exf_filter_rs( gen1, genmask, mythid )
               STOP 'S/R EXF_SET_RUNOFF: invalid exf_yftype'
            end if
         endif

         if (( first ) .or. ( changed )) then
            call exf_SwapFFields( gen0, gen1, mythid )

            call exf_GetYearlyFieldName(
     I         useExfYearlyFields, twoDigitYear, genperiod, year1,
     I         genfile,
     O         genfile1,
     I         mytime, myiter, mythid )

            if (exf_yftype .eq. 'RL') then
               CALL READ_REC_3D_RL( genfile1, exf_iprec, 1,
     &                              gen1, count1, myIter, myThid )
               call exf_filter_rl( gen1, genmask, mythid )
            else
c              CALL READ_REC_3D_RS( genfile1, exf_iprec, 1,
c    &                              gen1, count1, myIter, myThid )
c              call exf_filter_rs( gen1, genmask, mythid )
               STOP 'S/R EXF_SET_RUNOFF: invalid exf_yftype'
            endif
         endif

c     Loop over tiles.
         do bj = mybylo(mythid),mybyhi(mythid)
          do bi = mybxlo(mythid),mybxhi(mythid)
           do j = 1,sny
            do i = 1,snx
c     Interpolate linearly onto the  time.
             genfld(i,j,bi,bj) = exf_inscal_gen * (
     &                          fac * gen0(i,j,bi,bj) +
     &              (exf_one - fac) * gen1(i,j,bi,bj) )
             genfld(i,j,bi,bj) =
     &            genfld(i,j,bi,bj) -
     &            exf_inscal_gen * ( genremove_intercept +
     &            genremove_slope*(mytime-starttime) )
            enddo
           enddo
          enddo
         enddo

      endif

      RETURN
      END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine exf_init_runoff (
     &     genfile, genperiod, exf_inscal_gen, genmask,
     &     genconst, genfld, gen0, gen1,
     &     mythid )


c     ==================================================================
c     SUBROUTINE exf_init_runoff
c     ==================================================================
c
c     o set external forcing runoff
c       this is copy of exf_set_init, but with the spatial interpolation
c       capability removed, so that this routine always expects
c       the runoff file to contain pre-interpolated data
c
c     ==================================================================
c     SUBROUTINE exf_init_runoff
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "EXF_PARAM.h"

c     == routine arguments ==

      _RL genperiod, exf_inscal_gen, genconst
      _RL genfld(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL gen0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL gen1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      character*1 genmask
      character*(128) genfile
      integer mythid

c     == local variables ==

      integer bi, bj, i, j, count

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1-oly, sny+oly
            do i = 1-olx, snx+olx
              genfld(i,j,bi,bj)  = genconst
              gen0(i,j,bi,bj)    = genconst
              gen1(i,j,bi,bj)    = genconst
            enddo
          enddo
        enddo
      enddo

      if ( genfile .NE. ' ' .and. genperiod .eq. 0. ) then
         count = 1

         if (exf_yftype .eq. 'RL') then
            CALL READ_REC_3D_RL( genfile, exf_iprec, 1,
     &                           genfld, count, 0, myThid )
            call exf_filter_rl( genfld, genmask, mythid )
         else
c           CALL READ_REC_3D_RS( genfile, exf_iprec, 1,
c    &                           genfld, count, 0, myThid )
c           call exf_filter_rs( genfld, genmask, mythid )
            STOP 'S/R EXF_INIT_RUNOFF: invalid exf_yftype'
         endif

c     Loop over tiles and scale genfld
         do bj = mybylo(mythid),mybyhi(mythid)
            do bi = mybxlo(mythid),mybxhi(mythid)
               do j = 1,sny
                  do i = 1,snx
                     genfld(i,j,bi,bj) =
     &                    exf_inscal_gen * genfld(i,j,bi,bj)
                  enddo
               enddo
            enddo
         enddo

      endif

      RETURN
      END
