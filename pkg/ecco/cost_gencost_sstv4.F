#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_sstv4(
     I                     myThid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_gencost_sstv4
c     ==================================================================
c
c     o Evaluate cost function contributions of sea surface temperature.
c       (Daily Pointwise and then Large Scale)
c
c       started: Gael Forget, Oct-2009
c
c     ==================================================================
c     SUBROUTINE cost_gencost_sstv4
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif
#ifdef ALLOW_SMOOTH
# include "SMOOTH.h"
#endif

c     == routine arguments ==

      integer myThid

#ifdef ALLOW_SMOOTH
#ifdef ALLOW_GENCOST_CONTRIBUTION
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec,jrec,krec
      integer ilps

      logical doglobalread
      logical ladinit

      _RL mydummy
      _RL mybar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL anom_sst(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL obs_sst (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL nb_sst (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL msk_sst (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tmp_sst (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mask2D (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL spval

      _RL junk,junkweight

      integer ndaysave
      _RL ndaysaveRL

      integer k2, k2_lsc

      character*(80) fname
      character*(80) fname2

#ifdef ALLOW_ECCO_DEBUG
      character*(MAX_LEN_MBUF) msgBuf
      INTEGER ioUnit
#endif
      logical exst

      _RL daytime
      _RL diffsecs
      integer il, localrec
      integer dayiter
      integer daydate(4)
      integer difftime(4)
      integer tempDate_1
      integer middate(4)
      integer locstartdate(4)
      integer yday, ymod
      integer md, dd, sd, ld, wd

      integer kgen, kgen_lsc
      logical dosumsq

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

#ifdef ALLOW_ECCO_DEBUG
      ioUnit=standardMessageUnit
#endif

c-- detect the relevant gencost indices
      kgen=0
      kgen_lsc=0
      k2_lsc=0
      do k=1,NGENCOST
       if (gencost_name(k).EQ.'sstv4-amsre') kgen=k
       if (gencost_name(k).EQ.'sstv4-amsre-lsc') then
        kgen_lsc=k
        do k2 = 1, NGENPPROC
         if (gencost_posproc(k2,kgen_lsc).EQ.'smooth') k2_lsc=k2
        enddo
       endif
      enddo

      if (kgen.NE.0) then
c ------

      dosumsq=.TRUE.
      call ecco_zero(gencost_weight(:,:,1,1,kgen),1,zeroRL,myThid)
      call ecco_zero(gencost_weight(:,:,1,1,kgen_lsc),1,zeroRL,myThid)
      if ( gencost_errfile(kgen) .NE. ' ' )
     &     call ecco_readwei(gencost_errfile(kgen),
     &     gencost_weight(:,:,1,1,kgen),1,1,dosumsq,myThid)
      if ( gencost_errfile(kgen_lsc) .NE. ' ' )
     &     call ecco_readwei(gencost_errfile(kgen_lsc),
     &     gencost_weight(:,:,1,1,kgen_lsc),1,1,dosumsq,myThid)

      call cal_FullDate(19920101,0,locstartdate,myThid)

c--   First, read tiled data.
      doglobalread = .false.
      ladinit      = .false.

      write(fname(1:80),'(80a)') ' '
      ilps=ilnblnk( gencost_barfile(kgen) )
      write(fname(1:80),'(2a,i10.10)')
     &     gencost_barfile(kgen)(1:ilps),'.',eccoiter

      spval = gencost_spmin(kgen)
      mydummy = gencost_dummy(kgen)

cgf =======================================================
cgf PART 1: compute smooth SST cost term
cgf =======================================================

      ndaysave=7
      ndaysaveRL=ndaysave

      do irec = 1, ndaysrec-ndaysave+1, 7

         do bj = jtlo,jthi
          do bi = itlo,ithi
           do j = jmin,jmax
            do i = imin,imax
              anom_sst(i,j,bi,bj)  = 0. _d 0
              obs_sst(i,j,bi,bj)  = 0. _d 0
              nb_sst(i,j,bi,bj)  = 0. _d 0
              msk_sst(i,j,bi,bj)  = 0. _d 0

              ! mask2D for smoothing
              mask2D(i,j,bi,bj) = maskC(i,j,1,bi,bj)
            enddo
           enddo
          enddo
         enddo

c PART 1.1: compute running sample average over ndaysave
c ------------------------------------------------------

         do jrec=1,ndaysave

           krec=irec+jrec-1

c get modeled sst:
#ifdef ALLOW_AUTODIFF
             call active_read_xy( fname, mybar, krec, doglobalread,
     &                       ladinit, eccoiter, myThid,
     &                       mydummy )
#else
             CALL READ_REC_XY_RL( fname, mybar, kRec, 1, myThid )
#endif

c get observed sst:
             daytime = FLOAT(secondsperday*(krec-1)) + modelstart
             dayiter = hoursperday*(krec-1)+modeliter0
             call cal_getdate( dayiter, daytime, daydate, myThid )
             call cal_convdate( daydate,yday,md,dd,sd,ld,wd,myThid )
             ymod = locstartdate(1)/10000
             if ( ymod .GE. yday ) then
               middate(1)=1
               call cal_FullDate(locstartdate(1),0,middate,myThid)
             else
               middate(1)=1
               tempDate_1 = yday*10000+100+1
               call cal_FullDate( tempDate_1, 0, middate, myThid)
             endif
             call cal_TimePassed( middate, daydate, difftime, myThid )
             call cal_ToSeconds( difftime, diffsecs, myThid )
c             localrec = floor(diffsecs/86400.) + 1
             localrec = int(diffsecs/86400.) + 1

             il=ilnblnk(gencost_datafile(kgen))
             write(fname2(1:80),'(2a,i4)')
     &         gencost_datafile(kgen)(1:il), '_', yday
             inquire( file=fname2, exist=exst )

#ifdef ALLOW_ECCO_DEBUG
        WRITE(msgBuf,'(A,I4,A,I4,A,I10,A,1PE15.2)') 'sstv4 reading ',
     &      yday,' ',ymod,' ',localrec,' ',diffsecs
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c
        CALL CAL_PRINTDATE(middate,myThid)
        CALL CAL_PRINTDATE(daydate,myThid)
        CALL CAL_PRINTDATE(difftime,myThid)
#endif

             if ( ( localrec .GT. 0 ).AND.(diffsecs .GT. 0.d0) ) then
               CALL READ_REC_3D_RL( fname2, cost_iprec, 1,
     &                              tmp_sst, localrec, 1, myThid )
             else
              do bj = jtlo,jthi
                do bi = itlo,ithi
                    do j = jmin,jmax
                      do i = imin,imax
                         tmp_sst(i,j,bi,bj) = spval
                      enddo
                    enddo
                enddo
              enddo
             endif

c accumulate obs and misfit:
             do bj = jtlo,jthi
              do bi = itlo,ithi
               do j = jmin,jmax
                do i = imin,imax
                 if ( (tmp_sst(i,j,bi,bj).GT.spval).AND.
     &                (maskc(i,j,1,bi,bj).EQ.1.) ) then
                  anom_sst(i,j,bi,bj)= anom_sst(i,j,bi,bj)+
     &               mybar(i,j,bi,bj)-tmp_sst(i,j,bi,bj)
                  obs_sst(i,j,bi,bj)= obs_sst(i,j,bi,bj)+
     &               tmp_sst(i,j,bi,bj)
                  nb_sst(i,j,bi,bj)=nb_sst(i,j,bi,bj)+1. _d 0
                 endif
                enddo
               enddo
              enddo
             enddo

         enddo !do jrec=1,ndaysave

c average obs and misfit:
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do j = jmin,jmax
            do i = imin,imax
             if ( nb_sst(i,j,bi,bj) .NE. 0. ) then
              obs_sst(i,j,bi,bj) =
     &            obs_sst(i,j,bi,bj)/nb_sst(i,j,bi,bj)
              anom_sst(i,j,bi,bj) =
     &            anom_sst(i,j,bi,bj)/nb_sst(i,j,bi,bj)
              msk_sst(i,j,bi,bj) = 1. _d 0
             endif
            enddo
           enddo
          enddo
         enddo

c PART 1.2: smooth anom_sst in space
c ----------------------------------------

#ifdef ALLOW_GENCOST_SSTV4_OUTPUT
         write(fname2(1:80),'(1a)') 'sstdiff_raw'
         CALL WRITE_REC_3D_RL( fname2, precFloat32, 1,
     &                         anom_sst, irec, 1, myThid )
         write(fname2(1:80),'(1a)') 'sstobs_raw'
         CALL WRITE_REC_3D_RL( fname2, precFloat32, 1,
     &                         obs_sst, irec, 1, myThid )
#endif

         if ( useSMOOTH.AND.(k2_lsc.GT.0) )
     &     call smooth_hetero2d(anom_sst,mask2D,
     &     gencost_posproc_c(k2_lsc,kgen_lsc),
     &     gencost_posproc_i(k2_lsc,kgen_lsc),myThid)

#ifdef ALLOW_GENCOST_SSTV4_OUTPUT
         if ( useSMOOTH.AND.(k2_lsc.GT.0) )
     &     call smooth_hetero2d(obs_sst,mask2D,
     &     gencost_posproc_c(k2_lsc,kgen_lsc),
     &     gencost_posproc_i(k2_lsc,kgen_lsc),myThid)

         write(fname2(1:80),'(1a)') 'sstdiff_smooth'
         CALL WRITE_REC_3D_RL( fname2, precFloat32, 1,
     &                         anom_sst, irec, 1, myThid )
         write(fname2(1:80),'(1a)') 'sstobs_smooth'
         CALL WRITE_REC_3D_RL( fname2, precFloat32, 1,
     &                         obs_sst, irec, 1, myThid )
#endif

c PART 1.3: compute cost function term
c ------------------------------------

         do bj = jtlo,jthi
          do bi = itlo,ithi
           do j = jmin,jmax
            do i = imin,imax
             junk = anom_sst(i,j,bi,bj)
             junkweight = gencost_weight(i,j,bi,bj,kgen_lsc)*
     &          maskc(i,j,1,bi,bj)
             objf_gencost(kgen_lsc,bi,bj) =
     &          objf_gencost(kgen_lsc,bi,bj)
     &            +junk*junk*junkweight/ndaysaveRL
             if ( (junkweight.GT.0.).AND.(nb_sst(i,j,bi,bj).GT.0.) )
     &          num_gencost(kgen_lsc,bi,bj) =
     &          num_gencost(kgen_lsc,bi,bj) + 1. _d 0 /ndaysaveRL
            enddo
           enddo
          enddo
         enddo

      enddo

cgf =======================================================
cgf PART 2: compute raw SST cost term
cgf =======================================================

      do irec = 1, ndaysrec

c get modeled sst:
#ifdef ALLOW_AUTODIFF
        call active_read_xy( fname, mybar, irec, doglobalread,
     &                       ladinit, eccoiter, myThid,
     &                       mydummy )
#else
        CALL READ_REC_XY_RL( fname, mybar, iRec, 1, myThid )
#endif

c get observed sst:
             daytime = FLOAT(secondsperday*(irec-1)) + modelstart
             dayiter = hoursperday*(irec-1)+modeliter0
             call cal_getdate( dayiter, daytime, daydate, myThid )
             call cal_convdate( daydate,yday,md,dd,sd,ld,wd,myThid )
             ymod = locstartdate(1)/10000
             if ( ymod .GE. yday ) then
               middate(1)=1
               call cal_FullDate(locstartdate(1),0,middate,myThid)
             else
               middate(1)=1
               tempDate_1 = yday*10000+100+1
               call cal_FullDate( tempDate_1, 0, middate, myThid)
             endif
             call cal_TimePassed( middate, daydate, difftime, myThid )
             call cal_ToSeconds( difftime, diffsecs, myThid )
c             localrec = floor(diffsecs/86400.) + 1
             localrec = int(diffsecs/86400.) + 1

             il=ilnblnk(gencost_datafile(kgen))
             write(fname2(1:80),'(2a,i4)')
     &         gencost_datafile(kgen)(1:il), '_', yday
             inquire( file=fname2, exist=exst )

#ifdef ALLOW_ECCO_DEBUG
        WRITE(msgBuf,'(A,I4,A,I4,A,I10,A,1PE15.2)') 'sstv4 reading ',
     &      yday,' ',ymod,' ',localrec,' ',diffsecs
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c
        CALL CAL_PRINTDATE(middate,myThid)
        CALL CAL_PRINTDATE(daydate,myThid)
        CALL CAL_PRINTDATE(difftime,myThid)
#endif

             if ( ( localrec .GT. 0 ).AND.(diffsecs .GT. 0.d0) ) then
               call READ_REC_3D_RL( fname2, cost_iprec, 1,
     &                              tmp_sst, localrec, 1, myThid )
             else
              do bj = jtlo,jthi
                do bi = itlo,ithi
                    do j = jmin,jmax
                      do i = imin,imax
                         tmp_sst(i,j,bi,bj) = spval
                      enddo
                    enddo
                enddo
              enddo
             endif

c compute misfit:
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do j = jmin,jmax
            do i = imin,imax
             if ( (tmp_sst(i,j,bi,bj).GT.spval).AND.
     &            (maskc(i,j,1,bi,bj).EQ.1.) ) then
              anom_sst(i,j,bi,bj) =
     &               mybar(i,j,bi,bj)-tmp_sst(i,j,bi,bj)
              msk_sst(i,j,bi,bj) = 1. _d 0
             else
              anom_sst(i,j,bi,bj) = 0. _d 0
              msk_sst(i,j,bi,bj) = 0. _d 0
             endif
            enddo
           enddo
          enddo
         enddo

#ifdef ALLOW_GENCOST_SSTV4_OUTPUT
         write(fname2(1:80),'(1a)') 'sstdiff_point'
         CALL WRITE_REC_3D_RL( fname2, precFloat32, 1,
     &                         anom_sst, irec, 1, myThid )
#endif

c compute cost:

         do bj = jtlo,jthi
          do bi = itlo,ithi
           do j = jmin,jmax
            do i = imin,imax
             junk = anom_sst(i,j,bi,bj)
             junkweight = gencost_weight(i,j,bi,bj,kgen)*
     &          maskc(i,j,1,bi,bj)*msk_sst(i,j,bi,bj)
             objf_gencost(kgen,bi,bj) =
     &          objf_gencost(kgen,bi,bj)+junk*junk*junkweight
             if (junkweight.GT.0.)
     &          num_gencost(kgen,bi,bj) =
     &          num_gencost(kgen,bi,bj) + 1. _d 0
            enddo
           enddo
          enddo
         enddo

      enddo

c ------
      endif !if (kgen.NE.0) then

#endif /* ifdef ALLOW_GENCOST_CONTRIBUTION */
#endif /* ifdef ALLOW_SMOOTH */

      RETURN
      END
