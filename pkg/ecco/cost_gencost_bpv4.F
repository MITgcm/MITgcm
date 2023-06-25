#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_bpv4(
     I                     myThid )

c     ==================================================================
c     SUBROUTINE cost_gencost_bpv4
c     ==================================================================
c
c     o Evaluate cost function contribution of bottom pressure anoamlies
c       => GRACE data
c
c     started: Gael Forget Oct-2009
c
c     ==================================================================
c     SUBROUTINE cost_bp
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

c     == routine arguments ==
      integer myThid

#ifdef ALLOW_ECCO
#ifdef ALLOW_GENCOST_CONTRIBUTION

c     == external functions ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
      integer bi,bj
      integer i,j
      integer irec
      integer il
      logical doglobalread
      logical ladinit
      _RL locbpbar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL locbpdat(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL locbpmask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL locwbp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL bpdifmean ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )
      _RL bpdifanom ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )
      _RL bpdatmean ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )
      _RL bpdatanom ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )
      _RL bpcount ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )
      _RL junk
      character*(80) fname
      character*(80) fname4test
      _RL fac
      _RL offset
      _RL offset_sum
      integer k, kgen
      logical dosumsq
#ifdef ALLOW_SMOOTH
      integer k2_bp
      logical exst
#endif
#ifdef ECCO_VERBOSE
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
c     == end of interface ==

      kgen=0
      do k=1,NGENCOST
        if ( (gencost_name(k).EQ.'bpv4-grace').AND.
     &       (.NOT.gencost_is1d(k)).AND.
     &       (using_gencost(k)) ) kgen=k
      enddo

      if (kgen.GT.0) then

#ifdef ALLOW_SMOOTH
       k2_bp=0
       if ( useSMOOTH) then
          do k = 1,ngenpproc
            if (gencost_posproc(k,kgen).eq.'smooth') k2_bp=k
          enddo
       endif
#endif

       dosumsq=.TRUE.
       call ecco_zero( gencost_weight(1-OLx,1-OLy,1,1,kgen),
     &                 1, zeroRL, myThid )
       if ( gencost_errfile(kgen) .NE. ' ' )
     &   call ecco_readwei( gencost_errfile(kgen),
     &     gencost_weight(1-OLx,1-OLy,1,1,kgen),
     &     1, 1, 1, dosumsq, myThid )

c-- initialise local variables
cgf convert phibot from m2/s2 to cm
       fac = 1. _d 2 / 9.81 _d 0
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          do j = 1,sNy
           do i = 1,sNx
             bpdifmean(i,j,bi,bj) = 0. _d 0
             bpdifanom(i,j,bi,bj) = 0. _d 0
             bpdatmean(i,j,bi,bj) = 0. _d 0
             bpdatanom(i,j,bi,bj) = 0. _d 0
             bpcount(i,j,bi,bj) = 0. _d 0
             locwbp(i,j,bi,bj) = 0. _d 0
             locbpbar(i,j,bi,bj) = 0. _d 0
             locbpdat(i,j,bi,bj) = 0. _d 0
             locbpmask(i,j,bi,bj) = 0. _d 0
           enddo
          enddo
        ENDDO
       ENDDO

       doglobalread = .false.
       ladinit      = .false.

c-- map global variable to local variables

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          do j = 1,sNy
           do i = 1,sNx
             locwbp(i,j,bi,bj) = gencost_weight(i,j,bi,bj,kgen)
           enddo
          enddo
        ENDDO
       ENDDO

#ifdef ALLOW_CTRL
       il=ilnblnk( gencost_barfile(kgen) )
       write(fname(1:80),'(2a,i10.10)')
     &     gencost_barfile(kgen)(1:il),'.',eccoiter
#endif

c--   ============
c--   Mean values.
c--   ============

#ifdef ECCO_VERBOSE
       WRITE(msgBuf,'(A,1x,i5,1x,i10)')
     &      ' bpv4, kgen, nmonsrec: ',kgen, nmonsrec
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
#endif
       do irec = 1, nmonsrec

c--     Compute the mean over all bpdat records.
#ifdef ALLOW_AUTODIFF
        call active_read_xy( fname, locbpbar, irec, doglobalread,
     &                       ladinit, eccoiter, myThid,
     &                       gencost_dummy(kgen) )
#else
        CALL READ_REC_XY_RL( fname, locbpbar,
     &                       iRec, 1, myThid )
#endif

        call cost_bp_read( gencost_datafile(kgen),
     &       gencost_startdate(1,kgen),
     &       locbpdat, locbpmask, irec, myThid )

#ifdef ECCO_VERBOSE
        il=ilnblnk( gencost_datafile(kgen) )
        WRITE(msgBuf,'(A,1x,A,1x,i10)') ' bpv4, datafile, irec: ',
     &        gencost_datafile(kgen)(1:il), irec
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        write(fname4test(1:80),'(2a,i4.4)')
     &       gencost_datafile(kgen)(1:il),'.',irec
        call write_rec_xy_rl( fname4test, locbpdat, 1, 1, myThid)
#endif

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           do j = 1,sNy
            do i = 1,sNx
              if ( (locbpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &             (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                bpdifmean(i,j,bi,bj) = bpdifmean(i,j,bi,bj) +
     &              ( fac*locbpbar(i,j,bi,bj) - locbpdat(i,j,bi,bj) )
                bpdatmean(i,j,bi,bj) = bpdatmean(i,j,bi,bj) +
     &              locbpdat(i,j,bi,bj)
                bpcount(i,j,bi,bj) = bpcount(i,j,bi,bj) + 1. _d 0
              endif
            enddo
           enddo
         ENDDO
        ENDDO

       enddo

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          do j = 1,sNy
           do i = 1,sNx
             if (bpcount(i,j,bi,bj).GT. 0. _d 0) then
               bpdifmean(i,j,bi,bj) =
     &              bpdifmean(i,j,bi,bj)/bpcount(i,j,bi,bj)
               bpdatmean(i,j,bi,bj) =
     &              bpdatmean(i,j,bi,bj)/bpcount(i,j,bi,bj)
             endif
           enddo
          enddo
        ENDDO
       ENDDO

c--   ==========
c--   Anomalies.
c--   ==========

c--   Loop over records for the second time.
       do irec = 1, nmonsrec
#ifdef ALLOW_AUTODIFF
        call active_read_xy( fname, locbpbar, irec, doglobalread,
     &                       ladinit, eccoiter, myThid,
     &                       gencost_dummy(kgen) )
#else
        CALL READ_REC_XY_RL( fname, locbpbar,
     &                       iRec, 1, myThid )
#endif

        call cost_bp_read( gencost_datafile(kgen),
     &       gencost_startdate(1,kgen),
     &       locbpdat, locbpmask, irec, myThid )

c--    Compute field of anomalies
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           do j = 1,sNy
            do i = 1,sNx
              if ( (locbpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &             (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                bpdifanom(i,j,bi,bj) =
     &              ( fac*locbpbar(i,j,bi,bj) - locbpdat(i,j,bi,bj) )
     &              - bpdifmean(i,j,bi,bj)
                bpdatanom(i,j,bi,bj) =
     &              locbpdat(i,j,bi,bj) - bpdatmean(i,j,bi,bj)
              else
                bpdifanom(i,j,bi,bj) = 0. _d 0
                bpdatanom(i,j,bi,bj) = 0. _d 0
              endif
            enddo
           enddo
         ENDDO
        ENDDO

c--    Remove global mean value
        offset     = 0. _d 0
        offset_sum = 0. _d 0

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           do j = 1,sNy
            do i = 1,sNx
              if ( (locbpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &             (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                offset  = offset + RA(i,j,bi,bj)*bpdifanom(i,j,bi,bj)
                offset_sum = offset_sum + RA(i,j,bi,bj)
              endif
            enddo
           enddo
         ENDDO
        ENDDO

        _GLOBAL_SUM_RL( offset     , myThid )
        _GLOBAL_SUM_RL( offset_sum , myThid )

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           do j = 1,sNy
            do i = 1,sNx
              if ( (offset_sum.GT. 0. _d 0).AND.
     &             (locbpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &             (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                bpdifanom(i,j,bi,bj) = bpdifanom(i,j,bi,bj)
     &                               - offset/offset_sum
              endif
            enddo
           enddo
         ENDDO
        ENDDO

c--    Smooth field of anomalies
        if (gencost_outputlevel(kgen).GT.0) then
         write(fname4test(1:80),'(1a)') 'bpdifanom_raw'
         CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                         bpdifanom, irec, 1, myThid )
         write(fname4test(1:80),'(1a)') 'bpdatanom_raw'
         CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                         bpdatanom, irec, 1, myThid )
        endif

#ifdef ALLOW_SMOOTH
        IF ( k2_bp.GE.1 ) THEN
C- Note: Unlike all other generic-cost smoothing, original code was using
C  "smooth_basic2D" with hard-coded smoothing length (3.e+5) and time
C  multiplicator (3000). Keep original call (now using run-time params setting)
C  when the scaling file gencost_posproc_c is missing:
         il=ilnblnk( gencost_posproc_c(k2_bp,kgen) )
         inquire(file=gencost_posproc_c(k2_bp,kgen)(1:il), exist=exst)
         IF ( il .gt. 1 .and. exst ) THEN
           call smooth_hetero2d(bpdifanom,maskc,
     &      gencost_posproc_c(k2_bp,kgen),
     &      gencost_posproc_i(k2_bp,kgen),myThid)
         ELSE
           call smooth_basic2D(bpdifanom,maskInC,
     &      gencost_posproc_r(k2_bp,kgen),
     &      gencost_posproc_i(k2_bp,kgen),myThid)
         ENDIF
        ENDIF
#endif

        if (gencost_outputlevel(kgen).GT.0) then
#ifdef ALLOW_SMOOTH
         IF ( k2_bp.GE.1 ) THEN
C--   Skip second evaluation of "il" & "exst" (unchanged since first one above)
c         il=ilnblnk( gencost_posproc_c(k2_bp,kgen) )
c         inquire(file=gencost_posproc_c(k2_bp,kgen)(1:il), exist=exst)
          IF ( il .gt. 1 .and. exst ) THEN
           call smooth_hetero2d(bpdatanom,maskc,
     &      gencost_posproc_c(k2_bp,kgen),
     &      gencost_posproc_i(k2_bp,kgen),myThid)
          ELSE
           call smooth_basic2D(bpdatanom,maskInC,
     &      gencost_posproc_r(k2_bp,kgen),
     &      gencost_posproc_i(k2_bp,kgen),myThid)
          ENDIF
         ENDIF
#endif

         write(fname4test(1:80),'(1a)') 'bpdifanom_smooth'
         CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                         bpdifanom, irec, 1, myThid )
         write(fname4test(1:80),'(1a)') 'bpdatanom_smooth'
         CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                         bpdatanom, irec, 1, myThid )
        endif

c--    Compute cost function
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           do j = 1,sNy
            do i = 1,sNx
c-- map to global cost variables
              if ( (locwbp(i,j,bi,bj).NE. 0. _d 0).AND.
     &             (locbpmask(i,j,bi,bj).NE. 0. _d 0).AND.
     &             (maskc(i,j,1,bi,bj).NE. 0. _d 0) ) then
                 junk = bpdifanom(i,j,bi,bj)
                 objf_gencost(bi,bj,kgen) = objf_gencost(bi,bj,kgen)
     &               + junk*junk*locwbp(i,j,bi,bj)
                 num_gencost(bi,bj,kgen) = num_gencost(bi,bj,kgen)
     &               + 1. _d 0
              endif
            enddo
           enddo
         ENDDO
        ENDDO

       enddo

      endif !if (kgen.GT.0) then

#endif /* ifdef ALLOW_GENCOST_CONTRIBUTION */
#endif /* ifdef ALLOW_ECCO */

      RETURN
      END
