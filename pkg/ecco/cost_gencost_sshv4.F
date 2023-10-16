#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_sshv4(
     I                        myThid )

c     ==================================================================
c     SUBROUTINE cost_gencost_sshv4
c     ==================================================================
c
c     o Evaluate cost function contributions of sea surface height.
c
c        started: Gael Forget, Oct-2009
c
c        working assumption for the time mean dynamic topography (MDT)
c        constraint: the various SLA data sets (tp, ers, gfo) have been
c        consistenty cross-referenced, as done in the RADS data sets. We
c        do not need to know the reference dynamic topography (or
c        SSH/Geoid). Simply it is assumed that the biases between
c        instruments have been taken care of. This is only a matter for
c        the MDT constraint, not for SLA constraints (see below).
c
cgf 1) there are a few hardcoded numbers that will eventually be put in common
cgf     blocks/namelists
cgf 2) there are a several refinements that should be considered, such as
cgf     modulating weights with respect to numbers of samples
c
c     ==================================================================
c     SUBROUTINE cost_gencost_sshv4
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
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif
#ifdef ALLOW_SMOOTH
# include "SMOOTH.h"
#endif

c     == routine arguments ==
      integer myThid

#ifdef ALLOW_GENCOST_CONTRIBUTION

c     == functions ==
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec,jrec,krec
      integer ilps
      integer nrec_year, nrec_total, nrec_mdt_ref
c introduce use_mon to switch from using daily to monthly data if true
      logical use_mon

      logical doglobalread
      logical ladinit

c mapping to gencost
      integer igen_mdt, igen_lsc, igen_gmsl
      integer igen_tp, igen_ers, igen_gfo
      integer igen_etagcm

      _RL spval, factor
      _RL offset
      _RL offset_sum
      _RL slaoffset
      _RL slaoffset_sum
      _RL slaoffset_weight

c local arrays
      _RL num0array ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )
      _RL num0total
      integer tempinteger

      _RL diagnosfld ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )

      _RL mdtob(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tpob(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ersob(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gfoob(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mdtma(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tpma(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ersma(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gfoma(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL etagcm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      character*(MAX_LEN_FNAM) mdtfi, tpfi, ersfi, gfofi
      integer tpsta(4), erssta(4), gfosta(4)
      integer mdtsta(4), mdtend(4)
      _RL tpper, ersper, gfoper

c for PART 1: re-reference MDT (mdt) to the inferred SLA reference field
      _RL psmean    ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )
      _RL mean_slaobs_mdt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mean_slaobs_NUM(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

c for PART 2: compute time mean differences over the model period
      _RL mean_slaobs_model(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mean_psMssh_all(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mean_psMssh_all_NUM(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mean_psMssh_all_MSK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL mean_psMslaobs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mean_psMslaobs_MSK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL mean_psMtpobs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mean_psMtpobs_NUM(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL mean_psMersobs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mean_psMersobs_NUM(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL mean_psMgfoobs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mean_psMgfoobs_NUM(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

c for PART 4/5: compute smooth/raw anomalies
      _RL anom_psMslaobs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL anom_psMslaobs_NUM (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL anom_psMslaobs_MSK (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL anom_psMslaobs_SUB (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL anom_slaobs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL anom_slaobs_SUB  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL anom_psMtpobs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL anom_tpobs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL anom_psMersobs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL anom_ersobs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL anom_psMgfoobs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL anom_gfoobs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      integer mdt_y0,mdt_y1,year
      integer num0

      _RL junk,junkweight

      integer nrecsave

      integer k2, k2_mdt, k2_lsc
      logical dosumsq

      character*(MAX_LEN_FNAM) fname
      character*(MAX_LEN_FNAM) fname4test
      character*(MAX_LEN_MBUF) msgbuf

      LOGICAL doReference
      LOGICAL useEtaMean

c     == end of interface ==

C-    Initialise (again) gencost_weight
      DO k = 1, NGENCOST
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           gencost_weight(i,j,bi,bj,k) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c-- detect the relevant gencost indices
      igen_mdt=0
      igen_tp =0
      igen_ers=0
      igen_gfo=0
      igen_lsc=0
      igen_gmsl=0
      igen_etagcm=0
      do k=1,NGENCOST
        if (gencost_name(k).EQ.'sshv4-mdt') igen_mdt=k
        if (gencost_name(k).EQ.'sshv4-tp') igen_tp=k
        if (gencost_name(k).EQ.'sshv4-ers') igen_ers=k
        if (gencost_name(k).EQ.'sshv4-gfo') igen_gfo=k
        if (gencost_name(k).EQ.'sshv4-lsc') igen_lsc=k
        if (gencost_name(k).EQ.'sshv4-gmsl') igen_gmsl=k
      enddo

      k2_mdt=0
      k2_lsc=0
      if (igen_mdt.GT.0) then
       do k2 = 1, NGENPPROC
        if (gencost_posproc(k2,igen_mdt).EQ.'smooth') k2_mdt=k2
       enddo
      endif
      if (igen_lsc.GT.0) then
       do k2 = 1, NGENPPROC
        if (gencost_posproc(k2,igen_lsc).EQ.'smooth') k2_lsc=k2
       enddo
      endif
      write(msgbuf,'(a,2(1x,i5))')
     &  'sshv4: k2_[mdt,lsc]: ', k2_mdt, k2_lsc
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)

      dosumsq = .TRUE.
      if (igen_mdt.GT.0) then
       call ecco_zero( gencost_weight(1-OLx,1-OLy,1,1,igen_mdt),
     &                 1, zeroRL, myThid )
       if ( gencost_errfile(igen_mdt) .NE. ' ' )
     &   call ecco_readwei( gencost_errfile(igen_mdt),
     &     gencost_weight(1-OLx,1-OLy,1,1,igen_mdt),
     &     1, 1, 1, dosumsq, myThid )
      endif
      if (igen_lsc.GT.0) then
       call ecco_zero( gencost_weight(1-OLx,1-OLy,1,1,igen_lsc),
     &                 1, zeroRL, myThid )
       if ( gencost_errfile(igen_lsc) .NE. ' ' )
     &   call ecco_readwei( gencost_errfile(igen_lsc),
     &     gencost_weight(1-OLx,1-OLy,1,1,igen_lsc),
     &     1, 1, 1, dosumsq, myThid )
      endif
      if (igen_tp.GT.0) then
       call ecco_zero( gencost_weight(1-OLx,1-OLy,1,1,igen_tp),
     &                 1, zeroRL, myThid )
       if ( gencost_errfile(igen_tp) .NE. ' ' )
     &   call ecco_readwei( gencost_errfile(igen_tp),
     &     gencost_weight(1-OLx,1-OLy,1,1,igen_tp),
     &     1, 1, 1, dosumsq, myThid )
      endif
      if (igen_ers.GT.0) then
       call ecco_zero( gencost_weight(1-OLx,1-OLy,1,1,igen_ers),
     &                 1, zeroRL, myThid )
       if ( gencost_errfile(igen_ers) .NE. ' ' )
     &   call ecco_readwei( gencost_errfile(igen_ers),
     &     gencost_weight(1-OLx,1-OLy,1,1,igen_ers),
     &     1, 1, 1, dosumsq, myThid )
      endif
      if (igen_gfo.GT.0) then
       call ecco_zero( gencost_weight(1-OLx,1-OLy,1,1,igen_gfo),
     &                 1, zeroRL, myThid )
       if ( gencost_errfile(igen_gfo) .NE. ' ' )
     &   call ecco_readwei( gencost_errfile(igen_gfo),
     &     gencost_weight(1-OLx,1-OLy,1,1,igen_gfo),
     &     1, 1, 1, dosumsq, myThid )
      endif

c switch for excluding global mean
      useEtaMean=.TRUE.

      write(msgbuf,'(a,l)')
     & ' sshv4: use global mean of eta useEtaMean=',useEtaMean
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)

C Check to determine use_mon and nrec_year. Check all igen_[tp,ers,gfo]
C in case some of the data files are not defined.
      use_mon = .FALSE.
      IF ( igen_tp.GT.0 ) use_mon = use_mon .OR.
     &     gencost_avgperiod(igen_tp) .EQ.'month' .OR.
     &     gencost_avgperiod(igen_tp) .EQ.'MONTH'
      IF ( igen_ers.GT.0 ) use_mon = use_mon .OR.
     &     gencost_avgperiod(igen_ers) .EQ.'month' .OR.
     &     gencost_avgperiod(igen_ers) .EQ.'MONTH'
      IF ( igen_gfo.GT.0 ) use_mon = use_mon .OR.
     &      gencost_avgperiod(igen_gfo).EQ.'month' .OR.
     &      gencost_avgperiod(igen_gfo).EQ.'MONTH'

c minimum number of observations to consider re-referenced MDT
      nrec_year=366
      nrec_total=ndaysrec
      IF (use_mon) THEN
        num0=3
        nrec_mdt_ref=12
        nrec_year=12
        nrec_total=nmonsrec
      else
        num0=100
        nrec_mdt_ref=365
      endif

      write(msgbuf,'(a,L5,2(1x,i10),2(1x,i5))')
     & ' sshv4: use_mon, nrec_year, nrec_total, num0, nrec_mdt_ref: ',
     &          use_mon, nrec_year, nrec_total, num0, nrec_mdt_ref
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)
c determine whether or not to re-reference
c-- not only model period has to fall within the mdt period
c-- but that there has to be at least 365 days (or 12 months) of model
c-- run so for short model run, this will always get set to FALSE
C Note also the hard-coded years 1992,2011 here that should be
c removed at some point in the future if the reference MDT start
c date no longer falls between 1992 and 2011
      doReference=.FALSE.
      IF ((modelstartdate(1).GT.1992*10000).AND.
     &    (modelstartdate(1).LT.2011*10000).AND.
     &    (nrec_total.GE.nrec_mdt_ref))  doReference=.TRUE.

      write(msgbuf,'(a,l)') ' sshv4:re-reference MDT=',doReference
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)

c-- initialize local arrays
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              mdtma(i,j,bi,bj) = 0. _d 0
              mdtob(i,j,bi,bj) = 0. _d 0
              tpma(i,j,bi,bj) = 0. _d 0
              tpob(i,j,bi,bj) = 0. _d 0
              ersma(i,j,bi,bj) = 0. _d 0
              ersob(i,j,bi,bj) = 0. _d 0
              gfoma(i,j,bi,bj) = 0. _d 0
              gfoob(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

c mdtfi, mdtsta, mdtend
      if (igen_mdt.GT.0) then
       ilps=ilnblnk( gencost_datafile(igen_mdt) )
       mdtfi=gencost_datafile(igen_mdt)(1:ilps)
       call cal_CopyDate(gencost_startdate(1,igen_mdt),mdtsta,myThid)
       call cal_CopyDate(gencost_enddate(1,igen_mdt), mdtend, myThid)
       if (gencost_barfile(igen_mdt)(1:5).EQ.'m_eta')
     &    igen_etagcm=igen_mdt
      endif
c tpfi, tpsta, tpper
      if (igen_tp.GT.0) then
       ilps=ilnblnk( gencost_datafile(igen_tp) )
       tpfi=gencost_datafile(igen_tp)(1:ilps)
       call cal_CopyDate(gencost_startdate(1,igen_tp),tpsta,myThid)
       tpper=gencost_period(igen_tp)
       if (gencost_barfile(igen_tp)(1:5).EQ.'m_eta')
     &    igen_etagcm=igen_tp
      endif
c ersfi, erssta, ersper
      if (igen_ers.GT.0) then
       ilps=ilnblnk( gencost_datafile(igen_ers) )
       ersfi=gencost_datafile(igen_ers)(1:ilps)
       call cal_CopyDate(gencost_startdate(1,igen_ers),erssta,myThid)
       ersper=gencost_period(igen_ers)
       if (gencost_barfile(igen_ers)(1:5).EQ.'m_eta')
     &    igen_etagcm=igen_ers
      endif
c gfofi, gfosta, gfoper
      if (igen_gfo.GT.0) then
       ilps=ilnblnk( gencost_datafile(igen_gfo) )
       gfofi=gencost_datafile(igen_gfo)(1:ilps)
       call cal_CopyDate(gencost_startdate(1,igen_gfo),gfosta,myThid)
       gfoper=gencost_period(igen_gfo)
       if (gencost_barfile(igen_gfo)(1:5).EQ.'m_eta')
     &    igen_etagcm=igen_gfo
      endif

c mdt_y0, mdt_y1
C in the case the model run is within the valid mdt years,
C we use the max/min check below to get the bound instead of
C basing on mdt years alone.
c       mdt_y0=mdtsta(1)/10000
c       mdt_y1=mdtend(1)/10000
       mdt_y0=max(mdtsta(1)/10000,modelstartdate(1)/10000)
       mdt_y1=min(mdtend(1)/10000,modelenddate(1)/10000)

       write(msgbuf,'(a,i8,a,i8)')
     &  'mdt[start,end]date(1): ', mdtsta(1), ',', mdtend(1)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , myThid)
        write(msgbuf,'(a,i4,a,i4)')
     &  '  TP MDT yrrange:          ', mdt_y0, ',', mdt_y1
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , myThid)

c--   First, read tiled data.
      doglobalread = .false.
      ladinit      = .false.

      ilps=ilnblnk( gencost_barfile(igen_etagcm) )
      write(fname,'(2a,i10.10)')
     &     gencost_barfile(igen_etagcm)(1:ilps),'.',eccoiter

cgf =======================================================
cgf PART 1:
cgf        x Get the MDT (mdt) ... compute the sample mean
cgf        (mean_slaobs_mdt) of the SLA data (i.e. RADS for tp, ers, and gfo
cgf        together) over the time interval of the MDT ... subtract
cgf        mean_slaobs_mdt from mdt.
cgf        x At this point, mdt is the inferred SLA reference field.
cgf        x From there on, mdt+sla will be directly comparable to
cgf        the model SSH (etagcm).
cgf =======================================================

c--   Read mean field and mask

      write(msgbuf,'(a,l)') ' sshv4:using_mdt=',using_mdt
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)
      if (using_mdt)
     &  CALL READ_REC_3D_RL( mdtfi, cost_iprec, 1,
     &                       mdtob, 1, 1, myThid )

      factor =  0.01 _d 0
      spval  = -9990. _d 0

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              if ( (maskInC(i,j,bi,bj) .EQ. 0.).OR.
#ifndef ALLOW_SHALLOW_ALTIMETRY
     &             (R_low(i,j,bi,bj).GT.-200.).OR.
#endif
     &             (mdtob(i,j,bi,bj) .LT. spval ).OR.
     &             (mdtob(i,j,bi,bj) .EQ. 0. _d 0) ) then
                mdtma(i,j,bi,bj) = 0. _d 0
                mdtob(i,j,bi,bj) = 0. _d 0
              else
                mdtma(i,j,bi,bj) = 1. _d 0
                mdtob(i,j,bi,bj) = mdtob(i,j,bi,bj)*factor
              endif
            enddo
          enddo
        enddo
      enddo

c--   Compute mean_slaobs_mdt: sample mean SLA over the time period of mdt.

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
              mean_slaobs_mdt(i,j,bi,bj)  = 0. _d 0
              mean_slaobs_NUM(i,j,bi,bj)  = 0. _d 0
          enddo
         enddo
        enddo
       enddo

      do year=mdt_y0,mdt_y1
       do irec=1,nrec_year

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
              tpma(i,j,bi,bj) = 0. _d 0
              tpob(i,j,bi,bj) = 0. _d 0
              ersma(i,j,bi,bj) = 0. _d 0
              ersob(i,j,bi,bj) = 0. _d 0
              gfoma(i,j,bi,bj) = 0. _d 0
              gfoob(i,j,bi,bj) = 0. _d 0
          enddo
         enddo
        enddo
       enddo

#ifdef ECCO_VERBOSE
       write(msgbuf,'(a,3(1x,l))') ' sshv4:using_[tpj,ers,gfo]: ',
     &   using_tpj, using_ers, using_gfo
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , myThid)
#endif
       if (using_tpj)
     & call cost_sla_read_yd( tpfi, tpsta, use_mon,
     &                tpob, tpma,
     &                year, irec, myThid )
       if (using_ers)
     & call cost_sla_read_yd( ersfi, erssta, use_mon,
     &                ersob, ersma,
     &                year, irec, myThid )
       if (using_gfo)
     & call cost_sla_read_yd( gfofi, gfosta, use_mon,
     &                gfoob, gfoma,
     &                year, irec, myThid )

       if (igen_tp .GT. 0) then
        do bj = jtlo,jthi
         do bi = itlo,ithi
          do j = jmin,jmax
           do i = imin,imax
      if ( tpma(i,j,bi,bj)*mdtma(i,j,bi,bj)*
     &    gencost_weight(i,j,bi,bj,igen_tp) .NE. 0. ) then
          mean_slaobs_mdt(i,j,bi,bj)= mean_slaobs_mdt(i,j,bi,bj)+
     &  tpob(i,j,bi,bj)
          mean_slaobs_NUM(i,j,bi,bj)= mean_slaobs_NUM(i,j,bi,bj)+1. _d 0
      endif
           enddo
          enddo
         enddo
        enddo
       endif
       if (igen_ers .GT. 0) then
        do bj = jtlo,jthi
         do bi = itlo,ithi
          do j = jmin,jmax
           do i = imin,imax
      if ( ersma(i,j,bi,bj)*mdtma(i,j,bi,bj)*
     &    gencost_weight(i,j,bi,bj,igen_ers) .NE. 0. ) then
          mean_slaobs_mdt(i,j,bi,bj)= mean_slaobs_mdt(i,j,bi,bj)+
     &  ersob(i,j,bi,bj)
          mean_slaobs_NUM(i,j,bi,bj)= mean_slaobs_NUM(i,j,bi,bj)+1. _d 0
      endif
           enddo
          enddo
         enddo
        enddo
       endif
       if (igen_gfo .GT. 0) then
        do bj = jtlo,jthi
         do bi = itlo,ithi
          do j = jmin,jmax
           do i = imin,imax
      if ( gfoma(i,j,bi,bj)*mdtma(i,j,bi,bj)*
     &    gencost_weight(i,j,bi,bj,igen_gfo) .NE. 0. ) then
          mean_slaobs_mdt(i,j,bi,bj)= mean_slaobs_mdt(i,j,bi,bj)+
     &  gfoob(i,j,bi,bj)
          mean_slaobs_NUM(i,j,bi,bj)= mean_slaobs_NUM(i,j,bi,bj)+1. _d 0
      endif
           enddo
          enddo
         enddo
        enddo
       endif

       enddo !do irec=1,nrec_year
      enddo !do year=mdt_y0,mdt_y1

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
               if ( ( mean_slaobs_NUM(i,j,bi,bj) .NE. 0. ).AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ).AND.
     &              ( mdtma(i,j,bi,bj) .NE. 0. ) ) then
                  mean_slaobs_mdt(i,j,bi,bj) =
     &                 mean_slaobs_mdt(i,j,bi,bj) /
     &                 mean_slaobs_NUM(i,j,bi,bj)
               else
                  mean_slaobs_mdt(i,j,bi,bj) = 0. _d 0
               endif
            enddo
          enddo
        enddo
      enddo

c--   smooth mean_slaobs_mdt:

      if (igen_mdt.GT.0) then
       if (gencost_outputlevel(igen_mdt).GT.0) then
        write(fname4test,'(1a)') 'sla2mdt_raw'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        mean_slaobs_mdt, 1, 1, myThid )
       endif

#ifdef ALLOW_SMOOTH
       if ( useSMOOTH.AND.(k2_mdt.GT.0) ) then
        call smooth_hetero2d(mean_slaobs_mdt,maskInC,
     &     gencost_posproc_c(k2_mdt,igen_mdt),
     &     gencost_posproc_i(k2_mdt,igen_mdt),myThid)

        if (gencost_outputlevel(igen_mdt).GT.0) then
         write(fname4test,'(1a)') 'sla2mdt_smooth'
         CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                         mean_slaobs_mdt, 1, 1, myThid )
        endif
       endif
#endif
c     endif igen_mdt .gt. 0
      endif

c--   re-reference mdt:
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
               if ( ( mdtma(i,j,bi,bj) .NE. 0. ).AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ).AND.
     &              ( doReference ) ) then
                  mdtob(i,j,bi,bj) = mdtob(i,j,bi,bj)
     &                 -mean_slaobs_mdt(i,j,bi,bj)
               endif
            enddo
          enddo
        enddo
      enddo

cgf =======================================================
cgf PART 2: compute sample means of etagcm-slaobs over the
cgf         period that is covered by the model (i.e. etagcm).
cgf       x for all SLA data sets together: mean_psMssh_all, mean_psMssh_all_MSK,
cgf         and offset will be used in PART 3 (MDT cost term).
cgf       x for each SLA data individually. mean_psMtpobs, mean_psMtpobs_MS, etc.
cgf         will be used in PART 4&5 (SLA cost terms).
cgf =======================================================

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax

              psmean(i,j,bi,bj)    = 0. _d 0
              mean_psMslaobs(i,j,bi,bj)  = 0. _d 0
              mean_psMtpobs(i,j,bi,bj)  = 0. _d 0
              mean_psMersobs(i,j,bi,bj) = 0. _d 0
              mean_psMgfoobs(i,j,bi,bj) = 0. _d 0
              mean_psMssh_all(i,j,bi,bj) = 0. _d 0
              mean_slaobs_model(i,j,bi,bj)  = 0. _d 0

              mean_psMtpobs_NUM(i,j,bi,bj)  = 0. _d 0
              mean_psMersobs_NUM(i,j,bi,bj) = 0. _d 0
              mean_psMgfoobs_NUM(i,j,bi,bj) = 0. _d 0
              mean_psMssh_all_NUM(i,j,bi,bj) = 0. _d 0
              mean_psMslaobs_MSK(i,j,bi,bj)  = 0. _d 0

            enddo
          enddo
        enddo
      enddo
      offset     = 0. _d 0
      offset_sum = 0. _d 0

#ifdef ECCO_VERBOSE
      write(msgbuf,'(a,3(1x,i6))')
     & 'sshv4: before entering irec loop, n[mon,day]srec,nrec_total: ',
     & nmonsrec,ndaysrec,nrec_total
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)
#endif

      do irec = 1, nrec_total

#ifdef ALLOW_AUTODIFF
        call active_read_xy( fname, etagcm, irec, doglobalread,
     &                       ladinit, eccoiter, myThid,
     &                       gencost_dummy(igen_etagcm) )
#else
        CALL READ_REC_XY_RL( fname, etagcm, iRec, 1, myThid )
#endif

        if (.NOT.useEtaMean)
     &  CALL REMOVE_MEAN_RL( 1, etagcm, maskInC, maskInC, rA, drF,
     &                       'etagcm', zeroRL, 1, myThid )

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
              tpma(i,j,bi,bj) = 0. _d 0
              tpob(i,j,bi,bj) = 0. _d 0
              ersma(i,j,bi,bj) = 0. _d 0
              ersob(i,j,bi,bj) = 0. _d 0
              gfoma(i,j,bi,bj) = 0. _d 0
              gfoob(i,j,bi,bj) = 0. _d 0
          enddo
         enddo
        enddo
       enddo

        if (using_tpj)
     &  call cost_sla_read( tpfi, tpsta, tpper, use_mon,
     &                zeroRL, zeroRL,
     &                tpob, tpma,
     &                irec, myThid )
        if (using_ers)
     &  call cost_sla_read( ersfi, erssta, ersper, use_mon,
     &                zeroRL, zeroRL,
     &                ersob, ersma,
     &                irec, myThid )
        if (using_gfo)
     &  call cost_sla_read( gfofi, gfosta, gfoper, use_mon,
     &                zeroRL, zeroRL,
     &                gfoob, gfoma,
     &                irec, myThid )

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
                psmean(i,j,bi,bj) = psmean(i,j,bi,bj) +
     &                etagcm(i,j,bi,bj) / float(nrec_total)
                if ( igen_tp .GT. 0 ) then
                  if ( tpma(i,j,bi,bj)*
     &             gencost_weight(i,j,bi,bj,igen_tp) .NE. 0. ) then
                   mean_slaobs_model(i,j,bi,bj)=
     &              mean_slaobs_model(i,j,bi,bj)+tpob(i,j,bi,bj)
                   mean_psMtpobs(i,j,bi,bj) =
     &                 mean_psMtpobs(i,j,bi,bj) +
     &                 etagcm(i,j,bi,bj)-tpob(i,j,bi,bj)
                   mean_psMtpobs_NUM(i,j,bi,bj) =
     &                 mean_psMtpobs_NUM(i,j,bi,bj) + 1. _d 0
                  endif
                endif
                if ( igen_ers .GT. 0 ) then
                  if ( ersma(i,j,bi,bj)*
     &             gencost_weight(i,j,bi,bj,igen_ers) .NE. 0. ) then
                   mean_slaobs_model(i,j,bi,bj)=
     &              mean_slaobs_model(i,j,bi,bj)+ersob(i,j,bi,bj)
                   mean_psMersobs(i,j,bi,bj) =
     &                 mean_psMersobs(i,j,bi,bj) +
     &                 etagcm(i,j,bi,bj)-ersob(i,j,bi,bj)
                   mean_psMersobs_NUM(i,j,bi,bj) =
     &                 mean_psMersobs_NUM(i,j,bi,bj) + 1. _d 0
                  endif
                endif
                if ( igen_gfo .GT. 0 ) then
                  if ( gfoma(i,j,bi,bj)*
     &             gencost_weight(i,j,bi,bj,igen_gfo) .NE. 0. ) then
                   mean_slaobs_model(i,j,bi,bj)=
     &              mean_slaobs_model(i,j,bi,bj)+gfoob(i,j,bi,bj)
                   mean_psMgfoobs(i,j,bi,bj) =
     &                 mean_psMgfoobs(i,j,bi,bj) +
     &                 etagcm(i,j,bi,bj)-gfoob(i,j,bi,bj)
                   mean_psMgfoobs_NUM(i,j,bi,bj) =
     &                 mean_psMgfoobs_NUM(i,j,bi,bj) + 1. _d 0
                  endif
                endif
              enddo
            enddo
          enddo
        enddo

c--   END loop over irec records for the first time.
      enddo

      call ecco_zero(num0array,1,oneRL,myThid)

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
               if ( ( mean_psMtpobs_NUM(i,j,bi,bj) .NE. 0. )
     &              ) then
                  mean_psMssh_all(i,j,bi,bj) =
     &                 mean_psMssh_all(i,j,bi,bj) +
     &                 mean_psMtpobs(i,j,bi,bj)
                  mean_psMssh_all_NUM(i,j,bi,bj) =
     &                 mean_psMssh_all_NUM(i,j,bi,bj) +
     &                 mean_psMtpobs_NUM(i,j,bi,bj)
                  mean_psMtpobs(i,j,bi,bj) =
     &                 mean_psMtpobs(i,j,bi,bj) /
     &                 mean_psMtpobs_NUM(i,j,bi,bj)
               endif
               if ( ( mean_psMersobs_NUM(i,j,bi,bj) .NE. 0. )
     &              ) then
                  mean_psMssh_all(i,j,bi,bj) =
     &                 mean_psMssh_all(i,j,bi,bj) +
     &                 mean_psMersobs(i,j,bi,bj)
                  mean_psMssh_all_NUM(i,j,bi,bj) =
     &                 mean_psMssh_all_NUM(i,j,bi,bj) +
     &                 mean_psMersobs_NUM(i,j,bi,bj)
                  mean_psMersobs(i,j,bi,bj) =
     &                 mean_psMersobs(i,j,bi,bj) /
     &                 mean_psMersobs_NUM(i,j,bi,bj)
               endif
               if ( ( mean_psMgfoobs_NUM(i,j,bi,bj) .NE. 0. )
     &              ) then
                  mean_psMssh_all(i,j,bi,bj) =
     &                 mean_psMssh_all(i,j,bi,bj) +
     &                 mean_psMgfoobs(i,j,bi,bj)
                  mean_psMssh_all_NUM(i,j,bi,bj) =
     &                 mean_psMssh_all_NUM(i,j,bi,bj) +
     &                 mean_psMgfoobs_NUM(i,j,bi,bj)
                  mean_psMgfoobs(i,j,bi,bj) =
     &                 mean_psMgfoobs(i,j,bi,bj) /
     &                 mean_psMgfoobs_NUM(i,j,bi,bj)
               endif

               if ( ( mean_psMssh_all_NUM(i,j,bi,bj) .GT. 0 ).AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. )
     &              ) then
                  mean_psMslaobs(i,j,bi,bj) =
     &                 mean_psMssh_all(i,j,bi,bj) /
     &                 mean_psMssh_all_NUM(i,j,bi,bj)
                  mean_psMslaobs_MSK(i,j,bi,bj) = 1. _d 0
               else
                  mean_psMslaobs(i,j,bi,bj) = 0. _d 0
                  mean_psMslaobs_MSK(i,j,bi,bj) = 0. _d 0
               endif

               if ( ( mean_psMssh_all_NUM(i,j,bi,bj) .GT. num0 ).AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ) .AND.
     &              ( mdtma(i,j,bi,bj) .NE. 0. ).AND.
     &              ( doReference ) ) then
                  mean_slaobs_model(i,j,bi,bj) =
     &                 mean_slaobs_model(i,j,bi,bj) /
     &                 mean_psMssh_all_NUM(i,j,bi,bj)
                  mean_psMssh_all(i,j,bi,bj) =
     &                 mean_psMssh_all(i,j,bi,bj) /
     &                 mean_psMssh_all_NUM(i,j,bi,bj)-mdtob(i,j,bi,bj)
                  mean_psMssh_all_MSK(i,j,bi,bj) = 1. _d 0
                  offset=offset+RA(i,j,bi,bj)*mean_psMssh_all(i,j,bi,bj)
                  offset_sum=offset_sum+RA(i,j,bi,bj)
               elseif ( ( mdtma(i,j,bi,bj) .NE. 0. ) .AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ).AND.
     &              ( .NOT.doReference ) ) then
                  mean_slaobs_model(i,j,bi,bj) = 0.d0
                  mean_psMssh_all(i,j,bi,bj) = 0. _d 0
                  mean_psMssh_all_MSK(i,j,bi,bj) = 1. _d 0
                  offset=offset+RA(i,j,bi,bj)
     &                  *( psmean(i,j,bi,bj) -mdtob(i,j,bi,bj) )
                  offset_sum=offset_sum+RA(i,j,bi,bj)
                  num0array(i,j,bi,bj)=0. _d 0
               else
                  mean_slaobs_model(i,j,bi,bj) = 0.d0
                  mean_psMssh_all(i,j,bi,bj) = 0. _d 0
                  mean_psMssh_all_MSK(i,j,bi,bj) = 0. _d 0
                  num0array(i,j,bi,bj)=0. _d 0
               endif
              enddo
            enddo
          enddo
        enddo

c--   Do a global summation.
      _GLOBAL_SUM_RL( offset     , myThid )
      _GLOBAL_SUM_RL( offset_sum , myThid )

C --  add warning that written mean_slaobs_model is all zero
c     due to having less data points that hardcoded num0
        num0total=0. _d 0
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
                num0total=num0total+num0array(i,j,bi,bj)
              enddo
            enddo
          enddo
        enddo
      if(num0total.LT.1. _d 0) then
        write(msgbuf,'(A,i5,A)')
     &  '** WARNING ** S/R COST_GENCOST_SSHV4: There are <',num0,' pts'
      call print_message( msgbuf, errormessageunit,
     &                    SQUEEZE_RIGHT , myThid)
        write(msgbuf,'(A)')
     &  '                  at all grid points for combined tp+ers+gfo.'
      call print_message( msgbuf, errormessageunit,
     &                    SQUEEZE_RIGHT , myThid)
        write(msgbuf,'(A)')
     &  'So, all model slaob minus model sla2model_raw are set to 0.'
      call print_message( msgbuf, errormessageunit,
     &                    SQUEEZE_RIGHT , myThid)
      endif

      if (igen_gmsl.GT.0) then
       if (offset_sum .EQ. 0.0) then
        if (gencost_outputlevel(igen_gmsl).GT.0) then
        _BEGIN_MASTER( myThid )
        write(msgbuf,'(a)') ' sshv4: offset_sum = zero!'
        call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , myThid)
        _END_MASTER( myThid )
        endif
c       stop   '  ... stopped in cost_ssh.'
       else
        if (gencost_outputlevel(igen_gmsl).GT.0) then
        _BEGIN_MASTER( myThid )
        write(msgbuf,'(a,2d22.15)') ' sshv4:offset,sum=',
     &      offset,offset_sum
        call print_message( msgbuf, standardmessageunit,
     &                          SQUEEZE_RIGHT , myThid)
        _END_MASTER( myThid )
        endif
       endif
c     endif igen_gmsl .gt. 0
      endif

c--   Compute (average) offset
      if ( offset_sum.NE.0. _d 0 ) then
        offset = offset / offset_sum
      else
        offset = 0. _d 0
      endif

c--   subtract offset from mean_psMssh_all and psmean
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax

               if ( (mean_psMssh_all_NUM(i,j,bi,bj) .GT. num0) .AND.
     &              ( mdtma(i,j,bi,bj) .NE. 0. ) .AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ) .AND.
     &              ( doReference ) ) then
c use the re-referencing approach
                   mean_psMssh_all(i,j,bi,bj) =
     &                 mean_psMssh_all(i,j,bi,bj) - offset
                   mean_psMssh_all_MSK(i,j,bi,bj) = 1. _d 0

               elseif ( ( mdtma(i,j,bi,bj) .NE. 0. ) .AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. ) .AND.
     &              ( .NOT.doReference ) ) then
c use the simpler approach
                   mean_psMssh_all(i,j,bi,bj) =
     &             psmean(i,j,bi,bj) -mdtob(i,j,bi,bj) - offset
                   mean_psMssh_all_MSK(i,j,bi,bj) = 1. _d 0

               else
                   mean_psMssh_all(i,j,bi,bj) = 0. _d 0
                   mean_psMssh_all_MSK(i,j,bi,bj) = 0. _d 0
               endif

               if ( maskc(i,j,1,bi,bj) .NE. 0. )
     &            psmean(i,j,bi,bj)=psmean(i,j,bi,bj)-offset

            enddo
          enddo
        enddo
      enddo

c--    smooth mean_psMssh_all
      if (igen_mdt.GT.0) then
       if (gencost_outputlevel(igen_mdt).GT.0) then
        write(fname4test,'(1a)') 'mdtdiff_raw'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        mean_psMssh_all, 1, 1, myThid )
        write(fname4test,'(1a)') 'sla2model_raw'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        mean_slaobs_model, 1, 1, myThid )
       endif

#ifdef ALLOW_SMOOTH
       if ( useSMOOTH.AND.(k2_mdt.GT.0) ) then
        call smooth_hetero2d(mean_psMssh_all,maskInC,
     &     gencost_posproc_c(k2_mdt,igen_mdt),
     &     gencost_posproc_i(k2_mdt,igen_mdt),myThid)
        call smooth_hetero2d(mean_slaobs_model,maskInC,
     &     gencost_posproc_c(k2_mdt,igen_mdt),
     &     gencost_posproc_i(k2_mdt,igen_mdt),myThid)

        if (gencost_outputlevel(igen_mdt).GT.0) then
         write(fname4test,'(1a)') 'mdtdiff_smooth'
         CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                         mean_psMssh_all, 1, 1, myThid )
         write(fname4test,'(1a)') 'sla2model_smooth'
         CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                         mean_slaobs_model, 1, 1, myThid )
        endif
       endif
#endif

cgf at this point:
cgf     1) mean_psMssh_all is the sample mean <etagcm-slaobs-mdt-offset>,
cgf             to which a smoothing filter has been applied.
cgf     2) mean_psMtpobs is the (unsmoothed) sample mean <etagcm-tpobs>.
cgf             And similarly for ers and gfo, each treated separately.

cgf =======================================================
cgf PART 3: compute MDT cost term
cgf =======================================================

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
       if (mean_psMssh_all_MSK(i,j,bi,bj).NE.0. _d 0) then
         junk = mean_psMssh_all(i,j,bi,bj)
         junkweight = gencost_weight(i,j,bi,bj,igen_mdt)
     &              * mdtma(i,j,bi,bj)
       else
         junk = 0. _d 0
         junkweight = 0. _d 0
       endif
       objf_gencost(bi,bj,igen_mdt) = objf_gencost(bi,bj,igen_mdt)
     &      + junk*junk*junkweight
       if ( junkweight .NE. 0. ) num_gencost(bi,bj,igen_mdt) =
     &      num_gencost(bi,bj,igen_mdt) + 1. _d 0
       diagnosfld(i,j,bi,bj) = junk*junk*junkweight
          enddo
         enddo
        enddo
       enddo

       if (gencost_outputlevel(igen_mdt).GT.0) then
        write(fname4test,'(1a)') 'misfits_mdt'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        diagnosfld, 1, 1, myThid )
       endif
c     endif igen_mdt .gt. 0
      endif

cgf =======================================================
cgf PART 4: compute smooth SLA cost term
cgf =======================================================

      if (use_mon) then
        nrecsave=1
      else
        nrecsave=35
      endif

C add a warning if nrec_total is less than the hardcoded nrecsave
      tempinteger=nrec_total-nrecsave+1
#ifdef ECCO_VERBOSE
      write(msgbuf,'(A,i5,1x,i5,1x,i5)')
     & 'sshv4: nrec[save,_total],tempinteger',
     &     nrecsave,nrec_total,tempinteger
#endif
      if(tempinteger.LT.1) then
        write(msgbuf,'(A,i5)')
     &  '** WARNING ** S/R COST_GENCOST_SSHV4: There are < ',nrecsave
        call print_message( msgbuf, errormessageunit,
     &                      SQUEEZE_RIGHT , myThid)
        if (use_mon) then
         write(msgbuf,'(A)')
     &   '        months required to calculate running mean tp+ers+gfo.'
         call print_message( msgbuf, errormessageunit,
     &                       SQUEEZE_RIGHT , myThid)
        else
         write(msgbuf,'(A)')
     &   '        days required to calculate running mean tp+ers+gfo.'
         call print_message( msgbuf, errormessageunit,
     &                       SQUEEZE_RIGHT , myThid)
        endif
        write(msgbuf,'(A)')
     &  'PART 4 in cost_gencost_sshv4 is skipped entirely, and there'
      call print_message( msgbuf, errormessageunit,
     &                    SQUEEZE_RIGHT , myThid)
        write(msgbuf,'(A)')
     &  'will be NO report of [tp,ers,gfo,lsc,gmsl] in costfunction.'
      call print_message( msgbuf, errormessageunit,
     &                    SQUEEZE_RIGHT , myThid)
      endif

      do irec = 1, nrec_total-nrecsave+1

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
              anom_psMslaobs(i,j,bi,bj)  = 0. _d 0
              anom_psMslaobs_MSK(i,j,bi,bj)  = 0. _d 0
              anom_psMslaobs_SUB(i,j,bi,bj)  = 0. _d 0
              anom_slaobs(i,j,bi,bj)  = 0. _d 0
              anom_slaobs_SUB(i,j,bi,bj)  = 0. _d 0
              anom_psMslaobs_NUM(i,j,bi,bj)  = 0. _d 0
          enddo
         enddo
        enddo
       enddo

c PART 4.1: compute running sample average over nrecsave
c ------------------------------------------------------

      do jrec=1,nrecsave

        krec=irec+jrec-1

#ifdef ALLOW_AUTODIFF
        call active_read_xy( fname, etagcm, krec, doglobalread,
     &                       ladinit, eccoiter, myThid,
     &                       gencost_dummy(igen_etagcm) )
#else
        CALL READ_REC_XY_RL( fname, etagcm, kRec, 1, myThid )
#endif

        if (.NOT.useEtaMean)
     &  CALL REMOVE_MEAN_RL( 1, etagcm, maskInC, maskInC, rA, drF,
     &                       'etagcm', zeroRL, 1, myThid )

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
              tpma(i,j,bi,bj) = 0. _d 0
              tpob(i,j,bi,bj) = 0. _d 0
              ersma(i,j,bi,bj) = 0. _d 0
              ersob(i,j,bi,bj) = 0. _d 0
              gfoma(i,j,bi,bj) = 0. _d 0
              gfoob(i,j,bi,bj) = 0. _d 0
          enddo
         enddo
        enddo
       enddo

        if (using_tpj)
     &  call cost_sla_read( tpfi, tpsta, tpper, use_mon,
     &                zeroRL, zeroRL,
     &                tpob, tpma,
     &                krec, myThid )
        if (using_ers)
     &  call cost_sla_read( ersfi, erssta, ersper, use_mon,
     &                zeroRL, zeroRL,
     &                ersob, ersma,
     &                krec, myThid )
        if (using_gfo)
     &  call cost_sla_read( gfofi, gfosta, gfoper, use_mon,
     &                zeroRL, zeroRL,
     &                gfoob, gfoma,
     &                krec, myThid )

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
      if ( tpma(i,j,bi,bj)*mean_psMslaobs_MSK(i,j,bi,bj)
     &  .NE.0. ) then
           anom_psMslaobs(i,j,bi,bj)= anom_psMslaobs(i,j,bi,bj)+
     &        etagcm(i,j,bi,bj)-tpob(i,j,bi,bj)
     &        -mean_psMslaobs(i,j,bi,bj)
           anom_slaobs(i,j,bi,bj)= anom_slaobs(i,j,bi,bj)+
     &        tpob(i,j,bi,bj)
           anom_psMslaobs_NUM(i,j,bi,bj)=
     &        anom_psMslaobs_NUM(i,j,bi,bj)+1. _d 0
           if ( (2*jrec.EQ.nrecsave).OR.(2*jrec-1.EQ.nrecsave) )
     &        anom_psMslaobs_MSK(i,j,bi,bj)  = 1. _d 0
      endif
      if ( ersma(i,j,bi,bj)*mean_psMslaobs_MSK(i,j,bi,bj)
     &  .NE.0. ) then
           anom_psMslaobs(i,j,bi,bj)= anom_psMslaobs(i,j,bi,bj)+
     &        etagcm(i,j,bi,bj)-ersob(i,j,bi,bj)
     &        -mean_psMslaobs(i,j,bi,bj)
           anom_slaobs(i,j,bi,bj)= anom_slaobs(i,j,bi,bj)+
     &        ersob(i,j,bi,bj)
           anom_psMslaobs_NUM(i,j,bi,bj)=
     &        anom_psMslaobs_NUM(i,j,bi,bj)+1. _d 0
           if ( (2*jrec.EQ.nrecsave).OR.(2*jrec-1.EQ.nrecsave) )
     &        anom_psMslaobs_MSK(i,j,bi,bj)  = 1. _d 0
      endif
      if ( gfoma(i,j,bi,bj)*mean_psMslaobs_MSK(i,j,bi,bj)
     &  .NE.0. ) then
           anom_psMslaobs(i,j,bi,bj)= anom_psMslaobs(i,j,bi,bj)+
     &        etagcm(i,j,bi,bj)-gfoob(i,j,bi,bj)
     &        -mean_psMslaobs(i,j,bi,bj)
           anom_slaobs(i,j,bi,bj)= anom_slaobs(i,j,bi,bj)+
     &        gfoob(i,j,bi,bj)
           anom_psMslaobs_NUM(i,j,bi,bj)=
     &        anom_psMslaobs_NUM(i,j,bi,bj)+1. _d 0
           if ( (2*jrec.EQ.nrecsave).OR.(2*jrec-1.EQ.nrecsave) )
     &        anom_psMslaobs_MSK(i,j,bi,bj)  = 1. _d 0
      endif
          enddo
         enddo
        enddo
       enddo

      enddo !do jrec=1,nrecsave

        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
               if ( ( anom_psMslaobs_NUM(i,j,bi,bj) .NE. 0. ).AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. )
     &            ) then
                  anom_psMslaobs(i,j,bi,bj) =
     &                 anom_psMslaobs(i,j,bi,bj) /
     &                 anom_psMslaobs_NUM(i,j,bi,bj)
                  anom_slaobs(i,j,bi,bj) =
     &                 anom_slaobs(i,j,bi,bj) /
     &                 anom_psMslaobs_NUM(i,j,bi,bj)
               else
                  anom_psMslaobs(i,j,bi,bj) = 0. _d 0
                  anom_slaobs(i,j,bi,bj) = 0. _d 0
               endif
              enddo
            enddo
          enddo
        enddo

c PART 4.11: separate time variable offset
c ----------------------------------------

      slaoffset     = 0. _d 0
      slaoffset_sum = 0. _d 0
      slaoffset_weight = 0. _d 0

      if (igen_lsc.GT.0) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
               if ( ( anom_psMslaobs_NUM(i,j,bi,bj) .NE. 0. ).AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. )
     &            ) then
                  slaoffset=slaoffset+RA(i,j,bi,bj)*
     &                      anom_psMslaobs(i,j,bi,bj)
                  slaoffset_sum=slaoffset_sum+RA(i,j,bi,bj)
C Of interest is the total weight for an average of N
C indep sample (not the area weighted average of weight)
C Since slaoffset is itself area weighted, however, the
C total weight is only approx the simple weight sum:
                  slaoffset_weight=slaoffset_weight+
     &                      gencost_weight(i,j,bi,bj,igen_lsc)
               endif
              enddo
            enddo
          enddo
        enddo

      _GLOBAL_SUM_RL( slaoffset     , myThid )
      _GLOBAL_SUM_RL( slaoffset_weight , myThid )
      _GLOBAL_SUM_RL( slaoffset_sum , myThid )

       if (igen_gmsl.GT.0) then
        if (slaoffset_sum .EQ. 0.0) then
         if (gencost_outputlevel(igen_gmsl).GT.0) then
         _BEGIN_MASTER( myThid )
         write(msgbuf,'(a)') ' sshv4: slaoffset_sum = zero!'
         call print_message( msgbuf, standardmessageunit,
     &                           SQUEEZE_RIGHT , myThid)
         _END_MASTER( myThid )
         endif
c        stop   '  ... stopped in cost_ssh.'
        else
         if (gencost_outputlevel(igen_gmsl).GT.0) then
         _BEGIN_MASTER( myThid )
         write(msgbuf,'(a,3d22.15)') ' sshv4:slaoffset,sum,weight=',
     &          slaoffset,slaoffset_sum,slaoffset_weight
         call print_message( msgbuf, standardmessageunit,
     &                           SQUEEZE_RIGHT , myThid)
         _END_MASTER( myThid )
         endif
        endif
c      endif igen_gmsl .gt. 0
       endif

c--   Compute (average) slaoffset
       if (slaoffset_sum.NE.0. _d 0) then
        slaoffset = slaoffset / slaoffset_sum
       else
        slaoffset = 0. _d 0
       endif

c--   Subtract slaoffset from anom_psMslaobs
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
               if ( ( anom_psMslaobs_NUM(i,j,bi,bj) .NE. 0. ).AND.
     &              ( maskc(i,j,1,bi,bj) .NE. 0. )
     &            ) then
                  anom_psMslaobs(i,j,bi,bj) =
     &                 anom_psMslaobs(i,j,bi,bj) - slaoffset
                  anom_slaobs(i,j,bi,bj) =
     &                 anom_slaobs(i,j,bi,bj) - slaoffset
               else
                  anom_psMslaobs(i,j,bi,bj) = 0. _d 0
                  anom_slaobs(i,j,bi,bj) = 0. _d 0
               endif
              enddo
            enddo
          enddo
        enddo

c PART 4.2: smooth anom_psMslaobs in space
c ----------------------------------------

      if (gencost_outputlevel(igen_lsc).GT.0) then
        write(fname4test,'(1a)') 'sladiff_raw'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        anom_psMslaobs, irec, 1, myThid )
        write(fname4test,'(1a)') 'slaobs_raw'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        anom_slaobs, irec, 1, myThid )
      endif

#ifdef ALLOW_SMOOTH
      if ( useSMOOTH.AND.(k2_lsc.GT.0) ) then
        call smooth_hetero2d(anom_psMslaobs,maskInC,
     &     gencost_posproc_c(k2_lsc,igen_lsc),
     &     gencost_posproc_i(k2_lsc,igen_lsc),myThid)
        call smooth_hetero2d(anom_slaobs,maskInC,
     &     gencost_posproc_c(k2_lsc,igen_lsc),
     &     gencost_posproc_i(k2_lsc,igen_lsc),myThid)

        if (gencost_outputlevel(igen_lsc).GT.0) then
         write(fname4test,'(1a)') 'sladiff_smooth'
         CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                         anom_psMslaobs, irec, 1, myThid )
         write(fname4test,'(1a)') 'slaobs_smooth'
         CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                         anom_slaobs, irec, 1, myThid )
        endif
      endif
#endif

c PART 4.3: compute cost function term
c ------------------------------------

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
          if ( (gencost_weight(i,j,bi,bj,igen_lsc).GT.0.).AND.
     &         (anom_psMslaobs_MSK(i,j,bi,bj).GT.0.) ) then
            junk = anom_psMslaobs(i,j,bi,bj)
            anom_slaobs_SUB(i,j,bi,bj) = anom_slaobs(i,j,bi,bj)
            anom_psMslaobs_SUB(i,j,bi,bj) = anom_psMslaobs(i,j,bi,bj)
          else
            junk = 0. _d 0
            anom_slaobs_SUB(i,j,bi,bj)= 0. _d 0
            anom_psMslaobs_SUB(i,j,bi,bj)= 0. _d 0
          endif
          objf_gencost(bi,bj,igen_lsc) = objf_gencost(bi,bj,igen_lsc)
     &        + junk*junk*gencost_weight(i,j,bi,bj,igen_lsc)
          if ( (gencost_weight(i,j,bi,bj,igen_lsc).GT.0.).AND.
     &         (anom_psMslaobs_MSK(i,j,bi,bj).GT.0.) )
     &         num_gencost(bi,bj,igen_lsc) =
     &         num_gencost(bi,bj,igen_lsc) + 1. _d 0
           enddo
         enddo
        enddo
       enddo

       if (gencost_outputlevel(igen_lsc).GT.0) then
        write(fname4test,'(1a)') 'sladiff_subsample'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        anom_psMslaobs_SUB, irec, 1, myThid )
        write(fname4test,'(1a)') 'slaobs_subsample'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        anom_slaobs_SUB, irec, 1, myThid )
       endif

c     endif igen_lsc .gt. 0
      endif

c PART 4.4: compute cost function term for global mean sea level
c --------------------------------------------------------------

      IF ( MASTER_CPU_THREAD(myThid) .AND. ( igen_gmsl.GT.0 )
     &     .AND. ( slaoffset_sum.GT.0.25 _d 0 * globalArea ) ) THEN
          junk=slaoffset
          junkweight=1. _d 0
          objf_gencost(1,1,igen_gmsl) = objf_gencost(1,1,igen_gmsl)
     &        + junk*junk*junkweight
          num_gencost(1,1,igen_gmsl) = num_gencost(1,1,igen_gmsl)
     &        + 1. _d 0
c      print*,'igen_gmsl',igen_gmsl,
      ENDIF

cgf =======================================================
cgf PART 5: compute raw SLA cost term
cgf =======================================================

c time associated with this ndaysrec mean
        krec = irec + (nrecsave/2)

#ifdef ALLOW_AUTODIFF
        call active_read_xy( fname, etagcm, krec, doglobalread,
     &                       ladinit, eccoiter, myThid,
     &                       gencost_dummy(igen_etagcm) )
#else
        CALL READ_REC_XY_RL( fname, etagcm, kRec, 1, myThid )
#endif

        if (.NOT.useEtaMean)
     &  CALL REMOVE_MEAN_RL( 1, etagcm, maskInC, maskInC, rA, drF,
     &                       'etagcm', zeroRL, 1, myThid )

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
              tpma(i,j,bi,bj) = 0. _d 0
              tpob(i,j,bi,bj) = 0. _d 0
              ersma(i,j,bi,bj) = 0. _d 0
              ersob(i,j,bi,bj) = 0. _d 0
              gfoma(i,j,bi,bj) = 0. _d 0
              gfoob(i,j,bi,bj) = 0. _d 0
          enddo
         enddo
        enddo
       enddo

        if (using_tpj)
     &  call cost_sla_read( tpfi, tpsta, tpper, use_mon,
     &                zeroRL, zeroRL,
     &                tpob, tpma,
     &                krec, myThid )
        if (using_ers)
     &  call cost_sla_read( ersfi, erssta, ersper, use_mon,
     &                zeroRL, zeroRL,
     &                ersob, ersma,
     &                krec, myThid )
        if (using_gfo)
     &  call cost_sla_read( gfofi, gfosta, gfoper, use_mon,
     &                zeroRL, zeroRL,
     &                gfoob, gfoma,
     &                krec, myThid )

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
              anom_psMtpobs(i,j,bi,bj)  = 0. _d 0
              anom_psMersobs(i,j,bi,bj) = 0. _d 0
              anom_psMgfoobs(i,j,bi,bj) = 0. _d 0
              anom_tpobs(i,j,bi,bj)  = 0. _d 0
              anom_ersobs(i,j,bi,bj) = 0. _d 0
              anom_gfoobs(i,j,bi,bj) = 0. _d 0
          enddo
         enddo
        enddo
       enddo

       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
      if ( tpma(i,j,bi,bj)*mean_psMslaobs_MSK(i,j,bi,bj).NE.0. )
     & then
         anom_psMtpobs(i,j,bi,bj)=
     &       etagcm(i,j,bi,bj) - tpob(i,j,bi,bj)
     &       - mean_psMslaobs(i,j,bi,bj) - slaoffset
     &       - anom_psMslaobs(i,j,bi,bj)
         anom_tpobs(i,j,bi,bj)=tpob(i,j,bi,bj) - slaoffset
     &       - anom_slaobs(i,j,bi,bj)
      endif
      if ( ersma(i,j,bi,bj)*mean_psMslaobs_MSK(i,j,bi,bj).NE.0. )
     & then
         anom_psMersobs(i,j,bi,bj)=
     &       etagcm(i,j,bi,bj) - ersob(i,j,bi,bj)
     &       - mean_psMslaobs(i,j,bi,bj) - slaoffset
     &       - anom_psMslaobs(i,j,bi,bj)
         anom_ersobs(i,j,bi,bj)=ersob(i,j,bi,bj) - slaoffset
     &       - anom_slaobs(i,j,bi,bj)
      endif
      if ( gfoma(i,j,bi,bj)*mean_psMslaobs_MSK(i,j,bi,bj).NE.0. )
     & then
         anom_psMgfoobs(i,j,bi,bj)=
     &       etagcm(i,j,bi,bj) - gfoob(i,j,bi,bj)
     &       - mean_psMslaobs(i,j,bi,bj) - slaoffset
     &       - anom_psMslaobs(i,j,bi,bj)
         anom_gfoobs(i,j,bi,bj)=gfoob(i,j,bi,bj) - slaoffset
     &       - anom_slaobs(i,j,bi,bj)
      endif
          enddo
         enddo
        enddo
       enddo

      if ( igen_tp .GT. 0 ) then
       if (gencost_outputlevel(igen_tp).GT.0) then
        write(fname4test,'(1a)') 'sladiff_tp_raw'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        anom_psMtpobs, irec, 1, myThid )
        write(fname4test,'(1a)') 'slaobs_tp_raw'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        anom_tpobs, irec, 1, myThid )
       endif
      endif

      if ( igen_ers .GT. 0 ) then
       if (gencost_outputlevel(igen_ers).GT.0) then
        write(fname4test,'(1a)') 'sladiff_ers_raw'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        anom_psMersobs, irec, 1, myThid )
        write(fname4test,'(1a)') 'slaobs_ers_raw'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        anom_ersobs, irec, 1, myThid )
       endif
      endif

      if ( igen_gfo .GT. 0 ) then
       if (gencost_outputlevel(igen_gfo).GT.0) then
        write(fname4test,'(1a)') 'sladiff_gfo_raw'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        anom_psMgfoobs, irec, 1, myThid )
        write(fname4test,'(1a)') 'slaobs_gfo_raw'
        CALL WRITE_REC_3D_RL( fname4test, precFloat32, 1,
     &                        anom_gfoobs, irec, 1, myThid )
       endif
      endif
      if ( igen_tp .GT. 0 ) then
       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
c--             The array psobs contains SSH anomalies.
                junkweight = mean_psMslaobs_MSK(i,j,bi,bj)
     &                      *gencost_weight(i,j,bi,bj,igen_tp)
     &                      *tpma(i,j,bi,bj)
                junk = anom_psMtpobs(i,j,bi,bj)
                objf_gencost(bi,bj,igen_tp) =
     &              objf_gencost(bi,bj,igen_tp)+junk*junk*junkweight
                if ( junkweight .NE. 0. )
     &              num_gencost(bi,bj,igen_tp) =
     &              num_gencost(bi,bj,igen_tp) + 1. _d 0
          enddo
         enddo
        enddo
       enddo
      endif
      if ( igen_ers .GT. 0 ) then
       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
c--             The array ersobs contains SSH anomalies.
                junkweight = mean_psMslaobs_MSK(i,j,bi,bj)
     &                      *gencost_weight(i,j,bi,bj,igen_ers)
     &                      *ersma(i,j,bi,bj)
                junk = anom_psMersobs(i,j,bi,bj)
                objf_gencost(bi,bj,igen_ers) =
     &               objf_gencost(bi,bj,igen_ers)+junk*junk*junkweight
                if ( junkweight .NE. 0. )
     &              num_gencost(bi,bj,igen_ers) =
     &              num_gencost(bi,bj,igen_ers) + 1. _d 0
          enddo
         enddo
        enddo
       enddo
      endif
      if ( igen_gfo .GT. 0 ) then
       do bj = jtlo,jthi
        do bi = itlo,ithi
         do j = jmin,jmax
          do i = imin,imax
c--             The array gfoobs contains SSH anomalies.
                junkweight = mean_psMslaobs_MSK(i,j,bi,bj)
     &                      *gencost_weight(i,j,bi,bj,igen_gfo)
     &                      *gfoma(i,j,bi,bj)
                junk = anom_psMgfoobs(i,j,bi,bj)
                objf_gencost(bi,bj,igen_gfo) =
     &              objf_gencost(bi,bj,igen_gfo)+junk*junk*junkweight
                if ( junkweight .NE. 0. )
     &              num_gencost(bi,bj,igen_gfo) =
     &              num_gencost(bi,bj,igen_gfo) + 1. _d 0
          enddo
         enddo
        enddo
       enddo
      endif

      enddo !do irec = 1, nrec_total-nrecsave+1

#endif /* ifdef ALLOW_GENCOST_CONTRIBUTION */

      RETURN
      END
