#include "ECCO_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine cost_averagesfields( mytime, mythid )

c     ==================================================================
c     SUBROUTINE cost_averagesfields
c     ==================================================================
c
c     o Compute time averages of etaN, theta, and salt. The counters
c       are explicitly calculated instead of being incremented. This
c       reduces dependencies. The latter is useful for the adjoint code
c       generation.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 24-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_averagesfields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "CG2D.h"

#ifndef ECCO_CTRL_DEPRECATED
# include "ECCO_SIZE.h"
# include "ECCO.h"
#else
# include "ecco_cost.h"
# ifdef ALLOW_CTRL
#  include "optim.h"
#  include "CTRL_SIZE.h"
#  include "ctrl.h"
#  include "ctrl_dummy.h"
#  include "CTRL_GENARR.h"
# endif
# ifdef ALLOW_EXF
#  include "EXF_FIELDS.h"
# endif
# ifdef ALLOW_SEAICE
#  include "SEAICE_SIZE.h"
#  include "SEAICE.h"
#  include "SEAICE_COST.h"
# endif
#endif

c     == routine arguments ==

      _RL     mytime
      integer mythid

c     == local variables ==

      integer myiter
#ifdef ECCO_CTRL_DEPRECATED
      integer bi,bj
      integer i,j
      integer ig,jg
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer ilps, ils,ilt
      integer locdayrec
      logical intmp
#endif
      integer k
      logical first
      logical startofday
      logical startofmonth
      logical startofyear
      logical inday
      logical inmonth
      logical inyear
      logical last
      logical endofday
      logical endofmonth
      logical endofyear
#ifdef ALLOW_GENCOST_CONTRIBUTION
      logical startofgen(NGENCOST)
      logical endofgen(NGENCOST)
      logical ingen(NGENCOST)
      integer sum1gen(NGENCOST)
      integer genrec(NGENCOST)
      integer kk
#endif
#if (defined (ALLOW_CTRL) && \
     defined (ALLOW_GENTIM2D_CONTROL) && \
     defined (ALLOW_PSBAR_GENPRECIP))
      _RL genprecipGloH
      INTEGER iarr
#endif

#ifdef ECCO_CTRL_DEPRECATED

      character*(128) fnamepsbar
      character*(128) fnametbar
      character*(128) fnamesbar
      character*(128) fnameubar
      character*(128) fnamevbar
      character*(128) fnamewbar
      character*(128) fnametauxbar
      character*(128) fnametauybar

      _RL  etanLeads (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_PSBAR_STERIC
      Real*8 sterGloH
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif

#ifdef ALLOW_IESTAU_COST_CONTRIBUTION
      _RL iestau(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL c0mm,prmm,salmm,vtmm,tmpmm,vsmm,vpmm,vstpmm
      _RL csmm(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nr,nsx,nsy)
      _RL SW_TEMP
#endif

c     == external functions ==

      integer  ilnblnk
      external ilnblnk
#ifdef ALLOW_IESTAU_COST_CONTRIBUTION
      external SW_TEMP
#endif

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

#endif /* ECCO_CTRL_DEPRECATED */

      myiter = niter0 + INT((mytime-starttime)/deltaTClock+0.5)

c--   Get the time flags and record numbers for the time averaging.

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_CALL('cost_averagesflags',myThid)
#endif
      call cost_averagesflags(
     I                    myiter,     mytime,       mythid,
     O                    first,      last,
     O                    startofday, startofmonth, startofyear,
     O                    inday,      inmonth,      inyear,
     O                    endofday,   endofmonth,   endofyear,
     O                    sum1day,    dayrec,
     O                    sum1mon,    monrec,
     O                    sum1year,   yearrec
     &                  )

#ifdef ALLOW_GENCOST_CONTRIBUTION
      call cost_gencost_assignperiod(
     I                    startofday, startofmonth, startofyear,
     I                    inday,      inmonth,      inyear,
     I                    endofday,   endofmonth,   endofyear,
     O                    startofgen, endofgen,     ingen,
     O                    sum1gen,    genrec,
     I                    myiter, mythid )
      call cost_gencost_customize( mythid )
#endif

#ifdef ECCO_CTRL_DEPRECATED

#ifdef ALLOW_SSH_COST_CONTRIBUTION
      IF (using_cost_altim) THEN
#ifdef ALLOW_DEBUG
      IF ( debugMode )
     &    CALL DEBUG_CALL('cost_averagesgeneric psbar',myThid)
#endif

#ifdef ALLOW_PSBAR_STERIC
      sterGloH=VOLsumGlob_0/globalArea
     &        *(1. _d 0 - RHOsumGlob/RHOsumGlob_0)

         WRITE(msgBuf,'(A,I6,A,1PE21.14)')
     &    ' iter=', myiter, ' ; sterGloH= ', sterGloH
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
#endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
            do j = jmin,jmax
              do i =  imin,imax
#if (defined (ALLOW_CTRL) && \
     defined (ALLOW_GENTIM2D_CONTROL) && \
     defined (ALLOW_PSBAR_GENPRECIP))
                genprecipGloH=0. _d 0
                do iarr = 1, maxCtrlTim2D
                if (xx_gentim2d_file(iarr).EQ.'xx_gen_precip')
     &          genprecipGloH=xx_gentim2d(i,j,bi,bj,iarr)
     &          *rhoConstFresh*recip_rhoConst*deltaTClock
                enddo
#endif

                etanLeads(i,j,bi,bj)=etan(i,j,bi,bj)
#ifdef ALLOW_SEAICE
     &                +sIceLoad(i,j,bi,bj)*recip_rhoConst
#endif
#ifdef ALLOW_PSBAR_STERIC
     &                +sterGloH
#endif
#if (defined (ALLOW_CTRL) && \
     defined (ALLOW_GENTIM2D_CONTROL) && \
     defined (ALLOW_PSBAR_GENPRECIP))
     &                +genprecipGloH
#endif
              enddo
            enddo
        enddo
      enddo
      call cost_averagesgeneric(
     &     psbarfile,
     &     psbar, etanLeads, xx_psbar_mean_dummy,
     &     first, last, startofday, endofday, inday,
     &     sum1day, dayrec, 1, mythid )
      ENDIF
#endif

#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
#ifdef ALLOW_DEBUG
      IF ( debugMode )
     &    CALL DEBUG_CALL('cost_averagesgeneric sigmaRbar',myThid)
#endif
      call cost_averagesgeneric(
     &     sigmaRbarfile,
     &     sigmaRbar, sigmaRfield, xx_sigmaRbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, nr, mythid )
#endif /* ALLOW_SIGMAR_COST_CONTRIBUTION */

#if (defined (ALLOW_THETA_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDT_COST_CONTRIBUTION) || \
     defined (ALLOW_XBT_COST_CONTRIBUTION) || \
     defined (ALLOW_ARGO_THETA_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
#ifdef ALLOW_DEBUG
      IF ( debugMode )
     &    CALL DEBUG_CALL('cost_averagesgeneric tbar',myThid)
#endif
      call cost_averagesgeneric(
     &     tbarfile,
     &     tbar, theta, xx_tbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, nr, mythid )
#else
#ifdef ALLOW_SST_COST_CONTRIBUTION
      call cost_averagesgeneric(
     &     tbarfile,
     &     tbar, theta(1-Olx,1-Oly,1,1,1), xx_tbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, 1, mythid )
#endif
#endif

#ifdef ALLOW_DAILYSST_COST_CONTRIBUTION
      IF (using_cost_sst) THEN
cph#ifdef ALLOW_SEAICE_COST_AREASST
#ifdef ALLOW_DEBUG
      IF ( debugMode )
     &    CALL DEBUG_CALL('cost_averagesgeneric sstbar',myThid)
#endif
      call cost_averagesgeneric(
     &     sstbarfile,
     &     sstbar, theta(1-Olx,1-Oly,1,1,1), xx_sstbar_mean_dummy,
     &     first, last, startofday, endofday, inday,
     &     sum1day, dayrec, 1, mythid )
      ENDIF !      IF (using_cost_sst) THEN
#endif

#if (defined (ALLOW_SALT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDS_COST_CONTRIBUTION) || \
     defined (ALLOW_ARGO_SALT_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
#ifdef ALLOW_DEBUG
      IF ( debugMode )
     &    CALL DEBUG_CALL('cost_averagesgeneric sbar',myThid)
#endif
      call cost_averagesgeneric(
     &     sbarfile,
     &     sbar, salt, xx_sbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, nr, mythid )
#else
#ifdef ALLOW_SSS_COST_CONTRIBUTION
      call cost_averagesgeneric(
     &     sbarfile,
     &     sbar, salt(1-Olx,1-Oly,1,1,1), xx_sbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, 1, mythid )
#endif
#endif

#ifdef ALLOW_DRIFTW_COST_CONTRIBUTION
      call cost_averagesgeneric(
     &     wbarfile,
     &     wbar, wvel, xx_wbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, nr, mythid )
#endif

#if (defined (ALLOW_DRIFTER_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
cph   There is a mismatch between the cost_drifer and the
cph   cost_obcs usage of ubar, vbar.
cph   cost_obcs refers to monthly means, cost_drifer to total mean.
cph   Needs to be updated for cost_obcs!!!.
c--   Next, do the averages for velocitty.
      if (first.or.startofmonth) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do j = jmin,jmax
                do i =  imin,imax
                  ubar(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)
                  vbar(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
                enddo
              enddo
            enddo
          enddo
        enddo
      else if (last .or. endofmonth) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do j = jmin,jmax
                do i = imin,imax
                  ubar(i,j,k,bi,bj)  = (ubar (i,j,k,bi,bj) +
     &                                  uVel(i,j,k,bi,bj)   )/
     &                                 float(sum1mon)
                  vbar(i,j,k,bi,bj)  = (vbar (i,j,k,bi,bj) +
     &                                  vVel(i,j,k,bi,bj)   )/
     &                                 float(sum1mon)
                enddo
              enddo
            enddo
          enddo
        enddo

c--     Save ubar and vbar.
        if (optimcycle .ge. 0) then
          ils=ilnblnk( ubarfile )
          write(fnameubar,'(2a,i10.10)') ubarfile(1:ils),'.',
     &                                             optimcycle
          write(fnamevbar,'(2a,i10.10)') vbarfile(1:ils),'.',
     &                                             optimcycle
        endif

        call active_write_xyz( fnameubar, ubar, monrec, optimcycle,
     &                         mythid, xx_ubar_mean_dummy)

        call active_write_xyz( fnamevbar, vbar, monrec, optimcycle,
     &                         mythid, xx_vbar_mean_dummy)

ce      , myiter, mytime )

      else if (       (       inmonth         ) .and.
     &          .not. (first .or. startofmonth) .and.
     &       .not. (last  .or. endofmonth  )      ) then
c--     Accumulate ubar and vbar.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do j = jmin,jmax
                do i =  imin,imax
                  ubar(i,j,k,bi,bj) = ubar (i,j,k,bi,bj) +
     &                                uVel (i,j,k,bi,bj)
                  vbar(i,j,k,bi,bj) = vbar (i,j,k,bi,bj) +
     &                                vVel (i,j,k,bi,bj)
                enddo
              enddo
            enddo
          enddo
        enddo
      else
        stop   ' ... stopped in cost_averagesfields; ubar part.'
      endif

#endif

      IF (using_cost_scat) THEN
#ifdef ALLOW_SCAT_COST_CONTRIBUTION
c--   Next, do the averages for velocitty.
      if (first.or. startofmonth) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i =  imin,imax
                  tauxbar(i,j,bi,bj) = ustress(i,j,bi,bj)
                  tauybar(i,j,bi,bj) = vstress(i,j,bi,bj)
                enddo
              enddo
          enddo
        enddo
      else if (last .or. endofmonth) then
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i = imin,imax
                  tauxbar(i,j,bi,bj)  = (tauxbar (i,j,bi,bj) +
     &                                  ustress(i,j,bi,bj)   )/
     &                                 float(sum1mon)
                  tauybar(i,j,bi,bj)  = (tauybar (i,j,bi,bj) +
     &                                  vstress(i,j,bi,bj)   )/
     &                                 float(sum1mon)
                enddo
              enddo
          enddo
        enddo

c--     Save ubar and vbar.
        if (optimcycle .ge. 0) then
          ils=ilnblnk( tauxbarfile )
          write(fnametauxbar,'(2a,i10.10)') tauxbarfile(1:ils),'.',
     &                                             optimcycle
          ils=ilnblnk( tauybarfile )
          write(fnametauybar,'(2a,i10.10)') tauybarfile(1:ils),'.',
     &                                             optimcycle
        endif

        call active_write_xy( fnametauxbar, tauxbar, monrec, optimcycle,
     &                         mythid, xx_taux_mean_dummy)

        call active_write_xy( fnametauybar, tauybar, monrec, optimcycle,
     &                         mythid, xx_tauy_mean_dummy)

      else if ( .not. (first.or. startofmonth) .and.
     &          .not. (last .or. endofmonth)       ) then
c--     Accumulate ubar and vbar.
        do bj = jtlo,jthi
          do bi = itlo,ithi
              do j = jmin,jmax
                do i =  imin,imax
                  tauxbar(i,j,bi,bj) = tauxbar (i,j,bi,bj) +
     &                                ustress (i,j,bi,bj)
                  tauybar(i,j,bi,bj) = tauybar (i,j,bi,bj) +
     &                                vstress (i,j,bi,bj)
                enddo
              enddo
          enddo
        enddo
      else
        stop   ' ... stopped in cost_averagesfields; tauxbar part.'
      endif

#else
#ifdef ALLOW_DAILYSCAT_COST_CONTRIBUTION
      call cost_averagesgeneric(
     &     tauxbarfile,
     &     tauxbar, ustress, xx_taux_mean_dummy,
     &     first, last, startofday, endofday, inday,
     &     sum1day, dayrec, 1, mythid )
      call cost_averagesgeneric(
     &     tauybarfile,
     &     tauybar, vstress, xx_tauy_mean_dummy,
     &     first, last, startofday, endofday, inday,
     &     sum1day, dayrec, 1, mythid )
#endif
#endif
      ENDIF !      IF (using_cost_scat) THEN

#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
cph: this is one mean over whole integration:
c      intmp = (.NOT. first) .and. (.NOT. last)
c      call cost_averagesgeneric(
c     &     hfluxmeanbarfile,
c     &     hfluxmeanbar, qnet, xx_hflux_mean_dummy,
c     &     first, last, .false., .false., intmp,
c     &     ntimesteps, 1, 1, mythid )
cph: switch to annual means:
#ifdef ALLOW_DEBUG
      IF ( debugMode )
     &    CALL DEBUG_CALL('cost_averagesgeneric hfluxmeanbar',myThid)
#endif
      call cost_averagesgeneric(
     &     hfluxmeanbarfile,
     &     hfluxmeanbar, qnet, xx_hflux_mean_dummy,
     &     first, last, startofyear, endofyear, inyear,
     &     sum1year, yearrec, 1, mythid )
#endif

#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
cph: these are annual means
# ifndef ALLOW_SEAICE
#ifdef ALLOW_DEBUG
      IF ( debugMode )
     &    CALL DEBUG_CALL('cost_averagesgeneric sfluxmeanbar',myThid)
#endif
      call cost_averagesgeneric(
     &     sfluxmeanbarfile,
     &     sfluxmeanbar, empmr, xx_sflux_mean_dummy,
     &     first, last, startofyear, endofyear, inyear,
     &     sum1year, yearrec, 1, mythid )
# else
#ifdef ALLOW_DEBUG
      IF ( debugMode )
     &    CALL DEBUG_CALL('cost_averagesgeneric sfluxmeanbar',myThid)
#endif
      call cost_averagesgeneric(
     &     sfluxmeanbarfile,
     &     sfluxmeanbar, frWtrAtm, xx_sflux_mean_dummy,
     &     first, last, startofyear, endofyear, inyear,
     &     sum1year, yearrec, 1, mythid )
# endif
#endif

#ifdef ALLOW_BP_COST_CONTRIBUTION
      IF (using_cost_bp) call cost_averagesgeneric(
     &     bpbarfile,
     &     bpbar, phiHydLow, xx_bpbar_mean_dummy,
     &     first, last, startofmonth, endofmonth, inmonth,
     &     sum1mon, monrec, 1, mythid )
#endif

#ifdef ALLOW_SEAICE
      if (useSEAICE) then
# ifdef ALLOW_SEAICE_COST_SMR_AREA
c
#ifdef ALLOW_DEBUG
      IF ( debugMode )
     &    CALL DEBUG_CALL('cost_averagesgeneric smrareabar',myThid)
#endif
      call cost_averagesgeneric(
     &     smrareabarfile,
     &     smrareabar, area, xx_smrareabar_mean_dummy,
     &     first, last, startofday, endofday, inday,
     &     sum1day, dayrec, 1, mythid )
c
#ifdef ALLOW_DEBUG
      IF ( debugMode )
     &    CALL DEBUG_CALL('cost_averagesgeneric smrsstbar',myThid)
#endif
      call cost_averagesgeneric(
     &     smrsstbarfile,
     &     smrsstbar, theta(1-Olx,1-Oly,1,1,1),
     &     xx_smrsstbar_mean_dummy,
     &     first, last, startofday, endofday, inday,
     &     sum1day, dayrec, 1, mythid )
c
#ifdef ALLOW_DEBUG
      IF ( debugMode )
     &    CALL DEBUG_CALL('cost_averagesgeneric smrsssbar',myThid)
#endif
      call cost_averagesgeneric(
     &     smrsssbarfile,
     &     smrsssbar, salt(1-Olx,1-Oly,1,1,1),
     &     xx_smrsssbar_mean_dummy,
     &     first, last, startofday, endofday, inday,
     &     sum1day, dayrec, 1, mythid )
c
# endif
      endif
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_IESTAU_COST_CONTRIBUTION
cmm  First need to determine sound speed for each cell
       c0mm=1402.392
       prmm = 0.0
       salmm = 0.0
       vtmm = 0.0
       tmpmm = 0.0
       vsmm = 0.0
       vpmm = 0.0
       vstpmm = 0.0
       do bj = jtlo,jthi
         do bi = itlo,ithi
            do j = jmin,jmax
              do i =  imin,imax
                iestau(i,j,bi,bj) = 0.0
              enddo
            enddo
         enddo
       enddo
       do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do j = jmin,jmax
                do i =  imin,imax
                 csmm(i,j,k,bi,bj) = 0.0
                enddo
              enddo
            enddo
          enddo
        enddo
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do j = jmin,jmax
                do i =  imin,imax
                 if (hFacC(i,j,k,bi,bj).gt.0.0) then
                  prmm = totPhiHyd(i,j,k,bi,bj)/gravity
     &                  -rC(k)
                  tmpmm = SW_TEMP(SALT(i,j,k,bi,bj),
     &            THETA(i,j,k,bi,bj),prmm,0. _d 0)
                  prmm = rhoConst*(
     &                   totPhiHyd(i,j,k,bi,bj)
     &                  -rC(k)*gravity )
cmm convert pressure to kg/cm^2 for compliance with aog_vsdg script
                  prmm = prmm*0.0001/gravity
                  salmm = SALT(i,j,k,bi,bj)
                  vtmm = (5.01109398873-(0.0550946843172
     &            - 0.000221535969240*tmpmm)*tmpmm)*tmpmm
                  vsmm=(1.32952290781 + 0.000128955756844*salmm)*salmm
                  vpmm=(0.156059257041 + (0.0000244998688441
     &             - 0.00000000883392332513*prmm)*prmm)*prmm
                  vstpmm=-0.0127562783426*tmpmm*salmm
     &             + 0.00635191613389*tmpmm*prmm
     &             + 0.0000000265484716608*tmpmm*tmpmm*prmm*prmm
     &             - 0.00000159349479045*tmpmm*prmm*prmm
     &             + 0.000000000522116437235*tmpmm*prmm*prmm*prmm
     &             - 0.000000438031096213*tmpmm*tmpmm*tmpmm*prmm
     &             - 0.00000000161674495909*salmm*salmm*prmm*prmm
     &             + 0.0000968403156410*tmpmm*tmpmm*salmm
     &             + 0.00000485639620015*tmpmm*salmm*salmm*prmm
     &             - 0.000340597039004*tmpmm*salmm*prmm
                  csmm(i,j,k,bi,bj) = c0mm+vtmm+vsmm+vpmm+vstpmm
                 endif
                enddo
              enddo
            enddo
          enddo
        enddo
CMM  now integrate to get round trip travel time
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do j = jmin,jmax
                do i =  imin,imax
                  if (csmm(i,j,k,bi,bj).ne.0.0) then
                    iestau(i,j,bi,bj) = iestau(i,j,bi,bj)
     &                   + 2*drF(k)*hFacC(i,j,k,bi,bj)
     &                              /csmm(i,j,k,bi,bj)
                    if (k.eq.1) then
                      iestau(i,j,bi,bj) = iestau(i,j,bi,bj)
     &                   + 2*etaN(i,j,bi,j)
     &                              /csmm(i,j,k,bi,bj)
                    endif
                  endif
               enddo
              enddo
            enddo
          enddo
        enddo
cmm   accumulate average
      call cost_averagesgeneric(
     &     iestaubarfile,
     &     iestaubar, iestau(1-Olx,1-Oly,1,1),
     &     xx_iestaubar_mean_dummy,
     &     first, last, startofday, endofday, inday,
     &     sum1day, dayrec, 1, mythid )
#endif

#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_GENCOST_CONTRIBUTION
      do k = 1, NGENCOST
      if ( (using_gencost(k)).AND.(.NOT.gencost_barskip(k)) ) then
      if ( .NOT.gencost_is3d(k) ) then
        call cost_averagesgeneric(
     &       gencost_barfile(k),
     &       gencost_barfld(1-Olx,1-Oly,1,1,k),
     &       gencost_modfld(1-Olx,1-Oly,1,1,k),
     &       gencost_dummy(k),
     &       first, last,
     &       startofgen(k), endofgen(k), ingen(k),
     &       sum1gen(k), genrec(k), 1, mythid )
#ifdef ALLOW_GENCOST3D
      else
        kk=gencost_pointer3d(k)
        call cost_averagesgeneric(
     &     gencost_barfile(k),
     &     gencost_bar3d(1-Olx,1-Oly,1,1,1,kk),
     &     gencost_mod3d(1-Olx,1-Oly,1,1,1,kk),
     &     gencost_dummy(k),
     &     first, last,
     &     startofgen(k), endofgen(k), ingen(k),
     &     sum1gen(k), genrec(k), nr, mythid )
#endif
      endif

      endif
      end do
#endif /* ALLOW_GENCOST_CONTRIBUTION */

#ifdef ECCO_CTRL_DEPRECATED

#ifdef ALLOW_TRANSPORT_COST_CONTRIBUTION
c--   Currently hard-coded Florida Strait transport for 1x1 deg.
c--   ECCO-GODAE version 1,2,3
c--   Next, do the averages for velocitty.
cph   For some funny reason cal only increments dayrec at the end
cph   of the day, i.e. for endofday.EQ.T
cph   Should fix/change this at some point.
cph   In the mean time increment ad hoc during day
       locdayrec = 0
       if (last .or. endofday) then
        locdayrec = dayrec
       else
        locdayrec = dayrec+1
       endif
       do bj = jtlo,jthi
        do bi = itlo,ithi
         if (first.or.startofday)
     &          transpbar(locdayrec,bi,bj) = 0. _d 0
         do k = 1,nr
          do j = jmin,jmax
           jg = myYGlobalLo-1+(bj-1)*sNy+j
           do i =  imin,imax
            ig = myXGlobalLo-1+(bi-1)*sNx+i
            if ( jg.EQ.106 .AND. ig.GE.280 .AND. ig.LE.285 ) then
               transpbar(locdayrec,bi,bj) = transpbar(locdayrec,bi,bj)
     &              +vVel(i,j,k,bi,bj)
     &              *_dxG(i,j,bi,bj)*drF(k)*_hFacS(i,j,k,bi,bj)
            endif
           enddo
          enddo
         enddo
         if (last .or. endofday) then
            transpbar(locdayrec,bi,bj) =
     &        transpbar(locdayrec,bi,bj)/float(sum1day)
         endif
        enddo
       enddo
#endif

c#ifdef ALLOW_COST_ATLANTIC
c--   Compute meridional heat transport
c     call timer_start('cost_atlantic      [ECCO SPIN-DOWN]', mythid)
c     call cost_atlantic(  mytime, myiter,mythid )
c     call timer_stop ('cost_atlantic      [ECCO SPIN-DOWN]', mythid)
c#endif

#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_LEAVE('cost_averagesfields',myThid)
#endif

      return
      end
