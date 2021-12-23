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

#ifdef ECCO_CTRL_DEPRECATED
# include "ecco_cost.h"
# ifdef ALLOW_CTRL
#  include "ctrl.h"
# endif
#else
# include "ECCO_SIZE.h"
# include "ECCO.h"
# include "ecco_local_params.h"
#endif /* ECCO_CTRL_DEPRECATED */
#ifdef ALLOW_CTRL
# include "CTRL_OBCS.h"
#endif

c     == routine arguments ==

      integer  myThid

c     == local variables ==

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS)) \
      || (defined ECCO_CTRL_DEPRECATED)
      integer k
      integer gwUnit
      integer ilo,ihi
      integer iobcs
#endif
#ifdef ECCO_CTRL_DEPRECATED
      integer bi,bj
      integer i,j
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec,nnz
      integer num_var
#endif

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS)) \
      || (defined ECCO_CTRL_DEPRECATED)
      _RL ratio
      _RL wti(nr)
      _RL wsi(nr)
      _RL wui(nr)
      _RL wvi(nr)
      _RL dummy
#endif
#ifdef ECCO_CTRL_DEPRECATED
      _RL factor
      _RL whflux0m
      _RL wsflux0m
      _RL wtau0m
      _RS dummyRS
      _RL wsshv4tmp ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )

      logical lwtheta2InUse
      logical lwsalt2InUse
      logical lwthetaLevInUse
      logical lwsaltLevInUse
#endif

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS)) \
      || (defined ECCO_CTRL_DEPRECATED)
      logical  exst
#endif

c     == external ==

      integer  ifnblnk
      external ifnblnk
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

#ifdef ECCO_CTRL_DEPRECATED
      lwtheta2InUse   = .false.
      lwsalt2InUse    = .false.
      lwthetaLevInUse = .false.
      lwsaltLevInUse  = .false.

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1-OLy
      jmax = sNy+OLy
      imin = 1-OLx
      imax = sNx+OLx

c--   Initialize background weights
      whflux0m = whflux0
      wsflux0m = wsflux0
      wtau0m   = wtau0
#endif /* ECCO_CTRL_DEPRECATED */

c--   Initialize variance (weight) fields.
#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS)) \
      || (defined ECCO_CTRL_DEPRECATED)
      do k = 1,nr
         wti(k) = 0. _d 0
         wsi(k) = 0. _d 0
         wui(k) = 0. _d 0
         wvi(k) = 0. _d 0
      enddo
#endif
#ifdef ECCO_CTRL_DEPRECATED
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              whflux  (i,j,bi,bj) = 0. _d 0
              whfluxm (i,j,bi,bj) = 0. _d 0
              wsflux  (i,j,bi,bj) = 0. _d 0
              wsfluxm (i,j,bi,bj) = 0. _d 0
              wtauu   (i,j,bi,bj) = 0. _d 0
              wtauum  (i,j,bi,bj) = 0. _d 0
              wtauv   (i,j,bi,bj) = 0. _d 0
              wtauvm  (i,j,bi,bj) = 0. _d 0
              watemp  (i,j,bi,bj) = 0. _d 0
              waqh    (i,j,bi,bj) = 0. _d 0
              wprecip (i,j,bi,bj) = 0. _d 0
              wswflux (i,j,bi,bj) = 0. _d 0
              wswdown (i,j,bi,bj) = 0. _d 0
              wsnowprecip (i,j,bi,bj) = 0. _d 0
              wlwflux (i,j,bi,bj) = 0. _d 0
              wlwdown (i,j,bi,bj) = 0. _d 0
              wevap   (i,j,bi,bj) = 0. _d 0
              wapressure(i,j,bi,bj) = 0. _d 0
              wrunoff (i,j,bi,bj) = 0. _d 0
              wuwind  (i,j,bi,bj) = 0. _d 0
              wvwind  (i,j,bi,bj) = 0. _d 0
              wsst    (i,j,bi,bj) = 0. _d 0
              wsss    (i,j,bi,bj) = 0. _d 0
              wtp     (i,j,bi,bj) = 0. _d 0
              wers    (i,j,bi,bj) = 0. _d 0
              wgfo    (i,j,bi,bj) = 0. _d 0
              wetan   (i,j,bi,bj) = 0. _d 0
              do num_var=1,NSSHV4COST
                wsshv4  (i,j,num_var,bi,bj) = 0. _d 0
              enddo
              wp      (i,j,bi,bj) = 0. _d 0
              wudrift (i,j,bi,bj) = 0. _d 0
              wvdrift (i,j,bi,bj) = 0. _d 0
cph(
              whflux2 (i,j,bi,bj) = 0. _d 0
              wsflux2 (i,j,bi,bj) = 0. _d 0
              wtauu2  (i,j,bi,bj) = 0. _d 0
              wtauv2  (i,j,bi,bj) = 0. _d 0
cph)
              wbottomdrag (i,j,bi,bj) = wbottomdrag0
            enddo
          enddo
        enddo
      enddo
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,Nr
            wtheta (k,bi,bj) = 0. _d 0
            wsalt  (k,bi,bj) = 0. _d 0
            wuvel  (k,bi,bj) = 0. _d 0
            wvvel  (k,bi,bj) = 0. _d 0
            wctdt  (k,bi,bj) = 0. _d 0
            wctds  (k,bi,bj) = 0. _d 0
            wdiffkr(k,bi,bj) = wdiffkr0
            wkapgm (k,bi,bj) = wkapgm0
            wkapredi (k,bi,bj) = wkapredi0
            wedtaux(k,bi,bj) = wedtau0
            wedtauy(k,bi,bj) = wedtau0
            do j = jmin,jmax
              do i = imin,imax
                wtheta2 (i,j,k,bi,bj) = 0. _d 0
                wsalt2  (i,j,k,bi,bj) = 0. _d 0
                wdiffkr2(i,j,k,bi,bj) = wdiffkr0
                wkapgm2 (i,j,k,bi,bj) = wkapgm0
                wkapredi2 (i,j,k,bi,bj) = wkapredi0
                wedtaux2(i,j,k,bi,bj) = wedtau0
                wedtauy2(i,j,k,bi,bj) = wedtau0
                wthetaLev (i,j,k,bi,bj) = 0. _d 0
                wsaltLev  (i,j,k,bi,bj) = 0. _d 0
                wdiffkrFld(i,j,k,bi,bj) = wdiffkr0
                wkapgmFld (i,j,k,bi,bj) = wkapgm0
                wkaprediFld (i,j,k,bi,bj) = wkapredi0
                wedtauxFld(i,j,k,bi,bj) = wedtau0
                wedtauyFld(i,j,k,bi,bj) = wedtau0
#if (defined (ALLOW_UVEL0_COST_CONTRIBUTION) || defined (ALLOW_UVEL0_CONTROL))
#if (defined (ALLOW_VVEL0_COST_CONTRIBUTION) || defined (ALLOW_VVEL0_CONTROL))
                wuvel3d(i,j,k,bi,bj) = 0. _d 0
                wvvel3d(i,j,k,bi,bj) = 0. _d 0
#endif
#endif
              enddo
            enddo
          enddo
        enddo
      enddo
#endif /* ECCO_CTRL_DEPRECATED */

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))

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

#endif /* ALLOW_CTRL and ALLOW_OBCS */

#ifdef ECCO_CTRL_DEPRECATED
c--   Build area weighting matrix used in the cost function
c--   contributions.

c--   Define frame.
      do j = jmin,jmax
        do i = imin,imax
c--       North/South and West/East edges set to zero.
cph          if ( (j .lt. 1) .or. (j .gt. sNy) .or.
cph     &         (i .lt. 1) .or. (i .gt. sNx)      ) then
cph            frame(i,j) = 0. _d 0
cph          else
            frame(i,j) = 1. _d 0
cph          endif
        enddo
      enddo

c--   First account for the grid used.
      if (usingCartesianGrid) then
        factor = 0. _d 0
      else if (usingSphericalPolarGrid) then
        factor = 1. _d 0
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
cds              cosphi(i,j,bi,bj) = cos(yc(i,j,bi,bj)*deg2rad*factor)*
cds     &                            frame(i,j)
              cosphi(i,j,bi,bj) = frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif /* ECCO_CTRL_DEPRECATED */

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS)) \
      || (defined ECCO_CTRL_DEPRECATED)
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
#endif

#ifdef ECCO_CTRL_DEPRECATED
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

      do bj = jtlo,jthi
        do bi = itlo,ithi
c indices are inconsistent with ecco_cost.h declaration
c          wsfluxmm(bi,bj) = 1.
c          whfluxmm(bi,bj) = 1.

c--       The "classic" state estimation tool wastes memory here;
c--       as long as there is not more information available there
c--       is no need to add the zonal and meridional directions.
          do k = 1,nr
            wtheta(k,bi,bj)   = wti(k)
            wsalt (k,bi,bj)   = wsi(k)
            wcurrent(k,bi,bj) = wvi(k)
c--
            if (wtheta(k,bi,bj) .ne. 0.) then
              wtheta(k,bi,bj) = ratio/wtheta(k,bi,bj)/wtheta(k,bi,bj)
            else
              wtheta(k,bi,bj) = 0.0 _d 0
            endif
            if (wsalt(k,bi,bj) .ne. 0.) then
              wsalt(k,bi,bj) = ratio/wsalt(k,bi,bj)/wsalt(k,bi,bj)
            else
              wsalt(k,bi,bj) = 0.0 _d 0
            endif
          enddo
        enddo
      enddo
#endif /* ECCO_CTRL_DEPRECATED */

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
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
#endif /* ALLOW_CTRL and ALLOW_OBCS */

#ifdef ECCO_CTRL_DEPRECATED
#if (defined (ALLOW_SALT0_COST_CONTRIBUTION) || \
      defined (ALLOW_SALT0_CONTROL) || \
      defined (ALLOW_WSALTLEV))
      lwsaltLevInUse = .true.
      if ( salt0errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( salt0errfile, cost_iprec, Nr,
     &                      wsaltLev, 1, 1, myThid )
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
c--           Test for missing values.
              if ( wsaltLev(i,j,k,bi,bj).eq.0 ) then
                 wsaltLev(i,j,k,bi,bj) = 0. _d 0
              else
                 wsaltLev(i,j,k,bi,bj)=frame(i,j)*maskC(i,j,k,bi,bj)/
     $              ( wsaltLev(i,j,k,bi,bj)*wsaltLev(i,j,k,bi,bj) )
              endif
             enddo
            enddo
           enddo
          enddo
         enddo
      else
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
               wsaltLev(i,j,k,bi,bj)=
     $             wsalt(k,bi,bj)*frame(i,j)*maskC(i,j,k,bi,bj)
             enddo
            enddo
           enddo
          enddo
         enddo
      endif
      call active_write_xyz( 'wsaltLev', wsaltLev,
     &     1, 0, myThid, dummy)
      _EXCH_XYZ_RL( wsaltLev, myThid )
#endif

#if (defined (ALLOW_THETA0_COST_CONTRIBUTION) || \
     defined (ALLOW_THETA0_CONTROL) || \
     defined (ALLOW_WTHETALEV))
      lwthetaLevInUse = .true.
      if ( temp0errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( temp0errfile, cost_iprec, Nr,
     &                      wthetaLev, 1, 1, myThid )
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
c--           Test for missing values.
              if ( wthetaLev(i,j,k,bi,bj).eq.0 ) then
                 wthetaLev(i,j,k,bi,bj) = 0. _d 0
              else
                 wthetaLev(i,j,k,bi,bj)=frame(i,j)*maskC(i,j,k,bi,bj)/
     $              ( wthetaLev(i,j,k,bi,bj)*wthetaLev(i,j,k,bi,bj) )
              endif
             enddo
            enddo
           enddo
          enddo
         enddo
      else
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
               wthetaLev(i,j,k,bi,bj)=
     $             wtheta(k,bi,bj)*frame(i,j)*maskC(i,j,k,bi,bj)
             enddo
            enddo
           enddo
          enddo
         enddo
      endif
      call active_write_xyz( 'wthetaLev', wthetaLev,
     &     1, 0, myThid, dummy)
      _EXCH_XYZ_RL( wthetaLev, myThid )
#endif

#if (defined (ALLOW_ARGO_SALT_COST_CONTRIBUTION) || \
     defined (ALLOW_SSS_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDS_COST_CONTRIBUTION)|| \
     defined (ALLOW_CTDSCLIM_COST_CONTRIBUTION))

      lwsalt2InUse = .true.
      if ( salterrfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( salterrfile, cost_iprec, Nr,
     &                      wsalt2, 1, 1, myThid )

         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
c--           Test for missing values.
              if (wsalt(k,bi,bj).eq.0. .or.
     $             wsalt2(i,j,k,bi,bj).eq.0.) then
                 wsalt2(i,j,k,bi,bj) = 0. _d 0
              else
                 wsalt2(i,j,k,bi,bj)=frame(i,j)*maskC(i,j,k,bi,bj)/
     $              ( wsalt2(i,j,k,bi,bj)*wsalt2(i,j,k,bi,bj) )
              endif
             enddo
            enddo
           enddo
          enddo
         enddo
      else
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
                wsalt2(i,j,k,bi,bj)=
     $             wsalt(k,bi,bj)*frame(i,j)*maskC(i,j,k,bi,bj)
             enddo
            enddo
           enddo
          enddo
         enddo
      endif
      _EXCH_XYZ_RL( wsalt2, myThid )
#endif

#if (defined (ALLOW_ARGO_THETA_COST_CONTRIBUTION) || \
     defined (ALLOW_SST_COST_CONTRIBUTION) || \
     defined (ALLOW_TMI_SST_COST_CONTRIBUTION) || \
     defined (ALLOW_DAILYSST_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDTCLIM_COST_CONTRIBUTION) || \
     defined (ALLOW_XBT_COST_CONTRIBUTION))

      lwtheta2InUse = .true.
      if ( temperrfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( temperrfile, cost_iprec, Nr,
     &                      wtheta2, 1, 1, myThid )
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
c--           Test for missing values.
              if (wtheta(k,bi,bj).eq.0. .or.
     $            wtheta2(i,j,k,bi,bj).eq.0.) then
                 wtheta2(i,j,k,bi,bj) = 0. _d 0
              else
                 wtheta2(i,j,k,bi,bj)= frame(i,j)*maskC(i,j,k,bi,bj)/
     $                  ( wtheta2(i,j,k,bi,bj)*wtheta2(i,j,k,bi,bj) )
              endif
             enddo
            enddo
           enddo
          enddo
         enddo
      else
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
              if (wtheta(k,bi,bj).eq.0 ) then
                 wtheta2(i,j,k,bi,bj) = 0. _d 0
              else
                 wtheta2(i,j,k,bi,bj) =
     $              wtheta(k,bi,bj)*frame(i,j)*maskC(i,j,k,bi,bj)
              endif
             enddo
            enddo
           enddo
          enddo
         enddo
      endif
      _EXCH_XYZ_RL( wtheta2, myThid )
#endif

#if (defined (ALLOW_SST_COST_CONTRIBUTION) || defined (ALLOW_SST_CONTROL))
      if ( ( using_cost_sst ).AND.( ssterrfile .NE. ' ' ) )
     & CALL READ_REC_3D_RL( ssterrfile, cost_iprec, 1,
     &                      wsst, 1, 1, myThid )
#endif
#if (defined (ALLOW_SSS_COST_CONTRIBUTION) || defined (ALLOW_SSS_CONTROL))
      if ( ssserrfile .NE. ' ' )
     & CALL READ_REC_3D_RL( ssserrfile, cost_iprec, 1,
     &                      wsss, 1, 1, myThid )
#endif

      k = 1
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
#if (defined (ALLOW_SST_COST_CONTRIBUTION) || \
     defined (ALLOW_DAILYSST_COST_CONTRIBUTION) || \
     defined (ALLOW_SST_CONTROL))
              IF ( using_cost_sst ) THEN
              if ( ssterrfile .NE. ' ' ) then
cgf use specific weights for sst
                if (wsst(i,j,bi,bj).ne.0)
     &               wsst(i,j,bi,bj)= frame(i,j)*maskC(i,j,k,bi,bj)/
     &                  ( wsst(i,j,bi,bj)*wsst(i,j,bi,bj) )
              else
cgf use general hydrography weights
                if ( lwtheta2InUse ) then
                  wsst(i,j,bi,bj) = wtheta2(i,j,k,bi,bj)
                elseif ( lwthetaLevInUse ) then
                  wsst(i,j,bi,bj) = wthetaLev(i,j,k,bi,bj)
                else
                  wsst(i,j,bi,bj) =
     &               wtheta(k,bi,bj)*frame(i,j)*maskC(i,j,k,bi,bj)
                endif
              endif
              ENDIF ! IF ( using_cost_sst ) THEN
#endif

#if (defined (ALLOW_SSS_COST_CONTRIBUTION) || defined (ALLOW_SSS_CONTROL))
              if ( ssserrfile .NE. ' ' ) then
cgf use specific weights for sss
                if (wsss(i,j,bi,bj).ne.0)
     &               wsss(i,j,bi,bj)= frame(i,j)*maskC(i,j,k,bi,bj)/
     &                  ( wsss(i,j,bi,bj)*wsss(i,j,bi,bj) )

              else
cgf use general hydrography weights
                if ( lwsalt2InUse ) then
                  wsss(i,j,bi,bj) = wsalt2(i,j,k,bi,bj)
                elseif ( lwsaltLevInUse ) then
                  wsss(i,j,bi,bj) = wsaltLev(i,j,k,bi,bj)
                else
                  wsss(i,j,bi,bj) =
     &               wsalt(k,bi,bj)*frame(i,j)*maskC(i,j,k,bi,bj)
                endif
              endif
#endif
            enddo
          enddo
        enddo
      enddo
#if (defined (ALLOW_SST_COST_CONTRIBUTION) || \
     defined (ALLOW_DAILYSST_COST_CONTRIBUTION) || \
     defined (ALLOW_SST_CONTROL))
      IF (using_cost_sst)
     &    call active_write_xy_loc( 'wsst', wsst, 1, 0, myThid, dummy)
#endif
#if (defined (ALLOW_SSS_COST_CONTRIBUTION) || defined (ALLOW_SSS_CONTROL))
      call active_write_xy_loc( 'wsss', wsss, 1, 0, myThid, dummy)
#endif

      IF (using_cost_altim) THEN

#if (defined (ALLOW_SSH_MEAN_COST_CONTRIBUTION) || \
     defined (ALLOW_SSH_COST_CONTRIBUTION) )
#ifdef ALLOW_EGM96_ERROR_DIAG
c--   Read egm-96 geoid covariance. Data in units of meters.
      nnz   =  1
      irec  =  1
      if ( geoid_errfile .NE. ' ' ) then
      CALL READ_REC_3D_RL( geoid_errfile, cost_iprec, nnz,
     &                     wp, irec, 1, myThid )
c--   Set all tile edges to zero.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              wp(i,j,bi,bj) = wp(i,j,bi,bj)*frame(i,j)
cph-indonesian(
              if ( xC(i,j,bi,bj) .GT. 120. .AND.
     &             xC(i,j,bi,bj) .LT. 130. .AND.
     &             yC(i,j,bi,bj) .GT. -10. .AND.
     &             yC(i,j,bi,bj) .LT.  10. ) then
                 wp(i,j,bi,bj) = wp(i,j,bi,bj)*100.
              endif
cph-indonesian)
            enddo
          enddo
        enddo
      enddo
      endif
#else
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              wp(i,j,bi,bj) = frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif
#endif /* ALLOW_SSH_MEAN_COST_CONTRIBUTION */

#ifdef ALLOW_SSH_COST_CONTRIBUTION
c--   Read SSH anomaly rms field. Data in units of centimeters.
      nnz   =   1
      irec  =   1
      if ( ssh_errfile .NE. ' ' ) then
        CALL READ_REC_3D_RL( ssh_errfile, cost_iprec, nnz,
     &                       wtp, irec, 1, myThid )

         do bj = jtlo,jthi
          do bi = itlo,ithi
           k = 1
           do j = jmin,jmax
            do i = imin,imax
c--           Unit conversion to meters. ERS error is set to
c--           T/P error + 5cm
               if (maskC(i,j,k,bi,bj) .eq. 0.) then
                  wtp (i,j,bi,bj) = 0. _d 0
                  wers(i,j,bi,bj) = 0. _d 0
                  wgfo(i,j,bi,bj) = 0. _d 0
               else
                  wtp (i,j,bi,bj) = ( wtp(i,j,bi,bj) * 0.01 * 0.5 )
     &                 *frame(i,j)
                  wers(i,j,bi,bj) = ( wtp(i,j,bi,bj) + 0.05 )
     &                 *frame(i,j)
                  wgfo(i,j,bi,bj) = wers(i,j,bi,bj)
               endif
            enddo
           enddo
          enddo
         enddo
      endif

c-- overwrite T/P error field, if available:
      if ( tp_errfile .NE. ' ' )
     &  CALL READ_REC_3D_RL( tp_errfile, cost_iprec, nnz,
     &                       wtp, irec, 1, myThid )

c-- overwrite ERS error field, if available:
      if ( ers_errfile .NE. ' ' )
     &  CALL READ_REC_3D_RL( ers_errfile, cost_iprec, nnz,
     &                       wers, irec, 1, myThid )

c-- overwrite GFO error field, if available:
      if ( gfo_errfile .NE. ' ' )
     &  CALL READ_REC_3D_RL( gfo_errfile, cost_iprec, nnz,
     &                       wgfo, irec, 1, myThid )

         do bj = jtlo,jthi
          do bi = itlo,ithi
           k = 1
           do j = jmin,jmax
            do i = imin,imax
             if (maskC(i,j,k,bi,bj) .eq. 0.) then
              if ( tp_errfile .NE. ' ' )
     &               wtp (i,j,bi,bj) = 0. _d 0
              if ( ers_errfile .NE. ' ' )
     &             wers(i,j,bi,bj) = 0. _d 0
              if ( gfo_errfile .NE. ' ' )
     &             wgfo(i,j,bi,bj) = 0. _d 0
             else
c-- convert from cm to m and set to 0.1m for missing values.
              if ( tp_errfile .NE. ' ' ) then
                 wtp (i,j,bi,bj) = wtp (i,j,bi,bj) * 0.01 * frame(i,j)
cph should not be necessary for T/P and Jason
cph                 if ( wtp (i,j,bi,bj) .EQ. 0. )
cph     &                wtp (i,j,bi,bj) = 0.1 * frame(i,j)
              endif
              if ( ers_errfile .NE. ' ' ) then
                 wers(i,j,bi,bj) = wers(i,j,bi,bj) * 0.01 * frame(i,j)
                 if ( wers(i,j,bi,bj) .EQ. 0. )
     &                wers(i,j,bi,bj) = 0.1 * frame(i,j)
              endif
              if ( gfo_errfile .NE. ' ' ) then
                 wgfo(i,j,bi,bj) = wgfo(i,j,bi,bj) * 0.01 * frame(i,j)
                 if ( wgfo(i,j,bi,bj) .EQ. 0. )
     &                wgfo(i,j,bi,bj) = 0.1 * frame(i,j)
              endif
             endif
            enddo
           enddo
          enddo
         enddo

cph-indonesian(
      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
              if ( xC(i,j,bi,bj) .GT. 120. .AND.
     &             xC(i,j,bi,bj) .LT. 130. .AND.
     &             yC(i,j,bi,bj) .GT. -10. .AND.
     &             yC(i,j,bi,bj) .LT.  10. ) then
                 wtp(i,j,bi,bj)  = wtp(i,j,bi,bj)*100.
                 wers(i,j,bi,bj) = wers(i,j,bi,bj)*100.
                 wgfo(i,j,bi,bj) = wgfo(i,j,bi,bj)*100.
              endif
            enddo
          enddo
        enddo
      enddo
cph-indonesian)

#endif /* ALLOW_SSH_COST_CONTRIBUTION */

#ifdef ALLOW_SSHV4_COST

      do num_var=1,NSSHV4COST

      if ( sshv4cost_errfile(num_var) .NE. ' ' ) then
c--   read error standard deviation
      CALL READ_REC_3D_RL( sshv4cost_errfile(num_var), cost_iprec, 1,
     &                     wsshv4tmp, 1, 1, myThid )
c--   convert to units of meters
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
            wsshv4tmp(i,j,bi,bj)=wsshv4tmp(i,j,bi,bj)
     &           *sshv4cost_errfactor(num_var)
            enddo
          enddo
        enddo
      enddo
      else
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              wsshv4tmp(i,j,bi,bj)=0. _d 0
            enddo
          enddo
        enddo
      enddo
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
      if (wsshv4tmp(i,j,bi,bj).ne.0) then
        wsshv4tmp(i,j,bi,bj)= frame(i,j)*maskC(i,j,k,bi,bj)/
     &        ( wsshv4tmp(i,j,bi,bj)* wsshv4tmp(i,j,bi,bj) )
        wsshv4(i,j,num_var,bi,bj)=wsshv4tmp(i,j,bi,bj)
      endif
            enddo
          enddo
        enddo
      enddo

      call active_write_xy_loc( 'wsshv4', wsshv4tmp,
     &   num_var, 0, myThid, dummy)

      enddo

#endif
      ENDIF !IF (using_cost_altim) THEN

#ifdef ALLOW_BP_COST_CONTRIBUTION
      IF (using_cost_bp) THEN
      if ( bperrfile .NE. ' ' )
     &   CALL READ_REC_3D_RL( bperrfile, cost_iprec, 1,
     &                        wbp, 1, 1, myThid)

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
                if (wbp(i,j,bi,bj).ne.0)
     &               wbp(i,j,bi,bj)= frame(i,j)*maskC(i,j,k,bi,bj)/
     &                  ( wbp(i,j,bi,bj)* wbp(i,j,bi,bj) )
            enddo
          enddo
        enddo
      enddo

      call active_write_xy_loc( 'wbp', wbp, 1, 0, myThid, dummy)
      ENDIF ! IF (using_cost_bp) THEN
#endif

#ifdef ALLOW_IESTAU_COST_CONTRIBUTION
      if ( ieserrfile .NE. ' ' )
     &   CALL READ_REC_3D_RL( ieserrfile, cost_iprec, 1,
     &                        wies, 1, 1, myThid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
                if (wies(i,j,bi,bj).ne.0)
     &               wies(i,j,bi,bj)= frame(i,j)*maskC(i,j,k,bi,bj)/
     &                  ( wies(i,j,bi,bj)* wies(i,j,bi,bj) )
            enddo
          enddo
        enddo
      enddo

      call active_write_xy_loc( 'wies', wies, 1, 0, myThid, dummy)
#endif

c--   Read zonal wind stress variance.
#if (defined (ALLOW_SCAT_COST_CONTRIBUTION) || \
     defined (ALLOW_DAILYSCAT_COST_CONTRIBUTION) )
      IF (using_cost_scat) THEN

      nnz   =   1
      irec  =   1
      if ( scatx_errfile .NE. ' ' )
     &  CALL READ_REC_3D_RL( scatx_errfile, cost_iprec, nnz,
     &                       wscatx, irec, 1, myThid )
      if ( scaty_errfile .NE. ' ' )
     &  CALL READ_REC_3D_RL( scaty_errfile,cost_iprec, nnz,
     &                       wscaty, irec, 1, myThid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wscatx(i,j,bi,bj) .lt. -9900.) then
                wscatx(i,j,bi,bj) = 0. _d 0
              endif
              wscatx(i,j,bi,bj) = wscatx(i,j,bi,bj)
              wscatx(i,j,bi,bj) = max(wscatx(i,j,bi,bj),wtau0)
              wscatx(i,j,bi,bj) = wscatx(i,j,bi,bj)*maskw(i,j,k,bi,bj)
     &                            *frame(i,j)
              if (wscaty(i,j,bi,bj) .lt. -9900.) then
                wscaty(i,j,bi,bj) = 0. _d 0
              endif
              wscaty(i,j,bi,bj) = wscaty(i,j,bi,bj)
              wscaty(i,j,bi,bj) = max(wscaty(i,j,bi,bj),wtau0)
              wscaty(i,j,bi,bj) = wscaty(i,j,bi,bj)*masks(i,j,k,bi,bj)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo

      ENDIF ! IF (using_cost_scat) THEN
#endif

c--   Read zonal wind stress variance.
#if (defined (ALLOW_STRESS_MEAN_COST_CONTRIBUTION))
      nnz   =   1
      irec  =   1
cph      call mdsreadfield( tauum_errfile, cost_iprec, cost_yftype,
cph     &                   nnz, wtauum, irec, myThid )
cph      call mdsreadfield( tauvm_errfile, cost_iprec, cost_yftype,
cph     &                   nnz, wtauvm, irec, myThid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wtauum(i,j,bi,bj) .lt. -9900.) then
                wtauum(i,j,bi,bj) = 0. _d 0
              endif
              wtauum(i,j,bi,bj) = max(wtauum(i,j,bi,bj),wtau0m)
     &                            *frame(i,j)
c--           Test for missing values.
              if (wtauvm(i,j,bi,bj) .lt. -9900.) then
                wtauvm(i,j,bi,bj) = 0. _d 0
              endif
              wtauvm(i,j,bi,bj) = max(wtauvm(i,j,bi,bj),wtau0m)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_USTRESS_COST_CONTRIBUTION) || defined (ALLOW_USTRESS_CONTROL))
      nnz   =   1
ce      irec  =   2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( tauu_errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( tauu_errfile, cost_iprec, nnz,
     &                      wtauu, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wtauu(i,j,bi,bj) .lt. -9900.) then
                wtauu(i,j,bi,bj) = 0. _d 0
              endif
              wtauu(i,j,bi,bj) = max(wtauu(i,j,bi,bj),wtau0)
#ifndef ALLOW_ROTATE_UV_CONTROLS
              wtauu(i,j,bi,bj) = wtauu(i,j,bi,bj)*maskw(i,j,k,bi,bj)
     &                            *frame(i,j)
cph(
              wtauu2(i,j,bi,bj) = wtau0*maskW(i,j,k,bi,bj)*frame(i,j)
cph)
#else
              wtauu(i,j,bi,bj) = wtauu(i,j,bi,bj)*maskc(i,j,k,bi,bj)
     &                            *frame(i,j)
              wtauu2(i,j,bi,bj) = wtau0*maskc(i,j,k,bi,bj)*frame(i,j)
#endif
             enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_UWIND_COST_CONTRIBUTION) || defined (ALLOW_UWIND_CONTROL))
      nnz   =   1
ce      irec  =   2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( uwind_errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( uwind_errfile, cost_iprec, nnz,
     &                      wuwind, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wuwind(i,j,bi,bj) .lt. -9900.) then
                wuwind(i,j,bi,bj) = 0. _d 0
              endif
              wuwind(i,j,bi,bj) = wuwind(i,j,bi,bj)
              wuwind(i,j,bi,bj) = max(wuwind(i,j,bi,bj),wwind0)
              wuwind(i,j,bi,bj) = wuwind(i,j,bi,bj)*maskc(i,j,k,bi,bj)
     &                            *frame(i,j)
             enddo
          enddo
        enddo
      enddo
#endif

c--   Read meridional wind stress variance.
#if (defined (ALLOW_VSTRESS_COST_CONTRIBUTION) || defined (ALLOW_VSTRESS_CONTROL))
      nnz   =   1
ce      irec  =   2
ce(   due to Patrick processing:
      irec  = 1
ce)

      if ( tauv_errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( tauv_errfile, cost_iprec, nnz,
     &                      wtauv, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wtauv(i,j,bi,bj) .lt. -9900.) then
                wtauv(i,j,bi,bj) = 0. _d 0
              endif
              wtauv(i,j,bi,bj) = max(wtauv(i,j,bi,bj),wtau0)
#ifndef ALLOW_ROTATE_UV_CONTROLS
              wtauv(i,j,bi,bj) = wtauv(i,j,bi,bj)*masks(i,j,k,bi,bj)
     &                            *frame(i,j)
cph(
              wtauv2(i,j,bi,bj) = wtau0*maskS(i,j,k,bi,bj)*frame(i,j)
cph)
#else
              wtauv(i,j,bi,bj) = wtauv(i,j,bi,bj)*maskc(i,j,k,bi,bj)
     &                            *frame(i,j)
              wtauv2(i,j,bi,bj) = wtau0*maskc(i,j,k,bi,bj)*frame(i,j)
#endif
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_VWIND_COST_CONTRIBUTION) || defined (ALLOW_VWIND_CONTROL))
      nnz   =   1
ce      irec  =   2
ce(   due to Patrick processing:
      irec  = 1
ce)

      if ( vwind_errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( vwind_errfile, cost_iprec, nnz,
     &                      wvwind, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wvwind(i,j,bi,bj) .lt. -9900.) then
                wvwind(i,j,bi,bj) = 0. _d 0
              endif
              wvwind(i,j,bi,bj) = wvwind(i,j,bi,bj)
              wvwind(i,j,bi,bj) = max(wvwind(i,j,bi,bj),wwind0)
              wvwind(i,j,bi,bj) = wvwind(i,j,bi,bj)*maskc(i,j,k,bi,bj)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_HFLUX_COST_CONTRIBUTION) || defined (ALLOW_HFLUX_CONTROL))
c--   Read heat flux flux variance.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( hflux_errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( hflux_errfile, cost_iprec, nnz,
     &                      whflux, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (whflux(i,j,bi,bj) .lt. -9900.) then
                whflux(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of W/m**2.
              whflux(i,j,bi,bj) = whflux(i,j,bi,bj)/3.
              whflux(i,j,bi,bj) = max(whflux(i,j,bi,bj),whflux0)
     &                            *frame(i,j)
              whfluxm(i,j,bi,bj) = max(whfluxm(i,j,bi,bj),whflux0m)
     &                            *frame(i,j)
cph(
              whflux2(i,j,bi,bj) = whflux0*frame(i,j)
cph)
            enddo
          enddo
        enddo
      enddo
#elif (defined (ALLOW_ATEMP_COST_CONTRIBUTION) || defined (ALLOW_ATEMP_CONTROL))
c--   Read atmos. temp. variance.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( atemp_errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( atemp_errfile, cost_iprec, nnz,
     &                      watemp, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (watemp(i,j,bi,bj) .lt. -9900.) then
                watemp(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of W/m**2?? should be in degC or degK
              watemp(i,j,bi,bj) = watemp(i,j,bi,bj)
              watemp(i,j,bi,bj) = max(watemp(i,j,bi,bj),watemp0)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo

#endif

#if (defined (ALLOW_SFLUX_COST_CONTRIBUTION) || defined (ALLOW_SFLUX_CONTROL))
c--   Read salt flux variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( sflux_errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( sflux_errfile, cost_iprec, nnz,
     &                      wsflux, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wsflux(i,j,bi,bj) .lt. -9900.) then
                wsflux(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              wsflux(i,j,bi,bj) = wsflux(i,j,bi,bj) / 3.
              wsflux(i,j,bi,bj) = max(wsflux(i,j,bi,bj),wsflux0)
     &                            *frame(i,j)
              wsfluxm(i,j,bi,bj) = max(wsfluxm(i,j,bi,bj),wsflux0m)
     &                            *frame(i,j)
cph(
              wsflux2(i,j,bi,bj) = wsflux0*frame(i,j)
cph)
            enddo
          enddo
        enddo
      enddo
#elif (defined (ALLOW_AQH_COST_CONTRIBUTION) || defined (ALLOW_AQH_CONTROL))
c--   Secific humid. variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( aqh_errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( aqh_errfile, cost_iprec, nnz,
     &                      waqh, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (waqh(i,j,bi,bj) .lt. -9900.) then
                waqh(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              waqh(i,j,bi,bj) = waqh(i,j,bi,bj)
              waqh(i,j,bi,bj) = max(waqh(i,j,bi,bj),waqh0)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_PRECIP_COST_CONTRIBUTION) || defined (ALLOW_PRECIP_CONTROL))
c--   Atmos. precipitation variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( precip_errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( precip_errfile, cost_iprec, nnz,
     &                      wprecip, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wprecip(i,j,bi,bj) .lt. -9900.) then
                wprecip(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              wprecip(i,j,bi,bj) = wprecip(i,j,bi,bj)
              wprecip(i,j,bi,bj) = max(wprecip(i,j,bi,bj),wprecip0)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_SWFLUX_COST_CONTRIBUTION) || defined (ALLOW_SWFLUX_CONTROL))
c--   Atmos. swfluxitation variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( swflux_errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( swflux_errfile, cost_iprec, nnz,
     &                      wswflux, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wswflux(i,j,bi,bj) .lt. -9900.) then
                wswflux(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              wswflux(i,j,bi,bj) = wswflux(i,j,bi,bj)
              wswflux(i,j,bi,bj) = max(wswflux(i,j,bi,bj),wswflux0)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_SWDOWN_COST_CONTRIBUTION) || defined (ALLOW_SWDOWN_CONTROL))
c--   Atmos. swdownitation variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( swdown_errfile .NE. ' ' ) then
       CALL READ_REC_3D_RL( swdown_errfile, cost_iprec, nnz,
     &                      wswdown, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wswdown(i,j,bi,bj) .lt. -9900.) then
                wswdown(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              wswdown(i,j,bi,bj) = wswdown(i,j,bi,bj)
              wswdown(i,j,bi,bj) = max(wswdown(i,j,bi,bj),wswdown0)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_LWFLUX_COST_CONTRIBUTION) || defined (ALLOW_LWFLUX_CONTROL))
c--   Atmos. lwfluxitation variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( lwflux_errfile .NE. ' ' ) then
        CALL READ_REC_3D_RL( lwflux_errfile, cost_iprec, nnz,
     &                       wlwflux, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wlwflux(i,j,bi,bj) .lt. -9900.) then
                wlwflux(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              wlwflux(i,j,bi,bj) = wlwflux(i,j,bi,bj)
              wlwflux(i,j,bi,bj) = max(wlwflux(i,j,bi,bj),wlwflux0)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_LWDOWN_COST_CONTRIBUTION) || defined (ALLOW_LWDOWN_CONTROL))
c--   Atmos. lwdownitation variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( lwdown_errfile .NE. ' ' ) then
        CALL READ_REC_3D_RL( lwdown_errfile, cost_iprec, nnz,
     &                       wlwdown ,irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wlwdown(i,j,bi,bj) .lt. -9900.) then
                wlwdown(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              wlwdown(i,j,bi,bj) = wlwdown(i,j,bi,bj)
              wlwdown(i,j,bi,bj) = max(wlwdown(i,j,bi,bj),wlwdown0)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_SNOWPRECIP_COST_CONTRIBUTION) || defined (ALLOW_SNOWPRECIP_CONTROL))
c--   Atmos. snowprecipitation variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( snowprecip_errfile .NE. ' ' ) then
        CALL READ_REC_3D_RL( snowprecip_errfile, cost_iprec, nnz,
     &                       wsnowprecip, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wsnowprecip(i,j,bi,bj) .lt. -9900.) then
                wsnowprecip(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              wsnowprecip(i,j,bi,bj) = wsnowprecip(i,j,bi,bj)
              wsnowprecip(i,j,bi,bj) =
     &            max(wsnowprecip(i,j,bi,bj),wsnowprecip0)
     &            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_EVAP_COST_CONTRIBUTION) || defined (ALLOW_EVAP_CONTROL))
c--   Atmos. evapitation variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( evap_errfile .NE. ' ' ) then
        CALL READ_REC_3D_RL( evap_errfile, cost_iprec, nnz,
     &                       wevap, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wevap(i,j,bi,bj) .lt. -9900.) then
                wevap(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              wevap(i,j,bi,bj) = wevap(i,j,bi,bj)
              wevap(i,j,bi,bj) = max(wevap(i,j,bi,bj),wevap0)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_APRESSURE_COST_CONTRIBUTION) || defined (ALLOW_APRESSURE_CONTROL))
c--   Atmos. apressureitation variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( apressure_errfile .NE. ' ' ) then
        CALL READ_REC_3D_RL( apressure_errfile, cost_iprec, nnz,
     &                       wapressure, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wapressure(i,j,bi,bj) .lt. -9900.) then
                wapressure(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              wapressure(i,j,bi,bj) = wapressure(i,j,bi,bj)
              wapressure(i,j,bi,bj) =
     &            max(wapressure(i,j,bi,bj),wapressure0)
     &            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_RUNOFF_COST_CONTRIBUTION) || defined (ALLOW_RUNOFF_CONTROL))
c--   Atmos. runoffitation variance. Second read: data in units of m/s.
      nnz   =  1
c--   First  record in data file:  mean field.
c--   Second record in data file:  rms  field.
ce      irec  =  2
ce(   due to Patrick processing:
      irec  = 1
ce)
      if ( runoff_errfile .NE. ' ' ) then
        CALL READ_REC_3D_RL( runoff_errfile, cost_iprec, nnz,
     &                       wrunoff, irec, 1, myThid )
      endif

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wrunoff(i,j,bi,bj) .lt. -9900.) then
                wrunoff(i,j,bi,bj) = 0. _d 0
              endif
c--           Data are in units of m/s.
              wrunoff(i,j,bi,bj) = wrunoff(i,j,bi,bj)
              wrunoff(i,j,bi,bj) = max(wrunoff(i,j,bi,bj),wrunoff0)
     &                            *frame(i,j)
            enddo
          enddo
        enddo
      enddo
#endif

#if (defined (ALLOW_BOTTOMDRAG_COST_CONTRIBUTION) || defined (ALLOW_BOTTOMDRAG_CONTROL))
      if ( bottomdrag_errfile .NE. ' ' ) then

        CALL READ_REC_3D_RL( bottomdrag_errfile, cost_iprec, nnz,
     &                       wbottomdrag, irec, 1, myThid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wbottomdrag(i,j,bi,bj) .lt. -9900.) then
                wbottomdrag(i,j,bi,bj) = 0. _d 0
              endif
            enddo
          enddo
        enddo
      enddo

      endif
#endif

#if (defined (ALLOW_DIFFKR_COST_CONTRIBUTION) || defined (ALLOW_DIFFKR_CONTROL))
      if ( diffkr_errfile .NE. ' ' ) then

        CALL READ_REC_3D_RL( diffkr_errfile, cost_iprec, Nr,
     &                       wdiffkr2, 1, 1, myThid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
           do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wdiffkr2(i,j,k,bi,bj) .lt. -9900.) then
                wdiffkr2(i,j,k,bi,bj) = 0. _d 0
              endif
            enddo
           enddo
          enddo
        enddo
      enddo

      endif
#endif

#if (defined (ALLOW_KAPGM_COST_CONTRIBUTION) || defined (ALLOW_KAPGM_CONTROL))
      if ( kapgm_errfile .NE. ' ' ) then

        CALL READ_REC_3D_RL( kapgm_errfile, cost_iprec, Nr,
     &                       wkapgm2, 1, 1, myThid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
           do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wkapgm2(i,j,k,bi,bj) .lt. -9900.) then
                wkapgm2(i,j,k,bi,bj) = 0. _d 0
              endif
            enddo
           enddo
          enddo
        enddo
      enddo

      endif
#endif

#if (defined (ALLOW_KAPREDI_COST_CONTRIBUTION) || defined (ALLOW_KAPREDI_CONTROL))
      if ( kapredi_errfile .NE. ' ' ) then

        CALL READ_REC_3D_RL( kapredi_errfile, cost_iprec, Nr,
     &                       wkapredi2, 1, 1, myThid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
           do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wkapredi2(i,j,k,bi,bj) .lt. -9900.) then
                wkapredi2(i,j,k,bi,bj) = 0. _d 0
              endif
            enddo
           enddo
          enddo
        enddo
      enddo

      endif
#endif

#if ( defined (ALLOW_EDDYPSI_COST_CONTRIBUTION) || defined (ALLOW_EDDYPSI_CONTROL) )
      if ( edtau_errfile .NE. ' ' ) then

        CALL READ_REC_3D_RL( edtau_errfile, cost_iprec, Nr,
     &                       wedtaux2, 1 ,1, myThid )

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
           do j = jmin,jmax
            do i = imin,imax
c--           Test for missing values.
              if (wedtaux2(i,j,k,bi,bj) .lt. -9900.) then
                wedtaux2(i,j,k,bi,bj) = 0. _d 0
              endif
              wedtauy2(i,j,k,bi,bj)=wedtaux2(i,j,k,bi,bj)
            enddo
           enddo
          enddo
        enddo
      enddo

      endif
#endif

#if (defined (ALLOW_ETAN0_COST_CONTRIBUTION) || defined (ALLOW_ETAN0_CONTROL))
      if ( etan0errfile .NE. ' ' ) then
         CALL READ_REC_3D_RL( etan0errfile, cost_iprec, 1,
     &                        wetan, 1, 1, myThid )
         do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
             do i = imin,imax
c--           Test for missing values.
              if ( wetan(i,j,bi,bj).eq.0 ) then
                 wetan(i,j,bi,bj) = 0. _d 0
              else
                 wetan(i,j,bi,bj)=frame(i,j)*maskC(i,j,1,bi,bj)/
     $              ( wetan(i,j,bi,bj)*wetan(i,j,bi,bj) )
              endif
             enddo
            enddo
          enddo
         enddo
!      else
!         do bj = jtlo,jthi
!          do bi = itlo,ithi
!            do j = jmin,jmax
!             do i = imin,imax
!               wetan(i,j,bi,bj)=
!     $             wetan(i,j,bi,bj)*frame(i,j)*maskC(i,j,1,bi,bj)
!             enddo
!            enddo
!          enddo
!         enddo
      endif
      call active_write_xy( 'wetan', wetan,
     &     1, 0, myThid, dummy)
      _EXCH_XY_RL( wetan, myThid )
#endif

#if (defined (ALLOW_UVEL0_COST_CONTRIBUTION) || defined (ALLOW_UVEL0_CONTROL))
#if (defined (ALLOW_VVEL0_COST_CONTRIBUTION) || defined (ALLOW_VVEL0_CONTROL))

      if ( uvel0errfile .NE. ' ' .AND. vvel0errfile .NE. ' ' ) then
         CALL READ_REC_3D_RL( uvel0errfile, cost_iprec, Nr,
     &                        wuvel3d, 1, 1, myThid )
         CALL READ_REC_3D_RL( vvel0errfile, cost_iprec, Nr,
     &                        wvvel3d, 1, 1, myThid )
         do bj = jtlo,jthi
          do bi = itlo,ithi
           do k = 1,nr
            do j = jmin,jmax
             do i = imin,imax
c--           Test for missing values.
              if ( wuvel3d(i,j,k,bi,bj).eq.0 ) then
                 wuvel3d(i,j,k,bi,bj) = 0. _d 0
              else
                 wuvel3d(i,j,k,bi,bj)=frame(i,j)*maskW(i,j,k,bi,bj)/
     $              ( wuvel3d(i,j,k,bi,bj)*wuvel3d(i,j,k,bi,bj) )
              endif
              if ( wvvel3d(i,j,k,bi,bj).eq.0 ) then
                 wvvel3d(i,j,k,bi,bj) = 0. _d 0
              else
                 wvvel3d(i,j,k,bi,bj)=frame(i,j)*maskS(i,j,k,bi,bj)/
     $              ( wvvel3d(i,j,k,bi,bj)*wvvel3d(i,j,k,bi,bj) )
              endif
             enddo
            enddo
           enddo
          enddo
         enddo
      endif
      call active_write_xyz( 'wuvel', wuvel3d,
     &     1, 0, myThid, dummy)
      call active_write_xyz( 'wvvel', wvvel3d,
     &     1, 0, myThid, dummy)
      _EXCH_XYZ_RL( wuvel3d, myThid )
      _EXCH_XYZ_RL( wvvel3d, myThid )
#endif
#endif

c--   Units have to be checked!
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              if (wtp(i,j,bi,bj) .ne. 0.) then
                wtp (i,j,bi,bj) = 1./wtp(i,j,bi,bj)/wtp(i,j,bi,bj)
              endif
              if (wers(i,j,bi,bj) .ne. 0.) then
                wers(i,j,bi,bj) = 1./wers(i,j,bi,bj)/wers(i,j,bi,bj)
              endif
              if (wgfo(i,j,bi,bj) .ne. 0.) then
                wgfo(i,j,bi,bj) = 1./wgfo(i,j,bi,bj)/wgfo(i,j,bi,bj)
              endif
              if (wp(i,j,bi,bj) .ne. 0.) then
                wp(i,j,bi,bj) = 1./wp(i,j,bi,bj)/wp(i,j,bi,bj)
              endif
              if (wtauu(i,j,bi,bj) .ne. 0.) then
                wtauu(i,j,bi,bj) = 1./wtauu(i,j,bi,bj)/wtauu(i,j,bi,bj)
              else
                wtauu(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wtauum(i,j,bi,bj) .ne. 0.) then
                wtauum(i,j,bi,bj) =
     &            1./wtauum(i,j,bi,bj)/wtauum(i,j,bi,bj)
              else
                wtauum(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wscatx(i,j,bi,bj) .ne. 0.) then
                wscatx(i,j,bi,bj) =
     &            1./wscatx(i,j,bi,bj)/wscatx(i,j,bi,bj)
              else
                wscatx(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wtauv(i,j,bi,bj) .ne. 0.) then
                wtauv(i,j,bi,bj) = 1./wtauv(i,j,bi,bj)/wtauv(i,j,bi,bj)
              else
                wtauv(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wtauvm(i,j,bi,bj) .ne. 0.) then
                wtauvm(i,j,bi,bj) =
     &           1./wtauvm(i,j,bi,bj)/wtauvm(i,j,bi,bj)
              else
                wtauvm(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wscaty(i,j,bi,bj) .ne. 0.) then
                wscaty(i,j,bi,bj) =
     &           1./wscaty(i,j,bi,bj)/wscaty(i,j,bi,bj)
              else
                wscaty(i,j,bi,bj) = 0.0 _d 0
              endif
              if (whflux(i,j,bi,bj) .ne. 0.) then
                whflux(i,j,bi,bj) =
     &                1./whflux(i,j,bi,bj)/whflux(i,j,bi,bj)
              else
                whflux(i,j,bi,bj) = 0.0 _d 0
              endif
              if (whfluxm(i,j,bi,bj) .ne. 0.) then
                whfluxm(i,j,bi,bj) =
     &                1./whfluxm(i,j,bi,bj)/whfluxm(i,j,bi,bj)
              else
                whfluxm(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wsflux(i,j,bi,bj) .ne. 0.) then
                wsflux(i,j,bi,bj) =
     &                1./wsflux(i,j,bi,bj)/wsflux(i,j,bi,bj)
              else
                wsflux(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wsfluxm(i,j,bi,bj) .ne. 0.) then
                wsfluxm(i,j,bi,bj) =
     &                1./wsfluxm(i,j,bi,bj)/wsfluxm(i,j,bi,bj)
              else
                wsfluxm(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wuwind(i,j,bi,bj) .ne. 0.) then
                wuwind(i,j,bi,bj) =
     &                1./wuwind(i,j,bi,bj)/wuwind(i,j,bi,bj)
              else
                wuwind(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wvwind(i,j,bi,bj) .ne. 0.) then
                wvwind(i,j,bi,bj) =
     &                1./wvwind(i,j,bi,bj)/wvwind(i,j,bi,bj)
              else
                wvwind(i,j,bi,bj) = 0.0 _d 0
              endif
              if (watemp(i,j,bi,bj) .ne. 0.) then
                watemp(i,j,bi,bj) =
     &                1./watemp(i,j,bi,bj)/watemp(i,j,bi,bj)
              else
                watemp(i,j,bi,bj) = 0.0 _d 0
              endif
              if (waqh(i,j,bi,bj) .ne. 0.) then
                waqh(i,j,bi,bj) =
     &                1./waqh(i,j,bi,bj)/waqh(i,j,bi,bj)
              else
                waqh(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wprecip(i,j,bi,bj) .ne. 0.) then
                wprecip(i,j,bi,bj) =
     &                1./wprecip(i,j,bi,bj)/wprecip(i,j,bi,bj)
              else
                wprecip(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wswflux(i,j,bi,bj) .ne. 0.) then
                wswflux(i,j,bi,bj) =
     &                1./wswflux(i,j,bi,bj)/wswflux(i,j,bi,bj)
              else
                wswflux(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wswdown(i,j,bi,bj) .ne. 0.) then
                wswdown(i,j,bi,bj) =
     &                1./wswdown(i,j,bi,bj)/wswdown(i,j,bi,bj)
              else
                wswdown(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wlwflux(i,j,bi,bj) .ne. 0.) then
                wlwflux(i,j,bi,bj) =
     &                1./wlwflux(i,j,bi,bj)/wlwflux(i,j,bi,bj)
              else
                wlwflux(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wlwdown(i,j,bi,bj) .ne. 0.) then
                wlwdown(i,j,bi,bj) =
     &                1./wlwdown(i,j,bi,bj)/wlwdown(i,j,bi,bj)
              else
                wlwdown(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wevap(i,j,bi,bj) .ne. 0.) then
                wevap(i,j,bi,bj) =
     &                1./wevap(i,j,bi,bj)/wevap(i,j,bi,bj)
              else
                wevap(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wsnowprecip(i,j,bi,bj) .ne. 0.) then
                wsnowprecip(i,j,bi,bj) =
     &                1./wsnowprecip(i,j,bi,bj)/wsnowprecip(i,j,bi,bj)
              else
                wsnowprecip(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wapressure(i,j,bi,bj) .ne. 0.) then
                wapressure(i,j,bi,bj) =
     &                1./wapressure(i,j,bi,bj)/wapressure(i,j,bi,bj)
              else
                wapressure(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wrunoff(i,j,bi,bj) .ne. 0.) then
                wrunoff(i,j,bi,bj) =
     &                1./wrunoff(i,j,bi,bj)/wrunoff(i,j,bi,bj)
              else
                wrunoff(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wbottomdrag(i,j,bi,bj) .ne. 0.) then
                wbottomdrag(i,j,bi,bj) =
     &                1./wbottomdrag(i,j,bi,bj)/wbottomdrag(i,j,bi,bj)
              else
                wbottomdrag(i,j,bi,bj) = 0.0 _d 0
              endif

c the following makes no sense inside i,j loop
c              if (wsfluxmm(bi,bj).ne.0.)
c     &             wsfluxmm(bi,bj) = 1./wsfluxmm(bi,bj)*wsfluxmm(bi,bj)
c              if (whfluxmm(bi,bj).ne.0.)
c     &             whfluxmm(bi,bj) = 1./whfluxmm(bi,bj)*whfluxmm(bi,bj)

cph(
              if (whflux2(i,j,bi,bj) .ne. 0.) then
                 whflux2(i,j,bi,bj) =
     &                1./whflux2(i,j,bi,bj)/whflux2(i,j,bi,bj)
              else
                 whflux2(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wsflux2(i,j,bi,bj) .ne. 0.) then
                 wsflux2(i,j,bi,bj) =
     &                1./wsflux2(i,j,bi,bj)/wsflux2(i,j,bi,bj)
              else
                 wsflux2(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wtauu2(i,j,bi,bj) .ne. 0.) then
                 wtauu2(i,j,bi,bj) =
     &                1./wtauu2(i,j,bi,bj)/wtauu2(i,j,bi,bj)
              else
                 wtauu2(i,j,bi,bj) = 0.0 _d 0
              endif
              if (wtauv2(i,j,bi,bj) .ne. 0.) then
                 wtauv2(i,j,bi,bj) =
     &                1./wtauv2(i,j,bi,bj)/wtauv2(i,j,bi,bj)
              else
                 wtauv2(i,j,bi,bj) = 0.0 _d 0
              endif
cph)
            enddo
          enddo

        enddo
      enddo

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            if (wdiffkr(k,bi,bj) .ne. 0.) then
              wdiffkr(k,bi,bj) = 1./wdiffkr(k,bi,bj)/wdiffkr(k,bi,bj)
            else
              wdiffkr(k,bi,bj) = 0.0 _d 0
            endif
            if (wkapgm(k,bi,bj) .ne. 0.) then
              wkapgm(k,bi,bj) = 1./wkapgm(k,bi,bj)/wkapgm(k,bi,bj)
            else
              wkapgm(k,bi,bj) = 0.0 _d 0
            endif
            if (wkapredi(k,bi,bj) .ne. 0.) then
              wkapredi(k,bi,bj) = 1./wkapredi(k,bi,bj)/wkapredi(k,bi,bj)
            else
              wkapredi(k,bi,bj) = 0.0 _d 0
            endif
            if (wedtaux(k,bi,bj) .ne. 0.) then
              wedtaux(k,bi,bj) = 1./wedtaux(k,bi,bj)/wedtaux(k,bi,bj)
            else
              wedtaux(k,bi,bj) = 0.0 _d 0
            endif
            if (wedtauy(k,bi,bj) .ne. 0.) then
              wedtauy(k,bi,bj) = 1./wedtauy(k,bi,bj)/wedtauy(k,bi,bj)
            else
              wedtauy(k,bi,bj) = 0.0 _d 0
            endif
            do j = jmin,jmax
              do i = imin,imax
                if (wdiffkr2(i,j,k,bi,bj) .ne. 0.) then
                   wdiffkr2(i,j,k,bi,bj) = frame(i,j)/
     &                  wdiffkr2(i,j,k,bi,bj)/wdiffkr2(i,j,k,bi,bj)
                else
                   wdiffkr2(i,j,k,bi,bj) = 0.0 _d 0
                endif
                wdiffkrFld(i,j,k,bi,bj) = wdiffkr2(i,j,k,bi,bj)
c
                if (wkapgm2(i,j,k,bi,bj) .ne. 0.) then
                   wkapgm2(i,j,k,bi,bj) = frame(i,j)/
     &                  wkapgm2(i,j,k,bi,bj)/wkapgm2(i,j,k,bi,bj)
                else
                   wkapgm2(i,j,k,bi,bj) = 0.0 _d 0
                endif
                wkapgmFld(i,j,k,bi,bj) = wkapgm2(i,j,k,bi,bj)
c
                if (wkapredi2(i,j,k,bi,bj) .ne. 0.) then
                   wkapredi2(i,j,k,bi,bj) = frame(i,j)/
     &                  wkapredi2(i,j,k,bi,bj)/wkapredi2(i,j,k,bi,bj)
                else
                   wkapredi2(i,j,k,bi,bj) = 0.0 _d 0
                endif
                wkaprediFld(i,j,k,bi,bj) = wkapredi2(i,j,k,bi,bj)
c
                if (wedtaux2(i,j,k,bi,bj) .ne. 0.) then
                   wedtaux2(i,j,k,bi,bj) = frame(i,j)/
     &                  wedtaux2(i,j,k,bi,bj)/wedtaux2(i,j,k,bi,bj)
                else
                   wedtaux2(i,j,k,bi,bj) = 0.0 _d 0
                endif
                wedtauxFld(i,j,k,bi,bj) = wedtaux2(i,j,k,bi,bj)
c
                if (wedtauy2(i,j,k,bi,bj) .ne. 0.) then
                   wedtauy2(i,j,k,bi,bj) = frame(i,j)/
     &                  wedtauy2(i,j,k,bi,bj)/wedtauy2(i,j,k,bi,bj)
                else
                   wedtauy2(i,j,k,bi,bj) = 0.0 _d 0
                endif
                wedtauyFld(i,j,k,bi,bj) = wedtauy2(i,j,k,bi,bj)
              enddo
            enddo
          enddo
        enddo
      enddo

c
c     write masks and weights to files to be read by a master process
c
c#ifdef REAL4_IS_SLOW
C     leave this commented out (in case of problems with ACTIVE_WRITE_GEN_RS)
c     call active_write_xyz_loc( 'maskCtrlC', maskC,
c    &     1, 0, myThid, dummy)
c     call active_write_xyz_loc( 'maskCtrlW', maskW,
c    &     1, 0, myThid, dummy)
c     call active_write_xyz_loc( 'maskCtrlS', maskS,
c    &     1, 0, myThid, dummy)
c#else
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlC', maskC, 'XY', Nr,
     I                          1, .TRUE., 0, myThid, dummyRS )
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlW', maskW, 'XY', Nr,
     I                          1, .TRUE., 0, myThid, dummyRS )
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlS', maskS, 'XY', Nr,
     I                          1, .TRUE., 0, myThid, dummyRS )
c#endif

#if   (defined (ALLOW_HFLUX_COST_CONTRIBUTION) || defined (ALLOW_HFLUX_CONTROL))
      call active_write_xy_loc( 'whflux', whflux, 1, 0, myThid, dummy)
      call active_write_xy_loc( 'whflux2', whflux2, 1, 0, myThid, dummy)
#elif (defined (ALLOW_ATEMP_COST_CONTRIBUTION) || defined (ALLOW_ATEMP_CONTROL))
      call active_write_xy_loc( 'watemp', watemp, 1, 0, myThid, dummy)
#endif

#if   (defined (ALLOW_SFLUX_COST_CONTRIBUTION) || defined (ALLOW_SFLUX_CONTROL))
      call active_write_xy_loc( 'wsflux', wsflux, 1, 0, myThid, dummy)
      call active_write_xy_loc( 'wsflux2', wsflux2, 1, 0, myThid, dummy)
#elif (defined (ALLOW_AQH_COST_CONTRIBUTION) || defined (ALLOW_AQH_CONTROL))
      call active_write_xy_loc( 'waqh', waqh, 1, 0, myThid, dummy)
#endif

#if (defined (ALLOW_PRECIP_COST_CONTRIBUTION) || defined (ALLOW_PRECIP_CONTROL))
      call active_write_xy_loc( 'wprecip', wprecip, 1, 0, myThid, dummy)
#endif

#if (defined (ALLOW_SWFLUX_COST_CONTRIBUTION) || defined (ALLOW_SWFLUX_CONTROL))
      call active_write_xy_loc( 'wswflux', wswflux, 1, 0, myThid, dummy)
#endif

#if (defined (ALLOW_SWDOWN_COST_CONTRIBUTION) || defined (ALLOW_SWDOWN_CONTROL))
      call active_write_xy_loc( 'wswdown', wswdown, 1, 0, myThid, dummy)
#endif

#if (defined (ALLOW_LWFLUX_COST_CONTRIBUTION) || defined (ALLOW_LWFLUX_CONTROL))
      call active_write_xy_loc( 'wlwflux', wlwflux, 1, 0, myThid, dummy)
#endif

#if (defined (ALLOW_LWDOWN_COST_CONTRIBUTION) || defined (ALLOW_LWDOWN_CONTROL))
      call active_write_xy_loc( 'wlwdown', wlwdown, 1, 0, myThid, dummy)
#endif

#if (defined (ALLOW_EVAP_COST_CONTRIBUTION) || defined (ALLOW_EVAP_CONTROL))
      call active_write_xy_loc( 'wevap', wevap, 1, 0, myThid, dummy)
#endif

#if (defined (ALLOW_SNOWPRECIP_COST_CONTRIBUTION) || defined (ALLOW_SNOWPRECIP_CONTROL))
      call active_write_xy_loc( 'wsnowprecip', wsnowprecip,
     &    1, 0, myThid, dummy)
#endif

#if (defined (ALLOW_APRESSURE_COST_CONTRIBUTION) || defined (ALLOW_APRESSURE_CONTROL))
      call active_write_xy_loc( 'wapressure', wapressure,
     &    1, 0, myThid, dummy)
#endif

#if (defined (ALLOW_RUNOFF_COST_CONTRIBUTION) || defined (ALLOW_RUNOFF_CONTROL))
      call active_write_xy_loc( 'wrunoff', wrunoff, 1, 0, myThid, dummy)
#endif

#if   (defined (ALLOW_USTRESS_COST_CONTRIBUTION) || defined (ALLOW_USTRESS_CONTROL))
      call active_write_xy_loc( 'wtauu', wtauu,   1, 0, myThid, dummy)
      call active_write_xy_loc( 'wtauu2', wtauu2,   1, 0, myThid, dummy)
#endif

#if   (defined (ALLOW_UWIND_COST_CONTRIBUTION) || defined (ALLOW_UWIND_CONTROL))
      call active_write_xy_loc( 'wuwind', wuwind, 1, 0, myThid, dummy)
#endif

#if   (defined (ALLOW_VSTRESS_COST_CONTRIBUTION) || defined (ALLOW_VSTRESS_CONTROL))
      call active_write_xy_loc( 'wtauv', wtauv,   1, 0, myThid, dummy)
      call active_write_xy_loc( 'wtauv2', wtauv2,   1, 0, myThid, dummy)
#endif

#if   (defined (ALLOW_VWIND_COST_CONTRIBUTION) || defined (ALLOW_VWIND_CONTROL))
      call active_write_xy_loc( 'wvwind', wvwind, 1, 0, myThid, dummy)
#endif

#if   (defined (ALLOW_KAPGM_COST_CONTRIBUTION) || defined (ALLOW_KAPGM_CONTROL))
      call active_write_xyz( 'wkapgmFld',wkapgmFld,
     &     1, 0, myThid, dummy)
#endif

#if   (defined (ALLOW_KAPREDI_COST_CONTRIBUTION) || defined (ALLOW_KAPREDI_CONTROL))
      call active_write_xyz( 'wkaprediFld',wkaprediFld,
     &     1, 0, myThid, dummy)
#endif

#if   (defined (ALLOW_DIFFKR_COST_CONTRIBUTION) || defined (ALLOW_DIFFKR_CONTROL))
      call active_write_xyz( 'wdiffkrFld',wdiffkrFld,
     &     1, 0, myThid, dummy)
#endif

#if   ( defined (ALLOW_EDDYPSI_COST_CONTRIBUTION) || defined (ALLOW_EDDYPSI_CONTROL) )
      call active_write_xyz( 'wedtauxFld',wedtauxFld,
     &     1, 0, myThid, dummy)
      call active_write_xyz( 'wedtauyFld',wedtauyFld,
     &     1, 0, myThid, dummy)
#endif

#if (defined (ALLOW_BOTTOMDRAG_COST_CONTRIBUTION) || defined (ALLOW_BOTTOMDRAG_CONTROL))
      call active_write_xy_loc( 'wbottomdrag', wbottomdrag
     & , 1, 0, myThid, dummy)
#endif
#endif /* ECCO_CTRL_DEPRECATED */

      RETURN
      END
