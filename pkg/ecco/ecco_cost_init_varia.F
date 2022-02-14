#include "ECCO_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine ecco_cost_init_varia( myThid )

c     ==================================================================
c     SUBROUTINE ecco_cost_init_varia
c     ==================================================================
c
c     o Initialise the variable cost function part.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     changed: Christian Eckert eckert@mit.edu 18-Apr-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c     heimbach@mit.edu 05-Nov-2003 Now ecco part of cost
c
c     ==================================================================
c     SUBROUTINE ecco_cost_init_varia
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#ifdef ALLOW_ECCO
# ifdef ECCO_CTRL_DEPRECATED
#  include "ecco_cost.h"
# else
#  include "ECCO_SIZE.h"
#  include "ECCO.h"
# endif
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OBCS.h"
#endif
#ifdef ALLOW_COST
# include "cost.h"
#endif

c     == routine arguments ==
      integer myThid

c     == local variables ==
      integer bi,bj
      integer imin, imax
      integer jmin, jmax
      integer i,j,k
c     logical exst

c     == end of interface ==

      jmin = 1-OLy
      jmax = sNy+OLy
      imin = 1-OLx
      imax = sNx+OLy

c--   Initialise adjoint of monthly mean files calculated
c--   in cost_averagesfields (and their ad...).
      call cost_averagesinit( myThid )
      _BARRIER

#ifndef ALLOW_TANGENTLINEAR_RUN
cph(
cph   The following init. shoud not be applied if in the middle
cph   of a divided adjoint run
cph)
c      inquire( file='costfinal', exist=exst )
c      if ( .NOT. exst) then
c         call ecco_cost_init_barfiles( myThid )
c      endif
#endif

c--   Initialize the tiled cost function contributions.
      do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ECCO_CTRL_DEPRECATED
          objf_hflux(bi,bj)    = 0. _d 0
          objf_hfluxm(bi,bj)   = 0. _d 0
          objf_sflux(bi,bj)    = 0. _d 0
          objf_sfluxm(bi,bj)   = 0. _d 0
          objf_tauu(bi,bj)     = 0. _d 0
          objf_tauum(bi,bj)    = 0. _d 0
          objf_tauv(bi,bj)     = 0. _d 0
          objf_tauvm(bi,bj)    = 0. _d 0
          objf_temp(bi,bj)     = 0. _d 0
          objf_salt(bi,bj)     = 0. _d 0
          objf_temp0(bi,bj)    = 0. _d 0
          objf_salt0(bi,bj)    = 0. _d 0
          objf_temp0smoo(bi,bj)  = 0. _d 0
          objf_salt0smoo(bi,bj)  = 0. _d 0
          objf_etan0(bi,bj)    = 0. _d 0
          objf_uvel0(bi,bj)    = 0. _d 0
          objf_vvel0(bi,bj)    = 0. _d 0
          objf_tmi(bi,bj)      = 0. _d 0
          objf_sst(bi,bj)      = 0. _d 0
          objf_sss(bi,bj)      = 0. _d 0
          objf_bp(bi,bj)       = 0. _d 0
          objf_h(bi,bj)        = 0. _d 0
          objf_tp(bi,bj)       = 0. _d 0
          objf_ers(bi,bj)      = 0. _d 0
          objf_gfo(bi,bj)      = 0. _d 0
          objf_ctdt(bi,bj)     = 0. _d 0
          objf_ctds(bi,bj)     = 0. _d 0
          objf_ctdtclim(bi,bj) = 0. _d 0
          objf_ctdsclim(bi,bj) = 0. _d 0
          objf_xbt(bi,bj)      = 0. _d 0
          objf_argot(bi,bj)    = 0. _d 0
          objf_argos(bi,bj)    = 0. _d 0
          objf_drift(bi,bj)    = 0. _d 0
          objf_wdrift(bi,bj)   = 0. _d 0
          objf_sdrift(bi,bj)   = 0. _d 0
          objf_tdrift(bi,bj)   = 0. _d 0
          objf_scatx(bi,bj)    = 0. _d 0
          objf_scaty(bi,bj)    = 0. _d 0
          objf_scatxm(bi,bj)   = 0. _d 0
          objf_scatym(bi,bj)   = 0. _d 0
          objf_atemp(bi,bj)    = 0. _d 0
          objf_aqh(bi,bj)      = 0. _d 0
          objf_precip(bi,bj)   = 0. _d 0
          objf_swflux(bi,bj)   = 0. _d 0
          objf_swdown(bi,bj)   = 0. _d 0
          objf_lwflux(bi,bj)   = 0. _d 0
          objf_lwdown(bi,bj)   = 0. _d 0
          objf_uwind(bi,bj)    = 0. _d 0
          objf_vwind(bi,bj)    = 0. _d 0
#if ( defined (ALLOW_COST) && defined (ALLOW_COST_ATLANTIC) )
          objf_atl(bi,bj)      = 0. _d 0
#endif
          objf_curmtr(bi,bj)   = 0. _d 0
          objf_diffkr(bi,bj)   = 0. _d 0
          objf_kapgm(bi,bj)    = 0. _d 0
          objf_kapredi(bi,bj)    = 0. _d 0
          objf_theta_ini_fin(bi,bj) = 0. _d 0
          objf_salt_ini_fin(bi,bj)  = 0. _d 0
          objf_eddytau(bi,bj)    = 0. _d 0
          objf_bottomdrag(bi,bj)    = 0. _d 0
          do k=1,NUSERCOST
            objf_usercost(k,bi,bj)   = 0. _d 0
          enddo
          do k=1,NSSHV4COST
            objf_sshv4cost(k,bi,bj)  = 0. _d 0
          enddo
          num_hflux(bi,bj)    = 0. _d 0
          num_hfluxm(bi,bj)   = 0. _d 0
          num_hfluxmm(bi,bj)  = 0. _d 0
          num_sflux(bi,bj)    = 0. _d 0
          num_sfluxm(bi,bj)   = 0. _d 0
          num_sfluxmm(bi,bj)  = 0. _d 0
          num_tauu(bi,bj)     = 0. _d 0
          num_tauum(bi,bj)    = 0. _d 0
          num_tauv(bi,bj)     = 0. _d 0
          num_tauvm(bi,bj)    = 0. _d 0
          num_temp(bi,bj)     = 0. _d 0
          num_salt(bi,bj)     = 0. _d 0
          num_temp0(bi,bj)    = 0. _d 0
          num_salt0(bi,bj)    = 0. _d 0
          num_etan0(bi,bj)    = 0. _d 0
          num_uvel0(bi,bj)    = 0. _d 0
          num_vvel0(bi,bj)    = 0. _d 0
          num_tmi(bi,bj)      = 0. _d 0
          num_sst(bi,bj)      = 0. _d 0
          num_sss(bi,bj)      = 0. _d 0
          num_bp(bi,bj)       = 0. _d 0
          num_h(bi,bj)        = 0. _d 0
          num_tp(bi,bj)       = 0. _d 0
          num_ers(bi,bj)      = 0. _d 0
          num_gfo(bi,bj)      = 0. _d 0
          num_ctdt(bi,bj)     = 0. _d 0
          num_ctds(bi,bj)     = 0. _d 0
          num_ctdtclim(bi,bj) = 0. _d 0
          num_ctdsclim(bi,bj) = 0. _d 0
          num_xbt(bi,bj)      = 0. _d 0
          num_argot(bi,bj)    = 0. _d 0
          num_argos(bi,bj)    = 0. _d 0
          num_drift(bi,bj)    = 0. _d 0
          num_wdrift(bi,bj)   = 0. _d 0
          num_sdrift(bi,bj)   = 0. _d 0
          num_tdrift(bi,bj)   = 0. _d 0
          num_scatx(bi,bj)    = 0. _d 0
          num_scaty(bi,bj)    = 0. _d 0
          num_scatxm(bi,bj)   = 0. _d 0
          num_scatym(bi,bj)   = 0. _d 0
          num_atemp(bi,bj)    = 0. _d 0
          num_aqh(bi,bj)      = 0. _d 0
          num_precip(bi,bj)   = 0. _d 0
          num_swflux(bi,bj)   = 0. _d 0
          num_swdown(bi,bj)   = 0. _d 0
          num_lwflux(bi,bj)   = 0. _d 0
          num_lwdown(bi,bj)   = 0. _d 0
          num_uwind(bi,bj)    = 0. _d 0
          num_vwind(bi,bj)    = 0. _d 0
          num_curmtr(bi,bj)   = 0. _d 0
          num_diffkr(bi,bj)   = 0. _d 0
          num_kapgm(bi,bj)    = 0. _d 0
          num_kapredi(bi,bj)    = 0. _d 0
          num_eddytau(bi,bj)    = 0. _d 0
          num_bottomdrag(bi,bj)    = 0. _d 0
          do k=1,NUSERCOST
          num_usercost(k,bi,bj)    = 0. _d 0
          enddo
          do k=1,NSSHV4COST
          num_sshv4cost(k,bi,bj)    = 0. _d 0
          enddo
          num_theta_ini_fin(bi,bj) = 0. _d 0
          num_salt_ini_fin(bi,bj)  = 0. _d 0
#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_GENCOST_CONTRIBUTION
          do k=1,NGENCOST
            objf_gencost(bi,bj,k)    = 0. _d 0
            num_gencost(bi,bj,k)    = 0. _d 0
          enddo
#endif

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
          objf_obcsn(bi,bj)    = 0. _d 0
          objf_obcss(bi,bj)    = 0. _d 0
          objf_obcsw(bi,bj)    = 0. _d 0
          objf_obcse(bi,bj)    = 0. _d 0
          objf_ageos(bi,bj)    = 0. _d 0
          num_obcsn(bi,bj)    = 0. _d 0
          num_obcss(bi,bj)    = 0. _d 0
          num_obcsw(bi,bj)    = 0. _d 0
          num_obcse(bi,bj)    = 0. _d 0
          num_ageos(bi,bj)    = 0. _d 0
#endif

          do j = jmin,jmax
            do i = imin,imax
              frame(i,j)        = 1. _d 0
              cosphi(i,j,bi,bj) = 1. _d 0
            enddo
          enddo

        enddo
      enddo

#ifdef ECCO_CTRL_DEPRECATED

      k = 1
      do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
          do j = jmin,jmax
            do i = imin,imax
#ifdef ALLOW_SSH_COST_CONTRIBUTION
               if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                  mdtmask(i,j,bi,bj) = 0. _d 0
               else
                  mdtmask(i,j,bi,bj) = 1. _d 0
               endif
               mdt(i,j,bi,bj)     = 0. _d 0
#endif
#ifdef ALLOW_SSH_TPANOM_COST_CONTRIBUTION
               if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                  tpmask(i,j,bi,bj) = 0. _d 0
               else
                  tpmask(i,j,bi,bj) = 1. _d 0
               endif
               tpobs(i,j,bi,bj)      = 0. _d 0
#endif
#ifdef ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
               if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                  ersmask(i,j,bi,bj) = 0. _d 0
               else
                  ersmask(i,j,bi,bj) = 1. _d 0
               endif
               ersobs(i,j,bi,bj)     = 0. _d 0
#endif
#ifdef ALLOW_SSH_GFOANOM_COST_CONTRIBUTION
               if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                  gfomask(i,j,bi,bj) = 0. _d 0
               else
                  gfomask(i,j,bi,bj) = 1. _d 0
               endif
               gfoobs(i,j,bi,bj)     = 0. _d 0
#endif
#ifdef ALLOW_TMI_SST_COST_CONTRIBUTION
               if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                  tmimask(i,j,bi,bj) = 0. _d 0
               else
                  tmimask(i,j,bi,bj) = 1. _d 0
               endif
#endif
#ifdef ALLOW_SST_COST_CONTRIBUTION
               if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                  sstmask(i,j,bi,bj) = 0. _d 0
               else
                  sstmask(i,j,bi,bj) = 1. _d 0
               endif
#endif
#ifdef ALLOW_SSS_COST_CONTRIBUTION
               if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                  sssmask(i,j,bi,bj) = 0. _d 0
               else
                  sssmask(i,j,bi,bj) = 1. _d 0
               endif
#endif
#ifdef ALLOW_BP_COST_CONTRIBUTION
               if (_hFacC(i,j,k,bi,bj) .eq. 0.) then
                  bpmask(i,j,bi,bj) = 0. _d 0
               else
                  bpmask(i,j,bi,bj) = 1. _d 0
               endif
               bpdat(i,j,bi,bj)     = 0. _d 0
#endif
            enddo
          enddo
        enddo
      enddo

#ifdef GENERIC_BAR_MONTH
      do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
        do k =1,Nr
         do j = jmin,jmax
          do i = imin,imax
           tbar_gen(i,j,k,bi,bj) = 0. _d 0
           sbar_gen(i,j,k,bi,bj) = 0. _d 0
          enddo
         enddo
        enddo
       enddo
      enddo
#endif

c--   Initialise the "global" parts of the cost function.
      _BEGIN_MASTER( myThid )
        objf_hmean   = 0. _d 0
        objf_hfluxmm = 0. _d 0
        objf_sfluxmm = 0. _d 0
        objf_transp  = 0. _d 0
        num_hmean   = 0. _d 0
        num_transp  = 0. _d 0
      _END_MASTER( myThid )

#endif /* ECCO_CTRL_DEPRECATED */

c--   Initialise the "global" parts of the cost function.
#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
      _BEGIN_MASTER( myThid )
        objf_obcsvol = 0. _d 0
        num_obcsvol = 0. _d 0
      _END_MASTER( myThid )
#endif

      _BARRIER

      RETURN
      END
