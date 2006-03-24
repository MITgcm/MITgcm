C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_cost_final.F,v 1.21 2006/03/24 22:58:25 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine ecco_cost_final( mythid )

c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================
c
c     o Sum of all cost function contributions.
c
c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

#include "cost.h"
#include "ecco_cost.h"
#include "ctrl.h"
#include "optim.h"
#ifdef ALLOW_PROFILES
# include "profiles.h"
#else
      integer NFILESPROFMAX
      parameter (NFILESPROFMAX=1)
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer ifc
      integer totnum
      integer num_file,num_var

      _RL f_temp0, f_salt0, f_temp0smoo, f_salt0smoo
      _RL f_temp, f_salt 
      _RL f_uwind, f_vwind
      _RL f_atemp, f_aqh, f_precip
      _RL f_swflux, f_swdown
      _RL f_uwindm, f_vwindm
      _RL f_atempm, f_aqhm, f_precipm
      _RL f_swfluxm, f_swdownm
      _RL f_uwindsmoo, f_vwindsmoo
      _RL f_atempsmoo, f_aqhsmoo, f_precipsmoo
      _RL f_swfluxsmoo, f_swdownsmoo
      _RL f_tauu, f_tauv, f_hflux, f_sflux 
      _RL f_tauum, f_tauvm, f_hfluxm, f_sfluxm 
      _RL f_tauusmoo, f_tauvsmoo, f_hfluxsmoo, f_sfluxsmoo
      _RL f_hfluxmm, f_sfluxmm, f_hfluxmm2, f_sfluxmm2
      _RL f_sst, f_tmi, f_sss, f_atl, f_ctdt, f_ctds
      _RL f_drifter, f_xbt, f_tdrift, f_sdrift, f_wdrift
      _RL f_argot, f_argos, f_ssh ,f_ctdtclim, f_ctdsclim
      _RL f_scatx, f_scaty, f_scatxm, f_scatym
      _RL f_obcsn, f_obcss, f_obcsw, f_obcse
      _RL f_ageos, f_curmtr
      _RL f_ini_fin
      _RL f_kapgm, f_diffkr
      _RL f_eddytau
      _RL f_profiles(NFILESPROFMAX,4)

      _RL no_temp0, no_salt0,  no_temp, no_salt 
      _RL no_uwind, no_vwind
      _RL no_atemp, no_aqh, no_precip, no_swflux, no_swdown
      _RL no_uwindm, no_vwindm
      _RL no_atempm, no_aqhm, no_precipm, no_swfluxm, no_swdownm
      _RL no_tauu, no_tauv, no_hflux, no_sflux 
      _RL no_tauum, no_tauvm, no_hfluxm, no_sfluxm 
      _RL no_hfluxmm, no_sfluxmm 
      _RL no_sst, no_tmi, no_sss, no_atl, no_ctdt, no_ctds
      _RL no_drifter, no_xbt, no_tdrift, no_sdrift, no_wdrift
      _RL no_argot, no_argos, no_ssh ,no_ctdtclim, no_ctdsclim
      _RL no_scatx, no_scaty, no_scatxm, no_scatym
      _RL no_obcsn, no_obcss, no_obcsw, no_obcse
      _RL no_ageos, no_curmtr
      _RL no_ini_fin
      _RL no_kapgm, no_diffkr
      _RL no_eddytau
      _RL no_hmean
      _RL no_profiles(NFILESPROFMAX,4)

      character*20 cfname
#ifdef ECCO_VERBOSE
      character*(MAX_LEN_MBUF) msgbuf
#endif

      INTEGER IL
C Functions
      INTEGER ILNBLNK

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      ifc = 30
      
      f_temp = 0. _d 0
      f_salt = 0. _d 0
      f_temp0 = 0. _d 0
      f_salt0 = 0. _d 0 
      f_temp0smoo = 0. _d 0
      f_salt0smoo = 0. _d 0 
      f_tauu = 0. _d 0 
      f_tauum = 0. _d 0 
      f_tauusmoo = 0. _d 0 
      f_tauv = 0. _d 0 
      f_tauvm = 0. _d 0 
      f_tauvsmoo = 0. _d 0 
      f_hflux = 0. _d 0 
      f_hfluxm = 0. _d 0
      f_hfluxsmoo = 0. _d 0 
      f_hfluxmm = 0. _d 0 
      f_hfluxmm2 = 0. _d 0 
      f_sflux  = 0. _d 0
      f_sfluxsmoo = 0. _d 0 
      f_sfluxm  = 0. _d 0
      f_sfluxmm  = 0. _d 0
      f_sfluxmm2  = 0. _d 0
      f_uwind  = 0. _d 0
      f_vwind  = 0. _d 0
      f_atemp  = 0. _d 0
      f_aqh  = 0. _d 0
      f_precip = 0. _d 0
      f_swflux = 0. _d 0
      f_swdown = 0. _d 0
      f_uwindm  = 0. _d 0
      f_vwindm  = 0. _d 0
      f_atempm  = 0. _d 0
      f_aqhm  = 0. _d 0
      f_precipm = 0. _d 0
      f_swfluxm = 0. _d 0
      f_swdownm = 0. _d 0
      f_uwindsmoo  = 0. _d 0
      f_vwindsmoo  = 0. _d 0
      f_atempsmoo  = 0. _d 0
      f_aqhsmoo  = 0. _d 0
      f_precipsmoo = 0. _d 0
      f_swfluxsmoo = 0. _d 0
      f_swdownsmoo = 0. _d 0
      f_ssh = 0. _d 0
      f_sst = 0. _d 0
      f_tmi = 0. _d 0
      f_sss = 0. _d 0
      f_atl = 0. _d 0
      f_ctdt = 0. _d 0 
      f_ctds = 0. _d 0
      f_ctdtclim = 0. _d 0 
      f_ctdsclim = 0. _d 0
      f_xbt = 0. _d 0
      f_argot = 0. _d 0
      f_argos = 0. _d 0
      f_drifter = 0. _d 0 
      f_sdrift = 0. _d 0
      f_tdrift = 0. _d 0
      f_wdrift = 0. _d 0
      f_scatx = 0. _d 0
      f_scaty = 0. _d 0
      f_scatxm = 0. _d 0
      f_scatym = 0. _d 0
      f_obcsn   = 0. _d 0
      f_obcss   = 0. _d 0
      f_obcsw   = 0. _d 0
      f_obcse   = 0. _d 0
      f_curmtr  = 0. _d 0
      f_ageos   = 0. _d 0
      f_ini_fin = 0. _d 0
      f_kapgm   = 0. _d 0
      f_diffkr  = 0. _d 0
      f_eddytau = 0. _d 0
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,4
        f_profiles(num_file,num_var)= 0. _d 0
       enddo
      enddo
#endif

      no_temp = 0. _d 0
      no_salt = 0. _d 0
      no_temp0 = 0. _d 0
      no_salt0 = 0. _d 0 
      no_tauu = 0. _d 0 
      no_tauum = 0. _d 0 
      no_tauv = 0. _d 0 
      no_tauvm = 0. _d 0 
      no_hflux = 0. _d 0 
      no_hfluxm = 0. _d 0 
      no_hfluxmm = 0. _d 0 
      no_sflux  = 0. _d 0
      no_sfluxm  = 0. _d 0
      no_sfluxmm  = 0. _d 0
      no_uwind  = 0. _d 0
      no_vwind  = 0. _d 0
      no_atemp  = 0. _d 0
      no_aqh  = 0. _d 0
      no_precip = 0. _d 0
      no_swflux = 0. _d 0
      no_swdown = 0. _d 0
      no_uwindm  = 0. _d 0
      no_vwindm  = 0. _d 0
      no_atempm  = 0. _d 0
      no_aqhm  = 0. _d 0
      no_precipm = 0. _d 0
      no_swfluxm = 0. _d 0
      no_swdownm = 0. _d 0
      no_ssh = 0. _d 0
      no_sst = 0. _d 0
      no_tmi = 0. _d 0
      no_sss = 0. _d 0
      no_atl = 0. _d 0
      no_ctdt = 0. _d 0 
      no_ctds = 0. _d 0
      no_ctdtclim = 0. _d 0 
      no_ctdsclim = 0. _d 0
      no_xbt = 0. _d 0
      no_argot = 0. _d 0
      no_argos = 0. _d 0
      no_drifter = 0. _d 0 
      no_sdrift = 0. _d 0
      no_tdrift = 0. _d 0
      no_wdrift = 0. _d 0
      no_scatx = 0. _d 0
      no_scaty = 0. _d 0
      no_scatxm = 0. _d 0
      no_scatym = 0. _d 0
      no_obcsn   = 0. _d 0
      no_obcss   = 0. _d 0
      no_obcsw   = 0. _d 0
      no_obcse   = 0. _d 0
      no_curmtr  = 0. _d 0
      no_ageos   = 0. _d 0
      no_ini_fin = 0. _d 0
      no_kapgm   = 0. _d 0
      no_diffkr  = 0. _d 0
      no_eddytau = 0. _d 0
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,4
        no_profiles(num_file,num_var)= 0. _d 0
       enddo
      enddo
#endif

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi
 
          fc = fc
     &         + mult_temp    * objf_temp(bi,bj)
     &         + mult_salt    * objf_salt(bi,bj)
     &         + mult_temp0   * ( objf_temp0(bi,bj)
     &                         +mult_smooth_ic*objf_temp0smoo(bi,bj) )
     &         + mult_salt0   * ( objf_salt0(bi,bj)
     &                         +mult_smooth_ic*objf_salt0smoo(bi,bj) )
     &         + mult_sst     * objf_sst(bi,bj) 
     &         + mult_tmi     * objf_tmi(bi,bj) 
     &         + mult_sss     * objf_sss(bi,bj) 
     &         + mult_tauu    * ( objf_tauu(bi,bj)+objf_tauum(bi,bj)
     &                         +mult_smooth_bc*objf_tauusmoo(bi,bj) )
     &         + mult_tauv    * ( objf_tauv(bi,bj)+objf_tauvm(bi,bj)
     &                         +mult_smooth_bc*objf_tauvsmoo(bi,bj) )
     &         + mult_hflux   * ( objf_hflux(bi,bj)+objf_hfluxm(bi,bj)
     &                         +mult_smooth_bc*objf_hfluxsmoo(bi,bj) )
     &         + mult_sflux   * ( objf_sflux(bi,bj)+objf_sfluxm(bi,bj)
     &                         +mult_smooth_bc*objf_sfluxsmoo(bi,bj) )
     &         + mult_h       * objf_h(bi,bj)
     &         + mult_atl     * objf_atl(bi,bj)
     &         + mult_ctdt    * objf_ctdt(bi,bj)
     &         + mult_ctds    * objf_ctds(bi,bj)
     &         + mult_ctdtclim* objf_ctdtclim(bi,bj)
     &         + mult_ctdsclim* objf_ctdsclim(bi,bj)
     &         + mult_xbt     * objf_xbt(bi,bj)
     &         + mult_argot   * objf_argot(bi,bj)
     &         + mult_argos   * objf_argos(bi,bj)
     &         + mult_drift   * objf_drift(bi,bj)
     &         + mult_sdrift  * objf_sdrift(bi,bj)
     &         + mult_tdrift  * objf_tdrift(bi,bj)
     &         + mult_wdrift  * objf_wdrift(bi,bj)
     &         + mult_scatx   * objf_scatx(bi,bj)
     &         + mult_scaty   * objf_scaty(bi,bj)
     &         + mult_scatx   * objf_scatxm(bi,bj)
     &         + mult_scaty   * objf_scatym(bi,bj)
     &         + mult_uwind   * ( objf_uwind(bi,bj)+objf_uwindm(bi,bj)
     &                         +mult_smooth_bc*objf_uwindsmoo(bi,bj) )
     &         + mult_vwind   * ( objf_vwind(bi,bj)+objf_vwindm(bi,bj)
     &                         +mult_smooth_bc*objf_vwindsmoo(bi,bj) )
     &         + mult_atemp   * ( objf_atemp(bi,bj)+objf_atempm(bi,bj)
     &                         +mult_smooth_bc*objf_atempsmoo(bi,bj) )
     &         + mult_aqh     * ( objf_aqh(bi,bj)+objf_aqhm(bi,bj)
     &                         +mult_smooth_bc*objf_aqhsmoo(bi,bj) )
     &         + mult_precip  * ( objf_precip(bi,bj)+objf_precipm(bi,bj)
     &                         +mult_smooth_bc*objf_precipsmoo(bi,bj) )
     &         + mult_swflux  * ( objf_swflux(bi,bj)+objf_swfluxm(bi,bj)
     &                         +mult_smooth_bc*objf_swfluxsmoo(bi,bj) )
     &         + mult_swdown  * ( objf_swdown(bi,bj)+objf_swdownm(bi,bj)
     &                         +mult_smooth_bc*objf_swdownsmoo(bi,bj) )
     &         + mult_obcsn   * objf_obcsn(bi,bj)  
     &         + mult_obcss   * objf_obcss(bi,bj)  
     &         + mult_obcsw   * objf_obcsw(bi,bj)  
     &         + mult_obcse   * objf_obcse(bi,bj)  
     &         + mult_curmtr  * objf_curmtr(bi,bj)
     &         + mult_ageos   * objf_ageos(bi,bj)
     &         + mult_kapgm   * objf_kapgm(bi,bj)
     &         + mult_diffkr  * objf_diffkr(bi,bj)
     &         + mult_ini_fin *(objf_theta_ini_fin(bi,bj) +
     &                          objf_salt_ini_fin(bi,bj))
     &         + mult_eddytau * objf_eddytau(bi,bj)
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,4
          fc = fc
     &            + mult_profiles(num_file,num_var)
     &            *objf_profiles(num_file,num_var,bi,bj)
       enddo
      enddo
#endif

          f_temp = f_temp + objf_temp(bi,bj)
          f_salt = f_salt + objf_salt(bi,bj)
          f_temp0 = f_temp0 + objf_temp0(bi,bj)
          f_salt0 = f_salt0 + objf_salt0(bi,bj) 
          f_temp0smoo = f_temp0smoo + objf_temp0smoo(bi,bj)
          f_salt0smoo = f_salt0smoo + objf_salt0smoo(bi,bj) 
          f_tauu = f_tauu + objf_tauu(bi,bj)
          f_tauum  = f_tauum + objf_tauum(bi,bj)
          f_tauusmoo = f_tauusmoo + objf_tauusmoo(bi,bj)
          f_tauv = f_tauv + objf_tauv(bi,bj)
          f_tauvm  = f_tauvm + objf_tauvm(bi,bj)
          f_tauvsmoo = f_tauvsmoo + objf_tauvsmoo(bi,bj)
          f_hflux = f_hflux + objf_hflux(bi,bj)   
          f_hfluxm = f_hfluxm + objf_hfluxm(bi,bj)   
          f_hfluxmm = f_hfluxmm + objf_hfluxmm(bi,bj)   
          f_hfluxsmoo = f_hfluxsmoo + objf_hfluxsmoo(bi,bj)   
          f_sflux = f_sflux + objf_sflux(bi,bj)  
          f_sfluxm = f_sfluxm + objf_sfluxm(bi,bj)  
          f_sfluxmm = f_sfluxmm + objf_sfluxmm(bi,bj)  
          f_sfluxsmoo = f_sfluxsmoo + objf_sfluxsmoo(bi,bj)   
          f_uwind  = f_uwind + objf_uwind(bi,bj)
          f_vwind  = f_vwind + objf_vwind(bi,bj)
          f_atemp  = f_atemp + objf_atemp(bi,bj)
          f_aqh    = f_aqh + objf_aqh(bi,bj)
          f_precip = f_precip + objf_precip(bi,bj)
          f_swflux = f_swflux + objf_swflux(bi,bj)
          f_swdown = f_swdown + objf_swdown(bi,bj)
          f_uwindm   = f_uwindm + objf_uwindm(bi,bj)
          f_vwindm   = f_vwindm + objf_vwindm(bi,bj)
          f_atempm   = f_atempm + objf_atempm(bi,bj)
          f_aqhm     = f_aqhm   + objf_aqhm(bi,bj)
          f_precipm  = f_precipm + objf_precipm(bi,bj)
          f_swfluxm  = f_swfluxm + objf_swfluxm(bi,bj)
          f_swdownm  = f_swdownm + objf_swdownm(bi,bj)
          f_uwindsmoo  = f_uwindsmoo + objf_uwindsmoo(bi,bj)
          f_vwindsmoo  = f_vwindsmoo + objf_vwindsmoo(bi,bj)
          f_atempsmoo  = f_atempsmoo + objf_atempsmoo(bi,bj)
          f_aqhsmoo    = f_aqhsmoo + objf_aqhsmoo(bi,bj)
          f_precipsmoo = f_precipsmoo + objf_precipsmoo(bi,bj)
          f_swfluxsmoo = f_swfluxsmoo + objf_swfluxsmoo(bi,bj)
          f_swdownsmoo = f_swdownsmoo + objf_swdownsmoo(bi,bj)
          f_ssh  = f_ssh + objf_h(bi,bj) 
          f_sst  = f_sst + objf_sst(bi,bj) 
          f_tmi  = f_tmi + objf_tmi(bi,bj) 
          f_sss  = f_sss + objf_sss(bi,bj)
          f_atl  = f_atl + objf_atl(bi,bj)
          f_ctdt = f_ctdt + objf_ctdt(bi,bj)
          f_ctds = f_ctds + objf_ctds(bi,bj) 
          f_ctdtclim = f_ctdtclim + objf_ctdtclim(bi,bj)
          f_ctdsclim = f_ctdsclim + objf_ctdsclim(bi,bj) 
          f_xbt  = f_xbt + objf_xbt(bi,bj)
          f_argot  = f_argot + objf_argot(bi,bj)
          f_argos  = f_argos + objf_argos(bi,bj)
          f_drifter = f_drifter + objf_drift(bi,bj)
          f_sdrift = f_sdrift + objf_sdrift(bi,bj)
          f_tdrift = f_tdrift + objf_tdrift(bi,bj)
          f_wdrift = f_wdrift + objf_wdrift(bi,bj)
          f_scatx = f_scatx + objf_scatx(bi,bj)
          f_scaty = f_scaty + objf_scaty(bi,bj)
          f_scatxm = f_scatxm + objf_scatxm(bi,bj)
          f_scatym = f_scatym + objf_scatym(bi,bj)
          f_curmtr = f_curmtr + objf_curmtr(bi,bj)
          f_ageos = f_ageos + objf_ageos(bi,bj)
          f_kapgm = f_kapgm + objf_kapgm(bi,bj)
          f_diffkr = f_diffkr + objf_diffkr(bi,bj)
          f_ini_fin = f_ini_fin +
     &         objf_theta_ini_fin(bi,bj) + objf_salt_ini_fin(bi,bj)
          f_eddytau = f_eddytau + objf_eddytau(bi,bj) 
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,4
          f_profiles(num_file,num_var)=f_profiles(num_file,num_var)
     &            +objf_profiles(num_file,num_var,bi,bj)
       enddo
      enddo
#endif
          no_temp = no_temp + num_temp(bi,bj)
          no_salt = no_salt + num_salt(bi,bj)
          no_temp0 = no_temp0 + num_temp0(bi,bj)
          no_salt0 = no_salt0 + num_salt0(bi,bj) 
          no_tauu = no_tauu + num_tauu(bi,bj)
          no_tauum  = no_tauum + num_tauum(bi,bj)
          no_tauv = no_tauv + num_tauv(bi,bj)
          no_tauvm  = no_tauvm + num_tauvm(bi,bj)
          no_hflux= no_hflux + num_hflux(bi,bj)   
          no_hfluxm = no_hfluxm + num_hfluxm(bi,bj)   
          no_hfluxmm = no_hfluxmm + num_hfluxmm(bi,bj)   
          no_sflux= no_sflux + num_sflux(bi,bj)  
          no_sfluxm = no_sfluxm + num_sfluxm(bi,bj)  
          no_sfluxmm = no_sfluxmm + num_sfluxmm(bi,bj)  
          no_atemp  = no_atemp + num_atemp(bi,bj)
          no_aqh    = no_aqh + num_aqh(bi,bj)
          no_precip = no_precip + num_precip(bi,bj)
          no_swflux = no_swflux + num_swflux(bi,bj)
          no_swdown = no_swdown + num_swdown(bi,bj)
          no_uwind  = no_uwind + num_uwind(bi,bj)
          no_vwind  = no_vwind + num_vwind(bi,bj)
          no_atempm  = no_atempm + num_atempm(bi,bj)
          no_aqhm    = no_aqhm + num_aqhm(bi,bj)
          no_precipm = no_precipm + num_precipm(bi,bj)
          no_swfluxm = no_swfluxm + num_swfluxm(bi,bj)
          no_swdownm = no_swdownm + num_swdownm(bi,bj)
          no_uwindm  = no_uwindm + num_uwindm(bi,bj)
          no_vwindm  = no_vwindm + num_vwindm(bi,bj)
          no_ssh  = no_ssh + num_h(bi,bj) 
          no_sst  = no_sst + num_sst(bi,bj) 
          no_tmi  = no_tmi + num_tmi(bi,bj) 
          no_sss  = no_sss + num_sss(bi,bj)
          no_ctdt = no_ctdt + num_ctdt(bi,bj)
          no_ctds = no_ctds + num_ctds(bi,bj) 
          no_ctdtclim = no_ctdtclim + num_ctdtclim(bi,bj)
          no_ctdsclim = no_ctdsclim + num_ctdsclim(bi,bj) 
          no_xbt  = no_xbt + num_xbt(bi,bj)
          no_argot  = no_argot + num_argot(bi,bj)
          no_argos  = no_argos + num_argos(bi,bj)
          no_drifter = no_drifter + num_drift(bi,bj)
          no_sdrift = no_sdrift + num_sdrift(bi,bj)
          no_tdrift = no_tdrift + num_tdrift(bi,bj)
          no_wdrift = no_wdrift + num_wdrift(bi,bj)
          no_scatx = no_scatx + num_scatx(bi,bj)
          no_scaty = no_scaty + num_scaty(bi,bj)
          no_scatxm = no_scatxm + num_scatxm(bi,bj)
          no_scatym = no_scatym + num_scatym(bi,bj)
          no_curmtr = no_curmtr + num_curmtr(bi,bj)
          no_ageos = no_ageos + num_ageos(bi,bj)
          no_kapgm = no_kapgm + num_kapgm(bi,bj)
          no_diffkr = no_diffkr + num_diffkr(bi,bj)
          no_ini_fin = no_ini_fin +
     &         num_theta_ini_fin(bi,bj) + num_salt_ini_fin(bi,bj)
          no_eddytau = no_eddytau + num_eddytau(bi,bj) 
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,4
          no_profiles(num_file,num_var)=no_profiles(num_file,num_var)
     &            +num_profiles(num_file,num_var,bi,bj)
       enddo
      enddo
#endif

        enddo
      enddo


c--   Do global summation.
      _GLOBAL_SUM_R8( fc , myThid )

c--   Do global summation for each part of the cost function
            
      _GLOBAL_SUM_R8( f_temp , myThid )
      _GLOBAL_SUM_R8( f_salt , myThid )
      _GLOBAL_SUM_R8( f_temp0, myThid )
      _GLOBAL_SUM_R8( f_salt0, myThid )
      _GLOBAL_SUM_R8( f_temp0smoo, myThid )
      _GLOBAL_SUM_R8( f_salt0smoo, myThid )
      _GLOBAL_SUM_R8( f_tauu , myThid )
      _GLOBAL_SUM_R8( f_tauum , myThid )
      _GLOBAL_SUM_R8( f_tauusmoo , myThid )
      _GLOBAL_SUM_R8( f_tauv , myThid )
      _GLOBAL_SUM_R8( f_tauvm , myThid )
      _GLOBAL_SUM_R8( f_tauvsmoo , myThid )
      _GLOBAL_SUM_R8( f_hflux , myThid )
      _GLOBAL_SUM_R8( f_hfluxm , myThid )
      _GLOBAL_SUM_R8( f_hfluxmm , myThid )
      _GLOBAL_SUM_R8( f_hfluxsmoo , myThid )
      _GLOBAL_SUM_R8( f_sflux , myThid )
      _GLOBAL_SUM_R8( f_sfluxm , myThid )
      _GLOBAL_SUM_R8( f_sfluxmm , myThid )
      _GLOBAL_SUM_R8( f_sfluxsmoo , myThid )
      _GLOBAL_SUM_R8( f_uwind , myThid )
      _GLOBAL_SUM_R8( f_vwind , myThid )
      _GLOBAL_SUM_R8( f_atemp , myThid )
      _GLOBAL_SUM_R8( f_aqh , myThid )
      _GLOBAL_SUM_R8( f_precip , myThid )
      _GLOBAL_SUM_R8( f_swflux , myThid )
      _GLOBAL_SUM_R8( f_swdown , myThid )
      _GLOBAL_SUM_R8( f_uwindm , myThid )
      _GLOBAL_SUM_R8( f_vwindm , myThid )
      _GLOBAL_SUM_R8( f_atempm , myThid )
      _GLOBAL_SUM_R8( f_aqhm   , myThid )
      _GLOBAL_SUM_R8( f_precipm , myThid )
      _GLOBAL_SUM_R8( f_swfluxm , myThid )
      _GLOBAL_SUM_R8( f_swdownm , myThid )
      _GLOBAL_SUM_R8( f_uwindsmoo , myThid )
      _GLOBAL_SUM_R8( f_vwindsmoo , myThid )
      _GLOBAL_SUM_R8( f_atempsmoo , myThid )
      _GLOBAL_SUM_R8( f_aqhsmoo , myThid )
      _GLOBAL_SUM_R8( f_precipsmoo , myThid )
      _GLOBAL_SUM_R8( f_swfluxsmoo , myThid )
      _GLOBAL_SUM_R8( f_swdownsmoo , myThid )
      _GLOBAL_SUM_R8( f_ssh , myThid )
      _GLOBAL_SUM_R8( f_sst , myThid )
      _GLOBAL_SUM_R8( f_tmi , myThid )
      _GLOBAL_SUM_R8( f_sss , myThid )
      _GLOBAL_SUM_R8( f_atl , myThid )
      _GLOBAL_SUM_R8( f_ctdt , myThid )
      _GLOBAL_SUM_R8( f_ctds , myThid ) 
      _GLOBAL_SUM_R8( f_ctdtclim , myThid  )
      _GLOBAL_SUM_R8( f_ctdsclim , myThid  ) 
      _GLOBAL_SUM_R8( f_xbt , myThid )
      _GLOBAL_SUM_R8( f_argot , myThid )
      _GLOBAL_SUM_R8( f_argos , myThid )
      _GLOBAL_SUM_R8( f_drifter , myThid ) 
      _GLOBAL_SUM_R8( f_sdrift , myThid )
      _GLOBAL_SUM_R8( f_tdrift , myThid )    
      _GLOBAL_SUM_R8( f_wdrift , myThid )    
      _GLOBAL_SUM_R8( f_scatx , myThid )    
      _GLOBAL_SUM_R8( f_scaty , myThid )    
      _GLOBAL_SUM_R8( f_scatxm , myThid )    
      _GLOBAL_SUM_R8( f_scatym , myThid )    
      _GLOBAL_SUM_R8( f_obcsn , myThid )
      _GLOBAL_SUM_R8( f_obcss , myThid )
      _GLOBAL_SUM_R8( f_obcsw , myThid )
      _GLOBAL_SUM_R8( f_obcse , myThid )
      _GLOBAL_SUM_R8( f_curmtr , myThid )
      _GLOBAL_SUM_R8( f_ageos , myThid )
      _GLOBAL_SUM_R8( f_kapgm , myThid )
      _GLOBAL_SUM_R8( f_diffkr , myThid )
      _GLOBAL_SUM_R8( f_ini_fin , myThid )
      _GLOBAL_SUM_R8( f_eddytau , myThid )
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,4
        _GLOBAL_SUM_R8(f_profiles(num_file,num_var), myThid )
       enddo
      enddo
#endif

      _GLOBAL_SUM_R8( no_temp , myThid )
      _GLOBAL_SUM_R8( no_salt , myThid )
      _GLOBAL_SUM_R8( no_temp0, myThid )
      _GLOBAL_SUM_R8( no_salt0, myThid )
      _GLOBAL_SUM_R8( no_tauu , myThid )
      _GLOBAL_SUM_R8( no_tauum , myThid )
      _GLOBAL_SUM_R8( no_tauv , myThid )
      _GLOBAL_SUM_R8( no_tauvm , myThid )
      _GLOBAL_SUM_R8( no_hflux , myThid )
      _GLOBAL_SUM_R8( no_hfluxm , myThid )
      _GLOBAL_SUM_R8( no_hfluxmm , myThid )
      _GLOBAL_SUM_R8( no_sflux , myThid )
      _GLOBAL_SUM_R8( no_sfluxm , myThid )
      _GLOBAL_SUM_R8( no_sfluxmm , myThid )
      _GLOBAL_SUM_R8( no_uwind , myThid )
      _GLOBAL_SUM_R8( no_vwind , myThid )
      _GLOBAL_SUM_R8( no_atemp , myThid )
      _GLOBAL_SUM_R8( no_aqh , myThid )
      _GLOBAL_SUM_R8( no_precip , myThid )
      _GLOBAL_SUM_R8( no_swflux , myThid )
      _GLOBAL_SUM_R8( no_swdown , myThid )
      _GLOBAL_SUM_R8( no_uwindm , myThid )
      _GLOBAL_SUM_R8( no_vwindm , myThid )
      _GLOBAL_SUM_R8( no_atempm , myThid )
      _GLOBAL_SUM_R8( no_aqhm   , myThid )
      _GLOBAL_SUM_R8( no_precipm , myThid )
      _GLOBAL_SUM_R8( no_swfluxm , myThid )
      _GLOBAL_SUM_R8( no_swdownm , myThid )
      _GLOBAL_SUM_R8( no_ssh , myThid )
      _GLOBAL_SUM_R8( no_sst , myThid )
      _GLOBAL_SUM_R8( no_tmi , myThid )
      _GLOBAL_SUM_R8( no_sss , myThid )
      _GLOBAL_SUM_R8( no_atl , myThid )
      _GLOBAL_SUM_R8( no_ctdt , myThid )
      _GLOBAL_SUM_R8( no_ctds , myThid ) 
      _GLOBAL_SUM_R8( no_ctdtclim , myThid  )
      _GLOBAL_SUM_R8( no_ctdsclim , myThid  ) 
      _GLOBAL_SUM_R8( no_xbt , myThid )
      _GLOBAL_SUM_R8( no_argot , myThid )
      _GLOBAL_SUM_R8( no_argos , myThid )
      _GLOBAL_SUM_R8( no_drifter , myThid ) 
      _GLOBAL_SUM_R8( no_sdrift , myThid )
      _GLOBAL_SUM_R8( no_tdrift , myThid )    
      _GLOBAL_SUM_R8( no_wdrift , myThid )    
      _GLOBAL_SUM_R8( no_scatx , myThid )    
      _GLOBAL_SUM_R8( no_scaty , myThid )    
      _GLOBAL_SUM_R8( no_scatxm , myThid )    
      _GLOBAL_SUM_R8( no_scatym , myThid )    
      _GLOBAL_SUM_R8( no_obcsn , myThid )
      _GLOBAL_SUM_R8( no_obcss , myThid )
      _GLOBAL_SUM_R8( no_obcsw , myThid )
      _GLOBAL_SUM_R8( no_obcse , myThid )
      _GLOBAL_SUM_R8( no_curmtr , myThid )
      _GLOBAL_SUM_R8( no_ageos , myThid )
      _GLOBAL_SUM_R8( no_kapgm , myThid )
      _GLOBAL_SUM_R8( no_diffkr , myThid )
      _GLOBAL_SUM_R8( no_ini_fin , myThid )
      _GLOBAL_SUM_R8( no_eddytau , myThid )
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,4
        _GLOBAL_SUM_R8(no_profiles(num_file,num_var), myThid )
       enddo
      enddo
#endif

      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_temp    =',f_temp
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_salt    =',f_salt
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_temp0   =',f_temp0
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_salt0   =',f_salt0
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_temp0smoo =',f_temp0smoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_salt0smoo =',f_salt0smoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_sst     =',f_sst
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_tmi     =',f_tmi
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_sss     =',f_sss
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ssh       =',f_ssh
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_tauu    =',f_tauu
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_tauum   =',f_tauum
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_tauusmoo =',f_tauusmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_tauv    =',f_tauv
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_tauvm   =',f_tauvm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_tauvsmoo =',f_tauvsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_hflux   =',f_hflux
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_hfluxm  =',f_hfluxm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_hfluxmm =',f_hfluxmm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_hfluxsmoo =',f_hfluxsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_sflux   =',f_sflux
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_sfluxm  =',f_sfluxm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_sfluxmm =',f_sfluxmm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_sfluxsmoo =',f_sfluxsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_uwind     =',f_uwind
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_vwind     =',f_vwind
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_atemp     =',f_atemp
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_aqh     =',f_aqh
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_precip  =',f_precip
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_swflux  =',f_swflux
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_swdown  =',f_swdown
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_uwindm     =',f_uwindm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_vwindm     =',f_vwindm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_atempm     =',f_atempm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_aqhm     =',f_aqhm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_precipm  =',f_precipm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_swfluxm  =',f_swfluxm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_swdownm  =',f_swdownm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_uwindsmoo     =',f_uwindsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_vwindsmoo     =',f_vwindsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_atempsmoo     =',f_atempsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_aqhsmoo     =',f_aqhsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_precipsmoo  =',f_precipsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_swfluxsmoo  =',f_swfluxsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_swdownsmoo  =',f_swdownsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_atl     =',f_atl
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ctdt    =',f_ctdt
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ctds    =',f_ctds
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ctdtclim=',f_ctdtclim
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ctdsclim=',f_ctdsclim
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_xbt     =',f_xbt
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_argot   =',f_argot
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_argos   =',f_argos
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_drifter   =',f_drifter
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_tdrift  =',f_tdrift
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_sdrift  =',f_sdrift
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_wdrift  =',f_wdrift
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_scatx   =',f_scatx
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_scaty   =',f_scaty
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_scatxm  =',f_scatxm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_scatym  =',f_scatym
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcsn   =',f_obcsn
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcss   =',f_obcss
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcsw   =',f_obcsw
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcse   =',f_obcse
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ageos   =',f_ageos
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_curmtr  =',f_curmtr
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_kapgm   =',f_kapgm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_diffkr  =',f_diffkr
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_eddytau =', f_eddytau
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,4
        if (no_profiles(num_file,num_var).GT.0) then
         write(standardmessageunit,'(A,D22.15,i2.0,i2.0)')
     &     ' --> f_profiles =',f_profiles(num_file,num_var),
     &      num_file, num_var
        endif
       enddo
      enddo
#endif

c--   Each process has calculated the global part for itself.
      _BEGIN_MASTER( mythid )

        fc = fc + mult_hmean*objf_hmean
        no_hmean = num_hmean

        f_hfluxmm2 = mult_hfluxmm
     &       *(f_hfluxmm-1.0)*(f_hfluxmm-1.0)
     &       /(wmean_hflux*wmean_hflux)
        f_sfluxmm2 = mult_sfluxmm
     &       *f_sfluxmm*f_sfluxmm
     &       /(wmean_sflux*wmean_sflux)
        fc = fc + f_hfluxmm2 + f_sfluxmm2


        write(standardmessageunit,'(A,D22.15)')
     &           ' --> f_hfluxmm2           =',f_hfluxmm2
        write(standardmessageunit,'(A,D22.15)')
     &           ' --> f_sfluxmm2           =',f_sfluxmm2
        write(standardmessageunit,'(A,D22.15)')
     &           ' --> objf_hmean           =',objf_hmean
        write(standardmessageunit,'(A,D22.15)')
     &           ' --> fc               =', fc
    
        write(cfname,'(A,i4.4)') 'costfunction',optimcycle
        open(unit=ifc,file=cfname)
      
        write(ifc,*) 'fc =', fc
        write(ifc,*) 'f_temp =', f_temp, no_temp
        write(ifc,*) 'f_salt =', f_salt, no_salt
        write(ifc,*) 'f_temp0 =', f_temp0, no_temp0
        write(ifc,*) 'f_salt0 =', f_salt0, no_salt0
        write(ifc,*) 'f_temp0smoo =', f_temp0smoo, no_temp0
        write(ifc,*) 'f_salt0smoo =', f_salt0smoo, no_salt0
        write(ifc,*) 'f_tauu =', f_tauu, no_tauu
        write(ifc,*) 'f_tauum =', f_tauum, no_tauum
        write(ifc,*) 'f_tauusmoo =', f_tauusmoo, no_tauu
        write(ifc,*) 'f_tauv =', f_tauv, no_tauv
        write(ifc,*) 'f_tauvm =', f_tauvm, no_tauvm
        write(ifc,*) 'f_tauvsmoo =', f_tauvsmoo, no_tauv
        write(ifc,*) 'f_hflux =', f_hflux, no_hflux
        write(ifc,*) 'f_hfluxm =', f_hfluxm, no_hfluxm
        write(ifc,*) 'f_hfluxmm =', f_hfluxmm, no_hfluxmm
        write(ifc,*) 'f_hfluxmm2 =', f_hfluxmm2, mult_hfluxmm
        write(ifc,*) 'f_hfluxsmoo =', f_hfluxsmoo, no_hflux
        write(ifc,*) 'f_sflux =', f_sflux, no_sflux
        write(ifc,*) 'f_sfluxm =', f_sfluxm, no_sfluxm
        write(ifc,*) 'f_sfluxmm =', f_sfluxmm, no_sfluxmm
        write(ifc,*) 'f_sfluxmm2 =', f_sfluxmm2, mult_sfluxmm
        write(ifc,*) 'f_sfluxsmoo =', f_sfluxsmoo, no_sflux
        write(ifc,*) 'f_uwind  =', f_uwind, no_uwind
        write(ifc,*) 'f_vwind  =', f_vwind, no_vwind
        write(ifc,*) 'f_atemp  =', f_atemp, no_atemp
        write(ifc,*) 'f_aqh    =', f_aqh, no_aqh
        write(ifc,*) 'f_precip =', f_precip, no_precip
        write(ifc,*) 'f_swflux =', f_swflux, no_swflux
        write(ifc,*) 'f_swdown =', f_swdown, no_swdown
        write(ifc,*) 'f_uwindm   =', f_uwindm, no_uwindm
        write(ifc,*) 'f_vwindm   =', f_vwindm, no_vwindm
        write(ifc,*) 'f_atempm   =', f_atempm, no_atempm
        write(ifc,*) 'f_aqhm     =', f_aqhm, no_aqhm
        write(ifc,*) 'f_precipm  =', f_precipm, no_precipm
        write(ifc,*) 'f_swfluxm  =', f_swfluxm, no_swfluxm
        write(ifc,*) 'f_swdownm  =', f_swdownm, no_swdownm
        write(ifc,*) 'f_uwindsmoo  =', f_uwindsmoo, no_uwind
        write(ifc,*) 'f_vwindsmoo  =', f_vwindsmoo, no_vwind
        write(ifc,*) 'f_atempsmoo  =', f_atempsmoo, no_atemp
        write(ifc,*) 'f_aqhsmoo    =', f_aqhsmoo, no_aqh
        write(ifc,*) 'f_precipsmoo =', f_precipsmoo, no_precip
        write(ifc,*) 'f_swfluxsmoo =', f_swfluxsmoo, no_swflux
        write(ifc,*) 'f_swdownsmoo =', f_swdownsmoo, no_swdown
        write(ifc,*) 'f_ssh =', f_ssh, no_ssh
        write(ifc,*) 'f_sst =', f_sst, no_sst
        write(ifc,*) 'f_tmi =', f_tmi, no_tmi
        write(ifc,*) 'f_sss =', f_sss, no_sss
        write(ifc,*) 'f_atl =', f_atl, no_atl
        write(ifc,*) 'f_ctdt =', f_ctdt, no_ctdt
        write(ifc,*) 'f_ctds =', f_ctds, no_ctds
        write(ifc,*) 'f_ctdtclim =', f_ctdtclim, no_ctdtclim
        write(ifc,*) 'f_ctdsclim =', f_ctdsclim, no_ctdsclim
        write(ifc,*) 'f_xbt =', f_xbt, no_xbt
        write(ifc,*) 'f_argot =', f_argot, no_argot
        write(ifc,*) 'f_argos =', f_argos, no_argos
        write(ifc,*) 'objf_hmean =', objf_hmean, no_hmean
        write(ifc,*) 'f_drifter =', f_drifter, no_drifter
        write(ifc,*) 'f_sdrift =', f_sdrift, no_sdrift
        write(ifc,*) 'f_tdrift =', f_tdrift, no_tdrift
        write(ifc,*) 'f_wdrift =', f_wdrift, no_wdrift
        write(ifc,*) 'f_scatx =', f_scatx, no_scatx
        write(ifc,*) 'f_scaty =', f_scaty, no_scaty
        write(ifc,*) 'f_scatxm =', f_scatxm, no_scatxm
        write(ifc,*) 'f_scatym =', f_scatym, no_scatym
        write(ifc,*) 'f_obcsn =', f_obcsn, no_obcsn
        write(ifc,*) 'f_obcss =', f_obcss, no_obcss
        write(ifc,*) 'f_obcsw =', f_obcsw, no_obcsw
        write(ifc,*) 'f_obcse =', f_obcse, no_obcse
        write(ifc,*) 'f_ageos =', f_ageos, no_ageos
        write(ifc,*) 'f_kapgm =', f_kapgm, no_kapgm
        write(ifc,*) 'f_diffkr=', f_diffkr, no_diffkr
        write(ifc,*) 'f_ini_fin =', f_ini_fin, no_ini_fin
        write(ifc,*) 'f_eddytau =', f_eddytau, no_eddytau
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,4
        if (no_profiles(num_file,num_var).GT.0) then
         IL  = ILNBLNK( profilesfiles(num_file) )
         write(ifc,*) profilesfiles(num_file)(1:IL), 
     &    prof_names(num_var), ' = ', 
     &    f_profiles(num_file,num_var), 
     &    no_profiles(num_file,num_var)
        endif
       enddo
      enddo
#endif

        close(ifc)
        
      _END_MASTER( mythid )

      call cost_trans_merid( mythid )
      call cost_trans_zonal( mythid )

      taveFreq   = 0.
      dumpFreq   = 0.
      pChkptFreq = 0.
      monitorFreq = 0.
      useDiagnostics = .FALSE.

#ifdef ALLOW_MNC
      monitor_mnc=.FALSE.
      snapshot_mnc=.FALSE.
      timeave_mnc=.FALSE.
#endif

#ifdef ECCO_VERBOSE
      write(msgbuf,'(a,D22.15)')
     &  ' cost_Final: final cost function = ',fc
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &  '             cost function evaluation finished.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

      end
