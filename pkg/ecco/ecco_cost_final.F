#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine ecco_cost_final( myThid )

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

#ifdef ALLOW_COST
# include "cost.h"
#endif
#ifdef ALLOW_ECCO
# ifdef ECCO_CTRL_DEPRECATED
#  include "ecco_cost.h"
# else
#  include "ECCO_SIZE.h"
#  include "ECCO.h"
# endif
#endif
#ifdef ALLOW_CTRL
# include "ctrl.h"
# include "optim.h"
# include "CTRL_SIZE.h"
# include "CTRL_GENARR.h"
# ifdef ALLOW_OBCS
#  include "CTRL_OBCS.h"
# endif /* ALLOW_OBCS */
#endif
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

c     == routine arguments ==

      integer myThid

C     === Functions ====
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

c     == local variables ==

      integer bi,bj
      integer ifc
      integer num_var

#ifndef ALLOW_PROFILES
      integer NFILESPROFMAX
      parameter (NFILESPROFMAX=1)
      integer NVARMAX
      parameter (NVARMAX=1)
#endif

#ifndef ALLOW_COST
c This quick fix allows to compile and run fwd but, as far as
c the adjoint, pkg/autodiff most likely require cost though.
      _RL fc, glofc
#endif
      _RL locfc

#ifdef ECCO_CTRL_DEPRECATED
      _RL f_temp0, f_salt0, f_temp0smoo, f_salt0smoo
      _RL f_etan0, f_uvel0, f_vvel0
      _RL f_temp, f_salt
      _RL f_uwind, f_vwind
      _RL f_atemp, f_aqh, f_precip
      _RL f_swflux, f_swdown
      _RL f_lwflux, f_lwdown
      _RL f_uwindm, f_vwindm
      _RL f_atempm, f_aqhm, f_precipm
      _RL f_swfluxm, f_swdownm
      _RL f_lwfluxm, f_lwdownm
      _RL f_uwindsmoo, f_vwindsmoo
      _RL f_atempsmoo, f_aqhsmoo, f_precipsmoo
      _RL f_swfluxsmoo, f_swdownsmoo
      _RL f_lwfluxsmoo, f_lwdownsmoo
      _RL f_tauu, f_tauv, f_hflux, f_sflux
      _RL f_tauum, f_tauvm, f_hfluxm, f_sfluxm
      _RL f_tauusmoo, f_tauvsmoo, f_hfluxsmoo, f_sfluxsmoo
      _RL f_hfluxmm, f_sfluxmm, f_hfluxmm2, f_sfluxmm2
      _RL f_sst, f_tmi, f_sss, f_bp, f_atl, f_ctdt, f_ctds
      _RL f_drifter, f_xbt, f_tdrift, f_sdrift, f_wdrift
      _RL f_ssh , f_tp, f_ers, f_gfo
      _RL f_argot, f_argos, f_ctdtclim, f_ctdsclim
      _RL f_scatx, f_scaty, f_scatxm, f_scatym
      _RL f_curmtr
      _RL f_ini_fin
      _RL f_kapgm, f_kapredi, f_diffkr
      _RL f_eddytau, f_bottomdrag
      _RL f_transp, f_ies
#endif /* ECCO_CTRL_DEPRECATED */

      _RL f_gencost(NGENCOST)
#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      _RL f_obcsn, f_obcss, f_obcsw, f_obcse, f_ageos
#endif
#ifdef ALLOW_PROFILES
      _RL f_profiles(NFILESPROFMAX,NVARMAX)
      _RL f_profiles_mean(NVARMAX)
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      _RL f_gentim2d(maxCtrlTim2D)
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      _RL f_genarr2d(maxCtrlArr2D)
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      _RL f_genarr3d(maxCtrlArr3D)
#endif

#ifdef ECCO_CTRL_DEPRECATED
      _RL no_temp0, no_salt0,  no_temp, no_salt
      _RL no_etan0, no_uvel0, no_vvel0
      _RL no_uwind, no_vwind
      _RL no_atemp, no_aqh, no_precip, no_swflux, no_swdown
      _RL no_lwflux, no_lwdown
      _RL no_uwindm, no_vwindm
      _RL no_atempm, no_aqhm, no_precipm, no_swfluxm, no_swdownm
      _RL no_lwfluxm, no_lwdownm
      _RL no_tauu, no_tauv, no_hflux, no_sflux
      _RL no_tauum, no_tauvm, no_hfluxm, no_sfluxm
      _RL no_hfluxmm, no_sfluxmm
      _RL no_sst, no_tmi, no_sss, no_bp, no_atl, no_ctdt, no_ctds
      _RL no_drifter, no_xbt, no_tdrift, no_sdrift, no_wdrift
      _RL no_ssh, no_tp, no_ers, no_gfo
      _RL no_argot, no_argos, no_ctdtclim, no_ctdsclim
      _RL no_scatx, no_scaty, no_scatxm, no_scatym
      _RL no_curmtr
      _RL no_ini_fin
      _RL no_kapgm, no_kapredi, no_diffkr
      _RL no_eddytau, no_bottomdrag
      _RL no_hmean
      _RL no_transp, no_ies
#endif /* ECCO_CTRL_DEPRECATED */

      _RL no_gencost(NGENCOST)
#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      _RL no_obcsn, no_obcss, no_obcsw, no_obcse, no_ageos
#endif
#ifdef ALLOW_PROFILES
      integer num_file
      _RL no_profiles(NFILESPROFMAX,NVARMAX)
      _RL no_profiles_mean(NVARMAX)
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      _RL no_gentim2d(maxCtrlTim2D)
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      _RL no_genarr2d(maxCtrlArr2D)
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      _RL no_genarr3d(maxCtrlArr3D)
#endif

      character*20 cfname
      character*(MAX_LEN_MBUF) msgBuf

      INTEGER IL

c     == end of interface ==

      ifc = 30

      locfc = 0. _d 0
#ifdef ECCO_CTRL_DEPRECATED
      f_temp = 0. _d 0
      f_salt = 0. _d 0
      f_temp0 = 0. _d 0
      f_salt0 = 0. _d 0
      f_temp0smoo = 0. _d 0
      f_salt0smoo = 0. _d 0
      f_etan0 = 0. _d 0
      f_uvel0 = 0. _d 0
      f_vvel0 = 0. _d 0
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
      f_lwflux = 0. _d 0
      f_lwdown = 0. _d 0
      f_uwindm  = 0. _d 0
      f_vwindm  = 0. _d 0
      f_atempm  = 0. _d 0
      f_aqhm  = 0. _d 0
      f_precipm = 0. _d 0
      f_swfluxm = 0. _d 0
      f_swdownm = 0. _d 0
      f_lwfluxm = 0. _d 0
      f_lwdownm = 0. _d 0
      f_uwindsmoo  = 0. _d 0
      f_vwindsmoo  = 0. _d 0
      f_atempsmoo  = 0. _d 0
      f_aqhsmoo  = 0. _d 0
      f_precipsmoo = 0. _d 0
      f_swfluxsmoo = 0. _d 0
      f_swdownsmoo = 0. _d 0
      f_lwfluxsmoo = 0. _d 0
      f_lwdownsmoo = 0. _d 0
      f_ssh = 0. _d 0
      f_tp = 0. _d 0
      f_ers = 0. _d 0
      f_gfo = 0. _d 0
      f_sst = 0. _d 0
      f_tmi = 0. _d 0
      f_sss = 0. _d 0
      f_bp = 0. _d 0
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
      f_curmtr  = 0. _d 0
      f_ini_fin = 0. _d 0
      f_kapgm   = 0. _d 0
      f_kapredi   = 0. _d 0
      f_diffkr  = 0. _d 0
      f_eddytau = 0. _d 0
      f_bottomdrag = 0. _d 0
      f_transp  = 0. _d 0
      f_ies = 0. _d 0
#endif /* ECCO_CTRL_DEPRECATED */

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      f_obcsn   = 0. _d 0
      f_obcss   = 0. _d 0
      f_obcsw   = 0. _d 0
      f_obcse   = 0. _d 0
      f_ageos   = 0. _d 0
#endif
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        f_profiles(num_file,num_var)= 0. _d 0
       enddo
      enddo
      do num_var=1,NVARMAX
       f_profiles_mean(num_var)= 0. _d 0
      enddo
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        f_gencost(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        f_gentim2d(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        f_genarr2d(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        f_genarr3d(num_var)= 0. _d 0
       enddo
#endif

#ifdef ECCO_CTRL_DEPRECATED
      no_temp = 0. _d 0
      no_salt = 0. _d 0
      no_temp0 = 0. _d 0
      no_salt0 = 0. _d 0
      no_etan0 = 0. _d 0
      no_uvel0 = 0. _d 0
      no_vvel0 = 0. _d 0
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
      no_lwflux = 0. _d 0
      no_lwdown = 0. _d 0
      no_uwindm  = 0. _d 0
      no_vwindm  = 0. _d 0
      no_atempm  = 0. _d 0
      no_aqhm  = 0. _d 0
      no_precipm = 0. _d 0
      no_swfluxm = 0. _d 0
      no_swdownm = 0. _d 0
      no_lwfluxm = 0. _d 0
      no_lwdownm = 0. _d 0
      no_ssh = 0. _d 0
      no_tp = 0. _d 0
      no_ers = 0. _d 0
      no_gfo = 0. _d 0
      no_sst = 0. _d 0
      no_tmi = 0. _d 0
      no_sss = 0. _d 0
      no_bp = 0. _d 0
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
      no_curmtr  = 0. _d 0
      no_ini_fin = 0. _d 0
      no_kapgm   = 0. _d 0
      no_kapredi   = 0. _d 0
      no_diffkr  = 0. _d 0
      no_eddytau = 0. _d 0
      no_bottomdrag = 0. _d 0
      no_transp  = 0. _d 0
      no_ies = 0. _d 0
#endif /* ECCO_CTRL_DEPRECATED */

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      no_obcsn   = 0. _d 0
      no_obcss   = 0. _d 0
      no_obcsw   = 0. _d 0
      no_obcse   = 0. _d 0
      no_ageos   = 0. _d 0
#endif
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        no_profiles(num_file,num_var)= 0. _d 0
       enddo
      enddo
      do num_var=1,NVARMAX
       no_profiles_mean(num_var)= 0. _d 0
      enddo
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        no_gencost(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        no_gentim2d(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        no_genarr2d(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        no_genarr3d(num_var)= 0. _d 0
       enddo
#endif

c--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

          tile_fc(bi,bj) = tile_fc(bi,bj)
#ifdef ECCO_CTRL_DEPRECATED
     &         + mult_temp    * objf_temp(bi,bj)
     &         + mult_salt    * objf_salt(bi,bj)
     &         + mult_temp0   * ( objf_temp0(bi,bj)
     &                         +mult_smooth_ic*objf_temp0smoo(bi,bj) )
     &         + mult_salt0   * ( objf_salt0(bi,bj)
     &                         +mult_smooth_ic*objf_salt0smoo(bi,bj) )
     &         + mult_etan0   * objf_etan0(bi,bj)
     &         + mult_uvel0   * objf_uvel0(bi,bj)
     &         + mult_vvel0   * objf_vvel0(bi,bj)
     &         + mult_sst     * objf_sst(bi,bj)
     &         + mult_tmi     * objf_tmi(bi,bj)
     &         + mult_sss     * objf_sss(bi,bj)
     &         + mult_bp      * objf_bp(bi,bj)
     &         + mult_ies     * objf_ies(bi,bj)
     &         + mult_tauu    * ( objf_tauu(bi,bj)+objf_tauum(bi,bj)
     &                         +mult_smooth_bc*objf_tauusmoo(bi,bj) )
     &         + mult_tauv    * ( objf_tauv(bi,bj)+objf_tauvm(bi,bj)
     &                         +mult_smooth_bc*objf_tauvsmoo(bi,bj) )
     &         + mult_hflux   * ( objf_hflux(bi,bj)
     &                         +mult_smooth_bc*objf_hfluxsmoo(bi,bj) )
     &         + mult_sflux   * ( objf_sflux(bi,bj)
     &                         +mult_smooth_bc*objf_sfluxsmoo(bi,bj) )
     &         + mult_h       * ( mult_tp  * objf_tp(bi,bj)
     &                          + mult_ers * objf_ers(bi,bj)
     &                          + mult_gfo * objf_gfo(bi,bj) )
#if ( defined (ALLOW_COST) && defined (ALLOW_COST_ATLANTIC) )
     &         + mult_atl     * objf_atl(bi,bj)
#endif
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
     &         + mult_lwflux  * ( objf_lwflux(bi,bj)+objf_lwfluxm(bi,bj)
     &                         +mult_smooth_bc*objf_lwfluxsmoo(bi,bj) )
     &         + mult_lwdown  * ( objf_lwdown(bi,bj)+objf_lwdownm(bi,bj)
     &                         +mult_smooth_bc*objf_lwdownsmoo(bi,bj) )
     &         + mult_curmtr  * objf_curmtr(bi,bj)
     &         + mult_kapgm   * objf_kapgm(bi,bj)
     &         + mult_kapredi   * objf_kapredi(bi,bj)
     &         + mult_diffkr  * objf_diffkr(bi,bj)
     &         + mult_ini_fin *(objf_theta_ini_fin(bi,bj) +
     &                          objf_salt_ini_fin(bi,bj))
     &         + mult_edtau * objf_eddytau(bi,bj)
     &         + mult_bottomdrag * objf_bottomdrag(bi,bj)

#endif /* ECCO_CTRL_DEPRECATED */

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &         + mult_obcsn   * objf_obcsn(bi,bj)
     &         + mult_obcss   * objf_obcss(bi,bj)
     &         + mult_obcsw   * objf_obcsw(bi,bj)
     &         + mult_obcse   * objf_obcse(bi,bj)
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
     &         + mult_ageos   * objf_ageos(bi,bj)
# endif
#endif
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_profiles(num_file,num_var)
     &            *objf_profiles(num_file,num_var,bi,bj)
       enddo
      enddo
      do num_var=1,NVARMAX
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &           + mult_profiles_mean(num_var)
     &           *objf_profiles_mean(num_var,bi,bj)
      enddo
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_gencost(num_var)
     &            *objf_gencost(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_gentim2d(num_var)
     &            *objf_gentim2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_genarr2d(num_var)
     &            *objf_genarr2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_genarr3d(num_var)
     &            *objf_genarr3d(bi,bj,num_var)
       enddo
#endif

#ifdef ECCO_CTRL_DEPRECATED
          f_temp = f_temp + objf_temp(bi,bj)
          f_salt = f_salt + objf_salt(bi,bj)
          f_temp0 = f_temp0 + objf_temp0(bi,bj)
          f_salt0 = f_salt0 + objf_salt0(bi,bj)
          f_temp0smoo = f_temp0smoo + objf_temp0smoo(bi,bj)
          f_salt0smoo = f_salt0smoo + objf_salt0smoo(bi,bj)
          f_etan0 = f_etan0 + objf_etan0(bi,bj)
          f_uvel0 = f_uvel0 + objf_uvel0(bi,bj)
          f_vvel0 = f_vvel0 + objf_vvel0(bi,bj)
          f_tauu = f_tauu + objf_tauu(bi,bj)
          f_tauum  = f_tauum + objf_tauum(bi,bj)
          f_tauusmoo = f_tauusmoo + objf_tauusmoo(bi,bj)
          f_tauv = f_tauv + objf_tauv(bi,bj)
          f_tauvm  = f_tauvm + objf_tauvm(bi,bj)
          f_tauvsmoo = f_tauvsmoo + objf_tauvsmoo(bi,bj)
          f_hflux = f_hflux + objf_hflux(bi,bj)
          f_hfluxsmoo = f_hfluxsmoo + objf_hfluxsmoo(bi,bj)
          f_sflux = f_sflux + objf_sflux(bi,bj)
          f_sfluxsmoo = f_sfluxsmoo + objf_sfluxsmoo(bi,bj)
          f_uwind  = f_uwind + objf_uwind(bi,bj)
          f_vwind  = f_vwind + objf_vwind(bi,bj)
          f_atemp  = f_atemp + objf_atemp(bi,bj)
          f_aqh    = f_aqh + objf_aqh(bi,bj)
          f_precip = f_precip + objf_precip(bi,bj)
          f_swflux = f_swflux + objf_swflux(bi,bj)
          f_swdown = f_swdown + objf_swdown(bi,bj)
          f_lwflux = f_lwflux + objf_lwflux(bi,bj)
          f_lwdown = f_lwdown + objf_lwdown(bi,bj)
          f_uwindm   = f_uwindm + objf_uwindm(bi,bj)
          f_vwindm   = f_vwindm + objf_vwindm(bi,bj)
          f_atempm   = f_atempm + objf_atempm(bi,bj)
          f_aqhm     = f_aqhm   + objf_aqhm(bi,bj)
          f_precipm  = f_precipm + objf_precipm(bi,bj)
          f_swfluxm  = f_swfluxm + objf_swfluxm(bi,bj)
          f_swdownm  = f_swdownm + objf_swdownm(bi,bj)
          f_lwfluxm  = f_lwfluxm + objf_lwfluxm(bi,bj)
          f_lwdownm  = f_lwdownm + objf_lwdownm(bi,bj)
          f_uwindsmoo  = f_uwindsmoo + objf_uwindsmoo(bi,bj)
          f_vwindsmoo  = f_vwindsmoo + objf_vwindsmoo(bi,bj)
          f_atempsmoo  = f_atempsmoo + objf_atempsmoo(bi,bj)
          f_aqhsmoo    = f_aqhsmoo + objf_aqhsmoo(bi,bj)
          f_precipsmoo = f_precipsmoo + objf_precipsmoo(bi,bj)
          f_swfluxsmoo = f_swfluxsmoo + objf_swfluxsmoo(bi,bj)
          f_swdownsmoo = f_swdownsmoo + objf_swdownsmoo(bi,bj)
          f_lwfluxsmoo = f_lwfluxsmoo + objf_lwfluxsmoo(bi,bj)
          f_lwdownsmoo = f_lwdownsmoo + objf_lwdownsmoo(bi,bj)
          f_ssh  = f_ssh + objf_h(bi,bj)
          f_tp   = f_tp  + objf_tp(bi,bj)
          f_ers  = f_ers + objf_ers(bi,bj)
          f_gfo  = f_gfo + objf_gfo(bi,bj)
          f_sst  = f_sst + objf_sst(bi,bj)
          f_tmi  = f_tmi + objf_tmi(bi,bj)
          f_sss  = f_sss + objf_sss(bi,bj)
          f_bp   = f_bp  + objf_bp(bi,bj)
          f_ies  = f_ies + objf_ies(bi,bj)
#if ( defined (ALLOW_COST) && defined (ALLOW_COST_ATLANTIC) )
          f_atl  = f_atl + objf_atl(bi,bj)
#endif
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
          f_kapgm = f_kapgm + objf_kapgm(bi,bj)
          f_kapredi = f_kapredi + objf_kapredi(bi,bj)
          f_diffkr = f_diffkr + objf_diffkr(bi,bj)
          f_ini_fin = f_ini_fin +
     &         objf_theta_ini_fin(bi,bj) + objf_salt_ini_fin(bi,bj)
          f_eddytau = f_eddytau + objf_eddytau(bi,bj)
          f_bottomdrag = f_bottomdrag + objf_bottomdrag(bi,bj)
#endif /* ECCO_CTRL_DEPRECATED */

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
          f_obcsn  = f_obcsn + objf_obcsn(bi,bj)
          f_obcss  = f_obcss + objf_obcss(bi,bj)
          f_obcsw  = f_obcsw + objf_obcsw(bi,bj)
          f_obcse  = f_obcse + objf_obcse(bi,bj)
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
          f_ageos  = f_ageos + objf_ageos(bi,bj)
# endif
#endif
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
          f_profiles(num_file,num_var)=f_profiles(num_file,num_var)
     &            +objf_profiles(num_file,num_var,bi,bj)
       enddo
      enddo
      do num_var=1,NVARMAX
         f_profiles_mean(num_var)=f_profiles_mean(num_var)
     &           +objf_profiles_mean(num_var,bi,bj)
      enddo
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
          f_gencost(num_var)=f_gencost(num_var)
     &            +objf_gencost(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
          f_gentim2d(num_var) = f_gentim2d(num_var)
     &            +objf_gentim2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
          f_genarr2d(num_var) = f_genarr2d(num_var)
     &            +objf_genarr2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
          f_genarr3d(num_var) = f_genarr3d(num_var)
     &            +objf_genarr3d(bi,bj,num_var)
       enddo
#endif

#ifdef ECCO_CTRL_DEPRECATED
          no_temp = no_temp + num_temp(bi,bj)
          no_salt = no_salt + num_salt(bi,bj)
          no_temp0 = no_temp0 + num_temp0(bi,bj)
          no_salt0 = no_salt0 + num_salt0(bi,bj)
          no_etan0 = no_etan0 + num_etan0(bi,bj)
          no_uvel0 = no_uvel0 + num_uvel0(bi,bj)
          no_vvel0 = no_vvel0 + num_vvel0(bi,bj)
          no_tauu = no_tauu + num_tauu(bi,bj)
          no_tauum  = no_tauum + num_tauum(bi,bj)
          no_tauv = no_tauv + num_tauv(bi,bj)
          no_tauvm  = no_tauvm + num_tauvm(bi,bj)
          no_hflux= no_hflux + num_hflux(bi,bj)
          no_hfluxmm = no_hfluxmm + num_hfluxmm(bi,bj)
          no_sflux= no_sflux + num_sflux(bi,bj)
          no_sfluxmm = no_sfluxmm + num_sfluxmm(bi,bj)
          no_atemp  = no_atemp + num_atemp(bi,bj)
          no_aqh    = no_aqh + num_aqh(bi,bj)
          no_precip = no_precip + num_precip(bi,bj)
          no_swflux = no_swflux + num_swflux(bi,bj)
          no_swdown = no_swdown + num_swdown(bi,bj)
          no_lwflux = no_lwflux + num_lwflux(bi,bj)
          no_lwdown = no_lwdown + num_lwdown(bi,bj)
          no_uwind  = no_uwind + num_uwind(bi,bj)
          no_vwind  = no_vwind + num_vwind(bi,bj)
          no_atempm  = no_atempm + num_atempm(bi,bj)
          no_aqhm    = no_aqhm + num_aqhm(bi,bj)
          no_precipm = no_precipm + num_precipm(bi,bj)
          no_swfluxm = no_swfluxm + num_swfluxm(bi,bj)
          no_swdownm = no_swdownm + num_swdownm(bi,bj)
          no_lwfluxm = no_lwfluxm + num_lwfluxm(bi,bj)
          no_lwdownm = no_lwdownm + num_lwdownm(bi,bj)
          no_uwindm  = no_uwindm + num_uwindm(bi,bj)
          no_vwindm  = no_vwindm + num_vwindm(bi,bj)
          no_ssh  = no_ssh + num_h(bi,bj)
          no_tp   = no_tp  + num_tp(bi,bj)
          no_ers  = no_ers + num_ers(bi,bj)
          no_gfo  = no_gfo + num_gfo(bi,bj)
          no_sst  = no_sst + num_sst(bi,bj)
          no_tmi  = no_tmi + num_tmi(bi,bj)
          no_sss  = no_sss + num_sss(bi,bj)
          no_bp   = no_bp  + num_bp(bi,bj)
          no_ies  = no_ies + num_ies(bi,bj)
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
          no_kapgm = no_kapgm + num_kapgm(bi,bj)
          no_kapredi = no_kapredi + num_kapredi(bi,bj)
          no_diffkr = no_diffkr + num_diffkr(bi,bj)
          no_ini_fin = no_ini_fin +
     &         num_theta_ini_fin(bi,bj) + num_salt_ini_fin(bi,bj)
          no_eddytau = no_eddytau + num_eddytau(bi,bj)
          no_bottomdrag = no_bottomdrag + num_bottomdrag(bi,bj)

#endif /* ECCO_CTRL_DEPRECATED */

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
          no_obcsn = no_obcsn + num_obcsn(bi,bj)
          no_obcss = no_obcss + num_obcss(bi,bj)
          no_obcse = no_obcse + num_obcse(bi,bj)
          no_obcsw = no_obcsw + num_obcsw(bi,bj)
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
          no_ageos = no_ageos + num_ageos(bi,bj)
# endif
#endif
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
          no_profiles(num_file,num_var)=no_profiles(num_file,num_var)
     &            +num_profiles(num_file,num_var,bi,bj)
       enddo
      enddo
      do num_var=1,NVARMAX
         no_profiles_mean(num_var)=no_profiles_mean(num_var)
     &           +num_profiles_mean(num_var,bi,bj)
      enddo
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
          no_gencost(num_var)=no_gencost(num_var)
     &            +num_gencost(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
          no_gentim2d(num_var) = no_gentim2d(num_var)
     &            +num_gentim2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
          no_genarr2d(num_var) = no_genarr2d(num_var)
     &            +num_genarr2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
          no_genarr3d(num_var) = no_genarr3d(num_var)
     &            +num_genarr3d(bi,bj,num_var)
       enddo
#endif

C-   end bi,bj loops
       ENDDO
      ENDDO

c local copy used in print statements, for
c which we always want to do the global sum.
      CALL GLOBAL_SUM_TILE_RL( tile_fc, locfc, myThid )

#ifndef ALLOW_COST
cgf global sum is now done in cost_final if allow_cost
c--   Do global summation.
      _GLOBAL_SUM_RL( fc , myThid )
#endif

c--   Do global summation for each part of the cost function

#ifdef ECCO_CTRL_DEPRECATED
      _GLOBAL_SUM_RL( f_temp , myThid )
      _GLOBAL_SUM_RL( f_salt , myThid )
      _GLOBAL_SUM_RL( f_temp0, myThid )
      _GLOBAL_SUM_RL( f_salt0, myThid )
      _GLOBAL_SUM_RL( f_temp0smoo, myThid )
      _GLOBAL_SUM_RL( f_salt0smoo, myThid )
      _GLOBAL_SUM_RL( f_etan0, myThid )
      _GLOBAL_SUM_RL( f_uvel0, myThid )
      _GLOBAL_SUM_RL( f_vvel0, myThid )
      _GLOBAL_SUM_RL( f_tauu , myThid )
      _GLOBAL_SUM_RL( f_tauum , myThid )
      _GLOBAL_SUM_RL( f_tauusmoo , myThid )
      _GLOBAL_SUM_RL( f_tauv , myThid )
      _GLOBAL_SUM_RL( f_tauvm , myThid )
      _GLOBAL_SUM_RL( f_tauvsmoo , myThid )
      _GLOBAL_SUM_RL( f_hflux , myThid )
      _GLOBAL_SUM_RL( f_hfluxmm , myThid )
      _GLOBAL_SUM_RL( f_hfluxsmoo , myThid )
      _GLOBAL_SUM_RL( f_sflux , myThid )
      _GLOBAL_SUM_RL( f_sfluxsmoo , myThid )
      _GLOBAL_SUM_RL( f_uwind , myThid )
      _GLOBAL_SUM_RL( f_vwind , myThid )
      _GLOBAL_SUM_RL( f_atemp , myThid )
      _GLOBAL_SUM_RL( f_aqh , myThid )
      _GLOBAL_SUM_RL( f_precip , myThid )
      _GLOBAL_SUM_RL( f_swflux , myThid )
      _GLOBAL_SUM_RL( f_swdown , myThid )
      _GLOBAL_SUM_RL( f_lwflux , myThid )
      _GLOBAL_SUM_RL( f_lwdown , myThid )
      _GLOBAL_SUM_RL( f_uwindm , myThid )
      _GLOBAL_SUM_RL( f_vwindm , myThid )
      _GLOBAL_SUM_RL( f_atempm , myThid )
      _GLOBAL_SUM_RL( f_aqhm   , myThid )
      _GLOBAL_SUM_RL( f_precipm , myThid )
      _GLOBAL_SUM_RL( f_swfluxm , myThid )
      _GLOBAL_SUM_RL( f_swdownm , myThid )
      _GLOBAL_SUM_RL( f_lwfluxm , myThid )
      _GLOBAL_SUM_RL( f_lwdownm , myThid )
      _GLOBAL_SUM_RL( f_uwindsmoo , myThid )
      _GLOBAL_SUM_RL( f_vwindsmoo , myThid )
      _GLOBAL_SUM_RL( f_atempsmoo , myThid )
      _GLOBAL_SUM_RL( f_aqhsmoo , myThid )
      _GLOBAL_SUM_RL( f_precipsmoo , myThid )
      _GLOBAL_SUM_RL( f_swfluxsmoo , myThid )
      _GLOBAL_SUM_RL( f_swdownsmoo , myThid )
      _GLOBAL_SUM_RL( f_lwfluxsmoo , myThid )
      _GLOBAL_SUM_RL( f_lwdownsmoo , myThid )
      _GLOBAL_SUM_RL( f_ssh , myThid )
      _GLOBAL_SUM_RL( f_tp , myThid )
      _GLOBAL_SUM_RL( f_ers , myThid )
      _GLOBAL_SUM_RL( f_gfo , myThid )
      _GLOBAL_SUM_RL( f_sst , myThid )
      _GLOBAL_SUM_RL( f_tmi , myThid )
      _GLOBAL_SUM_RL( f_sss , myThid )
      _GLOBAL_SUM_RL( f_bp , myThid )
      _GLOBAL_SUM_RL( f_ies , myThid )
      _GLOBAL_SUM_RL( f_atl , myThid )
      _GLOBAL_SUM_RL( f_ctdt , myThid )
      _GLOBAL_SUM_RL( f_ctds , myThid )
      _GLOBAL_SUM_RL( f_ctdtclim , myThid  )
      _GLOBAL_SUM_RL( f_ctdsclim , myThid  )
      _GLOBAL_SUM_RL( f_xbt , myThid )
      _GLOBAL_SUM_RL( f_argot , myThid )
      _GLOBAL_SUM_RL( f_argos , myThid )
      _GLOBAL_SUM_RL( f_drifter , myThid )
      _GLOBAL_SUM_RL( f_sdrift , myThid )
      _GLOBAL_SUM_RL( f_tdrift , myThid )
      _GLOBAL_SUM_RL( f_wdrift , myThid )
      _GLOBAL_SUM_RL( f_scatx , myThid )
      _GLOBAL_SUM_RL( f_scaty , myThid )
      _GLOBAL_SUM_RL( f_scatxm , myThid )
      _GLOBAL_SUM_RL( f_scatym , myThid )
      _GLOBAL_SUM_RL( f_curmtr , myThid )
      _GLOBAL_SUM_RL( f_kapgm , myThid )
      _GLOBAL_SUM_RL( f_kapredi, myThid )
      _GLOBAL_SUM_RL( f_diffkr , myThid )
      _GLOBAL_SUM_RL( f_ini_fin , myThid )
      _GLOBAL_SUM_RL( f_eddytau , myThid )
      _GLOBAL_SUM_RL( f_bottomdrag , myThid )
#endif /* ECCO_CTRL_DEPRECATED */

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      _GLOBAL_SUM_RL( f_obcsn , myThid )
      _GLOBAL_SUM_RL( f_obcss , myThid )
      _GLOBAL_SUM_RL( f_obcsw , myThid )
      _GLOBAL_SUM_RL( f_obcse , myThid )
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
      _GLOBAL_SUM_RL( f_ageos , myThid )
# endif
#endif
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        _GLOBAL_SUM_RL(f_profiles(num_file,num_var), myThid )
       enddo
      enddo
      do num_var=1,NVARMAX
       _GLOBAL_SUM_RL(f_profiles_mean(num_var), myThid )
      enddo
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        _GLOBAL_SUM_RL(f_gencost(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        _GLOBAL_SUM_RL(f_gentim2d(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        _GLOBAL_SUM_RL(f_genarr2d(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        _GLOBAL_SUM_RL(f_genarr3d(num_var), myThid )
       enddo
#endif

#ifdef ECCO_CTRL_DEPRECATED
      _GLOBAL_SUM_RL( no_temp , myThid )
      _GLOBAL_SUM_RL( no_salt , myThid )
      _GLOBAL_SUM_RL( no_temp0, myThid )
      _GLOBAL_SUM_RL( no_salt0, myThid )
      _GLOBAL_SUM_RL( no_etan0, myThid )
      _GLOBAL_SUM_RL( no_uvel0, myThid )
      _GLOBAL_SUM_RL( no_vvel0, myThid )
      _GLOBAL_SUM_RL( no_tauu , myThid )
      _GLOBAL_SUM_RL( no_tauum , myThid )
      _GLOBAL_SUM_RL( no_tauv , myThid )
      _GLOBAL_SUM_RL( no_tauvm , myThid )
      _GLOBAL_SUM_RL( no_hflux , myThid )
      _GLOBAL_SUM_RL( no_hfluxmm , myThid )
      _GLOBAL_SUM_RL( no_sflux , myThid )
      _GLOBAL_SUM_RL( no_sfluxmm , myThid )
      _GLOBAL_SUM_RL( no_uwind , myThid )
      _GLOBAL_SUM_RL( no_vwind , myThid )
      _GLOBAL_SUM_RL( no_atemp , myThid )
      _GLOBAL_SUM_RL( no_aqh , myThid )
      _GLOBAL_SUM_RL( no_precip , myThid )
      _GLOBAL_SUM_RL( no_swflux , myThid )
      _GLOBAL_SUM_RL( no_swdown , myThid )
      _GLOBAL_SUM_RL( no_lwflux , myThid )
      _GLOBAL_SUM_RL( no_lwdown , myThid )
      _GLOBAL_SUM_RL( no_uwindm , myThid )
      _GLOBAL_SUM_RL( no_vwindm , myThid )
      _GLOBAL_SUM_RL( no_atempm , myThid )
      _GLOBAL_SUM_RL( no_aqhm   , myThid )
      _GLOBAL_SUM_RL( no_precipm , myThid )
      _GLOBAL_SUM_RL( no_swfluxm , myThid )
      _GLOBAL_SUM_RL( no_swdownm , myThid )
      _GLOBAL_SUM_RL( no_lwfluxm , myThid )
      _GLOBAL_SUM_RL( no_lwdownm , myThid )
      _GLOBAL_SUM_RL( no_ssh , myThid )
      _GLOBAL_SUM_RL( no_tp , myThid )
      _GLOBAL_SUM_RL( no_ers , myThid )
      _GLOBAL_SUM_RL( no_gfo , myThid )
      _GLOBAL_SUM_RL( no_sst , myThid )
      _GLOBAL_SUM_RL( no_tmi , myThid )
      _GLOBAL_SUM_RL( no_sss , myThid )
      _GLOBAL_SUM_RL( no_bp , myThid )
      _GLOBAL_SUM_RL( no_ies , myThid )
      _GLOBAL_SUM_RL( no_atl , myThid )
      _GLOBAL_SUM_RL( no_ctdt , myThid )
      _GLOBAL_SUM_RL( no_ctds , myThid )
      _GLOBAL_SUM_RL( no_ctdtclim , myThid  )
      _GLOBAL_SUM_RL( no_ctdsclim , myThid  )
      _GLOBAL_SUM_RL( no_xbt , myThid )
      _GLOBAL_SUM_RL( no_argot , myThid )
      _GLOBAL_SUM_RL( no_argos , myThid )
      _GLOBAL_SUM_RL( no_drifter , myThid )
      _GLOBAL_SUM_RL( no_sdrift , myThid )
      _GLOBAL_SUM_RL( no_tdrift , myThid )
      _GLOBAL_SUM_RL( no_wdrift , myThid )
      _GLOBAL_SUM_RL( no_scatx , myThid )
      _GLOBAL_SUM_RL( no_scaty , myThid )
      _GLOBAL_SUM_RL( no_scatxm , myThid )
      _GLOBAL_SUM_RL( no_scatym , myThid )
      _GLOBAL_SUM_RL( no_curmtr , myThid )
      _GLOBAL_SUM_RL( no_kapgm , myThid )
      _GLOBAL_SUM_RL( no_kapredi , myThid )
      _GLOBAL_SUM_RL( no_diffkr , myThid )
      _GLOBAL_SUM_RL( no_ini_fin , myThid )
      _GLOBAL_SUM_RL( no_eddytau , myThid )
      _GLOBAL_SUM_RL( no_bottomdrag , myThid )
#endif /* ECCO_CTRL_DEPRECATED */

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      _GLOBAL_SUM_RL( no_obcsn , myThid )
      _GLOBAL_SUM_RL( no_obcss , myThid )
      _GLOBAL_SUM_RL( no_obcsw , myThid )
      _GLOBAL_SUM_RL( no_obcse , myThid )
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
      _GLOBAL_SUM_RL( no_ageos , myThid )
# endif
#endif
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        _GLOBAL_SUM_RL(no_profiles(num_file,num_var), myThid )
       enddo
      enddo
      do num_var=1,NVARMAX
       _GLOBAL_SUM_RL(no_profiles_mean(num_var), myThid )
      enddo
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        _GLOBAL_SUM_RL(no_gencost(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        _GLOBAL_SUM_RL(no_gentim2d(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        _GLOBAL_SUM_RL(no_genarr2d(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        _GLOBAL_SUM_RL(no_genarr3d(num_var), myThid )
       enddo
#endif

#ifdef ECCO_CTRL_DEPRECATED
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
     &     ' --> f_etan0   =',f_etan0
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_uvel0   =',f_uvel0
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_vvel0   =',f_vvel0
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_sst     =',f_sst
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_tmi     =',f_tmi
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_sss     =',f_sss
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_bp      =',f_bp
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ies      =',f_ies
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ssh       =',f_ssh
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_tp        =',f_tp
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ers       =',f_ers
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_gfo       =',f_gfo
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
     &     ' --> f_hfluxmm =',f_hfluxmm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_hfluxsmoo =',f_hfluxsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_sflux   =',f_sflux
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
     &     ' --> f_lwflux  =',f_lwflux
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_lwdown  =',f_lwdown
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
     &     ' --> f_lwfluxm  =',f_lwfluxm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_swdownm  =',f_swdownm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_lwdownm  =',f_lwdownm
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
     &     ' --> f_lwfluxsmoo  =',f_lwfluxsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_swdownsmoo  =',f_swdownsmoo
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_lwdownsmoo  =',f_lwdownsmoo
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
     &     ' --> f_curmtr  =',f_curmtr
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_kapgm   =',f_kapgm
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_kapredi   =',f_kapredi
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_diffkr  =',f_diffkr
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_eddytau =', f_eddytau
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_bottomdrag =', f_bottomdrag
#endif /* ECCO_CTRL_DEPRECATED */

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcsn   =',f_obcsn
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcss   =',f_obcss
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcsw   =',f_obcsw
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcse   =',f_obcse
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ageos   =',f_ageos
# endif
#endif
#ifdef ALLOW_PROFILES
      if (usePROFILES) then
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        if ( no_profiles(num_file,num_var).GT.zeroRL ) then
         write(msgBuf,'(A,D22.15,i2.0,i2.0)')
     &     ' --> f_profiles =',f_profiles(num_file,num_var),
     &      num_file, num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
       enddo
      enddo
      do num_var=1,NVARMAX
        if ( no_profiles_mean(num_var).GT.zeroRL ) then
         write(msgBuf,'(A,D22.15,i2.0,i2.0)')
     &     ' --> f_profiles_mean =',f_profiles_mean(num_var),
     &      num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
      enddo
      endif
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        if (no_gencost(num_var).GT.0) then
         write(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_gencost =',f_gencost(num_var),
     &      num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        if (no_gentim2d(num_var).GT.0. _d 0) then
         write(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_gentim2d =',f_gentim2d(num_var),
     &      num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        if (no_genarr2d(num_var).GT.0. _d 0) then
         write(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_genarr2d =',f_genarr2d(num_var),
     &      num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        if (no_genarr3d(num_var).GT.0. _d 0) then
         write(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_genarr3d =',f_genarr3d(num_var),
     &      num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
       enddo
#endif

#ifdef ECCO_CTRL_DEPRECATED

c--   Each process has calculated the global part for itself.

        glofc = glofc
     &          + mult_hmean*objf_hmean
        no_hmean = num_hmean

cph(
cph this is from annual mean misfits;
cph simple sums and squares needed to be taken at annual intervals
        f_hfluxmm  = f_hfluxmm + objf_hfluxmm
        f_hfluxmm2 = mult_hfluxmm*f_hfluxmm
c
        f_sfluxmm  = f_sfluxmm + objf_sfluxmm
        f_sfluxmm2 = mult_sfluxmm*f_sfluxmm
c
        f_transp   = mult_transp*objf_transp

        no_transp = num_transp
cph)
        glofc = glofc
     &          + f_hfluxmm2 + f_sfluxmm2
     &          + f_transp

#endif /* ECCO_CTRL_DEPRECATED */

#ifndef ALLOW_COST
cgf this sum is now done in cost_final if allow_cost
        fc = fc + glofc
#endif

      locfc=locfc+glofc

C     only master thread of master CPU open and write to file
      IF ( MASTER_CPU_THREAD(myThid) ) THEN

#ifdef ECCO_CTRL_DEPRECATED
        write(standardmessageunit,'(A,D22.15)')
     &           ' --> f_hfluxmm2           =',f_hfluxmm2
        write(standardmessageunit,'(A,D22.15)')
     &           ' --> f_sfluxmm2           =',f_sfluxmm2
        write(standardmessageunit,'(A,D22.15)')
     &           ' --> f_transp             =',f_transp
        write(standardmessageunit,'(A,D22.15)')
     &           ' --> objf_hmean           =',objf_hmean
#endif /* ECCO_CTRL_DEPRECATED */
        write(msgBuf,'(A,D22.15)')
     &           ' --> fc               =', locfc
        call print_message( msgBuf, standardmessageunit,
     &                      SQUEEZE_RIGHT, myThid )

        write(cfname,'(A,i4.4)') 'costfunction',eccoiter
        open(unit=ifc,file=cfname)

#ifdef ALLOW_ECCO_OLD_FC_PRINT
        write(ifc,*)
#else
        write(ifc,'(A,2D22.15)')
#endif
     &       'fc =', locfc, 0.
#ifdef ECCO_CTRL_DEPRECATED
        write(ifc,'(A,2D22.15)')
     &       'f_temp =', f_temp, no_temp
        write(ifc,'(A,2D22.15)')
     &       'f_salt =', f_salt, no_salt
        write(ifc,'(A,2D22.15)')
     &       'f_temp0 =', f_temp0, no_temp0
        write(ifc,'(A,2D22.15)')
     &       'f_salt0 =', f_salt0, no_salt0
        write(ifc,'(A,2D22.15)')
     &       'f_temp0smoo =', f_temp0smoo, no_temp0
        write(ifc,'(A,2D22.15)')
     &       'f_salt0smoo =', f_salt0smoo, no_salt0
        write(ifc,'(A,2D22.15)')
     &       'f_etan0 =', f_etan0, no_etan0
        write(ifc,'(A,2D22.15)')
     &       'f_uvel0 =', f_uvel0, no_uvel0
        write(ifc,'(A,2D22.15)')
     &       'f_vvel0 =', f_vvel0, no_vvel0
        write(ifc,'(A,2D22.15)')
     &       'f_tauu =', f_tauu, no_tauu
        write(ifc,'(A,2D22.15)')
     &       'f_tauum =', f_tauum, no_tauum
        write(ifc,'(A,2D22.15)')
     &       'f_tauusmoo =', f_tauusmoo, no_tauu
        write(ifc,'(A,2D22.15)')
     &       'f_tauv =', f_tauv, no_tauv
        write(ifc,'(A,2D22.15)')
     &       'f_tauvm =', f_tauvm, no_tauvm
        write(ifc,'(A,2D22.15)')
     &       'f_tauvsmoo =', f_tauvsmoo, no_tauv
        write(ifc,'(A,2D22.15)')
     &       'f_hflux =', f_hflux, no_hflux
        write(ifc,'(A,2D22.15)')
     &       'f_hfluxm =', f_hfluxm, no_hfluxm
        write(ifc,'(A,2D22.15)')
     &       'f_hfluxmm =', f_hfluxmm, no_hfluxmm
        write(ifc,'(A,2D22.15)')
     &       'f_hfluxmm2 =', f_hfluxmm2, mult_hfluxmm
        write(ifc,'(A,2D22.15)')
     &       'f_hfluxsmoo =', f_hfluxsmoo, no_hflux
        write(ifc,'(A,2D22.15)')
     &       'f_sflux =', f_sflux, no_sflux
        write(ifc,'(A,2D22.15)')
     &       'f_sfluxm =', f_sfluxm, no_sfluxm
        write(ifc,'(A,2D22.15)')
     &       'f_sfluxmm =', f_sfluxmm, no_sfluxmm
        write(ifc,'(A,2D22.15)')
     &       'f_sfluxmm2 =', f_sfluxmm2, mult_sfluxmm
        write(ifc,'(A,2D22.15)')
     &       'f_sfluxsmoo =', f_sfluxsmoo, no_sflux
        write(ifc,'(A,2D22.15)')
     &       'f_uwind  =', f_uwind, no_uwind
        write(ifc,'(A,2D22.15)')
     &       'f_vwind  =', f_vwind, no_vwind
        write(ifc,'(A,2D22.15)')
     &       'f_atemp  =', f_atemp, no_atemp
        write(ifc,'(A,2D22.15)')
     &       'f_aqh    =', f_aqh, no_aqh
        write(ifc,'(A,2D22.15)')
     &       'f_precip =', f_precip, no_precip
        write(ifc,'(A,2D22.15)')
     &       'f_swflux =', f_swflux, no_swflux
        write(ifc,'(A,2D22.15)')
     &       'f_lwflux =', f_lwflux, no_lwflux
        write(ifc,'(A,2D22.15)')
     &       'f_swdown =', f_swdown, no_swdown
        write(ifc,'(A,2D22.15)')
     &       'f_lwdown =', f_lwdown, no_lwdown
        write(ifc,'(A,2D22.15)')
     &       'f_uwindm   =', f_uwindm, no_uwindm
        write(ifc,'(A,2D22.15)')
     &       'f_vwindm   =', f_vwindm, no_vwindm
        write(ifc,'(A,2D22.15)')
     &       'f_atempm   =', f_atempm, no_atempm
        write(ifc,'(A,2D22.15)')
     &       'f_aqhm     =', f_aqhm, no_aqhm
        write(ifc,'(A,2D22.15)')
     &       'f_precipm  =', f_precipm, no_precipm
        write(ifc,'(A,2D22.15)')
     &       'f_swfluxm  =', f_swfluxm, no_swfluxm
        write(ifc,'(A,2D22.15)')
     &       'f_lwfluxm  =', f_lwfluxm, no_lwfluxm
        write(ifc,'(A,2D22.15)')
     &       'f_swdownm  =', f_swdownm, no_swdownm
        write(ifc,'(A,2D22.15)')
     &       'f_lwdownm  =', f_lwdownm, no_lwdownm
        write(ifc,'(A,2D22.15)')
     &       'f_uwindsmoo  =', f_uwindsmoo, no_uwind
        write(ifc,'(A,2D22.15)')
     &       'f_vwindsmoo  =', f_vwindsmoo, no_vwind
        write(ifc,'(A,2D22.15)')
     &       'f_atempsmoo  =', f_atempsmoo, no_atemp
        write(ifc,'(A,2D22.15)')
     &       'f_aqhsmoo    =', f_aqhsmoo, no_aqh
        write(ifc,'(A,2D22.15)')
     &       'f_precipsmoo =', f_precipsmoo, no_precip
        write(ifc,'(A,2D22.15)')
     &       'f_swfluxsmoo =', f_swfluxsmoo, no_swflux
        write(ifc,'(A,2D22.15)')
     &       'f_lwfluxsmoo =', f_lwfluxsmoo, no_lwflux
        write(ifc,'(A,2D22.15)')
     &       'f_swdownsmoo =', f_swdownsmoo, no_swdown
        write(ifc,'(A,2D22.15)')
     &       'f_lwdownsmoo =', f_lwdownsmoo, no_lwdown
        write(ifc,'(A,2D22.15)')
     &       'f_ssh =', f_ssh, no_ssh
        write(ifc,'(A,2D22.15)')
     &       'f_tp  =', f_tp, no_tp
        write(ifc,'(A,2D22.15)')
     &       'f_ers =', f_ers, no_ers
        write(ifc,'(A,2D22.15)')
     &       'f_gfo =', f_gfo, no_gfo
        write(ifc,'(A,2D22.15)')
     &       'f_sst =', f_sst, no_sst
        write(ifc,'(A,2D22.15)')
     &       'f_tmi =', f_tmi, no_tmi
        write(ifc,'(A,2D22.15)')
     &       'f_sss =', f_sss, no_sss
        write(ifc,'(A,2D22.15)')
     &       'f_bp =', f_bp, no_bp
        write(ifc,'(A,2D22.15)')
     &       'f_ies =', f_ies, no_ies
        write(ifc,'(A,2D22.15)')
     &       'f_atl =', f_atl, no_atl
        write(ifc,'(A,2D22.15)')
     &       'f_ctdt =', f_ctdt, no_ctdt
        write(ifc,'(A,2D22.15)')
     &       'f_ctds =', f_ctds, no_ctds
        write(ifc,'(A,2D22.15)')
     &       'f_ctdtclim =', f_ctdtclim, no_ctdtclim
        write(ifc,'(A,2D22.15)')
     &       'f_ctdsclim =', f_ctdsclim, no_ctdsclim
        write(ifc,'(A,2D22.15)')
     &       'f_xbt =', f_xbt, no_xbt
        write(ifc,'(A,2D22.15)')
     &       'f_argot =', f_argot, no_argot
        write(ifc,'(A,2D22.15)')
     &       'f_argos =', f_argos, no_argos
        write(ifc,'(A,2D22.15)')
     &       'objf_hmean =', objf_hmean, no_hmean
        write(ifc,'(A,2D22.15)')
     &       'f_drifter =', f_drifter, no_drifter
        write(ifc,'(A,2D22.15)')
     &       'f_sdrift =', f_sdrift, no_sdrift
        write(ifc,'(A,2D22.15)')
     &       'f_tdrift =', f_tdrift, no_tdrift
        write(ifc,'(A,2D22.15)')
     &       'f_wdrift =', f_wdrift, no_wdrift
        write(ifc,'(A,2D22.15)')
     &       'f_scatx =', f_scatx, no_scatx
        write(ifc,'(A,2D22.15)')
     &       'f_scaty =', f_scaty, no_scaty
        write(ifc,'(A,2D22.15)')
     &       'f_scatxm =', f_scatxm, no_scatxm
        write(ifc,'(A,2D22.15)')
     &       'f_scatym =', f_scatym, no_scatym
        write(ifc,'(A,2D22.15)')
     &       'f_kapgm =', f_kapgm, no_kapgm
        write(ifc,'(A,2D22.15)')
     &       'f_kapredi =', f_kapredi, no_kapredi
        write(ifc,'(A,2D22.15)')
     &       'f_diffkr =', f_diffkr, no_diffkr
        write(ifc,'(A,2D22.15)')
     &       'f_ini_fin =', f_ini_fin, no_ini_fin
        write(ifc,'(A,2D22.15)')
     &       'f_eddytau =', f_eddytau, no_eddytau
        write(ifc,'(A,2D22.15)')
     &       'f_bottomdrag =', f_bottomdrag, no_bottomdrag
        write(ifc,'(A,2D22.15)')
     &       'f_transp =', f_transp, no_transp
#endif /* ECCO_CTRL_DEPRECATED */

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
        write(ifc,'(A,2D22.15)')
     &       'f_obcsn =', f_obcsn, no_obcsn
        write(ifc,'(A,2D22.15)')
     &       'f_obcss =', f_obcss, no_obcss
        write(ifc,'(A,2D22.15)')
     &       'f_obcsw =', f_obcsw, no_obcsw
        write(ifc,'(A,2D22.15)')
     &       'f_obcse =', f_obcse, no_obcse
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
        write(ifc,'(A,2D22.15)')
     &       'f_ageos =', f_ageos, no_ageos
# endif
#endif
#ifdef ALLOW_PROFILES
      if (usePROFILES) then
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        if ( no_profiles(num_file,num_var).GT.zeroRL ) then
         IL  = ILNBLNK( profilesfiles(num_file) )
         IL  = max (IL,30)
         write(ifc,'(4A,2D22.15)')
     &    profilesfiles(num_file)(1:IL),' ',
     &    prof_names(num_file,num_var), ' = ',
     &    f_profiles(num_file,num_var),
     &    no_profiles(num_file,num_var)
        endif
       enddo
      enddo
      do num_var=1,NVARMAX
        if ( no_profiles_mean(num_var).GT.zeroRL ) then
         write(ifc,'(3A,2D22.15)')
     &    'profile_mean ',
     &    prof_names(1,num_var), ' = ',
     &    f_profiles_mean(num_var),
     &    no_profiles_mean(num_var)
        endif
      enddo
      endif
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        if (no_gencost(num_var).GT.0) then
         IL  = ILNBLNK( gencost_name(num_var) )
         IL  = max (IL,15)
         write(ifc,'(2A,i2.0,A,2D22.15)')
     &    gencost_name(num_var)(1:IL),' (gencost ', num_var, ') = ',
     &    f_gencost(num_var),
     &    no_gencost(num_var)
         endif
       enddo
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        if (no_gentim2d(num_var).GT.0. _d 0) then
         IL  = ILNBLNK( xx_gentim2d_file(num_var) )
         IL  = max (IL,15)
         write(ifc,'(2A,i2.0,A,2D22.15)')
     &    xx_gentim2d_file(num_var)(1:IL),
     &    ' (gentim2d ', num_var, ') = ',
     &    f_gentim2d(num_var),
     &    no_gentim2d(num_var)
         endif
       enddo
#endif

#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        if (no_genarr2d(num_var).GT.0. _d 0) then
         IL  = ILNBLNK( xx_genarr2d_file(num_var) )
         IL  = max (IL,15)
         write(ifc,'(2A,i2.0,A,2D22.15)')
     &    xx_genarr2d_file(num_var)(1:IL),
     &    ' (genarr2d ', num_var, ') = ',
     &    f_genarr2d(num_var),
     &    no_genarr2d(num_var)
         endif
       enddo
#endif

#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        if (no_genarr3d(num_var).GT.0. _d 0) then
         IL  = ILNBLNK( xx_genarr3d_file(num_var) )
         IL  = max (IL,15)
         write(ifc,'(2A,i2.0,A,2D22.15)')
     &    xx_genarr3d_file(num_var)(1:IL),
     &    ' (genarr3d ', num_var, ') = ',
     &    f_genarr3d(num_var),
     &    no_genarr3d(num_var)
         endif
       enddo
#endif

        close(ifc)

      ENDIF

c#ifdef ECCO_CTRL_DEPRECATED
c      call cost_trans_merid( mythid )
c      call cost_trans_zonal( mythid )
c#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ECCO_VERBOSE
      write(msgBuf,'(a,D22.15)')
     &  ' cost_Final: final cost function = ',locfc
      call print_message( msgBuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgBuf,'(a)') ' '
      call print_message( msgBuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgBuf,'(a)')
     &  '             cost function evaluation finished.'
      call print_message( msgBuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgBuf,'(a)') ' '
      call print_message( msgBuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

      return
      end
