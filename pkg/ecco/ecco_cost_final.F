C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_cost_final.F,v 1.6 2005/02/28 17:33:02 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine ecco_cost_final( mythid )

c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================
c
c     o Sum of all cost function contributions.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     added sea-ice term: menemenlis@jpl.nasa.gov 26-Feb-2003
c
c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "cost.h"
#include "ecco_cost.h"
#include "ctrl.h"
#include "optim.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer ifc

      _RL f_temp0, f_salt0,  f_temp, f_salt 
      _RL f_tauu, f_tauv, f_hflux, f_sflux 
      _RL f_tauum, f_tauvm, f_hfluxm, f_sfluxm 
      _RL f_hfluxmm, f_sfluxmm 
      _RL f_sst, f_tmi, f_sss, f_atl, f_ctdt, f_ctds
      _RL f_drifter, f_xbt, f_tdrift, f_sdrift, f_wdrift
      _RL f_argot, f_argos, f_ssh ,f_ctdtclim, f_ctdsclim
      _RL f_scatx, f_scaty, f_scatxm, f_scatym
      _RL f_obcsn, f_obcss, f_obcsw, f_obcse
      _RL f_ageos, f_curmtr
      _RL f_ice
      _RL f_ini_fin
      _RL f_kapgm, f_diffkr
      _RL f_eddytau

      character*20 cfname
#ifdef ECCO_VERBOSE
      character*(MAX_LEN_MBUF) msgbuf
#endif

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
      f_tauu = 0. _d 0 
      f_tauum = 0. _d 0 
      f_tauv = 0. _d 0 
      f_tauvm = 0. _d 0 
      f_hflux = 0. _d 0 
      f_hfluxm = 0. _d 0 
      f_hfluxmm = 0. _d 0 
      f_sflux  = 0. _d 0
      f_sfluxm  = 0. _d 0
      f_sfluxmm  = 0. _d 0
      f_ssh = 0. _d 0
      f_sst = 0. _d 0
      f_tmi = 0. _d 0
      f_sss = 0. _d 0
      f_atl = 0. _d 0
      f_ctdt = 0. _d 0 
      f_ctds = 0. _d 0
      f_ctdtclim = 0.D0 
      f_ctdsclim = 0.D0
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
      f_ice     = 0. _d 0
      f_ini_fin = 0. _d 0
      f_kapgm   = 0. _d 0
      f_diffkr  = 0. _d 0
      f_eddytau = 0. _d 0

c--   Sum up all contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi

          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_temp(bi,bj)    =',objf_temp(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_salt(bi,bj)    =',objf_salt(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_temp0(bi,bj)   =',objf_temp0(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_salt0(bi,bj)   =',objf_salt0(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_sst(bi,bj)     =',objf_sst(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_tmi(bi,bj)     =',objf_tmi(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_sss(bi,bj)     =',objf_sss(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_h(bi,bj)       =',objf_h(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_hmean          =',objf_hmean
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_tauu(bi,bj)    =',objf_tauu(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_tauum(bi,bj)   =',objf_tauum(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_tauv(bi,bj)    =',objf_tauv(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_tauvm(bi,bj)   =',objf_tauvm(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_hflux(bi,bj)   =',objf_hflux(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_hflux(bi,bj)   =',objf_hfluxm(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_hflux(bi,bj)   =',objf_hfluxmm(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_sflux(bi,bj)   =',objf_sflux(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_sflux(bi,bj)   =',objf_sfluxm(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_sflux(bi,bj)   =',objf_sfluxmm(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_atl(bi,bj)     =',objf_atl(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_ctdt(bi,bj)    =',objf_ctdt(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_ctds(bi,bj)    =',objf_ctds(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_ctdtclim(bi,bj)=',objf_ctdtclim(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_ctdsclim(bi,bj)=',objf_ctdsclim(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_xbt(bi,bj)     =',objf_xbt(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_argot(bi,bj)   =',objf_argot(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_argos(bi,bj)   =',objf_argos(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_drift(bi,bj)   =',objf_drift(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_tdrift(bi,bj)  =',objf_tdrift(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_sdrift(bi,bj)  =',objf_sdrift(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_wdrift(bi,bj)  =',objf_wdrift(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_scatx(bi,bj)   =',objf_scatx(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_scaty(bi,bj)   =',objf_scaty(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_scatxm(bi,bj)  =',objf_scatxm(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_scatym(bi,bj)  =',objf_scatym(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_uwind(bi,bj)   =',objf_uwind(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_vwind(bi,bj)   =',objf_vwind(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_atemp(bi,bj)   =',objf_atemp(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_aqh(bi,bj)     =',objf_aqh(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_obcsn(bi,bj)   =',objf_obcsn(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_obcss(bi,bj)   =',objf_obcss(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_obcsw(bi,bj)   =',objf_obcsw(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_obcse(bi,bj)   =',objf_obcse(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_ageos(bi,bj)   =',objf_ageos(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_curmtr(bi,bj)  =',objf_curmtr(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_obcsvol        =',objf_obcsvol
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_ice(bi,bj)     =',objf_ice(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_kapgm(bi,bj)   =',objf_kapgm(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_diffkr(bi,bj)  =',objf_diffkr(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_ini_fin(bi,bj) =',
     &         objf_theta_ini_fin(bi,bj) + objf_salt_ini_fin(bi,bj)
          write(standardmessageunit,'(A,D22.15)')
     &          ' --> objf_eddytau(bi,bj) =', objf_eddytau(bi,bj)
 
          fc = fc
     &            + mult_temp    * objf_temp(bi,bj)
     &            + mult_salt    * objf_salt(bi,bj)
     &            + mult_temp0   * objf_temp0(bi,bj)
     &            + mult_salt0   * objf_salt0(bi,bj)
     &            + mult_sst     * objf_sst(bi,bj) 
     &            + mult_tmi     * objf_tmi(bi,bj) 
     &            + mult_sss     * objf_sss(bi,bj) 
     &            + mult_tauu    * objf_tauu(bi,bj)
     &            + mult_tauu    * objf_tauum(bi,bj)
     &            + mult_tauv    * objf_tauv(bi,bj)
     &            + mult_tauv    * objf_tauvm(bi,bj)
     &            + mult_hflux   * objf_hflux(bi,bj)  
     &            + mult_hflux   * objf_hfluxm(bi,bj)  
     &            + mult_sflux   * objf_sflux(bi,bj)  
     &            + mult_sflux   * objf_sfluxm(bi,bj)  
     &            + mult_h       * objf_h(bi,bj)
     &            + mult_atl     * objf_atl(bi,bj)
     &            + mult_ctdt    * objf_ctdt(bi,bj)
     &            + mult_ctds    * objf_ctds(bi,bj)
     &            + mult_ctdtclim* objf_ctdtclim(bi,bj)
     &            + mult_ctdsclim* objf_ctdsclim(bi,bj)
     &            + mult_xbt     * objf_xbt(bi,bj)
     &            + mult_argot   * objf_argot(bi,bj)
     &            + mult_argos   * objf_argos(bi,bj)
     &            + mult_drift   * objf_drift(bi,bj)
     &            + mult_sdrift  * objf_sdrift(bi,bj)
     &            + mult_tdrift  * objf_tdrift(bi,bj)
     &            + mult_wdrift  * objf_wdrift(bi,bj)
     &            + mult_scatx   * objf_scatx(bi,bj)
     &            + mult_scaty   * objf_scaty(bi,bj)
     &            + mult_scatx   * objf_scatxm(bi,bj)
     &            + mult_scaty   * objf_scatym(bi,bj)
     &            + mult_uwind   * objf_uwind(bi,bj)
     &            + mult_vwind   * objf_vwind(bi,bj)
     &            + mult_atemp   * objf_atemp(bi,bj)  
     &            + mult_aqh     * objf_aqh(bi,bj)  
     &            + mult_obcsn   * objf_obcsn(bi,bj)  
     &            + mult_obcss   * objf_obcss(bi,bj)  
     &            + mult_obcsw   * objf_obcsw(bi,bj)  
     &            + mult_obcse   * objf_obcse(bi,bj)  
     &            + mult_curmtr  * objf_curmtr(bi,bj)
     &            + mult_ageos   * objf_ageos(bi,bj)
     &            + mult_ice     * objf_ice(bi,bj)
     &            + mult_kapgm   * objf_kapgm(bi,bj)
     &            + mult_diffkr  * objf_diffkr(bi,bj)
     &            + mult_ini_fin *(objf_theta_ini_fin(bi,bj) +
     &                             objf_salt_ini_fin(bi,bj))
     &            + mult_eddytau * objf_eddytau(bi,bj)

          f_temp = f_temp + objf_temp(bi,bj)
          f_salt = f_salt + objf_salt(bi,bj)
          f_temp0 = f_temp0 + objf_temp0(bi,bj)
          f_salt0 = f_salt0 + objf_salt0(bi,bj) 
          f_tauu = f_tauu + objf_tauu(bi,bj)
          f_tauum  = f_tauum + objf_tauum(bi,bj)
          f_tauv = f_tauv + objf_tauv(bi,bj)
          f_tauvm  = f_tauvm + objf_tauvm(bi,bj)
          f_hflux= f_hflux + objf_hflux(bi,bj)   
          f_hfluxm = f_hfluxm + objf_hfluxm(bi,bj)   
          f_hfluxmm = f_hfluxmm + objf_hfluxmm(bi,bj)   
          f_sflux= f_sflux + objf_sflux(bi,bj)  
          f_sfluxm = f_sfluxm + objf_sfluxm(bi,bj)  
          f_sfluxmm = f_sfluxmm + objf_sfluxmm(bi,bj)  
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
          f_ice = f_ice + objf_ice(bi,bj)
          f_kapgm = f_kapgm + objf_kapgm(bi,bj)
          f_diffkr = f_diffkr + objf_diffkr(bi,bj)
          f_ini_fin = f_ini_fin +
     &         objf_theta_ini_fin(bi,bj) + objf_salt_ini_fin(bi,bj)
          f_eddytau = f_eddytau + objf_eddytau(bi,bj) 

        enddo
      enddo


c--   Do global summation.
      _GLOBAL_SUM_R8( fc , myThid )

c--   Do global summation for each part of the cost function
            
      _GLOBAL_SUM_R8( f_temp , myThid )
      _GLOBAL_SUM_R8( f_salt , myThid )
      _GLOBAL_SUM_R8( f_temp0, myThid )
      _GLOBAL_SUM_R8( f_salt0, myThid )
      _GLOBAL_SUM_R8( f_tauu , myThid )
      _GLOBAL_SUM_R8( f_tauum , myThid )
      _GLOBAL_SUM_R8( f_tauv , myThid )
      _GLOBAL_SUM_R8( f_tauvm , myThid )
      _GLOBAL_SUM_R8( f_hflux , myThid )
      _GLOBAL_SUM_R8( f_hfluxm , myThid )
      _GLOBAL_SUM_R8( f_hfluxmm , myThid )
       f_hfluxmm = f_hfluxmm*f_hfluxmm/float(npx*npy)
      _GLOBAL_SUM_R8( f_sflux , myThid )
      _GLOBAL_SUM_R8( f_sfluxm , myThid )
      _GLOBAL_SUM_R8( f_sfluxmm , myThid )
       f_sfluxmm = f_sfluxmm*f_sfluxmm/float(npx*npy)
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
      _GLOBAL_SUM_R8( f_ice , myThid )
      _GLOBAL_SUM_R8( f_ini_fin , myThid )
      _GLOBAL_SUM_R8( f_eddytau , myThid )

c--   Each process has calculated the global part for itself.
      _BEGIN_MASTER( mythid )

        fc = fc + mult_hmean*objf_hmean
        fc = fc + 0.*mult_hflux*f_hfluxmm
        fc = fc + 0.*mult_sflux*f_sfluxmm
        write(standardmessageunit,'(A,D22.15)')
     &           ' --> objf_hmean           =',objf_hmean
        write(standardmessageunit,'(A,D22.15)')
     &           ' --> fc               =', fc
    
        write(cfname,'(A,i4.4)') 'costfunction',optimcycle
        open(unit=ifc,file=cfname)
      
        write(ifc,*) 'fc =', fc
        write(ifc,*) 'f_temp =', f_temp 
        write(ifc,*) 'f_salt =', f_salt 
        write(ifc,*) 'f_temp0 =', f_temp0 
        write(ifc,*) 'f_salt0 =', f_salt0
        write(ifc,*) 'f_tauu =', f_tauu 
        write(ifc,*) 'f_tauum =', f_tauum 
        write(ifc,*) 'f_tauv =', f_tauv 
        write(ifc,*) 'f_tauvm =', f_tauvm 
        write(ifc,*) 'f_hflux =', f_hflux 
        write(ifc,*) 'f_hfluxm =', f_hfluxm 
        write(ifc,*) 'f_hfluxmm =', f_hfluxmm 
        write(ifc,*) 'f_sflux =', f_sflux 
        write(ifc,*) 'f_sfluxm =', f_sfluxm 
        write(ifc,*) 'f_sfluxmm =', f_sfluxmm 
        write(ifc,*) 'f_ssh =', f_ssh
        write(ifc,*) 'f_sst =', f_sst
        write(ifc,*) 'f_tmi =', f_tmi
        write(ifc,*) 'f_sss =', f_sss
        write(ifc,*) 'f_atl =', f_atl
        write(ifc,*) 'f_ctdt =', f_ctdt 
        write(ifc,*) 'f_ctds =', f_ctds
        write(ifc,*) 'f_ctdtclim =', f_ctdtclim
        write(ifc,*) 'f_ctdsclim =', f_ctdsclim
        write(ifc,*) 'f_xbt =', f_xbt
        write(ifc,*) 'f_argot =', f_argot
        write(ifc,*) 'f_argos =', f_argos
        write(ifc,*) 'objf_hmean =', objf_hmean 
        write(ifc,*) 'f_drifter =', f_drifter 
        write(ifc,*) 'f_sdrift =', f_sdrift
        write(ifc,*) 'f_tdrift =', f_tdrift
        write(ifc,*) 'f_wdrift =', f_wdrift
        write(ifc,*) 'f_scatx =', f_scatx
        write(ifc,*) 'f_scaty =', f_scaty
        write(ifc,*) 'f_scatxm =', f_scatxm
        write(ifc,*) 'f_scatym =', f_scatym
        write(ifc,*) 'f_obcsn =', f_obcsn
        write(ifc,*) 'f_obcss =', f_obcss
        write(ifc,*) 'f_obcsw =', f_obcsw
        write(ifc,*) 'f_obcse =', f_obcse
        write(ifc,*) 'f_obcsvol=', objf_obcsvol
        write(ifc,*) 'f_ageos =', f_ageos
        write(ifc,*) 'f_ice   =', f_ice
        write(ifc,*) 'f_kapgm =', f_kapgm
        write(ifc,*) 'f_diffkr=', f_diffkr
        write(ifc,*) 'f_ini_fin =', f_ini_fin
        write(ifc,*) 'f_eddytau =', f_eddytau

        close(ifc)
        
      _END_MASTER( mythid )

      taveFreq   = 0.
      dumpFreq   = 0.
      pChkptFreq = 0.
      monitorFreq = 0.

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
