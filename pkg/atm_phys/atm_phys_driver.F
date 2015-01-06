C $Header: /u/gcmpack/MITgcm/pkg/atm_phys/atm_phys_driver.F,v 1.5 2015/01/06 21:20:57 jmc Exp $
C $Name:  $

#include "ATM_PHYS_OPTIONS.h"

CBOP
C !ROUTINE: ATM_PHYS_DRIVER

C !INTERFACE: ==========================================================
      SUBROUTINE ATM_PHYS_DRIVER(
     I                    myTime, myIter, myThid )

C !DESCRIPTION:
C Calculate custom tendency terms outside k-loop in DO_OCEANIC_PHYS

C !USES: ===============================================================
      use radiation_mod
      use lscale_cond_mod
      use dargan_bettsmiller_mod
      use surface_flux_mod
      use vert_turb_driver_mod
      use vert_diff_mod, only: gcm_vert_diff_down,
     &                         gcm_vert_diff_up,
     &                         surf_diff_type
      use mixed_layer_mod, only: mixed_layer
      use constants_mod, only:  HLv

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"
#include "ATM_PHYS_PARAMS.h"
#include "ATM_PHYS_VARS.h"

C !INPUT PARAMETERS: ===================================================
C  myTime   :: Current time in simulation
C  myIter   :: Current time-step number
C  myThid   :: my Thread Id number
      _RL     myTime
      INTEGER myIter, myThid

C !OUTPUT PARAMETERS: ==================================================

C !LOCAL VARIABLES: ====================================================
C  bi,bj    :: Tile indices
C  lat2d    :: latitude of grid-cell center          [rad]
C pHalf3d   :: pressure at interface between 2 levels [Pa]
C pFull3d   :: pressure at level center               [Pa]
C zHalf3d   :: height of interface between 2 levels   [m]
C zFull3d   :: height of level center                 [m]
C  t3d      :: absolute temperature                   [K]
C  q3d      :: specific humidity                    [kg/kg]
C  u3d      :: wind speed, 1rst component (X-dir)    [m/s]
C  v3d      :: wind speed, 2nd  component (Y-dir)    [m/s]
      INTEGER bi, bj
      _RL lat2d   (sNx,sNy)
      _RL pHalf3d (sNx,sNy,Nr+1)
      _RL pFull3d (sNx,sNy,Nr)
      _RL zHalf3d (sNx,sNy,Nr+1)
      _RL zFull3d (sNx,sNy,Nr)
      _RL t3d     (sNx,sNy,Nr)
      _RL q3d     (sNx,sNy,Nr)
      _RL u3d     (sNx,sNy,Nr)
      _RL v3d     (sNx,sNy,Nr)
      _RL tdt3d   (sNx,sNy,Nr)
      _RL qdt3d   (sNx,sNy,Nr)
      _RL udt3d   (sNx,sNy,Nr)
      _RL vdt3d   (sNx,sNy,Nr)
CEOP
      _RL s_sw_dwn(sNx,sNy)
      _RL s_lw_dwn(sNx,sNy)
      _RL t_surf  (sNx,sNy)
C-- radiation fields:
      _RL albedo_2d (sNx,sNy)
      _RL dtrans_3d (sNx,sNy,Nr)
      _RL b_3d      (sNx,sNy,Nr)
      _RL lw_down_3d(sNx,sNy,Nr+1)
      _RL sw_down_3d(sNx,sNy,Nr+1)
      _RL olr_2d    (sNx,sNy)
      _RL tsr_2d    (sNx,sNy)
      LOGICAL coldT (sNx,sNy)
C-- output from convection & LSC :
      _RL t3d_tmp    (sNx,sNy,Nr)
      _RL q3d_tmp    (sNx,sNy,Nr)
      _RL cond_dt_tg (sNx,sNy,Nr)
      _RL cond_dt_qg (sNx,sNy,Nr)
      _RL rain2d     (sNx,sNy)
      _RL snow2d     (sNx,sNy)
      _RL q_ref      (sNx,sNy,Nr)
      _RL t_ref      (sNx,sNy,Nr)
      _RL bmflag     (sNx,sNy)
      _RL klzbs      (sNx,sNy)
      _RL cape       (sNx,sNy)
      _RL cin        (sNx,sNy)
      _RL invtau_bm_t(sNx,sNy)
      _RL invtau_bm_q(sNx,sNy)
      _RL capeflag   (sNx,sNy)
C-- Input/Output for surface flux:
      _RL  q_surf(sNx,sNy)
      _RL  u_surf(sNx,sNy), v_surf(sNx,sNy)
      _RL  rough_mom(sNx,sNy), rough_heat(sNx,sNy)
      _RL  rough_moist(sNx,sNy), gust(sNx,sNy)
      _RL  flux_t(sNx,sNy), flux_q(sNx,sNy), flux_r(sNx,sNy)
      _RL  flux_u(sNx,sNy), flux_v(sNx,sNy)
      _RL  drag_m(sNx,sNy), drag_t(sNx,sNy), drag_q(sNx,sNy)
      _RL  w_atm(sNx,sNy)
      _RL  ustar(sNx,sNy), bstar(sNx,sNy), qstar(sNx,sNy)
      _RL  dhdt_surf(sNx,sNy), dedt_surf(sNx,sNy), dedq_surf(sNx,sNy)
      _RL  drdt_surf(sNx,sNy)
      _RL  dhdt_atm(sNx,sNy), dedq_atm(sNx,sNy), dtaudv_atm(sNx,sNy)
      LOGICAL land(sNx,sNy), avail(sNx,sNy)
C-- Input for turb:
      _RL fracland(sNx,sNy)
      _RL rough(sNx,sNy)
C-- Output from turbulence driver:
      _RL  diff_t(sNx,sNy,Nr), diff_m(sNx,sNy,Nr)
c     _RL  gust(sNx,sNy)
      _RL diff_dt_tg (sNx,sNy,Nr)
      _RL diff_dt_qg (sNx,sNy,Nr)
      _RL diss_heat  (sNx,sNy,Nr)
c     TYPE(surf_diff_type) :: tri_surf ! used by gcm_vert_diff
      _RL tri_surf_dtmass(sNx,sNy)
      _RL tri_surf_dflux_t(sNx,sNy), tri_surf_dflux_q(sNx,sNy)
      _RL tri_surf_delta_t(sNx,sNy), tri_surf_delta_q(sNx,sNy)
      _RL e_global(sNx,sNy,Nr-1)
      _RL f_t_global(sNx,sNy,Nr-1), f_q_global(sNx,sNy,Nr-1)
C-- Mixed Layer fields:
      _RL ocean_qflux(sNx,sNy)
      _RL delta_t_surf(sNx,sNy)
C--
      _RL conv_T2theta
      INTEGER k, kc
c     INTEGER ioUnit
c     _RS     dummyRS(1)
#ifdef COMPONENT_MODULE
      INTEGER i, j
      _RL taux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tauy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* COMPONENT_MODULE */

C--   Loops on tile indices bi,bj
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--   Get surface temperature
        t_surf(:,:) = atmPhys_SST(1:sNx,1:sNy,bi,bj)
#ifdef COMPONENT_MODULE
        IF ( useCoupler ) THEN
C--   take surface data from the ocean component
C     to replace MxL fields (if use sea-ice) or directly atm_phys SST
          CALL ATM_APPLY_IMPORT(
     I             maskInC,
     U             t_surf, taux,
c    U             sst1(1,myThid), oice1,
     I             myTime, myIter, bi, bj, myThid )
        ENDIF
#endif /* COMPONENT_MODULE */
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
C--   fill-up Atm_Phys diagnostics for state variables
         CALL DIAGNOSTICS_FILL( t_surf, 'AtPh_SST',
     &                          0, 1, 3, bi, bj, myThid )
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */
c       IF ( myIter.EQ.nIter0 ) THEN
c        ioUnit = 0
c        CALL MDS_WRITEVEC_LOC(
c    I                     'SST.check', writeBinaryPrec, ioUnit,
c    I                     'RL', sNx*sNy, t_surf, dummyRS,
c    I                     bi, bj, 1, myIter, myThid )
c       ENDIF
        ocean_qflux(:,:) = atmPhys_Qflx(1:sNx,1:sNy,bi,bj)

C--   Get grid and dynamical fields from main model common blocks
        CALL ATM_PHYS_DYN2PHYS(
     O                    lat2d, pHalf3d, pFull3d,
     O                    zHalf3d, zFull3d,
     O                    t3d, q3d, u3d, v3d,
     I                    bi, bj, myTime, myIter, myThid )

C--   initialise
        coldT(:,:) = .FALSE.
        EmPmR(:,:,bi,bj) = 0.
        rain2d(:,:) = 0.
        snow2d(:,:) = 0.

C--   initialise tendency
        tdt3d = 0.
        qdt3d = 0.
        udt3d = 0.
        vdt3d = 0.
        cond_dt_tg = 0.
        cond_dt_qg = 0.

        IF (lwet_convection) THEN
          CALL DARGAN_BETTSMILLER(
     I                   deltaT, t3d, q3d, pFull3d, pHalf3d, coldT,
     O                   rain2d, snow2d, cond_dt_tg, cond_dt_qg,
     O                   q_ref,      bmflag,
     O                   klzbs,        cape,
     O                   cin,         t_ref,
     O                   invtau_bm_t,     invtau_bm_q,
     O                   capeflag,
     I                   bi,bj,myIter,myThid )

C-    store updated state to pass to LSC (addition from POG)
          t3d_tmp = t3d + cond_dt_tg
          q3d_tmp = q3d + cond_dt_qg

          cond_dt_tg = cond_dt_tg / deltaT
          cond_dt_qg = cond_dt_qg / deltaT
          rain2d = rain2d / deltaT
          EmPmR(1:sNx,1:sNy,bi,bj) = -rain2d(:,:)

          tdt3d = tdt3d + cond_dt_tg
          qdt3d = qdt3d + cond_dt_qg

#ifdef ALLOW_DIAGNOSTICS
          IF ( useDiagnostics ) THEN
           CALL DIAGNOSTICS_FILL( rain2d , 'AtPhCnvP',
     &                            0, 1, 3, bi, bj, myThid )
           CALL DIAGNOSTICS_FILL( cape   , 'AtPhCAPE',
     &                            0, 1, 3, bi, bj, myThid )
           CALL DIAGNOSTICS_FILL( cin    , 'AtPhCnIn',
     &                            0, 1, 3, bi, bj, myThid )
           CALL DIAGNOSTICS_FILL( klzbs  , 'AtPhKlzb',
     &                            0, 1, 3, bi, bj, myThid )
           CALL DIAGNOSTICS_FILL( bmflag , 'AtPhConv',
     &                            0, 1, 3, bi, bj, myThid )
           CALL DIAGNOSTICS_FILL( invtau_bm_t, 'AtPhRlxT',
     &                            0, 1, 3, bi, bj, myThid )
           CALL DIAGNOSTICS_FILL( invtau_bm_q, 'AtPhRlxQ',
     &                            0, 1, 3, bi, bj, myThid )
           CALL DIAGNOSTICS_FILL( t_ref  , 'AtPh_Trf',
     &                           -1,Nr, 3, bi, bj, myThid )
           CALL DIAGNOSTICS_FILL( q_ref  , 'AtPh_Qrf',
     &                           -1,Nr, 3, bi, bj, myThid )
c          snow2d = snow2d / deltaT
c          CALL DIAGNOSTICS_FILL( snow2d , 'SDIAG1  ',
c    &                            0, 1, 3, bi, bj, myThid )
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */
        ELSE
          t3d_tmp = t3d
          q3d_tmp = q3d
        ENDIF

        cond_dt_tg = 0.
        cond_dt_qg = 0.
        rain2d(:,:) = 0.
        CALL LSCALE_COND(
     I              t3d_tmp, q3d_tmp, pFull3d, pHalf3d, coldT,
     O              rain2d, snow2d, cond_dt_tg, cond_dt_qg, q_ref,
     I              myThid )
        cond_dt_tg = cond_dt_tg / deltaT
        cond_dt_qg = cond_dt_qg / deltaT
        rain2d = rain2d / deltaT
        EmPmR(1:sNx,1:sNy,bi,bj) = EmPmR(1:sNx,1:sNy,bi,bj)
     &                           - rain2d(:,:)

        tdt3d = tdt3d + cond_dt_tg
        qdt3d = qdt3d + cond_dt_qg

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
           CALL DIAGNOSTICS_FILL( rain2d , 'AtPhLscP',
     &                            0, 1, 3, bi, bj, myThid )
C     Re-use "q_ref" array to calculate Relative Humidity (in %)
           q_ref = 100. _d 0 * q3d_tmp/q_ref
           CALL DIAGNOSTICS_FILL( q_ref , 'RELHUM  ',
     &                           -1, Nr, 3, bi, bj, myThid )
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

        IF ( two_stream ) THEN
          CALL RADIATION_DOWN(
     I                   sNx,sNy, myTime, lat2d, pHalf3d, t3d, q3d,
     O                   s_sw_dwn, s_lw_dwn,
     O                   albedo_2d, dtrans_3d, b_3d,
     O                   lw_down_3d, sw_down_3d, myThid )

c         write(errorMessageUnit,'(A,I5,I3,4F9.3)')
c    &      'it,bj,sw,lw:',myIter,bj,
c    &      sw_down_3d(1,1,15), sw_down_3d(1,sNy,15),
c    &      lw_down_3d(1,1,15), lw_down_3d(1,sNy,15)
        ENDIF

        IF (.TRUE.) THEN
         land = .false.
         avail = .true.
         rough_mom = roughness_mom
         rough_heat = roughness_heat
         rough_moist = roughness_moist
         gust = 1.0
         u_surf = 0.
         v_surf = 0.
         CALL SURFACE_FLUX(
     I            t3d(:,:,Nr), q3d(:,:,Nr), u3d(:,:,Nr), v3d(:,:,Nr),
     I            pFull3d(:,:,Nr), zFull3d(:,:,Nr), pHalf3d(:,:,Nr+1),
     I            t_surf, t_surf,
     U            q_surf,
     I            u_surf, v_surf,
     I            rough_mom, rough_heat, rough_moist, gust,
     O            flux_t, flux_q, flux_r, flux_u, flux_v,
     O            drag_m, drag_t, drag_q, w_atm,
     O            ustar, bstar, qstar,
     O            dhdt_surf, dedt_surf, dedq_surf, drdt_surf,
     O            dhdt_atm, dedq_atm, dtaudv_atm,
     I            deltaT, land(:,:), avail(:,:), myThid  )
        ENDIF

        IF ( two_stream ) THEN
          CALL RADIATION_UP(
     I                   sNx,sNy, myTime, lat2d, pHalf3d, t_surf, t3d,
     U                   tdt3d, olr_2d, tsr_2d,
     I                   albedo_2d, dtrans_3d, b_3d,
     I                   lw_down_3d, sw_down_3d, myThid )

c         write(errorMessageUnit,'(A,I5,I3,4F9.3)')
c    &      'it,bj,t,tdt:',myIter,bj,
c    &      t3d(1,1,15), t3d(1,sNy,15),
c    &      tdt3d(1,1,15)*86400., tdt3d(1,sNy,15)*86400.
        ENDIF

        IF (turb) THEN
         fracland = 0.0
         rough = roughness_mom
         CALL VERT_TURB_DRIVER(  1, 1, myTime, myTime+deltaT, deltaT,
     I             fracland(:,:), pHalf3d, pFull3d, zHalf3d, zFull3d,
     I             ustar, bstar, rough,
     I             u3d, v3d, t3d, q3d,
     O             diff_t(:,:,:), diff_m(:,:,:), gust(:,:),
     I             myThid )
        ENDIF

        diff_dt_tg = tdt3d
        diff_dt_qg = qdt3d

        CALL GCM_VERT_DIFF_DOWN( 1, 1, deltaT,
     I           u3d, v3d, t3d, q3d,
     I           diff_m(:,:,:), diff_t(:,:,:),
     I           pHalf3d, pFull3d, zFull3d,
     U           flux_u(:,:),  flux_v(:,:),  dtaudv_atm,
     U           udt3d, vdt3d, tdt3d,
     I           qdt3d,
     O           diss_heat(:,:,:),
     U           tri_surf_dtmass,
     U           tri_surf_dflux_t, tri_surf_dflux_q,
     U           tri_surf_delta_t, tri_surf_delta_q,
     O           e_global, f_t_global, f_q_global,
     I           myThid )

        CALL MIXED_LAYER(
     I                    myTime,
     U                    t_surf(:,:),
     I                    flux_t(:,:),
     I                    flux_q(:,:),
     I                    flux_r(:,:),
     I                    deltaT,
     I                    s_sw_dwn(:,:),
     I                    s_lw_dwn(:,:),
     U                    tri_surf_dtmass,
     U                    tri_surf_dflux_t, tri_surf_dflux_q,
     U                    tri_surf_delta_t, tri_surf_delta_q,
     I                    dhdt_surf(:,:),
     I                    dedt_surf(:,:),
     I                    dedq_surf(:,:),
     I                    drdt_surf(:,:),
     I                    dhdt_atm(:,:),
     I                    dedq_atm(:,:),
     I                    ocean_qflux(:,:),
     O                    delta_t_surf(:,:),
     I                    myThid )

        CALL GCM_VERT_DIFF_UP ( 1, 1, deltat,
     I           tri_surf_delta_t, tri_surf_delta_q,
     I           e_global, f_t_global, f_q_global,
     O           tdt3d, qdt3d,
     I           myThid )

        diff_dt_tg = tdt3d - diff_dt_tg
        diff_dt_qg = qdt3d - diff_dt_qg

C--- update OLR (assume adjustment of surf LW emission goes directly to space):
        olr_2d = olr_2d + drdt_surf*delta_t_surf

        DO k=1,Nr
          kc = Nr-k+1
          conv_T2theta = (atm_po/rC(kc))**atm_kappa
#ifdef NONLIN_FRSURF
          IF ( select_rStar.GE.1 ) THEN
           atmPhys_dT(1:sNx,1:sNy,kc,bi,bj) = tdt3d(:,:,k)
     &                   * conv_T2theta/pStarFacK(1:sNx,1:sNy,bi,bj)
          ELSE
#else /* NONLIN_FRSURF */
          IF ( .TRUE. ) THEN
#endif /* NONLIN_FRSURF */
           atmPhys_dT(1:sNx,1:sNy,kc,bi,bj) = tdt3d(:,:,k)
     &                   * conv_T2theta
          ENDIF
          atmPhys_dQ(1:sNx,1:sNy,kc,bi,bj)  = qdt3d(:,:,k)
          atmPhys_dU (1:sNx,1:sNy,kc,bi,bj) = udt3d(:,:,k)
          atmPhys_dV (1:sNx,1:sNy,kc,bi,bj) = vdt3d(:,:,k)
        ENDDO

C--   Update SST
        IF ( atmPhys_stepSST ) THEN
          atmPhys_SST(1:sNx,1:sNy,bi,bj) = t_surf(:,:)
        ENDIF

        EmPmR(1:sNx,1:sNy,bi,bj) = EmPmR(1:sNx,1:sNy,bi,bj)
     &                           + flux_q(:,:)
        Qnet(1:sNx,1:sNy,bi,bj) = flux_t(:,:) + flux_r(:,:)
     &                          - s_lw_dwn(:,:) - s_sw_dwn(:,:)
     &                          + flux_q(:,:)*HLv
        Qsw (1:sNx,1:sNy,bi,bj) = -s_sw_dwn(:,:)
#ifdef COMPONENT_MODULE
C-    surface wind-stress (+ = down) at grid cell center (A-grid):
        taux(1:sNx,1:sNy,bi,bj) = -flux_u
        tauy(1:sNx,1:sNy,bi,bj) = -flux_v
#endif /* COMPONENT_MODULE */

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL( atmPhys_dT , 'AtPhdTdt',
     &                           0, Nr, 1, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( atmPhys_dQ , 'AtPhdQdt',
     &                           0, Nr, 1, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( atmPhys_dU , 'AtPhdUdt',
     &                           0, Nr, 1, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( atmPhys_dV , 'AtPhdVdt',
     &                           0, Nr, 1, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( diff_t , 'AtPhDifT',
     &                           -1, Nr, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( diff_m , 'AtPhDifM',
     &                           -1, Nr, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( sw_down_3d,'AtPhInSR',
     &                           0, 1, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( tsr_2d ,'AtPhNTSR',
     &                           0, 1, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( olr_2d ,   'AtPhOLR ',
     &                           0, 1, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( sw_down_3d(:,:,Nr+1),'AtPhDSSR',
     &                           0, 1, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( s_sw_dwn,  'AtPhNSSR',
     &                           0, 1, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( s_lw_dwn,  'AtPhDSLR',
     &                           0, 1, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( flux_r ,   'AtPhUSLR',
     &                           0, 1, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( flux_t ,   'AtPhSens',
     &                           0, 1, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( flux_q ,   'AtPhEvap',
     &                           0, 1, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( flux_u , 'AtPhTauX',
     &                           0, 1, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( flux_v , 'AtPhTauY',
     &                           0, 1, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( diff_dt_tg, 'AtPhdtTg',
     &                           -1, Nr, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( diff_dt_qg, 'AtPhdtQg',
     &                           -1, Nr, 3, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( diss_heat,  'AtPhDisH',
     &                           -1, Nr, 3, bi, bj, myThid )
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   end bi,bj loops.
       ENDDO
      ENDDO

      CALL EXCH_UV_AGRID_3D_RL( atmPhys_dU, atmPhys_dV,
     &                          .TRUE., Nr, myThid )
#ifdef COMPONENT_MODULE
C-    surface wind-stress at grid cell center (A-grid):
      IF ( useCoupler ) THEN
        CALL EXCH_UV_AGRID_3D_RL( taux, tauy,
     &                           .TRUE., 1, myThid )
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
            fu(i,j,bi,bj) = halfRL
     &                    *( taux(i-1,j,bi,bj) + taux(i,j,bi,bj) )
            fv(i,j,bi,bj) = halfRL
     &                    *( tauy(i,j-1,bi,bj) + tauy(i,j,bi,bj) )
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL ATM_STORE_MY_DATA( myTime, myIter, myThid )
      ENDIF
#endif /* COMPONENT_MODULE */

      RETURN
      END
