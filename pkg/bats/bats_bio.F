#include "BATS_OPTIONS.h"

CBOP
      SUBROUTINE BATS_BIO( PTR_SPC, PTR_TRC, PTR_UNC,
 I           PTR_BAC, PTR_PRTC, PTR_MZC,
 I           PTR_DVMZC, PTR_DETC, PTR_NH4,
 I           PTR_NO3, PTR_PO4,
 I           PTR_DOC, PTR_DON, PTR_DOP
 I           PTR_SDOC, PTR_SDON, PTR_SDOP,
 O           G_SPC, G_TRC, G_UNC,
 O           G_BAC, G_PRTC, G_MZC,
 O           G_DVMZC, G_DETC, G_NH4,
 O           G_NO3, G_PO4,
 O           G_DOC, G_DON, G_DOP,
 O           G_SDOC, PTR_SDON, PTR_SDOP,
 I           bi, bj, imin, imax, jmin, jmax,
 I           myTime, myIter, myThid)

      use eco_params
      IMPLICIT NONE

#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BATS_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"

C     === Routine arguments ===
C     bi,bj         :: tile indices
C     iMin,iMax     :: computation DOPain: 1rst index range
C     jMin,jMax     :: computation DOPain: 2nd  index range
C     myTime        :: current time
C     myIter        :: current timestep
C     myThid        :: thread Id. number
      INTEGER bi, bj, imin, imax, jmin, jmax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     === Input ===
C     PTR_SPC       :: microphytoplankton carbon
C     PTR_TRC       :: Trichodesmium carbon
C     PTR_UNC       :: unicellular N2-fixer carbon
C     PTR_BAC       :: bacterial carbon
C     PTR_PRTC      :: protozoan carbon
C     PTR_MZC       :: metazoan carbon
C     PTR_DVMZC     :: vertically migrating metazoan carbon
C     PTR_DETC      :: detrital carbon
C     PTR_NH4       :: ammonium concentration
C     PTR_NO3       :: nitrate concentration
C     PTR_PO4       :: phosphate concentration
C     PTR_DOC       :: dissolved organic carbon concentration
C     PTR_DON       :: dissolved organic nitrogen concentration
C     PTR_DOP       :: dissolved organic phosphorus concentration
C     PTR_SDOC      :: semi-labile dissolved organic carbon concentration
C     PTR_SDON      :: semi-labile dissolved organic nitrogen concentration
C     PTR_SDOP      :: semi-labile dissolved organic phosphorus concentration
      _RL  PTR_SPC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_TRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_UNC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_BAC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PTRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_MZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DVMZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DETC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_NH4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DOC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_SDOC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_SDON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_SDOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C     === Output ===
C     G_xxx                :: tendency term for tracer xxx

      _RL  G_SPC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_TRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_UNC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_BAC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_PTRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_MZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DVMZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DETC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_NH4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DOC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_SDOC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_SDON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_SDOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)


C     === Local variables ===
C     i,j,k         :: loop indices
C
      INTEGER i,j,k
!-----------------------------------------------------------------------
!     Local variables
!-----------------------------------------------------------------------
! ecosystem model parameters
    double precision :: temp,temp1
    double precision :: ae,mu_SP,alpha_SP,a_SP,v_SPn,k_nh4SP,k_no3SP,&
      v_SPp,k_po4SP, zeta, theta, r_excrSP_1,r_excrSP_2,r_pomSP,&
      mu_TR,alpha_TR,a_TR,k_nh4TR,v_TRn,k_no3TR,&
      v_TRp, k_po4TR,mu_pickTRpo4, zeta_nf, &
      r_excrTR_1,r_excrTR_n,r_excrTR_2,r_pomTR,&
      mu_UN,alpha_UN,r_SDOM,k_DOM,mu_BA,& !b_SDONlabi,b_SDOPlabi,
      b_BAresp,r_BAadju,r_BAremi,r_BArefr,f_BAslct,r_BAresp_1, r_BAresp_min, &
      r_BAresp_max, r_BAmort, mu_PRT,g_sp,&
      g_ba,r_PRTex,f_exPRTldom,&
      r_PRTresp_1,r_PRTresp_2,r_PRTadju,r_PRTremi,&
      r_pomPRT,mu_MZ,g_prt,g_tr,r_MZex,&
      f_exMZldom,r_MZresp_1,r_MZresp_2,r_MZadju,r_MZremi,&
      r_MZpom,r_MZrefr,r_MZremv,f_HZsdom,f_HZpom,r_SDOMrefr, q_refrDOM_n, q_refrDOM_p, &
      q_POM_n,q_POM_p,r_nitrf,remin_prf_n,remin_prf_p,wnsvo,remin,k_pom,g_det,f_dvm
    ! local fixed parameters
    double precision :: q_SP_min_n,&
      q_SP_min_p, q_SP_max_n,q_SP_max_p, q_SP_rdf_n,&
      q_SP_rdf_p,v_UNn,v_UNp,q_UN_min0_n,q_UN_min0_p,&
      q_UN_max0_n,k_nh4UN,k_no3UN,k_po4UN, &
      r_excrUN_1,r_excrUN_n,r_excrUN_2,r_pomUN,q_BA_n,q_BA_p,&
      g_un,q_PRT_n,q_PRT_p,&
      q_MZ_n,q_MZ_p,Tref,a_UN
    ! local variables for temperature effect function
    double precision :: Tfunc
    ! local variables in the SP module
    double precision :: Nfunc_sp_n, Nfunc_sp_p, Pc_SPmax, growSPnh4, growSPno3
    double precision :: V_SPmax_n, V_SPmax_p, respSP,  growSPchl, grazSPchl, pomSPchl
    double precision :: growSPc,growSPn,growSPp,excrSP_1c,excrSP_1n,excrSP_1p
    double precision :: excrSP_2c,excrSP_2n,excrSP_2p,pomSPc,pomSPn,pomSPp
    double precision :: grazSPc,grazSPn,grazSPp
    ! local variables in the TR module
    double precision :: q_TR_min_n,q_TR_min_p, &
                           q_TR_max_n,q_TR_max_p, &
                           q_TR_rdf_n,q_TR_rdf_p, &
                           growTRnh4,growTRno3,growTRnf,maxTRnf, &
                           excrTR_nh4,growTRpo4,pickTRpo4
    double precision :: growTRc,growTRn,growTRp, &
                           excrTR_1c,excrTR_1n,excrTR_1p,excrTR_2c,excrTR_2n,excrTR_2p, &
                           pomTRc,pomTRn,pomTRp,grazTRc,grazTRn,grazTRp
    double precision ::  Nfunc_tr_n, Nfunc_tr_p, Pc_TRmax,V_TRmax_n, V_TRmax_p, &
                                         respTR,  growTRchl, grazTRchl, pomTRchl
    ! local variables in the UN module
    double precision :: q_UN_min_n,q_UN_min_p, &
                           q_UN_max_n,q_UN_max_p, &
                           q_UN_rdf_n,q_UN_rdf_p, &
                           growUNnh4,growUNno3,growUNnf,maxUNnf, &
                           excrUN_nh4
    double precision :: growUNc,growUNn,growUNp, &
                           excrUN_1c,excrUN_1n,excrUN_1p,excrUN_2c,excrUN_2n,excrUN_2p, &
                           pomUNc,pomUNn,pomUNp,grazUNc,grazUNn,grazUNp
    double precision ::  Nfunc_un_n, Nfunc_un_p, Pc_UNmax,V_UNmax_n, V_UNmax_p, &
                                         respUN,  growUNchl, grazUNchl, pomUNchl
    ! local variables in the BA module
    double precision :: ALC,ASC,Nfunc_ba_n,Nfunc_ba_p,&
                           growBAldoc,growBAldon,growBAldop,growBAsdoc,growBAsdon,growBAsdop,&
                           growBAnh4,growBAno3,growBApo4,respBA,fluxBAnh4,fluxBAno3,fluxBApo4, &
                           growBAc,growBAn,growBAp,refrBAc,refrBAn,refrBAp,&
                           excrBAc,excrBAn,excrBAp,remiBAn,remiBAp,&
                           grazBAc,grazBAn,grazBAp,mortBAc,mortBAn,mortBAp
    ! local variables in the PRT module
    double precision :: growPRTc,growPRTn,growPRTp,&
                           excrPRTldomc,excrPRTldomn,excrPRTldomp, &
                           excrPRTsdomc,excrPRTsdomn,excrPRTsdomp, &
                           excrPRTsdom2c,excrPRTsdom2n,excrPRTsdom2p, &
                           remiPRTn,remiPRTp,pomPRTc,pomPRTn,pomPRTp, &
                           grazPRTc,grazPRTn,grazPRTp,respPRT
    ! local variables in the MZ module
    double precision :: growMZc,growMZn,growMZp,&
                           excrMZldomc,excrMZldomn,excrMZldomp, &
                           excrMZsdomc,excrMZsdomn,excrMZsdomp, &
                           excrMZsdom2c,excrMZsdom2n,excrMZsdom2p, &
                           remiMZn,remiMZp,pomMZc,pomMZn,pomMZp, &
                           refrMZc,refrMZn,refrMZp,remvMZc,remvMZn,remvMZp, &
                           pomHZc,pomHZn,pomHZp,excrHZsdomc,excrHZsdomn,excrHZsdomp, &
                           remiHZn,remiHZp,respMZ
    ! local variables in the DOM module
    double precision :: refrSDOMc,refrSDOMn,refrSDOMp
    ! local variables in the DET module
    double precision ::  disDETc,disDETn,disDETp
    ! local variables in the DIN module
    double precision :: nitrf
    ! local variables in newly added processes
    double precision :: growBApoc, growBApon, growBApop, grazDETc, grazDETn, grazDETp
   
! map bioparams to local copies
    ae          = bioparams(iae         )
    mu_SP          = bioparams(imu_SP          )
    alpha_SP       = bioparams(ialpha_SP       )
    a_SP       = bioparams(ia_SP       )
    v_SPn        = bioparams(iv_SPn        )
    k_nh4SP        = bioparams(ik_nh4SP        )
    k_no3SP        = bioparams(ik_no3SP        )
    v_SPp           = bioparams(iv_SPp       )
    k_po4SP        = bioparams(ik_po4SP        )
    zeta              = bioparams(izeta       )
    theta            = bioparams(itheta       )
    r_excrSP_1     = bioparams(ir_excrSP_1     )
    r_excrSP_2     = bioparams(ir_excrSP_2     )
    r_pomSP        = bioparams(ir_pomSP        )
    mu_TR          = bioparams(imu_TR          )
    alpha_TR       = bioparams(ialpha_TR       )
    a_TR           = bioparams(ia_TR       )
    v_TRn        = bioparams(iv_TRn        )
    k_nh4TR        = bioparams(ik_nh4TR        )
    k_no3TR        = bioparams(ik_no3TR        )
    v_TRp        = bioparams(iv_TRp        )
    k_po4TR        = bioparams(ik_po4TR        )
    mu_pickTRpo4 = bioparams(imu_pickTRpo4)
    zeta_nf        = bioparams(izeta_nf        )
    r_excrTR_1     = bioparams(ir_excrTR_1     )
    r_excrTR_n   = bioparams(ir_excrTR_n   )
    r_excrTR_2     = bioparams(ir_excrTR_2     )
    r_pomTR        = bioparams(ir_pomTR        )
    mu_UN          = bioparams(imu_UN          )
    alpha_UN       = bioparams(ialpha_UN       )
    k_DOM        = bioparams(ik_DOM        )
    !b_SDONlabi     = bioparams(ib_SDONlabi     )
    !b_SDOPlabi     = bioparams(ib_SDOPlabi     )
    r_SDOM      = bioparams(ir_SDOM    )
    mu_BA          = bioparams(imu_BA          )
    b_BAresp       = bioparams(ib_BAresp       )
    r_BAadju       = bioparams(ir_BAadju       )
    r_BAremi       = bioparams(ir_BAremi       )
    r_BArefr       = bioparams(ir_BArefr       )
    f_BAslct = bioparams(if_BAslct)
    r_BAresp_1 = bioparams(ir_BAresp_1)
    r_BAresp_min = bioparams(ir_BAresp_min)
    r_BAresp_max = bioparams(ir_BAresp_max)
    r_BAmort = bioparams(ir_BAmort)
    mu_PRT       = bioparams(imu_PRT       )
    g_sp        = bioparams(ig_sp        )
    g_ba        = bioparams(ig_ba        )
    r_PRTex      = bioparams(ir_PRTex      )
    f_exPRTldom    = bioparams(if_exPRTldom    )
    r_PRTresp_1    = bioparams(ir_PRTresp_1    )
    r_PRTresp_2    = bioparams(ir_PRTresp_2    )
    r_PRTadju      = bioparams(ir_PRTadju      )
    r_PRTremi      = bioparams(ir_PRTremi      )
    r_pomPRT       = bioparams(ir_pomPRT       )
    mu_MZ       = bioparams(imu_MZ       )
    g_prt        = bioparams(ig_prt        )
    g_tr         = bioparams(ig_tr         )
    r_MZex         = bioparams(ir_MZex         )
    f_exMZldom     = bioparams(if_exMZldom     )
    r_MZresp_1     = bioparams(ir_MZresp_1     )
    r_MZresp_2     = bioparams(ir_MZresp_2     )
    r_MZadju       = bioparams(ir_MZadju       )
    r_MZremi       = bioparams(ir_MZremi       )
    r_MZpom        = bioparams(ir_MZpom        )
    r_MZrefr       = bioparams(ir_MZrefr       )
    r_MZremv       = bioparams(ir_MZremv       )
    f_HZsdom       = bioparams(if_HZsdom       )
    f_HZpom        = bioparams(if_HZpom        )
    r_SDOMrefr     = bioparams(ir_SDOMrefr     )
    q_refrDOM_n = bioparams(iq_refrDOM_n)
    q_refrDOM_p = bioparams(iq_refrDOM_p)
    q_POM_n        = bioparams(iq_POM_n        )
    q_POM_p        = bioparams(iq_POM_p        )
    r_nitrf        = bioparams(ir_nitrf        )
    remin_prf_n    = bioparams(iremin_prf_n    )
    remin_prf_p    = bioparams(iremin_prf_p    )
    wnsvo          = bioparams(iwnsvo          )
    remin          = bioparams(iremin          )
    k_pom          = bioparams(ik_pom          )
    g_det          = bioparams(ig_det          )
    f_dvm          = bioparams(if_dvm          )

    !Fixed Parameters
    ! minimum [N,P]:C for SP
    q_SP_min_n = 0.034d0
    q_SP_min_p = 0.00375d0
    ! maximum [N,P]:C for SP
    q_SP_max_n = 0.17d0
    q_SP_max_p = 0.00926d0
    ! Redfield ratios
    q_SP_rdf_n = 0.15d0
    q_SP_rdf_p = 0.0094d0
    q_TR_min_n = 0.12d0
    q_TR_min_p = 0.001d0
    q_TR_max_n = 0.20d0
    q_TR_max_p = 0.0060d0
    q_TR_rdf_n = 0.16d0
    q_TR_rdf_p = 0.0035d0
    q_UN_min_n = 0.12d0
    q_UN_min_p = q_TR_min_p
    q_UN_max_n = 0.20d0
    q_UN_max_p = q_TR_max_p
    q_UN_rdf_n = 0.16d0
    q_UN_rdf_p =  q_TR_rdf_p
    v_UNn = v_SPn
    k_nh4UN = k_nh4SP
    k_no3UN = k_no3SP
    v_UNp = v_SPp
    k_po4UN = k_po4SP
    r_excrUN_1=r_excrSP_1
    r_excrUN_n=r_excrTR_n
    r_excrUN_2=r_excrSP_2
    r_pomUN=r_pomSP
    ! Optimal baterial [C, N, P]:C ratio
    q_BA_n = 0.18d0
    q_BA_p = 0.02d0
    g_un=g_sp
    ! Optimal protozoa [C, N, P]:C ratio
    q_PRT_n = 0.2d0
    q_PRT_p = 0.022d0
    ! Optimal metazoa [C, N, P]:C ratio
    q_MZ_n = 0.2d0
    q_MZ_p = 0.008d0
    a_UN = a_SP
    Tref = 25.d0 ! Reference Temperature for Function of Temperatue Effects
    ! Temperature Effects
    Tfunc = exp(-ae*( 1/(Tdat(iz,istep)+273.15) - 1/(Tref+273.15) ) )
    mu_SP = mu_SP * Tfunc
    v_SPn = v_SPn * Tfunc
    v_SPp = v_SPp * Tfunc
    mu_TR = mu_TR * Tfunc
    v_TRn = v_TRn * Tfunc
    v_TRp = v_TRp * Tfunc
    mu_UN = mu_UN * Tfunc
    v_UNn = v_UNn * Tfunc
    v_UNp = v_UNp * Tfunc
    mu_BA = mu_BA * Tfunc
    mu_PRT = mu_PRT * Tfunc
    r_BAresp_1 = r_BAresp_1 * Tfunc
    r_PRTresp_1 = r_PRTresp_1 * Tfunc
    mu_MZ = mu_MZ * Tfunc
    r_MZresp_1 = r_MZresp_1 * Tfunc

CEOP

c-----------------------------------------------------------
c  avoid negative nutrient concentrations that can result from
c  advection when low concentrations

#ifdef BATS_NO_NEG
      CALL BATS_MIN_VAL(PTR_NH4,  1. _d -11, o2_adj,  bi, bj)
      CALL BATS_MIN_VAL(PTR_NO3,  1. _d -11, fe_adj,  bi, bj)
      CALL BATS_MIN_VAL(PTR_PO4, 1. _d -8,  po4_adj, bi, bj)
      CALL BATS_MIN_VAL(PTR_DOC, 1. _d -11, dop_adj, bi, bj)
      CALL BATS_MIN_VAL(PTR_DON, 1. _d -11, dop_adj, bi, bj)
      CALL BATS_MIN_VAL(PTR_DOP, 1. _d -11, dop_adj, bi, bj)
#endif

c-----------------------------------------------------------
c  light availability for biological production

       CALL BATS_LIGHT(
     U                    irr_inst,
     I                    bi, bj, imin, imax, jmin, jmax,
     I                    myTime, myIter, myThid )
c ---------------------------------------------------------------------
c  Nutrient uptake and partitioning between organic pools

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN

c ---------------------------------------------------------------------
c  Microphytoplankton processes

c  Macro-nutrient limitation

          PO4_lim(i,j,k) = PTR_PO4(i,j,k)/(PTR_PO4(i,j,k)+k_PO4)

c  Iron to macro-nutrient uptake. More iron is utilized relative
c  to macro-nutrient under iron-replete conditions.

          FetoP_up(i,j,k) = FetoP_max*PTR_FE(i,j,k)/(k_Fe+PTR_FE(i,j,k))

c  Iron limitation

          Fe_lim(i,j,k) = Fe_lim_min + (1-Fe_lim_min)*
     &                    (FetoP_up(i,j,k)/(k_FetoP + FetoP_up(i,j,k)))*
     &                    (k_FetoP+FetoP_max)/FetoP_max

c ---------------------------------------------------------------------
c  Light-saturated maximal photosynthesis rate

c  NB: The temperature effect of Eppley (1972) is used instead of that in
c  Geider et al (1997) for both simplicity and to incorporate combined
c  effects on uptake, incorporation into organic matter and photorespiration.
c  Values of PCc_m are normalized to 0C rather than 20C in Geider et al. (1997)

          expkT(i,j,k) = exp(kappa_eppley * theta(i,j,k,bi,bj))

c  For the effective resource limitation, there is an option to replace
c  the default Liebig limitation (the minimum of Michaelis-Menton
c  PO4-limitation, or iron-limitation) by the product (safer for adjoint)

#ifdef MULT_NUT_LIM
          Pc_m = Pc_0*expkT(i,j,k)
     &           *PO4_lim(i,j,k)*Fe_lim(i,j,k)*maskC(i,j,k,bi,bj)
#else
          Pc_m = Pc_0*expkT(i,j,k)
     &           *min(PO4_lim(i,j,k), Fe_lim(i,j,k))*maskC(i,j,k,bi,bj)
#endif

c ---------------------------------------------------------------------
c  Fe limitation 1) reduces photosynthetic efficiency (alpha_Fe)
c  and 2) reduces the maximum achievable Chl:C ratio (theta_Fe)
c  below a prescribed, Fe-replete maximum value (theta_Fe_max),
c  to approach a prescribed minimum Chl:C (theta_Fe_min) under extreme
c  Fe-limitation.

          alpha_Fe = alpha_min + (alpha_max-alpha_min)*Fe_lim(i,j,k)

          theta_Fe_max = theta_Fe_max_lo+
     &                  (theta_Fe_max_hi-theta_Fe_max_lo)*Fe_lim(i,j,k)

          theta_Fe(i,j,k) = theta_Fe_max/(1. _d 0 + alpha_Fe
     &                  *theta_Fe_max*irr_mem(i,j,k,bi,bj)/
     &                  (2. _d 0*Pc_m))

c  for diagnostics: C:Chl ratio in g C / g Chl

          IF ( theta_Fe(i,j,k) .EQ.0. ) THEN
           theta_Fe_inv(i,j,k) = 0.
          ELSE
           theta_Fe_inv(i,j,k) = 1./theta_Fe(i,j,k)
          ENDIF

c ---------------------------------------------------------------------
c  Nutrient-limited efficiency of algal photosystems, irrk, is calculated
c  with the iron limitation term included as a multiplier of the
c  theta_Fe_max to represent the importance of Fe in forming chlorophyll
c  accessory antennae, which do not affect the Chl:C but still affect the
c  phytoplankton ability to use light (eg Stzrepek & Harrison, Nature 2004).

          irrk(i,j,k) = Pc_m/(epsln + alpha_Fe*theta_Fe_max) +
     &              irr_mem(i,j,k,bi,bj)/2. _d 0

          light_lim(i,j,k) = ( 1. _d 0 - exp(-irr_eff(i,j,k)
     &               /(epsln + irrk(i,j,k))))

c  Carbon-specific photosynthesis rate

          Pc_tot = Pc_m * light_lim(i,j,k)

c ---------------------------------------------------------------------
c  Account for the maintenance effort that phytoplankton must exert in
c  order to combat decay. This is prescribed as a fraction of the
c  light-saturated photosynthesis rate, resp_frac. The result of this
c  is to set a level of energy availability below which net growth
c  (and therefore nutrient uptake) is zero, given by resp_frac * Pc_m.

          mu(i,j,k) = max(0. _d 0, Pc_tot - resp_frac*Pc_m)

c ---------------------------------------------------------------------
c  In order to convert this net carbon-specific growth rate to nutrient
c  uptake rates, the quantities we are interested in, a biomass is required.
c  This is determined by the balance between growth and grazing.

c  Since there is no explicit biomass tracer, use the result of Dunne
c  et al. (GBC, 2005) to calculate an implicit biomass from the uptake
c  rate through the application of a simple idealized grazing law.

c  instantaneous nutrient concentration in phyto biomass

          biomass_lg = pivotal*(mu(i,j,k)/(lambda_0
     &                *expkT(i,j,k)))**3

          biomass_sm = pivotal*(mu(i,j,k)/(lambda_0
     &                *expkT(i,j,k)))

c  phytoplankton biomass diagnostic
c  for no lag: set gamma_biomass to 0

          phy_sm_local(i,j,k) = phy_sm_local(i,j,k) +
     &       (biomass_sm - phy_sm_local(i,j,k))
     &       *min(1., gamma_biomass*PTRACERS_dTLev(k))

          phy_lg_local(i,j,k) = phy_lg_local(i,j,k) +
     &       (biomass_lg - phy_lg_local(i,j,k))
     &       *min(1., gamma_biomass*PTRACERS_dTLev(k))

c  use the diagnostic biomass to calculate the chl concentration
c  in mg/m3 (carbon = 12.01 g/mol)

          chl(i,j,k,bi,bj) = max(chl_min, CtoP * 12.01 * 1. _d 3 *
     &           theta_Fe(i,j,k) *
     &           (Phy_lg_local(i,j,k) + Phy_sm_local(i,j,k)))

c  Nutrient uptake

          P_uptake(i,j,k) = mu(i,j,k)*(phy_sm_local(i,j,k)
     &                        + phy_lg_local(i,j,k))

c  Iron is then taken up as a function of nutrient uptake and iron
c  limitation, with a maximum Fe:P uptake ratio of Fe2p_max

          Fe_uptake(i,j,k) = P_uptake(i,j,k)*FetoP_up(i,j,k)

         ENDIF
        ENDDO
       ENDDO
      ENDDO

c  Separate loop for adjoint stores
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE Phy_sm_local   = comlev1_bibj, key=tkey, kind=isbyte
CADJ STORE Phy_lg_local   = comlev1_bibj, key=tkey, kind=isbyte
#endif

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN

c  update biomass
          phyto_lg(i,j,k,bi,bj) = Phy_lg_local(i,j,k)
          phyto_sm(i,j,k,bi,bj) = Phy_sm_local(i,j,k)

         ENDIF
        ENDDO
       ENDDO
      ENDDO

c ---------------------------------------------------------------------
c  Partitioning between organic pools

c  The uptake of nutrients is assumed to contribute to the growth of
c  phytoplankton, which subsequently die and are consumed by heterotrophs.
c  This can involve the transfer of nutrient elements between many
c  organic pools, both particulate and dissolved, with complex histories.
c  We take a simple approach here, partitioning the total uptake into two
c  fractions - sinking and non-sinking - as a function of temperature,
c  following Dunne et al. (2005).
c  Then, the non-sinking fraction is further subdivided, such that the
c  majority is recycled instantaneously to the inorganic nutrient pool,
c  representing the fast turnover of labile dissolved organic matter via
c  the microbial loop, and the remainder is converted to semi-labile
c  dissolved organic matter. Iron and macro-nutrient are treated
c  identically for the first step, but all iron is recycled
c  instantaneously in the second step (i.e. there is no dissolved organic
c  iron pool).

      DO k=1,Nr
       DO j=jmin,jmax
        DO i=imin,imax

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN

c  sinking particulate organic matter

          frac_exp = (phi_sm + phi_lg *
     &               (mu(i,j,k)/(lambda_0*expkT(i,j,k)))**2.)/
     &               (1. + (mu(i,j,k)/(lambda_0*expkT(i,j,k)))**2.)*
     &               exp(kappa_remin * theta(i,j,k,bi,bj))

          P_spm(i,j,k) = frac_exp * P_uptake(i,j,k)

          Fe_spm(i,j,k) = P_spm(i,j,k)*FetoP_up(i,j,k)

c  the remainder is divided between instantaneously recycled and
c  long-lived dissolved organic matter.
c  (recycling = P_uptake - P_spm - DOP_prod)

          DOP_prod(i,j,k) = phi_DOM*(P_uptake(i,j,k)
     &                      - P_spm(i,j,k))

          P_recycle(i,j,k) = P_uptake(i,j,k) - P_spm(i,j,k)
     &                        - DOP_prod(i,j,k)

          Fe_recycle(i,j,k) = Fe_uptake(i,j,k) - Fe_spm(i,j,k)

c  Carbon flux diagnostic

          POC_flux(i,j,k) = CtoP*P_spm(i,j,k)

c ---------------------------------------------------------------------
c  Calcium carbonate production

c  Alkalinity is consumed through the production of CaCO3. Here, this is
c  simply a linear function of the implied growth rate of small
c  phytoplankton, which gave a reasonably good fit to the global
c  observational synthesis of Dunne (2009). This is consistent
c  with the findings of Jin et al. (GBC,2006).

          CaCO3_uptake(i,j,k) = phy_sm_local(i,j,k)*phi_sm
     &           *expkT(i,j,k)*mu(i,j,k)*CatoP

         ENDIF
        ENDDO
       ENDDO
      ENDDO

c ---------------------------------------------------------------------
c  Nutrients export/remineralization, CaCO3 export/dissolution
c
c  The flux at the bottom of a grid cell equals
C  Fb = (Ft + prod*dz) / (1 + zremin*dz)
C  where Ft is the flux at the top, and prod*dz is the integrated
C  production of new sinking particles within the layer.
C  Ft = 0 in the first layer.

C$TAF LOOP = parallel
       DO j=jmin,jmax
C$TAF LOOP = parallel
        DO i=imin,imax

C  Initialize upper flux

        POPflux_u            = 0. _d 0
        PFEflux_u            = 0. _d 0
        CaCO3flux_u          = 0. _d 0

        DO k=1,Nr

C Initialization here helps taf

         Fe_ads_org(i,j,k)    = 0. _d 0

c  check if we are at a bottom cell

         bottomlayer = 1
          IF (k.LT.Nr) THEN
           IF (hFacC(i,j,k+1,bi,bj).GT.0) THEN
c  not a bottom cell
            bottomlayer = 0
           ENDIF
          ENDIF

         IF ( hFacC(i,j,k,bi,bj).gt.0. _d 0 ) THEN

C  Sinking speed is evaluated at the bottom of the cell

          depth_l=-rF(k+1)
          IF (depth_l .LE. wsink0z)  THEN
           wsink = wsink0_2d(i,j,bi,bj)
          ELSE
           wsink = wsinkacc * (depth_l - wsink0z) + wsink0_2d(i,j,bi,bj)
          ENDIF

C  Nutrient remineralization lengthscale
C  Not an e-folding scale: this term increases with remineralization.

          zremin = gamma_POM_2d(i,j,bi,bj) * ( PTR_O2(i,j,k)**2 /
     &               (k_O2**2 + PTR_O2(i,j,k)**2) * (1-remin_min)
     &               + remin_min )/(wsink + epsln)

C  Calcium remineralization relaxed toward the inverse of the
C  ca_remin_depth constant value as the calcite saturation approaches 0.

          zremin_caco3 = 1. _d 0/ca_remin_depth*(1. _d 0 - min(1. _d 0,
     &               omegaC(i,j,k,bi,bj) + epsln ))

C  POM flux leaving the cell

          POPflux_l = (POPflux_u+P_spm(i,j,k)*drF(k)
     &           *hFacC(i,j,k,bi,bj))/(1+zremin*drF(k)
     &           *hFacC(i,j,k,bi,bj))

C  CaCO3 flux leaving the cell

          CaCO3flux_l = (caco3flux_u+CaCO3_uptake(i,j,k)*drF(k)
     &           *hFacC(i,j,k,bi,bj))/(1+zremin_caco3*drF(k)
     &           *hFacC(i,j,k,bi,bj))

C  Begin iron uptake calculations by determining ligand bound and free iron.
C  Both forms are available for biology, but only free iron is scavenged
C  onto particles and forms colloids.

          kFe_eq_lig = kFe_eq_lig_max-(kFe_eq_lig_max-kFe_eq_lig_min)
     &             *(irr_inst(i,j,k)**2
     &             /(kFe_eq_lig_irr**2+irr_inst(i,j,k)**2))
     &             *max(epsln,min(1. _d 0,(PTR_FE(i,j,k)
     &             -kFe_eq_lig_Femin)/
     &             (PTR_FE(i,j,k)+epsln)*1.2 _d 0))

C  Use the quadratic equation to solve for binding between iron and ligands

          FreeFe = (-(1+kFe_eq_lig*(ligand-PTR_FE(i,j,k)))
     &            +((1+kFe_eq_lig*(ligand-PTR_FE(i,j,k)))**2+4*
     &            kFe_eq_lig*PTR_FE(i,j,k))**(0.5))/(2*
     &            kFe_eq_lig)

C  Iron scavenging does not occur in anoxic water (Fe2+ is soluble), so set
C  FreeFe = 0 when anoxic.  FreeFe should be interpreted the free iron that
C  participates in scavenging.

          IF (PTR_O2(i,j,k) .LT. oxic_min)  THEN
           FreeFe = 0. _d 0
          ENDIF

C  Two mechanisms for iron uptake, in addition to biological production:
C  colloidal scavenging and scavenging by organic matter.

           Fe_ads_inorg(i,j,k) =
     &       kFe_inorg*(max(1. _d -8,FreeFe))**(1.5)

C  Scavenging of iron by organic matter:
C  The POM value used is the bottom boundary flux. This does not occur in
C  oxic waters, but FreeFe is set to 0 in such waters earlier.

           IF ( POPflux_l .GT. 0. _d 0 ) THEN
            Fe_ads_org(i,j,k) =
     &           kFE_org*(POPflux_l/(epsln + wsink)
     &             * MasstoN*NtoP)**(0.58)*FreeFe
           ENDIF

C  If water is oxic then the iron is remineralized normally. Otherwise
C  it is completely remineralized (fe 2+ is soluble, but unstable
C  in oxidizing environments).

           IF ( PTR_O2(i,j,k) .LT. oxic_min ) THEN
            PFEflux_l = 0. _d 0
           ELSE
            PFEflux_l = (PFEflux_u+(Fe_spm(i,j,k)+Fe_ads_inorg(i,j,k)
     &            +Fe_ads_org(i,j,k))*drF(k)
     &            *hFacC(i,j,k,bi,bj))/(1+zremin*drF(k)
     &            *hFacC(i,j,k,bi,bj))
           ENDIF

C  Nutrient accumulation in a cell is given by the biological production
C  (and instant remineralization) of particulate organic matter
C  plus flux thought upper interface minus flux through lower interface.
C  If this layer is adjacent to bottom topography or it is the deepest
C  cell of the domain, then remineralize/dissolve in this grid cell
C  i.e. do not subtract off lower boundary fluxes when calculating remin

C  For the deepest cells:

          IF (bottomlayer.EQ.1) THEN

           POPflux_l   = 0. _d 0
           CACO3flux_l = 0. _d 0

C  Efflux Fed out of sediments
C  The phosphate flux hitting the bottom boundary
C  is used to scale the return of iron to the water column.
C  Maximum value added for numerical stability.

           POC_sed = POPflux_l * CtoP

           Fe_sed(i,j,k) = max(epsln, FetoC_sed * POC_sed * recip_drF(k)
     &                   * recip_hFacC(i,j,k,bi,bj))

          ELSE

           Fe_sed(i,j,k) = 0. _d 0

          ENDIF

          P_reminp(i,j,k) = (POPflux_u + P_spm(i,j,k) * drF(k)
     &                      * hFacC(i,j,k,bi,bj) - POPflux_l)
     &                      * recip_drF(k) * recip_hFacC(i,j,k,bi,bj)

          CaCO3_diss(i,j,k) = (CaCO3flux_u + CaCO3_uptake(i,j,k)
     &                      * drF(k) * hFacC(i,j,k,bi,bj) - CaCO3flux_l)
     &                      * recip_drF(k) * recip_hFacC(i,j,k,bi,bj)

          Fe_sed(i,j,k) = 0. _d 0

          Fe_reminp(i,j,k) = (PFEflux_u + (Fe_spm(i,j,k)
     &                     + Fe_ads_inorg(i,j,k) + Fe_ads_org(i,j,k))
     &                     * drF(k) * hFacC(i,j,k,bi,bj) - PFEflux_l)
     &                     * recip_drF(k) * recip_hFacC(i,j,k,bi,bj)

          Fe_reminsum(i,j,k) = Fe_reminp(i,j,k) + Fe_sed(i,j,k)
     &                       - Fe_ads_org(i,j,k) - Fe_ads_inorg(i,j,k)

C  Added the burial flux of sinking particulate iron here as a
C  diagnostic, needed to calculate mass balance of iron.
C  this is calculated last for the deepest cell

           Fe_burial(i,j) = PFEflux_l

C  Prepare the tracers for the next layer down

           POPflux_u   = POPflux_l
           PFEflux_u   = PFEflux_l
           CaCO3flux_u = CaCO3flux_l

         ENDIF

        ENDDO
       ENDDO
      ENDDO

C-----------------------------------------------------------
C  add all tendencies

       DO k=1,Nr
         DO j=jmin,jmax
          DO i=imin,imax

C  Dissolved organic matter slow remineralization

#ifdef BLING_NO_NEG
           DOP_remin(i,j,k) = MAX(maskC(i,j,k,bi,bj)*gamma_DOP
     &                    *PTR_DOP(i,j,k),0. _d 0)
#else
           DOP_remin(i,j,k) = maskC(i,j,k,bi,bj)*gamma_DOP
     &                    *PTR_DOP(i,j,k)
#endif

c  Tendencies

           G_PO4(i,j,k) = -P_uptake(i,j,k) + P_recycle(i,j,k)
     &                    + DOP_remin(i,j,k)
     &                    + (1-phi_DOM) * P_reminp(i,j,k)

           G_DOP(i,j,k) = DOP_prod(i,j,k) - DOP_remin(i,j,k)
     &                   + phi_DOM * P_reminp(i,j,k)

           if ( PTR_O2(i,j,k) .GT. oxic_min ) then
             G_O2(i,j,k) = -O2toP*G_PO4(i,j,k)
           else
             G_O2(i,j,k) = 0. _d 0
           endif

           G_FE(i,j,k) = - Fe_uptake(i,j,k) + Fe_reminsum(i,j,k)
     &                   + Fe_recycle(i,j,k)

C  Carbon system diagnostics
C  Change in DIC from primary production, from recycling and
C  remineralization, change in carbonate ions concentration
C  from biological activity:

           G_CaCO3(i,j,k) = CaCO3_diss(i,j,k) - CaCO3_uptake(i,j,k)

           NPP(i,j,k) = P_uptake(i,j,k) * CtoP

           NCP(i,j,k) = -G_PO4(i,j,k)*CtoP

           G_ALK(i,j,k) = 2. _d 0*G_CaCO3(i,j,k) - NtoP*G_PO4(i,j,k)

           G_DIC(i,j,k) = -NCP(i,j,k) + G_CaCO3(i,j,k)

c  Carbon flux diagnostic

           POC_flux(i,j,k) = CtoP * P_spm(i,j,k)

c  for diagnostics: convert to mol C/m3

           Phy_lg_local(i,j,k) = Phy_lg_local(i,j,k) * CtoP
           Phy_sm_local(i,j,k) = Phy_sm_local(i,j,k) * CtoP

c  for constraints determine POC, assuming that phytoplankton carbon
c  is 30% of POC

           poc(i,j,k,bi,bj) = (Phy_lg_local(i,j,k) +
     &                        Phy_sm_local(i,j,k)) * 3.33333 _d 0

          ENDDO
         ENDDO
       ENDDO

      RETURN
      END
