#include "BATS_OPTIONS.h"

CBOP
      SUBROUTINE BATS_BIO(PTR_BAC,PTR_BAN,PTR_BAP,PTR_DETC,PTR_DETN,
     I                    PTR_DETP,PTR_DVMZC,PTR_DVMZN,PTR_DVMZP,
     I                    PTR_DOC,PTR_DON, PTR_DOP,
     I                    PTR_MZC, PTR_MZN, PTR_MZP,
     I                    PTR_NH4, PTR_NO3, PTR_PO4,
     I                    PTR_PRTC, PTR_PRTN, PTR_PRTP,
     I                    PTR_SDOC, PTR_SDON, PTR_SDOP,
     I                    PTR_SPC, PTR_SPchl, PTR_SPN, PTR_SPP,
     I                    PTR_TRC, PTR_TRchl, PTR_TRN, PTR_TRP,
     I                    PTR_UNC, PTR_UNchl, PTR_UNN, PTR_UNP,
     O                    G_BAC, G_BAN, G_BAP,
     O                    G_DETC, G_DETN, G_DETP,
     O                    G_DVMZC, G_DVMZN, G_DVMZP,
     O                    G_DOC, G_DON, G_DOP,
     O                    G_MZC, G_MZN, G_MZP,
     O                    G_NH4, G_NO3, G_PO4,
     O                    G_PRTC, G_PRTN, G_PRTP,
     O                    G_SDOC, G_SDON, G_SDOP,
     O                    G_SPC, G_SPchl, G_SPN, G_SPP,
     O                    G_TRC, G_TRchl, G_TRN, G_TRP,
     O                    G_UNC, G_UNchl, G_UNN, G_UNP,
     I                    bi, bj, imin, imax, jmin, jmax,
     I                    myTime, myIter, myThid)

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
C     PTR_SPX       :: microphytoplankton carbon, chl, nitrogen, phosphorous
C     PTR_TRX       :: Trichodesmium carbon, chl, nitrogen, phosphorous
C     PTR_UNX       :: unicellular N2-fixer carbon, nitrogen, phosphorous
C     PTR_BAX       :: bacterial carbon, nitrogen, phosphorous
C     PTR_PRTX      :: protozoan carbon, nitrogen, phosphorous
C     PTR_MZX       :: metazoan carbon, nitrogen, phosphorous
C     PTR_DVMZX     :: vertically migrating metazoan carbon, nitrogen, phosphorous
C     PTR_DETX      :: detrital carbon, nitrogen, phosphorous
C     PTR_NH4       :: ammonium concentration
C     PTR_NO3       :: nitrate concentration
C     PTR_PO4       :: phosphate concentration
C     PTR_DOX       :: dissolved organic carbon, nitrogen, phosphorous
C     PTR_SDOX      :: semi-labile dissolved organic carbon, nitrogen, phosphorous
      double precision :: PTR_SPC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_SPchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_SPN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_SPP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_TRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_TRchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_TRN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_TRP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_UNC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_UNchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_UNN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_UNP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_BAC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_BAN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_BAP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_PRTC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_PRTN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_PRTP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_MZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_MZN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_MZP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_DVMZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_DVMZN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_DVMZP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_DETC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_DETN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_DETP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_NH4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_DOC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_SDOC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_SDON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      double precision :: PTR_SDOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C     === Output ===
C     G_xxx                :: tendency term for tracer xxx

      _RL  G_SPC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_SPchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_SPN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_SPP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_TRC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_TRchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_TRN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_TRP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_UNC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_UNchl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_UNN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_UNP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_BAC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_BAN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_BAP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_PRTC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_PRTN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_PRTP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_MZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_MZN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_MZP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DVMZC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DVMZN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DVMZP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DETC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DETN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DETP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
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
       double precision :: ae,mu_SP,alpha_SP,a_SP,v_SPn,k_nh4SP,k_no3SP
       double precision :: v_SPp,k_po4SP, zeta, theta, r_excrSP_1
       double precision :: r_excrSP_2,r_pomSP
       double precision :: mu_TR,alpha_TR,a_TR,k_nh4TR,v_TRn,k_no3TR
       double precision :: v_TRp, k_po4TR,mu_pickTRpo4, zeta_nf
       double precision :: r_excrTR_1,r_excrTR_n,r_excrTR_2,r_pomTR
       double precision :: mu_UN,alpha_UN,r_SDOM,k_DOM,mu_BA,b_BAresp
       double precision :: r_BAadju,r_BAremi,r_BArefr,f_BAslct,
     &                     r_BAresp_1, r_BAresp_min
       double precision :: r_BAresp_max, r_BAmort, mu_PRT,g_sp
       double precision :: g_ba,r_PRTex,f_exPRTldom
       double precision :: r_PRTresp_1,r_PRTresp_2,r_PRTadju,r_PRTremi
       double precision :: r_pomPRT,mu_MZ,g_prt,g_tr,r_MZex
       double precision :: f_exMZldom,r_MZresp_1,r_MZresp_2,r_MZadju,
     &                     r_MZremi
       double precision :: r_MZpom,r_MZrefr,r_MZremv,f_HZsdom,f_HZpom,
     &                     r_SDOMrefr, q_refrDOM_n, q_refrDOM_p
       double precision :: q_POM_n,q_POM_p,r_nitrf,remin_prf_n,
     &                     remin_prf_p,wnsvo,remin,k_pom,g_det,f_dvm
       ! local fixed parameters
       double precision :: q_SP_min_n,q_SP_min_p,q_SP_max_n,
     &                     q_SP_max_p,q_SP_rdf_n
       double precision :: q_SP_rdf_p,v_UNn,v_UNp,q_UN_min0_n,
     &                     q_UN_min0_p
       double precision :: q_UN_max0_n,k_nh4UN,k_no3UN,k_po4UN
       double precision :: r_excrUN_1,r_excrUN_n,r_excrUN_2,r_pomUN,
     &                     q_BA_n,q_BA_p
       double precision :: g_un,q_PRT_n,q_PRT_p
       double precision :: q_MZ_n,q_MZ_p,Tref,a_UN
       ! local variables for temperature effect function
       double precision :: Tfunc
       ! local variables in the SP module
       double precision :: Nfunc_sp_n, Nfunc_sp_p, Pc_SPmax, 
     &                     growSPnh4,growSPno3
       double precision :: V_SPmax_n, V_SPmax_p, respSP,  growSPchl, 
     &                     grazSPchl, pomSPchl
       double precision :: growSPc,growSPn,growSPp,excrSP_1c,excrSP_1n,
     &                     excrSP_1p
       double precision :: excrSP_2c,excrSP_2n,excrSP_2p,pomSPc,pomSPn,
     &                     pomSPp
       double precision :: grazSPc,grazSPn,grazSPp
       ! local variables in the TR module
       double precision :: q_TR_min_n,q_TR_min_p,q_TR_max_n,q_TR_max_p,
     &                     q_TR_rdf_n,q_TR_rdf_p
       double precision :: growTRnh4,growTRno3,growTRnf,maxTRnf,
     &                     excrTR_nh4,growTRpo4,pickTRpo4
       double precision :: growTRc,growTRn,growTRp,excrTR_1c,excrTR_1n,
     &                     excrTR_1p,excrTR_2c,excrTR_2n,excrTR_2p
       double precision :: pomTRc,pomTRn,pomTRp,grazTRc,grazTRn,grazTRp
       double precision :: Nfunc_tr_n, Nfunc_tr_p, Pc_TRmax,V_TRmax_n, 
     &                     V_TRmax_p,respTR,growTRchl,grazTRchl,pomTRchl
       ! local variables in the UN module
       double precision :: q_UN_min_n,q_UN_min_p
       double precision :: q_UN_max_n,q_UN_max_p,q_UN_rdf_n,q_UN_rdf_p
       double precision :: growUNnh4,growUNno3,growUNnf,maxUNnf,
     &                     excrUN_nh4
       double precision :: growUNc,growUNn,growUNp
       double precision :: excrUN_1c,excrUN_1n,excrUN_1p,excrUN_2c,
     &                     excrUN_2n,excrUN_2p
       double precision :: pomUNc,pomUNn,pomUNp,grazUNc,grazUNn,grazUNp
       double precision :: Nfunc_un_n, Nfunc_un_p, Pc_UNmax,V_UNmax_n,
     &                     V_UNmax_p
       double precision :: respUN,  growUNchl, grazUNchl, pomUNchl
       ! local variables in the BA module
       double precision :: ALC,ASC,Nfunc_ba_n,Nfunc_ba_p
       double precision :: growBAldoc,growBAldon,growBAldop,growBAsdoc
       double precision :: growBAsdon,growBAsdop
       double precision :: growBAnh4,growBAno3,growBApo4,respBA,
     &                     fluxBAnh4,fluxBAno3,fluxBApo4
       double precision :: growBAc,growBAn,growBAp,refrBAc,refrBAn,
     &                     refrBAp
       double precision :: excrBAc,excrBAn,excrBAp,remiBAn,remiBAp
       double precision :: grazBAc,grazBAn,grazBAp,mortBAc,
     &                     mortBAn,mortBAp
       ! local variables in the PRT module
       double precision :: growPRTc,growPRTn,growPRTp
       double precision :: excrPRTldomc,excrPRTldomn,excrPRTldomp
       double precision :: excrPRTsdomc,excrPRTsdomn,excrPRTsdomp
       double precision :: excrPRTsdom2c,excrPRTsdom2n,excrPRTsdom2p
       double precision :: remiPRTn,remiPRTp,pomPRTc,pomPRTn,pomPRTp
       double precision :: grazPRTc,grazPRTn,grazPRTp,respPRT
       ! local variables in the MZ module
       double precision :: growMZc,growMZn,growMZp
       double precision :: excrMZldomc,excrMZldomn,excrMZldomp
       double precision :: excrMZsdomc,excrMZsdomn,excrMZsdomp
       double precision :: excrMZsdom2c,excrMZsdom2n,excrMZsdom2p
       double precision :: remiMZn,remiMZp,pomMZc,pomMZn,pomMZp
       double precision :: refrMZc,refrMZn,refrMZp,remvMZc,
     &                     remvMZn,remvMZp
       double precision :: pomHZc,pomHZn,pomHZp,excrHZsdomc
       double precision :: excrHZsdomn,excrHZsdomp
       double precision :: remiHZn,remiHZp,respMZ
       ! local variables in the DOM module
       double precision :: refrSDOMc,refrSDOMn,refrSDOMp
       ! local variables in the DET module
       double precision :: disDETc,disDETn,disDETp
       ! local variables in the DIN module
       double precision :: nitrf
       ! local variables in newly added processes
       double precision :: growBApoc, growBApon, growBApop, 
     &                     grazDETc, grazDETn, grazDETp
       double precision :: c0, c1
      
! ma   p bioparams to local copies
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
   
       c0 = 0.0
       c1 = 1.0

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

            Tref = 25.d0 ! Reference Temperature for Function of Temperatue Effects
            ! Temperature Effects
            Tfunc = exp(-ae*( 1/theta(i,j,k,bi,bj)
     &              +273.15)-1/(Tref+273.15))
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
        
c --        -------------------------------------------------------------------
c  M        icrophytoplankton processes
            Nfunc_sp_n = (PTR_SPN(i,j,k)/PTR_SPC(i,j,k) - q_SP_min_n)/
     &                   (q_SP_rdf_n - q_SP_min_n)
            Nfunc_sp_p = (PTR_SPP(i,j,k)/PTR_SPC(i,j,k) - q_SP_min_p)/
     &                   (q_SP_rdf_p - q_SP_min_p)
            temp = min(Nfunc_sp_n, Nfunc_sp_p)
            temp = min(temp, c1)
            temp = max(temp, c0)
            Pc_SPmax = mu_SP * temp
        
            ! Light Limitation
            IF ((Pc_SPmax.gt.c0).and.(PTR_SPC(i,j,k).gt.c0)) THEN
                growSPc = PTR_SPC(i,j,k) * Pc_SPmax * 
     &                    (1-exp(-alpha_SP*PTR_SPchl(i,j,k)/
     &            PTR_SPC(i,j,k)*irr_inst/Pc_SPmax))*exp(-a_SP*irr_inst)
            ELSE
                growSPc = c0
            END IF
            ! Nutrient uptake
            V_SPmax_n = max(c0, (q_SP_max_n - PTR_SPN(i,j,k)/
     &                      PTR_SPC(i,j,k) )/(q_SP_max_n -q_SP_rdf_n))
            V_SPmax_n = min(c1, V_SPmax_n)
            growSPnh4 = PTR_SPC(i,j,k) * v_SPn * V_SPmax_n * 
     &                  PTR_NH4(i,j,k)/(PTR_NH4(i,j,k)+k_nh4SP+
     &                  PTR_NO3(i,j,k) * k_nh4SP/k_no3SP)
            growSPno3 = PTR_SPC(i,j,k) * v_SPn * V_SPmax_n * 
     &                  PTR_NO3(i,j,k)/(PTR_NO3(i,j,k)+k_no3SP+
     &                  PTR_NH4(i,j,k) * k_no3SP/k_nh4SP)
            growSPn = growSPnh4 + growSPno3
            
            V_SPmax_p = max(c0, (q_SP_max_p - PTR_SPP(i,j,k)/
     &                  PTR_SPC(i,j,k) )/(q_SP_max_p-q_SP_rdf_p))
            V_SPmax_p = min(c1, V_SPmax_p)
            growSPp = PTR_SPC(i,j,k) * v_SPp * V_SPmax_p * 
     &                PTR_PO4(i,j,k) /( PTR_PO4(i,j,k) + k_po4SP)
            
            respSP = growSPno3*zeta
        
        
! Ch        lorophyll
            IF (irr_inst .gt. c0) THEN
                growSPchl = theta * growSPn * growSPc/
     &          (alpha_SP*PTR_SPchl(i,j,k)*irr_inst*exp(-a_SP*irr_inst))
            ELSE
                growSPchl = c0
            END IF
            
            ! SP excretion (could be also considered as mortality)
            excrSP_1c = r_excrSP_1 * PTR_SPC(i,j,k) + r_excrSP_2 * 
     &                  growSPc * 0.75 !             Passive
            excrSP_1n = r_excrSP_1 * PTR_SPN(i,j,k)
            excrSP_1p = r_excrSP_1 * PTR_SPP(i,j,k)
            temp = MAX(c1-PTR_SPN(i,j,k)/PTR_SPC(i,j,k)/
     &          q_SP_rdf_n,c1-PTR_SPP(i,j,k)/PTR_SPC(i,j,k)/q_SP_rdf_p)
            excrSP_2c = 0.5 * PTR_SPC(i,j,k) * MAX(temp,c0)
            IF (excrSP_2c > c0) THEN
                temp = MAX(c0, c1 - PTR_SPP(i,j,k)/PTR_SPN(i,j,k)/
     &                 (q_SP_rdf_p/q_SP_rdf_n))
                temp1 = MAX(c0, c1 - PTR_SPN(i,j,k)/PTR_SPP(i,j,k)/
     &                  (q_SP_rdf_n/q_SP_rdf_p))
                excrSP_2n = 0.5 * MIN(0.25d0 * PTR_SPN(i,j,k) * temp,
     &                      excrSP_2c*q_SP_rdf_n)
                excrSP_2p = 0.5 * MIN(0.25d0 * PTR_SPP(i,j,k) * temp1,
     &                      excrSP_2c*q_SP_rdf_p)
            ELSE
                excrSP_2n = c0
                excrSP_2p = c0
            END IF
            excrSP_2c = excrSP_2c + r_excrSP_2 * growSPc * 0.25
        
            ! Aggregation of SP
            pomSPc = r_pomSP * PTR_SPC(i,j,k)*PTR_SPC(i,j,k)
            pomSPn = pomSPc * PTR_SPN(i,j,k) / PTR_SPC(i,j,k)
            pomSPp = pomSPc * PTR_SPP(i,j,k) / PTR_SPC(i,j,k)
            pomSPchl = pomSPn * PTR_SPchl(i,j,k) / PTR_SPN(i,j,k)
            ! SP grazed
            grazSPc = mu_PRT * PTR_PRTC(i,j,k) * PTR_SPC(i,j,k) * 
     &             PTR_SPC(i,j,k)/(PTR_SPC(i,j,k) * PTR_SPC(i,j,k) + 
     &             g_sp * g_sp + 
     &             PTR_UNC(i,j,k)*PTR_UNC(i,j,k)/g_un/g_un*g_sp*g_sp + 
     &             PTR_BAC(i,j,k)*PTR_BAC(i,j,k)/g_ba/g_ba*g_sp*g_sp)
            grazSPn = grazSPc * PTR_SPN(i,j,k) /PTR_SPC(i,j,k)
            grazSPp = grazSPc * PTR_SPP(i,j,k) /PTR_SPC(i,j,k)
            grazSPchl = grazSPn * PTR_SPchl(i,j,k) / PTR_SPN(i,j,k)
            ! SP derivs
            G_SPC(i,j,k) = (growSPc - excrSP_1c - excrSP_2c - 
     &                     grazSPc - pomSPc)/SecPerDay
            G_SPN(i,j,k) = (growSPn - excrSP_1n - excrSP_2n - 
     &                     grazSPn - pomSPn)/SecPerDay
            G_SPP(i,j,k) = (growSPp - excrSP_1p - excrSP_2p - 
     &                     grazSPp - pomSPp)/SecPerDay
            G_SPchl(i,j,k) = (growSPchl - grazSPchl - 
     &                      pomSPchl)/ SecPerDay
        
            !-----------------------------------------------------------------------
            !      Trichodesmium Processes
            !-----------------------------------------------------------------------
            ! Primary Production
            ! Nutrient quota Limitation
            Nfunc_tr_n = (PTR_TRN(i,j,k)/PTR_TRC(i,j,k) - q_TR_min_n) / 
     &                                 (q_TR_rdf_n - q_TR_min_n)
            Nfunc_tr_p = (PTR_TRP(i,j,k)/PTR_TRC(i,j,k) - q_TR_min_p) / 
     &                                (q_TR_rdf_p - q_TR_min_p)
            temp = min(Nfunc_tr_n, Nfunc_tr_p)
            temp = min(temp, c1)
            temp = max(temp, c0)
            Pc_TRmax = mu_TR * temp
             ! Light Limitation
            IF ((Pc_TRmax.gt.c0).and.(PTR_TRC(i,j,k).gt.c0)) THEN
                growTRc = PTR_TRC(i,j,k) * Pc_TRmax * (c1 -
     &          exp(-alpha_TR*PTR_TRchl(i,j,k)/PTR_TRC(i,j,k)*
     &          irr_inst/Pc_TRmax))* exp(-a_TR*irr_inst)
            ELSE
                growTRc = c0
            END IF
            ! Nutrient uptake
            V_TRmax_n = max(c0, (q_TR_max_n - PTR_TRN(i,j,k)/
     &                  PTR_TRC(i,j,k) ) /(q_TR_max_n -     q_TR_rdf_n))
            V_TRmax_n = min(c1, V_TRmax_n)
            growTRnh4 = PTR_TRC(i,j,k) * v_TRn * V_TRmax_n * 
     &                      PTR_NH4(i,j,k) / (PTR_NH4(i,j,k) + 
     &                  k_nh4TR + PTR_NO3(i,j,k)  * k_nh4TR/k_no3TR)
            growTRno3 = PTR_TRC(i,j,k) * v_TRn * V_TRmax_n *
     &                  PTR_NO3(i,j,k) / (PTR_NO3(i,j,k) + k_no3TR + 
     &                  PTR_NH4(i,j,k)  * k_no3TR/k_nh4TR)
            IF (theta(i,j,k,bi,bj) < 20.0) THEN
                maxTRnf = c0
            ELSE
                maxTRnf = MAX(c0, 
     &                    ((PTR_TRC(i,j,k) + growTRc - growTRno3 * zeta)
     &            * q_TR_max_n 
     &            - PTR_TRN(i,j,k) - growTRno3 - growTRnh4) 
     &             / (c1 + zeta_nf * q_TR_max_n) * V_TRmax_n * Tfunc)
            END IF
            growTRn = MIN( PTR_TRC(i,j,k) * v_TRn * V_TRmax_n, 
     &               growTRnh4 + growTRno3 + maxTRnf )
            growTRnf = growTRn - growTRnh4 - growTRno3
            
            V_TRmax_p = max(c0, (q_TR_max_p - PTR_TRP(i,j,k)/
     &                  PTR_TRC(i,j,k) ) /(q_TR_max_p -     q_TR_rdf_p))
            V_TRmax_p = min(c1, V_TRmax_p)
            growTRpo4 = PTR_TRC(i,j,k) * v_TRp * V_TRmax_p * 
     &                  PTR_PO4(i,j,k) /( PTR_PO4(i,j,k) + k_po4TR)
            pickTRpo4 = mu_pickTRpo4 *
     &                  MAX( PTR_TRC(i,j,k)* (q_TR_max_p+q_TR_rdf_P)/2 -
     &                  PTR_TRP(i,j,k),   c0 )
            growTRp = growTRpo4 + pickTRpo4
            ! Respiration
            respTR = growTRno3 *  zeta + growTRnf * zeta_nf
            ! Chlorophyll
            IF (irr_inst .gt. c0) THEN
                growTRchl = theta * growTRn * 
     &                   growTRc /(alpha_TR*PTR_TRchl(i,j,k)*
     &                   irr_inst*exp(-a_TR*irr_inst))
            ELSE
                growTRchl = c0
            END IF
            ! TR excretion (mortality)
            excrTR_1c = r_excrTR_1 * PTR_TRC(i,j,k) + r_excrTR_2 * 
     &                  growTRc * 0.75 !         Passive
            excrTR_1n = r_excrTR_1 * PTR_TRN(i,j,k)
     &                   + 0.5* r_excrTR_n * growTRnf * 
     &                  min(c1,Nfunc_tr_n)
            excrTR_1p = r_excrTR_1 * PTR_TRP(i,j,k)
            excrTR_nh4 = 0.5* r_excrTR_n * growTRnf * 
     &                   min(c1, Nfunc_tr_n)
            
            temp = MAX(c1-PTR_TRN(i,j,k)/PTR_TRC(i,j,k)/q_TR_rdf_n,
     &             c1-PTR_TRP(i,j,k)/PTR_TRC(i,j,k)/q_TR_rdf_p)
            excrTR_2c = r_excrTR_2 * PTR_TRC(i,j,k) * MAX(temp, c0)
            IF (excrTR_2c > c0) THEN
                temp = MAX(c0, c1 - PTR_TRP(i,j,k)/PTR_TRN(i,j,k)/
     &                 (q_TR_rdf_p/q_TR_rdf_n))
                temp1 = MAX(c0, c1 - PTR_TRN(i,j,k)/PTR_TRP(i,j,k)/
     &                  (q_TR_rdf_n/q_TR_rdf_p))
                excrTR_2n = r_excrTR_2 * MIN(0.25d0 * PTR_TRN(i,j,k) * 
     &                      temp,excrTR_2c *   q_TR_rdf_n)
                excrTR_2p = r_excrTR_2 * MIN(0.25d0 * PTR_TRP(i,j,k) * 
     &                      temp1,excrTR_2c *  q_TR_rdf_p)
            ELSE
                excrTR_2n = c0
                excrTR_2p = c0
            END IF
            excrTR_2c = excrTR_2c + r_excrTR_2 * growTRc * 0.25
            ! Aggregation of TR
            pomTRc = r_pomTR * PTR_TRC(i,j,k)*PTR_TRC(i,j,k)
            pomTRn = pomTRc * PTR_TRN(i,j,k) / PTR_TRC(i,j,k)
            pomTRp = pomTRc * PTR_TRP(i,j,k) / PTR_TRC(i,j,k)
            pomTRchl = pomTRc * PTR_TRchl(i,j,k) / PTR_TRC(i,j,k)
            ! TR grazed
            grazTRc = mu_MZ * PTR_MZC(i,j,k) * PTR_TRC(i,j,k) * 
     &               PTR_TRC(i,j,k) 
     &           / (PTR_TRC(i,j,k) * PTR_TRC(i,j,k) + g_tr * g_tr + 
     &            PTR_PRTC(i,j,k)*PTR_PRTC(i,j,k)/g_prt/g_prt*g_tr*g_tr+
     &            PTR_DETC(i,j,k)*PTR_DETC(i,j,k)/g_det/g_det*g_tr*g_tr)
            grazTRn = grazTRc * PTR_TRN(i,j,k)/PTR_TRC(i,j,k)
            grazTRp = grazTRc * PTR_TRP(i,j,k)/PTR_TRC(i,j,k)
            grazTRchl = grazTRc * PTR_TRchl(i,j,k)/PTR_TRC(i,j,k)
            ! New TR
            G_TRC(i,j,k) = (growTRc-excrTR_1c-excrTR_2c-grazTRc-pomTRc)/
     &            SecPerDay
            G_TRN(i,j,k) = (growTRn-excrTR_1n-excrTR_2n-grazTRn-pomTRn
     &                      - excrTR_nh4)/ SecPerDay
            G_TRP(i,j,k) = (growTRp-excrTR_1p-excrTR_2p-grazTRp-pomTRp)/
     &            SecPerDay
            G_TRchl(i,j,k) =(growTRchl-grazTRchl-pomTRchl)/ SecPerDay
        
        
            !-----------------------------------------------------------------------
            !      Unicellular N2-fixers Processes
            !-----------------------------------------------------------------------
            ! Primary Production
            ! Nutrient quota Limitation
            Nfunc_un_n = (PTR_UNN(i,j,k)/PTR_UNC(i,j,k) - q_UN_min_n) / 
     &                                 (q_UN_rdf_n - q_UN_min_n)
            Nfunc_un_p = (PTR_UNP(i,j,k)/PTR_UNC(i,j,k) - q_UN_min_p) / 
     &                                 (q_UN_rdf_p - q_UN_min_p)
            temp = min(Nfunc_un_n, Nfunc_un_p)
            temp = min(temp, c1)
            temp = max(temp, c0)
            Pc_UNmax = mu_UN * temp
             ! Light Limitation
             IF ((Pc_UNmax.gt.c0).and.(PTR_UNC(i,j,k).gt.c0)) THEN
                growUNc = PTR_UNC(i,j,k) * Pc_UNmax * (c1 - 
     &         exp(-alpha_UN*PTR_UNchl(i,j,k)/PTR_UNC(i,j,k)*irr_inst/
     &         Pc_UNmax)) * exp(-a_UN*irr_inst)
             ELSE
                growUNc = c0
             END IF
             ! Nutrient uptake
            V_UNmax_n = max(c0, (q_UN_max_n - PTR_UNN(i,j,k)/
     &                  PTR_UNC(i,j,k) ) /(q_UN_max_n - q_UN_rdf_n))
            V_UNmax_n = min(c1, V_UNmax_n)
            growUNnh4 = PTR_UNC(i,j,k) * v_UNn * V_UNmax_n * 
     &                  PTR_NH4(i,j,k) / (PTR_NH4(i,j,k) + k_nh4UN + 
     &                  PTR_NO3(i,j,k)  * k_nh4UN/k_no3UN)
            growUNno3 = PTR_UNC(i,j,k) * v_UNn * V_UNmax_n *
     &                     PTR_NO3(i,j,k) / (PTR_NO3(i,j,k) + k_no3UN + 
     &                     PTR_NH4(i,j,k)  * k_no3UN/k_nh4UN)
            !maxUNnf = MAX(c0, &
            !            PTR_UNC(i,j,k) * q_UN_max_n * mu_UNnf * (c1 -         exp(-alpha_UNnf  * (irr_inst - minPARunnf) / mu_UNnf)) )
            maxUNnf = MAX(c0, 
     &                  ((PTR_UNC(i,j,k) - growUNc - growUNno3 * zeta) *
     &                  q_UN_max_n
     &            - PTR_UNN(i,j,k) - growUNno3 - growUNnh4)
                      / (c1 + zeta_nf * q_UN_max_n) * V_UNmax_n * Tfunc)
            
            growUNn = MIN( PTR_UNC(i,j,k) * v_UNn * V_UNmax_n,
                    growUNnh4 + growUNno3 + maxUNnf )
            growUNnf = growUNn - growUNnh4 - growUNno3
            
            V_UNmax_p = max(c0, (q_UN_max_p - PTR_UNP(i,j,k)/
     &                  PTR_UNC(i,j,k) ) /   (q_UN_max_p - q_UN_rdf_p))
            V_UNmax_p = min(c1, V_UNmax_p)
            growUNp = PTR_UNC(i,j,k) * v_UNp * V_UNmax_p * 
     &                      PTR_PO4(i,j,k) /( PTR_PO4(i,j,k) + k_po4UN)
            respUN = growUNno3 *  zeta + growUNnf * zeta_nf
             ! Chlorophyll
            IF (irr_inst .gt. c0) THEN
                growUNchl = theta * growUNn *
     &                      growUNc /(alpha_UN*PTR_UNchl(i,j,k)*
     &                      irr_inst*exp(-a_UN*irr_inst))
            ELSE
                growUNchl = c0
            END IF
            ! UN excretion
            excrUN_1c = r_excrUN_1 * PTR_UNC(i,j,k) + 
     &                  r_excrUN_2 * growUNc * 0.75 !             Passive
            excrUN_1n = r_excrUN_1 * PTR_UNN(i,j,k)
     &                  + 0.5 * r_excrUN_n * growUNnf * 
     &                  min(c1,         Nfunc_un_n)
            excrUN_1p = r_excrUN_1 * PTR_UNP(i,j,k)
            excrUN_nh4 = 0.5*r_excrUN_n*growUNnf * min(c1, Nfunc_un_n)
            
            temp = MAX(c1-PTR_UNN(i,j,k)/PTR_UNC(i,j,k)/q_UN_rdf_n,
     &             c1-PTR_UNP(i,j,k)/PTR_UNC(i,j,k)/q_UN_rdf_p)
            excrUN_2c = r_excrUN_2 * PTR_UNC(i,j,k) * MAX(temp,c0)
            IF (excrUN_2c > c0) THEN
                temp = MAX(c0, c1 - PTR_UNP(i,j,k)/PTR_UNN(i,j,k) / 
     &                 (q_UN_rdf_p/q_UN_rdf_n))
                temp1 = MAX(c0, c1 - PTR_UNN(i,j,k)/PTR_UNP(i,j,k) /
     &                 (q_UN_rdf_n/q_UN_rdf_p))
                excrUN_2n = r_excrUN_2 * MIN(0.25d0 * PTR_UNN(i,j,k) * 
     &                 temp, excrUN_2c  * q_UN_rdf_n)
                excrUN_2p = r_excrUN_2 * MIN(0.25d0 * PTR_UNP(i,j,k) * 
     &                 temp1,excrUN_2c     * q_UN_rdf_p)
            ELSE
                excrUN_2n = c0
                excrUN_2p = c0
            END IF
            excrUN_2c = excrUN_2c + r_excrUN_2 * growUNc * 0.25
            
            ! Aggregation of UN
            pomUNc = r_pomUN * PTR_UNC(i,j,k)*PTR_UNC(i,j,k)
            pomUNn = pomUNc * PTR_UNN(i,j,k) / PTR_UNC(i,j,k)
            pomUNp = pomUNc * PTR_UNP(i,j,k) / PTR_UNC(i,j,k)
            pomUNchl = pomUNc * PTR_UNchl(i,j,k) / PTR_UNC(i,j,k)
            ! UN grazed
            grazUNc = mu_PRT * PTR_PRTC(i,j,k) * PTR_UNC(i,j,k) * 
     &               PTR_UNC(i,j,k)
     &           / (PTR_UNC(i,j,k) * PTR_UNC(i,j,k) + g_un * g_un +
     &              PTR_SPC(i,j,k)*PTR_SPC(i,j,k)/g_sp/g_sp*g_un*g_un +
                   PTR_BAC(i,j,k)*PTR_BAC(i,j,k)/g_ba/g_ba*g_un*g_un)
            grazUNn = grazUNc * PTR_UNN(i,j,k)/PTR_UNC(i,j,k)
            grazUNp = grazUNc * PTR_UNP(i,j,k)/PTR_UNC(i,j,k)
            grazUNchl = grazUNc * PTR_UNchl(i,j,k)/PTR_UNC(i,j,k)
            ! New UN
            G_UNC(i,j,k) = (growUNc - excrUN_1c - excrUN_2c - grazUNc - 
     &                     pomUNc)/             SecPerDay
            G_UNN(i,j,k) = (growUNn - excrUN_1n - excrUN_2n - grazUNn - 
     &                     pomUNn
     &                      - excrUN_nh4)/ SecPerDay
            G_UNP(i,j,k) = (growUNp - excrUN_1p - excrUN_2p - 
     &                      grazUNp - pomUNp)/             SecPerDay
            G_UNchl(i,j,k) = (growUNchl-grazUNchl - pomUNchl)/ SecPerDay
           
            !-----------------------------------------------------------------------
            !      Bacterial Processes (PA-bacteria from BFM-CMCC-ESM2, Lovato et         al.   2022)
            !-----------------------------------------------------------------------
            ! 1. Gross Grow
            ! Maximum possible C amount for bacterial use
            ALC = PTR_DOC(i,j,k)
            !temp = MIN(c1, exp(b_SDONlabi *         (PTR_SDON(i,j,k)/PTR_SDOC(i,j,k)/q_BA_n - c1)) )
            !temp = MIN(temp, exp(b_SDOPlabi *         (PTR_SDOP(i,j,k)/PTR_SDOC(i,j,k)/q_BA_p - c1)) )
            ASC = PTR_SDOC(i,j,k) * r_SDOM
            ! Carbon Usage
            Nfunc_ba_n = PTR_BAN(i,j,k)/PTR_BAC(i,j,k)/q_BA_n
            Nfunc_ba_p = PTR_BAP(i,j,k)/PTR_BAC(i,j,k)/q_BA_p
            temp = min(Nfunc_ba_n, Nfunc_ba_p)
            temp = min(temp, c1)
            growBAldoc = mu_BA * PTR_BAC(i,j,k) * temp * ALC
     &                          / (ALC+ k_DOM + ASC)
            growBAsdoc = mu_BA * PTR_BAC(i,j,k) * temp * ASC
     &                              / (ASC+ k_DOM + ALC)
            growBApoc  = mu_BA * PTR_BAC(i,j,k) * PTR_DETC(i,j,k)
     &                    / ( PTR_DETC(i,j,k) + k_POM * PTR_BAC(i,j,k) ) !          particle-attached bacteria
            ! DON and DOP usage
            growBAldon = growBAldoc/PTR_DOC(i,j,k)*PTR_DON(i,j,k) ! available         labile N
            growBAldop = growBAldoc/PTR_DOC(i,j,k)*PTR_DOP(i,j,k) ! available         labile P
            growBAsdon = growBAsdoc * min(q_BA_n,
     &                  (PTR_SDON(i,j,k)/PTR_SDOC(i,j,k) + f_BAslct
     &            /Nfunc_ba_n*(q_BA_n-PTR_SDON(i,j,k)/PTR_SDOC(i,j,k))))
     &             growBAsdop = growBAsdoc * min(q_BA_p,
     &                  (PTR_SDOP(i,j,k)/PTR_SDOC(i,j,k) +f_BAslct/
     &             Nfunc_ba_p*(q_BA_p-PTR_SDOP(i,j,k)/PTR_SDOC(i,j,k))))
            growBApon  = growBApoc/PTR_DETC(i,j,k)*PTR_DETN(i,j,k)   ! available         labile     N
            growBApop  = growBApoc/PTR_DETC(i,j,k)*PTR_DETP(i,j,k)   ! available         labile     P
            ! inorganic nutrients uptake
            growBAnh4 = growBAldon / PTR_DON(i,j,k) * PTR_NH4(i,j,k) * min(c1,1/Nfunc_ba_n)
            if (Nfunc_ba_n.lt.c1) then
                growBAno3 = min(0.1 * (growBAldon + growBAsdon) / (
     &          PTR_DON(i,j,k) + PTR_SDON(i,j,k))
     &           * PTR_NO3(i,j,k) * min(c1, 1/Nfunc_ba_n),
     &            (growBAldon + growBAsdon) / (PTR_DON(i,j,k) +
     &           PTR_SDON(i,j,k))*(PTR_NO3(i,j,k)  + PTR_NH4(i,j,k)) -
     &           growBAnh4)
                growBAno3 = max(c0, growBAno3)
            else
                growBAno3 = c0
            end if
            growBApo4 = growBAldop / PTR_DOP(i,j,k) * 
     &                  PTR_PO4(i,j,k)*min(c1,1/Nfunc_ba_p)
            ! Bacteria gross growth
            growBAc = growBAldoc + growBAsdoc + growBApoc
            growBAn = growBAldon + growBAsdon + growBAnh4 + 
     &                growBAno3 + growBApon
            growBAp = growBAldop + growBAsdop + growBApo4 + growBApop
            ! 2. respiration
            respBA = r_BAresp_1 * PTR_BAC(i,j,k) + zeta * growBAno3 +
     &           (r_BAresp_min + (r_BAresp_max -r_BAresp_min)*
     &           EXP(-b_BAresp*growBAc) ) * growBAc
            ! 3. excreting refractory DOM
            refrBAc = r_BArefr * PTR_BAC(i,j,k)
            refrBAn = q_refrDOM_n * refrBAc
            refrBAp = q_refrDOM_p * refrBAc
            ! 4. excreting semi-labile DOM and regenerating DIN
            IF ( (PTR_BAC(i,j,k) < PTR_BAN(i,j,k)/q_BA_n) .AND.
     &            (PTR_BAC(i,j,k) < PTR_BAP(i,j,k)/q_BA_p) ) THEN  !Cabon in short
                 excrBAc = c0
                 excrBAn = c0
                 excrBAp = c0
                 remiBAn = r_BAremi * (PTR_BAN(i,j,k) - 
     &                     PTR_BAC(i,j,k) * q_BA_n)
                 remiBAp = r_BAremi * (PTR_BAP(i,j,k) - 
     &                     PTR_BAC(i,j,k) * q_BA_p)
            ELSE IF ( (PTR_BAC(i,j,k) > PTR_BAN(i,j,k)/q_BA_n) .AND.
     &            (PTR_BAP(i,j,k)/q_BA_p > PTR_BAN(i,j,k)/q_BA_n) ) THEN !Nitrogen         in    short
                 excrBAc = r_BAadju*(PTR_BAC(i,j,k)-
     &                     PTR_BAN(i,j,k)/q_BA_n)
                 excrBAn = c0
                 excrBAp = r_BAadju * (PTR_BAP(i,j,k) - PTR_BAN(i,j,k)/
     &                     q_BA_n *         q_BA_p)
                 remiBAn = c0
                 remiBAp = c0
            ELSE !Phosphorus in short
                 excrBAc = r_BAadju * (PTR_BAC(i,j,k) - 
     &                     PTR_BAP(i,j,k)/q_BA_p)
                 excrBAn = r_BAadju * (PTR_BAN(i,j,k) - 
     &                     PTR_BAP(i,j,k)/q_BA_p *q_BA_n)
                 excrBAp = c0
                 remiBAn = c0
                 remiBAp = c0
            END IF
            !6. removal by grazing
            grazBAc = mu_PRT * PTR_PRTC(i,j,k) * PTR_BAC(i,j,k) * 
     &                PTR_BAC(i,j,k)
     &           / (PTR_BAC(i,j,k) * PTR_BAC(i,j,k) + g_ba * g_ba +
     &              PTR_SPC(i,j,k)*PTR_SPC(i,j,k)/g_sp/g_sp*g_ba*g_ba +
                   PTR_UNC(i,j,k)*PTR_UNC(i,j,k)/g_un/g_un*g_ba*g_ba)
            grazBAn = grazBAc / PTR_BAC(i,j,k) * PTR_BAN(i,j,k)
            grazBAp = grazBAc / PTR_BAC(i,j,k) * PTR_BAP(i,j,k)
            !6b. Mortality due to viruses
            mortBAc = r_BAmort * PTR_BAC(i,j,k)
            mortBAn = r_BAmort * PTR_BAN(i,j,k)
            mortBAp = r_BAmort * PTR_BAP(i,j,k)
            !7. BA Derivs
            G_BAC(i,j,k) = (growBAc - refrBAc - excrBAc- grazBAc
     &                      - respBA - mortBAc)/ SecPerDay
            G_BAN(i,j,k) = (growBAn-refrBAn-excrBAn-remiBAn-grazBAn-
     &                     mortBAn) / SecPerDay
            G_BAP(i,j,k) = (growBAp-refrBAp-excrBAp-remiBAp - grazBAp -
     &                      mortBAp) / SecPerDay
            !8. Flux of inorganic nutrients through bacteria
            fluxBAnh4 = growBAnh4 - remiBAn
            fluxBAno3 = growBAno3
            fluxBApo4 = growBApo4 - remiBAp
        
            !-----------------------------------------------------------------------
            !      Protozoan Processes
            !-----------------------------------------------------------------------
            ! 1. gross growth
            growPRTc = grazSPc + grazBAc + grazUNc
            growPRTn = grazSPn + grazBAn + grazUNn
            growPRTp = grazSPp + grazBAp + grazUNp
            ! 2. DOM excretion
            excrPRTldomc = f_exPRTldom * r_PRTex * growPRTc
            excrPRTldomn = f_exPRTldom * r_PRTex * growPRTn
            excrPRTldomp = f_exPRTldom * r_PRTex * growPRTp
            excrPRTsdomc = (c1 - f_exPRTldom) * r_PRTex * growPRTc
            excrPRTsdomn = (c1 - f_exPRTldom) * r_PRTex * growPRTn *
     &                      PTR_PRTN(i,j,k)/PTR_PRTC(i,j,k)/q_PRT_n
            !                                *         EXP(3*(PTR_PRTN(i,j,k)/PTR_PRTC(i,j,k)/q_PRT_n-c1))
            excrPRTsdomp = (c1 - f_exPRTldom) * r_PRTex * growPRTp * 
     &                    PTR_PRTP(i,j,k)/PTR_PRTC(i,j,k)/q_PRT_p
            !                               *         EXP(3*(PTR_PRTP(i,j,k)/PTR_PRTC(i,j,k)/q_PRT_p-c1))
            ! 3. respiration
            respPRT = r_PRTresp_1 * PTR_PRTC(i,j,k) + 
     &                 r_PRTresp_2 * growPRTc
            ! 4. adjust body stoichiometry by excreting semi-labile DOM
            temp = MAX(c1 - PTR_PRTN(i,j,k)/PTR_PRTC(i,j,k)/q_PRT_n, 
     &                  c1 - PTR_PRTP(i,j,k)/PTR_PRTC(i,j,k)/q_PRT_p)
            temp = MAX(c0, temp)
            excrPRTsdom2c = r_PRTadju * PTR_PRTC(i,j,k) * temp
            excrPRTsdom2n = 0.5d0*excrPRTsdom2c * 
     &                            PTR_PRTN(i,j,k)/PTR_PRTC(i,j,k)
            excrPRTsdom2p = 0.5d0*excrPRTsdom2c * 
     &                            PTR_PRTP(i,j,k)/PTR_PRTC(i,j,k)
            ! 5. adjust body stoichiometry by remineralizing inorganic nutrients
            remiPRTn = MAX(r_PRTremi*(PTR_PRTN(i,j,k)-
     &                      q_PRT_n * PTR_PRTC(i,j,k)),
     &                      r_PRTremi*(PTR_PRTN(i,j,k)-
     &                 q_PRT_n/q_PRT_p*PTR_PRTP        (i,j,k)) )
            remiPRTn = MAX(c0, remiPRTn)
            remiPRTp = MAX(r_PRTremi*(PTR_PRTP(i,j,k)-
     &                     q_PRT_p * PTR_PRTC(i,j,k)),
     &                      r_PRTremi*(PTR_PRTP(i,j,k)-
     &                 q_PRT_p/q_PRT_n*PTR_PRTN        (i,j,k)) )
            remiPRTp = MAX(c0, remiPRTp)
            ! POM production
            pomPRTc = r_pomPRT * growPRTc
            pomPRTn = pomPRTc * q_POM_n
            pomPRTp = pomPRTc * q_POM_p
            ! 6. removal by microzooplankton
            grazPRTc = mu_MZ * PTR_MZC(i,j,k) * 
     &              PTR_PRTC(i,j,k) * PTR_PRTC(i,j,k)
     &           / (PTR_PRTC(i,j,k) * PTR_PRTC(i,j,k) + g_prt * g_prt +
     &              PTR_TRC(i,j,k)*PTR_TRC(i,j,k)/g_tr/g_tr*g_prt*g_prt+
     &          PTR_DETC(i,j,k)*PTR_DETC(i,j,k)/g_det/g_det*g_prt*g_prt)
            grazPRTn = grazPRTc * PTR_PRTN(i,j,k) / PTR_PRTC(i,j,k)
            grazPRTp = grazPRTc * PTR_PRTP(i,j,k) / PTR_PRTC(i,j,k)
            ! 7. new PRT
            G_PRTC(i,j,k) = (growPRTc - excrPRTldomc - 
     &                      excrPRTsdomc -excrPRTsdom2c-grazPRTc-pomPRTc
     &                      - respPRT)/ SecPerDay
            G_PRTN(i,j,k) = (growPRTn - excrPRTldomn - excrPRTsdomn - 
     &                      excrPRTsdom2n -    remiPRTn - grazPRTn
     &                      - pomPRTn)/ SecPerDay
            G_PRTP(i,j,k) = (growPRTp - excrPRTldomp - excrPRTsdomp -
     &                      excrPRTsdom2p  -remiPRTp - grazPRTp
     &                      - pomPRTp)/ SecPerDay
            
            !-----------------------------------------------------------------------
            !      Metazoan Processes
            !-----------------------------------------------------------------------
            ! removal of detritus by detrivore metazoa
            grazDETc = mu_MZ * PTR_MZC(i,j,k) * PTR_DETC(i,j,k) * 
     &              PTR_DETC(i,j,k)
     &           / (PTR_DETC(i,j,k) * PTR_DETC(i,j,k) + g_det * g_det +
     &              PTR_PRTC(i,j,k)*PTR_PRTC(i,j,k)/g_prt/g_prt*
     &              g_det*g_det +
     &              PTR_TRC(i,j,k)*PTR_TRC(i,j,k)/g_tr/g_tr*g_det*g_det)
            grazDETn = grazDETc * PTR_DETN(i,j,k) / PTR_DETC(i,j,k)
            grazDETp = grazDETc * PTR_DETP(i,j,k) / PTR_DETC(i,j,k)
            
            ! 1. gross growth
            growMZc = grazPRTc + grazTRc + grazDETc
            growMZn = grazPRTn + grazTRn + grazDETn
            growMZp = grazPRTp + grazTRp + grazDETp
            ! 2. DOM excretion
            excrMZldomc = f_exMZldom * r_MZex * growMZc
            excrMZldomn = f_exMZldom * r_MZex * growMZn
            excrMZldomp = f_exMZldom * r_MZex * growMZp
            excrMZsdomc = (c1 - f_exMZldom) * r_MZex * growMZc
            excrMZsdomn = (c1 - f_exMZldom) * r_MZex * growMZn *
     &                    PTR_MZN(i,j,k)/PTR_MZC(i,j,k)/q_MZ_n
            !                                   *         EXP(3*(PTR_MZN(i,j,k)/PTR_MZC(i,j,k)/q_MZ_n-c1))
            excrMZsdomp = (c1 - f_exMZldom) * r_MZex * growMZp *
     &                    PTR_MZP(i,j,k)/PTR_MZC(i,j,k)/q_MZ_p
            !                                   *         EXP(3*(PTR_MZP(i,j,k)/PTR_MZC(i,j,k)/q_MZ_p-c1))
            
            ! 3. respiration
            respMZ = r_MZresp_1 * PTR_MZC(i,j,k) + r_MZresp_2 * growMZc
            ! 4. adjust body stoichiometry by excreting semi-labile DOM
            temp = MAX(c1 - PTR_MZN(i,j,k)/PTR_MZC(i,j,k)/q_MZ_n, c1 - 
     &              PTR_MZP(i,j,k)/PTR_MZC(i,j,k)/q_MZ_p)
            temp = MAx(temp, c0)
            excrMZsdom2c = r_MZadju * PTR_MZC(i,j,k) * temp
            excrMZsdom2n= excrMZsdom2c * PTR_MZN(i,j,k)/PTR_MZC(i,j,k)*0.5d0
            excrMZsdom2p= excrMZsdom2c * PTR_MZP(i,j,k)/PTR_MZC(i,j,k)*0.5d0
            ! 5. adjust body stoichiometry by remineralizing inorganic nutrients
            remiMZn = MAX(r_MZremi*(PTR_MZN(i,j,k)-q_MZ_n * 
     &                   PTR_MZC(i,j,k)),
     &         r_MZremi*(PTR_MZN(i,j,k)-q_MZ_n/q_MZ_p*PTR_MZP(i,j,k)) )
            remiMZn = MAX(remiMZn, c0)
            remiMZp = MAX(r_MZremi*(PTR_MZP(i,j,k)-q_MZ_p * 
     &                    PTR_MZC(i,j,k)),
     &          r_MZremi*(PTR_MZP(i,j,k)-q_MZ_p/q_MZ_n*PTR_MZN(i,j,k)) )
            remiMZp = MAX(remiMZp, c0)
            ! 6. indissolved POM and refractory DOM production
            pomMZc = r_MZpom * growMZc
            pomMZn = q_POM_n * pomMZc
            pomMZp = q_POM_p * pomMZc
            refrMZc = r_MZrefr * growMZc
            refrMZn = q_refrDOM_n * refrMZc
            refrMZp = q_refrDOM_p * refrMZc
            ! 7. removal by higher-level zooplankton
            remvMZc = r_MZremv * PTR_MZC(i,j,k) * PTR_MZC(i,j,k)
            remvMZn = remvMZc / PTR_MZC(i,j,k) * PTR_MZN(i,j,k)
            remvMZp = remvMZc / PTR_MZC(i,j,k) * PTR_MZP(i,j,k)
            pomHZc = f_HZpom * remvMZc
            pomHZn = f_HZpom * remvMZn
            pomHZp = f_HZpom * remvMZp
            excrHZsdomc = remvMZc * f_HZsdom
            excrHZsdomn = remvMZn * f_HZsdom
            excrHZsdomp = remvMZp * f_HZsdom
            remiHZn = remvMZn - excrHZsdomn - pomHZn
            remiHZp = remvMZp - excrHZsdomp - pomHZp
            ! 8. new MZ
            G_MZC(i,j,k) = (growMZc - excrMZldomc -excrMZsdomc - 
     &                      excrMZsdom2c
     &                - pomMZc - refrMZc - remvMZc - respMZ) / SecPerDay
            G_MZN(i,j,k) = (growMZn - excrMZldomn -excrMZsdomn - remiMZn
     &             -         excrMZsdom2n
     &                     - pomMZn - refrMZn - remvMZn)/ SecPerDay
            G_MZP(i,j,k) = (growMZp - excrMZldomp -excrMZsdomp - remiMZp
     &                   -         excrMZsdom2p
     &                   - pomMZp - refrMZp - remvMZp)/ SecPerDay
            ! 9. DVM MZ
            G_DVMZC(i,j,k) = PTR_MZC(i,j,k)*f_dvm
            G_DVMZN(i,j,k)=PTR_DVMZC(i,j,k)*PTR_MZN(i,j,k)/
     &                     PTR_MZC(i,j,k)
            G_DVMZP(i,j,k)=PTR_DVMZC(i,j,k)*PTR_MZP(i,j,k)/
     &                     PTR_MZC(i,j,k)
            
            !-----------------------------------------------------------------------
            !      Detritus Processes
            !-----------------------------------------------------------------------
            disDETc = remin * PTR_DETC(i,j,k)
            disDETn = remin * PTR_DETN(i,j,k) * remin_prf_n
            disDETp = remin * PTR_DETP(i,j,k) * remin_prf_p
            
            G_DETC(i,j,k) = (pomSPc + pomTRc + pomUNc + pomPRTc + pomMZc
     &                     +pomHZc-disDETc-growBApoc-grazDETc)/SecPerDay
            G_DETN(i,j,k) = (pomSPn + pomTRn + pomUNn + pomPRTn + pomMZn
     &    + pomHZn -          disDETn - growBApon - grazDETn)/ SecPerDay
            G_DETP(i,j,k) = (pomSPp + pomTRp + pomUNp + pomPRTp + pomMZp
     &    + pomHZp -          disDETp - growBApop - grazDETp)/ SecPerDay
            
            !-----------------------------------------------------------------------
            !      Inorganic Nutrients Processes
            !-----------------------------------------------------------------------
            nitrf = r_nitrf * PTR_NH4(i,j,k)
            G_NH4(i,j,k) = (remiPRTn + remiMZn + remiHZn + excrTR_nh4 + excrUN_nh4
     &                     - growSPnh4 - growTRnh4 - growUNnh4 - fluxBAnh4 - nitrf)/             SecPerDay
            G_NO3(i,j,k) = (-growSPno3 - growTRno3 - growUNno3 - fluxBAno3 +         nitrf)/     SecPerDay
            G_PO4(i,j,k) = (remiPRTp + remiMZp + remiHZp - growSPp - growTRpo4
     &                     - growUNp - fluxBApo4)/ SecPerDay
            
            !-----------------------------------------------------------------------
            !      Dissolved Organic Matter (DOM) Processes
            !-----------------------------------------------------------------------
            temp = MIN(PTR_SDON(i,j,k)/PTR_SDOC(i,j,k)/q_refrDOM_n,         PTR_SDOP(i,j,k)/PTR_SDOC(i,j,k)/q_refrDOM_p)
            refrSDOMc = r_SDOMrefr * PTR_SDOC(i,j,k) * EXP(c1 - temp)
            refrSDOMn = refrSDOMc * q_refrDOM_n
            refrSDOMp = refrSDOMc * q_refrDOM_p
            temp = MAX(PTR_SDOC(i,j,k)-PTR_SDON(i,j,k)/q_refrDOM_n,         PTR_SDOC(i,j,k)-PTR_SDOP(i,j,k)/q_refrDOM_p)
            temp = MAX(temp, c0)
            refrSDOMc = refrSDOMc + temp
            G_DOC(i,j,k) = (excrSP_1c + excrTR_1c + excrUN_1c + excrPRTldomc
     &                        + excrMZldomc - growBAldoc + mortBAc)/ SecPerDay
            G_DON(i,j,k) = (excrSP_1n + excrTR_1n + excrUN_1n + excrPRTldomn
     &                        + excrMZldomn - growBAldon + mortBAn)/ SecPerDay
            G_DOP(i,j,k) = (excrSP_1p + excrTR_1p + excrUN_1p + excrPRTldomp
     &                        + excrMZldomp - growBAldop + mortBAp)/ SecPerDay
            G_SDOC(i,j,k) = (excrSP_2c + excrTR_2c + excrUN_2c + excrBAc +         excrPRTsdomc     + excrPRTsdom2c
     &                      + excrMZsdomc + excrMZsdom2c + excrHZsdomc + disDETc -           growBAsdoc - refrSDOMc)/ SecPerDay
            G_SDON(i,j,k) = (excrSP_2n + excrTR_2n + excrUN_2n + excrBAn +         excrPRTsdomn     + excrPRTsdom2n
     &                      + excrMZsdomn + excrMZsdom2n + excrHZsdomn + disDETn -           growBAsdon - refrSDOMn)/ SecPerDay
            G_SDOP(i,j,k) = (excrSP_2p + excrTR_2p + excrUN_2p + excrBAp +         excrPRTsdomp     + excrPRTsdom2p
     &                      + excrMZsdomp + excrMZsdom2p + excrHZsdomp + disDETp -           growBAsdop - refrSDOMp)/ SecPerDay
        
         ENDIF
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
