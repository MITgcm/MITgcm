!----------------------------------------------------------------------------
!     CVS:$Id: derivs_mod.F90,v 1.4 2005/01/11 20:08:18 duse Exp $
!     Yawei Luo 2009/01/14
!     CVS:$Name:  $
!----------------------------------------------------------------------------

module derivs_mod

contains

  subroutine derivs(y,dydtt,dydtt_diag,istep,iz,bioparams)

    use const, only : c0,c1,SecPerDay
    use eco_common, only : rlt,wnsvflag
    use eco_params, only : iae, imu_SP,ialpha_SP,ia_SP,iv_SPn,ik_nh4SP,ik_no3SP,&
          iv_SPp,ik_po4SP,izeta,itheta, ir_excrSP_1,ir_excrSP_2,ir_pomSP,imu_TR,ialpha_TR,ia_TR,&
          iv_TRn,ik_nh4TR,ik_no3TR,iv_TRp,ik_po4TR,imu_pickTRpo4, izeta_nf,&
          ir_excrTR_1,ir_excrTR_n,ir_excrTR_2,ir_pomTR,imu_UN,&
          ialpha_UN,ir_SDOM,ik_DOM,imu_BA,& !ib_SDONlabi,ib_SDOPlabi,
          ib_BAresp,ir_BAadju,ir_BAremi,ir_BArefr,&
          if_BAslct,ir_BAresp_1, ir_BAresp_min, ir_BAresp_max,ir_BAmort, & 
          imu_PRT,ig_sp,ig_ba,&
          ir_PRTex,if_exPRTldom,ir_PRTresp_1,&
          ir_PRTresp_2,ir_PRTadju,ir_PRTremi,&
          ir_pomPRT,imu_MZ,ig_prt,ig_tr,&
          ir_MZex,if_exMZldom,ir_MZresp_1,ir_MZresp_2,&
          ir_MZadju,ir_MZremi,ir_MZpom,ir_MZrefr,ir_MZremv,if_HZsdom,if_HZpom,ir_SDOMrefr,&
          iq_refrDOM_n, iq_refrDOM_p,iq_POM_n,iq_POM_p,&
          ir_nitrf,iremin_prf_n,iremin_prf_p,iwnsvo,iremin,&
          ik_POM,ig_det,if_dvm
    use eco_params, only : NumStateVar,NumDiagVar
    use eco_params, only : iSPc,iSPn,iSPp,iTRc,iTRn,iTRp,iUNc,iUNn,iUNp, &
                            iBAc,iBAn,iBAp,iPRTc,iPRTn,iPRTp,iMZc,iMZn,iMZp, &
                            iLDOMc,iLDOMn,iLDOMp,iSDOMc,iSDOMn,iSDOMp, &
                            iDETc,iDETn,iDETp,iNH4,iNO3,iPO4,iSPchl,iTRchl,iUNchl,iDVMZc,iDVMZn,iDVMZp
    use eco_params, only : iPP, iprBAc, &
				   igrowSPc, igrowSPnh4, igrowSPno3, &
				   igrowSPn, igrowSPp, &
				   iexcrSP_1c, iexcrSP_1n, iexcrSP_1p, &
				   iexcrSP_2c, iexcrSP_2n, iexcrSP_2p, &
				   ipomSPc, ipomSPn, ipomSPp, &
				   igrazSPc, igrazSPn, igrazSPp, & 
				   igrowTRc, igrowTRnh4, igrowTRno3, igrowTRnf, &
				   igrowTRn, igrowTRpo4, ipickTRpo4, igrowTRp, &
				   iexcrTR_1c, iexcrTR_1n, iexcrTR_1p, iexcrTR_nh4, &
				   iexcrTR_2c, iexcrTR_2n, iexcrTR_2p, &
				   ipomTRc, ipomTRn, ipomTRp, &
				   igrazTRc, igrazTRn, igrazTRp, &
				   igrowUNc, igrowUNnh4, igrowUNno3, igrowUNnf, &
				   igrowUNn, igrowUNp, &
				   iexcrUN_1c, iexcrUN_1n, iexcrUN_1p, iexcrUN_nh4, &
				   iexcrUN_2c, iexcrUN_2n, iexcrUN_2p, &
				   ipomUNc, ipomUNn, ipomUNp, &
				   igrazUNc, igrazUNn, igrazUNp, &
				   igrowBAldoc, igrowBAldon, igrowBAldop, &
				   igrowBAsdoc, igrowBAsdon, igrowBAsdop, &
				   igrowBAnh4, igrowBAno3, igrowBApo4, &
				   igrowBAc, igrowBAn, igrowBAp, irespBA, &
				   irefrBAc, irefrBAn, irefrBAp, &
				   iexcrBAc, iexcrBAn, iexcrBAp, &
				   iremiBAn, iremiBAp, &
				   igrazBAc, igrazBAn, igrazBAp, &
				   imortBAc, imortBAn, imortBAp, &
				   ifluxBAnh4, ifluxBApo4, &
				   igrowPRTc, igrowPRTn, igrowPRTp, irespPRT, &
				   iexcrPRTldomc, iexcrPRTldomn, iexcrPRTldomp, &
				   iexcrPRTsdomc, iexcrPRTsdomn, iexcrPRTsdomp, &
				   iexcrPRTsdom2c, iexcrPRTsdom2n, iexcrPRTsdom2p, &
				   iremiPRTn, iremiPRTp, &
				   ipomPRTc, ipomPRTn, ipomPRTp, &
				   igrazPRTc, igrazPRTn, igrazPRTp, &
				   igrowMZc, igrowMZn, igrowMZp, &
				   irespMZ, &
				   iexcrMZldomc, iexcrMZldomn, iexcrMZldomp, &
				   iexcrMZsdomc, iexcrMZsdomn, iexcrMZsdomp, &
				   iexcrMZsdom2c, iexcrMZsdom2n, iexcrMZsdom2p, &
				   iremiMZn, iremiMZp, &
				   irefrMZc, irefrMZn, irefrMZp, &
				   ipomMZc, ipomMZn, ipomMZp, &
				   iremvMZc, iremvMZn, iremvMZp, &
				   ipomHZc, ipomHZn, ipomHZp, &
				   iexcrHZsdomc, iexcrHZsdomn, iexcrHZsdomp, &
				   iremiHZn, iremiHZp, &
				   idisDETc, idisDETn, idisDETp, &
				   initrf, &
				   irefrSDOMc, irefrSDOMn, irefrSDOMp, &
				   iexportc, iexportn, iexportp, &
				   irespSP, irespTR, irespUN, &
                                   igrowBApoc, igrowBApon, igrowBApop, &
                                   igrazDETc, igrazDETn, igrazDETp, iHETres  
    use forcing, only : Tdat

    implicit none

!-----------------------------------------------------------------------
!     Yawei Luo's microbial-loop and N2-fixation model
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     Arguments
!-----------------------------------------------------------------------

    double precision, dimension(:) :: y, dydtt,dydtt_diag
    integer :: istep,iz
    double precision, dimension(:) :: bioparams

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

   
    !-----------------------------------------------------------------------
    !      Calculate terms used in the ecosystem model.
    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------
    !      Initialization
    !-----------------------------------------------------------------------
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
    !-----------------------------------------------------------------------
    !      Microphytoplankton Processes
    !-----------------------------------------------------------------------
    ! Nutrient quota Limitation
    Nfunc_sp_n = (y(iSPn)/y(iSPc) - q_SP_min_n) / &
                              (q_SP_rdf_n - q_SP_min_n) 
    Nfunc_sp_p = (y(iSPp)/y(iSPc) - q_SP_min_p) / &
                              (q_SP_rdf_p - q_SP_min_p) 
    temp = min(Nfunc_sp_n, Nfunc_sp_p)
    temp = min(temp, c1)
    temp = max(temp, c0)
    Pc_SPmax = mu_SP * temp
    ! Light Limitation
    IF ((Pc_SPmax.gt.c0).and.(y(iSPc).gt.c0)) THEN
        growSPc = y(iSPc) * Pc_SPmax * (c1 - exp(-alpha_SP*y(iSPchl)/y(iSPc)*rlt/Pc_SPmax)) &
                        * exp(-a_SP*rlt)
    ELSE
        growSPc = c0
    END IF  
    ! Nutrient uptake
    V_SPmax_n = max(c0, (q_SP_max_n - y(iSPn)/y(iSPc) ) / (q_SP_max_n - q_SP_rdf_n))
    V_SPmax_n = min(c1, V_SPmax_n)
    growSPnh4 = y(iSPc) * v_SPn * V_SPmax_n * &
                   y(iNH4) / (y(iNH4) + k_nh4SP + y(iNO3) * k_nh4SP/k_no3SP)
    growSPno3 = y(iSPc) * v_SPn * V_SPmax_n * &
                   y(iNO3) / (y(iNO3) + k_no3SP + y(iNH4) * k_no3SP/k_nh4SP) 
    growSPn = growSPnh4 + growSPno3
    
    V_SPmax_p = max(c0, (q_SP_max_p - y(iSPp)/y(iSPc) ) / (q_SP_max_p - q_SP_rdf_p))
    V_SPmax_p = min(c1, V_SPmax_p)
    growSPp = y(iSPc) * v_SPp * V_SPmax_p * &
                   y(iPO4) /( y(iPO4) + k_po4SP)
     
    respSP = growSPno3 *  zeta
    

    ! Chlorophyll
    IF (rlt .gt. c0) THEN
        growSPchl = theta * growSPn * &
                        growSPc / (alpha_SP*y(iSPchl)*rlt*exp(-a_SP*rlt)) 
    ELSE
        growSPchl = c0
    END IF  
    
    ! SP excretion (could be also considered as mortality)
    excrSP_1c = r_excrSP_1 * y(iSPc) + r_excrSP_2 * growSPc * 0.75 ! Passive
    excrSP_1n = r_excrSP_1 * y(iSPn)
    excrSP_1p = r_excrSP_1 * y(iSPp)
    temp = MAX(c1-y(iSPn)/y(iSPc)/q_SP_rdf_n, c1-y(iSPp)/y(iSPc)/q_SP_rdf_p)
    excrSP_2c = 0.5 * y(iSPc) * MAX(temp,c0)
    IF (excrSP_2c > c0) THEN 
        temp = MAX(c0, c1 - y(iSPp)/y(iSPn) / (q_SP_rdf_p/q_SP_rdf_n))
        temp1 = MAX(c0, c1 - y(iSPn)/y(iSPp) / (q_SP_rdf_n/q_SP_rdf_p))
        excrSP_2n = 0.5 * MIN(0.25d0 * y(iSPn) * temp, excrSP_2c * q_SP_rdf_n) 
        excrSP_2p = 0.5 * MIN(0.25d0 * y(iSPp) * temp1, excrSP_2c * q_SP_rdf_p) 
    ELSE
        excrSP_2n = c0
        excrSP_2p = c0
    END IF
    excrSP_2c = excrSP_2c + r_excrSP_2 * growSPc * 0.25
    
    ! Aggregation of SP
    pomSPc = r_pomSP * y(iSPc)*y(iSPc)
    pomSPn = pomSPc * y(iSPn) / y(iSPc)
    pomSPp = pomSPc * y(iSPp) / y(iSPc)
    pomSPchl = pomSPn * y(iSPchl) / y(iSPn)
    ! SP grazed
    grazSPc = mu_PRT * y(iPRTc) * y(iSPc) * y(iSPc) &
        / (y(iSPc) * y(iSPc) + g_sp * g_sp + &
           y(iUNc)*y(iUNc)/g_un/g_un*g_sp*g_sp + &
           y(iBAc)*y(iBAc)/g_ba/g_ba*g_sp*g_sp)
    grazSPn = grazSPc * y(iSPn) /y(iSPc)
    grazSPp = grazSPc * y(iSPp) /y(iSPc)
    grazSPchl = grazSPn * y(iSPchl) / y(iSPn)
    ! SP derivs
    dydtt(iSPc) = (growSPc - excrSP_1c - excrSP_2c - grazSPc - pomSPc)/ SecPerDay
    dydtt(iSPn) = (growSPn - excrSP_1n - excrSP_2n - grazSPn - pomSPn)/ SecPerDay
    dydtt(iSPp) = (growSPp - excrSP_1p - excrSP_2p - grazSPp - pomSPp)/ SecPerDay
    dydtt(iSPchl) = (growSPchl - grazSPchl - pomSPchl)/ SecPerDay
    
    !-----------------------------------------------------------------------
    !      Trichodesmium Processes
    !-----------------------------------------------------------------------
    ! Primary Production
    ! Nutrient quota Limitation
    Nfunc_tr_n = (y(iTRn)/y(iTRc) - q_TR_min_n) / &
                              (q_TR_rdf_n - q_TR_min_n) 
    Nfunc_tr_p = (y(iTRp)/y(iTRc) - q_TR_min_p) / &
                              (q_TR_rdf_p - q_TR_min_p) 
    temp = min(Nfunc_tr_n, Nfunc_tr_p)
    temp = min(temp, c1)
    temp = max(temp, c0)
    Pc_TRmax = mu_TR * temp
     ! Light Limitation
    IF ((Pc_TRmax.gt.c0).and.(y(iTRc).gt.c0)) THEN 
        growTRc = y(iTRc) * Pc_TRmax * (c1 - exp(-alpha_TR*y(iTRchl)/y(iTRc)*rlt/Pc_TRmax)) &
                        * exp(-a_TR*rlt)
    ELSE
        growTRc = c0
    END IF  
    ! Nutrient uptake
    V_TRmax_n = max(c0, (q_TR_max_n - y(iTRn)/y(iTRc) ) / (q_TR_max_n - q_TR_rdf_n))
    V_TRmax_n = min(c1, V_TRmax_n)
    growTRnh4 = y(iTRc) * v_TRn * V_TRmax_n * &
                   y(iNH4) / (y(iNH4) + k_nh4TR + y(iNO3) * k_nh4TR/k_no3TR)
    growTRno3 = y(iTRc) * v_TRn * V_TRmax_n * &
                   y(iNO3) / (y(iNO3) + k_no3TR + y(iNH4) * k_no3TR/k_nh4TR) 
    IF (Tdat(iz,istep) < 20.0) THEN
        maxTRnf = c0
    ELSE 
        maxTRnf = MAX(c0, &
                 ((y(iTRc) + growTRc - growTRno3 * zeta) * q_TR_max_n &
		 - y(iTRn) - growTRno3 - growTRnh4) &
                / (c1 + zeta_nf * q_TR_max_n) * V_TRmax_n * Tfunc)
    END IF
    growTRn = MIN( y(iTRc) * v_TRn * V_TRmax_n, &
            growTRnh4 + growTRno3 + maxTRnf )
    growTRnf = growTRn - growTRnh4 - growTRno3
    
    V_TRmax_p = max(c0, (q_TR_max_p - y(iTRp)/y(iTRc) ) / (q_TR_max_p - q_TR_rdf_p))
    V_TRmax_p = min(c1, V_TRmax_p)
    growTRpo4 = y(iTRc) * v_TRp * V_TRmax_p * &
                   y(iPO4) /( y(iPO4) + k_po4TR)
    pickTRpo4 = mu_pickTRpo4 * & 
                           MAX( y(iTRc)* (q_TR_max_p+q_TR_rdf_P)/2 - y(iTRp), c0 )
    growTRp = growTRpo4 + pickTRpo4  
    ! Respiration
    respTR = growTRno3 *  zeta + growTRnf * zeta_nf
    ! Chlorophyll
    IF (rlt .gt. c0) THEN
        growTRchl = theta * growTRn * &
                        growTRc / (alpha_TR*y(iTRchl)*rlt*exp(-a_TR*rlt)) 
    ELSE
        growTRchl = c0
    END IF  
    ! TR excretion (mortality)
    excrTR_1c = r_excrTR_1 * y(iTRc) + r_excrTR_2 * growTRc * 0.75 ! Passive
    excrTR_1n = r_excrTR_1 * y(iTRn) &
                           + 0.5* r_excrTR_n * growTRnf * min(c1, Nfunc_tr_n)
    excrTR_1p = r_excrTR_1 * y(iTRp)
    excrTR_nh4 = 0.5* r_excrTR_n * growTRnf * min(c1, Nfunc_tr_n)
    
    temp = MAX(c1-y(iTRn)/y(iTRc)/q_TR_rdf_n, c1-y(iTRp)/y(iTRc)/q_TR_rdf_p)
    excrTR_2c = r_excrTR_2 * y(iTRc) * MAX(temp, c0)
    IF (excrTR_2c > c0) THEN 
        temp = MAX(c0, c1 - y(iTRp)/y(iTRn) / (q_TR_rdf_p/q_TR_rdf_n))
        temp1 = MAX(c0, c1 - y(iTRn)/y(iTRp) / (q_TR_rdf_n/q_TR_rdf_p))
        excrTR_2n = r_excrTR_2 * MIN(0.25d0 * y(iTRn) * temp, excrTR_2c * q_TR_rdf_n) 
        excrTR_2p = r_excrTR_2 * MIN(0.25d0 * y(iTRp) * temp1, excrTR_2c * q_TR_rdf_p) 
    ELSE
        excrTR_2n = c0
        excrTR_2p = c0
    END IF
    excrTR_2c = excrTR_2c + r_excrTR_2 * growTRc * 0.25
    ! Aggregation of TR
    pomTRc = r_pomTR * y(iTRc)*y(iTRc)
    pomTRn = pomTRc * y(iTRn) / y(iTRc)
    pomTRp = pomTRc * y(iTRp) / y(iTRc)
    pomTRchl = pomTRc * y(iTRchl) / y(iTRc)
    ! TR grazed
    grazTRc = mu_MZ * y(iMZc) * y(iTRc) * y(iTRc) &
        / (y(iTRc) * y(iTRc) + g_tr * g_tr + &
           y(iPRTc)*y(iPRTc)/g_prt/g_prt*g_tr*g_tr + & 
           y(iDETc)*y(iDETc)/g_det/g_det*g_tr*g_tr) 
    grazTRn = grazTRc * y(iTRn)/y(iTRc)
    grazTRp = grazTRc * y(iTRp)/y(iTRc)
    grazTRchl = grazTRc * y(iTRchl)/y(iTRc)
    ! New TR
    dydtt(iTRc) = (growTRc - excrTR_1c - excrTR_2c - grazTRc - pomTRc)/ SecPerDay
    dydtt(iTRn) = (growTRn - excrTR_1n - excrTR_2n - grazTRn - pomTRn &
                   - excrTR_nh4)/ SecPerDay
    dydtt(iTRp) = (growTRp - excrTR_1p - excrTR_2p - grazTRp - pomTRp)/ SecPerDay
    dydtt(iTRchl) = (growTRchl - grazTRchl - pomTRchl)/ SecPerDay


    !-----------------------------------------------------------------------
    !      Unicellular N2-fixers Processes
    !-----------------------------------------------------------------------
    ! Primary Production
    ! Nutrient quota Limitation
    Nfunc_un_n = (y(iUNn)/y(iUNc) - q_UN_min_n) / &
                              (q_UN_rdf_n - q_UN_min_n) 
    Nfunc_un_p = (y(iUNp)/y(iUNc) - q_UN_min_p) / &
                              (q_UN_rdf_p - q_UN_min_p) 
    temp = min(Nfunc_un_n, Nfunc_un_p)
    temp = min(temp, c1)
    temp = max(temp, c0)
    Pc_UNmax = mu_UN * temp
     ! Light Limitation
     IF ((Pc_UNmax.gt.c0).and.(y(iUNc).gt.c0)) THEN 
        growUNc = y(iUNc) * Pc_UNmax * (c1 - exp(-alpha_UN*y(iUNchl)/y(iUNc)*rlt/Pc_UNmax)) &
                        * exp(-a_UN*rlt)
     ELSE
        growUNc = c0
     END IF   
     ! Nutrient uptake
    V_UNmax_n = max(c0, (q_UN_max_n - y(iUNn)/y(iUNc) ) / (q_UN_max_n - q_UN_rdf_n))
    V_UNmax_n = min(c1, V_UNmax_n)
    growUNnh4 = y(iUNc) * v_UNn * V_UNmax_n * &
                   y(iNH4) / (y(iNH4) + k_nh4UN + y(iNO3) * k_nh4UN/k_no3UN)
    growUNno3 = y(iUNc) * v_UNn * V_UNmax_n * &
                   y(iNO3) / (y(iNO3) + k_no3UN + y(iNH4) * k_no3UN/k_nh4UN) 
    !maxUNnf = MAX(c0, &
    !            y(iUNc) * q_UN_max_n * mu_UNnf * (c1 - exp(-alpha_UNnf * (rlt - minPARunnf) / mu_UNnf)) )
    maxUNnf = MAX(c0, &
                 ((y(iUNc) - growUNc - growUNno3 * zeta) * q_UN_max_n &
		 - y(iUNn) - growUNno3 - growUNnh4) &
                / (c1 + zeta_nf * q_UN_max_n) * V_UNmax_n * Tfunc)
                
    growUNn = MIN( y(iUNc) * v_UNn * V_UNmax_n, &
            growUNnh4 + growUNno3 + maxUNnf )
    growUNnf = growUNn - growUNnh4 - growUNno3
    
    V_UNmax_p = max(c0, (q_UN_max_p - y(iUNp)/y(iUNc) ) / (q_UN_max_p - q_UN_rdf_p))
    V_UNmax_p = min(c1, V_UNmax_p)
    growUNp = y(iUNc) * v_UNp * V_UNmax_p * &
                   y(iPO4) /( y(iPO4) + k_po4UN)
    respUN = growUNno3 *  zeta + growUNnf * zeta_nf 
     ! Chlorophyll
    IF (rlt .gt. c0) THEN
        growUNchl = theta * growUNn * &
                        growUNc / (alpha_UN*y(iUNchl)*rlt*exp(-a_UN*rlt)) 
    ELSE
        growUNchl = c0
    END IF  
    ! UN excretion
    excrUN_1c = r_excrUN_1 * y(iUNc) + r_excrUN_2 * growUNc * 0.75 ! Passive
    excrUN_1n = r_excrUN_1 * y(iUNn) &
                            + 0.5 * r_excrUN_n * growUNnf * min(c1, Nfunc_un_n)
    excrUN_1p = r_excrUN_1 * y(iUNp)
    excrUN_nh4 = 0.5 * r_excrUN_n * growUNnf * min(c1, Nfunc_un_n)
    
    temp = MAX(c1-y(iUNn)/y(iUNc)/q_UN_rdf_n, c1-y(iUNp)/y(iUNc)/q_UN_rdf_p)
    excrUN_2c = r_excrUN_2 * y(iUNc) * MAX(temp,c0)
    IF (excrUN_2c > c0) THEN 
        temp = MAX(c0, c1 - y(iUNp)/y(iUNn) / (q_UN_rdf_p/q_UN_rdf_n))
        temp1 = MAX(c0, c1 - y(iUNn)/y(iUNp) / (q_UN_rdf_n/q_UN_rdf_p))
        excrUN_2n = r_excrUN_2 * MIN(0.25d0 * y(iUNn) * temp, excrUN_2c * q_UN_rdf_n) 
        excrUN_2p = r_excrUN_2 * MIN(0.25d0 * y(iUNp) * temp1, excrUN_2c * q_UN_rdf_p) 
    ELSE
        excrUN_2n = c0
        excrUN_2p = c0
    END IF
    excrUN_2c = excrUN_2c + r_excrUN_2 * growUNc * 0.25 
    
    ! Aggregation of UN
    pomUNc = r_pomUN * y(iUNc)*y(iUNc)
    pomUNn = pomUNc * y(iUNn) / y(iUNc)
    pomUNp = pomUNc * y(iUNp) / y(iUNc)
    pomUNchl = pomUNc * y(iUNchl) / y(iUNc)
    ! UN grazed
    grazUNc = mu_PRT * y(iPRTc) * y(iUNc) * y(iUNc) &
        / (y(iUNc) * y(iUNc) + g_un * g_un + &
           y(iSPc)*y(iSPc)/g_sp/g_sp*g_un*g_un + &
           y(iBAc)*y(iBAc)/g_ba/g_ba*g_un*g_un)
    grazUNn = grazUNc * y(iUNn)/y(iUNc)
    grazUNp = grazUNc * y(iUNp)/y(iUNc)
    grazUNchl = grazUNc * y(iUNchl)/y(iUNc)
    ! New UN
    dydtt(iUNc) = (growUNc - excrUN_1c - excrUN_2c - grazUNc - pomUNc)/ SecPerDay
    dydtt(iUNn) = (growUNn - excrUN_1n - excrUN_2n - grazUNn - pomUNn &
                   - excrUN_nh4)/ SecPerDay
    dydtt(iUNp) = (growUNp - excrUN_1p - excrUN_2p - grazUNp - pomUNp)/ SecPerDay
    dydtt(iUNchl) = (growUNchl - grazUNchl - pomUNchl)/ SecPerDay
       
    !-----------------------------------------------------------------------
    !      Bacterial Processes (PA-bacteria from BFM-CMCC-ESM2, Lovato et al. 2022)
    !-----------------------------------------------------------------------
    ! 1. Gross Grow
    ! Maximum possible C amount for bacterial use
    ALC = y(iLDOMc)
    !temp = MIN(c1, exp(b_SDONlabi * (y(iSDOMn)/y(iSDOMc)/q_BA_n - c1)) )
    !temp = MIN(temp, exp(b_SDOPlabi * (y(iSDOMp)/y(iSDOMc)/q_BA_p - c1)) )
    ASC = y(iSDOMc) * r_SDOM
    ! Carbon Usage
    Nfunc_ba_n = y(iBAn)/y(iBAc)/q_BA_n
    Nfunc_ba_p = y(iBAp)/y(iBAc)/q_BA_p
    temp = min(Nfunc_ba_n, Nfunc_ba_p)
    temp = min(temp, c1)
    growBAldoc = mu_BA * y(iBAc) * temp * ALC &
                          / (ALC+ k_DOM + ASC) 
    growBAsdoc = mu_BA * y(iBAc) * temp * ASC &
                          / (ASC+ k_DOM + ALC)
    growBApoc  = mu_BA * y(iBAc) * y(iDETc) &
                  / ( y(iDETc) + k_POM * y(iBAc) ) ! particle-attached bacteria
    ! DON and DOP usage
    growBAldon = growBAldoc/y(iLDOMc)*y(iLDOMn) ! available labile N
    growBAldop = growBAldoc/y(iLDOMc)*y(iLDOMp) ! available labile P
    growBAsdon = growBAsdoc * min(q_BA_n, &
               (y(iSDOMn)/y(iSDOMc) + f_BAslct/Nfunc_ba_n*(q_BA_n-y(iSDOMn)/y(iSDOMc))))
    growBAsdop = growBAsdoc * min(q_BA_p, &
               (y(iSDOMp)/y(iSDOMc) + f_BAslct/Nfunc_ba_p*(q_BA_p-y(iSDOMp)/y(iSDOMc))))
    growBApon  = growBApoc/y(iDETc)*y(iDETn)   ! available labile N
    growBApop  = growBApoc/y(iDETc)*y(iDETp)   ! available labile P
    ! inorganic nutrients uptake
    growBAnh4 = growBAldon / y(iLDOMn) * y(iNH4) * min(c1, 1/Nfunc_ba_n)
    if (Nfunc_ba_n.lt.c1) then
        growBAno3 = min(0.1 * (growBAldon + growBAsdon) / (y(iLDOMn) + y(iSDOMn)) &
                            * y(iNO3) * min(c1, 1/Nfunc_ba_n), &
			    (growBAldon + growBAsdon) / (y(iLDOMn) + y(iSDOMn)) &
                            * (y(iNO3)  + y(iNH4)) - growBAnh4)
        growBAno3 = max(c0, growBAno3)
    else
        growBAno3 = c0
    end if
    growBApo4 = growBAldop / y(iLDOMp) * y(iPO4) * min(c1, 1/Nfunc_ba_p)
    ! Bacteria gross growth
    growBAc = growBAldoc + growBAsdoc + growBApoc
    growBAn = growBAldon + growBAsdon + growBAnh4 + growBAno3 + growBApon
    growBAp = growBAldop + growBAsdop + growBApo4 + growBApop
    ! 2. respiration
    respBA = r_BAresp_1 * y(iBAc) + zeta * growBAno3 + &
        (r_BAresp_min + (r_BAresp_max - r_BAresp_min)*EXP(-b_BAresp*growBAc) ) &
        * growBAc
    ! 3. excreting refractory DOM
    refrBAc = r_BArefr * y(iBAc)
    refrBAn = q_refrDOM_n * refrBAc
    refrBAp = q_refrDOM_p * refrBAc
    ! 4. excreting semi-labile DOM and regenerating DIN
    IF ( (y(iBAc) < y(iBAn)/q_BA_n) .AND. &
         (y(iBAc) < y(iBAp)/q_BA_p) ) THEN  !Cabon in short
         excrBAc = c0
         excrBAn = c0
         excrBAp = c0
         remiBAn = r_BAremi * (y(iBAn) - y(iBAc) * q_BA_n)
         remiBAp = r_BAremi * (y(iBAp) - y(iBAc) * q_BA_p)
    ELSE IF ( (y(iBAc) > y(iBAn)/q_BA_n) .AND. &
         (y(iBAp)/q_BA_p > y(iBAn)/q_BA_n) ) THEN !Nitrogen in short
         excrBAc = r_BAadju * (y(iBAc) - y(iBAn)/q_BA_n)
         excrBAn = c0
         excrBAp = r_BAadju * (y(iBAp) - y(iBAn)/q_BA_n * q_BA_p)
         remiBAn = c0
         remiBAp = c0
    ELSE !Phosphorus in short
         excrBAc = r_BAadju * (y(iBAc) - y(iBAp)/q_BA_p)
         excrBAn = r_BAadju * (y(iBAn) - y(iBAp)/q_BA_p * q_BA_n)
         excrBAp = c0
         remiBAn = c0
         remiBAp = c0
    END IF
    !6. removal by grazing
    grazBAc = mu_PRT * y(iPRTc) * y(iBAc) * y(iBAc) &
        / (y(iBAc) * y(iBAc) + g_ba * g_ba + &
           y(iSPc)*y(iSPc)/g_sp/g_sp*g_ba*g_ba + &
           y(iUNc)*y(iUNc)/g_un/g_un*g_ba*g_ba)
    grazBAn = grazBAc / y(iBAc) * y(iBAn)
    grazBAp = grazBAc / y(iBAc) * y(iBAp)
    !6b. Mortality due to viruses
    mortBAc = r_BAmort * y(iBAc)
    mortBAn = r_BAmort * y(iBAn)
    mortBAp = r_BAmort * y(iBAp)
    !7. BA Derivs
    dydtt(iBAc) = (growBAc - refrBAc - excrBAc                   - grazBAc &
                   - respBA - mortBAc)/ SecPerDay
    dydtt(iBAn) = (growBAn - refrBAn - excrBAn - remiBAn - grazBAn - mortBAn) &
                   / SecPerDay
    dydtt(iBAp) = (growBAp - refrBAp - excrBAp - remiBAp - grazBAp - mortBAp) &
                   / SecPerDay
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
    excrPRTsdomn = (c1 - f_exPRTldom) * r_PRTex * growPRTn * y(iPRTn)/y(iPRTc)/q_PRT_n
   !                                * EXP(3*(y(iPRTn)/y(iPRTc)/q_PRT_n-c1))
    excrPRTsdomp = (c1 - f_exPRTldom) * r_PRTex * growPRTp * y(iPRTp)/y(iPRTc)/q_PRT_p
    !                               * EXP(3*(y(iPRTp)/y(iPRTc)/q_PRT_p-c1))
    ! 3. respiration
    respPRT = r_PRTresp_1 * y(iPRTc) + r_PRTresp_2 * growPRTc
    ! 4. adjust body stoichiometry by excreting semi-labile DOM
    temp = MAX(c1 - y(iPRTn)/y(iPRTc)/q_PRT_n, c1 - y(iPRTp)/y(iPRTc)/q_PRT_p)
    temp = MAX(c0, temp)
    excrPRTsdom2c = r_PRTadju * y(iPRTc) * temp
    excrPRTsdom2n = 0.5d0*excrPRTsdom2c * y(iPRTn)/y(iPRTc)
    excrPRTsdom2p = 0.5d0*excrPRTsdom2c * y(iPRTp)/y(iPRTc)
    ! 5. adjust body stoichiometry by remineralizing inorganic nutrients
    remiPRTn = MAX(r_PRTremi*(y(iPRTn)-q_PRT_n * y(iPRTc)), &
                   r_PRTremi*(y(iPRTn)-q_PRT_n/q_PRT_p*y(iPRTp)) )
    remiPRTn = MAX(c0, remiPRTn)
    remiPRTp = MAX(r_PRTremi*(y(iPRTp)-q_PRT_p * y(iPRTc)), &
                   r_PRTremi*(y(iPRTp)-q_PRT_p/q_PRT_n*y(iPRTn)) )
    remiPRTp = MAX(c0, remiPRTp)
    ! POM production
    pomPRTc = r_pomPRT * growPRTc
    pomPRTn = pomPRTc * q_POM_n
    pomPRTp = pomPRTc * q_POM_p
    ! 6. removal by microzooplankton
    grazPRTc = mu_MZ * y(iMZc) * y(iPRTc) * y(iPRTc) &
        / (y(iPRTc) * y(iPRTc) + g_prt * g_prt + &
           y(iTRc)*y(iTRc)/g_tr/g_tr*g_prt*g_prt + &
           y(iDETc)*y(iDETc)/g_det/g_det*g_prt*g_prt) 
    grazPRTn = grazPRTc * y(iPRTn) / y(iPRTc)
    grazPRTp = grazPRTc * y(iPRTp) / y(iPRTc)
    ! 7. new PRT
    dydtt(iPRTc) = (growPRTc - excrPRTldomc - excrPRTsdomc - excrPRTsdom2c - grazPRTc - pomPRTc &
                   - respPRT)/ SecPerDay
    dydtt(iPRTn) = (growPRTn - excrPRTldomn - excrPRTsdomn - excrPRTsdom2n - remiPRTn - grazPRTn &
                   - pomPRTn)/ SecPerDay
    dydtt(iPRTp) = (growPRTp - excrPRTldomp - excrPRTsdomp - excrPRTsdom2p - remiPRTp - grazPRTp &
                   - pomPRTp)/ SecPerDay

    !-----------------------------------------------------------------------
    !      Metazoan Processes
    !-----------------------------------------------------------------------
    ! removal of detritus by detrivore metazoa
    grazDETc = mu_MZ * y(iMZc) * y(iDETc) * y(iDETc) &
        / (y(iDETc) * y(iDETc) + g_det * g_det + &
           y(iPRTc)*y(iPRTc)/g_prt/g_prt*g_det*g_det + &
           y(iTRc)*y(iTRc)/g_tr/g_tr*g_det*g_det)
    grazDETn = grazDETc * y(iDETn) / y(iDETc)
    grazDETp = grazDETc * y(iDETp) / y(iDETc)

    ! 1. gross growth
    growMZc = grazPRTc + grazTRc + grazDETc
    growMZn = grazPRTn + grazTRn + grazDETn
    growMZp = grazPRTp + grazTRp + grazDETp
    ! 2. DOM excretion
    excrMZldomc = f_exMZldom * r_MZex * growMZc
    excrMZldomn = f_exMZldom * r_MZex * growMZn
    excrMZldomp = f_exMZldom * r_MZex * growMZp
    excrMZsdomc = (c1 - f_exMZldom) * r_MZex * growMZc
    excrMZsdomn = (c1 - f_exMZldom) * r_MZex * growMZn * y(iMZn)/y(iMZc)/q_MZ_n
!                                   * EXP(3*(y(iMZn)/y(iMZc)/q_MZ_n-c1)) 
    excrMZsdomp = (c1 - f_exMZldom) * r_MZex * growMZp * y(iMZp)/y(iMZc)/q_MZ_p
!                                   * EXP(3*(y(iMZp)/y(iMZc)/q_MZ_p-c1))
    
    ! 3. respiration
    respMZ = r_MZresp_1 * y(iMZc) + r_MZresp_2 * growMZc
    ! 4. adjust body stoichiometry by excreting semi-labile DOM
    temp = MAX(c1 - y(iMZn)/y(iMZc)/q_MZ_n, c1 - y(iMZp)/y(iMZc)/q_MZ_p)
    temp = MAx(temp, c0)
    excrMZsdom2c = r_MZadju * y(iMZc) * temp
    excrMZsdom2n = excrMZsdom2c * y(iMZn)/y(iMZc)*0.5d0
    excrMZsdom2p = excrMZsdom2c * y(iMZp)/y(iMZc)*0.5d0
    ! 5. adjust body stoichiometry by remineralizing inorganic nutrients
    remiMZn = MAX(r_MZremi*(y(iMZn)-q_MZ_n * y(iMZc)), &
                  r_MZremi*(y(iMZn)-q_MZ_n/q_MZ_p*y(iMZp)) )
    remiMZn = MAX(remiMZn, c0)
    remiMZp = MAX(r_MZremi*(y(iMZp)-q_MZ_p * y(iMZc)), &
                  r_MZremi*(y(iMZp)-q_MZ_p/q_MZ_n*y(iMZn)) )
    remiMZp = MAX(remiMZp, c0)
    ! 6. indissolved POM and refractory DOM production
    pomMZc = r_MZpom * growMZc
    pomMZn = q_POM_n * pomMZc
    pomMZp = q_POM_p * pomMZc
    refrMZc = r_MZrefr * growMZc
    refrMZn = q_refrDOM_n * refrMZc
    refrMZp = q_refrDOM_p * refrMZc
    ! 7. removal by higher-level zooplankton
    remvMZc = r_MZremv * y(iMZc) * y(iMZc)
    remvMZn = remvMZc / y(iMZc) * y(iMZn)
    remvMZp = remvMZc / y(iMZc) * y(iMZp)
    pomHZc = f_HZpom * remvMZc
    pomHZn = f_HZpom * remvMZn
    pomHZp = f_HZpom * remvMZp
    excrHZsdomc = remvMZc * f_HZsdom
    excrHZsdomn = remvMZn * f_HZsdom
    excrHZsdomp = remvMZp * f_HZsdom
    remiHZn = remvMZn - excrHZsdomn - pomHZn
    remiHZp = remvMZp - excrHZsdomp - pomHZp
    ! 8. new MZ
    dydtt(iMZc) = (growMZc - excrMZldomc -excrMZsdomc - excrMZsdom2c &
                    - pomMZc - refrMZc - remvMZc - respMZ) / SecPerDay
    dydtt(iMZn) = (growMZn - excrMZldomn -excrMZsdomn - remiMZn - excrMZsdom2n &
                    - pomMZn - refrMZn - remvMZn)/ SecPerDay
    dydtt(iMZp) = (growMZp - excrMZldomp -excrMZsdomp - remiMZp - excrMZsdom2p &
                    - pomMZp - refrMZp - remvMZp)/ SecPerDay
    ! 9. DVM MZ 
    y(iDVMZc) = y(iMZc)*f_dvm
    y(iDVMZn) = y(iDVMZc)*y(iMZn)/y(iMZc)
    y(iDVMZp) = y(iDVMZc)*y(iMZp)/y(iMZc)  

    !-----------------------------------------------------------------------
    !      Detritus Processes
    !-----------------------------------------------------------------------
    disDETc = remin * y(iDETc)
    disDETn = remin * y(iDETn) * remin_prf_n
    disDETp = remin * y(iDETp) * remin_prf_p

    dydtt(iDETc) = (pomSPc + pomTRc + pomUNc + pomPRTc + pomMZc + pomHZc - disDETc - growBApoc - grazDETc)/ SecPerDay
    dydtt(iDETn) = (pomSPn + pomTRn + pomUNn + pomPRTn + pomMZn + pomHZn - disDETn - growBApon - grazDETn)/ SecPerDay
    dydtt(iDETp) = (pomSPp + pomTRp + pomUNp + pomPRTp + pomMZp + pomHZp - disDETp - growBApop - grazDETp)/ SecPerDay

    !-----------------------------------------------------------------------
    !      Inorganic Nutrients Processes
    !-----------------------------------------------------------------------
    nitrf = r_nitrf * y(iNH4)
    dydtt(iNH4) = (remiPRTn + remiMZn + remiHZn + excrTR_nh4 + excrUN_nh4 &
                  - growSPnh4 - growTRnh4 - growUNnh4 - fluxBAnh4 - nitrf)/ SecPerDay
    dydtt(iNO3) = (-growSPno3 - growTRno3 - growUNno3 - fluxBAno3 + nitrf)/ SecPerDay
    dydtt(iPO4) = (remiPRTp + remiMZp + remiHZp - growSPp - growTRpo4 &
                  - growUNp - fluxBApo4)/ SecPerDay

    !-----------------------------------------------------------------------
    !      Dissolved Organic Matter (DOM) Processes
    !-----------------------------------------------------------------------
    temp = MIN(y(iSDOMn)/y(iSDOMc)/q_refrDOM_n, y(iSDOMp)/y(iSDOMc)/q_refrDOM_p)
    refrSDOMc = r_SDOMrefr * y(iSDOMc) * EXP(c1 - temp)
    refrSDOMn = refrSDOMc * q_refrDOM_n
    refrSDOMp = refrSDOMc * q_refrDOM_p
    temp = MAX(y(iSDOMc)-y(iSDOMn)/q_refrDOM_n, y(iSDOMc)-y(iSDOMp)/q_refrDOM_p)
    temp = MAX(temp, c0)
    refrSDOMc = refrSDOMc + temp
    dydtt(iLDOMc) = (excrSP_1c + excrTR_1c + excrUN_1c + excrPRTldomc &
                     + excrMZldomc - growBAldoc + mortBAc)/ SecPerDay
    dydtt(iLDOMn) = (excrSP_1n + excrTR_1n + excrUN_1n + excrPRTldomn &
                     + excrMZldomn - growBAldon + mortBAn)/ SecPerDay
    dydtt(iLDOMp) = (excrSP_1p + excrTR_1p + excrUN_1p + excrPRTldomp &
                     + excrMZldomp - growBAldop + mortBAp)/ SecPerDay
    dydtt(iSDOMc) = (excrSP_2c + excrTR_2c + excrUN_2c + excrBAc + excrPRTsdomc + excrPRTsdom2c &
                   + excrMZsdomc + excrMZsdom2c + excrHZsdomc + disDETc - growBAsdoc - refrSDOMc)/ SecPerDay
    dydtt(iSDOMn) = (excrSP_2n + excrTR_2n + excrUN_2n + excrBAn + excrPRTsdomn + excrPRTsdom2n &
                   + excrMZsdomn + excrMZsdom2n + excrHZsdomn + disDETn - growBAsdon - refrSDOMn)/ SecPerDay
    dydtt(iSDOMp) = (excrSP_2p + excrTR_2p + excrUN_2p + excrBAp + excrPRTsdomp + excrPRTsdom2p &
                   + excrMZsdomp + excrMZsdom2p + excrHZsdomp + disDETp - growBAsdop - refrSDOMp)/ SecPerDay
                   
!    dydtt_diag(iPP)=(growSPc+growTRc+growUNc &
!                                  - respSP - respTR - respUN &
!                                   - excrSP_1c - excrTR_1c - excrUN_1c &
!                                   - excrSP_2c - excrTR_2c - excrUN_2c &
!                                   )/SecPerDay*12.d0  
    dydtt_diag(iPP)=(growSPc+growTRc+growUNc)/SecPerDay*12.d0
    dydtt_diag(iprBAc) = (growBAc-respBA)/SecPerDay   
    dydtt_diag(igrowSPc) = growSPc/SecPerDay          
    dydtt_diag(igrowSPnh4) = growSPnh4/SecPerDay
    dydtt_diag(igrowSPno3) = growSPno3/SecPerDay
    dydtt_diag(igrowSPn) = growSPn/SecPerDay
    dydtt_diag(igrowSPp) = growSPp/SecPerDay
    dydtt_diag(iexcrSP_1c) = excrSP_1c/SecPerDay
    dydtt_diag(iexcrSP_1n) = excrSP_1n/SecPerDay
    dydtt_diag(iexcrSP_1p) = excrSP_1p/SecPerDay
    dydtt_diag(iexcrSP_2c) = excrSP_2c/SecPerDay
    dydtt_diag(iexcrSP_2n) = excrSP_2n/SecPerDay
    dydtt_diag(iexcrSP_2p) = excrSP_2p/SecPerDay
    dydtt_diag(ipomSPc) = pomSPc/SecPerDay
    dydtt_diag(ipomSPn) = pomSPn/SecPerDay
    dydtt_diag(ipomSPp) = pomSPp/SecPerDay
    dydtt_diag(igrazSPc) = grazSPc/SecPerDay
    dydtt_diag(igrazSPn) = grazSPn/SecPerDay
    dydtt_diag(igrazSPp) = grazSPp/SecPerDay
    dydtt_diag(igrowTRc) = growTRc/SecPerDay
    dydtt_diag(igrowTRnh4) = growTRnh4/SecPerDay
    dydtt_diag(igrowTRno3) = growTRno3/SecPerDay
    dydtt_diag(igrowTRnf) = growTRnf/SecPerDay
    dydtt_diag(igrowTRn) = growTRn/SecPerDay
    dydtt_diag(igrowTRpo4) = growTRpo4/SecPerDay
    dydtt_diag(ipickTRpo4) = pickTRpo4/SecPerDay
    dydtt_diag(igrowTRp) = growTRp/SecPerDay
    dydtt_diag(iexcrTR_1c) = excrTR_1c/SecPerDay
    dydtt_diag(iexcrTR_1n) = excrTR_1n/SecPerDay
    dydtt_diag(iexcrTR_1p) = excrTR_1p/SecPerDay
    dydtt_diag(iexcrTR_nh4) = excrTR_nh4/SecPerDay
    dydtt_diag(iexcrTR_2c) = excrTR_2c/SecPerDay
    dydtt_diag(iexcrTR_2n) = excrTR_2n/SecPerDay
    dydtt_diag(iexcrTR_2p) = excrTR_2p/SecPerDay
    dydtt_diag(ipomTRc) = pomTRc/SecPerDay
    dydtt_diag(ipomTRn) = pomTRn/SecPerDay
    dydtt_diag(ipomTRp) = pomTRp/SecPerDay
    dydtt_diag(igrazTRc) = grazTRc/SecPerDay
    dydtt_diag(igrazTRn) = grazTRn/SecPerDay
    dydtt_diag(igrazTRp) = grazTRp/SecPerDay
    dydtt_diag(igrowUNc) = growUNc/SecPerDay
    dydtt_diag(igrowUNnh4) = growUNnh4/SecPerDay
    dydtt_diag(igrowUNno3) = growUNno3/SecPerDay
    dydtt_diag(igrowUNnf) = growUNnf/SecPerDay
    dydtt_diag(igrowUNn) = growUNn/SecPerDay
    dydtt_diag(igrowUNp) = growUNp/SecPerDay
    dydtt_diag(iexcrUN_1c) = excrUN_1c/SecPerDay
    dydtt_diag(iexcrUN_1n) = excrUN_1n/SecPerDay
    dydtt_diag(iexcrUN_1p) = excrUN_1p/SecPerDay
    dydtt_diag(iexcrUN_nh4) = excrUN_nh4/SecPerDay
    dydtt_diag(iexcrUN_2c) = excrUN_2c/SecPerDay
    dydtt_diag(iexcrUN_2n) = excrUN_2n/SecPerDay
    dydtt_diag(iexcrUN_2p) = excrUN_2p/SecPerDay
    dydtt_diag(ipomUNc) = pomUNc/SecPerDay
    dydtt_diag(ipomUNn) = pomUNn/SecPerDay
    dydtt_diag(ipomUNp) = pomUNp/SecPerDay
    dydtt_diag(igrazUNc) = grazUNc/SecPerDay
    dydtt_diag(igrazUNn) = grazUNn/SecPerDay
    dydtt_diag(igrazUNp) = grazUNp/SecPerDay
    dydtt_diag(igrowBAldoc) = growBAldoc/SecPerDay
    dydtt_diag(igrowBAldon) = growBAldon/SecPerDay
    dydtt_diag(igrowBAldop) = growBAldop/SecPerDay
    dydtt_diag(igrowBAsdoc) = growBAsdoc/SecPerDay
    dydtt_diag(igrowBAsdon) = growBAsdon/SecPerDay
    dydtt_diag(igrowBAsdop) = growBAsdop/SecPerDay
    dydtt_diag(igrowBAnh4) = growBAnh4/SecPerDay
    dydtt_diag(igrowBAno3) = growBAno3/SecPerDay
    dydtt_diag(igrowBApo4) = growBApo4/SecPerDay
    dydtt_diag(igrowBAc) = growBAc/SecPerDay
    dydtt_diag(igrowBAn) = growBAn/SecPerDay
    dydtt_diag(igrowBAp) = growBAp/SecPerDay
    dydtt_diag(irespBA) = respBA/SecPerDay
    dydtt_diag(irefrBAc) = refrBAc/SecPerDay
    dydtt_diag(irefrBAn) = refrBAn/SecPerDay
    dydtt_diag(irefrBAp) = refrBAp/SecPerDay
    dydtt_diag(iexcrBAc) = excrBAc/SecPerDay
    dydtt_diag(iexcrBAn) = excrBAn/SecPerDay
    dydtt_diag(iexcrBAp) = excrBAp/SecPerDay
    dydtt_diag(iremiBAn) = remiBAn/SecPerDay
    dydtt_diag(iremiBAp) = remiBAp/SecPerDay
    dydtt_diag(igrazBAc) = grazBAc/SecPerDay
    dydtt_diag(igrazBAn) = grazBAn/SecPerDay
    dydtt_diag(igrazBAp) = grazBAp/SecPerDay
    dydtt_diag(imortBAc) = mortBAc/SecPerDay
    dydtt_diag(imortBAn) = mortBAn/SecPerDay
    dydtt_diag(imortBAp) = mortBAp/SecPerDay
    dydtt_diag(ifluxBAnh4) = fluxBAnh4/SecPerDay
    dydtt_diag(ifluxBApo4) = fluxBApo4/SecPerDay
    dydtt_diag(igrowPRTc) = growPRTc/SecPerDay
    dydtt_diag(igrowPRTn) = growPRTn/SecPerDay
    dydtt_diag(igrowPRTp) = growPRTp/SecPerDay
    dydtt_diag(irespPRT) = respPRT/SecPerDay
    dydtt_diag(iexcrPRTldomc) = excrPRTldomc/SecPerDay
    dydtt_diag(iexcrPRTldomn) = excrPRTldomn/SecPerDay
    dydtt_diag(iexcrPRTldomp) = excrPRTldomp/SecPerDay
    dydtt_diag(iexcrPRTsdomc) = excrPRTsdomc/SecPerDay
    dydtt_diag(iexcrPRTsdomn) = excrPRTsdomn/SecPerDay
    dydtt_diag(iexcrPRTsdomp) = excrPRTsdomp/SecPerDay
    dydtt_diag(iexcrPRTsdom2c) = excrPRTsdom2c/SecPerDay
    dydtt_diag(iexcrPRTsdom2n) = excrPRTsdom2n/SecPerDay
    dydtt_diag(iexcrPRTsdom2p) = excrPRTsdom2p/SecPerDay
    dydtt_diag(iremiPRTn) = remiPRTn/SecPerDay
    dydtt_diag(iremiPRTp) = remiPRTp/SecPerDay
    dydtt_diag(ipomPRTc) = pomPRTc/SecPerDay
    dydtt_diag(ipomPRTn) = pomPRTn/SecPerDay
    dydtt_diag(ipomPRTp) = pomPRTp/SecPerDay
    dydtt_diag(igrazPRTc) = grazPRTc/SecPerDay
    dydtt_diag(igrazPRTn) = grazPRTn/SecPerDay
    dydtt_diag(igrazPRTp) = grazPRTp/SecPerDay
    dydtt_diag(igrowMZc) = growMZc/SecPerDay
    dydtt_diag(igrowMZn) = growMZn/SecPerDay
    dydtt_diag(igrowMZp) = growMZp/SecPerDay
    dydtt_diag(irespMZ) = respMZ/SecPerDay
    dydtt_diag(iexcrMZldomc) = excrMZldomc/SecPerDay
    dydtt_diag(iexcrMZldomn) = excrMZldomn/SecPerDay
    dydtt_diag(iexcrMZldomp) = excrMZldomp/SecPerDay
    dydtt_diag(iexcrMZsdomc) = excrMZsdomc/SecPerDay
    dydtt_diag(iexcrMZsdomn) = excrMZsdomn/SecPerDay
    dydtt_diag(iexcrMZsdomp) = excrMZsdomp/SecPerDay
    dydtt_diag(iexcrMZsdom2c) = excrMZsdom2c/SecPerDay
    dydtt_diag(iexcrMZsdom2n) = excrMZsdom2n/SecPerDay
    dydtt_diag(iexcrMZsdom2p) = excrMZsdom2p/SecPerDay
    dydtt_diag(iremiMZn) = remiMZn/SecPerDay
    dydtt_diag(iremiMZp) = remiMZp/SecPerDay
    dydtt_diag(irefrMZc) = refrMZc/SecPerDay
    dydtt_diag(irefrMZn) = refrMZn/SecPerDay
    dydtt_diag(irefrMZp) = refrMZp/SecPerDay
    dydtt_diag(ipomMZc) = pomMZc/SecPerDay
    dydtt_diag(ipomMZn) = pomMZn/SecPerDay
    dydtt_diag(ipomMZp) = pomMZp/SecPerDay
    dydtt_diag(iremvMZc) = remvMZc/SecPerDay
    dydtt_diag(iremvMZn) = remvMZn/SecPerDay
    dydtt_diag(iremvMZp) = remvMZp/SecPerDay
    dydtt_diag(ipomHZc) = pomHZc/SecPerDay
    dydtt_diag(ipomHZn) = pomHZn/SecPerDay
    dydtt_diag(ipomHZp) = pomHZp/SecPerDay
    dydtt_diag(iexcrHZsdomc) = excrHZsdomc/SecPerDay
    dydtt_diag(iexcrHZsdomn) = excrHZsdomn/SecPerDay
    dydtt_diag(iexcrHZsdomp) = excrHZsdomp/SecPerDay
    dydtt_diag(iremiHZn) = remiHZn/SecPerDay
    dydtt_diag(iremiHZp) = remiHZp/SecPerDay
    dydtt_diag(idisDETc) = disDETc/SecPerDay
    dydtt_diag(idisDETn) = disDETn/SecPerDay
    dydtt_diag(idisDETp) = disDETp/SecPerDay
    dydtt_diag(initrf) = nitrf/SecPerDay
    dydtt_diag(irefrSDOMc) = refrSDOMc/SecPerDay
    dydtt_diag(irefrSDOMn) = refrSDOMn/SecPerDay
    dydtt_diag(irefrSDOMp) = refrSDOMp/SecPerDay
    dydtt_diag(iexportc) = wnsvo*wnsvflag(iDETc) * y(iDETc) /SecPerDay
    dydtt_diag(iexportn) = wnsvo*wnsvflag(iDETn) * y(iDETn) /SecPerDay
    dydtt_diag(iexportp) = wnsvo*wnsvflag(iDETp) * y(iDETp) /SecPerDay
    dydtt_diag(irespSP) = respSP/SecPerDay
    dydtt_diag(irespTR) = respTR/SecPerDay
    dydtt_diag(irespUN) = respUN/SecPerDay
    dydtt_diag(igrowBApoc) = growBApoc/SecPerDay
    dydtt_diag(igrowBApon) = growBApon/SecPerDay
    dydtt_diag(igrowBApop) = growBApop/SecPerDay
    dydtt_diag(igrazDETc) = grazDETc/SecPerDay
    dydtt_diag(igrazDETn) = grazDETn/SecPerDay
    dydtt_diag(igrazDETp) = grazDETp/SecPerDay
    dydtt_diag(iHETres) = (respBA+respMZ*f_dvm)/SecPerDay

  end subroutine derivs

end module derivs_mod

