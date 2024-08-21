!-------------------------------------------------------------------------
! indices for state variables
!-------------------------------------------------------------------------
  integer, parameter :: iSPc=1,iSPn=2,iSPp=3,
 &                      iTRc=4,iTRn=5,iTRp=6,
 &                      iUNc=7,iUNn=8,iUNp=9,
 &                      iBAc=10,iBAn=11,iBAp=12,
 &                      iPRTc=13,iPRTn=14,iPRTp=15,
 &                      iMZc=16,iMZn=17,iMZp=18,
 &                      iLDOMc=19,iLDOMn=20,iLDOMp=21,
 &                      iSDOMc=22,iSDOMn=23,iSDOMp=24,
 &                      iDETc=25,iDETn=26,iDETp=27,
 &                      iNH4=28,iNO3=29,iPO4=30,
 &                      iSPchl=31, iTRchl = 32, iUNchl = 33,
 &                      iDVMZc=34, iDVMZn=35, iDVMZp=36

!-------------------------------------------------------------------------
! ***Required*** numstatevar - number of ecosystem scalars.  Should be
! equal to maximum index above.
!-------------------------------------------------------------------------
  integer, parameter :: numstatevar=36

!-------------------------------------------------------------------------
! indices for diagnostic variables
!-------------------------------------------------------------------------
  integer, parameter :: iPP=1, iprBAc=2, &
                   igrowSPc=3, igrowSPnh4=4, igrowSPno3=5, &
                   igrowSPn=6, igrowSPp=7, &
                   iexcrSP_1c=8, iexcrSP_1n=9, iexcrSP_1p=10, &
                   iexcrSP_2c=11, iexcrSP_2n=12, iexcrSP_2p=13, &
                   ipomSPc=14, ipomSPn=15, ipomSPp=16, &
                   igrazSPc=17, igrazSPn=18, igrazSPp=19, &
                   igrowTRc=20, igrowTRnh4=21, igrowTRno3=22, igrowTRnf=23, &
                   igrowTRn=24, igrowTRpo4=25, ipickTRpo4=26, igrowTRp=27, &
                   iexcrTR_1c=28, iexcrTR_1n=29, iexcrTR_1p=30, iexcrTR_nh4=31, &
                   iexcrTR_2c=32, iexcrTR_2n=33, iexcrTR_2p=34, &
                   ipomTRc=35, ipomTRn=36, ipomTRp=37, &
                   igrazTRc=38, igrazTRn=39, igrazTRp=40, &
                   igrowUNc=41, igrowUNnh4=42, igrowUNno3=43, igrowUNnf=44, &
                   igrowUNn=45, igrowUNp=46, &
                   iexcrUN_1c=47, iexcrUN_1n=48, iexcrUN_1p=49, iexcrUN_nh4=50, &
                   iexcrUN_2c=51, iexcrUN_2n=52, iexcrUN_2p=53, &
                   ipomUNc=54, ipomUNn=55, ipomUNp=56, &
                   igrazUNc=57, igrazUNn=58, igrazUNp=59, &
                   igrowBAldoc=60, igrowBAldon=61, igrowBAldop=62, &
                   igrowBAsdoc=63, igrowBAsdon=64, igrowBAsdop=65, &
                   igrowBAnh4=66, igrowBAno3=67, igrowBApo4=68, &
                   igrowBAc=69, igrowBAn=70, igrowBAp=71, irespBA=72, &
                   irefrBAc=73, irefrBAn=74, irefrBAp=75, &
                   iexcrBAc=76, iexcrBAn=77, iexcrBAp=78, &
                   iremiBAn=79, iremiBAp=80, &
                   igrazBAc=81, igrazBAn=82, igrazBAp=83, &
                   imortBAc=84, imortBAn=85, imortBAp=86, &
                   ifluxBAnh4=87, ifluxBApo4=88, &
                   igrowPRTc=89, igrowPRTn=90, igrowPRTp=91, irespPRT=92, &
                   iexcrPRTldomc=93, iexcrPRTldomn=94, iexcrPRTldomp=95, &
                   iexcrPRTsdomc=96, iexcrPRTsdomn=97, iexcrPRTsdomp=98, &
                   iexcrPRTsdom2c=99, iexcrPRTsdom2n=100, iexcrPRTsdom2p=101, &
                   iremiPRTn=102, iremiPRTp=103, &
                   ipomPRTc=104, ipomPRTn=105, ipomPRTp=106, &
                   igrazPRTc=107, igrazPRTn=108, igrazPRTp=109, &
                   igrowMZc=110, igrowMZn=111, igrowMZp=112, &
                   irespSP=113, irespTR=114, irespUN=115, irespMZ=116, &
                   iexcrMZldomc=117, iexcrMZldomn=118, iexcrMZldomp=119, &
                   iexcrMZsdomc=120, iexcrMZsdomn=121, iexcrMZsdomp=122, &
                   iexcrMZsdom2c=123, iexcrMZsdom2n=124, iexcrMZsdom2p=125, &
                   iremiMZn=126, iremiMZp=127, &
                   irefrMZc=128, irefrMZn=129, irefrMZp=130, &
                   ipomMZc=131, ipomMZn=132, ipomMZp=133, &
                   iremvMZc=134, iremvMZn=135, iremvMZp=136, &
                   ipomHZc=137, ipomHZn=138, ipomHZp=139, &
                   iexcrHZsdomc=140, iexcrHZsdomn=141, iexcrHZsdomp=142, &
                   iremiHZn=143, iremiHZp=144, &
                   idisDETc=145, idisDETn=146, idisDETp=147, &
                   initrf=148, &
                   irefrSDOMc=149, irefrSDOMn=150, irefrSDOMp=151, &
                   iexportc=152,iexportn=153,iexportp=154,&
                                   igrowBApoc=155,igrowBApon=156,igrowBApop=157,&
                                   igrazDETc=158,igrazDETn=159,igrazDETp=160,iHETres=161

!-------------------------------------------------------------------------
! ***Required*** numdiagvar - number of diagnostic variables.  Must be
! equal to the maximum index above
!-------------------------------------------------------------------------
  integer, parameter :: numdiagvar=161

!-------------------------------------------------------------------------
! indices for ecosystem parameters
!-------------------------------------------------------------------------
  integer, parameter :: &
       iae          = 1, &
       imu_SP          = iae    + 1, &
       ialpha_SP       = imu_SP          + 1, &
       ia_SP       = ialpha_SP          + 1, &
       iv_SPn     = ia_SP             + 1, &
       ik_nh4SP        = iv_SPn       + 1, &
       ik_no3SP        = ik_nh4SP        + 1, &
       iv_SPp            = ik_no3SP        + 1, &
       ik_po4SP        = iv_SPp        + 1, &
       izeta               = ik_po4SP     + 1, &
       itheta             = izeta        + 1, &
       ir_excrSP_1     = itheta        + 1, &
       ir_excrSP_2     = ir_excrSP_1     + 1, &
       ir_pomSP        = ir_excrSP_2     + 1, &
       imu_TR          = ir_pomSP        + 1, &
       ialpha_TR       = imu_TR          + 1, &
       ia_TR       = ialpha_TR         + 1, &
       iv_TRn     = ia_TR              + 1, &
       ik_nh4TR        = iv_TRn       + 1, &
       ik_no3TR        = ik_nh4TR        + 1, &
       iv_TRp             = ik_no3TR       + 1, &
       ik_po4TR        = iv_TRp        + 1, &
       imu_pickTRpo4 = ik_po4TR + 1, &
       izeta_nf            = imu_pickTRpo4        + 1, &
       ir_excrTR_1     = izeta_nf        + 1, &
       ir_excrTR_n   = ir_excrTR_1     + 1, &
       ir_excrTR_2     = ir_excrTR_n   + 1, &
       ir_pomTR        = ir_excrTR_2     + 1, &
       imu_UN          = ir_pomTR        + 1, &
       ialpha_UN       = imu_UN          + 1, &
       ik_DOM        = ialpha_UN        + 1, &
       !ib_SDONlabi     = ik_DOM        + 1, &
       !ib_SDOPlabi     = ib_SDONlabi     + 1, &
       ir_SDOM = ik_DOM + 1, &
       imu_BA          = ir_SDOM     + 1, &
       ib_BAresp       = imu_BA       + 1, &
       ir_BAadju       = ib_BAresp       + 1, &
       ir_BAremi       = ir_BAadju       + 1, &
       ir_BArefr       = ir_BAremi       + 1, &
       if_BAslct = ir_BArefr + 1, &
       ir_BAresp_1 = if_BAslct + 1, &
       ir_BAresp_min = ir_BAresp_1 + 1, &
       ir_BAresp_max = ir_BAresp_min  + 1, &
       ir_BAmort = ir_BAresp_max  + 1, &
       imu_PRT       = ir_BAmort        + 1, &
       ig_sp        = imu_PRT       + 1, &
       ig_ba        = ig_sp       + 1, &
       ir_PRTex        = ig_ba        + 1, &
       if_exPRTldom    = ir_PRTex      + 1, &
       ir_PRTresp_1    = if_exPRTldom    + 1, &
       ir_PRTresp_2    = ir_PRTresp_1    + 1, &
       ir_PRTadju      = ir_PRTresp_2    + 1, &
       ir_PRTremi      = ir_PRTadju      + 1, &
       ir_pomPRT       = ir_PRTremi      + 1, &
       imu_MZ       = ir_pomPRT       + 1, &
       ig_prt        = imu_MZ       + 1, &
       ig_tr         = ig_prt        + 1, &
       ir_MZex         = ig_tr     + 1, &
       if_exMZldom     = ir_MZex         + 1, &
       ir_MZresp_1     = if_exMZldom     + 1, &
       ir_MZresp_2     = ir_MZresp_1     + 1, &
       ir_MZadju       = ir_MZresp_2     + 1, &
       ir_MZremi       = ir_MZadju       + 1, &
       ir_MZpom        = ir_MZremi       + 1, &
       ir_MZrefr       = ir_MZpom        + 1, &
       ir_MZremv       = ir_MZrefr        + 1, &
       if_HZsdom       = ir_MZremv       + 1, &
       if_HZpom        = if_HZsdom       + 1, &
       ir_SDOMrefr     = if_HZpom        + 1, &
       iq_refrDOM_n = ir_SDOMrefr     + 1, &
       iq_refrDOM_p = iq_refrDOM_n + 1, &
       iq_POM_n        = iq_refrDOM_p     + 1, &
       iq_POM_p        = iq_POM_n        + 1, &
       ir_nitrf        = iq_POM_p        + 1, &
       iremin_prf_n    = ir_nitrf        + 1, &
       iremin_prf_p    = iremin_prf_n    + 1, &
       iwnsvo    = iremin_prf_p    + 1, &
       iremin          = iwnsvo    + 1, &
       ik_pom          = iremin    + 1, &
       ig_det          = ik_pom    + 1, &
       if_dvm          = ig_det    + 1

! mwC              molecular weight of Carbon (g/mol)
! mwN              molecular weight of Nitrogen (g/mol)
! mwN              molecular weight of Phosphorus (g/mol)
  double precision, parameter :: mwC=12.0,mwN=14.0,mwP=31

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
