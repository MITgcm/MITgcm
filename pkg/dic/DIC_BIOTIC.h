#ifdef DIC_BIOTIC
C     /==========================================================\
C     | DIC_BIOTIC.h                                                |
C     | o Biological Carbon Variables                                      |
C     |==========================================================|

       COMMON /BIOTIC_NEEDS/
     &    BIOave, CARave, SURave, SUROave, pCO2ave, pHave, 
     &    fluxCO2ave, dic_timeave,
     &    alpha, rain_ratio, InputFe,
     &    Kpo4, DOPfraction, zcrit, KRemin,
     &    KDOPremin,zca,R_op,R_cp,R_np, R_fep, nlev,
     &    o2crit, alpfe, KScav, ligand_stab, ligand_tot, KFE,
     &    k0, lit0
      integer nlev
c for averages
      _RL BIOave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL CARave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL SURave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SUROave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL pCO2ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL pHave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fluxCO2ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL DIC_timeave(nSx,nSy,nR)
c values for biogeochemistry
      _RL alpha(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rain_ratio(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL InputFe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Kpo4            
      _RL DOPfraction   
      _RL zcrit
      _RL KRemin
      _RL KDOPremin
      _RL zca
      _RL R_op
      _RL R_cp
      _RL R_np
      _RL R_fep
      _RL o2crit
      _RL alpfe
      _RL KScav
      _RL ligand_stab
      _RL ligand_tot
      _RL  KFe
cn values for light limited bio activity
      _RL k0,lit0
#endif
