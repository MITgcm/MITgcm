C $Header: /u/gcmpack/MITgcm/pkg/dic/Attic/DIC_BIOTIC.h,v 1.10 2008/04/03 22:49:53 dfer Exp $
C $Name:  $

#ifdef DIC_BIOTIC
C     /==========================================================\
C     | DIC_BIOTIC.h
C     | o Biological Carbon Variables
C     |==========================================================|

      COMMON /BIOTIC_NEEDS/
     &     BIOave, CARave, SURave, SUROave, pCO2ave, pHave, 
     &     fluxCO2ave, omegaCave, pfluxave, epfluxave, cfluxave, 
     &     dic_timeave,
     &     alpha, rain_ratio, InputFe, omegaC,
     &     Kpo4, DOPfraction, zcrit, KRemin,
     &     KDOPremin,zca,R_op,R_cp,R_NP, R_FeP,
     &     O2crit, alpfe, KScav, ligand_stab, ligand_tot, KFE,
     &     freefemax, par,
     &     parfrac, k0, lit0,
     &     nlev, QSW_underice

      integer nlev

C     For averages
      _RL BIOave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL CARave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL SURave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SUROave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL pCO2ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL pHave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fluxCO2ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL OmegaCave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL pfluxave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL epfluxave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL cfluxave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL DIC_timeave(nSx,nSy,nR)

C     values for biogeochemistry
      _RL par(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL alpha(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rain_ratio(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL InputFe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL omegaC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL Kpo4            
      _RL DOPfraction   
      _RL zcrit
      _RL KRemin
      _RL KDOPremin
      _RL zca
      _RL R_op
      _RL R_cp
      _RL R_NP
      _RL R_FeP
      _RL O2crit
      _RL alpfe
      _RL KScav
      _RL ligand_stab
      _RL ligand_tot
      _RL  KFe
      _RL freefemax
C     values for light limited bio activity
      _RL k0, parfrac, lit0

      LOGICAL QSW_underice
#endif


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
