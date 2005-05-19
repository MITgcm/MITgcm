C $Header: /u/gcmpack/MITgcm/pkg/dic/DIC_COST.h,v 1.3 2005/05/19 21:46:15 ce107 Exp $
C $Name:  $

c DIC_COST.h
         _RL totcost
c control variables
cQQ      _RL alpha(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
cQQ      _RL rain_ratio(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
cQQ      _RL KScav
cQQ      _RL ligand_stab
cQQ      _RL ligand_tot

         COMMON /DIC_COST_CTRL/
     &    totcost
cQQ  &   ,alpha, rain_ratio, KScav, ligand_stab, ligand_tot


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
