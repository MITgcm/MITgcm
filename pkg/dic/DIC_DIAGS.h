C $Header: /u/gcmpack/MITgcm/pkg/dic/Attic/DIC_DIAGS.h,v 1.3 2007/08/24 16:08:37 dfer Exp $
C $Name:  $

#ifdef ALLOW_DIC_COST

c DIC_DIAGS.h
         _RL po4obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
         _RL o2obs  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
         _RL po4ann (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
         _RL o2ann  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
         _RL obs_timetave(nSx,nSy,nR)
         _RL po4var(nR)
         _RL o2var(nR)
         COMMON /DIC_AVERAGE/
     &    po4obs, o2obs, po4ann, o2ann, obs_timetave, po4var, o2var

         _RL po4obsl1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
	 _RL po4obsl2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
	 _RL po4obsl3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
cQQ	 _RL po4obsl4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
	 _RL o2obsl1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
	 _RL o2obsl2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
	 _RL o2obsl3 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
cQQ	 _RL o2obsl4 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
	 _RL po4lev1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,12,nSx,nSy)
	 _RL po4lev2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,12,nSx,nSy)
	 _RL po4lev3 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,12,nSx,nSy)
cQQ	 _RL po4lev4 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,12,nSx,nSy)
	 _RL o2lev1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,12,nSx,nSy)
	 _RL o2lev2  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,12,nSx,nSy)
	 _RL o2lev3  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,12,nSx,nSy)
cQQ	 _RL O2lev4  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,12,nSx,nSy)
	 _RL obsm_timetave(nSx,nSy,12)
	 _RL po4varm(12,3)
	 _RL o2varm(12,3)
	 COMMON /DIC_AVERAGE_MONTH/
     &    po4obsl1, po4obsl2, po4obsl3, 
     &    o2obsl1, o2obsl2, o2obsl3, 
     &    po4lev1,  po4lev2,  po4lev3 , 
     &    o2lev1 , o2lev2 , o2lev3 , 
     &    obsm_timetave, po4varm, o2varm

#endif /* ALLOW_DIC_COST */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
