C $Header: /u/gcmpack/MITgcm/pkg/dic/DIC_LOAD.h,v 1.6 2008/04/24 21:35:53 gforget Exp $
C $Name:  $

      COMMON /dic_load/
     &    dicwind0, dicwind1, ice0, ice1, atmosp0,
     &    atmosp1, silica0, silica1
#ifdef READ_PAR
     &    ,par0, par1
#endif
#ifdef ALLOW_FE
     &    ,feinput0,feinput1
#endif
              _RS dicwind0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
              _RS dicwind1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS ice0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS ice1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS atmosp0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS atmosp1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS silica0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS silica1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef READ_PAR
              _RS par0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
              _RS par1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_FE
	      _RS feinput0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS feinput1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
