#include "GCHEM_OPTIONS.h"

      COMMON /dic_load/
     &    wspeed0, wspeed1, ice0, ice1, atmosp0,
     &    atmosp1, silica0, silica1
#ifdef ALLOW_FE
     &    ,feinput0,feinput1
#endif
           _RS wspeed0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
              _RS wspeed1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS wind  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS ice0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS ice1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS atmosp0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS atmosp1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS silica0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS silica1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_FE
	      _RS feinput0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
	      _RS feinput1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

