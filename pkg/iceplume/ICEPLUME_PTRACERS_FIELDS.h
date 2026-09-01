#include "PTRACERS_SIZE.h"

#ifdef ALLOW_ICEPLUME
#ifdef ALLOW_PTRACERS
C Parameters relating to PTRACERS

      COMMON /ICEPLUME_PARM02_L/ useInputPtracers
      LOGICAL useInputPtracers

      COMMON /ICEPLUME_PARM02_C/ ptracerMaskFile
      CHARACTER*(512) ptracerMaskFile

      COMMON /ICEPLUME_PTRACERS_RL/
     &     ptr_addMass3D,
     &     ptracerMask
      _RL ptr_addMass3d
     &     (1-OLx:sNx+OLx,1-Oly:sNy+Oly,Nr,nSx,nSy,PTRACERS_num)
      _RL ptracerMask
     &     (1-Olx:sNx+Olx,1-Oly:sNy+Oly,PTRACERS_num,nSx,nSy)
#endif /* ALLOW_PTRACERS */

# endif /* ALLOW_ICEPLUME */
