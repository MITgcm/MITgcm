#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
c#ifdef ALLOW_AUTODIFF
c# include "AUTODIFF_OPTIONS.h"
c#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

C start with a copy of THE_MAIN_LOOP
C and remove some the obvious extras
      SUBROUTINE openad_dumpAdjoint( myTime, myIter, myThid )
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

c**************************************
#ifdef ALLOW_AUTODIFF

c These includes are needed for
c AD-checkpointing.
c They provide the fields to be stored.

# include "GRID.h"
# include "DYNVARS.h"
# include "FFIELDS.h"
# include "EOS.h"
# ifdef ALLOW_GENERIC_ADVDIFF
#  include "GAD.h"
# endif
# ifdef ALLOW_CD_CODE
#  include "CD_CODE_VARS.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS_FIELDS.h"
# endif
# ifdef EXACT_CONSERV
#  include "SURFACE.h"
# endif
# ifdef ALLOW_OBCS
#  include "OBCS_PARAMS.h"
#  include "OBCS_GRID.h"
#  include "OBCS_FIELDS.h"
#  include "OBCS_SEAICE.h"
# endif
# ifdef ALLOW_EXF
#  include "EXF_FIELDS.h"
#  include "EXF_PARAM.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "EXF_CONSTANTS.h"
#  endif
# endif /* ALLOW_EXF */
# ifdef ALLOW_SEAICE
#  include "SEAICE.h"
# endif
# ifdef ALLOW_EBM
#  include "EBM.h"
# endif
# ifdef ALLOW_KPP
#  include "KPP.h"
#  include "KPP_TAVE.h"
#  include "KPP_PARAMS.h"
# endif

# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"

#endif /* ALLOW_AUTODIFF */

      _RL     myTime
      INTEGER myIter
      INTEGER myThid

      _RL     foo4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     foo5(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(10) suff

      IF ( MOD(myIter,360).EQ.0 ) THEN
         IF ( rwSuffixType.EQ.0 ) THEN
           WRITE(suff,'(I10.10)') myIter
         ELSE
           CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
         ENDIF
# ifndef ALLOW_STREAMICE
         foo5=theta%d
         call write_fld_xyz_rl('adjtheta.',suff,foo5,myIter,1)
         foo5=salt%d
         call write_fld_xyz_rl('adjsalt.',suff,foo5,myIter,1)
         foo5=uvel%d
         call write_fld_xyz_rl('adjuvel.',suff,foo5,myIter,1)
         foo5=vvel%d
         call write_fld_xyz_rl('adjvvel.',suff,foo5,myIter,1)
         foo5=wvel%d
         call write_fld_xyz_rl('adjwvel.',suff,foo5,myIter,1)
         foo4=etan%d
         call write_fld_xy_rl('adjetan.',suff,foo4,myIter,1)
# endif
# ifdef ALLOW_DIFFKR_CONTROL
         foo5=diffkr%d
         call write_fld_xy_rl('adjdiffkr.',suff,foo5,myIter,1)
# endif
# ifdef ALLOW_HFLUX0_CONTROL
         foo4=qnet%d
         call write_fld_xy_rl('adjqnet.',suff,foo4,myIter,1)
# endif
# ifdef ALLOW_SFLUX0_CONTROL
         foo4=empmr%d
         call write_fld_xy_rl('adjempmr.',suff,foo4,myIter,1)
# endif
# ifdef ALLOW_TAUU0_CONTROL
         foo4=fu%d
         call write_fld_xy_rl('adjfu.',suff,foo4,myIter,1)
# endif
# ifdef ALLOW_TAUV0_CONTROL
         foo4=fv%d
         call write_fld_xy_rl('adjfv.',suff,foo4,myIter,1)
# endif
# ifdef ALLOW_HFLUXM_CONTROL
         foo4=qnetm%d
         call write_fld_xy_rl('adjhfluxm.',suff,foo4,myIter,1)
#endif
      ENDIF

      RETURN
      END
