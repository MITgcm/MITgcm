C $Header: /u/gcmpack/MITgcm/verification/OpenAD/code_ad/Attic/the_third_level_loop.F,v 1.3 2005/12/10 17:44:23 utke Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
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
      SUBROUTINE THE_THIRD_LEVEL_LOOP(iLev_4, iLev_3, 
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +myTime, myIter,myThid )
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

c**************************************
#ifdef ALLOW_AUTODIFF_TAMC

c These includes are needed for 
c AD-checkpointing. 
c They provide the fields to be stored.

# include "GRID.h"
# include "DYNVARS.h"
# include "FFIELDS.h"
# include "EOS.h"
# include "GAD.h"
# ifdef ALLOW_CD_CODE
#  include "CD_CODE_VARS.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS.h"
# endif
# ifdef EXACT_CONSERV
#  include "SURFACE.h"
# endif
# ifdef ALLOW_OBCS
#  include "OBCS.h"
# endif
# ifdef ALLOW_EXF
#  include "exf_fields.h"
#  include "exf_clim_fields.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "exf_constants.h"
#  endif
# endif /* ALLOW_EXF */
# ifdef ALLOW_SEAICE
#  include "SEAICE.h"
# endif
# ifdef ALLOW_EBM
#  include "EBM.h"
# endif
# ifdef ALLOW_DIVIDED_ADJOINT_MPI
#  include "mpif.h"
# endif

# include "tamc.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"

#endif /* ALLOW_AUTODIFF_TAMC */

      integer iLev_4
      integer iLev_3
      integer  uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4
      INTEGER myThid 
      INTEGER myIter
      _RL     myTime

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer ilev_2

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_THIRD_LEVEL_LOOP',myThid)
#endif
      print *, 'JU: THE_THIRD_LEVEL_LOOP(',iLev_4,',',iLev_3,')'
      if (uCheckLev2 .gt. 0 ) then 
         do ilev_2 = 0,uCheckLev2 - 1
#ifdef ALLOW_DEBUG
            IF (debugMode) CALL DEBUG_CALL('THE_SECOND_LEVEL_LOOP',myThid)
#endif
            CALL THE_SECOND_LEVEL_LOOP( iLev_4, iLev_3, iLev_2,
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +mytime, myiter, mythid )
         enddo
      else
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('THE_SECOND_LEVEL_PLAIN',myThid)
#endif
         CALL THE_SECOND_LEVEL_PLAIN( iLev_4, iLev_3, 
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +mytime, myiter, mythid )
      end if
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THE_THIRD_LEVEL_LOOP',myThid)
#endif

      END

      SUBROUTINE THE_THIRD_LEVEL_PLAIN(iLev_4, 
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +myTime, myIter,myThid )
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

c**************************************
#ifdef ALLOW_AUTODIFF_TAMC

c These includes are needed for 
c AD-checkpointing. 
c They provide the fields to be stored.

# include "GRID.h"
# include "DYNVARS.h"
# include "FFIELDS.h"
# include "EOS.h"
# include "GAD.h"
# ifdef ALLOW_CD_CODE
#  include "CD_CODE_VARS.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS.h"
# endif
# ifdef EXACT_CONSERV
#  include "SURFACE.h"
# endif
# ifdef ALLOW_OBCS
#  include "OBCS.h"
# endif
# ifdef ALLOW_EXF
#  include "exf_fields.h"
#  include "exf_clim_fields.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "exf_constants.h"
#  endif
# endif /* ALLOW_EXF */
# ifdef ALLOW_SEAICE
#  include "SEAICE.h"
# endif
# ifdef ALLOW_EBM
#  include "EBM.h"
# endif
# ifdef ALLOW_DIVIDED_ADJOINT_MPI
#  include "mpif.h"
# endif

# include "tamc.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"

#endif /* ALLOW_AUTODIFF_TAMC */

      integer iLev_4
      integer  uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4
      INTEGER myThid 
      INTEGER myIter
      _RL     myTime

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer ilev_2

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_THIRD_LEVEL_PLAIN',myThid)
#endif
      print *, 'JU: THE_THIRD_LEVEL_PLAIN(',iLev_4,')'
      if (uCheckLev2 .gt. 0 ) then 
         do ilev_2 = 0,uCheckLev2 - 1
#ifdef ALLOW_DEBUG
            IF (debugMode) CALL DEBUG_CALL('THE_SECOND_LEVEL_LOOP',myThid)
#endif
            CALL THE_SECOND_LEVEL_LOOP( iLev_4, 0, iLev_2,
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +mytime, myiter, mythid )
         enddo
      else
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('THE_SECOND_LEVEL_PLAIN',myThid)
#endif
         CALL THE_SECOND_LEVEL_PLAIN( iLev_4, 0, 
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +mytime, myiter, mythid )
      end if
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THE_THIRD_LEVEL_PLAIN',myThid)
#endif

      END
