C $Header: /u/gcmpack/MITgcm/verification/OpenAD/code_oad_all/Attic/the_third_level_loop.F,v 1.3 2011/05/24 20:59:04 jmc Exp $
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
     +theCurrentStep,
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
# ifdef ALLOW_CAL
#  include "cal.h"
# endif
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
#  include "KPP_DIAGS.h"
#  include "KPP_PARAMS.h"
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
      integer theCurrentStep
      INTEGER myThid 
      INTEGER myIter
      _RL     myTime

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer ilev_2

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_THIRD_LEVEL_LOOP',myThid)
#endif
#ifdef OAD_DEBUG_JOINT
      write(standardmessageunit,'(A,2(I6,A))')
     + 'OAD: THE_THIRD_LEVEL_LOOP(',iLev_4,',',iLev_3,')'
#endif
      do ilev_2 = 1,uCheckLev2 
         if (theCurrentStep .le. nTimeSteps) then
#ifdef ALLOW_DEBUG
            IF (debugMode) CALL DEBUG_CALL('THE_SECOND_LEVEL_LOOP',
     +myThid)
#endif
            CALL THE_SECOND_LEVEL_LOOP( iLev_4, iLev_3, iLev_2,
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +theCurrentStep,
     +mytime, myiter, mythid )
         end if
      end do
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THE_THIRD_LEVEL_LOOP',myThid)
#endif

      END

      SUBROUTINE THE_THIRD_LEVEL_PLAIN(
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +theCurrentStep,
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
# ifdef ALLOW_CAL
#  include "cal.h"
# endif
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
#  include "KPP_DIAGS.h"
#  include "KPP_PARAMS.h"
# endif
# ifdef ALLOW_DIVIDED_ADJOINT_MPI
#  include "mpif.h"
# endif

# include "tamc.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"

#endif /* ALLOW_AUTODIFF_TAMC */

      integer  uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4
      integer theCurrentStep
      INTEGER myThid 
      INTEGER myIter
      _RL     myTime

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer ilev_2

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THE_THIRD_LEVEL_PLAIN',myThid)
#endif
#ifdef OAD_DEBUG_JOINT
      write(standardmessageunit,'(A)')
     + 'OAD: THE_THIRD_LEVEL_PLAIN()'
#endif
      if (uCheckLev2 .gt. 0 ) then 
         do ilev_2 = 1,uCheckLev2 
            if (theCurrentStep .le. nTimeSteps) then
#ifdef ALLOW_DEBUG
               IF (debugMode) CALL DEBUG_CALL('THE_SECOND_LEVEL_LOOP',
     +myThid)
#endif
               CALL THE_SECOND_LEVEL_LOOP( 1, 1, iLev_2,
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +theCurrentStep,
     +mytime, myiter, mythid )
            end if
         end do
      else
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('THE_SECOND_LEVEL_PLAIN',myThid)
#endif
         CALL THE_SECOND_LEVEL_PLAIN( 
     +uCheckLev1, uCheckLev2, uCheckLev3,uCheckLev4,
     +theCurrentStep,
     +mytime, myiter, mythid )
      end if
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THE_THIRD_LEVEL_PLAIN',myThid)
#endif

      END
