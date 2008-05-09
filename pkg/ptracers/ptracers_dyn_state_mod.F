#include "PTRACERS_OPTIONS.h"
#ifdef PTRACERS_ALLOW_DYN_STATE

      MODULE PTRACERS_DYN_STATE_MOD

! ptracers internal state data structures
      TYPE PTRACERS_ISTATE 
       _RL, POINTER :: som_P(:,:,:,:,:,:)
      END TYPE

      CONTAINS
      SUBROUTINE PTRACERS_INIT_FIXED_DYNAMIC(
     &                                  PTR_ISTATE_ARR,
     &                                  PTRACERS_numInUse,
     &                                  PTRACERS_SOM_Advection,
     &                                  sNx, sNy, Nr, OLx, OLy,
     &                                  nSx, nSy, nSOM,
     &                                  myThid )

        IMPLICIT NONE

        TYPE(PTRACERS_ISTATE), POINTER :: PTR_ISTATE_ARR(:)
        INTEGER PTRACERS_numInUse
        LOGICAL PTRACERS_SOM_Advection(PTRACERS_numInUse)
        INTEGER sNx, sNy, Nr, OLx, OLy, nSx, nSy, nSOM
        INTEGER myThid

        INTEGER iPtr

        ALLOCATE( PTR_ISTATE_ARR(PTRACERS_numInUse) )

        DO iPtr = 1, PTRACERS_numInUse
         IF ( PTRACERS_SOM_Advection(iPtr) ) THEN
          ALLOCATE( PTR_ISTATE_ARR(iPtr)%som_P(1-OLx:sNx+OLx,
     &                                         1-OLy:sNy+OLy,
     &                                         Nr,
     &                                         nSx,
     &                                         nSy,
     &                                         nSOM) )
         ENDIF
        ENDDO

        RETURN
      END SUBROUTINE

      END MODULE
#else
!     some compilers do not like empty files, so ...
      SUBROUTINE PTRACERS_DYN_STATE_MOD_DUMMY
        RETURN
      END SUBROUTINE
#endif

