#include "PTRACERS_OPTIONS.h"
#ifdef PTRACERS_ALLOW_DYN_STATE

      MODULE PTRACERS_DYN_STATE_DATA_MOD

      use ptracers_dyn_state_mod

! instance of ptracers internal state data structure:
! array with one PTRACERS_ISTATE for each Ptracer
      TYPE(PTRACERS_ISTATE), POINTER :: PtrISt(:)

      END MODULE
#else
!     Nothing
#endif

