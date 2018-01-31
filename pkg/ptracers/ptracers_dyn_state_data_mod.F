#include "PTRACERS_OPTIONS.h"
#ifdef PTRACERS_ALLOW_DYN_STATE

      MODULE PTRACERS_DYN_STATE_DATA_MOD

      use ptracers_dyn_state_mod

C     instance of ptracers internal state data structure:
C     array with one PTRACERS_ISTATE for each Ptracer
      TYPE(PTRACERS_ISTATE), POINTER :: PtrISt(:)

      END MODULE
#else
C     some compilers do not like empty files, so ...
      SUBROUTINE PTRACERS_DYN_STATE_DATA_DUMMY
        RETURN
      END
#endif

