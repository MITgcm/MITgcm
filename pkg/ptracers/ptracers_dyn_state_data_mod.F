C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_dyn_state_data_mod.F,v 1.5 2008/05/12 14:08:24 jahn Exp $
C $Name:  $

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

