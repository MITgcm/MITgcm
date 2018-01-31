#include "FIZHI_OPTIONS.h"
      subroutine my_finalize
      implicit none
#ifdef ALLOW_USE_MPI
      integer  ierror
      call mpi_finalize (ierror )
#endif
      return
      end
                                                                                
      subroutine my_exit (irc)
      implicit none
      integer irc
#ifdef ALLOW_USE_MPI
      integer  ierror
      call system ('touch gcm_error')
      call mpi_finalize (ierror)
#endif
      call exit (irc)
      return
      end

