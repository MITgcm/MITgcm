C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_mpistuff.F,v 1.2 2004/06/04 19:03:10 molod Exp $
C $Name:  $

      subroutine my_finalize
      implicit none
      integer  ierror
#ifdef ALLOW_USE_MPI
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

