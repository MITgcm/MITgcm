C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/gsum_jam.F,v 1.3 2001/02/04 14:38:43 cnh Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

#undef  USE_MPI_GSUM
#define USE_JAM_GSUM

      SUBROUTINE GLOBAL_SUM_R8_JAM( ans, myThid )

#ifdef ALLOW_MPI
#include "mpif.h"
#endif

      Real*8 ans
      INTEGER myThid

#ifdef LETS_MAKE_JAM

      Real*8 phi
      INTEGER RC

      phi = ans

#ifdef USE_MPI_GSUM
      CALL MPI_Allreduce( phi,
     &                    ans,
     &                      1,
     &                    MPI_REAL8,
     &                    MPI_SUM,
     &                    MPI_COMM_WORLD,
     &                    rc
     &                  )
#endif

#ifdef USE_JAM_GSUM
C     JAM global sum
      CALL JAM_barrier_start( phi )
      CALL JAM_barrier_done(  ans )
#endif

C     WRITE(6,*) ' phi = ', phi, ' Sum = ', ans
C     CALL MPI_Finalize( rc )
C     STOP

#endif /* LETS_MAKE_JAM */

      RETURN
      END
