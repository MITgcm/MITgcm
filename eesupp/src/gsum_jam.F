C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/gsum_jam.F,v 1.4 2001/09/21 03:54:35 cnh Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

#undef  USE_MPI_GSUM
#define USE_JAM_GSUM

CBOP
C     !ROUTINE: GLOBAL_SUM_R8_JAM

C     !INTERFACE:
      SUBROUTINE GLOBAL_SUM_R8_JAM( ans, myThid )

C     !DESCRIPTION:
C     *=========================================================================*
C     | SUBROUTINE GLOBAL_SUM_R8_JAM
C     | o JAM binding of global sum routine
C     *=========================================================================*
C     | Does a super fast global sum over the JAM library. With JAM
C     | timings for dual proc SMP nodes over Arctic are about 
C     | 4.3usecs log2(NP/2). When it was done this was faster than any other 
C     | cluster on the planet :-).
C     *=========================================================================*

C     !USES:
#ifdef ALLOW_MPI
#include "mpif.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     ans    :: Returned sum value
C     myThid :: Thread number of this instance
      Real*8 ans
      INTEGER myThid

#ifdef LETS_MAKE_JAM

C     !LOCAL VARIABLES:
C     phi :: Temp. for accumulating sum
C     RC  :: Return code
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
