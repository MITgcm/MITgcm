C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/ini_jam.F,v 1.5 2001/09/21 03:54:35 cnh Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

#define USE_JAM_INIT

CBOP

C     !ROUTINE: INI_JAM

C     !INTERFACE:
      SUBROUTINE INI_JAM
      IMPLICIT NONE

C     !DESCRIPTION:
C     *=================================================================================*
C     | SUBROUTINE INI_JAM
C     | o Initialise JAM based communication
C     *=================================================================================*
C     | Connect to synchronising daemon process that allocates jam network ids to
C     | each process and initialises network once all processes are connected.
C     *=================================================================================*


C     !USES:
C     == Global data ==
#ifdef LETS_MAKE_JAM
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "JAM_INFO.h"
#include "MPI_INFO.h"
#ifdef ALLOW_MPI
#include "mpif.h"
#endif

C     !LOCAL VARIABLES:
C     == Local variables ==
C     dummyVal :: Variable which is used in sync. call
C     rc       :: Return code
C     myTwoProcRank :: Temp. for holding ranking within processor pairs.
      Real*8  dummyVal
      INTEGER rc
#ifdef JAM_WITH_TWO_PROCS_PER_NODE
      INTEGER myTwoProcRank
#endif
CEOP

C     Names used in JAM exch testing for MPI based exchange
      mpi_northId = mpiPidN
      mpi_southId = mpiPidS

#ifdef USE_JAM_INIT
C     JAM initialisation. This should work with or without
C     MPI. If we dont use MPI we have to start procs. by hand
C     though!
#ifndef JAM_WITH_TWO_PROCS_PER_NODE
      CALL JAM_collective_init
#else  /* ! JAM_WITH_TWO_PROCS_PER_NODE */
      myTwoProcRank = MOD(myProcId,2)
      IF ( myTwoProcRank .EQ. 0 ) THEN
       myTwoProcRank = 1
      ELSE
       myTwoProcRank = 0
      ENDIF
      CALL JAM_collective_init( myTwoProcRank )
#endif /* JAM_WITH_TWO_PROCS_PER_NODE */

      CALL GET_JAM_self_address(   jam_pid )
      CALL GET_JAM_partition_size( jam_np  )
      jam_exchKey = 100
      dummyVal    = 0.
      CALL JAM_barrier_start( dummyVal )
      CALL JAM_barrier_done(  dummyVal )

C     Set up connectivity
      jam_northId = jam_pid+1
      IF ( jam_northId .EQ. jam_np ) jam_northId = 0
      jam_southId = jam_pid-1
      IF ( jam_southId .LT. 0      ) jam_southId = jam_np-1

      numberOfProcs = jam_np
      myProcId      = jam_pid

#ifdef JAM_WITH_TWO_PROCS_PER_NODE
C     The following fixes the instance where MPI and
C     James JAM arrive at a different id for this process
C     *note* this assumes nPx=1 (no parallization in X)
      myPid         = jam_pid
      pidN          = mod(jam_pid+1,jam_np)
      pidS          = mod(jam_pid-1+jam_np,jam_np)
      pidW          = jam_pid
      pidE          = jam_pid
      myYGlobalLo   = sNy*nSy*jam_pid+1
      myXGlobalLo   = 1
#endif

#endif
#endif /* LETS_MAKE_JAM */

      RETURN
      END
