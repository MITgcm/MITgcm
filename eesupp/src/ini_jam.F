C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/ini_jam.F,v 1.1 2000/03/14 16:10:23 adcroft Exp $

#include "CPP_EEOPTIONS.h"

#define USE_JAM_INIT

      SUBROUTINE INI_JAM

C     Initialise JAM based communication
      IMPLICIT NONE

#ifdef LETS_MAKE_JAM

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

#include "JAM_INFO.h"
#include "MPI_INFO.h"

#ifdef ALLOW_MPI
#include "mpif.h"
#endif

C     == Local variables ==
      REAL*8  dummyVal
      INTEGER rc
      CHARACTER*(MAX_LEN_FNAM) fnam
#ifdef JAM_WITH_TWO_PROCS_PER_NODE
      INTEGER myTwoProcRank
#endif

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

#endif
#endif /* LETS_MAKE_JAM */

      RETURN
      END
