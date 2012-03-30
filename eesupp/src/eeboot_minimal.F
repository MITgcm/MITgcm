C $Header: /u/gcmpack/MITgcm/eesupp/src/eeboot_minimal.F,v 1.24 2012/03/30 14:16:18 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: EEBOOT_MINIMAL

C     !INTERFACE:
      SUBROUTINE EEBOOT_MINIMAL

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EEBOOT\_MINIMAL
C     | o Set an initial environment that is predictable i.e.
C     | behaves in a similar way on all machines and stable.
C     *==========================================================*
C     | Under MPI this routine calls MPI\_INIT to setup the
C     | mpi environment ( on some systems the code is running as
C     | a single process prior to MPI\_INIT, on others the mpirun
C     | script has already created multiple processes). Until
C     | MPI\_Init is called it is unclear what state the
C     | application is in. Once this routine has been run it is
C     | "safe" to do things like I/O to report erros and to get
C     | run parameters.
C     | Note: This routine can also be compiled with CPP
C     | directives set so that no multi-processing is initialise.
C     | This is OK and will work fine.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !LOCAL VARIABLES:
C     == Local variables ==
C     myThid     :: Temp. dummy thread number.
C     fNam       :: Used to build file name for standard and error output.
C     msgBuf     :: Used to build messages for printing.
      INTEGER myThid
      CHARACTER*13 fNam
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_USE_MPI
C     mpiRC      :: Error code reporting variable used with MPI.
      INTEGER mpiRC
      LOGICAL doReport
#ifdef COMPONENT_MODULE
      INTEGER mpiMyWid
#endif
#if defined(ALLOW_NEST_PARENT) || defined(ALLOW_NEST_CHILD)
      INTEGER mpiMyWid, color
#endif
#endif /* ALLOW_USE_MPI */
CEOP

C--   Default values set to single processor case
      numberOfProcs = 1
      myProcId      = 0
      pidIO         = myProcId
      myProcessStr  = '------'
C     Set a dummy value for myThid because we are not multi-threading yet.
      myThid        = 1

      IF ( .NOT.usingMPI ) THEN

        WRITE(myProcessStr,'(I4.4)') myProcId
        WRITE(fNam,'(A,A)') 'STDERR.', myProcessStr(1:4)
        OPEN(errorMessageUnit,FILE=fNam,STATUS='unknown')
c       WRITE(fNam,'(A,A)') 'STDOUT.', myProcessStr(1:4)
c       OPEN(standardMessageUnit,FILE=fNam,STATUS='unknown')

#ifdef ALLOW_USE_MPI
      ELSE
C--   MPI style multiple-process initialisation
C--   =========================================

C--    Initialise MPI multi-process parallel environment.
C      On some systems program forks at this point. Others have already
C      forked within mpirun - now thats an open standard!
       CALL MPI_INIT( mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuf,'(A,I5)')
     &        'EEBOOT_MINIMAL: MPI_INIT return code', mpiRC
        CALL PRINT_ERROR( msgBuf, myThid )
        GOTO 999
       ENDIF

C--    MPI has now been initialized but now we need to either
C      ask for a communicator or pretend that we have:
C      Pretend that we have asked for a communicator
       MPI_COMM_MODEL = MPI_COMM_WORLD
       doReport = .FALSE.

#ifdef ALLOW_OASIS
C      add a 1rst preliminary call EESET_PARAMS to set useOASIS
C      (needed to decide either to call OASIS_INIT or not)
       CALL EESET_PARMS ( doReport )
       IF ( useOASIS ) CALL OASIS_INIT(MPI_COMM_MODEL)
#endif /* ALLOW_OASIS */

#ifdef COMPONENT_MODULE
C--    Set the running directory
       CALL MPI_COMM_RANK( MPI_COMM_WORLD, mpiMyWId, mpiRC )
       CALL SETDIR( mpiMyWId )

C- jmc: test:
C      add a 1rst preliminary call EESET_PARAMS to set useCoupler
C      (needed to decide either to call CPL_INIT or not)
       CALL EESET_PARMS ( doReport )
C- jmc: test end ; otherwise, uncomment next line:
c      useCoupler = .TRUE.

C--    Ask coupler interface for a communicator
       IF ( useCoupler) CALL CPL_INIT
#endif /* COMPONENT_MODULE */

C--    Case with Nest(ing)
#if defined(ALLOW_NEST_PARENT) || defined(ALLOW_NEST_CHILD)
C--    Set the running directory
       CALL MPI_COMM_RANK( MPI_COMM_WORLD, mpiMyWId, mpiRC )
       CALL SETDIR( mpiMyWId )

C--    Setup Nesting Execution Environment
       CALL NEST_EEINIT( mpiMyWId, color )
#endif /* ALLOW_NEST_PARENT | ALLOW_NEST_CHILD */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--    Get my process number
       CALL MPI_COMM_RANK( MPI_COMM_MODEL, mpiMyId, mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuf,'(A,I5)')
     &        'EEBOOT_MINIMAL: MPI_COMM_RANK return code', mpiRC
        CALL PRINT_ERROR( msgBuf, myThid )
        GOTO 999
       ENDIF
       myProcId = mpiMyId
       WRITE(myProcessStr,'(I4.4)') myProcId
       mpiPidIo = myProcId
       pidIO    = mpiPidIo
       IF ( mpiPidIo .EQ. myProcId ) THEN
        WRITE(fNam,'(A,A)') 'STDERR.', myProcessStr(1:4)
        OPEN(errorMessageUnit,FILE=fNam,STATUS='unknown')
        WRITE(fNam,'(A,A)') 'STDOUT.', myProcessStr(1:4)
        OPEN(standardMessageUnit,FILE=fNam,STATUS='unknown')
       ENDIF

#if defined(ALLOW_NEST_PARENT) || defined(ALLOW_NEST_CHILD)
       WRITE(standardMessageUnit,'(2(A,I6))')
     &           ' mpiMyWId =', mpiMyWId, ' , color =',color
#endif /* ALLOW_NEST_PARENT | ALLOW_NEST_CHILD */

C--    Synchronise all processes
C      Strictly this is superfluous, but by using it we can guarantee to
C      find out about processes that did not start up.
       CALL MPI_BARRIER( MPI_COMM_MODEL, mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuf,'(A,I6)')
     &        'EEBOOT_MINIMAL: MPI_BARRIER return code', mpiRC
        CALL PRINT_ERROR( msgBuf, myThid )
        GOTO 999
       ENDIF

C--    Get number of MPI processes
       CALL MPI_COMM_SIZE ( MPI_COMM_MODEL, mpiNProcs, mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuf,'(A,I6)')
     &        'EEBOOT_MINIMAL: MPI_COMM_SIZE return code', mpiRC
        CALL PRINT_ERROR( msgBuf, myThid )
        GOTO 999
       ENDIF
       numberOfProcs = mpiNProcs

#endif /* ALLOW_USE_MPI */
      ENDIF

C--    Can not have more processes than compile time MAX_NO_PROCS
       IF ( numberOfProcs .GT. MAX_NO_PROCS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuf,'(A,2I6)')
     &    'EEBOOT_MINIMAL: Nb. of procs exceeds MAX_NO_PROCS',
     &    numberOfProcs, MAX_NO_PROCS
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)')
     &    ' Needs to increase MAX_NO_PROCS',
     &    ' in file "EEPARAMS.h" and to re-compile'
        CALL PRINT_ERROR( msgBuf, myThid )
        GOTO 999
       ENDIF
C--    Under MPI only allow same number of processes as proc grid size.
C      Strictly we are allowed more procs but knowing there
C      is an exact match makes things easier.
       IF ( numberOfProcs .NE. nPx*nPy ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuf,'(2(A,I6))')
     &  'EEBOOT_MINIMAL: No. of procs=', numberOfProcs,
     &  ' not equal to nPx*nPy=', nPx*nPy
        CALL PRINT_ERROR( msgBuf, myThid )
        GOTO 999
       ENDIF

#ifdef USE_LIBHPM
       CALL F_HPMINIT(myProcId, "mitgcmuv")
#endif

 999  CONTINUE

      RETURN
      END
