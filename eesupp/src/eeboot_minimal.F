C $Header: /u/gcmpack/MITgcm/eesupp/src/eeboot_minimal.F,v 1.8 2001/09/21 03:54:34 cnh Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: EEBOOT_MINIMAL

C     !INTERFACE:
      SUBROUTINE EEBOOT_MINIMAL
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EEBOOT_MINIMAL                                 
C     | o Set an initial environment that is predictable i.e.     
C     | behaves in a similar way on all machines and stable.      
C     *==========================================================*
C     | Under MPI this routine calls MPI_INIT to setup the        
C     | mpi environment ( on some systems the code is running as  
C     | a single process prior to MPI_INIT, on others the mpirun  
C     | script has already created multiple processes). Until     
C     | MPI_Init is called it is unclear what state the           
C     | application is in. Once this routine has been run it is   
C     | "safe" to do things like I/O to report erros and to get   
C     | run parameters.                                           
C     | Note: This routine can also be compiled with CPP          
C     | directives set so that no multi-processing is initialise. 
C     | This is OK and will work fine.                          
C     *==========================================================*

C     !USES:
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !LOCAL VARIABLES:
C     == Local variables ==
C     myThid           :: Temp. dummy thread number.
      INTEGER myThid     
#ifdef ALLOW_USE_MPI
C     mpiRC            :: Error code reporting variable used 
C                         with MPI.
C     fNam             :: Used to build name of file for standard
C                         output and error output.
C     msgBuffer        :: Used to build messages for printing.
      CHARACTER*(MAX_LEN_MBUF) msgBuffer
      INTEGER mpiRC
      CHARACTER*13 fNam
#endif /* ALLOW_USE_MPI */
CEOP

C--   Default values set to single processor case
      numberOfProcs = 1
      myProcId      = 0
      pidIO         = myProcId 
      myProcessStr  = '------'
C     Set a dummy value for myThid because we are not multi-threading
C     yet.
      myThid        = 1
#ifdef ALLOW_USE_MPI
C--
C--   MPI style multiple-process initialisation
C--   =========================================
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
C--    Initialise MPI multi-process parallel environment.
C      On some systems program forks at this point. Others have already
C      forked within mpirun - now thats an open standard!
       CALL MPI_INIT( mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &        'S/R INI_PROCS: MPI_INIT return code',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF
C--    MPI has now been initialized but now we need to either
C      ask for a communicator or pretend that we have:
C      Pretend that we have asked for a communicator
       MPI_COMM_MODEL = MPI_COMM_WORLD
C      Ask coupler interface for a communicator
c hook call MITCOMPONENT_init( 'MITgcmUV', MPI_COMM_MODEL )

C--    Get my process number
       CALL MPI_COMM_RANK( MPI_COMM_MODEL, mpiMyId, mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &        'S/R INI_PROCS: MPI_COMM_RANK return code',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
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

C--    Synchronise all processes
C      Strictly this is superfluous, but by using it we can guarantee to 
C      find out about processes that did not start up.
       CALL MPI_BARRIER( MPI_COMM_MODEL, mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &        'S/R INI_PROCS: MPI_BARRIER return code',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF

C--    Get number of MPI processes
       CALL MPI_COMM_SIZE ( MPI_COMM_MODEL, mpiNProcs, mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &        'S/R INI_PROCS: MPI_COMM_SIZE return code',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF
       numberOfProcs = mpiNProcs

C--    Can not have more processes than compile time MAX_NO_PROCS
       IF ( numberOfProcs .GT. MAX_NO_PROCS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A)')
     &  'S/R INI_PROCS: No. of processes too large'
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF
C--    Under MPI only allow same number of processes as proc.
C--    grid size.
C      Strictly we are allowed more procs. but knowing there
C      is an exact match makes things easier.
       IF ( numberOfProcs .NE. nPx*nPy ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A)')
     &  'S/R INI_PROCS: No. of processes not equal to nPx*nPy'
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF

#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /* ALLOW_USE_MPI */

 999  CONTINUE

      RETURN
      END

