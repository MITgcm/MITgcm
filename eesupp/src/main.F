C $Header: /u/gcmpack/MITgcm/eesupp/src/main.F,v 1.9 2001/09/28 16:49:54 adcroft Exp $
C $Name:  $

CBOI
C
C !TITLE: WRAPPER CODE SYNOPSIS
C !AUTHORS: mitgcm developers ( support@mitgcm.org )
C !AFFILIATION: Massachussetts Institute of Technology
C !DATE:
C !INTRODUCTION: Wrapper synopsis and code
C Routines in the subdirectories under eesupp/ ( src/ and inc/ ) provide the core
C framework within which numerical and ancilliary software of MITgcm operates.
C The eesupp/ directories provide a collection of software we call {\bf WRAPPER} (
C ({\bf W}rappable {\bf A}pplication {\bf P}aralell {\bf P}rogramming {\bf E}nvironment {\bf R}esource).
C The {bf WRAPPER} provides a generic bootstrapping capability to start applications
C in a manner that allows them to exploit single and multi-processing environments on all present 
C day hardware platforms (spanning vector SMP systems to distributed memory and processing cluster
C systems). Numerical applications must be coded to fit within the {\bf WRAPPER}. This entails
C applications adopting a particular style for declaring data structures representing
C grids and values on grids. The {\bf WRAPPER} currently provides support for grid point
C models using a single global indexing system. This is sufficient for latitude-logitude,
C cylindrical, and cartesian coordinate configurations. There is also limited support for
C composing grids in which no single, sructured global index can be defined. At present, this 
C support is limited to specific configurations of projections of a cube onto the sphere.
C
C The main functions supported by the current {\bf WRAPPER} code are
C \begin{itemize}
C  \item program startup and termination including creation/management of multiple 
C        threads and/or processes
C  \item communication and synchronisatioin operations between multiple processes and/or threads 
C  \item multi-process input and output operations to disk and to other 
C  applications
C \end{itemize}
C
C Multi-process execution assumes the existence of MPI for process startup and termination. However,
C MPI does not have to be used for performance critical operations. Instead,
C {\bf WRAPPER} performance critical parallel primitives are implemented to allow them to bind to 
C different low-level system software layers. Bindings exist for using {\bf WRAPPER} with portable
C systems such as MPI and UNIX System V IPC memory mapping, as well bindings for high-performance 
C propreitary systems such as Myrinet GM software and Compaq IMC memory channel technology.
C 
CEOI


C--   Get C preprocessor options
#include "CPP_OPTIONS.h"
#include "CPP_EEOPTIONS.h"

CBOP
C !ROUTINE: MAIN

C !INTERFACE:
      PROGRAM MAIN
      IMPLICIT NONE

C !DESCRIPTION:
C     *==========================================================*
C     | PROGRAM MAIN                                            
C     | o MAIN wrapper for MITgcm UV implementation.            
C     *==========================================================*
C     | MAIN controls the "execution environment".               
C     | Its main functions are                                   
C     | 1. call procedure EEBOOT to perform execution environment
C     |    initialisation.                                       
C     | 2. call procedure THE_MODEL_MAIN once for each concurrent
C     |    thread. THE_MODEL_MAIN is the user supplied top-level 
C     |    routine.                                              
C     | 3. call procedure EEDIE to perform execution environment 
C     |    shutdown.                                             
C     *==========================================================*

C      !CALLING SEQUENCE:
C
C      main()       
C      |
C      |--eeboot()         :: WRAPPER initilization
C      |
C      |--check_threads()  :: Validate multiple thread start up.
C      |
C      |--the_model_main() :: Numerical code top-level driver routine
C      |
C      |--eedie()          :: WRAPPER termination

C     !USES:
C     == Global variables ==
C     Include all the "shared" data here. That means all common
C     blocks used in the model. On many implementations this is not
C     necessary but doing this is the safest method.
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "THE_MODEL_COMMON_BLOCKS.h"

C     !LOCAL VARIABLES:
C--   Local variables
      INTEGER myThid
      INTEGER I
CEOP

C--   Set up the execution environment
C     EEBOOT loads a execution environment parameter file 
C     ( called "eedata" by default ) and sets variables 
C     accordingly.
      CALL EEBOOT

C--   Trap errors 
      IF ( eeBootError ) THEN
       fatalError = .TRUE.
       GOTO 999
      ENDIF

C--   Start nThreads concurrent threads.   
C     Note: We do a fiddly check here. The check is performed
C           by CHECK_THREADS. CHECK_THREADS does a count
C           of all the threads. If after ten seconds it has not
C           found nThreads threads are running it flags an
C           error. This traps the case in which the input 
C           parameter nThreads is different from the actual 
C           number of concurrent threads the OS gives us. This
C           case causes a deadlock if we do not trap it here.
#include "MAIN_PDIRECTIVES1.h"
      DO I=1,nThreads
        myThid = I

C--     Do check to see if there are nThreads threads running
        IF ( .NOT. eeBootError ) THEN
         CALL CHECK_THREADS( myThid )
        ENDIF

C--     Invoke nThreads instances of the numerical model
        IF ( .NOT. eeBootError ) THEN
         CALL THE_MODEL_MAIN(myThid)
        ENDIF

C--     Each threads sets flag indicating it is done
        threadIsComplete(myThid) = .TRUE.
        IF ( .NOT. eeBootError ) THEN
         _BARRIER
        ENDIF
      ENDDO
#include "MAIN_PDIRECTIVES2.h"

  999 CONTINUE
C--   Shut down execution environment
      CALL EEDIE

C--   Write closedown status
      IF ( fatalError ) THEN
       STOP 'ABNORMAL END: PROGRAM MAIN'
      ELSE
       STOP 'NORMAL END'
      ENDIF
C
      END
