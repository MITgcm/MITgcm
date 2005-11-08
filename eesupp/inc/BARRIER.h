C $Header: /u/gcmpack/MITgcm/eesupp/inc/BARRIER.h,v 1.7 2005/11/08 15:53:41 cnh Exp $
C $Name:  $
CBOP
C     !ROUTINE: BARRIER.h
C     !INTERFACE:
C     include "BARRIER.h"
C     !DESCRIPTION:
C     *==========================================================*
C     | BARRIER.h                                                 
C     | o Globals used by Fortran barrier routine.                
C     *==========================================================*
C     | These variables support a simple Fortran shared memory    
C     | barrier routine. They do busy waiting, that is the        
C     | thread that is waiting sits in a tight loop reading       
C     | some memory location and waiting for all other threads.   
C     | On some systems this is sometimes a good method to use.   
C     | On the T3E memory is not shared so the routine should     
C     | use the T3E "eureka" barriers. On CRAY and NEC there are  
C     | hardware barriers that are accessed through a compiler    
C     | directives. Finally proper multi-threading compilers      
C     | support barrier compile directives - sometimes these      
C     | are good, sometimes they are lousy.                       
C     |  The barrier mechanism is used as follows                 
C     |  1. In the single-threaded part of the code               
C     |     CALL FBAR\_INIT                                        
C     |     on CRAY, NEC this routine does nothing                
C     |     on T3E there is no single-threaded code               
C     |        but there may be barrier initialisation -          
C     |        need to check.                                     
C     |  2. When we need to synchronize everybody just            
C     |     CALL FBAR( myThid )                                   
C     |     where myThid is myThreadId                            
C     |     on CRAY, NEC FBAR will just do C\$DIR BARRIER          
C     |     or the like.                                          
C     |     on T3E FBAR does CALL BARRIER(...) or something       
C     |     need to check this.                                   
C     *==========================================================*
CEOP
      COMMON / BARRIER_COMMON / key1,  key2,  key3, 
     &                          door1, door2, door3,
     &                          bCount, masterSet
      INTEGER key1(lShare4,MAX_NO_THREADS)
      INTEGER key2(lShare4,MAX_NO_THREADS)
      INTEGER key3(lShare4,MAX_NO_THREADS)
      INTEGER door1
      INTEGER door2
      INTEGER door3
      INTEGER VALID
      PARAMETER ( VALID = 1 )
      INTEGER INVALID
      PARAMETER ( INVALID = 0 )
      INTEGER OPEN
      PARAMETER ( OPEN = 1 )
      INTEGER SHUT
      PARAMETER ( SHUT = 0 )
      INTEGER bCount(MAX_NO_THREADS)
      INTEGER masterSet(MAX_NO_THREADS)
