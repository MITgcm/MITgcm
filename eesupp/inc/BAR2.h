C $Header: /u/gcmpack/MITgcm/eesupp/inc/BAR2.h,v 1.4 2004/03/27 03:51:50 edhill Exp $
C $Name:  $
C
CBOP
C     !ROUTINE: BAR2.h
C     !INTERFACE:
C     include "BAR2.h"
C     !DESCRIPTION:
C     *==========================================================*
C     | BAR2.h                                                    
C     | o Globals used by BAR2 Fortran barrier routine.           
C     |==========================================================*
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
C     |     CALL BAR2\_INIT                                        
C     |     on CRAY, NEC this routine does nothing                
C     |     on T3E there is no single-threaded code               
C     |        but there may be barrier initialisation -          
C     |        need to check.                                     
C     |  2. When we need to synchronize everybody just            
C     |     CALL BAR2( myThid )                                   
C     |     where myThid is myThreadId                            
C     |     on CRAY, NEC FBAR will just do C\$DIR BARRIER          
C     |     or CALL BARRIER or the like.                          
C     |     on T3E FBAR does CALL BARRIER(...) or something       
C     |     need to check this.                                   
C     *==========================================================*
CEOP
      COMMON /BAR2_BUFFER_I/
     &  BAR2_level,
     &  BAR2_barrierCount, 
     &  BAR2_spinsCount, BAR2_spinsMax, BAR2_spinsMin
      INTEGER BAR2_level(cacheLineSize/4,MAX_NO_THREADS)
      INTEGER BAR2_barrierCount(cacheLineSize/4,MAX_NO_THREADS)
      INTEGER BAR2_spinsCount(cacheLineSize/4,MAX_NO_THREADS)
      INTEGER BAR2_spinsMax(cacheLineSize/4,MAX_NO_THREADS)
      INTEGER BAR2_spinsMin(cacheLineSize/4,MAX_NO_THREADS)

      COMMON /BAR2_L/ bar2CollectStatistics
      LOGICAL bar2CollectStatistics
