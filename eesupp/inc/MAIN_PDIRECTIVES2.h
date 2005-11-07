C $Header: /u/gcmpack/MITgcm/eesupp/inc/MAIN_PDIRECTIVES2.h,v 1.7 2005/11/07 18:16:08 cnh Exp $
C $Name:  $
CBOP
C     !ROUTINE: MAIN_PDIRECTIVES2.h
C     !INTERFACE:
C     include "MAIN_PDIRECTIVES2.h"
C     !DESCRIPTION:
C     *==========================================================*
C     | MAIN\_PDIRECTIVES2.h                                       
C     *==========================================================*
C     | Parallel directives to generate multithreaded code for    
C     | various different compilers. The master preprocessor      
C     | file CPP\_OPTIONS is used to select which of these 
C     | options is included in the code.           
C     | Note: Only some of the directives require end blocks.     
C     |      For directives which do not require end blocks there 
C     |      is no entry here.                                    
C     *==========================================================*
CEOP

#if defined USE_KAP_THREADING
C--
C--  Parallel directives for Kuck and Associates compiler.
C--  This is used to generate multi-threaded code on Digital
C--  systems. It can also be used under NT. 
C--  Note: The KAP parallel proceesing tool has several bugs
C--        which means that if there are more threads (set via
C--        setenv PARALLEL) than iterations in the parallel
C--        loop the extra threads start on the section 
C--        after the loop!
C--        To work around this we could place an extra dummy
C--        parallel section here. KAP places a barrier at the
C--        start of each parallel region which ensures that 
C--        the extra threads wait (note this wait is in a busy loop).
C--        Without this feature the extra thread(s) will run on and may
C--        halt the program by calling STOP! Unfortunately that seems
C--        to cause a deadlock in a KAP library routine! Instead the
C--        current solution is to check for a thread reaching the
C--        shutdown part of main.F before other threads have
C--        completed computation ( see eedie.F ).
C
C*KAP*  END PARALLEL REGION
C   C*KAP*  PARALLEL REGION
C           CALL FOOL_THE_COMPILER
C   C*KAP*  END PARALLEL REGION
C
#endif
C

#ifdef USE_OMP_THREADING
C$OMP  END PARALLEL
#endif 
