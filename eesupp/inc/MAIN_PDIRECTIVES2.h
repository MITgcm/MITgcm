!BOP
! !ROUTINE: MAIN_PDIRECTIVES2.h
! !INTERFACE:
! include "MAIN_PDIRECTIVES2.h"
! !DESCRIPTION:
! *==========================================================*
! | MAIN\_PDIRECTIVES2.h
! *==========================================================*
! | Parallel directives to generate multithreaded code for
! | various different compilers. The master preprocessor
! | file CPP\_OPTIONS is used to select which of these
! | options is included in the code.
! | Note: Only some of the directives require end blocks.
! |      For directives which do not require end blocks there
! |      is no entry here.
! *==========================================================*
!EOP

#if defined USE_KAP_THREADING
!--
!--  Parallel directives for Kuck and Associates compiler.
!--  This is used to generate multi-threaded code on Digital
!--  systems. It can also be used under NT.
!--  Note: The KAP parallel proceesing tool has several bugs
!--        which means that if there are more threads (set via
!--        setenv PARALLEL) than iterations in the parallel
!--        loop the extra threads start on the section
!--        after the loop!
!--        To work around this we could place an extra dummy
!--        parallel section here. KAP places a barrier at the
!--        start of each parallel region which ensures that
!--        the extra threads wait (note this wait is in a busy loop).
!--        Without this feature the extra thread(s) will run on and may
!--        halt the program by calling STOP! Unfortunately that seems
!--        to cause a deadlock in a KAP library routine! Instead the
!--        current solution is to check for a thread reaching the
!--        shutdown part of main.F before other threads have
!--        completed computation ( see eedie.F ).
!
!*KAP*  END PARALLEL REGION
!   C*KAP*  PARALLEL REGION
!       CALL FOOL_THE_COMPILER
!   C*KAP*  END PARALLEL REGION
!
#endif
!

#ifdef USE_OMP_THREADING
!$OMP  END PARALLEL
#endif
