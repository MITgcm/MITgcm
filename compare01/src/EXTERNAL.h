C $Id: EXTERNAL.h,v 1.1 1998/05/25 20:21:06 cnh Exp $
C Declarations of external function types.
C     Model defined functions
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL IFNBLNK
      INTEGER  IFNBLNK
      EXTERNAL ILNBLNK
      INTEGER  ILNBLNK
      EXTERNAL TIMER_INDEX
      INTEGER  TIMER_INDEX
C     System or library routines
#ifdef USE_ETIME
      EXTERNAL ETIME
      Real*4   ETIME
#endif
