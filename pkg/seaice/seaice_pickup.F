C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/seaice_pickup.F,v 1.10 2006/03/20 21:36:11 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

C--   File seaice_pickup.F: Routines to handle reading and writing
C--   sea ice pickup files.
C--   Contents
C--   o SEAICE_READ_PICKUP  - Read sea ice pickup file for restarting.
C--   o SEAICE_WRITE_PICKUP - Write out sea ice pickup file.

CStartofinterface
      SUBROUTINE SEAICE_READ_PICKUP ( myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_READ_PICKUP                            |
C     | o Read in sea ice pickup file for restarting.            |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEndofinterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn

C--    Going to do some IO. Make everyone except master thread wait.
       _BARRIER
       _BEGIN_MASTER( myThid )

C--    Read ice model fields
       WRITE(fn,'(A,I10.10)') 'pickup_seaice.',nIter0
#ifdef SEAICE_MULTILEVEL
       CALL MDSREADFIELD
     &      ( fn, precFloat64, 'RL', MULTDIM, TICES, 1, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, YNEG , 8, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, HSNOW, 9, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, UICE , 4, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, VICE , 5, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, HEFF , 6, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, AREA , 7, myThid )
#if (defined(SEAICE_CGRID) && defined(SEAICE_ALLOW_EVP))
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, 
     &      seaice_sigma1,  22, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, 
     &      seaice_sigma2,  23, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, 
     &      seaice_sigma12, 24, myThid )
#endif /* SEAICE_ALLOW_EVP */
#else /* SEAICE_MULTILEVEL */
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, TICE , 1, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, YNEG , 2, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, HSNOW, 3, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, UICE , 2, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, VICE , 3, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, HEFF , 4, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 3, AREA , 5, myThid )
#if (defined(SEAICE_CGRID) && defined(SEAICE_ALLOW_EVP))
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, 
     &      seaice_sigma1,  16, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, 
     &      seaice_sigma2,  17, myThid )
       CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, 
     &      seaice_sigma12, 18, myThid )
#endif /* SEAICE_ALLOW_EVP */
#endif /* SEAICE_MULTILEVEL */

       _END_MASTER( myThid )
       _BARRIER

C--    Update overlap regions
       CALL SEAICE_EXCH_UV ( UICE, VICE, myThid )
       CALL SEAICE_EXCH ( HEFF, myThid )
       CALL SEAICE_EXCH ( AREA, myThid )
       _EXCH_XY_R8(TICE , myThid )
       _EXCH_XY_R8(YNEG , myThid )
       _EXCH_XY_R8(HSNOW, myThid )
       _EXCH_XY_R8(seaice_sigma1 , myThid )
       _EXCH_XY_R8(seaice_sigma2 , myThid )
       _EXCH_XY_R8(seaice_sigma12, myThid )

      RETURN
      END

CStartofinterface
      SUBROUTINE SEAICE_WRITE_PICKUP ( 
     I               prec, lgf, permCheckPoint, myIt, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_WRITE_PICKUP                           |
C     | o Write sea ice pickup file for restarting.              |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE.h"

C     == Routine arguments ==
C     prec     - Precision of pickup file ( precFloat64 )
C     lgf      - Flag to indicate whether to use global file mode.
C     permCheckPoint - Flag indicating whether a permanent checkpoint
C                      will be written.
C     myIt     - Iteration number
C     myThid   - Thread number for this instance of the routine.
      INTEGER prec
      LOGICAL permCheckPoint    
      INTEGER myThid
      INTEGER myIt
CEndofinterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      LOGICAL lgf

C--    Write model fields
       IF ( permCheckPoint ) THEN
        WRITE(fn,'(A,I10.10)') 'pickup_seaice.',myIt
       ELSE
        WRITE(fn,'(A,A)') 'pickup_seaice.',checkPtSuff(nCheckLev)
       ENDIF
#ifdef SEAICE_MULTILEVEL
       CALL MDSWRITEFIELD
     &      (fn,prec,lgf,'RL', MULTDIM,  TICES,1, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,  YNEG, 8, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, HSNOW, 9, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  UICE, 4, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  VICE, 5, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  HEFF, 6, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  AREA, 7, myIt,myThid)
#if (defined(SEAICE_CGRID) && defined(SEAICE_ALLOW_EVP))
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, 
     &      seaice_sigma1,  22, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, 
     &      seaice_sigma2,  23, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, 
     &      seaice_sigma12, 24, myIt,myThid)
#endif /* SEAICE_ALLOW_EVP */
#else /* SEAICE_MULTILEVEL */
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,  TICE, 1, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,  YNEG, 2, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, HSNOW, 3, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  UICE, 2, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  VICE, 3, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  HEFF, 4, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 3,  AREA, 5, myIt,myThid)
#if (defined(SEAICE_CGRID) && defined(SEAICE_ALLOW_EVP))
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, 
     &      seaice_sigma1,  16, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, 
     &      seaice_sigma2,  17, myIt,myThid)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, 
     &      seaice_sigma12, 18, myIt,myThid)
#endif /* SEAICE_ALLOW_EVP */
#endif /* SEAICE_MULTILEVEL */

      RETURN
      END
