C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/seaice_pickup.F,v 1.18 2007/09/15 01:38:59 dimitri Exp $
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
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEndofinterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER iRec

C--    Going to do some IO. Make everyone except master thread wait.
       _BARRIER
       _BEGIN_MASTER( myThid )

C--    Read ice model fields
       WRITE(fn,'(A,I10.10)') 'pickup_seaice.',nIter0
       iRec = 1
#ifdef SEAICE_MULTICATEGORY
       CALL MDSREADFIELD(fn,precFloat64,'RL',MULTDIM,TICES,iRec,myThid)
       iRec = iRec + MULTDIM
#else
       CALL MDSREADFIELD(fn,precFloat64,'RL',1,TICE,iRec,myThid)
       iRec = iRec + 1
#endif /* SEAICE_MULTICATEGORY */
       CALL MDSREADFIELD(fn,precFloat64,'RL',1,YNEG,iRec,myThid)
       iRec = iRec + 1
       CALL MDSREADFIELD(fn,precFloat64,'RL',1,HSNOW,iRec,myThid)
       iRec = int((iRec+3)/3)
       CALL MDSREADFIELD(fn,precFloat64,'RL',3,UICE,iRec,myThid)
       iRec = iRec + 1
       CALL MDSREADFIELD(fn,precFloat64,'RL',3,VICE,iRec,myThid)
       iRec = iRec + 1
       CALL MDSREADFIELD(fn,precFloat64,'RL',3,HEFF,iRec,myThid)
       iRec = iRec + 1
       CALL MDSREADFIELD(fn,precFloat64,'RL',3,AREA,iRec,myThid)
       iRec = iRec * 3 + 1
#if (defined(SEAICE_CGRID) && defined(SEAICE_ALLOW_EVP))
       IF ( SEAICEuseEVP .AND. SEAICEuseEVPpickup ) THEN
        CALL MDSREADFIELD(fn,precFloat64,'RL',1,
     &       seaice_sigma1,iRec,myThid )
        iRec = iRec + 1
        CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, 
     &       seaice_sigma2,iRec,myThid )
        iRec = iRec + 1
        CALL MDSREADFIELD( fn, precFloat64, 'RL', 1, 
     &       seaice_sigma12,iRec,myThid )
        iRec = iRec + 1
       ENDIF
#endif /* SEAICE_ALLOW_EVP */
#ifdef SEAICE_SALINITY
       CALL MDSREADFIELD(fn,precFloat64,'RL',1,HSALT,iRec,myThid)
#endif

       _END_MASTER( myThid )
       _BARRIER

C--    Update overlap regions
       CALL EXCH_UV_3D_RL( UICE, VICE, .TRUE., 3, myThid )
       CALL EXCH_3D_RL   ( HEFF, 3, myThid )
       CALL EXCH_3D_RL   ( AREA, 3, myThid )
#ifdef SEAICE_MULTICATEGORY
cdm       CALL EXCH_3D_RL   ( TICES, MULTDIM, myThid )
#else
       _EXCH_XY_R8(TICE , myThid )
#endif /* SEAICE_MULTICATEGORY */
       _EXCH_XY_R8(YNEG , myThid )
       _EXCH_XY_R8(HSNOW, myThid )
#if (defined(SEAICE_CGRID) && defined(SEAICE_ALLOW_EVP))
       IF ( SEAICEuseEVP .AND. SEAICEuseEVPpickup ) THEN
          _EXCH_XY_R8(seaice_sigma1 , myThid )
          _EXCH_XY_R8(seaice_sigma2 , myThid )
          _EXCH_XY_R8(seaice_sigma12, myThid )
       ENDIF
#endif /* SEAICE_CGRID SEAICE_ALLOW_EVP */
#ifdef SEAICE_SALINITY
       _EXCH_XY_R8(HSALT, myThid )
#endif

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
#include "SEAICE_PARAMS.h"
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
      INTEGER iRec
      LOGICAL lgf

C--    Write model fields
       IF ( permCheckPoint ) THEN
        WRITE(fn,'(A,I10.10)') 'pickup_seaice.',myIt
       ELSE
        WRITE(fn,'(A,A)') 'pickup_seaice.',checkPtSuff(nCheckLev)
       ENDIF
       iRec = 1
#ifdef SEAICE_MULTICATEGORY
       CALL MDSWRITEFIELD
     &      (fn,prec,lgf,'RL',MULTDIM,TICES,iRec,myIt,myThid)
       iRec = iRec + MULTDIM
#else
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,TICE,iRec,myIt,myThid)
       iRec = iRec + 1
#endif /* SEAICE_MULTICATEGORY */
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,YNEG,iRec,myIt,myThid)
       iRec = iRec + 1
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,HSNOW,iRec,myIt,myThid)
       iRec = int((iRec+3)/3)
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',3,UICE,iRec,myIt,myThid)
       iRec = iRec + 1
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',3,VICE,iRec,myIt,myThid)
       iRec = iRec + 1
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',3,HEFF,iRec,myIt,myThid)
       iRec = iRec + 1
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',3,AREA,iRec,myIt,myThid)
       iRec = iRec * 3 + 1
#if (defined(SEAICE_CGRID) && defined(SEAICE_ALLOW_EVP))
       IF ( SEAICEuseEVP ) THEN
          CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, 
     &         seaice_sigma1,iRec,myIt,myThid)
          iRec = iRec + 1
          CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, 
     &         seaice_sigma2,iRec,myIt,myThid)
          iRec = iRec + 1
          CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1, 
     &         seaice_sigma12,iRec,myIt,myThid)
          iRec = iRec + 1
       ENDIF
#endif /* SEAICE_ALLOW_EVP */
#ifdef SEAICE_SALINITY
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,HSALT,iRec,myIt,myThid)
#endif

      RETURN
      END
