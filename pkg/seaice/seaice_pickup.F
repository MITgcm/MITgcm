C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/seaice_pickup.F,v 1.19 2007/10/19 03:21:39 jmc Exp $
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

       _BARRIER

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

CBOP
C     !ROUTINE: SEAICE_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE SEAICE_WRITE_PICKUP ( permPickup, suff,
     I                                 myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE SEAICE_WRITE_PICKUP                           |
C     | o Write sea ice pickup file for restarting.              |
C     *==========================================================*
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     permPickup :: write a permanent pickup
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     == Local variables ==
C     prec    :: Precision of pickup file ( precFloat64 )
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER iRec
      INTEGER prec
      LOGICAL lgf

C--    Write model fields
       prec = precFloat64
       WRITE(fn,'(A,A)') 'pickup_seaice.',suff

       iRec = 1
#ifdef SEAICE_MULTICATEGORY
       CALL WRITE_REC_3D_RL(fn,prec,MULTDIM, TICES, iRec,myIter,myThid)
       iRec = iRec + MULTDIM
#else
       CALL WRITE_REC_3D_RL( fn, prec, 1, TICE , iRec, myIter, myThid )
       iRec = iRec + 1
#endif /* SEAICE_MULTICATEGORY */
       CALL WRITE_REC_3D_RL( fn, prec, 1, YNEG , iRec, myIter, myThid )
       iRec = iRec + 1
       CALL WRITE_REC_3D_RL( fn, prec, 1, HSNOW, iRec, myIter, myThid )
       iRec = int((iRec+3)/3)
       CALL WRITE_REC_3D_RL( fn, prec, 3, UICE , iRec, myIter, myThid )
       iRec = iRec + 1
       CALL WRITE_REC_3D_RL( fn, prec, 3, VICE , iRec, myIter, myThid )
       iRec = iRec + 1
       CALL WRITE_REC_3D_RL( fn, prec, 3, HEFF , iRec, myIter, myThid )
       iRec = iRec + 1
       CALL WRITE_REC_3D_RL( fn, prec, 3, AREA , iRec, myIter, myThid )
       iRec = iRec * 3 + 1
#if (defined(SEAICE_CGRID) && defined(SEAICE_ALLOW_EVP))
       IF ( SEAICEuseEVP ) THEN
          CALL WRITE_REC_3D_RL( fn, prec, 1,
     &         seaice_sigma1, iRec, myIter, myThid )
          iRec = iRec + 1
          CALL WRITE_REC_3D_RL( fn, prec, 1,
     &         seaice_sigma2, iRec, myIter, myThid )
          iRec = iRec + 1
          CALL WRITE_REC_3D_RL( fn, prec, 1,
     &         seaice_sigma12, iRec, myIter, myThid )
          iRec = iRec + 1
       ENDIF
#endif /* SEAICE_ALLOW_EVP */
#ifdef SEAICE_SALINITY
       CALL WRITE_REC_3D_RL( fn, prec, 1, HSALT, iRec, myIter, myThid )
#endif

      RETURN
      END
