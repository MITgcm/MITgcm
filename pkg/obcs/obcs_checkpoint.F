C $Header: /u/gcmpack/MITgcm/pkg/obcs/Attic/obcs_checkpoint.F,v 1.12 2009/10/01 21:09:10 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"
#undef OBCS_NEEDS_PICKUP

CStartofinterface
      SUBROUTINE OBCS_READ_PICKUP( myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE OBCS_READ_PICKUP
C     | o Read open boundary pickup file
C     *==========================================================*
C     |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"

C     == Routine arguments ==
      INTEGER myIter
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_OBCS
#ifdef OBCS_NEEDS_PICKUP

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      INTEGER prec
      INTEGER iRec

#ifndef ALLOW_MDSIO
      STOP 'ABNORMAL END: S/R OBCS_READ_CHECKPOINT: NEEDS MSDIO PKG'
#endif /* ALLOW_MDSIO */

      IF ( pickupSuff.EQ.' ' ) THEN
        WRITE(suff,'(I10.10)') myIter
      ELSE
        suff = pickupSuff
      ENDIF
      prec = precFloat64

C     READ N and S OB arrays
#ifdef ALLOW_OBCS_NORTH
      WRITE(fn,'(2A)') 'pickup_obN.',suff
      CALL READ_REC_XZ_RL(fn,prec,Nr,OBNu,1,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,OBNv,2,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,OBNt,3,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,OBNs,4,myIter,myThid)
      iRec = 4
# ifdef ALLOW_NONHYDROSTATIC
      CALL READ_REC_XZ_RL(fn,prec,Nr,OBNw,5,myIter,myThid)
      iRec = 5
# endif
# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       iRec = iRec*Nr
       CALL READ_REC_XZ_RL(fn,prec, 1,OBNa,   iRec+1,myIter,myThid)
       CALL READ_REC_XZ_RL(fn,prec, 1,OBNh,   iRec+2,myIter,myThid)
       CALL READ_REC_XZ_RL(fn,prec, 1,OBNsl,  iRec+3,myIter,myThid)
       CALL READ_REC_XZ_RL(fn,prec, 1,OBNsn,  iRec+4,myIter,myThid)
       CALL READ_REC_XZ_RL(fn,prec, 1,OBNuice,iRec+5,myIter,myThid)
       CALL READ_REC_XZ_RL(fn,prec, 1,OBNvice,iRec+6,myIter,myThid)
      ENDIF
# endif
#endif

#ifdef ALLOW_OBCS_SOUTH
      WRITE(fn,'(2A)') 'pickup_obS.',suff
      CALL READ_REC_XZ_RL(fn,prec,Nr,OBSu,1,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,OBSv,2,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,OBSt,3,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,OBSs,4,myIter,myThid)
      iRec = 4
# ifdef ALLOW_NONHYDROSTATIC
      CALL READ_REC_XZ_RL(fn,prec,Nr,OBSw,5,myIter,myThid)
      iRec = 5
# endif
# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       iRec = iRec*Nr
       CALL READ_REC_XZ_RL(fn,prec, 1,OBSa,   iRec+1,myIter,myThid)
       CALL READ_REC_XZ_RL(fn,prec, 1,OBSh,   iRec+2,myIter,myThid)
       CALL READ_REC_XZ_RL(fn,prec, 1,OBSsl,  iRec+3,myIter,myThid)
       CALL READ_REC_XZ_RL(fn,prec, 1,OBSsn,  iRec+4,myIter,myThid)
       CALL READ_REC_XZ_RL(fn,prec, 1,OBSuice,iRec+5,myIter,myThid)
       CALL READ_REC_XZ_RL(fn,prec, 1,OBSvice,iRec+6,myIter,myThid)
      ENDIF
# endif
#endif

C     READ E and W OB arrays
#ifdef ALLOW_OBCS_EAST
      WRITE(fn,'(2A)') 'pickup_obE.',suff
      CALL READ_REC_YZ_RL(fn,prec,Nr,OBEu,1,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,OBEv,2,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,OBEt,3,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,OBEs,4,myIter,myThid)
      iRec = 4
# ifdef ALLOW_NONHYDROSTATIC
      CALL READ_REC_YZ_RL(fn,prec,Nr,OBEw,5,myIter,myThid)
      iRec = 5
# endif
# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       iRec = iRec*Nr
       CALL READ_REC_YZ_RL(fn,prec, 1,OBEa,   iRec+1,myIter,myThid)
       CALL READ_REC_YZ_RL(fn,prec, 1,OBEh,   iRec+2,myIter,myThid)
       CALL READ_REC_YZ_RL(fn,prec, 1,OBEsl,  iRec+3,myIter,myThid)
       CALL READ_REC_YZ_RL(fn,prec, 1,OBEsn,  iRec+4,myIter,myThid)
       CALL READ_REC_YZ_RL(fn,prec, 1,OBEuice,iRec+5,myIter,myThid)
       CALL READ_REC_YZ_RL(fn,prec, 1,OBEvice,iRec+6,myIter,myThid)
      ENDIF
# endif
#endif

#ifdef ALLOW_OBCS_WEST
      WRITE(fn,'(2A)') 'pickup_obW.',suff
      CALL READ_REC_YZ_RL(fn,prec,Nr,OBWu,1,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,OBWv,2,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,OBWt,3,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,OBWs,4,myIter,myThid)
      iRec = 4
# ifdef ALLOW_NONHYDROSTATIC
      CALL READ_REC_YZ_RL(fn,prec,Nr,OBWw,5,myIter,myThid)
      iRec = 5
# endif
# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       iRec = iRec*Nr
       CALL READ_REC_YZ_RL(fn,prec, 1,OBWa,   iRec+1,myIter,myThid)
       CALL READ_REC_YZ_RL(fn,prec, 1,OBWh,   iRec+2,myIter,myThid)
       CALL READ_REC_YZ_RL(fn,prec, 1,OBWsl,  iRec+3,myIter,myThid)
       CALL READ_REC_YZ_RL(fn,prec, 1,OBWsn,  iRec+4,myIter,myThid)
       CALL READ_REC_YZ_RL(fn,prec, 1,OBWuice,iRec+5,myIter,myThid)
       CALL READ_REC_YZ_RL(fn,prec, 1,OBWvice,iRec+6,myIter,myThid)
      ENDIF
# endif
#endif

C     Fill in edge regions
c      _EXCH_XZ_RL(OBNu , myThid)
c      _EXCH_XZ_RL(OBNv , myThid)
c      _EXCH_XZ_RL(OBNt , myThid)
c      _EXCH_XZ_RL(OBNs , myThid)
c      _EXCH_XZ_RL(OBSu , myThid)
c      _EXCH_XZ_RL(OBSv , myThid)
c      _EXCH_XZ_RL(OBSt , myThid)
c      _EXCH_XZ_RL(OBSs , myThid)
c#ifdef ALLOW_NONHYDROSTATIC
c      _EXCH_XZ_RL(OBNw , myThid)
c      _EXCH_XZ_RL(OBSw , myThid)
c#endif

C     Fill in edge regions
c      _EXCH_YZ_RL(OBEu , myThid)
c      _EXCH_YZ_RL(OBEv , myThid)
c      _EXCH_YZ_RL(OBEt , myThid)
c      _EXCH_YZ_RL(OBEs , myThid)
c      _EXCH_YZ_RL(OBWu , myThid)
c      _EXCH_YZ_RL(OBWv , myThid)
c      _EXCH_YZ_RL(OBWt , myThid)
c      _EXCH_YZ_RL(OBWs , myThid)
c#ifdef ALLOW_NONHYDROSTATIC
c      _EXCH_YZ_RL(OBEw , myThid)
c      _EXCH_YZ_RL(OBWw , myThid)
c#endif
#endif /* OBCS_NEEDS_PICKUP */

#ifdef ALLOW_ORLANSKI
      IF ( useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &     useOrlanskiEast.OR.useOrlanskiWest ) THEN
        CALL ORLANSKI_READ_PICKUP(
     I                     myIter, myThid )
      ENDIF
#endif  /* ALLOW_ORLANSKI */

#endif  /* ALLOW_OBCS */
      RETURN
      END


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CStartofinterface
      SUBROUTINE OBCS_WRITE_PICKUP(
     I                      suff, myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE OBCS_WRITE_PICKUP
C     | o Write open boundary pickup file
C     *==========================================================*
C     |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"
#include "RESTART.h"

C     == Routine arguments ==
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_OBCS
#ifdef OBCS_NEEDS_PICKUP

C     === Functions ====
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
      INTEGER iRec
      INTEGER iL

#ifndef ALLOW_MDSIO
      STOP 'ABNORMAL END: S/R OBCS_READ_CHECKPOINT: NEEDS MSDIO PKG'
#endif /* ALLOW_MDSIO */

      iL = ILNBLNK(suff)
      iL = MAX(1,iL)
      prec = precFloat64

C     Write N and S OB arrays
#ifdef ALLOW_OBCS_NORTH
      WRITE(fn,'(2A)') 'pickup_obN.',suff(1:iL)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNu,1,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNv,2,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNt,3,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNs,4,myIter,myThid)
      iRec = 4
# ifdef ALLOW_NONHYDROSTATIC
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNw,5,myIter,myThid)
      iRec = 5
# endif
# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       iRec = iRec*Nr
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNa,   iRec+1,myIter,myThid)
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNh,   iRec+2,myIter,myThid)
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNsl,  iRec+3,myIter,myThid)
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNsn,  iRec+4,myIter,myThid)
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNuice,iRec+5,myIter,myThid)
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBNvice,iRec+6,myIter,myThid)
      ENDIF
# endif
#endif

#ifdef ALLOW_OBCS_SOUTH
      WRITE(fn,'(2A)') 'pickup_obS.',suff(1:iL)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSu,1,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSv,2,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSt,3,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSs,4,myIter,myThid)
      iRec = 4
# ifdef ALLOW_NONHYDROSTATIC
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSw,5,myIter,myThid)
      iRec = 5
# endif
# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       iRec = iRec*Nr
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSa,   iRec+1,myIter,myThid)
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSh,   iRec+2,myIter,myThid)
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSsl,  iRec+3,myIter,myThid)
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSsn,  iRec+4,myIter,myThid)
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSuice,iRec+5,myIter,myThid)
       CALL WRITE_REC_XZ_RL(fn,prec, 1,OBSvice,iRec+6,myIter,myThid)
      ENDIF
# endif
#endif

C     Write E and W OB arrays
#ifdef ALLOW_OBCS_EAST
      WRITE(fn,'(2A)') 'pickup_obE.',suff(1:iL)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEu,1,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEv,2,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEt,3,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEs,4,myIter,myThid)
      iRec = 4
# ifdef ALLOW_NONHYDROSTATIC
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEw,5,myIter,myThid)
      iRec = 5
# endif
# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       iRec = iRec*Nr
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEa,   iRec+1,myIter,myThid)
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEh,   iRec+2,myIter,myThid)
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEsl,  iRec+3,myIter,myThid)
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEsn,  iRec+4,myIter,myThid)
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEuice,iRec+5,myIter,myThid)
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBEvice,iRec+6,myIter,myThid)
      ENDIF
# endif
#endif

#ifdef ALLOW_OBCS_WEST
      WRITE(fn,'(2A)') 'pickup_obW.',suff(1:iL)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWu,1,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWv,2,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWt,3,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWs,4,myIter,myThid)
      iRec = 4
# ifdef ALLOW_NONHYDROSTATIC
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWw,5,myIter,myThid)
      iRec = 5
# endif
# ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
       iRec = iRec*Nr
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWa,   iRec+1,myIter,myThid)
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWh,   iRec+2,myIter,myThid)
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWsl,  iRec+3,myIter,myThid)
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWsn,  iRec+4,myIter,myThid)
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWuice,iRec+5,myIter,myThid)
       CALL WRITE_REC_YZ_RL(fn,prec, 1,OBWvice,iRec+6,myIter,myThid)
      ENDIF
# endif
#endif

#endif /* OBCS_NEEDS_PICKUP */

#ifdef ALLOW_ORLANSKI
      IF ( useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &     useOrlanskiEast.OR.useOrlanskiWest ) THEN
        CALL ORLANSKI_WRITE_PICKUP(
     I                      suff, myTime, myIter, myThid )
      ENDIF
#endif  /* ALLOW_ORLANSKI */

#endif  /* ALLOW_OBCS */
      RETURN
      END
