C $Header: 
C $Name: 

#include "OBCS_OPTIONS.h"

CStartofinterface
      SUBROUTINE OBCS_READ_CHECKPOINT( prec, myIt, suff, myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_READ_CHECKPOINT                          |
C     | o Read open boundary checkpoint arrays                   |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"

C     == Routine arguments ==
      INTEGER prec
      INTEGER myIt
      CHARACTER*(10) suff
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_OBCS

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn

C     READ N and S OB arrays
#ifdef ALLOW_OBCS_NORTH
      WRITE(fn,'(A,A10)') 'pickup_obN.',suff
      CALL MDSREADFIELDXZ(fn,prec,'RS',Nr,OBNu,1,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RS',Nr,OBNv,2,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RS',Nr,OBNt,3,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RS',Nr,OBNs,4,myThid)
# ifdef ALLOW_NONHYDROSTATIC
      CALL MDSREADFIELDXZ(fn,prec,'RS',Nr,OBNw,5,myThid)
# endif
#endif
#ifdef ALLOW_OBCS_SOUTH
      WRITE(fn,'(A,A10)') 'pickup_obS.',suff
      CALL MDSREADFIELDXZ(fn,prec,'RS',Nr,OBSu,1,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RS',Nr,OBSv,2,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RS',Nr,OBSt,3,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RS',Nr,OBSs,4,myThid)
# ifdef ALLOW_NONHYDROSTATIC
      CALL MDSREADFIELDXZ(fn,prec,'RS',Nr,OBSw,5,myThid)
# endif
#endif

C     READ E and W OB arrays
#ifdef ALLOW_OBCS_EAST
      WRITE(fn,'(A,A10)') 'pickup_obE.',suff
      CALL MDSREADFIELDYZ(fn,prec,'RS',Nr,OBEu,1,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RS',Nr,OBEv,2,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RS',Nr,OBEt,3,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RS',Nr,OBEs,4,myThid) 
# ifdef ALLOW_NONHYDROSTATIC
      CALL MDSREADFIELDYZ(fn,prec,'RS',Nr,OBEw,5,myThid)
# endif
#endif
      
#ifdef ALLOW_OBCS_WEST
      WRITE(fn,'(A,A10)') 'pickup_obW.',suff
      CALL MDSREADFIELDYZ(fn,prec,'RS',Nr,OBWu,1,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RS',Nr,OBWv,2,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RS',Nr,OBWt,3,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RS',Nr,OBWs,4,myThid)
# ifdef ALLOW_NONHYDROSTATIC
      CALL MDSREADFIELDYZ(fn,prec,'RS',Nr,OBWw,5,myThid)
# endif
#endif

C     Fill in edge regions
c      _EXCH_XZ_R4(OBNu , myThid)
c      _EXCH_XZ_R4(OBNv , myThid)
c      _EXCH_XZ_R4(OBNt , myThid)
c      _EXCH_XZ_R4(OBNs , myThid)
c      _EXCH_XZ_R4(OBSu , myThid)
c      _EXCH_XZ_R4(OBSv , myThid)
c      _EXCH_XZ_R4(OBSt , myThid)
c      _EXCH_XZ_R4(OBSs , myThid)
c#ifdef ALLOW_NONHYDROSTATIC
c      _EXCH_XZ_R4(OBNw , myThid)
c      _EXCH_XZ_R4(OBSw , myThid)
c#endif

C     Fill in edge regions
c      _EXCH_YZ_R4(OBEu , myThid)
c      _EXCH_YZ_R4(OBEv , myThid)
c      _EXCH_YZ_R4(OBEt , myThid)
c      _EXCH_YZ_R4(OBEs , myThid) 
c      _EXCH_YZ_R4(OBWu , myThid)
c      _EXCH_YZ_R4(OBWv , myThid)
c      _EXCH_YZ_R4(OBWt , myThid)
c      _EXCH_YZ_R4(OBWs , myThid)
c#ifdef ALLOW_NONHYDROSTATIC
c      _EXCH_YZ_R4(OBEw , myThid)
c      _EXCH_YZ_R4(OBWw , myThid)
c#endif

#ifdef ALLOW_ORLANSKI
      IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.useOrlanskiWest) THEN
        CALL ORLANSKI_READ_CHECKPOINT( 
     &          prec, myIt, suff, myThid ) 
      ENDIF
#endif  /* ALLOW_ORLANSKI */

#endif  /* ALLOW_OBCS */
      RETURN
      END


CStartofinterface
      SUBROUTINE OBCS_WRITE_CHECKPOINT( prec, lgf, permCheckPoint, 
     &                                  myIt, myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_WRITE_CHECKPOINT                         |
C     | o Write open boundary checkpoint arrays                  |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"

C     == Routine arguments ==
      INTEGER prec
      LOGICAL lgf
      LOGICAL permCheckPoint
      INTEGER myIt
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_OBCS

C     == Local variables ==      
      CHARACTER*(MAX_LEN_FNAM) fn

C     Write N and S OB arrays
#ifdef ALLOW_OBCS_NORTH
      IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_obN.',myIt
      ELSE
         WRITE(fn,'(A,A)') 'pickup_obN.',checkPtSuff(nCheckLev)
      ENDIF
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RS',Nr,OBNu,1,myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RS',Nr,OBNv,2,myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RS',Nr,OBNt,3,myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RS',Nr,OBNs,4,myIt,myThid)
# ifdef ALLOW_NONHYDROSTATIC
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RS',Nr,OBNw,5,myIt,myThid)
# endif
#endif

#ifdef ALLOW_OBCS_SOUTH
      IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_obS.',myIt
      ELSE
         WRITE(fn,'(A,A)') 'pickup_obS.',checkPtSuff(nCheckLev)
      ENDIF
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RS',Nr,OBSu,1,myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RS',Nr,OBSv,2,myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RS',Nr,OBSt,3,myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RS',Nr,OBSs,4,myIt,myThid)
# ifdef ALLOW_NONHYDROSTATIC
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RS',Nr,OBSw,5,myIt,myThid)
# endif
#endif

C     Write E and W OB arrays
#ifdef ALLOW_OBCS_EAST
      IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_obE.',myIt
      ELSE
         WRITE(fn,'(A,A)') 'pickup_obE.',checkPtSuff(nCheckLev)
      ENDIF
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RS',Nr,OBEu,1,myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RS',Nr,OBEv,2,myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RS',Nr,OBEt,3,myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RS',Nr,OBEs,4,myIt,myThid) 
# ifdef ALLOW_NONHYDROSTATIC
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RS',Nr,OBEw,5,myIt,myThid)
# endif
#endif

#ifdef ALLOW_OBCS_WEST
      IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_obW.',myIt
      ELSE
         WRITE(fn,'(A,A)') 'pickup_obW.',checkPtSuff(nCheckLev)
      ENDIF
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RS',Nr,OBWu,1,myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RS',Nr,OBWv,2,myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RS',Nr,OBWt,3,myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RS',Nr,OBWs,4,myIt,myThid)
# ifdef ALLOW_NONHYDROSTATIC
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RS',Nr,OBWw,5,myIt,myThid)
# endif
#endif


#ifdef ALLOW_ORLANSKI
      IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.useOrlanskiWest) THEN
        CALL ORLANSKI_WRITE_CHECKPOINT( 
     &          prec, lgf, permCheckPoint, myIt, myThid ) 
      ENDIF
#endif  /* ALLOW_ORLANSKI */

#endif  /* ALLOW_OBCS */
      RETURN
      END

