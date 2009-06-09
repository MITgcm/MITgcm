C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_write_local.F,v 1.2 2009/06/09 22:46:06 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_WRITE_LOCAL
C     !INTERFACE:
      SUBROUTINE AIM_WRITE_LOCAL(
     I               pref, suff, nNr, field,
     I               bi, bj, iRec, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE AIM_WRITE_LOCAL
C     | o Write local variable from AIM physics (=> no overlap)
C     |   and reverse K index.
C     *==========================================================*
C     !USES
      IMPLICIT NONE

C     == Global variables ===
#include "AIM_SIZE.h"

#include "EEPARAMS.h"
c #include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     pref   :: Prefix of the output file name
C     suff   :: Suffix of the output file name
C     nNr    :: 3rd dim. of the input field
C     field  :: Field (from aim-physics) to write
C     bi,bj  :: Tile index
C     iRec   :: reccord number in the output file
C     myIter :: Current iteration number in simulation
C     myThid :: my Thread Id number
      CHARACTER*(*) pref,suff
      INTEGER nNr
      _RL field(sNx,sNy,nNr)
      INTEGER bi, bj, iRec, myIter, myThid

#ifdef ALLOW_AIM

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER iL
      INTEGER i, j, k, Katm
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Check for argument list consistency
      IF ( nNr.LT.1 .OR. nNr.GT.Nr ) THEN
          iL = ILNBLNK( pref )
          WRITE(msgBuf,'(A,I10,A,2I5,A,I4,2A)')
     &      'AIM_WRITE_LOCAL (it=', myIter, ' bi,bj=', bi,bj,
     &      ' iRec=', iRec, ' ): try to write: ', pref(1:iL)
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I4,A,I4)')
     &      'AIM_WRITE_LOCAL: 3rd dim.(field)=',nNr,' has to be <',Nr
          CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL END: S/R AIM_WRITE_LOCAL'
      ENDIF

C-    Copy the input field into tempo. array:
      IF (nNr.EQ.Nr) THEN
C-     Reverse K index:
       DO k=1,Nr
        Katm = _KD2KA( k )
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j,k) = field(i,j,Katm)
         ENDDO
        ENDDO
       ENDDO
      ELSE
C-     Do simple copy
       DO k=1,nNr
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j,k) = field(i,j,k)
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C-    Write to file:
      CALL WRITE_LOCAL_RL( pref, suff, nNr, tmpFld,
     &                     bi, bj, iRec, myIter, myThid )

#endif /* ALLOW_AIM */
      RETURN
      END
