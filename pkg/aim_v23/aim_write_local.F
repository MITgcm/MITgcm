C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_write_local.F,v 1.1 2002/11/22 17:17:03 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_WRITE_LOCAL
C     !INTERFACE:
      SUBROUTINE AIM_WRITE_LOCAL(
     I          pref,suff,nNr,field,bi,bj,iRec,myIter,myThid)
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
C     pref   = Prefix of the output file name
C     suff   = Suffix of the output file name
C     Nr     = 3rd dim. of the input field
C     field  = Field (from aim-physics) to write
C     bi,bj  = Tile index
C     iRec   = reccord number in the output file
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine
      CHARACTER*(*) pref,suff
      INTEGER nNr
      _RL field(sNx,sNy,nNr)
      INTEGER bi, bj, iRec, myIter, myThid

#ifdef ALLOW_AIM

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER i,j,k,Katm

CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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
      ELSEIF (nNr.LT.Nr) THEN
       DO k=1,nNr
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j,k) = field(i,j,k)
         ENDDO
        ENDDO
       ENDDO
      ELSE
        WRITE(msgBuf,'(A,I4,A,I4)')
     & 'AIM_WRITE_LOCAL: 3rd dim.(field)=',nNr,' has to be <',Nr
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R AIM_WRITE_LOCAL'
      ENDIF

C-    Write to file:
      CALL WRITE_LOCAL_RL(pref,suff,nNr,tmpFld,
     &                    bi,bj,iRec,myIter,myThid)

#endif /* ALLOW_AIM */
      RETURN
      END
