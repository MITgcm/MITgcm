C $Header: /u/gcmpack/MITgcm/pkg/cd_code/cd_code_ini_vars.F,v 1.1 2003/10/30 12:00:41 edhill Exp $
C $Name:  $

#include "CD_CODE_OPTIONS.h"

      SUBROUTINE CD_CODE_INIT_VARS( myIter, myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CD_CODE_VARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_UVEL 
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      INTEGER prec
CEOP

#ifdef ALLOW_CD_CODE

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx

           uNM1(I,J,K,bi,bj)=0. _d 0
           vNM1(I,J,K,bi,bj)=0. _d 0
           uVeld(I,J,K,bi,bj)=0. _d 0
           vVeld(I,J,K,bi,bj)=0. _d 0

          ENDDO
         ENDDO
        ENDDO

C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx

          etaNm1(I,J,bi,bj)=0. _d 0

         ENDDO
        ENDDO

       ENDDO
      ENDDO

      IF ( startTime.NE.0. .OR. nIter0.NE.0 ) THEN

C--   Suffix for pickup files
         IF (pickupSuff.EQ.' ') THEN
            WRITE(suff,'(I10.10)') myIter
         ELSE
            WRITE(suff,'(A10)') pickupSuff
         ENDIF

         _BARRIER
         _BEGIN_MASTER( myThid )

         prec = precFloat64

         WRITE(fn,'(A,A10)') 'pickup_cd.',suff
         CALL MDSREADFIELD(fn,prec,'RL',Nr,uVelD,    1,myThid)
         CALL MDSREADFIELD(fn,prec,'RL',Nr,vVelD,    2,myThid)
         CALL MDSREADFIELD(fn,prec,'RL',Nr,uNM1,     3,myThid)
         CALL MDSREADFIELD(fn,prec,'RL',Nr,vNM1,     4,myThid)
         CALL MDSREADFIELD(fn,prec,'RL', 1,etaNm1,6*Nr+1,myThid)
         
         _EXCH_XYZ_R8( uVelD,    myThid )
         _EXCH_XYZ_R8( vVelD,    myThid )
         CALL EXCH_UV_XYZ_RL(uNM1,vNM1,.TRUE.,myThid)
         _EXCH_XY_R8( etaNm1, myThid )

         _END_MASTER( myThid )
         _BARRIER
       
      ENDIF

#endif /*  ALLOW_CD_CODE  */

      RETURN
      END
