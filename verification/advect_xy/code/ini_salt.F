C $Header: /u/gcmpack/MITgcm/verification/advect_xy/code/ini_salt.F,v 1.1 2001/09/13 20:06:19 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE INI_SALT ( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_SALT                                      |
C     | o Set model initial salinity field.                      |
C     |==========================================================|
C     | There are several options for setting the initial        |
C     | temperature file                                         |
C     |  1. Inline code                                          |
C     |  2. Vertical profile ( uniform S in X and Y )            |
C     |  3. Three-dimensional data from a file. For example from |
C     |     Levitus or from a checkpoint file from a previous    |
C     |     integration.                                         |
C     | In addition to setting the salinity field we also        |
C     | set the initial salinity tendency term here.             |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_SALT 
      INTEGER myThid

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K, localWarnings
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL rD

C--   Initialise salinity field to the vertical reference profile
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
            rD=sqrt( (XC(i,j,bi,bj)-40.E3)**2
     &              +(YC(i,j,bi,bj)-40.E3)**2
     &              +(RC(k)+50.E3)**2 )
           salt(I,J,K,bi,bj) = sRef(K)
           IF (rD.LE.60.E3) salt(I,J,K,bi,bj) = sRef(K)+1.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( hydrogSaltFile .NE. ' ' ) THEN
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XYZ_RL( hydrogSaltFile, ' ', salt, 0, myThid )
       _END_MASTER(myThid)
       _EXCH_XYZ_R8(salt  , myThid )
      ENDIF

C     Apply mask and test consistancy
      localWarnings=0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           IF (hFacC(I,J,K,bi,bj).EQ.0) salt(I,J,K,bi,bj) = 0.
           IF (hFacC(I,J,K,bi,bj).NE.0.AND.salt(I,J,K,bi,bj).EQ.0.
     &      .AND. sRef(k).NE.0.) THEN
             localWarnings=localWarnings+1
            ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      IF (localWarnings.NE.0) THEN
       WRITE(msgBuf,'(A,A)')
     &  'S/R INI_SALT: salt = 0 identically. If this is intentional',
     &  'you will need to edit ini_salt.F to avoid this safety check'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_SALT'
      ENDIF

      CALL PLOT_FIELD_XYZRL( salt, 'Initial Salinity' , Nr, 1, myThid )

      RETURN
      END
