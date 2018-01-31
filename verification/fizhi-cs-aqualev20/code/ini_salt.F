#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_SALT
C     !INTERFACE:
      SUBROUTINE INI_SALT ( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_SALT                                       
C     | o Set model initial salinity field.                       
C     *==========================================================*
C     | There are several options for setting the initial         
C     | temperature file                                          
C     |  1. Inline code                                           
C     |  2. Vertical profile ( uniform S in X and Y )             
C     |  3. Three-dimensional data from a file. For example from  
C     |     Levitus or from a checkpoint file from a previous     
C     |     integration.                                          
C     | In addition to setting the salinity field we also         
C     | set the initial salinity tendency term here.              
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_SALT 
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K, localWarnings
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

       _RL pedyn(Nr+1), pdyn(Nr), pkappa(Nr)
       integer Lbotij
       _RL getcon, kappa, dum, pinmb
       _RL temperature(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
       _RL rhum(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
       _RL qstar(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)

C Need to convert from input RH to q -- Need pressure, temperature
C Build pressures - we know there is no topography and eta = 0
       pedyn(1) = 100000.
       do K = 2,Nr+1
        pedyn(K) = pedyn(K-1) - drF(K-1)
       enddo
       do K = 1,Nr
        pdyn(K)=(pedyn(K+1)+pedyn(K))/2.
       enddo
       kappa = getcon('KAPPA')
       do K = 1,Nr
        pkappa(K)=(pdyn(K)/100000.)**kappa
       enddo
C Now convert from theta to Temperature
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           temperature(I,J,K,bi,bj) = theta(I,J,K,bi,bj) * pkappa(K)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Initialise salinity field to the vertical reference profile
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           salt(I,J,K,bi,bj) = sRef(K)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( hydrogSaltFile .NE. ' ' ) THEN
       _BEGIN_MASTER( myThid )
       CALL READ_FLD_XYZ_RL( hydrogSaltFile, ' ', rhum, 0, myThid )
       _END_MASTER(myThid)

C Now convert from rh to q
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr-1
         DO J=1,sNy
          DO I=1,sNx
           pinmb = pdyn(K)/100.
           call qsat(temperature(i,j,k,bi,bj),pinmb,qstar(i,j,k,bi,bj),
     .               dum,.false.)
           salt(I,J,K,bi,bj) = rhum(i,j,k,bi,bj) * qstar(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

       _EXCH_XYZ_RL(salt  , myThid )

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
