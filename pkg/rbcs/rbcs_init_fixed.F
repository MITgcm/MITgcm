#include "RBCS_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE RBCS_INIT_FIXED( myThid )

C !DESCRIPTION:
C calls subroutines that initializes fixed variables for relaxed
c boundary conditions

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
#endif
#include "RBCS_SIZE.h"
#include "RBCS_PARAMS.h"
#include "RBCS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_RBCS
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     i,j,k,bi,bj,irbc  :: loop indices
C     msgBuf      :: Informational/error message buffer
      INTEGER i,j,k,bi,bj
      INTEGER irbc, iLen
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(12) filName
#ifdef ALLOW_PTRACERS
      INTEGER iTr
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Report RBCS mask setting

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(2A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') ' ---  RBCS_INIT_FIXED:',
     &                     ' setting RBCS mask  ---'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      IF ( useRBCtemp ) THEN
        irbc = MIN(maskLEN,1)
        IF ( relaxMaskTrFile(irbc).EQ.' ' ) THEN
         WRITE(msgBuf,'(2A,I3,2A)') '** WARNING ** RBCS_INIT_FIXED:',
     &     ' relaxMaskFile(irbc=', irbc, ') unset ==> No relaxation',
     &     ' for Temp'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A,I3,2A)') 'Warning:',
     &     ' relaxMaskFile(irbc=', irbc, ') unset ==> No relaxation',
     &     ' for Temp'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ELSE
         iLen = ILNBLNK(relaxMaskTrFile(irbc))
         WRITE(msgBuf,'(A,I3,3A)') 'Use relaxMaskFile(irbc=', irbc,
     &     ') = "', relaxMaskTrFile(irbc)(1:iLen), '"'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(A,1PE21.13)')
     &     ' for Temp  relaxation with tauRelaxT =', tauRelaxT
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF
      IF ( useRBCsalt ) THEN
        irbc = MIN(maskLEN,2)
        IF ( relaxMaskTrFile(irbc).EQ.' ' ) THEN
         WRITE(msgBuf,'(2A,I3,2A)') '** WARNING ** RBCS_INIT_FIXED:',
     &     ' relaxMaskFile(irbc=', irbc, ') unset ==> No relaxation',
     &     ' for Salt'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A,I3,2A)') 'Warning:',
     &     ' relaxMaskFile(irbc=', irbc, ') unset ==> No relaxation',
     &     ' for Salt'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ELSE
         iLen = ILNBLNK(relaxMaskTrFile(irbc))
         WRITE(msgBuf,'(A,I3,3A)') 'Use relaxMaskFile(irbc=', irbc,
     &     ') = "', relaxMaskTrFile(irbc)(1:iLen), '"'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(A,1PE21.13)')
     &     ' for Salt  relaxation with tauRelaxS =', tauRelaxS
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
      ENDIF
      IF ( useRBCuVel ) THEN
        IF ( relaxMaskUFile.EQ. ' ' ) THEN
         WRITE(msgBuf,'(2A)') '** WARNING ** RBCS_INIT_FIXED:',
     &     ' relaxMaskUFile unset ==> use Temp mask instead'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A)') 'Warning:',
     &     ' relaxMaskUFile unset ==> use Temp mask instead'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ELSE
         iLen = ILNBLNK(relaxMaskUFile)
         WRITE(msgBuf,'(A,3A)') 'Use relaxMaskUFile',
     &     ' = "', relaxMaskUFile(1:iLen), '"'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
         WRITE(msgBuf,'(A,1PE21.13)')
     &     ' for U-Vel relaxation with tauRelaxU =', tauRelaxU
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( useRBCvVel ) THEN
        IF ( relaxMaskVFile.EQ. ' ' ) THEN
         WRITE(msgBuf,'(2A)') '** WARNING ** RBCS_INIT_FIXED:',
     &     ' relaxMaskVFile unset ==> use Temp mask instead'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A)') 'Warning:',
     &     ' relaxMaskVFile unset ==> use Temp mask instead'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ELSE
         iLen = ILNBLNK(relaxMaskVFile)
         WRITE(msgBuf,'(A,3A)') 'Use relaxMaskVFile',
     &     ' = "', relaxMaskVFile(1:iLen), '"'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
         WRITE(msgBuf,'(A,1PE21.13)')
     &     ' for V-Vel relaxation with tauRelaxV =', tauRelaxV
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( useRBCeta ) THEN
        IF ( relaxMaskEtaFile.EQ. ' ' ) THEN
         WRITE(msgBuf,'(2A)') '** WARNING ** RBCS_INIT_FIXED:',
     &     ' relaxMaskEtaFile unset ==> use Temp mask instead'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2A)') 'Warning:',
     &     ' relaxMaskEtaFile unset ==> use Temp mask instead'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ELSE
         iLen = ILNBLNK(relaxMaskEtaFile)
         WRITE(msgBuf,'(A,3A)') 'Use relaxMaskEtaFile',
     &     ' = "', relaxMaskEtaFile(1:iLen), '"'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
         WRITE(msgBuf,'(A,1PE21.13)')
     &     ' for Eta relaxation with tauRelaxEta =', tauRelaxEta
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
      ENDIF
#ifdef ALLOW_PTRACERS
      IF ( usePTRACERS .AND. PTRACERS_numInUse.GE.1 ) THEN
       DO iTr=1,PTRACERS_numInUse
        IF ( useRBCpTrNum(iTr) ) THEN
         irbc = MIN(maskLEN,2+iTr)
         IF ( relaxMaskTrFile(irbc).EQ.' ' ) THEN
          WRITE(msgBuf,'(2A,I3,2A,I3)')
     &     '** WARNING ** RBCS_INIT_FIXED:',
     &     ' relaxMaskFile(irbc=', irbc, ') unset ==> No relaxation',
     &     ' for pTr=', iTr
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(2A,I3,2A,I3)') 'Warning:',
     &     ' relaxMaskFile(irbc=', irbc, ') unset ==> No relaxation',
     &     ' for pTr=', iTr
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
         ELSE
          iLen = ILNBLNK(relaxMaskTrFile(irbc))
          WRITE(msgBuf,'(A,I3,3A)') 'Use relaxMaskFile(irbc=', irbc,
     &     ') = "', relaxMaskTrFile(irbc)(1:iLen), '"'
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A,I3,A,1PE21.13)')
     &     ' for pTr=', iTr, ' relaxation, tauRelaxPTR =',
     &                          tauRelaxPTR(iTr)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
         ENDIF
        ENDIF
       ENDDO
      ENDIF
#endif /* ALLOW_PTRACERS */

      _END_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifndef DISABLE_RBCS_MOM
C     Loop over tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             RBC_maskU(i,j,k,bi,bj) = 0. _d 0
             RBC_maskV(i,j,k,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
       ENDDO
      ENDDO
#endif /* DISABLE_RBCS_MOM */

C     Loop over mask index
      DO irbc=1,maskLEN

C     Loop over tiles
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)

C        Initialize arrays in common blocks :
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               RBC_mask(i,j,k,bi,bj,irbc) = 0. _d 0
             ENDDO
            ENDDO
           ENDDO

C        end bi,bj loops
         ENDDO
        ENDDO
C     end of mask index loop
      ENDDO

C read in mask for relaxing
      DO irbc=1,maskLEN
       IF ( relaxMaskTrFile(irbc).NE. ' ' ) THEN
         CALL READ_FLD_XYZ_RS( relaxMaskTrFile(irbc), ' ',
     &                RBC_mask(1-OLx,1-OLy,1,1,1,irbc), 0, myThid )
         CALL EXCH_XYZ_RS( RBC_mask(1-OLx,1-OLy,1,1,1,irbc), myThid )
C--   Apply mask:
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               RBC_mask(i,j,k,bi,bj,irbc) = RBC_mask(i,j,k,bi,bj,irbc)
     &                                    * maskC(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         IF ( debugLevel.GE.debLevC ) THEN
           WRITE(filName,'(A,I3.3)') 'RBC_mask_',irbc
           CALL WRITE_FLD_XYZ_RS( filName,' ',
     &                RBC_mask(1-OLx,1-OLy,1,1,1,irbc), 0, myThid )
         ENDIF
       ENDIF
      ENDDO

C--
      IF ( relaxMaskEtaFile.NE. ' ' ) THEN
        CALL READ_FLD_XY_RS( relaxMaskEtaFile, ' ',
     &                 RBC_Etamask(1-OLx,1-OLy,1,1), 0, myThid )
        CALL EXCH_XY_RS( RBC_Etamask(1-OLx,1-OLy,1,1), myThid )
C--   Apply mask:
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
             DO j=1-OLy,sNy+OLy
              DO i=1-OLx,sNx+OLx
                RBC_Etamask(i,j,bi,bj) = RBC_Etamask(i,j,bi,bj)
     &                                       * maskC(i,j,1,bi,bj)
              ENDDO
             ENDDO
          ENDDO
        ENDDO
      ENDIF


#ifndef DISABLE_RBCS_MOM
      IF ( useRBCuVel .AND. relaxMaskUFile.NE. ' ' ) THEN
        CALL READ_FLD_XYZ_RS(relaxMaskUFile,' ',RBC_maskU, 0, myThid)
      ELSEIF( useRBCuVel ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             DO i=2-OLx,sNx+OLx
               RBC_maskU(i,j,k,bi,bj) =
     &                  ( RBC_mask(i-1,j,k,bi,bj,1)
     &                  + RBC_mask( i ,j,k,bi,bj,1) )*0.5 _d 0
             ENDDO
            ENDDO
           ENDDO
         ENDDO
        ENDDO
      ENDIF
      IF ( useRBCvVel .AND. relaxMaskVFile.NE. ' ' ) THEN
        CALL READ_FLD_XYZ_RS(relaxMaskVFile,' ',RBC_maskV, 0, myThid)
      ELSEIF( useRBCvVel ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j=2-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               RBC_maskV(i,j,k,bi,bj) =
     &                  ( RBC_mask(i,j-1,k,bi,bj,1)
     &                  + RBC_mask(i, j ,k,bi,bj,1) )*0.5 _d 0
             ENDDO
            ENDDO
           ENDDO
         ENDDO
        ENDDO
      ENDIF
      IF( useRBCuVel .OR. useRBCvVel ) THEN
         CALL EXCH_UV_XYZ_RS( RBC_maskU, RBC_maskV, .FALSE., myThid )
C--   Apply mask:
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               RBC_maskU(i,j,k,bi,bj) = RBC_maskU(i,j,k,bi,bj)
     &                                * maskW(i,j,k,bi,bj)
               RBC_maskV(i,j,k,bi,bj) = RBC_maskV(i,j,k,bi,bj)
     &                                * maskS(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         IF ( debugLevel.GE.debLevC ) THEN
           CALL WRITE_FLD_XYZ_RS('RBC_maskU',' ',RBC_maskU,0,myThid )
           CALL WRITE_FLD_XYZ_RS('RBC_maskV',' ',RBC_maskV,0,myThid )
         ENDIF
      ENDIF
#endif /* DISABLE_RBCS_MOM */

      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(2A)') ' ---  RBCS_INIT_FIXED:',
     &                     ' setting RBCS mask done'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      _END_MASTER(myThid)

#endif /* ALLOW_RBCS */

      RETURN
      END
