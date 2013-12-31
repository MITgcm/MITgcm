C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_init_fixed.F,v 1.8 2013/12/31 22:28:39 jmc Exp $
C $Name:  $

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
#include "PTRACERS_SIZE.h"
#endif
#include "RBCS_SIZE.h"
#include "RBCS_PARAMS.h"
#include "RBCS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_RBCS
C     !LOCAL VARIABLES:
C     i,j,k,bi,bj,irbc  :: loop indices
C     msgBuf      :: Informational/error message buffer
      INTEGER i,j,k,bi,bj
      INTEGER irbc
#ifndef DISABLE_RBCS_MOM
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
      CHARACTER*(12) filName

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifndef DISABLE_RBCS_MOM
C     Loop over tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1-Oly,sNy+OLy
           DO i=1-Olx,sNx+Olx
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
            DO j=1-Oly,sNy+OLy
             DO i=1-Olx,sNx+Olx
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
       IF ( relaxMaskFile(irbc).NE. ' ' ) THEN
         CALL READ_FLD_XYZ_RS(relaxMaskFile(irbc),' ',
     &                RBC_mask(1-Olx,1-Oly,1,1,1,irbc), 0, myThid)
         CALL EXCH_XYZ_RS( RBC_mask(1-Olx,1-Oly,1,1,1,irbc), myThid )
C--   Apply mask:
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j=1-Oly,sNy+OLy
             DO i=1-Olx,sNx+Olx
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
     &                RBC_mask(1-Olx,1-Oly,1,1,1,irbc), 0, myThid )
         ENDIF
       ENDIF
      ENDDO

#ifndef DISABLE_RBCS_MOM
      IF ( useRBCuVel .AND. relaxMaskUFile.NE. ' ' ) THEN
        CALL READ_FLD_XYZ_RS(relaxMaskUFile,' ',RBC_maskU, 0, myThid)
      ELSEIF( useRBCuVel ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** RBCS_INIT_FIXED: ',
     &     'no relaxMaskUFile => use Temp mask instead'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j=1-Oly,sNy+OLy
             DO i=2-Olx,sNx+Olx
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
        WRITE(msgBuf,'(2A)') '** WARNING ** RBCS_INIT_FIXED: ',
     &     'no relaxMaskVFile => use Temp mask instead'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j=2-Oly,sNy+OLy
             DO i=1-Olx,sNx+Olx
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
            DO j=1-Oly,sNy+OLy
             DO i=1-Olx,sNx+Olx
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

#endif /* ALLOW_RBCS */

      RETURN
      END
