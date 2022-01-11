#include "SMOOTH_OPTIONS.h"

      SUBROUTINE SMOOTH_INIT3D( smoothOpNb, myThid )

C     *==========================================================*
C     | SUBROUTINE smooth_init3D
C     | o Routine that initializes one 3D smoothing/correlation operator
C     |   by computing/writing the corresponding diffusion operator
C     *==========================================================*

Cgf the choices of smooth3Dtype and smooth3Dsize need comments...
Cgf
Cgf smooth3DtypeH= 1) HORIZONTAL ALONG GRID AXIS
Cgf              2-3) GMREDI TYPES
Cgf                4) HORIZONTAL BUT WITH ROTATED AXIS
Cgf for now I focus on the simpler smooth3DtypeH=1 case

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SMOOTH.h"

      INTEGER smoothOpNb
      INTEGER myThid

      INTEGER i, j, k, bi, bj
      CHARACTER*(80) fnamegeneric
      _RL smooth3D_KzMax

      smooth3DtotTime = smooth3Dnbt(smoothOpNb)*smooth3DdelTime

C vertical smoothing:

      IF ( smooth3DsizeZ(smoothOpNb).EQ.3 ) THEN
        WRITE(fnamegeneric(1:80),'(1A,I3.3)')
     &        'smooth3DscalesZ', smoothOpNb
        CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &                       Nr, smooth3D_Lz, 1, 1, myThid )
        CALL EXCH_XYZ_RL( smooth3D_Lz, myThid )
      ELSE
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             smooth3D_Lz(i,j,k,bi,bj) = smooth3D_Lz0(smoothOpNb)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           smooth3D_kappaR(i,j,k,bi,bj) = smooth3D_Lz(i,j,k,bi,bj)
     &             *smooth3D_Lz(i,j,k,bi,bj)/smooth3DtotTime/2
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C avoid excessive vertical smoothing:
      IF ( smooth3DsizeZ(smoothOpNb).NE.3 ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             smooth3D_KzMax=drC(k)
             smooth3D_KzMax = smooth3D_KzMax*smooth3D_KzMax
     &                                      /smooth3DtotTime/2
             IF ( smooth3D_kappaR(i,j,k,bi,bj).GT.smooth3D_KzMax ) THEN
              smooth3D_kappaR(i,j,k,bi,bj) = smooth3D_KzMax
             ENDIF
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

      CALL EXCH_XYZ_RL( smooth3D_kappaR, myThid )

C horizontal smoothing:

      IF ( smooth3DsizeH(smoothOpNb).EQ.3 ) THEN
        WRITE(fnamegeneric(1:80),'(1A,I3.3)')
     &        'smooth3DscalesH', smoothOpNb
        CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &                       Nr, smooth3D_Lx, 1, 1, myThid )
        CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &                       Nr, smooth3D_Ly, 2, 1, myThid )
        CALL EXCH_XYZ_RL( smooth3D_Lx, myThid )
        CALL EXCH_XYZ_RL( smooth3D_Ly, myThid )
      ELSE
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             smooth3D_Lx(i,j,k,bi,bj) = smooth3D_Lx0(smoothOpNb)
             smooth3D_Ly(i,j,k,bi,bj) = smooth3D_Ly0(smoothOpNb)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           smooth3D_Kuy(i,j,k,bi,bj) = 0.
           smooth3D_Kvx(i,j,k,bi,bj) = 0.
           smooth3D_Kwx(i,j,k,bi,bj) = 0.
           smooth3D_Kwy(i,j,k,bi,bj) = 0.
           smooth3D_Kwz(i,j,k,bi,bj) = 0.
           smooth3D_Kux(i,j,k,bi,bj) = smooth3D_Lx(i,j,k,bi,bj)
     &              *smooth3D_Lx(i,j,k,bi,bj)/smooth3DtotTime/2
           smooth3D_Kvy(i,j,k,bi,bj) = smooth3D_Ly(i,j,k,bi,bj)
     &              *smooth3D_Ly(i,j,k,bi,bj)/smooth3DtotTime/2
           smooth3D_Kuz(i,j,k,bi,bj) = 0.
           smooth3D_Kvz(i,j,k,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C is exchange useful here?

      CALL EXCH_XYZ_RL( smooth3D_kappaR, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kwx, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kwy, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kwz, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kux, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kvy, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kuz, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kvz, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kuy, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kvx, myThid )

C write diffusion operator to file

      WRITE(fnamegeneric(1:80),'(1a,i3.3)')
     &      'smooth3Doperator', smoothOpNb
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      Nr, smooth3D_Kwx, 1, 1, myThid )
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      Nr, smooth3D_Kwy, 2, 1, myThid )
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      Nr, smooth3D_Kwz, 3, 1, myThid )
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      Nr, smooth3D_Kux, 4, 1, myThid )
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      Nr, smooth3D_Kvy, 5, 1, myThid )
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      Nr, smooth3D_Kuz, 6, 1, myThid )
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      Nr, smooth3D_Kvz, 7, 1, myThid )
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      Nr, smooth3D_Kuy, 8, 1, myThid )
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      Nr, smooth3D_Kvx, 9, 1, myThid )
      CALL WRITE_REC_3D_RL( fnamegeneric, smoothprec,
     &                      Nr, smooth3D_kappaR, 10, 1, myThid )

      RETURN
      END
