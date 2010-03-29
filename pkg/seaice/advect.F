C $Header: /u/gcmpack/MITgcm/pkg/seaice/advect.F,v 1.26 2010/03/29 12:35:54 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ADVECT( UI,VI,fld,fldNm1,iceMsk,myThid )
C     *==========================================================*
C     | SUBROUTINE ADVECT                                        |
C     | o Calculate ice advection                                |
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_PARAMS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      _RL UI     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VI     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fldNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceMsk (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      INTEGER k
      _RL DELTT
      _RL DIFFA  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tmpFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL afx    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      DELTT=SEAICE_deltaTtherm
C     save fld from previous time step
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fldNm1(i,j,bi,bj) = fld(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO k=1,2
cph       IF ( k .EQ. 1 ) THEN
C     Predition step
cph        DO bj=myByLo(myThid),myByHi(myThid)
cph         DO bi=myBxLo(myThid),myBxHi(myThid)
cph          DO j=1-OLy,sNy+OLy
cph           DO i=1-OLx,sNx+OLx
cph            tmpFld(I,J,bi,bj) = fld(I,J,bi,bj)
cph           ENDDO
cph          ENDDO
cph         ENDDO
cph        ENDDO
cph       ELSE       
C     Backward Euler correction step
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
cph for k=1 this is same as tmpFld = fld
            tmpFld(i,j,bi,bj)=HALF*(fld(I,J,bi,bj)
     &           +fldNm1(i,j,bi,bj))
           ENDDO
          ENDDO
         ENDDO
        ENDDO
cph       ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE fld    = comlev1, key = ikey_dynamics
cphCADJ STORE fldNm1 = comlev1, key = ikey_dynamics
cphCADJ STORE tmpFld = comlev1, key = ikey_dynamics
       DO J=1-Oly,sNy+Oly
        DO I=1-Olx,sNx+Olx
         afx(I,J) = 0. _d 0
         afy(I,J) = 0. _d 0
        ENDDO
       ENDDO
#endif /* ALLOW_AUTODIFF_TAMC */

C NOW GO THROUGH STANDARD CONSERVATIVE ADVECTION
       IF ( .NOT. SEAICEuseFluxForm ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO J=0,sNy+1
           DO I=0,sNx+1
CML   This formulation gives the same result as the original code on a
CML   lat-lon-grid, but may not be accurate on irregular grids
            fld(I,J,bi,bj)=fldNm1(I,J,bi,bj)
     &           -DELTT*(
     &           ( tmpFld(I  ,J  ,bi,bj)+tmpFld(I+1,J  ,bi,bj))
     &           *   UI(I+1,J,  bi,bj) -
     &           ( tmpFld(I  ,J  ,bi,bj)+tmpFld(I-1,J  ,bi,bj))
     &           *   UI(I  ,J,  bi,bj) )
     &           *(HALF * _recip_dxF(I,J,bi,bj))
     &           -DELTT*(
     &           ( tmpFld(I  ,J  ,bi,bj)+tmpFld(I  ,J+1,bi,bj))
     &           *   VI(I  ,J+1,  bi,bj)
     &           * _dxG(I  ,J+1,bi,bj) -
     &           ( tmpFld(I  ,J  ,bi,bj)+tmpFld(I  ,J-1,bi,bj))
     &           *   VI(I  ,J  ,  bi,bj)
     &           * _dxG(I,J,bi,bj))
     &           *(HALF * _recip_dyF(I,J,bi,bj) * _recip_dxF(I,J,bi,bj))
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ELSE
C--   Use flux form for MITgcm compliance, unfortunately changes results
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
C--   first compute fluxes across cell faces
          DO J=1,sNy+1
           DO I=1,sNx+1
            afx(I,J) = _dyG(I,J,bi,bj) * UI(I,J,bi,bj) 
     &           * 0.5 _d 0 * (tmpFld(I,J,bi,bj)+tmpFld(I-1,J,bi,bj))
            afy(I,J) = _dxG(I,J,bi,bj) * VI(I,J,bi,bj)
     &           * 0.5 _d 0 * (tmpFld(I,J,bi,bj)+tmpFld(I,J-1,bi,bj))
           ENDDO
          ENDDO
          DO J=1,sNy
           DO I=1,sNx
            fld(I,J,bi,bj)=fldNm1(I,J,bi,bj)
     &           -DELTT * ( 
     &             afx(I+1,J) - afx(I,J)
     &           + afy(I,J+1) - afy(I,J)
     &           )*recip_rA(I,J,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

       CALL EXCH_XY_RL( fld, myThid )

      ENDDO

C NOW DO DIFFUSION 
C     make a working copy of field from last time step
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          tmpFld(i,j,bi,bj)=fldNm1(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      DO k = 1,2
C NOW CALCULATE DIFFUSION COEF ROUGHLY
       IF ( k .EQ. 1 ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            DIFFA(I,J,bi,bj)=
     &           DIFF1*MIN( _dxF(I,J,bi,bj), _dyF(I,J,bi,bj))
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ELSE
C     use some strange quardatic form for the second time around
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            DIFFA(I,J,bi,bj)=
     &           -(MIN( _dxF(I,J,bi,bj),  _dyF(I,J,bi,bj)))**2/DELTT
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
       CALL DIFFUS(tmpFld,DIFFA,iceMsk,DELTT, myThid)

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           fld(I,J,bi,bj)=(fld(I,J,bi,bj)+tmpFld(i,j,bi,bj))
     &          *iceMsk(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       
      ENDDO

      RETURN
      END
