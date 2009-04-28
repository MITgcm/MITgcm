C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_tracer_s1.F,v 1.7 2009/04/28 18:20:30 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"
 
CBOP
C     !ROUTINE: SHAP_FILT_TRACER_S1
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_TRACER_S1( 
     U           field, tmpFld,
     I           nShapTr, kSize, myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHAP_FILT_TRACER_S1
C     | o Applies Shapiro filter to tracer field (cell center).
C     | o use filtering function "S1" = [1 - d_xx^n - d_yy^n]
C     |     with no grid spacing (computational Filter)
C     *==========================================================*
C     \ev
 
C     !USES:
      IMPLICIT NONE
 
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHAP_FILT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments
C     field :: cell-centered 2D field on which filter applies
C     tmpFld :: working temporary array
C     nShapTr :: (total) power of the filter for this tracer
C     kSize :: length of 3rd Dim : either =1 (2D field) or =Nr (3D field)
C     myTime :: Current time in simulation
C     myThid :: Thread number for this instance of SHAP_FILT_TRACER_S1
      INTEGER nShapTr, kSize
      _RL field(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL     myTime
      INTEGER myThid
 
#ifdef ALLOW_SHAP_FILT

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,K,I,J,N
      _RL tmpGrd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpScal
CEOP

      IF (nShapTr.gt.0) THEN

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,kSize
           DO J=1-OLy,sNy+OLy
            DO I=1-OLx,sNx+OLx
             tmpFld(i,j,k,bi,bj)=field(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO


C      d_xx^n tmpFld 

       DO N=1,nShapTr

        IF (kSize.EQ.Nr) THEN
          _EXCH_XYZ_RL( tmpFld, myThid )
        ELSE
          _EXCH_XY_RL( tmpFld, myThid )
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,kSize

           DO J=1,sNy
            DO I=1,sNx
             tmpGrd(i,j) = -0.25*(
     &        ( tmpFld(i+1,j,k,bi,bj)-tmpFld( i ,j,k,bi,bj) )
     &            *_maskW(i+1,j,k,bi,bj)
     &       -( tmpFld( i ,j,k,bi,bj)-tmpFld(i-1,j,k,bi,bj) )
     &            *_maskW(i,j,k,bi,bj) )
            ENDDO
           ENDDO

           DO J=1,sNy
            DO I=1,sNx
             tmpFld(i,j,k,bi,bj) = tmpGrd(i,j)
            ENDDO
           ENDDO

          ENDDO
         ENDDO
        ENDDO

       ENDDO

C      F <-  [1 - d_xx^n *deltaT/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,kSize
          DO J=1,sNy
           DO I=1,sNx
            tmpScal=field(i,j,k,bi,bj)
            field(i,j,k,bi,bj)=field(i,j,k,bi,bj)
     &          -0.5*tmpFld(i,j,k,bi,bj)*dTtracerLev(1)/Shap_Trtau
            tmpFld(i,j,k,bi,bj)=tmpScal
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO


C      d_yy^n tmpFld 

       DO N=1,nShapTr

        IF (kSize.EQ.Nr) THEN
          _EXCH_XYZ_RL( tmpFld, myThid )
        ELSE
          _EXCH_XY_RL( tmpFld, myThid )
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,kSize

           DO J=1,sNy
            DO I=1,sNx
             tmpGrd(i,j) = -0.25*(
     &        ( tmpFld(i,j+1,k,bi,bj)-tmpFld(i, j ,k,bi,bj) )
     &            *_maskS(i,j+1,k,bi,bj)
     &       -( tmpFld(i, j ,k,bi,bj)-tmpFld(i,j-1,k,bi,bj) )
     &            *_maskS(i,j,k,bi,bj) )
            ENDDO
           ENDDO

           DO J=1,sNy
            DO I=1,sNx
             tmpFld(i,j,k,bi,bj) = tmpGrd(i,j)
            ENDDO
           ENDDO

          ENDDO
         ENDDO
        ENDDO

       ENDDO

C      F <-  [1 - d_yy^n *deltaT/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,kSize
          DO J=1,sNy
           DO I=1,sNx
            field(i,j,k,bi,bj)=field(i,j,k,bi,bj)
     &          -0.5*tmpFld(i,j,k,bi,bj)*dTtracerLev(1)/Shap_Trtau
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

        IF (kSize.EQ.Nr) THEN
          _EXCH_XYZ_RL( field, myThid )
        ELSEIF (kSize.EQ.1) THEN
          _EXCH_XY_RL( field, myThid )
        ELSE
          STOP 'S/R SHAP_FILT_TRACER_S1: kSize is wrong'
        ENDIF

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
