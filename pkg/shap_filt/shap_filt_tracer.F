C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/Attic/shap_filt_tracer.F,v 1.3 2001/02/04 14:38:50 cnh Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

      SUBROUTINE SHAP_FILT_TRACER( 
     U           field, 
     I           myTime, myThid )
C     /==========================================================\
C     | S/R SHAP_FILT_TRACER                                     |
C     | Applies Shapiro filter to tracer field over one XY slice |
C     | of one tile at a time.                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments
      _RL field(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     myTime
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

C     == Local variables ==
      INTEGER bi,bj,K,I,J,N
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmpGrd(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifndef SEQUENTIAL_2D_SHAP
      _RL tmpScal
#endif

      IF (nShap.gt.0) THEN

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr
           DO J=1,sNy
            DO I=1,sNx
             tmpFld(i,j,k,bi,bj)=field(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO


C      d_xx^n tmpFld 

       DO N=1,nShap

        _EXCH_XYZ_R8( tmpFld, myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr

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

C      F <-  [1-d_xx^n]F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
#ifdef      SEQUENTIAL_2D_SHAP
            tmpFld(i,j,k,bi,bj)=field(i,j,k,bi,bj)-tmpFld(i,j,k,bi,bj)
            field(i,j,k,bi,bj)=tmpFld(i,j,k,bi,bj)
#else
            tmpScal=tmpFld(i,j,k,bi,bj)
            tmpFld(i,j,k,bi,bj)=field(i,j,k,bi,bj)
            field(i,j,k,bi,bj)=field(i,j,k,bi,bj)-0.5*tmpScal
#endif   /* SEQUENTIAL_2D_SHAP */
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO


C      d_yy^n tmpFld 

       DO N=1,nShap

        _EXCH_XYZ_R8( tmpFld, myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr

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

C      F <-  [1-d_yy^n]F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx
#ifdef      SEQUENTIAL_2D_SHAP
            field(i,j,k,bi,bj)=field(i,j,k,bi,bj)-tmpFld(i,j,k,bi,bj)
#else
            tmpScal=tmpFld(i,j,k,bi,bj)
            field(i,j,k,bi,bj)=field(i,j,k,bi,bj)-0.5*tmpScal
#endif   /* SEQUENTIAL_2D_SHAP */
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _EXCH_XYZ_R8( field, myThid )

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
