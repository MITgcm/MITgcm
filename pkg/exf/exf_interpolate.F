C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_interpolate.F,v 1.1 2012/01/05 20:22:28 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

C--  File exf_interp.F: Routines to interpolate input field on to model grid
C--   Contents
C--   o S/R EXF_INTERPOLATE
C--   o FCT LAGRAN

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: LAGRAN
C     !INTERFACE:
       _RL FUNCTION LAGRAN(i,x,a,sp)

C !DESCRIPTION: \bv
C  *==========================================================*
C  | FUNCTION LAGRAN
C  | o Provide linear (sp=2) and cubic (sp=4) interpolation
C  |   weight as lagrange polynomial coefficient.
C  *==========================================================*
C \ev

C !USES:
       IMPLICIT NONE

C !INPUT/OUTPUT PARAMETERS:
        INTEGER i
        _RS x
        _RL a(4)
        INTEGER sp

C !LOCAL VARIABLES:
        INTEGER k
        _RL numer,denom

        numer = 1. _d 0
        denom = 1. _d 0

#ifdef TARGET_NEC_SX
!CDIR UNROLL=8
#endif /* TARGET_NEC_SX */
        DO k=1,sp
         IF ( k .NE. i) THEN
          denom = denom*(a(i) - a(k))
          numer = numer*(x    - a(k))
         ENDIF
        ENDDO

        LAGRAN = numer/denom

       RETURN
       END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: EXF_INTERPOLATE
C     !INTERFACE:
       SUBROUTINE EXF_INTERPOLATE(
     I                inFile, irecord, method,
     I                nxIn, nyIn, x_in, y_in,
     I                arrayin,
     O                arrayout,
     I                xG, yG,
     I                w_ind, s_ind,
     I                bi, bj, myThid )

C !DESCRIPTION: \bv
C  *==========================================================*
C  | SUBROUTINE EXF_INTERPOLATE
C  | o Interpolate a regular lat-lon input field
C  |   on to the model grid location
C  *==========================================================*
C \ev

C !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT/OUTPUT PARAMETERS:
C   inFile     :: name of the binary input file (direct access)
C   irecord    :: record number in input file
C   method     :: 1,11,21 for bilinear; 2,12,22 for bicubic
C              :: 1,2 for tracer; 11,12 for U; 21,22 for V
C   nxIn,nyIn  :: size in x & y direction of input field
C    x_in      :: longitude vector defining input field grid
C    y_in      :: latitude  vector defining input field grid
C   arrayin    :: input field array (loaded from file)
C   arrayout   :: output array
C   xG, yG     :: coordinates for output grid to interpolate to
C    w_ind     :: input field longitudinal index, on western side of model grid pt
C    s_ind     :: input field latitudinal index, on southern side of model grid pt
C   bi, bj     :: tile indices
C   myThid     :: My Thread Id number

      CHARACTER*(*) inFile
      INTEGER       irecord, method
      INTEGER       nxIn, nyIn
      _RL           x_in(-1:nxIn+2), y_in(-1:nyIn+2)
      _RL           arrayin( -1:nxIn+2, -1:nyIn+2 )
      _RL           arrayout(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           xG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           yG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER       w_ind(sNx,sNy), s_ind(sNx,sNy)
      INTEGER       bi, bj, myThid

C !FUNCTIONS:
      EXTERNAL LAGRAN
      _RL      LAGRAN
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES:
C   px_ind     :: local copy of longitude position around current model grid pt
C   py_ind     :: local copy of latitude  position around current model grid pt
C   ew_val     :: intermediate field value after interpolation in East-West dir.
C   sp         :: number of input-field values used in 1.d interpolation
C   i, j, k, l :: loop indices
C   msgBuf     :: Informational/error message buffer
      _RL      px_ind(4), py_ind(4), ew_val(4)
      INTEGER  sp
      INTEGER  i, j, k, l
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef TARGET_NEC_SX
      _RL      ew_val1, ew_val2, ew_val3, ew_val4
#endif
CEOP

      IF (method.EQ.1 .OR. method.EQ.11 .OR. method.EQ.21) THEN

C--   Bilinear interpolation
         sp = 2
         DO j=1,sNy
          DO i=1,sNx
           arrayout(i,j,bi,bj) = 0.
           DO l=0,1
            px_ind(l+1) = x_in(w_ind(i,j)+l)
            py_ind(l+1) = y_in(s_ind(i,j)+l)
           ENDDO
#ifndef TARGET_NEC_SX
           DO k=1,2
            ew_val(k) = arrayin(w_ind(i,j)  ,s_ind(i,j)+k-1)
     &                    *LAGRAN(1,xG(i,j,bi,bj),px_ind,sp)
     &                + arrayin(w_ind(i,j)+1,s_ind(i,j)+k-1)
     &                    *LAGRAN(2,xG(i,j,bi,bj),px_ind,sp)
            arrayout(i,j,bi,bj) = arrayout(i,j,bi,bj)
     &         + ew_val(k)*LAGRAN(k,yG(i,j,bi,bj),py_ind,sp)
           ENDDO
#else
           ew_val1 = arrayin(w_ind(i,j)  ,s_ind(i,j)  )
     &                    *LAGRAN(1,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)+1,s_ind(i,j)  )
     &                    *LAGRAN(2,xG(i,j,bi,bj),px_ind,sp)
           ew_val2 = arrayin(w_ind(i,j)  ,s_ind(i,j)+1)
     &                    *LAGRAN(1,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)+1,s_ind(i,j)+1)
     &                    *LAGRAN(2,xG(i,j,bi,bj),px_ind,sp)
           arrayout(i,j,bi,bj)=
     &            +ew_val1*LAGRAN(1,yG(i,j,bi,bj),py_ind,sp)
     &            +ew_val2*LAGRAN(2,yG(i,j,bi,bj),py_ind,sp)
#endif /* TARGET_NEC_SX defined */
          ENDDO
         ENDDO
      ELSEIF (method .EQ. 2 .OR. method.EQ.12 .OR. method.EQ.22) THEN

C--   Bicubic interpolation
         sp = 4
         DO j=1,sNy
          DO i=1,sNx
           arrayout(i,j,bi,bj) = 0.
           DO l=-1,2
            px_ind(l+2) = x_in(w_ind(i,j)+l)
            py_ind(l+2) = y_in(s_ind(i,j)+l)
           ENDDO
#ifndef TARGET_NEC_SX
           DO k=1,4
            ew_val(k) = arrayin(w_ind(i,j)-1,s_ind(i,j)+k-2)
     &                    *LAGRAN(1,xG(i,j,bi,bj),px_ind,sp)
     &                + arrayin(w_ind(i,j)  ,s_ind(i,j)+k-2)
     &                    *LAGRAN(2,xG(i,j,bi,bj),px_ind,sp)
     &                + arrayin(w_ind(i,j)+1,s_ind(i,j)+k-2)
     &                    *LAGRAN(3,xG(i,j,bi,bj),px_ind,sp)
     &                + arrayin(w_ind(i,j)+2,s_ind(i,j)+k-2)
     &                    *LAGRAN(4,xG(i,j,bi,bj),px_ind,sp)
            arrayout(i,j,bi,bj) = arrayout(i,j,bi,bj)
     &         + ew_val(k)*LAGRAN(k,yG(i,j,bi,bj),py_ind,sp)
           ENDDO
#else
           ew_val1 = arrayin(w_ind(i,j)-1,s_ind(i,j)-1)
     &                    *LAGRAN(1,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)  ,s_ind(i,j)-1)
     &                    *LAGRAN(2,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)+1,s_ind(i,j)-1)
     &                    *LAGRAN(3,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)+2,s_ind(i,j)-1)
     &                    *LAGRAN(4,xG(i,j,bi,bj),px_ind,sp)
           ew_val2 = arrayin(w_ind(i,j)-1,s_ind(i,j)  )
     &                    *LAGRAN(1,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)  ,s_ind(i,j)  )
     &                    *LAGRAN(2,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)+1,s_ind(i,j)  )
     &                    *LAGRAN(3,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)+2,s_ind(i,j)  )
     &                    *LAGRAN(4,xG(i,j,bi,bj),px_ind,sp)
           ew_val3 = arrayin(w_ind(i,j)-1,s_ind(i,j)+1)
     &                    *LAGRAN(1,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)  ,s_ind(i,j)+1)
     &                    *LAGRAN(2,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)+1,s_ind(i,j)+1)
     &                    *LAGRAN(3,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)+2,s_ind(i,j)+1)
     &                    *LAGRAN(4,xG(i,j,bi,bj),px_ind,sp)
           ew_val4 = arrayin(w_ind(i,j)-1,s_ind(i,j)+2)
     &                    *LAGRAN(1,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)  ,s_ind(i,j)+2)
     &                    *LAGRAN(2,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)+1,s_ind(i,j)+2)
     &                    *LAGRAN(3,xG(i,j,bi,bj),px_ind,sp)
     &             + arrayin(w_ind(i,j)+2,s_ind(i,j)+2)
     &                    *LAGRAN(4,xG(i,j,bi,bj),px_ind,sp)
           arrayout(i,j,bi,bj) =
     &             ew_val1*LAGRAN(1,yG(i,j,bi,bj),py_ind,sp)
     &            +ew_val2*LAGRAN(2,yG(i,j,bi,bj),py_ind,sp)
     &            +ew_val3*LAGRAN(3,yG(i,j,bi,bj),py_ind,sp)
     &            +ew_val4*LAGRAN(4,yG(i,j,bi,bj),py_ind,sp)
#endif /* TARGET_NEC_SX defined */
          ENDDO
         ENDDO
      ELSE
         l = ILNBLNK(inFile)
         WRITE(msgBuf,'(3A,I6)')
     &    'EXF_INTERPOLATE: file="', inFile(1:l), '", rec=', irecord
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,I8,A)')
     &    'EXF_INTERPOLATE: method=', method,' not supported'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R EXF_INTERPOLATE: invalid method'
      ENDIF

      RETURN
      END
