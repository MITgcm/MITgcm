C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_interp.F,v 1.28 2012/01/01 15:25:23 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
#undef OLD_EXF_INTERP_LAT_INDEX

C==========================================*
C Flux Coupler using                       |
C Bilinear interpolation of forcing fields |
C                                          |
C B. Cheng (12/2002)                       |
C                                          |
C added Bicubic (bnc 1/2003)               |
C                                          |
C==========================================*

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       _RL FUNCTION LAGRAN(i,x,a,sp)

       IMPLICIT NONE

        INTEGER i
        _RS x
        _RL a(4)
        INTEGER sp

C-      local variables:
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
C     !ROUTINE: EXF_INTERP
C     !INTERFACE:
       SUBROUTINE EXF_INTERP(
     I                inFile,
     I                filePrec,
     O                arrayout,
     I                irecord, xG_in, yG,
     I                lon_0, lon_inc,
     I                lat_0, lat_inc,
     I                nxIn, nyIn, method, myThid )

C !DESCRIPTION: \bv
C  *==========================================================*
C  | SUBROUTINE EXF_INTERP
C  | o Load from file a regular lat-lon input field
C  |   and interpolate on to the model grid location
C  *==========================================================*
C \ev

C !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"


C !INPUT/OUTPUT PARAMETERS:
C  inFile      (string)  :: name of the binary input file (direct access)
C  filePrec    (integer) :: number of bits per word in file (32 or 64)
C  arrayout    ( _RL )   :: output array
C  irecord     (integer) :: record number to read
C     xG_in,yG           :: coordinates for output grid to interpolate to
C     lon_0, lat_0       :: lon and lat of sw corner of global input grid
C     lon_inc            :: scalar x-grid increment
C     lat_inc            :: vector y-grid increments
C  nxIn,nyIn   (integer) :: size in x & y direction of input file to read
C     method             :: 1,11,21 for bilinear; 2,12,22 for bicubic
C                        :: 1,2 for tracer; 11,12 for U; 21,22 for V
C  myThid      (integer) :: My Thread Id number

      CHARACTER*(*) infile
      INTEGER       filePrec, irecord, nxIn, nyIn
      _RL           arrayout(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           xG_in   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           yG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL           lon_0, lon_inc
c     _RL           lat_0, lat_inc(nyIn-1)
      _RL           lat_0, lat_inc(*)
      INTEGER       method, myThid

C !FUNCTIONS:
      EXTERNAL LAGRAN
      _RL      LAGRAN
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES:
C     msgBuf      :: Informational/error message buffer
C     bi, bj      :: tile indices
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER  bi, bj
      INTEGER  w_ind(sNx,sNy), s_ind(sNx,sNy)
      _RL      px_ind(4), py_ind(4), ew_val(4)
      _RL      arrayin( -1:nxIn+2, -1:nyIn+2 )
      _RL      x_in(-1:nxIn+2), y_in(-1:nyIn+2)
      _RL      NorthValue
      INTEGER  i, j, k, l, sp
#ifdef OLD_EXF_INTERP_LAT_INDEX
      INTEGER  js
#else
      INTEGER  nLoop
      _RL      tmpVar
#endif
#ifdef TARGET_NEC_SX
      INTEGER  ic, ii, icnt
      INTEGER  inx(sNx*sNy,2)
      _RL      ew_val1, ew_val2, ew_val3, ew_val4
#endif
      _RS      xG(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL      ninety
      PARAMETER ( ninety = 90. )
      _RS      threeSixtyRS
      PARAMETER ( threeSixtyRS = 360. )
      LOGICAL  xIsPeriodic
CEOP

C--   put xG in interval [ lon_0 , lon_0+360 [
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          xG(i,j,bi,bj) = xG_in(i,j,bi,bj)-lon_0
     &                  + threeSixtyRS*2.
          xG(i,j,bi,bj) = lon_0+MOD(xG(i,j,bi,bj),threeSixtyRS)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Load inut field
      CALL EXF_INTERP_READ(
     I         inFile, filePrec,
     O         arrayin,
     I         irecord, nxIn, nyIn, myThid )

C--   setup input longitude grid
      DO i=-1,nxIn+2
       x_in(i) = lon_0 + (i-1)*lon_inc
      ENDDO
      xIsPeriodic = nxIn.EQ.NINT( threeSixtyRS / lon_inc )

C--   setup input latitude grid
      y_in(1) = lat_0
      DO j=1,nyIn+1
       i = MIN(j,nyIn-1)
       y_in(j+1) = y_in(j) + lat_inc(i)
      ENDDO
C-    Add 2 row @ southern end; if one is beyond S.pole, put one @ S.pole
      y_in(0) = y_in(1) - lat_inc(1)
      y_in(-1)= y_in(0) - lat_inc(1)
c     IF ( y_in(1).GT.-ninety .AND. y_in(0).LT.-ninety ) THEN
c       y_in(0) = -ninety
c       y_in(-1) = -2.*ninety - y_in(1)
c     ENDIF
c     IF ( y_in(0).GT.-ninety .AND. y_in(-1).LT.-ninety ) THEN
c       y_in(-1) = -ninety
c     ENDIF
C-    Add 2 row @ northern end; if one is beyond N.pole, put one @ N.pole
      j = nyIn+1
      IF ( y_in(j-1).LT.ninety .AND. y_in(j).GT.ninety ) THEN
        y_in(j) = ninety
        y_in(j+1) = 2.*ninety - y_in(j-1)
      ENDIF
      j = nyIn+2
      IF ( y_in(j-1).LT.ninety .AND. y_in(j).GT.ninety ) THEN
        y_in(j) = ninety
      ENDIF

C--   enlarge boundary
      IF ( xIsPeriodic ) THEN
        DO j=1,nyIn
         arrayin( 0,j)     = arrayin(nxIn  ,j)
         arrayin(-1,j)     = arrayin(nxIn-1,j)
         arrayin(nxIn+1,j) = arrayin(1,j)
         arrayin(nxIn+2,j) = arrayin(2,j)
        ENDDO
      ELSE
        DO j=1,nyIn
         arrayin( 0,j)     = arrayin(1,j)
         arrayin(-1,j)     = arrayin(1,j)
         arrayin(nxIn+1,j) = arrayin(nxIn,j)
         arrayin(nxIn+2,j) = arrayin(nxIn,j)
        ENDDO
      ENDIF
      DO i=-1,nxIn+2
         arrayin(i, 0)     = arrayin(i,1)
         arrayin(i,-1)     = arrayin(i,1)
         arrayin(i,nyIn+1) = arrayin(i,nyIn)
         arrayin(i,nyIn+2) = arrayin(i,nyIn)
      ENDDO

C-    For tracer (method=1,2) set to northernmost zonal-mean value
C     at 90N to avoid sharp zonal gradients near the Pole.
C     For U (method=11,12) set to zero at 90N to minimize velocity
C     gradient at North Pole
C     For V (method=11,12) set to northernmost zonal value at 90N,
C     as is already done above in order to allow cross-PoleArctic flow
      DO j=nyIn,nyIn+2
       IF (y_in(j).EQ.ninety) THEN
        IF (method.EQ.1 .OR. method.EQ.2) THEN
         NorthValue = 0.
         DO i=1,nxIn
          NorthValue = NorthValue + arrayin(i,j)
         ENDDO
         NorthValue = NorthValue / nxIn
         DO i=-1,nxIn+2
          arrayin(i,j) = NorthValue
         ENDDO
        ELSEIF (method.EQ.11 .OR. method.EQ.12) THEN
         DO i=-1,nxIn+2
          arrayin(i,j) = 0.
         ENDDO
        ENDIF
       ENDIF
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--   Check validity of input/output coordinates
#ifdef ALLOW_DEBUG
        IF ( debugLevel.GE.debLevC ) THEN
         DO j=1,sNy
          DO i=1,sNx
           IF ( xG(i,j,bi,bj) .LT. x_in(0)        .OR.
     &          xG(i,j,bi,bj) .GE. x_in(nxIn+1)   .OR.
     &          yG(i,j,bi,bj) .LT. y_in(0)        .OR.
     &          yG(i,j,bi,bj) .GE. y_in(nyIn+1) ) THEN
            l = ILNBLNK(inFile)
            WRITE(msgBuf,'(3A,I6)')
     &        'EXF_INTERP: file="', inFile(1:l), '", rec=', irecord
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A)')
     &        'EXF_INTERP: input grid must encompass output grid.'
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,2I8,2I6,A,1P2E14.6)') 'i,j,bi,bj=',
     &        i,j,bi,bj, ' , xG,yG=', xG(i,j,bi,bj), yG(i,j,bi,bj)
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,I9,A,1P2E14.6)') 'nxIn=', nxIn,
     &        ' , x_in(0,nxIn+1)=', x_in(0) ,x_in(nxIn+1)
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,I9,A,1P2E14.6)') 'nyIn=', nyIn,
     &        ' , y_in(0,nyIn+1)=', y_in(0) ,y_in(nyIn+1)
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R EXF_INTERP'
           ENDIF
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DEBUG */

C--   Compute interpolation indices
#ifdef OLD_EXF_INTERP_LAT_INDEX
        DO j=1,sNy
         DO i=1,sNx
          IF (xG(i,j,bi,bj)-x_in(1) .GE. 0.) THEN
           w_ind(i,j) = INT((xG(i,j,bi,bj)-x_in(1))/lon_inc) + 1
          ELSE
           w_ind(i,j) = INT((xG(i,j,bi,bj)-x_in(1))/lon_inc)
          ENDIF
         ENDDO
        ENDDO
#ifndef TARGET_NEC_SX
C-    use the original and more readable variant of the algorithm that
C     has unvectorizable while-loops for each (i,j)
        DO j=1,sNy
         DO i=1,sNx
          js = nyIn*.5
          DO WHILE (yG(i,j,bi,bj) .LT. y_in(js))
           js = (js - 1)*.5
          ENDDO
          DO WHILE (yG(i,j,bi,bj) .GE. y_in(js+1))
           js = js + 1
          ENDDO
          s_ind(i,j) = js
         ENDDO
        ENDDO
#else /* TARGET_NEC_SX defined */
C-    this variant vectorizes more efficiently than the original one because
C     it moves the while loops out of the i,j loops (loop pushing) but
C     it is ugly and incomprehensible
        icnt = 0
        DO j=1,sNy
         DO i=1,sNx
          s_ind(i,j) = nyIn*.5
          icnt = icnt+1
          inx(icnt,1) = i
          inx(icnt,2) = j
         ENDDO
        ENDDO
        DO WHILE (icnt .GT. 0)
         ii = 0
!CDIR NODEP
         DO ic=1,icnt
          i = inx(ic,1)
          j = inx(ic,2)
          IF (yG(i,j,bi,bj) .LT. y_in(s_ind(i,j))) THEN
           s_ind(i,j) = (s_ind(i,j) - 1)*.5
           ii = ii+1
           inx(ii,1) = i
           inx(ii,2) = j
          ENDIF
         ENDDO
         icnt = ii
        ENDDO
        icnt = 0
        DO j=1,sNy
         DO i=1,sNx
          icnt = icnt+1
          inx(icnt,1) = i
          inx(icnt,2) = j
         ENDDO
        ENDDO
        DO WHILE (icnt .GT. 0)
         ii = 0
!CDIR NODEP
         DO ic=1,icnt
          i = inx(ic,1)
          j = inx(ic,2)
          IF (yG(i,j,bi,bj) .GE. y_in(s_ind(i,j)+1)) THEN
           s_ind(i,j) = s_ind(i,j) + 1
           ii = ii+1
           inx(ii,1) = i
           inx(ii,2) = j
          ENDIF
         ENDDO
         icnt = ii
        ENDDO
#endif /* TARGET_NEC_SX defined */
#else /* OLD_EXF_INTERP_LAT_INDEX */
C--     latitude index
        DO j=1,sNy
         DO i=1,sNx
          s_ind(i,j) = 0
          w_ind(i,j) = nyIn+1
         ENDDO
        ENDDO
C       # of pts = nyIn+2 ; # of interval = nyIn+1 ; evaluate nLoop as
C       1 + truncated log2(# interval -1); add epsil=1.e-3 for safey
        tmpVar = nyIn + 1. _d -3
        nLoop = 1 + INT( LOG(tmpVar)/LOG(2. _d 0) )
        DO l=1,nLoop
         DO j=1,sNy
          DO i=1,sNx
           IF ( w_ind(i,j).GT.s_ind(i,j)+1 ) THEN
            k = NINT( (s_ind(i,j)+w_ind(i,j))*0.5 )
            IF ( yG(i,j,bi,bj) .LT. y_in(k) ) THEN
              w_ind(i,j) = k
            ELSE
              s_ind(i,j) = k
            ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_DEBUG
        IF ( debugLevel.GE.debLevC ) THEN
C-      Check that we found the right lat. index
         DO j=1,sNy
          DO i=1,sNx
           IF ( w_ind(i,j).NE.s_ind(i,j)+1 ) THEN
            l = ILNBLNK(inFile)
            WRITE(msgBuf,'(3A,I4,A,I4)')
     &        'EXF_INTERP: file="', inFile(1:l), '", rec=', irecord,
     &        ', nLoop=', nLoop
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A)')
     &        'EXF_INTERP: did not found Latitude index for grid-pt:'
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,2I8,2I6,A,1PE16.8)')
     &        'EXF_INTERP: i,j,bi,bj=',i,j,bi,bj,' , yG=',yG(i,j,bi,bj)
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,I8,A,1PE16.8)')
     &        'EXF_INTERP: s_ind=',s_ind(i,j),', lat=',y_in(s_ind(i,j))
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,I8,A,1PE16.8)')
     &        'EXF_INTERP: n_ind=',w_ind(i,j),', lat=',y_in(w_ind(i,j))
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R EXF_INTERP'
           ENDIF
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DEBUG */
C--     longitude index
        DO j=1,sNy
         DO i=1,sNx
           w_ind(i,j) = INT((xG(i,j,bi,bj)-x_in(-1))/lon_inc) - 1
         ENDDO
        ENDDO
#endif /* ndef OLD_EXF_INTERP_LAT_INDEX */

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
     &        'EXF_INTERP: file="', inFile(1:l), '", rec=', irecord
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,I8,A)')
     &        'EXF_INTERP: method=', method,' not supported'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R EXF_INTERP: invalid method'
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
