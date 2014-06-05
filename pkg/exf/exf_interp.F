C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_interp.F,v 1.31 2014/06/05 15:37:46 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: EXF_INTERP
C     !INTERFACE:
       SUBROUTINE EXF_INTERP(
     I                inFile, filePrec,
     O                arrayout,
     I                irecord, xG_in, yG,
     I                lon_0, lon_inc, lat_0, lat_inc,
     I                nxIn, nyIn, method, myIter, myThid )

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
#ifdef ALLOW_DEBUG
# include "EXF_PARAM.h"
#endif

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
C  myIter      (integer) :: current iteration number
C  myThid      (integer) :: My Thread Id number

      CHARACTER*(*) inFile
      INTEGER       filePrec, irecord, nxIn, nyIn
      _RL           arrayout(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           xG_in   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           yG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL           lon_0, lon_inc
c     _RL           lat_0, lat_inc(nyIn-1)
      _RL           lat_0, lat_inc(*)
      INTEGER       method, myIter, myThid

C !FUNCTIONS:
#ifdef ALLOW_DEBUG
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif

C !LOCAL VARIABLES:
C     arrayin     :: input field array (loaded from file)
C     x_in        :: longitude vector defining input field grid
C     y_in        :: latitude  vector defining input field grid
C     w_ind       :: input field longitudinal index, on western side of model grid pt
C     s_ind       :: input field latitudinal index, on southern side of model grid pt
C     bi, bj      :: tile indices
C     i, j, k, l  :: loop indices
C     msgBuf      :: Informational/error message buffer
      _RL      arrayin( -1:nxIn+2, -1:nyIn+2 )
      _RL      x_in(-1:nxIn+2), y_in(-1:nyIn+2)
      INTEGER  w_ind(sNx,sNy), s_ind(sNx,sNy)
      INTEGER  bi, bj
      INTEGER  i, j, k, l
      INTEGER  nLoop
      _RL      tmpVar
      _RS      xG(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS      threeSixtyRS
      _RL      yPole, symSign, poleValue
      PARAMETER ( threeSixtyRS = 360. )
      PARAMETER ( yPole = 90. )
      INTEGER  nxd2
      LOGICAL  xIsPeriodic, poleSymmetry
#ifdef ALLOW_DEBUG
      LOGICAL  debugFlag
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RS      prtPole(-1:4)
#endif
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  Load inut field

      CALL EXF_INTERP_READ(
     I         inFile, filePrec,
     O         arrayin,
     I         irecord, nxIn, nyIn, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  Prepare input grid and input field

C--   setup input longitude grid
      DO i=-1,nxIn+2
       x_in(i) = lon_0 + (i-1)*lon_inc
      ENDDO
      xIsPeriodic = nxIn.EQ.NINT( threeSixtyRS / lon_inc )
      nxd2 = NINT( nxIn*0.5 )
      poleSymmetry = xIsPeriodic .AND. ( nxIn.EQ.2*nxd2 )
#ifdef EXF_USE_OLD_INTERP_POLE
      poleSymmetry = .FALSE.
#endif

C--   setup input latitude grid
      y_in(1) = lat_0
      DO j=1,nyIn+1
       i = MIN(j,nyIn-1)
       y_in(j+1) = y_in(j) + lat_inc(i)
      ENDDO
      y_in(0) = y_in(1) - lat_inc(1)
      y_in(-1)= y_in(0) - lat_inc(1)
#ifdef ALLOW_DEBUG
      DO l=-1,4
        prtPole(l) = 0.
      ENDDO
#endif
C--   For tracer (method=1,2) add 1 row @ the pole (if last row is not)
C     and will fill it with the polarmost-row zonally averaged value.
C     For vector component, cannot do much without the other component;
C     averaging properly done if uvInterp=T in S/R EXF_INTERP_UV
#ifdef EXF_USE_OLD_INTERP_POLE
      IF ( .TRUE. ) THEN
#else
      IF ( method.LT.10 ) THEN
C-    Add 2 row @ southern end; if one is beyond S.pole, put one @ S.pole
       j = 0
       IF ( ABS(y_in(j+1)).LT.yPole .AND. ABS(y_in(j)).GT.yPole ) THEN
         y_in(j) = -yPole
         y_in(j-1) = -2.*yPole - y_in(1)
#ifdef ALLOW_DEBUG
         prtPole(j)   = 1.
         prtPole(j-1) = 2.
#endif
       ENDIF
       j = -1
       IF ( ABS(y_in(j+1)).GT.yPole .AND. ABS(y_in(j)).GT.yPole ) THEN
         y_in(j) = -yPole
#ifdef ALLOW_DEBUG
         prtPole(j)   = 1.
#endif
       ENDIF
#endif /* EXF_USE_OLD_INTERP_POLE */
C-    Add 2 row @ northern end; if one is beyond N.pole, put one @ N.pole
       j = nyIn+1
       IF ( ABS(y_in(j-1)).LT.yPole .AND. ABS(y_in(j)).GT.yPole ) THEN
         y_in(j) = yPole
         y_in(j+1) = 2.*yPole - y_in(j-1)
#ifdef ALLOW_DEBUG
         prtPole(3)   = 1.
         prtPole(3+1) = 2.
#endif
       ENDIF
       j = nyIn+2
       IF ( ABS(y_in(j-1)).LT.yPole .AND. ABS(y_in(j)).GT.yPole ) THEN
         y_in(j) = yPole
#ifdef ALLOW_DEBUG
         prtPole(4)   = 1.
#endif
       ENDIF
      ENDIF

C--   Enlarge boundary
      IF ( xIsPeriodic ) THEN
C-    fill-in added column assuming periodicity
        DO j=1,nyIn
         arrayin( 0,j)     = arrayin(nxIn  ,j)
         arrayin(-1,j)     = arrayin(nxIn-1,j)
         arrayin(nxIn+1,j) = arrayin(1,j)
         arrayin(nxIn+2,j) = arrayin(2,j)
        ENDDO
      ELSE
C-    fill-in added column from nearest column
        DO j=1,nyIn
         arrayin( 0,j)     = arrayin(1,j)
         arrayin(-1,j)     = arrayin(1,j)
         arrayin(nxIn+1,j) = arrayin(nxIn,j)
         arrayin(nxIn+2,j) = arrayin(nxIn,j)
        ENDDO
      ENDIF
      symSign = 1. _d 0
      IF ( method.GE.10 ) symSign = -1. _d 0
      DO l=-1,2
       j = l
       IF ( l.GE.1 ) j = nyIn+l
       k = MAX(1,MIN(j,nyIn))
       IF ( poleSymmetry .AND. ABS(y_in(j)).GT.yPole ) THEN
C-    fill-in added row assuming pole-symmetry
        DO i=-1,nxd2
         arrayin(i,j) = symSign*arrayin(i+nxd2,k)
        ENDDO
        DO i=1,nxd2+2
         arrayin(i+nxd2,j) = symSign*arrayin(i,k)
        ENDDO
#ifdef ALLOW_DEBUG
        i = l + 2*( (l+1)/2 )
        prtPole(i) = prtPole(i) + 0.2
#endif
       ELSE
C-    fill-in added row from nearest column values
        DO i=-1,nxIn+2
         arrayin(i,j) = arrayin(i,k)
        ENDDO
       ENDIF
      ENDDO

C--   For tracer (method=1,2) set to northernmost zonal-mean value
C     at 90N to avoid sharp zonal gradients near the Pole.
C     For vector component, cannot do much without the other component;
C     averaging properly done if uvInterp=T in S/R EXF_INTERP_UV
#ifdef EXF_USE_OLD_INTERP_POLE
      IF ( .TRUE. ) THEN
       DO l= 3,4
#else
      IF ( method.LT.10 ) THEN
       DO l=-1,4
#endif
        j = l
        IF ( l.GE.2 ) j = nyIn+l-2
        IF ( ABS(y_in(j)).EQ.yPole ) THEN
         IF (method.EQ.1 .OR. method.EQ.2) THEN
          poleValue = 0.
          DO i=1,nxIn
           poleValue = poleValue + arrayin(i,j)
          ENDDO
          poleValue = poleValue / nxIn
          DO i=-1,nxIn+2
           arrayin(i,j) = poleValue
          ENDDO
#ifdef ALLOW_DEBUG
          prtPole(l) = prtPole(l) + 0.1
#endif
#ifdef EXF_USE_OLD_INTERP_POLE
         ELSEIF (method.EQ.11 .OR. method.EQ.12) THEN
          DO i=-1,nxIn+2
           arrayin(i,j) = 0.
          ENDDO
#ifdef ALLOW_DEBUG
          prtPole(l) = prtPole(l) + 0.9
#endif
#endif /* EXF_USE_OLD_INTERP_POLE */
         ENDIF
        ENDIF
       ENDDO
      ENDIF

#ifdef ALLOW_DEBUG
      debugFlag = ( exf_debugLev.GE.debLevC )
     &       .OR. ( exf_debugLev.GE.debLevB .AND. myIter.LE.nIter0 )
C     prtPole(l)=0 : extended, =1 : changed to pole, =2 : changed to symetric
      IF ( debugFlag ) THEN
        l = ILNBLNK(inFile)
        _BEGIN_MASTER(myThid)
        WRITE(msgBuf,'(3A,I6,A,2L5)')
     &   'EXF_INTERP: file="',inFile(1:l),'", rec=', irecord,
     &   ' , x-Per,P.Sym=', xIsPeriodic, poleSymmetry
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A,3F4.1,A,3F12.6)') 'S.edge (j=-1,0,1) :',
     &   ' proc=', (prtPole(j),j=-1,1), ', yIn=', (y_in(j),j=-1,1)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A,3F4.1,A,3F12.6)') 'N.edge (j=+0,+1,+2)',
     &   ' proc=', (prtPole(j),j=2,4), ', yIn=',(y_in(j),j=nyIn,nyIn+2)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        _END_MASTER(myThid)
      ENDIF
#endif /* ALLOW_DEBUG */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  Prepare output grid and interpolate for each tile

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
#ifdef ALLOW_DEBUG
C--   Check validity of input/output coordinates
        IF ( debugFlag ) THEN
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
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--   Compute interpolation lon & lat index mapping
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
        IF ( debugFlag ) THEN
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

C--   Do interpolation using lon & lat index mapping
        CALL EXF_INTERPOLATE(
     I                inFile, irecord, method,
     I                nxIn, nyIn, x_in, y_in,
     O                arrayin,
     O                arrayout,
     I                xG, yG,
     I                w_ind, s_ind,
     I                bi, bj, myThid )

       ENDDO
      ENDDO

      RETURN
      END
