C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_interp_uv.F,v 1.2 2014/06/05 15:37:46 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: EXF_INTERP_UV
C     !INTERFACE:
       SUBROUTINE EXF_INTERP_UV(
     I                inFileU, inFileV, filePrec, irecord,
     I                nxIn, nyIn,
     I                lon_0, lon_inc, lat_0, lat_inc,
     O                arrUout, arrVout,
     I                xG_in, yG,
     I                methodU, methodV, myIter, myThid )

C !DESCRIPTION: \bv
C  *==========================================================*
C  | SUBROUTINE EXF_INTERP_UV
C  | o Load from file the 2 vector components of regular
C  |   lat-lon input field and interpolate on to the model
C  |   grid location
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
C  inFileU     (string)  :: input file name for the 1rst component (U)
C  inFileV     (string)  :: input file name for the 2nd  component (V)
C  filePrec    (integer) :: number of bits per word in file (32 or 64)
C  irecord     (integer) :: record number to read
C  nxIn, nyIn  (integer) :: size in x & y direction of input file to read
C   lon_0, lat_0         :: lon and lat of sw corner of global input grid
C   lon_inc              :: scalar x-grid increment
C   lat_inc              :: vector y-grid increments
C  arrUout     ( _RL )   :: 1rst component (U) output array
C  arrVout     ( _RL )   :: 2nd  component (V) output array
C   xG_in,  yG           :: coordinates for output grid to interpolate to
C  methodU, methodV      :: 1,11,21 for bilinear; 2,12,22 for bicubic
C                        :: 1,2 for tracer; 11,12 for U; 21,22 for V
C  myIter      (integer) :: current iteration number
C  myThid      (integer) :: My Thread Id number

      CHARACTER*(*) inFileU
      CHARACTER*(*) inFileV
      INTEGER       filePrec, irecord, nxIn, nyIn
      _RL           arrUout(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL           arrVout(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           xG_in   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           yG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL           lon_0, lon_inc
c     _RL           lat_0, lat_inc(nyIn-1)
      _RL           lat_0, lat_inc(*)
      INTEGER       methodU, methodV, myIter, myThid

C !FUNCTIONS:
#ifdef ALLOW_DEBUG
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif

C !LOCAL VARIABLES:
C     arrUin      :: 1rst component input field array (loaded from file)
C     arrVin      :: 2nd  component input field array (loaded from file)
C     x_in        :: longitude vector defining input field grid
C     y_in        :: latitude  vector defining input field grid
C     w_ind       :: input field longitudinal index, on western side of model grid pt
C     s_ind       :: input field latitudinal index, on southern side of model grid pt
C     bi, bj      :: tile indices
C     i, j, k, l  :: loop indices
C     msgBuf      :: Informational/error message buffer
      _RL      arrUin( -1:nxIn+2, -1:nyIn+2 )
      _RL      arrVin( -1:nxIn+2, -1:nyIn+2 )
      _RL      x_in(-1:nxIn+2), y_in(-1:nyIn+2)
      INTEGER  w_ind(sNx,sNy), s_ind(sNx,sNy)
      INTEGER  bi, bj
      INTEGER  i, j, k, l
      INTEGER  nLoop
      _RL      tmpVar
      _RS      xG(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS      threeSixtyRS
      _RL      yPole, symSign, poleU, poleV
      _RL      csLon(-1:nxIn+2), snLon(-1:nxIn+2)
      _RL      csdLon, sndLon
      LOGICAL  calcLonCS
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
     I         inFileU, filePrec,
     O         arrUin,
     I         irecord, nxIn, nyIn, myThid )
      CALL EXF_INTERP_READ(
     I         inFileV, filePrec,
     O         arrVin,
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
C--   Add 1 row @ the pole (if last row is not) and will fill it
C     with the polarmost-row zonally averaged value.

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

C--   Enlarge boundary
      IF ( xIsPeriodic ) THEN
C-    fill-in added column assuming periodicity
        DO j=1,nyIn
         arrUin( 0,j)     = arrUin(nxIn  ,j)
         arrUin(-1,j)     = arrUin(nxIn-1,j)
         arrUin(nxIn+1,j) = arrUin(1,j)
         arrUin(nxIn+2,j) = arrUin(2,j)
         arrVin( 0,j)     = arrVin(nxIn  ,j)
         arrVin(-1,j)     = arrVin(nxIn-1,j)
         arrVin(nxIn+1,j) = arrVin(1,j)
         arrVin(nxIn+2,j) = arrVin(2,j)
        ENDDO
      ELSE
C-    fill-in added column from nearest column
        DO j=1,nyIn
         arrUin( 0,j)     = arrUin(1,j)
         arrUin(-1,j)     = arrUin(1,j)
         arrUin(nxIn+1,j) = arrUin(nxIn,j)
         arrUin(nxIn+2,j) = arrUin(nxIn,j)
         arrVin( 0,j)     = arrVin(1,j)
         arrVin(-1,j)     = arrVin(1,j)
         arrVin(nxIn+1,j) = arrVin(nxIn,j)
         arrVin(nxIn+2,j) = arrVin(nxIn,j)
        ENDDO
      ENDIF
      symSign = -1. _d 0
      DO l=-1,2
       j = l
       IF ( l.GE.1 ) j = nyIn+l
       k = MAX(1,MIN(j,nyIn))
       IF ( poleSymmetry .AND. ABS(y_in(j)).GT.yPole ) THEN
C-    fill-in added row assuming pole-symmetry
        DO i=-1,nxd2
         arrUin(i,j) = symSign*arrUin(i+nxd2,k)
         arrVin(i,j) = symSign*arrVin(i+nxd2,k)
        ENDDO
        DO i=1,nxd2+2
         arrUin(i+nxd2,j) = symSign*arrUin(i,k)
         arrVin(i+nxd2,j) = symSign*arrVin(i,k)
        ENDDO
#ifdef ALLOW_DEBUG
        i = l + 2*( (l+1)/2 )
        prtPole(i) = prtPole(i) + 0.2
#endif
       ELSE
C-    fill-in added row from nearest column values
        DO i=-1,nxIn+2
         arrUin(i,j) = arrUin(i,k)
         arrVin(i,j) = arrVin(i,k)
        ENDDO
       ENDIF
      ENDDO

C     For vector, replace value at the pole with northernmost/southermost
C     zonal-mean value (poleU,poleV corresponds to Lon=0.E orientation).
      IF ( methodU.GE.10 .AND. methodV.GE.10 ) THEN
       calcLonCS = .TRUE.
       DO l=-1,4
        j = l
        IF ( l.GE.2 ) j = nyIn+l-2
        IF ( ABS(y_in(j)).EQ.yPole ) THEN
          IF ( calcLonCS ) THEN
            csLon(-1) = cos(x_in(-1)*deg2rad)
            snLon(-1) = sin(x_in(-1)*deg2rad)
            csdLon = cos(lon_inc*deg2rad)
            sndLon = sin(lon_inc*deg2rad)
            DO i=-1,nxIn+1
              csLon(i+1) = csLon(i)*csdLon - snLon(i)*sndLon
              snLon(i+1) = csLon(i)*sndLon + snLon(i)*csdLon
            ENDDO
            calcLonCS = .FALSE.
c           write(0,'(a,1P3E13.5)') 'cs 1,nxIn+1,diff=',
c    &           csLon(1),csLon(nxIn+1),csLon(1)-csLon(nxIn+1)
c           write(0,'(a,1P3E13.5)') 'sn 1,nxIn+1,diff=',
c    &           snLon(1),snLon(nxIn+1),snLon(1)-snLon(nxIn+1)
          ENDIF
C     account for local orientation when averaging
          poleU = 0.
          poleV = 0.
          DO i=1,nxIn
            poleU = poleU +csLon(i)*arrUin(i,j) -snLon(i)*arrVin(i,j)
            poleV = poleV +snLon(i)*arrUin(i,j) +csLon(i)*arrVin(i,j)
          ENDDO
          poleU = poleU / nxIn
          poleV = poleV / nxIn
C     put back zonal-mean value but locally orientated
          DO i=-1,nxIn+2
           arrUin(i,j) =  csLon(i)*poleU + snLon(i)*poleV
           arrVin(i,j) = -snLon(i)*poleU + csLon(i)*poleV
          ENDDO
#ifdef ALLOW_DEBUG
          prtPole(l) = prtPole(l) + 0.1
#endif
        ENDIF
       ENDDO
      ENDIF

#ifdef ALLOW_DEBUG
      debugFlag = ( exf_debugLev.GE.debLevC )
     &       .OR. ( exf_debugLev.GE.debLevB .AND. myIter.LE.nIter0 )
C     prtPole(l)=0 : extended, =1 : changed to pole, =2 : changed to symetric
      IF ( debugFlag ) THEN
        l = ILNBLNK(inFileU)
        WRITE(msgBuf,'(3A,I6,A,2L5)')
     &   'EXF_INTERP_UV: fileU="',inFileU(1:l),'", rec=', irecord,
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
            l = ILNBLNK(inFileU)
            WRITE(msgBuf,'(3A,I6)')
     &        'EXF_INTERP_UV: fileU="',inFileU(1:l),'", rec=',irecord
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A)')
     &        'EXF_INTERP_UV: input grid must encompass output grid.'
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
            STOP 'ABNORMAL END: S/R EXF_INTERP_UV'
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
            l = ILNBLNK(inFileU)
            WRITE(msgBuf,'(3A,I4,A,I4)')
     &      'EXF_INTERP_UV: file="', inFileU(1:l), '", rec=', irecord,
     &      ', nLoop=', nLoop
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A)')
     &      'EXF_INTERP_UV: did not found Latitude index for grid-pt:'
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,2I8,2I6,A,1PE16.8)')
     &      'EXF_INTERP_UV: i,j,bi,bj=',i,j,bi,bj,' , yG=',yG(i,j,bi,bj)
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,I8,A,1PE16.8)')
     &      'EXF_INTERP_UV: s_ind=',s_ind(i,j),', lat=',y_in(s_ind(i,j))
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,I8,A,1PE16.8)')
     &      'EXF_INTERP_UV: n_ind=',w_ind(i,j),', lat=',y_in(w_ind(i,j))
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R EXF_INTERP_UV'
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
     I                inFileU, irecord, methodU,
     I                nxIn, nyIn, x_in, y_in,
     O                arrUin,
     O                arrUout,
     I                xG, yG,
     I                w_ind, s_ind,
     I                bi, bj, myThid )
        CALL EXF_INTERPOLATE(
     I                inFileV, irecord, methodV,
     I                nxIn, nyIn, x_in, y_in,
     O                arrVin,
     O                arrVout,
     I                xG, yG,
     I                w_ind, s_ind,
     I                bi, bj, myThid )

       ENDDO
      ENDDO

      RETURN
      END
