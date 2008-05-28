C $Header: /u/gcmpack/MITgcm/model/src/ini_curvilinear_grid.F,v 1.36 2008/05/28 03:02:43 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_CURVILINEAR_GRID
C     !INTERFACE:
      SUBROUTINE INI_CURVILINEAR_GRID( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_CURVILINEAR_GRID
C     | o Initialise curvilinear coordinate system
C     *==========================================================*
C     | Curvilinear grid settings are read from a file rather
C     | than coded in-line as for cartesian and spherical polar.
C     | This is more general but you have to create the grid
C     | yourself.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_CURVILINEAR_GRID
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj
      INTEGER i,j
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL anglesAreSet
#ifdef ALLOW_MNC
      CHARACTER*(80) mncFn
#endif
#ifndef OLD_GRID_IO
      INTEGER fp
      INTEGER myTile
      INTEGER iG, iL, iLen
      CHARACTER*(MAX_LEN_FNAM) fName
      CHARACTER*(MAX_LEN_MBUF) tmpBuf
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif
CEOP

C--   Set everything to zero everywhere
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          xC(i,j,bi,bj)=0.
          yC(i,j,bi,bj)=0.
          xG(i,j,bi,bj)=0.
          yG(i,j,bi,bj)=0.
          dxC(i,j,bi,bj)=0.
          dyC(i,j,bi,bj)=0.
          dxG(i,j,bi,bj)=0.
          dyG(i,j,bi,bj)=0.
          dxF(i,j,bi,bj)=0.
          dyF(i,j,bi,bj)=0.
          dxV(i,j,bi,bj)=0.
          dyU(i,j,bi,bj)=0.
          rA(i,j,bi,bj)=0.
          rAz(i,j,bi,bj)=0.
          rAw(i,j,bi,bj)=0.
          rAs(i,j,bi,bj)=0.
          tanPhiAtU(i,j,bi,bj)=0.
          tanPhiAtV(i,j,bi,bj)=0.
          angleCosC(i,j,bi,bj)=1.
          angleSinC(i,j,bi,bj)=0.
          cosFacU(j,bi,bj)=1.
          cosFacV(j,bi,bj)=1.
          sqCosFacU(j,bi,bj)=1.
          sqCosFacV(j,bi,bj)=1.
         ENDDO
        ENDDO

       ENDDO
      ENDDO

C--   Everyone must wait for the initialisation to be done
      _BARRIER

C     Here we make no assumptions about grid symmetry and simply
C     read the raw grid data from files

#ifdef OLD_GRID_IO

C-    Cell centered quantities
      CALL MDSREADFIELD('LONC.bin',readBinaryPrec,'RS',1,xC,  1,myThid)
      CALL MDSREADFIELD('LATC.bin',readBinaryPrec,'RS',1,yC,  1,myThid)
      _EXCH_XY_R4(xC,myThid)
      _EXCH_XY_R4(yC,myThid)

      CALL MDSREADFIELD('DXF.bin',readBinaryPrec,'RS',1,dxF,  1,myThid)
      CALL MDSREADFIELD('DYF.bin',readBinaryPrec,'RS',1,dyF,  1,myThid)
      CALL EXCH_UV_AGRID_3D_RS( dxF, dyF, .FALSE., 1, myThid )

      CALL MDSREADFIELD('RA.bin',readBinaryPrec,'RS',1,rA,  1,myThid)
      _EXCH_XY_R4(rA,myThid )

C-    Corner quantities
      CALL MDSREADFIELD('LONG.bin',readBinaryPrec,'RS',1,xG,  1,myThid)
      CALL MDSREADFIELD('LATG.bin',readBinaryPrec,'RS',1,yG,  1,myThid)
      IF (useCubedSphereExchange) THEN
cs-   this block needed by cubed sphere until we write more useful I/O routines
      bi=3
      bj=1
      yG(1,sNy+1,bj,1)=yG(1,1,bi,1)
      bj=bj+2
      yG(1,sNy+1,bj,1)=yG(1,1,bi,1)
      bj=bj+2
      yG(1,sNy+1,bj,1)=yG(1,1,bi,1)
      bi=6
      bj=2
      yG(sNx+1,1,bj,1)=yG(1,1,bi,1)
      bj=bj+2
      yG(sNx+1,1,bj,1)=yG(1,1,bi,1)
      bj=bj+2
      yG(sNx+1,1,bj,1)=yG(1,1,bi,1)
cs-   end block
      ENDIF
      CALL EXCH_Z_3D_RS( xG, 1, myThid )
      CALL EXCH_Z_3D_RS( yG, 1, myThid )

      CALL MDSREADFIELD('DXV.bin',readBinaryPrec,'RS',1,dxV,  1,myThid)
      CALL MDSREADFIELD('DYU.bin',readBinaryPrec,'RS',1,dyU,  1,myThid)
c     CALL EXCH_UV_BGRID_3D_RS( dxV, dyU, .FALSE., 1, myThid)
cs-   this block needed by cubed sphere until we write more useful I/O routines
      IF (.NOT.useCubedSphereExchange) THEN
      CALL EXCH_Z_3D_RS( dxV, 1, myThid )
      CALL EXCH_Z_3D_RS( dyU, 1, myThid )
      ELSE
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
cs! fix overlaps:
        DO j=1,sNy
         DO i=1,Olx
          dxV(1-i,j,bi,bj)=dxV(1+i,j,bi,bj)
          dxV(sNx+i,j,bi,bj)=dxV(i,j,bi,bj)
          dyU(1-i,j,bi,bj)=dyU(1+i,j,bi,bj)
          dyU(sNx+i,j,bi,bj)=dyU(i,j,bi,bj)
         ENDDO
        ENDDO
        DO j=1,Oly
         DO i=1-Olx,sNx+Olx
          dxV(i,1-j,bi,bj)=dxV(i,1+j,bi,bj)
          dxV(i,sNy+j,bi,bj)=dxV(i,j,bi,bj)
          dyU(i,1-j,bi,bj)=dyU(i,1+j,bi,bj)
          dyU(i,sNy+j,bi,bj)=dyU(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
cs-   end block
      ENDIF

      CALL MDSREADFIELD('RAZ.bin',readBinaryPrec,'RS',1,rAz,  1,myThid)
      IF (useCubedSphereExchange) THEN
cs-   this block needed by cubed sphere until we write more useful I/O routines
      CALL EXCH_Z_3D_RS( rAz, 1, myThid )
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        rAz(sNx+1,1,bi,bj)=rAz(1,1,bi,bj)
        rAz(1,sNy+1,bi,bj)=rAz(1,1,bi,bj)
       ENDDO
      ENDDO
cs-   end block
      ENDIF
      CALL EXCH_Z_3D_RS( rAz, 1, myThid )

C-    Staggered (u,v pairs) quantities
      CALL MDSREADFIELD('DXC.bin',readBinaryPrec,'RS',1,dxC,  1,myThid)
      CALL MDSREADFIELD('DYC.bin',readBinaryPrec,'RS',1,dyC,  1,myThid)
      CALL EXCH_UV_XY_RS(dxC,dyC,.FALSE.,myThid)

      CALL MDSREADFIELD('RAW.bin',readBinaryPrec,'RS',1,rAw,  1,myThid)
      CALL MDSREADFIELD('RAS.bin',readBinaryPrec,'RS',1,rAs,  1,myThid)
      CALL EXCH_UV_XY_RS(rAw,rAs,.FALSE.,myThid)

      CALL MDSREADFIELD('DXG.bin',readBinaryPrec,'RS',1,dxG,  1,myThid)
      CALL MDSREADFIELD('DYG.bin',readBinaryPrec,'RS',1,dyG,  1,myThid)
      CALL EXCH_UV_XY_RS(dyG,dxG,.FALSE.,myThid)
      CALL EXCH_UV_AGRID_3D_RS(angleSinC,angleCosC,.TRUE., 1, myThid)
      anglesAreSet = .FALSE.

c     write(10) xC
c     write(10) yC
c     write(10) dxF
c     write(10) dyF
c     write(10) rA
c     write(10) xG
c     write(10) yG
c     write(10) dxV
c     write(10) dyU
c     write(10) rAz
c     write(10) dxC
c     write(10) dyC
c     write(10) rAw
c     write(10) rAs
c     write(10) dxG
c     write(10) dyG

#else /* ifndef OLD_GRID_IO */

C--   Only do I/O if I am the master thread
      _BEGIN_MASTER(myThid)

#ifdef ALLOW_MNC
      IF (useMNC .AND. readgrid_mnc) THEN
C--   read NetCDF files:

        DO i = 1,80
          mncFn(i:i) = ' '
        ENDDO
        write(mncFn,'(a)') 'mitgrid'
        DO i = 1,MAX_LEN_MBUF
          msgBuf(i:i) = ' '
        ENDDO
        WRITE(msgBuf,'(2A)') msgBuf,' ; Reading grid info using MNC'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
        CALL MNC_FILE_CLOSE_ALL_MATCHING(mncFn, myThid)
        CALL MNC_CW_SET_UDIM(mncFn, 1, myThid)
        CALL MNC_CW_SET_CITER(mncFn, 2, -1, -1, -1, myThid)
        CALL MNC_CW_SET_UDIM(mncFn, 1, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'XC', xC,  myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'XG', xG,  myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'YC', yC,  myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'YG', yG,  myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dxC',dxC, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dyC',dyC, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dxF',dxF, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dyF',dyF, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dxG',dxG, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dyG',dyG, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dxV',dxV, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dyU',dyU, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'rA', rA,  myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'rAz',rAz, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'rAw',rAw, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'rAs',rAs, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'AngleCS',angleCosC,myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'AngleSN',angleSinC,myThid)
        anglesAreSet = .TRUE.

      ELSE
C--   read Binary files:
#endif /* ALLOW_MNC */

C--   File Precision: keep 64-bits precision (as it used to be)
C     but should probably change it to the standard file-prec (= readBinaryPrec)
      fp = precFloat64
c     fp = readBinaryPrec

      DO bj = 1,nSy
       DO bi = 1,nSx
        iG = bi+(myXGlobalLo-1)/sNx
        myTile = iG
#ifdef ALLOW_EXCH2
        myTile = W2_myTileList(bi)
        iG = exch2_myface(myTile)
#endif
        WRITE(tmpBuf,'(A,I4)') 'tile:',myTile

        iLen = ILNBLNK(horizGridFile)
        IF ( iLen .EQ. 0 ) THEN
          WRITE(fName,'("tile",I3.3,".mitgrid")') iG
        ELSE
          WRITE(fName,'(2A,I3.3,A)') horizGridFile(1:iLen),
     &                              '.face',iG,'.bin'
        ENDIF
        iLen = ILNBLNK(fName)
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(3A)') tmpBuf(1:iL),
     &                   ' ; Read from file ',fName(1:iLen)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(A)') '  =>'

#ifdef ALLOW_MDSIO
        CALL MDS_FACEF_READ_RS( fName, fp, 1,  xC, bi, bj, myThid )
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'xC'
        CALL MDS_FACEF_READ_RS( fName, fp, 2,  yC, bi, bj, myThid )
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'yC'
        CALL MDS_FACEF_READ_RS( fName, fp, 3, dxF, bi, bj, myThid )
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'dxF'
        CALL MDS_FACEF_READ_RS( fName, fp, 4, dyF, bi, bj, myThid )
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'dyF'
        CALL MDS_FACEF_READ_RS( fName, fp, 5,  rA, bi, bj, myThid )
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'rA'
        CALL MDS_FACEF_READ_RS( fName, fp, 6,  xG, bi, bj, myThid )
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'xG'
        CALL MDS_FACEF_READ_RS( fName, fp, 7,  yG, bi, bj, myThid )
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'yG'
        CALL MDS_FACEF_READ_RS( fName, fp, 8, dxV, bi, bj, myThid )
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'dxV'
        CALL MDS_FACEF_READ_RS( fName, fp, 9, dyU, bi, bj, myThid )
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'dyU'
        CALL MDS_FACEF_READ_RS( fName, fp,10, rAz, bi, bj, myThid )
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'rAz'
        CALL MDS_FACEF_READ_RS( fName, fp,11, dxC, bi, bj, myThid )
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'dxC'
        CALL MDS_FACEF_READ_RS( fName, fp,12, dyC, bi, bj, myThid )
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'dyC'
        CALL MDS_FACEF_READ_RS( fName, fp,13, rAw, bi, bj, myThid )
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'rAw'
        CALL MDS_FACEF_READ_RS( fName, fp,14, rAs, bi, bj, myThid )
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'rAs'
        CALL MDS_FACEF_READ_RS( fName, fp,15, dxG, bi, bj, myThid )
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'dxG'
        CALL MDS_FACEF_READ_RS( fName, fp,16, dyG, bi, bj, myThid )
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'dyG'

        iLen = ILNBLNK(horizGridFile)
        IF ( iLen.GT.0 ) THEN
         CALL MDS_FACEF_READ_RS(fName,fp,17,angleCosC,bi,bj,myThid)
         iL = ILNBLNK(msgBuf)
         WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'AngleCS'
         CALL MDS_FACEF_READ_RS(fName,fp,18,angleSinC,bi,bj,myThid)
         iL = ILNBLNK(tmpBuf)
         WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'AngleSN'
         anglesAreSet = .TRUE.
        ELSE
         anglesAreSet = .FALSE.
        ENDIF
#else /* ALLOW_MDSIO */
        WRITE(msgBuf,'(2A)')
     &   'INI_CURVILINEAR_GRID: Needs to compile MDSIO pkg'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_CURVILINEAR_GRID'
#endif /* ALLOW_MDSIO */

        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)

       ENDDO
      ENDDO

#ifdef ALLOW_MNC
      ENDIF
#endif /* ALLOW_MNC */

      _END_MASTER(myThid)

      CALL EXCH_XY_RS(xC,myThid)
      CALL EXCH_XY_RS(yC,myThid)
      CALL EXCH_UV_AGRID_3D_RS( dxF, dyF, .FALSE., 1, myThid )
      CALL EXCH_XY_RS(rA,myThid )
      CALL EXCH_Z_3D_RS( xG, 1, myThid )
      CALL EXCH_Z_3D_RS( yG, 1, myThid )
#ifdef ALLOW_EXCH2
# ifndef ALLOW_AUTODIFF_TAMC
      CALL EXCH2_UV_BGRID_3D_RS( dxV, dyU, .FALSE., 1, myThid)
# endif
#endif
      CALL EXCH_Z_3D_RS( rAz, 1, myThid )
      CALL EXCH_UV_XY_RS(dxC,dyC,.FALSE.,myThid)
      CALL EXCH_UV_XY_RS(rAw,rAs,.FALSE.,myThid)
      CALL EXCH_UV_XY_RS(dyG,dxG,.FALSE.,myThid)
      CALL EXCH_UV_AGRID_3D_RS(angleSinC,angleCosC,.TRUE., 1, myThid)

#endif /* OLD_GRID_IO */

C--   Stop if Angle have not been loaded but are needed :
      _BEGIN_MASTER(myThid)
      IF ( .NOT.anglesAreSet .AND. use3dCoriolis ) THEN
        WRITE(msgBuf,'(2A)')
     &   'INI_CURVILINEAR_GRID: Angle of CurvilinearGrid not set',
     &   ' but needed for 3-D Coriolis'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_CURVILINEAR_GRID'
      ENDIF
      _END_MASTER(myThid)

c     CALL WRITE_FULLARRAY_RL('dxV',dxV,1,0,0,0,myThid)
c     CALL WRITE_FULLARRAY_RL('dyU',dyU,1,0,0,0,myThid)
c     CALL WRITE_FULLARRAY_RL('rAz',rAz,1,0,0,0,myThid)
c     CALL WRITE_FULLARRAY_RL('xG',xG,1,0,0,0,myThid)
c     CALL WRITE_FULLARRAY_RL('yG',yG,1,0,0,0,myThid)

C--   Now let's look at all these beasts
      IF ( debugLevel .GE. debLevB ) THEN
        CALL PLOT_FIELD_XYRS( xC      , 'Current xC      ', 0, myThid )
        CALL PLOT_FIELD_XYRS( yC      , 'Current yC      ', 0, myThid )
        CALL PLOT_FIELD_XYRS( dxF     , 'Current dxF     ', 0, myThid )
        CALL PLOT_FIELD_XYRS( dyF     , 'Current dyF     ', 0, myThid )
        CALL PLOT_FIELD_XYRS( rA      , 'Current rA      ', 0, myThid )
        CALL PLOT_FIELD_XYRS( xG      , 'Current xG      ', 0, myThid )
        CALL PLOT_FIELD_XYRS( yG      , 'Current yG      ', 0, myThid )
        CALL PLOT_FIELD_XYRS( dxV     , 'Current dxV     ', 0, myThid )
        CALL PLOT_FIELD_XYRS( dyU     , 'Current dyU     ', 0, myThid )
        CALL PLOT_FIELD_XYRS( rAz     , 'Current rAz     ', 0, myThid )
        CALL PLOT_FIELD_XYRS( dxC     , 'Current dxC     ', 0, myThid )
        CALL PLOT_FIELD_XYRS( dyC     , 'Current dyC     ', 0, myThid )
        CALL PLOT_FIELD_XYRS( rAw     , 'Current rAw     ', 0, myThid )
        CALL PLOT_FIELD_XYRS( rAs     , 'Current rAs     ', 0, myThid )
        CALL PLOT_FIELD_XYRS( dxG     , 'Current dxG     ', 0, myThid )
        CALL PLOT_FIELD_XYRS( dyG     , 'Current dyG     ', 0, myThid )
        CALL PLOT_FIELD_XYRS(angleCosC, 'Current AngleCS ', 0, myThid )
        CALL PLOT_FIELD_XYRS(angleSinC, 'Current AngleSN ', 0, myThid )
      ENDIF

      RETURN
      END
