C $Header: /u/gcmpack/MITgcm/model/src/ini_curvilinear_grid.F,v 1.25 2005/11/05 01:00:57 jmc Exp $
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

#ifndef ALLOW_EXCH2
C- note: default is to use "new" grid files (OLD_GRID_IO undef) with EXCH2
C    but can still use (on 1 cpu) OLD_GRID_IO and EXCH2 independently
#ifdef ALLOW_MDSIO
#define OLD_GRID_IO
#endif
#endif /* ALLOW_EXCH2 */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_CURVILINEAR_GRID
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj, myIter
      INTEGER I,J
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_MNC
      CHARACTER*(80) mncFn
#endif
#ifndef OLD_GRID_IO
# ifdef ALLOW_EXCH2
      _RL buf(sNx*nSx*nPx+1)
      INTEGER myTile
# else
      _RL buf(sNx+1,sNy+1)
# endif
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

        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          XC(i,j,bi,bj)=0.
          YC(i,j,bi,bj)=0.
          XG(i,j,bi,bj)=0.
          YG(i,j,bi,bj)=0.
          DXC(i,j,bi,bj)=0.
          DYC(i,j,bi,bj)=0.
          DXG(i,j,bi,bj)=0.
          DYG(i,j,bi,bj)=0.
          DXF(i,j,bi,bj)=0.
          DYF(i,j,bi,bj)=0.
          DXV(i,j,bi,bj)=0.
          DYU(i,j,bi,bj)=0.
          RA(i,j,bi,bj)=0.
          RAZ(i,j,bi,bj)=0.
          RAW(i,j,bi,bj)=0.
          RAS(i,j,bi,bj)=0.
          tanPhiAtU(i,j,bi,bj)=0.
          tanPhiAtV(i,j,bi,bj)=0.
          angleCosC(i,j,bi,bj)=1.
          angleSinC(i,j,bi,bj)=0.
          cosFacU(J,bi,bj)=1.
          cosFacV(J,bi,bj)=1.
          sqcosFacU(J,bi,bj)=1.
          sqcosFacV(J,bi,bj)=1.
         ENDDO
        ENDDO

       ENDDO
      ENDDO


#ifdef ALLOW_MNC
      IF (useMNC .AND. readgrid_mnc) THEN

        _BEGIN_MASTER(myThid)
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
        CALL MNC_CW_RS_R('D',mncFn,0,0,'XC', XC,  myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'XG', XG,  myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'YC', YC,  myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'YG', YG,  myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dxC',DXC, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dyC',DYC, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dxF',DXF, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dyF',DYF, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dxG',DXG, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dyG',DYG, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dxV',DXV, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'dyU',DYU, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'rA', RA,  myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'rAz',RAZ, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'rAw',RAW, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'rAs',RAS, myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'AngleCS',angleCosC,myThid)
        CALL MNC_CW_RS_R('D',mncFn,0,0,'AngleSN',angleSinC,myThid)

        _END_MASTER(myThid)

        CALL EXCH_XY_RS(XC,myThid)
        CALL EXCH_XY_RS(YC,myThid)
#ifdef HRCUBE
        CALL EXCH_XY_RS(DXF,myThid)
        CALL EXCH_XY_RS(DYF,myThid)
#endif
        CALL EXCH_XY_RS(RA,myThid )
        CALL EXCH_Z_XY_RS(XG,myThid)
        CALL EXCH_Z_XY_RS(YG,myThid)
        CALL EXCH_Z_XY_RS(RAZ,myThid)
        CALL EXCH_UV_XY_RS(DXC,DYC,.FALSE.,myThid)
        CALL EXCH_UV_XY_RS(RAW,RAS,.FALSE.,myThid)
        CALL EXCH_UV_XY_RS(DYG,DXG,.FALSE.,myThid)
        CALL EXCH_UV_AGRID_XY_RS(angleSinC,angleCosC,.TRUE.,myThid)

      ELSE
#endif

C     Here we make no assumptions about grid symmetry and simply
C     read the raw grid data from files

#ifdef OLD_GRID_IO

C-    Cell centered quantities
      CALL MDSREADFIELD('LONC.bin',readBinaryPrec,'RS',1,XC,  1,myThid)
      CALL MDSREADFIELD('LATC.bin',readBinaryPrec,'RS',1,YC,  1,myThid)
      _EXCH_XY_R4(XC,myThid)
      _EXCH_XY_R4(YC,myThid)

      CALL MDSREADFIELD('DXF.bin',readBinaryPrec,'RS',1,DXF,  1,myThid)
      CALL MDSREADFIELD('DYF.bin',readBinaryPrec,'RS',1,DYF,  1,myThid)
C !!! _EXCH_OUV_XY_R4(DXF, DYF, unSigned, myThid )
cs!   this is not correct! <= need paired exchange for DXF,DYF
      _EXCH_XY_R4(DXF,myThid)
      _EXCH_XY_R4(DYF,myThid)
      IF (useCubedSphereExchange) THEN
cs! fix overlaps:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,Olx
          DXF(1-i,j,bi,bj)=DXF(i,j,bi,bj)
          DXF(sNx+i,j,bi,bj)=DXF(sNx+1-i,j,bi,bj)
          DYF(1-i,j,bi,bj)=DYF(i,j,bi,bj)
          DYF(sNx+i,j,bi,bj)=DYF(sNx+1-i,j,bi,bj)
         ENDDO
        ENDDO
        DO j=1,Oly
         DO i=1,sNx
          DXF(i,1-j,bi,bj)=DXF(i,j,bi,bj)
          DXF(i,sNy+j,bi,bj)=DXF(i,sNy+1-j,bi,bj)
          DYF(i,1-j,bi,bj)=DYF(i,j,bi,bj)
          DYF(i,sNy+j,bi,bj)=DYF(i,sNy+1-j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ENDIF
cs

      CALL MDSREADFIELD('RA.bin',readBinaryPrec,'RS',1,RA,  1,myThid)
      _EXCH_XY_R4(RA,myThid )

C-    Corner quantities
C       *********** this are not degbugged ************
      CALL MDSREADFIELD('LONG.bin',readBinaryPrec,'RS',1,XG,  1,myThid)
      CALL MDSREADFIELD('LATG.bin',readBinaryPrec,'RS',1,YG,  1,myThid)
      IF (useCubedSphereExchange) THEN
cs-   this block needed by cubed sphere until we write more useful I/O routines
      bi=3
      bj=1
      YG(1,sNy+1,bj,1)=YG(1,1,bi,1)
      bj=bj+2
      YG(1,sNy+1,bj,1)=YG(1,1,bi,1)
      bj=bj+2
      YG(1,sNy+1,bj,1)=YG(1,1,bi,1)
      bi=6
      bj=2
      YG(sNx+1,1,bj,1)=YG(1,1,bi,1)
      bj=bj+2
      YG(sNx+1,1,bj,1)=YG(1,1,bi,1)
      bj=bj+2
      YG(sNx+1,1,bj,1)=YG(1,1,bi,1)
cs-   end block
      ENDIF
      CALL EXCH_Z_XY_RS(XG,myThid)
      CALL EXCH_Z_XY_RS(YG,myThid)

      CALL MDSREADFIELD('DXV.bin',readBinaryPrec,'RS',1,DXV,  1,myThid)
      CALL MDSREADFIELD('DYU.bin',readBinaryPrec,'RS',1,DYU,  1,myThid)
cs-   this block needed by cubed sphere until we write more useful I/O routines
C !!! _EXCH_ZUV_XY_R4(DXV, DYU, unSigned, myThid)
cs!   this is not correct <= need paired exchange for dxv,dyu
      IF (.NOT.useCubedSphereExchange) THEN
      CALL EXCH_Z_XY_RS(DXV,myThid)
      CALL EXCH_Z_XY_RS(DYU,myThid)
      ELSE
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
cs! fix overlaps:
        DO j=1,sNy
         DO i=1,Olx
          DXV(1-i,j,bi,bj)=DXV(1+i,j,bi,bj)
          DXV(sNx+i,j,bi,bj)=DXV(i,j,bi,bj)
          DYU(1-i,j,bi,bj)=DYU(1+i,j,bi,bj)
          DYU(sNx+i,j,bi,bj)=DYU(i,j,bi,bj)
         ENDDO
        ENDDO
        DO j=1,Oly
         DO i=1-Olx,sNx+Olx
          DXV(i,1-j,bi,bj)=DXV(i,1+j,bi,bj)
          DXV(i,sNy+j,bi,bj)=DXV(i,j,bi,bj)
          DYU(i,1-j,bi,bj)=DYU(i,1+j,bi,bj)
          DYU(i,sNy+j,bi,bj)=DYU(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
cs-   end block
      ENDIF

      CALL MDSREADFIELD('RAZ.bin',readBinaryPrec,'RS',1,RAZ,  1,myThid)
      IF (useCubedSphereExchange) THEN
cs-   this block needed by cubed sphere until we write more useful I/O routines
      CALL EXCH_Z_XY_RS(RAZ , myThid )
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        RAZ(sNx+1,1,bi,bj)=RAZ(1,1,bi,bj)
        RAZ(1,sNy+1,bi,bj)=RAZ(1,1,bi,bj)
       ENDDO
      ENDDO
cs-   end block
      ENDIF
      CALL EXCH_Z_XY_RS(RAZ,myThid)

C-    Staggered (u,v pairs) quantities
      CALL MDSREADFIELD('DXC.bin',readBinaryPrec,'RS',1,DXC,  1,myThid)
      CALL MDSREADFIELD('DYC.bin',readBinaryPrec,'RS',1,DYC,  1,myThid)
      CALL EXCH_UV_XY_RS(DXC,DYC,.FALSE.,myThid)

      CALL MDSREADFIELD('RAW.bin',readBinaryPrec,'RS',1,RAW,  1,myThid)
      CALL MDSREADFIELD('RAS.bin',readBinaryPrec,'RS',1,RAS,  1,myThid)
      IF (useCubedSphereExchange) THEN
cs-   this block needed by cubed sphere until we write more useful I/O routines
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1,sNy
c        RAW(sNx+1,J,bi,bj)=RAW(1,J,bi,bj)
c        RAS(J,sNy+1,bi,bj)=RAS(J,1,bi,bj)
        ENDDO
       ENDDO
      ENDDO
cs-   end block
      ENDIF
      CALL EXCH_UV_XY_RS(RAW,RAS,.FALSE.,myThid)

      CALL MDSREADFIELD('DXG.bin',readBinaryPrec,'RS',1,DXG,  1,myThid)
      CALL MDSREADFIELD('DYG.bin',readBinaryPrec,'RS',1,DYG,  1,myThid)
      IF (useCubedSphereExchange) THEN
cs-   this block needed by cubed sphere until we write more useful I/O routines
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1,sNy
c        DYG(sNx+1,J,bi,bj)=DYG(1,J,bi,bj)
c        DXG(J,sNy+1,bi,bj)=DXG(J,1,bi,bj)
        ENDDO
       ENDDO
      ENDDO
cs-   end block
      ENDIF
      CALL EXCH_UV_XY_RS(DYG,DXG,.FALSE.,myThid)
      CALL EXCH_UV_AGRID_XY_RS(angleSinC,angleCosC,.TRUE.,myThid)

c     write(10) XC
c     write(10) YC
c     write(10) DXF
c     write(10) DYF
c     write(10) RA
c     write(10) XG
c     write(10) YG
c     write(10) DXV
c     write(10) DYU
c     write(10) RAZ
c     write(10) DXC
c     write(10) DYC
c     write(10) RAW
c     write(10) RAS
c     write(10) DXG
c     write(10) DYG

#else /* ifndef OLD_GRID_IO */

C--   Only do I/O if I am the master thread
      _BEGIN_MASTER(myThid)

      DO bj = 1,nSy
       DO bi = 1,nSx
        iG=bi+(myXGlobalLo-1)/sNx
        WRITE(tmpBuf,'(A,I4)') 'tile:',iG
#ifdef ALLOW_EXCH2
        myTile = W2_myTileList(bi)
        WRITE(tmpBuf,'(A,I4)') 'tile:',myTile
        iG = exch2_myface(myTile)
#endif
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

        CALL READSYMTILE_RS(fName,1,XC,bi,bj,buf,myThid)
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'XC'
        CALL READSYMTILE_RS(fName,2,YC,bi,bj,buf,myThid)
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'YC'
        CALL READSYMTILE_RS(fName,3,DXF,bi,bj,buf,myThid)
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'DXF'
        CALL READSYMTILE_RS(fName,4,DYF,bi,bj,buf,myThid)
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'DYF'
        CALL READSYMTILE_RS(fName,5,RA,bi,bj,buf,myThid)
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'RA'
        CALL READSYMTILE_RS(fName,6,XG,bi,bj,buf,myThid)
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'XG'
        CALL READSYMTILE_RS(fName,7,YG,bi,bj,buf,myThid)
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'YG'
        CALL READSYMTILE_RS(fName,8,DXV,bi,bj,buf,myThid)
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'DXV'
        CALL READSYMTILE_RS(fName,9,DYU,bi,bj,buf,myThid)
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'DYU'
        CALL READSYMTILE_RS(fName,10,RAZ,bi,bj,buf,myThid)
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'RAZ'
        CALL READSYMTILE_RS(fName,11,DXC,bi,bj,buf,myThid)
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'DXC'
        CALL READSYMTILE_RS(fName,12,DYC,bi,bj,buf,myThid)
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'DYC'
        CALL READSYMTILE_RS(fName,13,RAW,bi,bj,buf,myThid)
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'RAW'
        CALL READSYMTILE_RS(fName,14,RAS,bi,bj,buf,myThid)
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'RAS'
        CALL READSYMTILE_RS(fName,15,DXG,bi,bj,buf,myThid)
        iL = ILNBLNK(msgBuf)
        WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'DXG'
        CALL READSYMTILE_RS(fName,16,DYG,bi,bj,buf,myThid)
        iL = ILNBLNK(tmpBuf)
        WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'DYG'

        iLen = ILNBLNK(horizGridFile)
        IF ( iLen.GT.0 ) THEN
         CALL READSYMTILE_RS(fName,17,angleCosC,bi,bj,buf,myThid)
         iL = ILNBLNK(msgBuf)
         WRITE(tmpBuf,'(A,1X,A)') msgBuf(1:iL),'AngleCS'
         CALL READSYMTILE_RS(fName,18,angleSinC,bi,bj,buf,myThid)
         iL = ILNBLNK(tmpBuf)
         WRITE(msgBuf,'(A,1X,A)') tmpBuf(1:iL),'AngleSN'
        ENDIF

        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)

       ENDDO
      ENDDO

      _END_MASTER(myThid)

      CALL EXCH_XY_RS(XC,myThid)
      CALL EXCH_XY_RS(YC,myThid)
C !!! _EXCH_OUV_XY_R4(DXF, DYF, unSigned, myThid )
#ifdef HRCUBE
      CALL EXCH_XY_RS(DXF,myThid)
      CALL EXCH_XY_RS(DYF,myThid)
#endif
      CALL EXCH_XY_RS(RA,myThid )
      CALL EXCH_Z_XY_RS(XG,myThid)
      CALL EXCH_Z_XY_RS(YG,myThid)
C !!! _EXCH_ZUV_XY_R4(DXV, DYU, unSigned, myThid)
c     CALL EXCH_Z_XY_RS(DXV,myThid)
c     CALL EXCH_Z_XY_RS(DYU,myThid)
      CALL EXCH_Z_XY_RS(RAZ,myThid)
      CALL EXCH_UV_XY_RS(DXC,DYC,.FALSE.,myThid)
      CALL EXCH_UV_XY_RS(RAW,RAS,.FALSE.,myThid)
      CALL EXCH_UV_XY_RS(DYG,DXG,.FALSE.,myThid)
      CALL EXCH_UV_AGRID_XY_RS(angleSinC,angleCosC,.TRUE.,myThid)

#endif /* OLD_GRID_IO */

#ifdef ALLOW_MNC
      ENDIF
#endif /* ALLOW_MNC */

c     CALL WRITE_FULLARRAY_RL('DXV',DXV,1,0,0,0,myThid)
c     CALL WRITE_FULLARRAY_RL('DYU',DYU,1,0,0,0,myThid)
c     CALL WRITE_FULLARRAY_RL('RAZ',RAZ,1,0,0,0,myThid)
c     CALL WRITE_FULLARRAY_RL('XG',XG,1,0,0,0,myThid)
c     CALL WRITE_FULLARRAY_RL('YG',YG,1,0,0,0,myThid)

C--   Require that 0 <= longitude < 360 if using exf package
#ifdef ALLOW_EXF
      DO bj = 1,nSy
       DO bi = 1,nSx
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          IF (XC(i,j,bi,bj).lt.0.) XC(i,j,bi,bj)=XC(i,j,bi,bj)+360.
          IF (XG(i,j,bi,bj).lt.0.) XG(i,j,bi,bj)=XG(i,j,bi,bj)+360.
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_EXF */

C--   Now let's look at all these beasts
      IF ( debugLevel .GE. debLevB ) THEN
         myIter = 1
         CALL PLOT_FIELD_XYRL( XC      , 'Current XC      ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( YC      , 'Current YC      ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DXF     , 'Current DXF     ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( XC      , 'Current XC      ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DYF     , 'Current DYF     ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( RA      , 'Current RA      ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( XG      , 'Current XG      ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( YG      , 'Current YG      ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DXV     , 'Current DXV     ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DYU     , 'Current DYU     ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( RAZ     , 'Current RAZ     ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DXC     , 'Current DXC     ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DYC     , 'Current DYC     ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( RAW     , 'Current RAW     ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( RAS     , 'Current RAS     ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DXG     , 'Current DXG     ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL( DYG     , 'Current DYG     ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL(angleCosC, 'Current AngleCS ' ,
     &        myIter, myThid )
         CALL PLOT_FIELD_XYRL(angleSinC, 'Current AngleSN ' ,
     &        myIter, myThid )
      ENDIF

      RETURN
      END

C --------------------------------------------------------------------------

      SUBROUTINE READSYMTILE_RS(fName,irec,array,bi,bj,buf,myThid)
C     /==========================================================\
C     | SUBROUTINE READSYMTILE_RS                                |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */

C     == Routine arguments ==
      CHARACTER*(*) fName
      INTEGER irec
      _RS array(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      INTEGER bi,bj,myThid
#ifdef ALLOW_EXCH2
      _RL buf(1:sNx*nSx*nPx+1)
#else
      _RL buf(1:sNx+1,1:sNy+1)
#endif /* ALLOW_EXCH2 */

C     == Local variables ==
      INTEGER I,J,dUnit, iLen
      INTEGER length_of_rec
      INTEGER MDS_RECLEN
#ifdef ALLOW_EXCH2
      INTEGER TN, dNx, dNy, TBX, TBY, TNX, TNY, II, iBase
#endif
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

      iLen = ILNBLNK(fName)
#ifdef ALLOW_EXCH2
C     Figure out offset of tile within face
      TN  = W2_myTileList(bi)
      dNx = exch2_mydnx(TN)
      dNy = exch2_mydny(TN)
      TBX = exch2_tbasex(TN)
      TBY = exch2_tbasey(TN)
      TNX = exch2_tnx(TN)
      TNY = exch2_tny(TN)

      CALL MDSFINDUNIT( dUnit, myThid )
      length_of_rec=MDS_RECLEN( 64, (dNx+1), myThid )
      OPEN( dUnit, file=fName(1:iLen), status='old',
     &             access='direct', recl=length_of_rec )
      J=0
      iBase=(irec-1)*(dny+1)
      DO I=1+TBY,sNy+1+TBY
       READ(dUnit,rec=I+iBase)(buf(ii),ii=1,dNx+1)
#ifdef _BYTESWAPIO
#ifdef REAL4_IS_SLOW
       CALL MDS_BYTESWAPR8((dNx+1), buf)
#else
       CALL MDS_BYTESWAPR4((dNx+1), buf)
#endif
#endif
       J=J+1
       DO II=1,sNx+1
        array(II,J,bi,bj)=buf(II+TBX)
       ENDDO
      ENDDO
      CLOSE( dUnit )
      
#else /* ALLOW_EXCH2 */

      CALL MDSFINDUNIT( dUnit, myThid )
      length_of_rec=MDS_RECLEN( 64, (sNx+1)*(sNy+1), myThid )
      OPEN( dUnit, file=fName(1:iLen), status='old',
     &             access='direct', recl=length_of_rec )
      READ(dUnit,rec=irec) buf
      CLOSE( dUnit )

#ifdef _BYTESWAPIO
#ifdef REAL4_IS_SLOW
      CALL MDS_BYTESWAPR8((sNx+1)*(sNy+1), buf)
#else
      CALL MDS_BYTESWAPR4((sNx+1)*(sNy+1), buf)
#endif
#endif

      DO J=1,sNy+1
       DO I=1,sNx+1
        array(I,J,bi,bj)=buf(I,J)
       ENDDO
      ENDDO
c       write(0,*) irec,buf(1,1),array(1,1,1,1)

#endif /* ALLOW_EXCH2 */

      RETURN
      END
