C $Header: /u/gcmpack/MITgcm/verification/advect_cs/code/Attic/ini_curvilinear_grid.F,v 1.1 2001/09/28 02:30:09 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE INI_CURVILINEAR_GRID( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_CURVILINEAR_GRID                          |
C     | o Initialise curvilinear coordinate system               |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_CARTESIAN_GRID
      INTEGER myThid

C     == Local variables ==
      INTEGER bi,bj,I,J
      CHARACTER*(12) ff

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
          cosFacU(J,bi,bj)=1.
          cosFacV(J,bi,bj)=1.
          sqcosFacU(J,bi,bj)=1.
          sqcosFacV(J,bi,bj)=1.
         ENDDO
        ENDDO

        CALL READSYMTILE_RS('DXF.bin',DXF,0,0,bi,bj,myThid)
        CALL READSYMTILE_RS('DYF.bin',DYF,0,0,bi,bj,myThid)
        CALL READSYMTILE_RS('RA.bin',RA,0,0,bi,bj,myThid)
        CALL READSYMTILE_RS('DXV.bin',DXV,1,1,bi,bj,myThid)
        CALL READSYMTILE_RS('DYU.bin',DYU,1,1,bi,bj,myThid)
        CALL READSYMTILE_RS('RAZ.bin',RAZ,1,1,bi,bj,myThid)
        CALL READSYMTILE_RS('DXC.bin',DXC,1,0,bi,bj,myThid)
        CALL READSYMTILE_RS('DYC.bin',DYC,0,1,bi,bj,myThid)
        CALL READSYMTILE_RS('RAW.bin',RAW,1,0,bi,bj,myThid)
        CALL READSYMTILE_RS('RAS.bin',RAS,0,1,bi,bj,myThid)
        CALL READSYMTILE_RS('DXG.bin',DXG,0,1,bi,bj,myThid)
        CALL READSYMTILE_RS('DYG.bin',DYG,1,0,bi,bj,myThid)

        write(ff(1:12),'(a,i3.3,a)') 'LONC.',bi+(bj-1)*nSx,'.bin'
        CALL READSYMTILE_RS(ff,XC,0,0,bi,bj,myThid)
        write(ff(1:12),'(a,i3.3,a)') 'LATC.',bi+(bj-1)*nSx,'.bin'
        CALL READSYMTILE_RS(ff,YC,0,0,bi,bj,myThid)
        write(ff(1:12),'(a,i3.3,a)') 'LONG.',bi+(bj-1)*nSx,'.bin'
        CALL READSYMTILE_RS(ff,XG,1,1,bi,bj,myThid)
        write(ff(1:12),'(a,i3.3,a)') 'LATG.',bi+(bj-1)*nSx,'.bin'
        CALL READSYMTILE_RS(ff,YG,1,1,bi,bj,myThid)

       ENDDO ! bi
      ENDDO ! bj

C     Here we make no assumptions about grid symmetry and simply
C     read the raw grid data from files

C-    Cell centered quantities
c     CALL MDSREADFIELD('LONC.bin',readBinaryPrec,'RL',1,XC,  1,myThid)
c     CALL MDSREADFIELD('LATC.bin',readBinaryPrec,'RL',1,YC,  1,myThid)
      _EXCH_XY_R4(XC,myThid)
      _EXCH_XY_R4(YC,myThid)

c     CALL MDSREADFIELD('DXF.bin',readBinaryPrec,'RL',1,DXF,  1,myThid)
c     CALL MDSREADFIELD('DYF.bin',readBinaryPrec,'RL',1,DYF,  1,myThid)
C !!! _EXCH_OUV_XY_R4(DXF, DYF, unSigned, myThid )
cs!   this is not correct! <= need paired exchange for DXF,DYF
      _EXCH_XY_R4(DXF,myThid)
      _EXCH_XY_R4(DYF,myThid)

c     CALL MDSREADFIELD('RA.bin',readBinaryPrec,'RL',1,RA,  1,myThid)
      _EXCH_XY_R4(RA,myThid )

C-    Corner quantities
C       *********** this are not degbugged ************
c     CALL MDSREADFIELD('LONG.bin',readBinaryPrec,'RL',1,XG,  1,myThid)
c     CALL MDSREADFIELD('LATG.bin',readBinaryPrec,'RL',1,YG,  1,myThid)
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
      CALL EXCH_Z_XY_RS(XG,myThid)
      CALL EXCH_Z_XY_RS(YG,myThid)

c     CALL MDSREADFIELD('DXV.bin',readBinaryPrec,'RL',1,DXV,  1,myThid)
c     CALL MDSREADFIELD('DYU.bin',readBinaryPrec,'RL',1,DYU,  1,myThid)
cs-   this block needed by cubed sphere until we write more useful I/O routines
C !!! _EXCH_ZUV_XY_R4(DXV, DYU, unSigned, myThid)
cs!   this is not correct <= need paired exchange for dxv,dyu
      CALL EXCH_Z_XY_RS(DXV,myThid)
      CALL EXCH_Z_XY_RS(DYU,myThid)
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DXV(sNx+1,1,bi,bj)=DXV(1,1,bi,bj)
        DXV(1,sNy+1,bi,bj)=DXV(1,1,bi,bj)
        DYU(sNx+1,1,bi,bj)=DYU(1,1,bi,bj)
        DYU(1,sNy+1,bi,bj)=DYU(1,1,bi,bj)
       ENDDO
      ENDDO
cs-   end block
C !!! _EXCH_ZUV_XY_R4(DXV, DYU, unSigned, myThid)
cs!   this is not correct <= need paired exchange for dxv,dyu
      CALL EXCH_Z_XY_RS(DXV,myThid)
      CALL EXCH_Z_XY_RS(DYU,myThid)

c     CALL MDSREADFIELD('RAZ.bin',readBinaryPrec,'RL',1,RAZ,  1,myThid)
cs-   this block needed by cubed sphere until we write more useful I/O routines
      CALL EXCH_Z_XY_RS(RAZ , myThid )
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        RAZ(sNx+1,1,bi,bj)=RAZ(1,1,bi,bj)
        RAZ(1,sNy+1,bi,bj)=RAZ(1,1,bi,bj)
       ENDDO
      ENDDO
cs-   end block
      CALL EXCH_Z_XY_RS(RAZ,myThid)

C-    Staggered (u,v pairs) quantities
c     CALL MDSREADFIELD('DXC.bin',readBinaryPrec,'RL',1,DXC,  1,myThid)
c     CALL MDSREADFIELD('DYC.bin',readBinaryPrec,'RL',1,DYC,  1,myThid)
      CALL EXCH_UV_XY_RS(DXC,DYC,.FALSE.,myThid)

c     CALL MDSREADFIELD('RAW.bin',readBinaryPrec,'RL',1,RAW,  1,myThid)
c     CALL MDSREADFIELD('RAS.bin',readBinaryPrec,'RL',1,RAS,  1,myThid)
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
      CALL EXCH_UV_XY_RS(RAW,RAS,.FALSE.,myThid)

c     CALL MDSREADFIELD('DXG.bin',readBinaryPrec,'RL',1,DXG,  1,myThid)
c     CALL MDSREADFIELD('DYG.bin',readBinaryPrec,'RL',1,DYG,  1,myThid)
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
      CALL EXCH_UV_XY_RS(DYG,DXG,.FALSE.,myThid)

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

      RETURN
      END

      SUBROUTINE READSYMTILE_RS(fileName,array,Xol,Yol,bi,bj,myThid)
C     /==========================================================\
C     | SUBROUTINE READSYMTILE_RS                                |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
      CHARACTER*(*) fileName
      _RS array(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      INTEGER Xol,Yol,bi,bj,myThid

C     == Local variables ==
      INTEGER I,J

      _BEGIN_MASTER(myThid)
      OPEN(36,FILE=fileName,STATUS='OLD',ACCESS='DIRECT',
#ifdef REAL4_IS_SLOW
     &     RECL=((sNx+Xol)*(sNy+Yol))*WORDLENGTH*2 )
#else
     &     RECL=((sNx+Xol)*(sNy+Yol))*WORDLENGTH )
#endif
      READ(36,REC=1) ((array(I,J,bi,bj),I=1,sNx+Xol),J=1,sNy+Yol)
      CLOSE(36)
#ifdef _BYTESWAPIO
#ifdef REAL4_IS_SLOW
      CALL MDS_BYTESWAPR8((sNx+2*Olx)*(sNy+2*Oly),
     &          array(1-Olx,1-Oly,bi,bj))
#else
      CALL MDS_BYTESWAPR4((sNx+2*Olx)*(sNy+2*Oly),
     &          array(1-Olx,1-Oly,bi,bj))
#endif
#endif

C Avoid broken exchanges
      DO J=1,Oly
       DO I=1,sNx+Xol
        array(I,1-J,bi,bj)=array(I,J+Yol,bi,bj)
       ENDDO
      ENDDO
      DO J=1+Yol,Oly
       DO I=1,sNx+Xol
        array(I,sNy+J,bi,bj)=array(I,sNy+1-J+Yol,bi,bj)
       ENDDO
      ENDDO
      DO J=1,sNy+Yol
c     DO J=1-Oly,sNy+Oly
       DO I=1,Olx
        array(1-I,J,bi,bj)=array(I+Xol,J,bi,bj)
       ENDDO
      ENDDO
      DO J=1,sNy+Yol
c     DO J=1-Oly,sNy+Oly
       DO I=1+Xol,Olx
        array(sNx+I,J,bi,bj)=array(sNy+1-I+Xol,J,bi,bj)
       ENDDO
      ENDDO
      _END_MASTER(myThid)

      RETURN
      END
