C $Header: /u/gcmpack/MITgcm/model/src/ini_curvilinear_grid.F,v 1.5 2001/12/06 16:36:13 adcroft Exp $
C $Name:  $

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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_CARTESIAN_GRID
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj
      INTEGER I,J
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
          cosFacU(J,bi,bj)=1.
          cosFacV(J,bi,bj)=1.
          sqcosFacU(J,bi,bj)=1.
          sqcosFacV(J,bi,bj)=1.
         ENDDO
        ENDDO

       ENDDO ! bi
      ENDDO ! bj

C     Here we make no assumptions about grid symmetry and simply
C     read the raw grid data from files

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
cs

      CALL MDSREADFIELD('RA.bin',readBinaryPrec,'RS',1,RA,  1,myThid)
      _EXCH_XY_R4(RA,myThid )

C-    Corner quantities
C       *********** this are not degbugged ************
      CALL MDSREADFIELD('LONG.bin',readBinaryPrec,'RS',1,XG,  1,myThid)
      CALL MDSREADFIELD('LATG.bin',readBinaryPrec,'RS',1,YG,  1,myThid)
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

      CALL MDSREADFIELD('DXV.bin',readBinaryPrec,'RS',1,DXV,  1,myThid)
      CALL MDSREADFIELD('DYU.bin',readBinaryPrec,'RS',1,DYU,  1,myThid)
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
cs! fix overlaps:
        DO j=1,sNy
         DO i=1,Olx
          DXV(1-i,j,bi,bj)=DXV(1+i,j,bi,bj)
          DXV(sNx+i,j,bi,bj)=DXV(sNx-i,j,bi,bj)
          DYU(1-i,j,bi,bj)=DYU(1+i,j,bi,bj)
          DYU(sNx+i,j,bi,bj)=DYU(sNx-i,j,bi,bj)
         ENDDO
        ENDDO
        DO j=1,Oly
         DO i=1,sNx
          DXV(i,1-j,bi,bj)=DXV(i,1+j,bi,bj)
          DXV(i,sNy+j,bi,bj)=DXV(i,sNy-j,bi,bj)
          DYU(i,1-j,bi,bj)=DYU(i,1+j,bi,bj)
          DYU(i,sNy+j,bi,bj)=DYU(i,sNy-j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
cs-   end block
C !!! _EXCH_ZUV_XY_R4(DXV, DYU, unSigned, myThid)
cs!   this is not correct <= need paired exchange for dxv,dyu
cs    CALL EXCH_Z_XY_RS(DXV,myThid)
cs    CALL EXCH_Z_XY_RS(DYU,myThid)

      CALL MDSREADFIELD('RAZ.bin',readBinaryPrec,'RS',1,RAZ,  1,myThid)
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
      CALL MDSREADFIELD('DXC.bin',readBinaryPrec,'RS',1,DXC,  1,myThid)
      CALL MDSREADFIELD('DYC.bin',readBinaryPrec,'RS',1,DYC,  1,myThid)
      CALL EXCH_UV_XY_RS(DXC,DYC,.FALSE.,myThid)

      CALL MDSREADFIELD('RAW.bin',readBinaryPrec,'RS',1,RAW,  1,myThid)
      CALL MDSREADFIELD('RAS.bin',readBinaryPrec,'RS',1,RAS,  1,myThid)
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

      CALL MDSREADFIELD('DXG.bin',readBinaryPrec,'RS',1,DXG,  1,myThid)
      CALL MDSREADFIELD('DYG.bin',readBinaryPrec,'RS',1,DYG,  1,myThid)
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
