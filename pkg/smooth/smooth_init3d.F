C $Header: /u/gcmpack/MITgcm/pkg/smooth/smooth_init3d.F,v 1.4 2015/01/23 18:58:26 gforget Exp $
C $Name:  $

#include "SMOOTH_OPTIONS.h"

      subroutine smooth_init3D (smoothOpNb,mythid)

C     *==========================================================*
C     | SUBROUTINE smooth_init3D
C     | o Routine that initializes one 3D smoothing/correlation operator
C     |   by computing/writing the corresponding diffusion operator
C     *==========================================================*

cgf the choices of smooth3Dtype and smooth3Dsize need comments...
cgf
cgf smooth3DtypeH= 1) HORIZONTAL ALONG GRID AXIS
cgf              2-3) GMREDI TYPES
cgf                4) HORIZONTAL BUT WITH ROTATED AXIS
cgf for now I focus on the simpler smooth3DtypeH=1 case


      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SMOOTH.h"

      integer i,j,k, bi, bj, imin, imax, jmin, jmax
      integer itlo,ithi
      integer jtlo,jthi
      integer myThid
      character*( 80) fnamegeneric
      _RL smooth3D_KzMax
      integer ii,jj,kk
      integer smoothOpNb

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      smooth3DtotTime=smooth3Dnbt(smoothOpNb)*smooth3DdelTime

            WRITE(standardMessageUnit,'(A,I4,3f12.0)')
     & 'smooth 3D default parameters: ',
     & smooth3Dnbt(smoothOpNb),
     & smooth3D_Lx0(smoothOpNb),smooth3D_Ly0(smoothOpNb),
     & smooth3D_Lz0(smoothOpNb)


c vertical smoothing:

      if (smooth3DsizeZ(smoothOpNb).EQ.3) then
      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth3DscalesZ',smoothOpNb
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr, smooth3D_Lz,1,1,mythid)
      CALL EXCH_XYZ_RL( smooth3D_Lz, mythid )
      else
      DO bj=jtlo,jthi
       DO bi=itlo,ithi
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
          smooth3D_Lz(i,j,k,bi,bj)=smooth3D_Lz0(smoothOpNb)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      endif

      DO bj=jtlo,jthi
       DO bi=itlo,ithi
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
       smooth3D_kappaR(i,j,k,bi,bj)=smooth3D_Lz(i,j,k,bi,bj)*
     & smooth3D_Lz(i,j,k,bi,bj)/smooth3DtotTime/2
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

c avoid excessive vertical smoothing:
      if (smooth3DsizeZ(smoothOpNb).NE.3) then
      DO bj=jtlo,jthi
       DO bi=itlo,ithi
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx

       smooth3D_KzMax=drC(k)
       smooth3D_KzMax=smooth3D_KzMax*smooth3D_KzMax/smooth3DtotTime/2
       if (smooth3D_kappaR(i,j,k,bi,bj).GT.smooth3D_KzMax) then
       smooth3D_kappaR(i,j,k,bi,bj)=smooth3D_KzMax
       endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      endif

      CALL EXCH_XYZ_RL( smooth3D_kappaR,    myThid )


c hoizontal smoothing:

      if (smooth3DsizeH(smoothOpNb).EQ.3) then
      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth3DscalesH',smoothOpNb
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr, smooth3D_Lx,1,1,mythid)
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr, smooth3D_Ly,2,1,mythid)
      CALL EXCH_XYZ_RL( smooth3D_Lx, mythid )
      CALL EXCH_XYZ_RL( smooth3D_Ly, mythid )
      else
      DO bj=jtlo,jthi
       DO bi=itlo,ithi
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
          smooth3D_Lx(i,j,k,bi,bj)=smooth3D_Lx0(smoothOpNb)
          smooth3D_Ly(i,j,k,bi,bj)=smooth3D_Ly0(smoothOpNb)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      endif

      DO bj=jtlo,jthi
       DO bi=itlo,ithi
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
      smooth3D_Kuy(i,j,k,bi,bj)=0.
      smooth3D_Kvx(i,j,k,bi,bj)=0.
      smooth3D_Kwx(i,j,k,bi,bj)=0.
      smooth3D_Kwy(i,j,k,bi,bj)=0.
      smooth3D_Kwz(i,j,k,bi,bj)=0.
      smooth3D_Kux(i,j,k,bi,bj)=smooth3D_Lx(i,j,k,bi,bj)*
     & smooth3D_Lx(i,j,k,bi,bj)/smooth3DtotTime/2
      smooth3D_Kvy(i,j,k,bi,bj)=smooth3D_Ly(i,j,k,bi,bj)*
     & smooth3D_Ly(i,j,k,bi,bj)/smooth3DtotTime/2
      smooth3D_Kuz(i,j,k,bi,bj)=0.
      smooth3D_Kvz(i,j,k,bi,bj)=0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

c is exchange useful here?

      CALL EXCH_XYZ_RL( smooth3D_kappaR,    myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kwx,    myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kwy,    myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kwz,    myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kux,    myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kvy,    myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kuz,    myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kvz,    myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kuy,    myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kvx,    myThid )


c write diffusion operator to file

      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth3Doperator',smoothOpNb

      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr,smooth3D_Kwx,1,1,mythid)
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr,smooth3D_Kwy,2,1,mythid)
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr,smooth3D_Kwz,3,1,mythid)
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr,smooth3D_Kux,4,1,mythid)
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr,smooth3D_Kvy,5,1,mythid)
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr,smooth3D_Kuz,6,1,mythid)
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr,smooth3D_Kvz,7,1,mythid)
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr,smooth3D_Kuy,8,1,mythid)
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr,smooth3D_Kvx,9,1,mythid)
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr,smooth3D_kappaR,10,1,mythid)


      END
