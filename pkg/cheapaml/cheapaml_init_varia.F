C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_init_varia.F,v 1.11 2012/12/23 20:14:06 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CHEAPAML_INIT_VARIA( myThid )
C     *==========================================================*
C     | SUBROUTINE CHEAPAML_INIT_VARIA
C     | o Set cheapaml initial temp field
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "CHEAPAML.h"

C     == Routine arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  :: Loop counters
C     i,j
      INTEGER bi, bj
      INTEGER i, j
      INTEGER iG,jG
      INTEGER xmw
      _RL local,localt,xmf
      _RL ssq0,ssq1,ssq2,ssqa
C     INTEGER prec
C     CHARACTER*(MAX_LEN_FNAM) fn

C coefficients used to compute saturation specific humidity
      DATA   ssq0,           ssq1,           ssq2
     &     / 3.797915 _d 0 , 7.93252 _d -6 , 2.166847 _d -3 /
C latent heat (J/kg)
      lath=2.5d6
C sea level pressure
      p0=1000.d0

C--   Initialise CheapAML variables in common block:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          Tr            (i,j,bi,bj) = 0. _d 0
          qr            (i,j,bi,bj) = 0. _d 0
          Tair          (i,j,bi,bj) = 0. _d 0
          gTairm        (i,j,bi,bj) = 0. _d 0
          qair          (i,j,bi,bj) = 0. _d 0
          gqairm        (i,j,bi,bj) = 0. _d 0
          uwind         (i,j,bi,bj) = 0. _d 0
          vwind         (i,j,bi,bj) = 0. _d 0
          wwind         (i,j,bi,bj) = 0. _d 0
          solar         (i,j,bi,bj) = 0. _d 0
          ustress       (i,j,bi,bj) = 0. _d 0
          vstress       (i,j,bi,bj) = 0. _d 0
          wavesh        (i,j,bi,bj) = 0. _d 0
          wavesp        (i,j,bi,bj) = 0. _d 0
          Cheapmask     (i,j,bi,bj) = 0. _d 0
          xgs           (i,j,bi,bj) = 0. _d 0
          xrelf         (i,j,bi,bj) = 0. _d 0
          CheapHgrid    (i,j,bi,bj) = 0. _d 0
          Cheapprgrid   (i,j,bi,bj) = 0. _d 0
          Cheapclouds   (i,j,bi,bj) = 0. _d 0
          Cheapdlongwave(i,j,bi,bj) = 0. _d 0
          Cheaptracer   (i,j,bi,bj) = 0. _d 0
          CheaptracerR  (i,j,bi,bj) = 0. _d 0
          gCheaptracerm (i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( startTime.EQ.baseTime .AND. nIter0.EQ.0
     &                           .AND. pickupSuff.EQ.' ' ) THEN

       IF ( AirTempFile .NE. ' ' ) THEN
         write(*,*)'Tair initialized from  ->',AirTempFile
         CALL READ_FLD_XY_RL( AirTempFile,' ',Tair,0,myThid )
       ELSE
         write(*,*)'Tair initialized using standard profile'
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             jG = myYGlobalLo-1+(bj-1)*sNy+j
             iG=myXGlobalLo-1+(bi-1)*sNx+i
             localt=25.d0-float((jg-1))/float((ny-1))*10.d0
             localt=20+10.d0*exp(float(-((jg-30)**2+(ig-30)**2))/100.d0)
             Tair(i,j,bi,bj) = localt
            ENDDO
           ENDDO
          ENDDO
         ENDDO
       ENDIF
       _EXCH_XY_RL(Tair, myThid)

C do specific humidity
       IF ( AirQFile .NE. ' ') THEN
         write(*,*)'Qair initialized from  ->',AirQFile
         CALL READ_FLD_XY_RL( AirQFile,' ',qair,0,myThid )
       ELSE
C     default to 80% relative humidity
         write(*,*)'Qair initialized using standard profile'
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             local= Tair(i,j,bi,bj)+273.16 _d 0
             ssqa = ssq0*exp( lath*(ssq1-ssq2/local) ) / p0
             qair(i,j,bi,bj)=0.8 _d 0*ssqa
            ENDDO
           ENDDO
          ENDDO
         ENDDO
       ENDIF
       _EXCH_XY_RL(qair, myThid)

C do passive tracer
       IF ( TracerFile .NE. ' ') THEN
        write(*,*)'Tracer initialized from  ->',TracerFile
        CALL READ_FLD_XY_RL( TracerFile,' ',Cheaptracer,0,myThid )
       ELSE
C default value at 290 (!)
        write(*,*)'Tracer initialized using standard profile'
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
             Cheaptracer(i,j,bi,bj)=290.0 _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
       _EXCH_XY_RL(Cheaptracer, myThid)

      ELSE
C Restart from cheapaml_pickups
       CALL CHEAPAML_READ_PICKUP( nIter0, myThid )
C End start-from-iter-zero if/else block
      ENDIF

C Do  mask
      IF ( CheapMaskFile .NE. ' ') THEN
         write(*,*)'CheapMask initialized from  ->',CheapMaskFile
         CALL READ_FLD_XY_RL( CheapMaskFile,' ',Cheapmask,0,myThid )
      ELSE
         write(*,*)' Generate Cheapaml mask'
         xmw=Cheapaml_mask_width
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             Cheapmask(i,j,bi,bj)=0. _d 0
             iG=myXGlobalLo-1+(bi-1)*sNx+i
             jG = myYGlobalLo-1+(bj-1)*sNy+j
             IF (jG.GT.xmw) THEN
               IF (jG.LT.Ny-xmw+1) THEN
                 IF (iG.LE.xmw)
     &             Cheapmask(i,j,bi,bj)=1. _d 0 - (iG-1 )/(xmw-1. _d 0)
                 IF (iG.GE.Nx-xmw+1)
     &             Cheapmask(i,j,bi,bj)=1. _d 0 - (Nx-iG)/(xmw-1. _d 0)
               ELSE
                 xmf=1. _d 0-(Ny-jG)/(xmw-1 _d 0)
                 IF (iG.LE.xmw) THEN
                   xmf=(1. _d 0 - (iG-1)/(xmw - 1. _d 0)
     &                  *(Ny-jG)/(xmw-1. _d 0) )
                 ELSEIF (iG.GE.Nx-xmw+1) THEN
                   xmf= 1. _d 0-(Nx - iG)/(xmw - 1. _d 0)
     &                  *(Ny-jG)/(xmw-1. _d 0)
                 ENDIF
                 Cheapmask(i,j,bi,bj)=xmf
               ENDIF
             ELSE
               xmf=1. _d 0 - (jG-1)/(xmw-1. _d 0)
               IF (iG.LE.xmw) THEN
                 xmf=(1. _d 0 - (iG-1)/(xmw-1. _d 0)
     &                  *(jG-1)/(xmw-1. _d 0) )
               ELSEIF (iG.GE.Nx-xmw+1) THEN
                 xmf=(1. _d 0-(Nx - iG)/(xmw - 1. _d 0)
     &                  *(jG-1)/(xmw-1. _d 0) )
               ENDIF
               Cheapmask(i,j,bi,bj)=xmf
             ENDIF

             Cheapmask(i,j,bi,bj)=Cheapmask(i,j,bi,bj)*cheapaml_taurelax
            ENDDO
           ENDDO
          ENDDO
         ENDDO
      ENDIF

C     relaxation forced on land
       DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
             DO i=1,sNx
               IF( maskC(i,j,1,bi,bj).EQ.0. _d 0) THEN
                 Cheapmask(i,j,bi,bj)=cheapaml_taurelax
C     relaxation over the ocean
               ELSEIF( Cheapmask(i,j,bi,bj).EQ.0. _d 0) THEN
                 Cheapmask(i,j,bi,bj)=cheapaml_taurelaxocean
               ENDIF
             ENDDO
           ENDDO
         ENDDO
       ENDDO
       _EXCH_XY_RL(Cheapmask, myThid)

C relaxation time scales from input
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF (Cheapmask(i,j,bi,bj).NE.0) THEN
            xgs(i,j,bi,bj)=1. _d 0/Cheapmask(i,j,bi,bj)/8.64 _d 4
           ELSE
            xgs(i,j,bi,bj)=0. _d 0
           ENDIF
           xrelf(i,j,bi,bj)= xgs(i,j,bi,bj)*deltaT
     &                     /(1. _d 0+xgs(i,j,bi,bj)*deltaT)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _EXCH_XY_RL(xgs, myThid)
       _EXCH_XY_RL(xrelf, myThid)

C     construct cheaplayer thickness
       IF ( cheap_hFile .NE. ' ') THEN
        write(*,*)'BL thickness taken from  ->',cheap_hFile
        CALL READ_FLD_XY_RL( cheap_hFile,' ',cheaphgrid,0,myThid )
       ELSE
         DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
             DO j=1-OLy,sNy+OLy
               DO i=1-OLx,sNx+OLx
               cheaphgrid(i,j,bi,bj) = cheapaml_h
               ENDDO
             ENDDO
           ENDDO
         ENDDO
       ENDIF
       _EXCH_XY_RL(Cheaphgrid, myThid)

c!BD       IF ( cheap_prfile .NE. ' ') THEN
c!BD        write(*,*)'Conv precip taken from  ->',cheap_prFile
c!BD        CALL READ_FLD_XY_RL( cheap_prFile,' ',cheapprgrid,0,myThid )
c!BD       ELSE
c!BD         DO bj = myByLo(myThid), myByHi(myThid)
c!BD           DO bi = myBxLo(myThid), myBxHi(myThid)
c!BD             DO j=1-OLy,sNy+OLy
c!BD               DO i=1-OLx,sNx+OLx
c!BD               cheapprgrid(i,j,bi,bj) = 0.0 _d 0
c!BD               ENDDO
c!BD             ENDDO
c!BD           ENDDO
c!BD         ENDDO
c!BD       ENDIF
c!BD        _EXCH_XY_RL(Cheapprgrid, myThid)

C fill in outer edges
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           jG = myYGlobalLo-1+(bj-1)*sNy+j
           DO i=1-OLx,sNx+OLx
             iG=myXGlobalLo-1+(bi-1)*sNx+i
             IF (iG.LT.1) THEN
                 Tair(i,j,bi,bj)=Tair(1,j,bi,bj)
                 qair(i,j,bi,bj)=qair(1,j,bi,bj)
                 Cheaptracer(i,j,bi,bj)=Cheaptracer(1,j,bi,bj)
                 Cheapmask(i,j,bi,bj)=Cheapmask(1,j,bi,bj)
                 Cheaphgrid(i,j,bi,bj)=Cheaphgrid(1,j,bi,bj)
             ELSEIF (iG.GT.Nx) THEN
                 Tair(i,j,bi,bj)=Tair(sNx,j,bi,bj)
                 qair(i,j,bi,bj)=qair(sNx,j,bi,bj)
                 Cheaptracer(i,j,bi,bj)=Cheaptracer(sNx,j,bi,bj)
                 Cheapmask(i,j,bi,bj)=Cheapmask(sNx,j,bi,bj)
                 Cheaphgrid(i,j,bi,bj)=Cheaphgrid(sNx,j,bi,bj)
             ELSEIF (jG.LT.1) THEN
                 Tair(i,j,bi,bj)=Tair(i,1,bi,bj)
                 qair(i,j,bi,bj)=qair(i,1,bi,bj)
                 Cheaptracer(i,j,bi,bj)=Cheaptracer(i,1,bi,bj)
                 Cheaphgrid(i,j,bi,bj)=Cheaphgrid(i,1,bi,bj)
             ELSEIF (jG.GT.Ny) THEN
                 Tair(i,j,bi,bj)=Tair(i,sNy,bi,bj)
                 qair(i,j,bi,bj)=qair(i,sNy,bi,bj)
                 Cheaptracer(i,j,bi,bj)=Cheaptracer(i,sNy,bi,bj)
                 Cheaphgrid(i,j,bi,bj)=Cheaphgrid(i,sNy,bi,bj)
             ENDIF
           ENDDO
          ENDDO
        ENDDO
       ENDDO

      RETURN
      END
