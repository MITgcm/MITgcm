C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_init_varia.F,v 1.9 2011/11/16 20:00:31 wienders Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CHEAPAML_INIT_VARIA( myThid )
C     /==========================================================\
C     | SUBROUTINE CHEAPAML_INIT_VARIA                                      |
C     | o Set cheapaml initial temp field                          |
C     |==========================================================|
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c #include "DYNVARS.h"
#include "FFIELDS.h"
C #include "BULKF.h"
C #include "BULKF_DIAG.h"
C #ifdef CONSERV_BULKF
C #include "BULKF_CONSERV.h"
C #endif
#include "CHEAPAML.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of BULKF_INIT
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER I, J
      INTEGER IG,JG
      INTEGER xmw
      _RL local,localt,xmf
      _RL ssq0,ssq1,ssq2,ssqa

c coefficients used to compute saturation specific humidity
      DATA   ssq0,           ssq1,           ssq2
     &     / 3.797915 _d 0 , 7.93252 _d -6 , 2.166847 _d -3 /
c latent heat (J/kg)
      lath=2.5d6
c sea level pressure 
      p0=1000.d0
c     INTEGER prec
c     CHARACTER*(MAX_LEN_FNAM) fn


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
       _EXCH_XY_RL(Tair, mythid)
c
c do specific humidity
        
       IF ( AirQFile .NE. ' ') THEN
         write(*,*)'Qair initialized from  ->',AirQFile
         CALL READ_FLD_XY_RL( AirQFile,' ',qair,0,myThid )         
       ELSE
c     default to 80% relative humidity
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
       _EXCH_XY_RL(qair, mythid)


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
       _EXCH_XY_RL(Cheaptracer, mythid)

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
                  if(jG.gt.xmw)then
                        if(jG.lt.Ny-xmw+1)then
                                if(iG.le.xmw)
     .        Cheapmask(i,j,bi,bj)=1. _d 0 -(iG-1)/(xmw-1. _d 0)
                                if(iG.ge.Nx-xmw+1)
     .        Cheapmask(i,j,bi,bj)=1. _d 0 - (Nx-iG)/(xmw-1. _d 0)
                        else
                        xmf=1. _d 0-(Ny-jG)/(xmw-1 _d 0)
                   if(iG.le.xmw)then
                   xmf=(1. _d 0 - (iG-1)/(xmw-1. _d 0)*
     .             (Ny-jG)/(xmw-1. _d 0)) 
                    elseif(iG.ge.Nx-xmw+1) then 
                   xmf=1. _d 0-(Nx - iG)/(xmw - 1. _d 0)*
     .             (Ny-jG)/(xmw-1. _d 0) 
                        endif
                 Cheapmask(i,j,bi,bj)=xmf
                        endif
                else
                xmf=1. _d 0 - (jG-1)/(xmw-1. _d 0)
                    if(iG.le.xmw)then
             xmf=(1. _d 0 - (iG-1)/(xmw-1. _d 0)*(jG-1)/(xmw-1. _d 0))
                    elseif(iG.ge.Nx-xmw+1) then
             xmf=(1. _d 0-(Nx - iG)/(xmw - 1. _d 0)*(jG-1)/
     .        (xmw-1. _d 0))
                endif
                  Cheapmask(i,j,bi,bj)=xmf
                endif
                  
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
          if(maskC(i,j,1,bi,bj).eq.0. _d 0) 
     .             Cheapmask(i,j,bi,bj)=cheapaml_taurelax
          ENDDO
          ENDDO
          ENDDO
          ENDDO




        _EXCH_XY_RL(Cheapmask, mythid)


c relaxation time scales from input
      do bj=myByLo(myThid),myByHi(myThid)
      do bi=myBxLo(myThid),myBxHi(myThid)
      do j=1-oly,sny+oly
      do i=1-olx,snx+olx
        if (Cheapmask(i,j,bi,bj).NE.0) THEN
        xgs(i,j,bi,bj)=1. _d 0/Cheapmask(i,j,bi,bj)/8.64 _d 4
        ELSE
        xgs(i,j,bi,bj)=0. _d 0
        ENDIF
        xrelf(i,j,bi,bj)= xgs(i,j,bi,bj)*deltaT
     .                    /(1. _d 0+xgs(i,j,bi,bj)*deltaT) 
      enddo
      enddo
      enddo
      enddo   

        _EXCH_XY_RL(xgs, mythid)
        _EXCH_XY_RL(xrelf, mythid)

C     construct cheaplayer thickness
       IF ( cheap_hFile .NE. ' ') THEN
        write(*,*)'BL thickness taken from  ->',cheap_hFile
        CALL READ_FLD_XY_RL( cheap_hFile,' ',cheaphgrid,0,myThid )
       ELSE
         DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
             DO j=1-oly,sny+oly
               DO i=1-olx,snx+olx
               cheaphgrid(i,j,bi,bj) = cheapaml_h
               ENDDO
             ENDDO
           ENDDO
         ENDDO
       ENDIF
        _EXCH_XY_RL(Cheaphgrid, mythid)

C!BD       IF ( cheap_prfile .NE. ' ') THEN
C!BD        write(*,*)'Conv precip taken from  ->',cheap_prFile
C!BD        CALL READ_FLD_XY_RL( cheap_prFile,' ',cheapprgrid,0,myThid )
C!BD       ELSE
C!BD         DO bj = myByLo(myThid), myByHi(myThid)
C!BD           DO bi = myBxLo(myThid), myBxHi(myThid)
C!BD             DO j=1-oly,sny+oly
C!BD               DO i=1-olx,snx+olx
C!BD               cheapprgrid(i,j,bi,bj) = 0.0 _d 0
C!BD               ENDDO
C!BD             ENDDO
C!BD           ENDDO
C!BD         ENDDO
C!BD       ENDIF
C!BD        _EXCH_XY_RL(Cheapprgrid, mythid)

c fill in outer edges

         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
                do j=1-oly,sny+oly
                  jG = myYGlobalLo-1+(bj-1)*sNy+j
                do i=1-olx,snx+olx
                  iG=myXGlobalLo-1+(bi-1)*sNx+i
          
            if(iG.lt.1)then
                 Tair(i,j,bi,bj)=Tair(1,j,bi,bj)
                 qair(i,j,bi,bj)=qair(1,j,bi,bj)
                 Cheaptracer(i,j,bi,bj)=Cheaptracer(1,j,bi,bj)
                 Cheapmask(i,j,bi,bj)=Cheapmask(1,j,bi,bj)
                 Cheaphgrid(i,j,bi,bj)=Cheaphgrid(1,j,bi,bj)
            elseif(iG.gt.Nx)then
                 Tair(i,j,bi,bj)=Tair(sNx,j,bi,bj)
                 qair(i,j,bi,bj)=qair(sNx,j,bi,bj)
                 Cheaptracer(i,j,bi,bj)=Cheaptracer(sNx,j,bi,bj)
                 Cheapmask(i,j,bi,bj)=Cheapmask(sNx,j,bi,bj)
                 Cheaphgrid(i,j,bi,bj)=Cheaphgrid(sNx,j,bi,bj)
            elseif(jG.lt.1)then
                 Tair(i,j,bi,bj)=Tair(i,1,bi,bj)
                 qair(i,j,bi,bj)=qair(i,1,bi,bj)
                 Cheaptracer(i,j,bi,bj)=Cheaptracer(i,1,bi,bj)
                 Cheaphgrid(i,j,bi,bj)=Cheaphgrid(i,1,bi,bj)
            elseif(jG.gt.Ny)then
                 Tair(i,j,bi,bj)=Tair(i,sNy,bi,bj)
                 qair(i,j,bi,bj)=qair(i,sNy,bi,bj)
                 Cheaptracer(i,j,bi,bj)=Cheaptracer(i,sNy,bi,bj)
                 Cheaphgrid(i,j,bi,bj)=Cheaphgrid(i,sNy,bi,bj)
            endif
            ENDDO
           ENDDO
          ENDDO
         ENDDO

      RETURN
      END
      
