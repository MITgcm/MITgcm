C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_init_varia.F,v 1.4 2011/02/06 00:18:05 jmc Exp $
C $Name:  $

C--  File cheapaml_init_varia.F:
C--   Contents
C--   o CHEAPAML_INIT_VARIA
C--   o CHEAPAML_CONVERT_HUM

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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
c #include "GRID.h"
c #include "DYNVARS.h"
c #include "FFIELDS.h"
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
      INTEGER i, j
      INTEGER iG,jG
      INTEGER xmw
      _RL local,localt,xmf
      _RL ssq0,ssq1,ssq2,ssqa

C coefficients used to compute saturation specific humidity
      DATA   ssq0,           ssq1,           ssq2
     &     / 3.797915 _d 0 , 7.93252 _d -6 , 2.166847 _d -3 /
C latent heat (J/kg)
      lath=2.5d6
C sea level pressure
      p0=1000.d0

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

C do specific humidity

      IF ( AirQFile .NE. ' '.AND..NOT. useRelativeHumidity) THEN
         write(*,*)'Qair initialized from  ->',AirQFile
         CALL READ_FLD_XY_RS( AirQFile,' ',qair,0,myThid )
      ELSEIF ( AirQrelFile .NE. ' '.AND.useRelativeHumidity) THEN
         write(*,*)'Qair initialized from  ->',AirQrelFile
         CALL READ_FLD_XY_RS( AirQrelFile,' ',rair,0,myThid )
         write(*,*)'Computation of specific humidity'
         CALL CHEAPAML_CONVERT_HUM(rair,tair,qair, myThid )

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
      _EXCH_XY_RL(qair, mythid)

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
     &              Cheapmask(i,j,bi,bj)=1. _d 0 -(iG-1)/(xmw-1. _d 0)
                  if(iG.ge.Nx-xmw+1)
     &              Cheapmask(i,j,bi,bj)=1. _d 0 -(Nx-iG)/(xmw-1. _d 0)
                else
                  xmf=1. _d 0-(Ny-jG)/(xmw-1 _d 0)
                  if(iG.le.xmw)then
                   xmf=(1. _d 0 - (iG-1)/(xmw-1. _d 0)*
     &             (Ny-jG)/(xmw-1. _d 0))
                  elseif(iG.ge.Nx-xmw+1) then
                   xmf=1. _d 0-(Nx - iG)/(xmw - 1. _d 0)*
     &             (Ny-jG)/(xmw-1. _d 0)
                  endif
                  Cheapmask(i,j,bi,bj)=xmf
                endif
              else
                xmf=1. _d 0 - (jG-1)/(xmw-1. _d 0)
                if(iG.le.xmw)then
                  xmf=(1. _d 0 - (iG-1)/(xmw-1. _d 0)*(jG-1)
     &                /(xmw-1. _d 0))
                elseif(iG.ge.Nx-xmw+1) then
                  xmf=(1. _d 0-(Nx - iG)/(xmw - 1. _d 0)*(jG-1)
     &                /(xmw-1. _d 0))
                endif
                Cheapmask(i,j,bi,bj)=xmf
              endif
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      _EXCH_XY_RL(Cheapmask, mythid)
C fill in outer edges

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-oly,sny+oly
            jG = myYGlobalLo-1+(bj-1)*sNy+j
            DO i=1-olx,snx+olx
              iG=myXGlobalLo-1+(bi-1)*sNx+i
              if(iG.lt.1)then
                Tair(i,j,bi,bj)=Tair(1,j,bi,bj)
                qair(i,j,bi,bj)=qair(1,j,bi,bj)
                Cheapmask(i,j,bi,bj)=Cheapmask(1,j,bi,bj)
              elseif(iG.gt.Nx)then
                Tair(i,j,bi,bj)=Tair(sNx,j,bi,bj)
                qair(i,j,bi,bj)=qair(sNx,j,bi,bj)
                Cheapmask(i,j,bi,bj)=Cheapmask(sNx,j,bi,bj)
              elseif(jG.lt.1)then
                Tair(i,j,bi,bj)=Tair(i,1,bi,bj)
                qair(i,j,bi,bj)=qair(i,1,bi,bj)
                Cheapmask(i,j,bi,bj)=Cheapmask(i,1,bi,bj)
              elseif(jG.gt.sNy)then
                Tair(i,j,bi,bj)=Tair(i,sNy,bi,bj)
                qair(i,j,bi,bj)=qair(i,sNy,bi,bj)
                Cheapmask(i,j,bi,bj)=Cheapmask(i,sNy,bi,bj)
              endif
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE CHEAPAML_CONVERT_HUM(rel_in,T_in,q_out, myThid )
C     *==========================================================*
C     | SUBROUTINE CHEAPAML_CONVERT_HUM
C     | o Convert rel. humidity to spec. humidity
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "FFIELDS.h"
#include "CHEAPAML.h"

C     == Routine arguments ==
      INTEGER myThid
      _RL    rel_in (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL    T_in (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL    q_out  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER I, J
      _RL localt,localr
      _RL es_i, es_w, es

C     Pressure in hPa
      _RL pressure
      DATA pressure/ 1000 _d 2/
      _RL a1, t0, ti, rv, rd, a3_i, a4_i, a3_w, a4_w
c     _RL rh_min
      DATA a1,    t0,         ti,    rv,  rd
     &  /611.21 _d 0,  273.16 _d 0,  250.16 _d 0, 461.5 _d 0, 287. _d 0/
      DATA a3_i,  a4_i, a3_w,   a4_w
     &   /22.587 _d 0, -0.7 _d 0, 17.502 _d 0, 32.19 _d 0/
c     DATA rh_min / 0. _d 0/

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1,sNy
            DO i=1,sNx
C     in percent
              localr = rel_in(i,j,bi,bj)/100.0
C     readjust
              IF (localr.LT.0.) localr = 0.
              IF (localr.GT.1.) localr = 1.
C     Temp in K
              localt = T_in(i,j,bi,bj)  + t0
              IF (localt.GT.t0) THEN
                   es = a1*exp(a3_w*(localt-t0)/(localt-a4_w))
              ELSEIF (localt.GT.ti .AND. localt.LE.t0) THEN
                   es_w = a1*exp(a3_w*(localt-t0)/(localt-a4_w))
                   es_i = a1*exp(a3_i*(localt-t0)/(localt-a4_i))
                   es = es_i + (es_w-es_i)*((localt-ti)/(t0-ti))**2
              ELSEIF (localt.LE.ti) THEN
                   es = a1*exp(a3_i*(localt-t0)/(localt-a4_i))
              ENDIF
              q_out(i,j,bi,bj) = localr*rd/rv*es/
     &              (pressure-localr*(1.-rd/rv)*es)
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END
