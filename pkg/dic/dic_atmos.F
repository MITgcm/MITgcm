C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_atmos.F,v 1.12 2008/08/11 17:41:05 dfer Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: DIC_ATMOS

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_ATMOS( istate, myTime, myIter, myThid )

C !DESCRIPTION:
C  Calculate the atmospheric pCO2
C  dic_int1:
C  0=use default 278.d-6
C  1=use constant value - dic_pCO2, read in from data.dic
C  2=read in from file
C  3=interact with atmospheric box
C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DIC_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_FIELDS.h"
#include "DIC_ATMOS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  istate               :: 0=initial call, 1=subsequent calls
      INTEGER myIter, myThid, istate
      _RL myTime

#ifdef ALLOW_PTRACERS
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C !LOCAL VARIABLES: ====================================================
       INTEGER bi, bj, I,J,k
       INTEGER ntim, iUnit
c
       _RL total_flux
       _RL total_ocean_carbon_old
       _RL total_atmos_carbon_old
       _RL total_atmos_moles
       _RL atpco2
       _RL total_carbon_old, total_carbon, carbon_diff
       _RL tmp
       _RL year_diff_ocean, year_diff_atmos, year_total
       _RL start_diff_ocean, start_diff_atmos, start_total
C variables for reading CO2 input files
      _RL aWght, bWght
c
       CHARACTER*(MAX_LEN_FNAM) fn
       LOGICAL permCheckPoint
CEOP

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c if coupled to atmsopheric model, use the
c Co2 value passed from the coupler
#ifndef USE_ATMOSCO2

      IF ( nThreads .GT. 1 .AND.
     &     ( dic_int1.EQ.2 .OR. dic_int1.EQ.3 ) ) THEN
C     Problem with I/O and global-sum for multi-threaded execution
C     Needs to be fixed before using this S/R in multi-threaded run
        STOP 'S/R DIC_ATMOS: multi-threaded not right'
      ENDIF

c default - set only once
      if (dic_int1.eq.0.and.istate.eq.0) then
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
             AtmospCO2(i,j,bi,bj)=278.0 _d -6
          ENDDO
         ENDDO

        ENDDO
       ENDDO
      endif

c user specified value - set only once
      if (dic_int1.eq.1.and.istate.eq.0) then
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
             AtmospCO2(i,j,bi,bj)=dic_pCO2
          ENDDO
         ENDDO

        ENDDO
       ENDDO
      endif

c read from a file (note:
c                   dic_int2=number entries to read
c                   dic_int3=start timestep,
c                   dic_int4=timestep between file entries)
      if (dic_int1.eq.2) then
c linearly interpolate between file entries
          ntim=int((myIter-dic_int3)/dic_int4)+1
          aWght = FLOAT(myIter-dic_int3)
          bWght = FLOAT(dic_int4)
          aWght = 0.5 _d 0 + aWght/bWght - FLOAT(ntim-1)
          if (aWght.gt.1. _d 0) then
            ntim=ntim+1
            aWght=aWght-1. _d 0
          endif
          bWght = 1. _d 0 - aWght
          tmp=co2atmos(ntim)*bWght+co2atmos(ntim+1)*aWght
c         print*,'weights',ntim, aWght, bWght, tmp

          DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)

            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               AtmospCO2(i,j,bi,bj)=tmp
             ENDDO
            ENDDO

           print*,'AtmospCO2(20,20)',AtmospCO2(20,20,bi,bj)

           ENDDO
          ENDDO


      endif


c interactive atmosphere
      if (dic_int1.eq.3) then

c      _BEGIN_MASTER(myThid)

cMass dry atmosphere = (5.1352+/-0.0003)d18 kg (Trenberth & Smith,
cJournal of Climate 2005)
cand Mean molecular mass air = 28.97 g/mol (NASA earth fact sheet)
       total_atmos_moles= 1.77 _d 20
c for 278ppmv we need total_atmos_carbon=4.9206e+16

       if (istate.gt.0) then
        total_ocean_carbon_old=total_ocean_carbon
        total_atmos_carbon_old=total_atmos_carbon
       else
        total_ocean_carbon_old=0. _d 0
        total_atmos_carbon_old=0. _d 0
       endif

       total_flux= 0. _d 0
       total_ocean_carbon= 0. _d 0

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO i=1,sNx
          DO j=1,sNy
            if (istate.gt.0) then
              total_flux=total_flux+FluxCO2(i,j,bi,bj)*rA(i,j,bi,bj)*
     &                        maskC(i,j,1,bi,bj)*dTtracerLev(1)
            endif
            DO k=1,nR
              total_ocean_carbon= total_ocean_carbon+
     &              ( Ptracer(i,j,k,bi,bj,1)
#ifdef DIC_BIOTIC
     &               +R_cp*Ptracer(i,j,k,bi,bj,4) 
#endif
     &              ) * rA(i,j,bi,bj)*
     &                drF(k)*hFacC(i,j,k,bi,bj)
            ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _GLOBAL_SUM_R8(total_flux,myThid)
       _GLOBAL_SUM_R8(total_ocean_carbon,myThid)

       if (istate.eq.0) then
c use value read in dic_init_fixed
          total_atmos_carbon=total_atmos_carbon_ini
       else
c calculate new atmos pCO2
          total_atmos_carbon=total_atmos_carbon - total_flux
c write out if time for a new pickup
          permCheckPoint = .FALSE.
          permCheckPoint =
     &      DIFFERENT_MULTIPLE(pChkptFreq,myTime,deltaTClock)
          if (permCheckPoint) then
            DO i = 1,MAX_LEN_FNAM
                fn(i:i) = ' '
            ENDDO
            WRITE(fn,'(A,I10.10)') 'dic_atmos.',myIter
C     Going to really do some IO. Make everyone except master thread wait.
            _BARRIER
c write values to new pickup

            CALL MDSFINDUNIT( iUnit, myThid )
            open(iUnit,file=fn,status='new')
            write(iUnit,*) total_atmos_carbon, atpco2
            close(iUnit)

          endif
       endif

       atpco2=total_atmos_carbon/total_atmos_moles

c     print*,'QQpCO2', total_atmos_carbon, atpco2, total_ocean_carbon,
c    &                 total_flux

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
             AtmospCO2(i,j,bi,bj)=atpco2
          ENDDO
         ENDDO

        ENDDO
       ENDDO

      print*,'QQ atmos C, total, pCo2', total_atmos_carbon, atpco2
      total_carbon=total_atmos_carbon + total_ocean_carbon
      total_carbon_old=total_atmos_carbon_old + total_ocean_carbon_old
      carbon_diff=total_carbon-total_carbon_old
      print*,'QQ total C, current, old, diff', total_carbon,
     &                         total_carbon_old, carbon_diff
      carbon_diff=total_ocean_carbon-total_ocean_carbon_old
      tmp=carbon_diff-total_flux
      print*,'QQ ocean C, current, old, diff',total_ocean_carbon,
     &                   total_ocean_carbon_old, carbon_diff
      print*,'QQ air-sea flux, addition diff', total_flux, tmp

c if end of forcing cycle, find total change in ocean carbon
      if (istate.eq.0) then
       total_ocean_carbon_start=total_ocean_carbon
       total_ocean_carbon_year=total_ocean_carbon
       total_atmos_carbon_start=total_atmos_carbon
       total_atmos_carbon_year=total_atmos_carbon
      else
        permCheckPoint = .FALSE.
        permCheckPoint =
     &      DIFFERENT_MULTIPLE(externForcingCycle,myTime,deltaTClock)
        if (permCheckPoint) then
          year_diff_ocean=total_ocean_carbon-total_ocean_carbon_year
          year_diff_atmos=total_atmos_carbon-total_atmos_carbon_year
          year_total=(total_ocean_carbon+total_atmos_carbon) -
     &               (total_ocean_carbon_year+total_atmos_carbon_year)
          start_diff_ocean=total_ocean_carbon-total_ocean_carbon_start
          start_diff_atmos=total_atmos_carbon-total_atmos_carbon_start
          start_total=(total_ocean_carbon+total_atmos_carbon) -
     &            (total_ocean_carbon_start+total_atmos_carbon_start)
          print*,'QQ YEAR END'
          print*,'year diff: ocean, atmos, total', year_diff_ocean,
     &                year_diff_atmos, year_total
          print*,'start diff: ocean, atmos, total ', start_diff_ocean,
     &                start_diff_atmos, start_total
c
          total_ocean_carbon_year=total_ocean_carbon
          total_atmos_carbon_year=total_atmos_carbon
        endif
        endif

c     _END_MASTER(myThid)

      endif

#endif
#endif

      RETURN
      END
