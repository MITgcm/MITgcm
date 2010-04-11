C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_atmos.F,v 1.16 2010/04/11 21:04:39 jmc Exp $
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
C  3=interact with atmospheric box (use dic_pCO2 as initial atmos. value)

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "DIC_ATMOS.h"

C !INPUT PARAMETERS: ===================================================
C  istate               :: 0=initial call, 1=subsequent calls
C  myTime               :: current time
C  myIter               :: current iteration number
C  myThid               :: my Thread Id number
      INTEGER istate
      _RL myTime
      INTEGER myIter, myThid

#ifdef ALLOW_DIC

#ifdef USE_ATMOSCO2
C if coupled to atmsopheric model, use the
C CO2 value passed from the coupler

#else /* USE_ATMOSCO2 */

C !FUNCTIONS:       ====================================================
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C !LOCAL VARIABLES: ====================================================
C   total_atmos_moles :: atmosphere total gas content (should be parameter)
      _RL total_atmos_moles
      INTEGER bi, bj, i,j,k
      INTEGER ntim

      _RL tile_flux  (nSx,nSy)
      _RL tile_carbon(nSx,nSy)
      _RL total_flux
      _RL total_carbon

C for carbon budget ouput
      INTEGER ioUnit
      _RL total_ocean_carbon_old
      _RL total_atmos_carbon_old
      _RL total_carbon_old, carbon_diff
      _RL year_diff_ocean, year_diff_atmos, year_total
      _RL start_diff_ocean, start_diff_atmos, start_total
C variables for reading CO2 input files
      _RL tmp
      _RL aWght, bWght

      LOGICAL timeCO2budget
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      ioUnit = standardMessageUnit

C user specified value (or default = 278 ppm)- set only once
      IF ( (dic_int1.EQ.0 .OR. dic_int1.EQ.1) .AND. istate.EQ.0 ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             AtmospCO2(i,j,bi,bj)=dic_pCO2
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

C read from a file (note:
C                   dic_int2=number entries to read
C                   dic_int3=start timestep,
C                   dic_int4=timestep between file entries)
      IF (dic_int1.EQ.2) THEN
C linearly interpolate between file entries
        ntim=int((myIter-dic_int3)/dic_int4)+1
        aWght = FLOAT(myIter-dic_int3)
        bWght = FLOAT(dic_int4)
        aWght = 0.5 _d 0 + aWght/bWght - FLOAT(ntim-1)
        IF (aWght.GT.1. _d 0) THEN
          ntim=ntim+1
          aWght=aWght-1. _d 0
        ENDIF
        bWght = 1. _d 0 - aWght
        tmp=co2atmos(ntim)*bWght+co2atmos(ntim+1)*aWght
        WRITE(ioUnit,*) 'weights',ntim, aWght, bWght, tmp

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               AtmospCO2(i,j,bi,bj)=tmp
             ENDDO
           ENDDO
         ENDDO
        ENDDO

      ENDIF

C interactive atmosphere
      IF (dic_int1.EQ.3) THEN

C Mass dry atmosphere = (5.1352+/-0.0003)d18 kg (Trenberth & Smith,
C Journal of Climate 2005)
C and Mean molecular mass air = 28.97 g/mol (NASA earth fact sheet)
       total_atmos_moles= 1.77 _d 20
C for 278ppmv we need total_atmos_carbon=4.9206e+16

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         tile_flux(bi,bj)   = 0.
         tile_carbon(bi,bj) = 0.
         IF (istate.GT.0) THEN
          DO j=1,sNy
           DO i=1,sNx
             tile_flux(bi,bj) = tile_flux(bi,bj)
     &                        + FluxCO2(i,j,bi,bj)*rA(i,j,bi,bj)
     &                         *maskC(i,j,1,bi,bj)*dTtracerLev(1)
           ENDDO
          ENDDO
         ENDIF
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx
             tile_carbon(bi,bj) = tile_carbon(bi,bj)
     &            + ( pTracer(i,j,k,bi,bj,1)
#ifdef DIC_BIOTIC
     &               +R_cp*pTracer(i,j,k,bi,bj,4)
#endif
     &              ) * rA(i,j,bi,bj)
     &                *drF(k)*hFacC(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_SUM_TILE_RL( tile_flux,   total_flux,   myThid )
       CALL GLOBAL_SUM_TILE_RL( tile_carbon, total_carbon, myThid )

       IF (istate.EQ.0) THEN
C use dic_pCO2 as initial atmospheric pCO2 (not restart case):
         _BEGIN_MASTER(myThid)
         atpco2 = dic_pCO2
         total_atmos_carbon = total_atmos_moles*dic_pCO2
         _END_MASTER(myThid)
         IF ( nIter0.GT.PTRACERS_Iter0 .OR.
     &       (nIter0.EQ.PTRACERS_Iter0 .AND. pickupSuff.NE.' ')
     &      ) THEN
C restart case: read previous atmospheric CO2 content & pCO2 from pickup file
           CALL DIC_READ_CO2_PICKUP( nIter0, myThid )
         ENDIF
         _BEGIN_MASTER(myThid)
C store initial content:
         total_ocean_carbon_start=total_carbon
         total_atmos_carbon_start=total_atmos_carbon
         total_ocean_carbon_old = total_carbon
         total_atmos_carbon_old = total_atmos_carbon
         _END_MASTER(myThid)
       ELSE
         _BEGIN_MASTER(myThid)
#ifdef ALLOW_AUTODIFF_TAMC
         atpco2 = dic_pCO2
#endif
C store previous content:
         total_ocean_carbon_old = total_ocean_carbon
         total_atmos_carbon_old = total_atmos_carbon
C calculate new atmos pCO2
         total_atmos_carbon = total_atmos_carbon - total_flux
         _END_MASTER(myThid)
       ENDIF
       _BEGIN_MASTER(myThid)
       total_ocean_carbon = total_carbon
       atpco2 = total_atmos_carbon/total_atmos_moles

       WRITE(ioUnit,*) 'QQ atmos C, total, pCo2',
     &                     total_atmos_carbon, atpco2
       total_carbon=total_atmos_carbon + total_ocean_carbon
       total_carbon_old=total_atmos_carbon_old + total_ocean_carbon_old
       carbon_diff=total_carbon-total_carbon_old
       WRITE(ioUnit,*) 'QQ total C, current, old, diff',
     &                     total_carbon, total_carbon_old, carbon_diff
       carbon_diff=total_ocean_carbon-total_ocean_carbon_old
       WRITE(ioUnit,*) 'QQ ocean C, current, old, diff',
     &         total_ocean_carbon, total_ocean_carbon_old, carbon_diff
       WRITE(ioUnit,*) 'QQ air-sea flux, addition diff',
     &                     total_flux, carbon_diff-total_flux

C if end of forcing cycle, find total change in ocean carbon
       IF (istate.EQ.0) THEN
        total_ocean_carbon_year = total_ocean_carbon
        total_atmos_carbon_year = total_atmos_carbon
       ELSE
        timeCO2budget =
     &      DIFFERENT_MULTIPLE(externForcingCycle,myTime,deltaTClock)
        IF ( timeCO2budget ) THEN
          year_diff_ocean = total_ocean_carbon-total_ocean_carbon_year
          year_diff_atmos = total_atmos_carbon-total_atmos_carbon_year
          year_total = (total_ocean_carbon+total_atmos_carbon) -
     &               (total_ocean_carbon_year+total_atmos_carbon_year)
          start_diff_ocean = total_ocean_carbon-total_ocean_carbon_start
          start_diff_atmos = total_atmos_carbon-total_atmos_carbon_start
          start_total = (total_ocean_carbon+total_atmos_carbon) -
     &               (total_ocean_carbon_start+total_atmos_carbon_start)
          WRITE(ioUnit,*) 'QQ YEAR END'
          WRITE(ioUnit,*) 'year diff: ocean, atmos, total',
     &                  year_diff_ocean,  year_diff_atmos,  year_total
          WRITE(ioUnit,*) 'start diff: ocean, atmos, total ',
     &                 start_diff_ocean, start_diff_atmos, start_total

          total_ocean_carbon_year = total_ocean_carbon
          total_atmos_carbon_year = total_atmos_carbon
        ENDIF
       ENDIF

       _END_MASTER(myThid)
       _BARRIER

C--    Set AtmospCO2 for next iteration:
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            AtmospCO2(i,j,bi,bj) = atpco2
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ENDIF

#endif /* ndef USE_ATMOSCO2 */

#endif /* ALLOW_DIC */

      RETURN
      END
