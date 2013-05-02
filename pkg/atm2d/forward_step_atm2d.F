C $Header: /u/gcmpack/MITgcm/pkg/atm2d/forward_step_atm2d.F,v 1.27 2013/05/02 20:37:52 jmc Exp $
C $Name:  $

#include "ctrparam.h"
#ifdef OCEAN_3D
#  include "ATM2D_OPTIONS.h"
#endif
C
      SUBROUTINE FORWARD_STEP_ATM2D(iloop, myTime, myIter, myThid)
C     |==========================================================|
C     | Does time loop for one coupled period. The main loop     |
C     | this is the MITGCM main loop OR a separate driver for    |
C     | IGSM 2.2                                                 |
C     \==========================================================/
      IMPLICIT NONE

#include "ATMSIZE.h"
#include "DRIVER.h"

#ifdef OCEAN_3D
#  include "SIZE.h"
#  include "EEPARAMS.h"
#  include "PARAMS.h"
#  include "ATM2D_VARS.h"
#endif

#ifdef NCEPWIND
      COMMON  /SEED/JSEED,IFRST,NEXTN
      INTEGER JSEED,IFRST,NEXTN
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     iloop  - loop counter for coupled period time steps (main time loop)
C     myIter - iteration counter for this thread (ocean times steps +nIter0)
C     myTime - time counter for this thread (ocean time, from starttTime)
C     myThid - thread number for this instance of the routine.
      INTEGER iloop
      REAL*8  myTime
      INTEGER myIter
      INTEGER myThid

C     === Local variables ===
      INTEGER idyear ! year # of simulation, starting at year 1
      INTEGER iyr    ! year # of simulation, starting from specified inyear
      INTEGER inyr   ! hours into the current year, end of coupled period
      INTEGER monid  ! current month of the year
      INTEGER inday  ! hour of the day, end of the coupled period
      INTEGER dayid  ! day of the current month
      INTEGER j,mn,na,no   !loop counters
      INTEGER jdofmhr(0:12)
      DATA jdofmhr/0,744,1416,2160,2880,3624,4344,5088,
     &                  5832,6552,7296,8016,8760/
C i.e. 0,31*24,59*24,90*24,120*24,151*24,181*24,
C      212*24,243*24,273*24,304*24,334*24,365*24
#ifdef CPL_TEM
      INTEGER ndmonth(12)
      DATA ndmonth/31,28,31,30,31,30,31,31,30,31,30,31/
      REAL*4 totup, aduptt
      REAL*8 tcumn
#endif
#if (defined CPL_TEM) || (defined CPL_OCEANCO2)
      REAL*8 nepan
      REAL*8 ocuptp
#endif
#ifdef OCEAN_3D
      INTEGER iloop_ocn, i
      _RL qPrcRn(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#  ifdef NCEPWIND
      REAL*8  RAND
      CHARACTER *4 ncep_yr
#  endif
#endif
#ifdef DATA4TEM
      CHARACTER *40 f4tem,f4clm,f24tem,f34tem
      CHARACTER *4  cfile
      CHARACTER *8  f14tem,f14clm
      character *9 f124tem,f134tem
      f14tem='data4tem'
      f14clm='data4clm'
      f124tem='data24tem'
      f134tem='data34tem'
      nfile=1
#endif

C     print *,'*** Top of forwrdstep_atm',iloop,myTime,myIter
      idyear= int((iloop-1)*dtcouple/365.0/24.0) + 1
      iyr= idyear + startYear -1
      inyr = mod(iloop*dtcouple, 365*24)
      IF (inyr .EQ. 0) inyr=jdofmhr(12)
      DO mn=1,12
        IF ((inyr.GT.jdofmhr(mn-1)).AND.(inyr.LE.jdofmhr(mn))) monid=mn
      ENDDO
      inday= mod(iloop*dtcouple, 24)
      dayid= int((inyr-dtcouple-jdofmhr(monid-1))/24.0) +1
C     print *,'*** idyear,iyr,inyr,monid,inday,dayid',
C    &             idyear,iyr,inyr,monid,inday,dayid

      IF (inyr.EQ.dtcouple) THEN !do this block at start of new year
        PRINT *,'*** Starting a new year'
#ifdef NCEPWIND
        WRITE(ncep_yr,'(I4)') (NINT(RAND()*60.0+0.5) + 1947)
        PRINT *,'Using NCEP wind variations from year: ',ncep_yr
         OPEN(6007,
     &     FILE='ncep_taux_variations_'//ncep_yr//'.bin',STATUS='old',
     &     ACCESS='direct', RECL=4*sNx*sNy,
     &     FORM='unformatted')
         OPEN(6008,
     &     FILE='ncep_tauy_variations_'//ncep_yr//'.bin',STATUS='old',
     &     ACCESS='direct', RECL=4*sNx*sNy,
     &     FORM='unformatted')
         OPEN(6009,
     &     FILE='ncep_speed_variations_'//ncep_yr//'.bin',STATUS='old',
     &     ACCESS='direct', RECL=4*sNx*sNy,
     &     FORM='unformatted')
         ncep_counter = 1
#endif
#ifdef DATA4TEM
        IF (nfile.gt.1)THEN
          CLOSE(935)
          CLOSE(937)
          CLOSE(938)
          CLOSE(939)
        ENDIF
        IF(iyr.gt.1000) THEN
           nfile=iyr
        ELSE
          nfile=1000+iyr
        ENDIF
        WRITE (cfile,'i4'),nfile
        f4tem=f14tem//cfile
        f4clm=f14clm//cfile
        f24tem=f124tem//cfile
        f34tem=f134tem//cfile
        OPEN(935,file=f4clm,form='unformatted',status='new')
        OPEN(937,file=f4tem,form='unformatted',status='new')
        OPEN(938,file=f24tem,form='unformatted',status='new')
        OPEN(939,file=f34tem,form='unformatted',status='new')
        nfile=nfile+1
#endif
#ifdef CPL_TEM
        nepan=0.0
        ch4ann=0.0
        n2oann=0.0
        xco2ann=0.0
#endif
#ifdef CPL_OCEANCO2
        temuptann=0.
        DO j=1,jm0
          co24ocnan(j)=0.0
        ENDDO
#  ifdef ML_2D
          call kvcarbon(iyr)
#  endif
#endif
#ifdef CPL_TEM
        DO j=1,jm0
          antemnep(j)=0.
        ENDDO
#  ifndef CPL_CHEM
        CALL robso3(iyr)
#  endif
C For land use
        CALL updatelcluc(idyear)
#endif
#ifdef CPL_CHEM
        print *,' Before eppaemission'
        CALL eppaemission (iyr)
#endif
      ENDIF   !end block done at year-start

      IF (inyr.EQ.jdofmhr(monid-1)+dtcouple) THEN !do this block month start
       PRINT *,'***Starting a new month'
#ifdef CPL_TEM
        CALL zclimate2tem
#endif
#ifdef CPL_OCEANCO2
        ocumn=0.0
        DO j=1,jm0
          fluxco2mn(j)=0.0
        ENDDO
#endif
#ifdef OCEAN_3D
        new_mon= .TRUE.
#endif
      ENDIF  !end block at start of the month
C
C------------------- Top of Coupled Period Loop --------------------------
C

#ifdef OCEAN_3D
#  ifdef NCEPWIND
C     Read in the next timestep of ncep wind stress variations
C      PRINT *,'*** Read in next NCEP record'
      READ(6007, REC=ncep_counter), fu_ncep
      READ(6008, REC=ncep_counter), fv_ncep
      READ(6009, REC=ncep_counter), fs_ncep
      ncep_counter=ncep_counter+1
#  endif
#  ifdef ATM2D_MPI_ON
      CALL CPL_RECV_OCN_FIELDS
#  endif
      CALL GET_OCNVARS( myTime, myIter, myThid)
      IF ( (iloop.NE.1).OR. (iloop.EQ.1.AND.
     &      (startTime.NE.baseTime .OR. nIter0.NE.0)) ) THEN
C       don't run the ice growth/melt on step 1 if "cold" start
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          qPrcRn(i,j) = 0.
         ENDDO
        ENDDO
        CALL THSICE_STEP_FWD( 1, 1, 1, sNx, 1, sNy,
     I                        pass_prcAtm, snowPrc, qPrcRn,
     &                        myTime, myIter, myThid )
        CALL THSICE_AVE( 1,1, myTime, myIter, myThid )
      ENDIF
      CALL CALC_ZONAL_MEANS(.TRUE.,myThid)
      CALL PUT_OCNVARS(myTime,myIter,myThid)
      CALL SUM_YR_END_DIAGS(myTime,myIter,myThid)
#  ifdef ATM2D_MPI_ON
      CALL CPL_SEND_OCN_FIELDS
#  endif
#endif

C      PRINT *,'Top of ncall_atm Loop'
      DO na=1,ncall_atm    !loop for atmos forward time steps
        CALL atmosphere(dtatm,monid)
#ifdef OCEAN_3D
        CALL ATM2OCN_MAIN(iloop, na, monid, myIter, myThid)
        CALL SUM_OCN_FLUXES(myThid)
        CALL PASS_THSICE_FLUXES(myThid)
        CALL THSICE_IMPL_TEMP(netSW, sFlx, dTsurf, 1,1,
     &                        myTime, myIter, myThid)
        CALL SUM_THSICE_OUT(myThid)
        CALL CALC_ZONAL_MEANS(.FALSE.,myThid) !just mean Tsrf recalculated
#endif
      ENDDO  ! ncall_atm loop

C      PRINT *,'Top of ncall_ocean Loop'
      DO no=1,ncall_ocean   !loop for each ocean forward step

#ifdef OCEAN_3D
        iloop_ocn = nint((iloop-1)*dtcouplo/deltaTClock) + no
#  ifndef ATM2D_MPI_ON
        CALL FORWARD_STEP(iloop_ocn, myTime, myIter, myThid )
#  else
        myIter = nIter0 + iloop_ocn
        myTime = startTime + deltaTClock *float (iloop_ocn)
        CALL DO_THE_MODEL_IO( .FALSE., myTime, myIter, myThid )
        CALL DO_WRITE_PICKUP( .FALSE., myTime, myIter, myThid )
#  endif
#endif
#ifdef ML_2D
        CALL ocean_ml(dtocn*3600.,dtatm*3600.)
#endif

      ENDDO ! ncall_ocean loop

C Start of code done at the end of every coupled period

#ifdef OCEAN_3D
      CALL NORM_OCN_FLUXES(myThid)
      CALL ATM2D_WRITE_PICKUP(.FALSE., myTime, myIter, myThid)
#endif

C
C--------------------- End of coupled period loop --------------------
C
      IF (inday.EQ.0) THEN  !do this block if end-of-day
C        PRINT *,'***block at end of day'
      ENDIF  !end block end-of-day

      IF (inyr.EQ.jdofmhr(monid)) THEN !do block if month-end
        PRINT *,'***end of month reached'
#ifdef CLM
#  ifdef CPL_TEM
        CALL climate2tem(monid,ndmonth(monid))
c        PRINT *,'From driver before call tem',' idyear=',idyear
        CALL tem(idyear,monid-1)
        CALL tem2climate(idyear,monid-1)
        ch4mn=0.0
        n2omn=0.0
        nepmn=0.0
        DO j=1,jm0
          ch4mn=ch4mn+temch4(j)
          n2omn=n2omn+temn2o(j)
          nepmn=nepmn+temco2(j)
        ENDDO
#    ifdef CPL_NEM
        PRINT *,'Month=',monid
        PRINT *,'CH4=',ch4mn/1.e9,' N2O=',n2omn/1.e9
        OPEN(277,ACCESS='APPEND',FILE=fnememiss,form='unformatted'
     &     ,STATUS='old')
        WRITE (277) iyr,monid,ch4mn,n2omn,nepmn,
     &          temch4,temn2o,temco2
        CLOSE(277)
#    endif
        DO j=1,jm0
          temnep(monid,j)=temco2(j)
        ENDDO
        DO j=1,jm0
          antemnep(j)=antemnep(j)+temnep(monid,j)
          nepan=nepan+temnep(monid,j)
          ch4ann=ch4ann+temch4(j)
          n2oann=n2oann+temn2o(j)
        ENDDO

#  endif
#endif

#ifdef OCEAN_3D
        CALL MONTH_END_DIAGS( monid, myTime, myIter, myThid)
#endif

#ifdef CPL_OCEANCO2
        IF (monid.EQ.12) THEN
          ocupt=ocupt*12.e-15
C   12.e-15 from moles to Gt carbon
          ocuptp=ocupt
          ocupt=0.0
        ENDIF
#endif

#ifdef IPCC_EMI
        PRINT *,'Month=',monid
        nepmn=nepmn/1000.
C         nepmn is converted from Tg/mn to Gt/mn of C
        ocumn=ocumn*12.e-15
C         ocumn is converted from mole(C) to Gt (C)
C        tnow= jyear + (jday-.5)/365.
C        CALL emissipcc(tnow,nepmn,ocumn,CO2,xco2ann)
         print *,nepmn,ocumn,xco2ann
C         ch4mn is in kg/mn of CH4
C         nepmn is in Gt/mn of C
         tcumn=nepmn-ch4mn*12./16.*1.e-12
         print *,'tcumn,ocumn,xco2ann'
         print *,tcumn,ocumn,xco2ann
        CALL emissipcc_mn(tcumn,ocumn,xco2ann)
C       CALL emissipcc_mn(nepmn,ocumn,xco2ann)
#endif
      ENDIF  !end block done at month-end

      IF (inyr .EQ. jdofmhr(12)) THEN ! do this block at year-end
        PRINT *,'***end of year reached'
#ifdef CPL_TEM
        nepan=nepan/1000.
        IF (iyr.ge.1981.and.iyr.le.1990) THEN
          PRINT *,'Uptake avegaging year=',iyr
          nepav=nepav+nepan
          aocuav=aocuav+OCUPTP
          IF (iyr.eq.1990) THEN
            nepav=nepav/10.
            aocuav=aocuav/10.
            totup=nepav+aocuav
            aduptt=4.1-totup
            PRINT *,' Carbon uptake for spinup'
            PRINT *,' totup=',totup,' aocuav=',aocuav
            PRINT *,' nepav=',nepav,' aduptt=',aduptt
          ENDIF
        ENDIF

        IF (iyr.eq.endYear) THEN
C         NEM emissions and NEP for start of climate-chemistry run
          adupt=aduptt
           CALL wr_rstrt_nem
        ENDIF

#endif

#ifdef ML_2D
C    Data for the restart of the 2D ML model
        CALL wrrstrt_ocean
#endif

#ifdef OCEAN_3D
        IF ((mod(iyr,taveDump).EQ.0).AND.(idyear.GE.taveDump)) THEN
          CALL TAVE_END_DIAGS( taveDump, myTime, myIter, myThid)
        ELSEIF (mod(iyr,taveDump).EQ.0) THEN
          CALL TAVE_END_DIAGS( idyear, myTime, myIter, myThid)
        ENDIF
        CALL YR_END_DIAGS(iyr,myTime,myIter,myThid)
C If necessary, next line can be moved outside OCEAN_3D for IGSM2.2 cleanups
        IF (iloop.EQ.nTimeSteps) CALL ATM2D_FINISH(myThid)
#  ifdef NCEPWIND
        OPEN(unit=334,file='rand_state_new.dat',status='replace')
        WRITE(334,*) JSEED,IFRST,NEXTN
        CLOSE(334)
#  endif
#endif

#if (defined CPL_TEM) || (defined CPL_OCEANCO2)
        PRINT '(a6,i6,2(a5,f10.4))','Year=',iyr,
     &         ' NEP=',nepan,' OCU=',OCUPTP
#  ifdef IPCC_EMI
        PRINT '(a6,i6,(a5,f10.4))','Year=',iyr,
     &         ' CO2AN=',xco2ann/12.
        CALL emissipcc_yr
#  endif

#  ifdef CPL_NEM
        PRINT *,' CH4=',ch4ann,' N2O=',n2oann
#  endif
        OPEN(333,ACCESS='APPEND',FILE=caruptfile,STATUS='old')
#    ifndef CPL_TEM
C       For ocean carbon model only
        WRITE(333,'(i7,3e15.5)')iyr,ocuptp
#    else
#      ifndef CPL_OCEANCO2
C       For ocean TEM only
        WRITE(333,'(i7,3e15.5)')iyr,nepan,nepan-1.e-12*ch4ann*12./16.
#      else
C        For ocean both TEM OCM
        WRITE(333,'(i7,3e15.5)')iyr,nepan,nepan-1.e-12*ch4ann*12./16.
     &                  ,ocuptp
#      endif
#    endif
        CLOSE(333)
#  if (defined CPL_OCEANCO2) && (defined ML_2D)
        WRITE(602)iyr
        CALL wrgary
        CALL zerogary
#  endif
#endif

#ifdef CPL_OCEANCO2
        DO j=1,jm0
#  ifdef OCEAN_3D
          co24ocnan(j)=co24ocnan(j)*dtatm/24.0/365.0
#  else
          co24ocnan(j)=co24ocnan(j)/365.0
#  endif
        ENDDO
        PRINT *,' CO2 for ocean model',' ncallatm=',ncall_atm
        PRINT '(12f7.1,/,2(11f7.1,/),12f7.1)',co24ocnan
#endif

#ifdef CPL_CHEM
        PRINT *,' TEMUPTANN=',temuptann,' TOTAL UPTAKE='
     &          ,ocuptp+temuptann
#endif
      ENDIF  !year-end block

      RETURN
      END

#ifdef NCEPWIND
      REAL*8 FUNCTION RAND()
C
C  This function returns a pseudo-random number for each invocation.
C  It is a FORTRAN 77 adaptation of the "Integer Version 2" minimal
C  standard number generator whose Pascal code appears in the article:
C
C     Park, Steven K. and Miller, Keith W., "Random Number Generators:
C     Good Ones are Hard to Find", Communications of the ACM,
C     October, 1988.
C
      PARAMETER (MPLIER=16807,MODLUS=2147483647,MOBYMP=127773,
     +           MOMDMP=2836)
C
      COMMON  /SEED/JSEED,IFRST,NEXTN
      INTEGER JSEED,IFRST,NEXTN
      INTEGER HVLUE, LVLUE, TESTV
C
      IF (IFRST .EQ. 0) THEN
        NEXTN = JSEED
        IFRST = 1
      ENDIF
C
      HVLUE = NEXTN / MOBYMP
      LVLUE = MOD(NEXTN, MOBYMP)
      TESTV = MPLIER*LVLUE - MOMDMP*HVLUE
      IF (TESTV .GT. 0) THEN
        NEXTN = TESTV
      ELSE
        NEXTN = TESTV + MODLUS
      ENDIF
      RAND = REAL(NEXTN)/REAL(MODLUS)
C
      RETURN
      END
#endif
