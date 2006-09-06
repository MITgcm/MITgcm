      COMMON /DRIVER_VARS/
     &        dtcouple, dtatm, dtocn, startYear, endYear, taveDump,
     &        ncall_atm, ncall_ocean
       INTEGER dtcouple  ! coupling period (hrs)
       INTEGER dtatm     ! atmos time step (hrs)
       INTEGER dtocn     ! ocean time step (hrs)
       INTEGER startYear ! starting year of simulation (jan 1)
       INTEGER endYear   ! ending year of simulation (dec 31)
       INTEGER taveDump  ! frequency of tave dumps for atm2ocn fluxes (yrs)     
       INTEGER ncall_atm ! number of atm steps in a coupling period   
       INTEGER ncall_ocean ! number of ocn steps in a coupling period  

#ifdef CPL_TEM
      COMMON /DRIVER_VAR_TEM/
     &          nepan, nepav, n2oann, n2omn, nepmn,
     &          aocuav, ch4ann, ch4mn, xco2ann, nfile
       REAL*4 nepan, nepav, n2oann, n2omn, nepmn
       REAL*4 aocuav, ch4ann, ch4mn, xco2ann
       INTEGER nfile
#endif

#ifdef CPL_OCEANCO2 
      COMMON /DRIVER_VAR_CO2/
     &         ocumn, ocuptp, fluxco2mn
       REAL*4 ocumn
       REAL*4 ocuptp
       REAL*4 fluxco2mn(jm0)

      COMMON /GARYFLUX/pC_atm, wind_amp, fluxco2
       REAL*4 pC_atm(jm0)
       REAL*4 wind_amp
       REAL*4 fluxco2(jm0)
#endif

#ifdef IPCC_EMI
      COMMON /DRIVER_VAR_IPCC/nemis
       INTEGER nemis

      COMMON/ BACKGRGHG/ghgbgr
       REAL*4 ghgbgr(5)
#endif

#ifdef CLM 
#  include "CLM.COM"
#  ifdef CPL_TEM  
#    include "TEM.COM"
      external tem2climate,updatelcluc,tem,tem_init,lcluc_init,
     &   temclm_init
#  endif
#endif
