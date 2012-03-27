C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_driver.F,v 1.43 2012/03/27 15:48:27 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
       SUBROUTINE FIZHI_DRIVER (myid,im,jm,lm,bi,bj,ptracer,ntracer,
     & turbStart, xlats,xlons,
     & p,u,v,t,q,pl,ple,dpres,pkht,pkl,surfz,fracland,landtype,radswt,
     & phis_var,tgz,sea_ice,
     & nchp,chlat,chlon,igrd,nchptot,nchpland,chfr,ityp,
     & tcanopy,tdeep,ecanopy,swetshal,swetroot,swetdeep,capac,snodep,
     & ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     & albvisdr,albvisdf,albnirdr,albnirdf,emiss,alai,agrn,
     & chemq,chemo3,co2,cfc11,cfc12,cfc22,methane,n2o,
     & lwdt,lwdtclr,swdt,swdtclr,turbu,turbv,turbt,turbq,
     & moistu,moistv,moistt,moistq,
     & radswg,swgclr,fdirpar,fdifpar,osr,osrclr,tg0,radlwg,lwgclr,
     & st4,dst4,dlwdtg,rainlsp,raincon,snowfall,iras,nlwcld,
     & cldtot_lw,cldras_lw,cldlsp_lw,nlwlz,lwlz,nswcld,cldtot_sw,
     & cldras_sw,cldlsp_sw,nswlz,swlz,imstturbsw,imstturblw,qliqavesw,
     & qliqavelw,fccavesw,fccavelw,qq)
C***********************************************************************
C  Purpose
C  -------
C     Driver for the FIZHI high-end Atmospheric Physics
C
C  Arguments  Description
C  ----------------------
C     nymd ..... Current YYMMDD
C     nhms ..... Current HHMMSS
C     fracland.. Land Fractions
C     landtype.. Land Vegetation Types
C     radswt ... Incident Solar Radiation
C
C***********************************************************************
      IMPLICIT NONE

c Diagnostic Common
c -----------------
#ifdef ALLOW_DIAGNOSTICS
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
#endif

c Timers Common
c -------------
#include "chronos.h"

c Input Parameters
c ----------------
      INTEGER myid,im,jm,lm,bi,bj,ptracer,ntracer
      LOGICAL turbStart
      INTEGER nchp,igrd(nchp),nchptot,nchpland,ityp(nchp)
      INTEGER iras,nlwcld,nlwlz,nswcld,nswlz
      INTEGER imstturbsw,imstturblw

      _RL xlats(im,jm), xlons(im,jm)
      _RL p(im,jm)
      _RL u(im,jm,lm),v(im,jm,lm),t(im,jm,lm)
      _RL q(im,jm,lm,ntracer)
      _RL pl(im,jm,lm),ple(im,jm,lm+1),dpres(im,jm,lm)
      _RL pkht(im,jm,lm+1)
      _RL pkl(im,jm,lm)
      _RL surfz(im,jm)
      _RL radswt(im,jm), fracland(im,jm)
      INTEGER landtype(im,jm)
      _RL phis_var(im,jm), sea_ice(im,jm)
      _RL chlat(nchp),chlon(nchp),chfr(nchp)
      _RL tcanopy(nchp),tdeep(nchp),ecanopy(nchp),swetshal(nchp)
      _RL swetroot(nchp),swetdeep(nchp),capac(nchp),snodep(nchp)
      _RL ctmt(nchp),xxmt(nchp),yymt(nchp),zetamt(nchp)
      _RL xlmt(nchp,lm),khmt(nchp,lm),tke(nchp,lm)
      _RL co2,cfc11,cfc12,cfc22,methane(lm),n2o(lm)
      _RL lwdt(im,jm,lm),lwdtclr(im,jm,lm)
      _RL swdt(im,jm,lm),swdtclr(im,jm,lm)
      _RL turbu(im,jm,lm),turbv(im,jm,lm),turbt(im,jm,lm)
      _RL turbq(im,jm,lm,ntracer)
      _RL moistu(im,jm,lm),moistv(im,jm,lm),moistt(im,jm,lm)
      _RL moistq(im,jm,lm,ntracer)
      _RL chemo3(im,jm,lm),chemq(im,jm,lm)
      _RL albvisdr(im,jm),albvisdf(im,jm),albnirdr(im,jm)
      _RL albnirdf(im,jm),emiss(im,jm,10)
      _RL alai(nchp),agrn(nchp)
      _RL radswg(im,jm),swgclr(im,jm)
      _RL fdirpar(im,jm),fdifpar(im,jm),osr(im,jm),osrclr(im,jm)
      _RL tg0(im,jm),radlwg(im,jm),lwgclr(im,jm),st4(im,jm)
      _RL dst4(im,jm)
      _RL dlwdtg(im,jm,lm)
      _RL rainlsp(im,jm),raincon(im,jm),snowfall(im,jm)
      _RL cldtot_lw(im,jm,lm),cldras_lw(im,jm,lm)
      _RL cldlsp_lw(im,jm,lm)
      _RL lwlz(im,jm,lm)
      _RL cldtot_sw(im,jm,lm),cldras_sw(im,jm,lm)
      _RL cldlsp_sw(im,jm,lm)
      _RL swlz(im,jm,lm)
      _RL qliqavesw(im,jm,lm),qliqavelw(im,jm,lm)
      _RL fccavesw(im,jm,lm),fccavelw(im,jm,lm)
      _RL qq(im,jm,lm)
      _RL tgz(im,jm)

c Local Variables
c ---------------
      _RL rfu(im,jm,lm),rfv(im,jm,lm),rft(im,jm,lm)
      LOGICAL     alarm
      EXTERNAL    alarm
      INTEGER numpcheck
      PARAMETER (numpcheck = 5)
      INTEGER pchecklevs(numpcheck)
      _RL pcheckpress(numpcheck)
C     data pcheckpress/950.,750.,700.,400.,10./
      data pcheckpress/950.,850.,700.,400.,10./

      INTEGER low_level,mid_level,nltop,nsubmin,nsubmax,Lup
      INTEGER ndmoist,ndturb,ndlw,ndsw
      INTEGER istrip,npcs
      INTEGER i,j
      INTEGER ndpnt
      INTEGER ndum0, ndum1, ndum2
      _RL akap,getcon
      _RL ptop
      LOGICAL lpnt,cumfric
      INTEGER imglobal

      istrip = im*jm
      npcs   = 1

      ptop   =  0.
      akap = getcon('KAPPA')

C **********************************************************************
C ****                         Initialization                       ****
C **********************************************************************
      call get_alarm ( 'radlw',ndum0,ndum1, ndlw   ,ndum2 )
      call get_alarm ( 'radsw',ndum0,ndum1, ndsw   ,ndum2 )
      call get_alarm ( 'turb' ,ndum0,ndum1, ndturb ,ndum2 )
      call get_alarm ( 'moist',ndum0,ndum1, ndmoist,ndum2 )

      call get_alarm ( 'pnt'  ,ndum0,ndum1, ndpnt  ,ndum2 )
      lpnt = ndpnt.ne.0

C Fill array of model levels closest to a given pressure value

      call getpwhere(myid,numpcheck,pcheckpress,pchecklevs)

      low_level = pchecklevs(3)
      mid_level = pchecklevs(4)
      nltop = pchecklevs(5)
      nsubmin = pchecklevs(1)
      nsubmax = pchecklevs(2)
      Lup = pchecklevs(3)

CCC   cumfric = .true.
      cumfric = .false.

C **********************************************************************
C ****                  Call Physics Mini-Drivers                   ****
C **********************************************************************

C SHORT WAVE RADIATION
C ====================
      IF ( alarm('radsw') ) THEN

      call swrio ( nymd,nhms,bi,bj,ndsw,myid,istrip,npcs,
     &   low_level,mid_level,im,jm,lm,
     &   p,pl,ple,dpres,pkht,pkl,t,chemq,chemo3,co2,
     &   albvisdr,albvisdf,albnirdr,albnirdf,swdt,swdtclr,
     &   radswg,swgclr,fdifpar,fdirpar,osr,osrclr,
     &   ptop,nswcld,cldtot_sw,cldras_sw,nswlz,swlz,
     &   .false.,imstturbsw,qliqavesw,
     &   fccavesw,landtype,xlats,xlons )

      ENDIF

C LONG WAVE RADIATION
C ===================
      IF ( alarm('radlw') ) THEN

c Set Reference Ground Temperature
c --------------------------------
      do j=1,jm
      do i=1,im
      tg0(i,j) = tgz(i,j)
      enddo
      enddo

      call lwrio ( nymd,nhms,bi,bj,myid,istrip,npcs,
     &   low_level,mid_level,im,jm,lm,
     &   p,pl,ple,dpres,pkht,pkl,t,chemq,chemo3,co2,
     &   cfc11,cfc12,cfc22,methane,n2o,emiss,tgz,radlwg,st4,dst4,
     &   lwdt,dlwdtg,lwdtclr,lwgclr,
     &   nlwcld,cldtot_lw,cldras_lw,nlwlz,lwlz,
     &   .false.,imstturblw,qliqavelw,
     &   fccavelw,landtype )

      ENDIF

C TURBULENCE
C ==========
      IF ( alarm('turb') ) THEN

      call turbio (im,jm,lm,istrip,nymd,nhms,bi,bj,
     &  turbStart, ndturb,nltop,
     &  ptop,p,u,v,t,q,ntracer,ptracer,pl,ple,dpres,pkht,pkl,
     &  ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     &  tgz,fracland,landtype,
     &  tcanopy,ecanopy,tdeep,swetshal,swetroot,swetdeep,snodep,capac,
     &  nchp,nchptot,nchpland,chfr,chlat,chlon,igrd,ityp,
     &  alai,agrn,sea_ice,lpnt,
     &  turbu,turbv,turbt,turbq,radlwg,st4,dst4,radswg,radswt,
     &  fdifpar,fdirpar,rainlsp,raincon,snowfall,tg0,
     &  imstturblw,imstturbsw,qliqavelw,qliqavesw,fccavelw,fccavesw,qq,
     &  myid)

c Add Gravity-Wave Drag Tendency
c ------------------------------
C Comment this out for now

      imglobal = 128
      call gwdrag (myid,p,pl,ple,dpres,pkl,u,v,t,q,phis_var,
     &   turbu,turbv,turbt,im,jm,lm,bi,bj,istrip,npcs,imglobal)

c Add Rayleigh Friction Damping Above 70 Km
c -----------------------------------------

      call rayleigh(myid,pl,pkl,pkht,surfz,u,v,t,q,im,jm,lm,
     &                                                bi,bj,rfu,rfv,rft)
C Now Add Rayleigh Friction Tendencies to Turb Tendency

C     do L=1,lm
C     do j=1,jm
C     do i=1,im
C      turbu(i,j,L) = turbu(i,j,L) + rfu(i,j,L)
C      turbv(i,j,L) = turbv(i,j,L) + rfv(i,j,L)
C      turbt(i,j,L) = turbt(i,j,L) + rft(i,j,L)
C     enddo
C     enddo
C     enddo

      endif

C MOIST PROCESSES
C ===============
      if ( alarm('moist') ) then
      call moistio (ndmoist,istrip,npcs,
     &    low_level,mid_level,nltop,nsubmin,nsubmax,Lup,
     &    p,pl,ple,dpres,pkht,pkl,u,v,t,q,bi,bj,ntracer,ptracer,qq,
     &    moistu,moistv,moistt,moistq,cumfric,im,jm,lm,ptop,iras,
     &    rainlsp,raincon,snowfall,
     &    nswcld,cldtot_sw,cldras_sw,cldlsp_sw,nswlz,swlz,
     &    nlwcld,cldtot_lw,cldras_lw,cldlsp_lw,nlwlz,lwlz,
     &              .false.,myid)
      endif

      return
      end
