C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_turb.F,v 1.50 2012/03/27 15:48:27 jmc Exp $
C $Name:  $
#include "FIZHI_OPTIONS.h"

C--  File fizhi_turb.F:
C--   Contents
C--   o TURBIO
C--   o TRBFLX
C--   o SFCFLX
C--   o PHI
C--   o PSI
C--   o TRBLEN
C--   o TRBITP
C--   o TRBL20
C--   o TRBL25
C--   o TRBDIF
C--   o VTRI0
C--   o VTRI1
C--   o VTRI2
C--   o LINADJ
C--   o ZCSUB
C--   o SEAICE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE TURBIO (im,jm,nlay,istrip,nymd,nhms,bi,bj, qbeg
     1 ,ndturb,nltop,ptop, pz, uz, vz, tz, qz, ntracers,ptracers
     2 ,plz,plze,dpres,pkht,pkz,ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke
     3 ,tgz,fracland,landtype
     4 ,tcanopy,ecanopy,tdeep,swetshal,swetroot,swetdeep,snodep,capac
     5 ,nchp,nchptot,nchplnd,chfr,chlt,chlon,igrd,ityp,alai,agrn,thkz
     6 ,tprof
     8 ,duturb, dvturb, dtturb,dqturb,radlwg,st4,dst4,radswg,radswt
     9 ,fdifpar,fdirpar,rainlsp,rainconv,snowfall,tempref
     1 ,imstturblw,imstturbsw,qliqavelw,qliqavesw,fccavelw,fccavesw
     2 ,qqgrid,myid)
C-----------------------------------------------------------------------
C subroutine turbio - model interface routine to trbflx, the turbulence
C        parameterization, and tile, the land surface parameterization
C  input:
C      im      - number of points in the longitude direction
C      jm      - number of points in the latitude direction
C      nlay    - number of vertical levels
C      istrip  - number of horizontal points to be handled at a time on
C      nymd    - year and date integer in YYMMDD format (ie, 790212)
C      nhms    - date and time integer in HHMMSS format (ie, 123000)
C      qbeg    - set to true to indicate a cold start for turbulence
C      ndturb  - turbulence time step integer in HHMMSS format
C      nltop   - Top level at which to allow turbulence
C      ptop    - model top pressure - rigid lid assumed - real
C      pz      - surface pressure minus ptop in mb - real[lon,lat]
C      uz      - zonal wind in m/sec - real[lon,lat,level]
C      vz      - meridional wind in m/sec - real[lon,lat,level]
C      tz      - model theta (theta [deg K]/p0**k) - real[lon,lat,level]
C      qz      - specific humidity in kg/kg - real[lon,lat,level]
C      ntracers- total number of tracers - integer
C      ptracers- number of permanent tracers - integer
C      pkht    - pressure[mb]**k at bottom edges of levels - real[lon,lat,level]
C      fracland- not being used - real[lon,lat]
C      landtype- not being used - integer[lon,lat]
C      nchp    - nchplnd<nchp - total no chips (ocean too) - integer
C      nchplnd - <=nchp - number of land chips - integer
C      chfr    - chip fraction - real[nchp]
C      chlt    - tile space latitude array - real[nchp]
C      chlon   - tile space longitude array - real[nchp]
C      igrd    - tile space grid number - integer[nchp]
C      ityp    - tile space vegetation type - integer[nchp]
C      alai    - leaf area index - real[nchp]
C      agrn    - greenness fraction - real[nchp]
C      thkz    - sea ice thickness in m (0. for no ice) - real[lon,lat]
C      tprof   - logical flag for point by point diagnostic output
C      radlwg  - net longwave flux at ground (up-down) in w/m**2 - real[lon,lat]
C      st4     - upward longwave flux at ground in w/m**2 - real[lon,lat]
C      dst4    - delta-sigma-T**4, ie, derivative of upward lw flux at
C                ground with respect to ground Temperature - real[lon,lat]
C      radswg  - net shortwave flux at ground (down-up) NON-DIM - real[lon,lat]
C                  {NOTE: this field is divided by the incident shortwave
C                     at the top of the atmosphere to non-dimensionalize]
C      radswt  - incident shortwave at top of atmos in W/m**2 - real[lon,lat]
C      fdifpar - incident diffuse-beam PAR at surface in W/m**2 - real[lon,lat]
C      fdirpar - incident direct-beam PAR at surface in W/m**2 - real[lon,lat]
C      rainlsp - large-scale (frontal,supersat) rainfall in mm/sec - real[lon,lat]
C      rainconv- convective rainfall rate in mm/sec - real[lon,lat]
C      snowfall- total snowfall rate in mm/sec - real[lon,lat]
C updated:
C      tke     - turbulent k.e. in m**2/s**2 - real[tiles,levels]
C      tgz     - surface skin temperature in deg K - real[lon,lat]
C      tcanopy - canopy temperature in deg K real[tiles]
C                 (sea surface temp over the ocean tiles)
C      ecanopy - canopy vapor pressure in mb real[tiles]
C                 (qstar at tground over the sea ice and ocean tiles)
C      tdeep   - deep soil temp in deg K real[tiles]
C      swetshal- shallow level moisture field capacity fraction real[tiles]
C      swetroot- root level moisture field capacity fraction real[tiles]
C      swetdeep- deep soil level moisture field capacity fraction real[tiles]
C      snodep  - depth of snow pack in cm liquid water equiv real[tiles]
C      capac   - leaf canopy water reservoir in cm real[tiles]
C output:
C      duturb  - change in zonal wind component due to turbulent processes
C                per unit time in m/sec**2 - real[lon,lat,levels]
C      dvturb  - change in meridional wind component due to turbulent processes
C                per unit time in m/sec**2 - real[lon,lat,levels]
C      dtturb  - change in (model theta*pi) due to turbulent processes
C                per unit time  - real[lon,lat,levels] !! pi is pressure-ptop
C      dqturb  - change in (specific humidity*pi) due to turbulent processes
C                per unit time  - real[lon,lat,levels] !! pi is pressure-ptop
C      qliqavelw - Moist   Turbulence Liquid Water   for Longwave  - real[lon,lat,levels]
C      qliqavesw - Moist   Turbulence Liquid Water   for Shortwave - real[lon,lat,levels]
C      fccavelw  - Moist   Turbulence Cloud Fraction for Longwave  - real[lon,lat,levels]
C      fccavesw  - Moist   Turbulence Cloud Fraction for Shortwave - real[lon,lat,levels]
C      qqgrid    - Gridded Turbulent Kinetic Energy                - real[lon,lat,levels]
C-----------------------------------------------------------------------
      implicit none

      integer im,jm,nlay,istrip,nymd,nhms,bi,bj,ndturb,nltop
      integer ntracers, ptracers
      integer nchp,nchptot,nchplnd
      logical qbeg
      _RL ptop
      _RL pz(im*jm,1),uz(im*jm,1,nlay),vz(im*jm,1,nlay)
      _RL tz(im*jm,1,nlay),qz(im*jm,1,nlay,ntracers)
      _RL plz(im*jm,1,nlay),plze(im*jm,1,nlay+1),dpres(im*jm,1,nlay)
      _RL pkht(im*jm,1,nlay+1),pkz(im*jm,1,nlay)
      _RL ctmt(nchp),xxmt(nchp),yymt(nchp),zetamt(nchp)
      _RL xlmt(nchp,nlay),khmt(nchp,nlay),tke(nchp,nlay)
      _RL tgz(im*jm,1),fracland(im*jm,1)
      integer landtype(im*jm,1)
      _RL tcanopy(nchp),tdeep(nchp),ecanopy(nchp),swetshal(nchp)
      _RL swetroot(nchp),swetdeep(nchp),snodep(nchp),capac(nchp)
      _RL chfr(nchp),chlt(nchp),chlon(nchp)
      integer igrd(nchp),ityp(nchp)
      _RL alai(nchp),agrn(nchp),thkz(im*jm,1)
      logical tprof
      _RL duturb(im*jm,1,nlay),dvturb(im*jm,1,nlay)
      _RL dtturb(im*jm,1,nlay),dqturb(im*jm,1,nlay,ntracers)
      _RL st4(im*jm,1),dst4(im*jm,1)
      _RL radswg(im*jm,1),radswt(im*jm,1),radlwg(im*jm,1)
      _RL fdifpar(im*jm,1),fdirpar(im*jm,1)
      _RL rainlsp(im*jm,1),rainconv(im*jm,1),snowfall(im*jm,1)
      _RL tempref (im*jm,1)
      integer imstturblw, imstturbsw
      _RL qliqavesw(im*jm,1,nlay),qliqavelw(im*jm,1,nlay)
      _RL fccavelw (im*jm,1,nlay),fccavesw (im*jm,1,nlay)
      _RL qqgrid   (im*jm,1,nlay)
      integer myid

C Local Variables

      integer numstrips
      integer ijall
      _RL fmu,hice,tref,pref,cti,ed
C Set fmu and ed to zero for no background diffusion
      parameter ( fmu    = 0.00000    )
      parameter ( hice   =   300.     )
      parameter ( tref   =   258.     )
      parameter ( pref   =   500.     )
      parameter ( cti    =   0.0052   )
      parameter ( ed     =   0.0      )

      _RL qliqtmp(im*jm,1,nlay)
      _RL  fcctmp(im*jm,1,nlay)
      _RL tmpdiag(im*jm,1), tmpFac
      _RL   thtgz(im*jm)
      logical  diagnostics_is_on
      external diagnostics_is_on

      integer nland
      _RL alwcoeff(nchp),blwcoeff(nchp)
      _RL netsw(nchp)
      _RL cnvprec(nchp),lsprec(nchp)
      _RL snowprec(nchp)
      _RL pardiff(nchp),pardirct(nchp)
      _RL pmsc(nchp)
      _RL netlw(nchp)
      _RL sqscat(nchp), rsoil1(nchp)
      _RL rsoil2(nchp)
      _RL rdc(nchp),u2fac(nchp)
      _RL z2ch(nchp)
      _RL zoch(nchp),cdrc(nchp)
      _RL cdsc(nchp)
      _RL dqsdt(nchp)
      _RL tground(nchp),qground(nchp)
      _RL utility(nchp)
      _RL    qice(nchp)
      _RL   dqice(nchp)

      _RL dumsc(nchp,nlay),dvmsc(nchp,nlay)
      _RL dtmsc(nchp,nlay),dqmsc(nchp,nlay,ntracers)

      _RL shg(nchp),z0(nchp),icethk(nchp)
      integer water(nchp)

      _RL lats(istrip),lons(istrip),cosz(istrip),icest(istrip)
      _RL rainls(istrip),raincon(istrip),newsnow(istrip)
      _RL pardf(istrip),pardr(istrip),swnet(istrip)
      _RL hlwdwn(istrip),alwrad(istrip),blwrad(istrip)
      _RL tmpnlay(istrip)
      _RL laistrip(istrip),grnstrip(istrip),z2str(istrip),cd(istrip)
      _RL scatstr(istrip), rs1str(istrip), rs2str(istrip)
      _RL rdcstr(istrip),u2fstr(istrip),dqsdtstr(istrip)
      _RL eturb(istrip),dedqa(istrip),dedtc(istrip)
      _RL hsturb(istrip),dhsdqa(istrip),dhsdtc(istrip)
      _RL savetc(istrip),saveqa(istrip),lwstrip(istrip)
      _RL chfrstr(istrip),psurf(istrip),shgstr(istrip)
      integer types(istrip),igrdstr(istrip)
      _RL evap(istrip),shflux(istrip),runoff(istrip),bomb(istrip)
      _RL eint(istrip),esoi(istrip),eveg(istrip),esno(istrip)
      _RL smelt(istrip),hlatn(istrip),hlwup(istrip),gdrain(istrip)
      _RL runsrf(istrip),fwsoil(istrip),evpot(istrip)
      _RL strdg1(istrip),strdg2(istrip),strdg3(istrip),strdg4(istrip)
      _RL strdg5(istrip),strdg6(istrip),strdg7(istrip),strdg8(istrip)
      _RL strdg9(istrip),tmpstrip(istrip),qicestr(istrip)
      _RL dqicestr(istrip)

      _RL u(istrip,nlay+1), v(istrip,nlay+1), th(istrip,nlay+1)
      _RL sh(istrip,nlay+1), thv(istrip,nlay+1), pe(istrip,nlay+1)
      _RL tracers(istrip,nlay+1,ntracers)
      _RL dpstr(istrip,nlay),pke(istrip,nlay+1)
      _RL pk(istrip,nlay), qq(istrip,nlay),   p(istrip,nlay)
      _RL sri(istrip,nlay), skh(istrip,nlay), skm(istrip,nlay)
      _RL stuflux(istrip,nlay), stvflux(istrip,nlay)
      _RL sttflux(istrip,nlay), stqflux(istrip,nlay)
      _RL frqtrb(istrip,nlay-1)
      _RL dshdthg(istrip,nlay),dthdthg(istrip,nlay)
      _RL dshdshg(istrip,nlay),dthdshg(istrip,nlay)
      _RL transth(istrip,nlay), transsh(istrip,nlay)

      _RL tc(istrip),td(istrip),qa(istrip)
      _RL swet1(istrip),swet2(istrip),swet3(istrip)
      _RL capacity(istrip),snowdepth(istrip)
      _RL stz0(istrip)
      _RL stdiag(istrip)
      _RL tends(istrip),sustar(istrip), sz0(istrip),pbldpth(istrip)
      _RL sct(istrip), scu(istrip), swinds(istrip)
      _RL stu2m(istrip),stv2m(istrip),stt2m(istrip),stq2m(istrip)
      _RL stu10m(istrip),stv10m(istrip),stt10m(istrip),stq10m(istrip)
      integer  stwatr(istrip)
      _RL  wspeed(istrip)

      _RL ctsave(istrip),xxsave(istrip),yysave(istrip)
      _RL zetasave(istrip)
      _RL xlsave(istrip,nlay),khsave(istrip,nlay)
      _RL qliq(istrip,nlay),turbfcc(istrip,nlay)
      _RL qliqmsc(nchp,nlay),fccmsc(nchp,nlay)

      _RL pi,secday,sdayopi2,rgas,akap,cp,alhl
      _RL faceps,grav,caltoj,virtcon,getcon
      _RL heatw,undef,timstp,delttrb,dttrb,ra
      _RL edle,rmu,cltj10,atimstp,tice,const
      integer istnp1,istnlay,itrtrb,i,L,nn,nt
      integer nocean, nice
      integer ndmoist,time_left, ndum0,ndum1
      integer ntracedim
      _RL    dtfac,timstp2

      integer n,nsecf,nmonf,ndayf
      nsecf(n) = n/10000*3600 + mod(n,10000)/100* 60 + mod(n,100)
      nmonf(n) = mod(n,10000)/100
      ndayf(n) = mod(n,100)

#ifdef CRAY
#ifdef f77
cfpp$ expand (qsat)
#endif
#endif

C   compute variables that do not change

       pi = 4.*atan(1.)
       secday   = getcon('SDAY')
       sdayopi2 = getcon('SDAY') / (pi*2.)
       rgas     = getcon('RGAS')
       akap     = getcon('KAPPA')
       cp       = getcon('CP')
       alhl     = getcon('LATENT HEAT COND')
       faceps   = getcon('EPSFAC')
       grav     = getcon('GRAVITY')
       caltoj   = getcon('CALTOJ')
       virtcon  = getcon('VIRTCON')
       heatw    = getcon('HEATW')
       undef    = getcon('UNDEF')
       ntracedim= max(ntracers-ptracers,1)

       call get_alarm ( 'moist',ndum0,ndum1,ndmoist,time_left )
       timstp   = nsecf(ndturb)
       timstp2  = nsecf(ndmoist)
       dtfac    = min( 1.0 _d 0, timstp/timstp2 )

C delttrb is the internal turbulence time step
C a value equal to ndturb means one internal iteration
       delttrb = nsecf(ndturb)

       ijall    =   im * jm
       istnp1   =   istrip * (nlay+1)
       istnlay  =   istrip * nlay
       itrtrb   = ( timstp / delttrb ) + 0.1
       dttrb    =   timstp / float(itrtrb)
       edle     =   ed * 0.2

C   coefficient of viscosity (background momentum diffusion)

       rmu     = fmu * tref * rgas / pref
       cltj10  = 10. * caltoj
       atimstp = 1. / timstp
       tice = getcon('FREEZING-POINT')

C **********************************************************************
C                            Initialization
C **********************************************************************

C Initialize diagnostic for ground temperature change
C ---------------------------------------------------
      tmpFac = -1.
      CALL DIAGNOSTICS_SCALE_FILL( tgz,tmpFac,1,
     &                             'DTG     ',0,1,-3,bi,bj,myid)

C **********************************************************************
C  entire turbulence and land surface package will run in 'tile space'
C       do conversion of model state variables to tile space
C        (ocean points appended to tile space land point arrays)
C **********************************************************************

      numstrips   = ((nchptot-1)/istrip) + 1

      call grd2msc(pz(1,1),im,jm,igrd,pmsc,nchp,nchptot)

      call grd2msc(tgz,im,jm,igrd,tground,nchp,nchptot)
      do i = 1,ijall
       tmpdiag(i,1) = st4(i,1) + dst4(i,1)*(tgz(i,1)-tempref(i,1))
     1                         - dst4(i,1)* tgz(i,1)
      enddo
      call grd2msc(tmpdiag,im,jm,igrd,alwcoeff,nchp,nchptot)
      do i = 1,ijall
       tmpdiag(i,1) = dst4(i,1)
      enddo
      call grd2msc(tmpdiag,im,jm,igrd,blwcoeff,nchp,nchptot)
      do i = 1,ijall
       tmpdiag(i,1) = fdifpar(i,1) * radswt(i,1)
      enddo
      call grd2msc(tmpdiag,im,jm,igrd,pardiff,nchp,nchptot)
      do i = 1,ijall
       tmpdiag(i,1) = fdirpar(i,1) * radswt(i,1)
      enddo
      call grd2msc(tmpdiag,im,jm,igrd,pardirct,nchp,nchptot)
      do i = 1,ijall
       tmpdiag(i,1) = radswg(i,1) * radswt(i,1)
      enddo
      call grd2msc(tmpdiag,im,jm,igrd,netsw,nchp,nchptot)
      do i = 1,ijall
       tmpdiag(i,1) = radlwg(i,1) + dst4(i,1)*(tgz(i,1)-tempref(i,1))
      enddo
      call grd2msc(tmpdiag,im,jm,igrd,netlw,nchp,nchptot)
      call grd2msc(thkz,im,jm,igrd,icethk,nchp,nchptot)
      call grd2msc(rainlsp,im,jm,igrd,lsprec,nchp,nchptot)
      call grd2msc(rainconv,im,jm,igrd,cnvprec,nchp,nchptot)
      call grd2msc(snowfall,im,jm,igrd,snowprec,nchp,nchptot)

C Call chpprm to get non-varying vegetation and soil characteristics

      call chpprm(nymd,nhms,nchp,nchplnd,chlt,ityp,alai,
     1       agrn,zoch,z2ch,cdrc,cdsc,sqscat,u2fac,rsoil1,rsoil2,rdc)

C **********************************************************************
C ****                surface specification                         ****
C **********************************************************************

C   set water

      do i = 1,nchptot
       water(i) = 0
       if((ityp(i).eq.100).and.(icethk(i).eq.0. ))water(i) = 1
      enddo

C   roughness length z0

      do i =1,nchptot
       if (icethk(i).gt.0.) then
        z0(i) = 1.e-4
       else if (ityp(i).eq.100) then
        z0(i) = 3.e-4
       else
        z0(i) = zoch(i)
       endif
      enddo

C Fill Array Tground with canopy temperatures over land tiles
C  (it has sst from the tgz array over the sea ice and ocean tiles)

      do i = 1,nchplnd
       tground(i) = tcanopy(i)
      enddo

C value of sh at ground
C ---------------------
      do I =1,nchptot
      utility(I) = pmsc(i) + ptop
      call qsat ( tground(i),utility(i),shg(i),dqsdt(i),.true. )
      enddo

C Fill Array Qground with canopy air specific humidity over land tiles
C  (it has qstar at tground over the sea ice and ocean tiles)

      do i = 1,nchplnd
       qground(i) = ecanopy(i)
      enddo
      do i = nchplnd+1,nchptot
       qground(i) = shg(i)
      enddo

C Fill Array Swetshal with Value 1. over oceans and sea ice
      do i = nchplnd+1,nchptot
       swetshal(i) = 1.
      enddo

C compute heat conduction through ice
C -----------------------------------
      const = ( cti / hice ) * cltj10
      do i =1,nchptot
             qice(i) =  0.0
            dqice(i) =  0.0
       if( icethk(i).gt.0.0 ) then
             qice(i) =  const*(tice-tground(i))
            dqice(i) = -const
       endif
      enddo

      if(diagnostics_is_on('QICE    ',myid) ) then
       do i =1,ijall
        tmpdiag(i,1) = 0.0
       enddo
       call msc2grd (igrd,chfr,qice,nchp,nchptot,fracland,tmpdiag,im,jm)
       call diagnostics_fill(tmpdiag,'QICE    ',0,1,3,bi,bj,myid)
      endif

C***********************************************************************
C                        loop over regions
C***********************************************************************

      do 2000 nn = 1, numstrips

       call strip2tile(uz,igrd,u,nchp,ijall,istrip,nlay,nn)
       call strip2tile(vz,igrd,v,nchp,ijall,istrip,nlay,nn)
       call strip2tile(tz,igrd,th,nchp,ijall,istrip,nlay,nn)
       call strip2tile(qz(1,1,1,1),igrd,sh,nchp,ijall,istrip,nlay,nn)
       call strip2tile(dpres,igrd,dpstr,nchp,ijall,istrip,nlay,nn)
       call strip2tile(plz,igrd,p,nchp,ijall,istrip,nlay,nn)
       call strip2tile(plze,igrd,pe,nchp,ijall,istrip,nlay+1,nn)
       call strip2tile(pkz,igrd,pk,nchp,ijall,istrip,nlay,nn)
       call strip2tile(pkht,igrd,pke,nchp,ijall,istrip,nlay+1,nn)
c      do nt = 1,ntracers-ptracers
c      call strip2tile(qz(1,1,1,ptracers+nt),igrd,tracers(1,1,nt),nchp,
c    1                                             ijall,istrip,nlay,nn)
c      enddo

       call stripit  (z0,stz0,nchptot,nchp,istrip,1,nn)
       call stripit  (tground,th(1,nlay+1),nchptot,nchp,istrip,1,nn)
       call stripit  (pmsc,pe(1,nlay+1),nchptot,nchp,istrip,1,nn)
       call stripit  (tke,qq,nchptot,nchp,istrip,nlay-1,nn)
       call stripit  (ctmt,ctsave,nchptot,nchp,istrip,1,nn)
       call stripit  (xxmt,xxsave,nchptot,nchp,istrip,1,nn)
       call stripit  (yymt,yysave,nchptot,nchp,istrip,1,nn)
       call stripit  (zetamt,zetasave,nchptot,nchp,istrip,1,nn)
       call stripit  (xlmt,xlsave,nchptot,nchp,istrip,nlay,nn)
       call stripit  (khmt,khsave,nchptot,nchp,istrip,nlay,nn)
       call stripitint (water,stwatr,nchptot,nchp,istrip,1,nn)

       call stripitint (igrd,igrdstr,nchptot,nchp,istrip,1,nn)
       call stripit  (chfr,chfrstr,nchptot,nchp,istrip,1,nn)
       call stripit  (icethk,icest,nchptot,nchp,istrip,1,nn)
       call stripit  (pardiff,pardf,nchptot,nchp,istrip,1,nn)
       call stripit  (pardirct,pardr,nchptot,nchp,istrip,1,nn)
       call stripit  (chlt,lats,nchptot,nchp,istrip,1,nn)
       call stripit  (chlon,lons,nchptot,nchp,istrip,1,nn)
       call stripit  (lsprec,rainls,nchptot,nchp,istrip,1,nn)
       call stripit  (cnvprec,raincon,nchptot,nchp,istrip,1,nn)
       call stripit  (snowprec,newsnow,nchptot,nchp,istrip,1,nn)
       call stripit  (netsw,swnet,nchptot,nchp,istrip,1,nn)
       call stripit  (netlw,lwstrip,nchptot,nchp,istrip,1,nn)
       call stripit  (alwcoeff,alwrad,nchptot,nchp,istrip,1,nn)
       call stripit  (blwcoeff,blwrad,nchptot,nchp,istrip,1,nn)
       call stripit  (alai,laistrip,nchptot,nchp,istrip,1,nn)
       call stripit  (agrn,grnstrip,nchptot,nchp,istrip,1,nn)
       call stripit  (z2ch,z2str,nchptot,nchp,istrip,1,nn)
       call stripit  (sqscat,scatstr,nchptot,nchp,istrip,1,nn)
       call stripit  (rsoil1,rs1str,nchptot,nchp,istrip,1,nn)
       call stripit  (rsoil2,rs2str,nchptot,nchp,istrip,1,nn)
       call stripit  (rdc,rdcstr,nchptot,nchp,istrip,1,nn)
       call stripit  (u2fac,u2fstr,nchptot,nchp,istrip,1,nn)
       call stripit  (shg,shgstr,nchptot,nchp,istrip,1,nn)
       call stripit  (dqsdt,dqsdtstr,nchptot,nchp,istrip,1,nn)
       call stripit  ( qice, qicestr,nchptot,nchp,istrip,1,nn)
       call stripit  (dqice,dqicestr,nchptot,nchp,istrip,1,nn)
       call stripitint (ityp,types,nchptot,nchp,istrip,1,nn)

       call stripit  (tground,tc,nchptot,nchp,istrip,1,nn)
       call stripit  (tdeep,td,nchptot,nchp,istrip,1,nn)
       call stripit  (qground,qa,nchptot,nchp,istrip,1,nn)
       call stripit  (swetshal,swet1,nchptot,nchp,istrip,1,nn)
       call stripit  (swetroot,swet2,nchptot,nchp,istrip,1,nn)
       call stripit  (swetdeep,swet3,nchptot,nchp,istrip,1,nn)
       call stripit  (snodep,snowdepth,nchptot,nchp,istrip,1,nn)
       call stripit  (capac,capacity,nchptot,nchp,istrip,1,nn)

#ifdef FIZHI_USE_FIXED_DAY
       call astro ( 20040321,nhms,lats,lons,istrip,cosz,ra )
#else
       call astro ( nymd,nhms,lats,lons,istrip,cosz,ra )
#endif

C we need to count up the land, sea ice and ocean points
      nocean = 0
      nland  = 0
      nice   = 0
      do i = 1,istrip
       if( types(i).lt.100 ) nland  = nland  + 1
       if( types(i).eq.100 ) nocean = nocean + 1
       if( types(i).eq.100 .and. icest(i).gt.0.0 ) nice = nice + 1
      enddo

C Zero out velocities at the bottom edge of the model
C ---------------------------------------------------
      do i =1,istrip
       u(i,nlay+1) = 0.
       v(i,nlay+1) = 0.
      enddo

C convert temperature of level nlay+1 to theta & value of sh at ground
C --------------------------------------------------------------------
      do i =1,istrip
      th(i,nlay+1) = th(i,nlay+1) / pke(i,nlay+1)
      sh(i,nlay+1) = qa(i)
      enddo

      if(diagnostics_is_on('QG      ',myid) ) then
      do i=1,istrip
      tmpstrip(i) = sh(i,nlay+1)*1000
      enddo
      call diag_vegtile_fill (tmpstrip,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'QG      ', 1, 1, bi, bj, myid)
      endif

C value of tracers at the ground
C ------------------------------
c     do nt = 1,ntracers-ptracers
c      do i = 1,istrip
c       tracers(i,nlay+1,nt) = 0.
c      enddo
c     enddo

C compute virtual potential temperatures
C --------------------------------------
      do L = 1,nlay+1
      do i =1,istrip
      thv(i,L) = 1. + virtcon * sh(i,L)
      thv(i,L) = th(i,L) * thv(i,L)
      enddo
      enddo
      do i =1,istrip
      sh(i,nlay+1) = qa(i)
      enddo

C zero out arrays for output of qliq and fcc
      do L =1,nlay
      do i =1,istrip
      qliq(i,L) = 0.
      turbfcc(i,L) = 0.
      enddo
      enddo

C zero out fluxes and derivatives
C -------------------------------
      do i = 1,istrip
       eturb(i) = 0.
       scu(i) = 0.
       dedqa(i) = 0.
       dedtc(i) = 0.
       hsturb(i) = 0.
       dhsdqa(i) = 0.
       dhsdtc(i) = 0.
      enddo

C increment diagnostic arrays for quantities calculated before trbfl
C ------------------------------------------------------------------
      if(diagnostics_is_on('DTSRF   ',myid) ) then
       do i=1,istrip
        stdiag(i) = ( thv(i,nlay+1)-thv(i,nlay) ) / pke(i,nlay+1)
       enddo
       call diag_vegtile_fill (stdiag,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'DTSRF   ', 1, 1, bi, bj, myid)
      endif

C call trbflx
C -----------
      call trbflx(nn,th,thv,sh,u,v,qq,p,pe,pk,pke,dpstr,stwatr,stz0,
     1 tracers,ntracers-ptracers,ntracedim,dttrb,itrtrb,rmu,edle,qbeg,
     2 tprof,stuflux,stvflux,sri,skh,skm,swinds,sustar,sz0,frqtrb,
     3 pbldpth,sct,scu,stu2m,stv2m,stt2m,stq2m,stu10m,stv10m,stt10m,
     4 stq10m,istrip,nlay,nltop,nymd,nhms,grav,cp,rgas,faceps,virtcon,
     5 undef,dshdthg,dshdshg,dthdthg,dthdshg,eturb,dedqa,dedtc,
     6 hsturb,dhsdqa,dhsdtc,transth,transsh,
     7 ctsave,xxsave,yysave,zetasave,xlsave,khsave,qliq,turbfcc)

      call pastit (qq,tke,istrip,nchp,nchptot,nlay,nn)
      call pastit (ctsave,ctmt,istrip,nchp,nchptot,1,nn)
      call pastit (xxsave,xxmt,istrip,nchp,nchptot,1,nn)
      call pastit (yysave,yymt,istrip,nchp,nchptot,1,nn)
      call pastit (zetasave,zetamt,istrip,nchp,nchptot,1,nn)
      call pastit (xlsave,xlmt,istrip,nchp,nchptot,nlay,nn)
      call pastit (khsave,khmt,istrip,nchp,nchptot,nlay,nn)

      call pastit (qliq  ,qliqmsc,istrip,nchp,nchptot,nlay,nn)
      call pastit (turbfcc,fccmsc,istrip,nchp,nchptot,nlay,nn)

C  New diagnostic: potential evapotranspiration
      do i = 1,istrip
       evpot(i) = transsh(i,nlay) * (shgstr(i) - sh(i,nlay))
      enddo

C**********************************************************************
C   Call Land Surface Module
C**********************************************************************

      do i = 1,istrip
       savetc(i) = tc(i)
       saveqa(i) = qa(i)
      enddo
      do i = 1,istrip
       cosz(i) = max(cosz(i),0.0001 _d 0)
       cd(i) = scu(i)*scu(i)
       tmpnlay(i) = th(i,nlay)*pk(i,nlay)
       hlwdwn(i) = alwrad(i)+blwrad(i)*tc(i)-lwstrip(i)
       psurf(i) = pe(i,nlay+1)
       wspeed(i) = sqrt(u(i,nlay)*u(i,nlay) + v(i,nlay)*v(i,nlay))
       if(wspeed(i) .lt. 1.e-20) wspeed(i) = 1.e-20
C Note: This LSM precip bug needs to be cleaned up
c      newsnow(i) = newsnow(i)*dtfac
c      raincon(i) = raincon(i)*dtfac
c      rainls (i) = rainls (i)*dtfac
      enddo

      do i = 1,istrip
       eturb(i) = eturb(i) * pke(i,nlay+1)
       dedqa(i) = dedqa(i) * pke(i,nlay+1)
       hsturb(i) = hsturb(i) * pke(i,nlay+1)
      enddo

      do i = 1,istrip
       strdg1(i) = 0.
       strdg2(i) = 0.
       strdg3(i) = 0.
       strdg4(i) = 0.
       strdg5(i) = 0.
       strdg6(i) = 0.
       strdg7(i) = 0.
       strdg8(i) = 0.
       strdg9(i) = 0.
       bomb(i)   = 0.
       runoff(i) = 0.
       eint(i)   = 0.
       esoi(i)   = 0.
       eveg(i)   = 0.
       esno(i)   = 0.
       smelt(i)  = 0.
       hlatn(i)  = 0.
       hlwup(i)  = 0.
       gdrain(i) = 0.
       runsrf(i) = 0.
       fwsoil(i) = 0.
      enddo

C**********************************************************************
C   diagnostics: fill arrays for lsm input fields
C**********************************************************************
      if(diagnostics_is_on('SNOWFALL',myid) ) then
      do i = 1,istrip
      tmpstrip(i) = newsnow(i)*86400
      enddo
      call diag_vegtile_fill (tmpstrip,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'SNOWFALL', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('RAINCON ',myid) ) then
      do i = 1,istrip
      tmpstrip(i) = raincon(i)*86400
      enddo
      call diag_vegtile_fill (tmpstrip,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'RAINCON ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('RAINLSP ',myid) ) then
      do i = 1,istrip
      tmpstrip(i) = rainls(i)*86400
      enddo
      call diag_vegtile_fill (tmpstrip,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'RAINLSP ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('GREEN   ',myid) ) then
      call diag_vegtile_fill (grnstrip,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'GREEN   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('LAI     ',myid) ) then
      call diag_vegtile_fill (laistrip,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'LAI     ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('PARDR   ',myid) ) then
      call diag_vegtile_fill (pardr,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'PARDR   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('PARDF   ',myid) ) then
      call diag_vegtile_fill (pardf,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'PARDF   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('DLWDTC  ',myid) ) then
      call diag_vegtile_fill (blwrad,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'DLWDTC  ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('DHDTC   ',myid) ) then
      call diag_vegtile_fill (dhsdtc,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'DHDTC   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('DEDTC   ',myid) ) then
      call diag_vegtile_fill (dedtc,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'DEDTC   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('DHDQA   ',myid) ) then
      call diag_vegtile_fill (dhsdqa,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'DHDQA   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('DEDQA   ',myid) ) then
      call diag_vegtile_fill (dedqa,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'DEDQA   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('LWGDOWN ',myid) ) then
      call diag_vegtile_fill (hlwdwn,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'LWGDOWN ', 1, 1, bi, bj, myid)
      endif
C**********************************************************************

      if(nland.gt.0)then

       call tile (
     I   nland, timstp, types, rainls, raincon,  newsnow,  wspeed,
     I   eturb,  dedqa,  dedtc,  hsturb, dhsdqa, dhsdtc,
     I   tmpnlay, sh(1,nlay), cd, cosz, pardr, pardf,
     I   swnet,  hlwdwn, psurf, laistrip, grnstrip,  z2str,
     I   scatstr, rs1str, rs2str, rdcstr, u2fstr,
     I   shgstr, dqsdtstr, alwrad, blwrad,
     U   tc, td, qa, swet1, swet2, swet3, capacity, snowdepth,
     O   evap, shflux, runoff, bomb,
     O   eint, esoi, eveg, esno, smelt, hlatn,
     O   hlwup, gdrain, runsrf, fwsoil,
     O   strdg1, strdg2, strdg3, strdg4,
     O   strdg5, strdg6, strdg7, strdg8, strdg9)
      endif

      if( nice.gt.0 ) then
       call seaice ( nocean, timstp,     hice,
     &               eturb(nland+1),    dedtc(nland+1),
     &              hsturb(nland+1),   dhsdtc(nland+1),
     &             qicestr(nland+1), dqicestr(nland+1),
     &               swnet(nland+1),  lwstrip(nland+1), blwrad(nland+1),
     &          pke(nland+1,nlay+1),    icest(nland+1),
     &                  tc(nland+1),       qa(nland+1) )
      endif

C***********************************************************************
C   diagnostics: fill arrays for lsm output fields
C***********************************************************************

      if(diagnostics_is_on('RUNOFF  ',myid) ) then
      call diag_vegtile_fill (runoff,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'RUNOFF  ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('FWSOIL  ',myid) ) then
      call diag_vegtile_fill (fwsoil,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'FWSOIL  ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('GDRAIN  ',myid) ) then
      call diag_vegtile_fill (gdrain,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'GDRAIN  ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('SNOWMELT',myid) ) then
      call diag_vegtile_fill (smelt,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'SNOWMELT', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('EVEG    ',myid) ) then
      call diag_vegtile_fill (eveg,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'EVEG    ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('ESNOW   ',myid) ) then
      call diag_vegtile_fill (esno,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'ESNOW   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('ESOIL   ',myid) ) then
      call diag_vegtile_fill (esoi,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'ESOIL   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('ERESV   ',myid) ) then
      call diag_vegtile_fill (eint,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'ERESV   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('EVPOT   ',myid) ) then
      call diag_vegtile_fill (evpot,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'EVPOT   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('DTC     ',myid) ) then
      call diag_vegtile_fill (strdg1,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'DTC     ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('DQC     ',myid) ) then
      call diag_vegtile_fill (strdg2,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'DQC     ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('TCDTC   ',myid) ) then
      call diag_vegtile_fill (strdg3,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'TCDTC   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('RADDTC  ',myid) ) then
      call diag_vegtile_fill (strdg4,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'RADDTC  ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('SENSDTC ',myid) ) then
      call diag_vegtile_fill (strdg5,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'SENSDTC ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('LATDTC  ',myid) ) then
      call diag_vegtile_fill (strdg6,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'LATDTC  ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('TDDTC   ',myid) ) then
      call diag_vegtile_fill (strdg7,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'TDDTC   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('QCDTC   ',myid) ) then
      call diag_vegtile_fill (strdg8,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'QCDTC   ', 1, 1, bi, bj, myid)
      endif
C***********************************************************************

      call pastit (tc,tground,istrip,nchp,nchptot,1,nn)
      call pastit (td,tdeep,istrip,nchp,nchptot,1,nn)
      call pastit (qa,qground,istrip,nchp,nchptot,1,nn)
      call pastit (swet1,swetshal,istrip,nchp,nchptot,1,nn)
      call pastit (swet2,swetroot,istrip,nchp,nchptot,1,nn)
      call pastit (swet3,swetdeep,istrip,nchp,nchptot,1,nn)
      call pastit (capacity,capac,istrip,nchp,nchptot,1,nn)
      call pastit (snowdepth,snodep,istrip,nchp,nchptot,1,nn)

C**********************************************************************
C  Now update the theta and sh profiles with the new ground temperature
C**********************************************************************

      do i =1,istrip
      th(i,nlay+1) = tc(i) / pke(i,nlay+1)
      enddo
      do L = 1,nlay
      do i =1,istrip
      th(i,L) = th(i,L) + dthdthg(i,L)*(tc(i)-savetc(i))/pke(i,nlay+1)
      enddo
      enddo

      do i =1,istrip
      sh(i,nlay+1) = qa(i)
      enddo
      do L = 1,nlay
      do i =1,istrip
      sh(i,L) = sh(i,L) + dshdshg(i,L)*(qa(i)-saveqa(i))
      enddo
      enddo

      do L = 1,nlay
      do i =1,istrip
       sttflux(i,L) = transth(i,L) * (th(i,L+1)-th(i,L))
       stqflux(i,L) = transsh(i,L) * (sh(i,L+1)-sh(i,L))
      enddo
      enddo

C tendency updates
C ----------------
      do  l=1,nlay
      call strip2tile(uz(1,1,l),igrd,tmpstrip,nchp,ijall,
     1                                                 istrip,1,nn)
      do i =1,istrip
      tends(i) = ( u(i,l)-tmpstrip(i) )
      enddo
      call pastit (tends,dumsc(1,l),istrip,nchp,nchptot,1,nn)

      call strip2tile(vz(1,1,l),igrd,tmpstrip,nchp,ijall,
     1                                                 istrip,1,nn)
      do i =1,istrip
      tends(i) = ( v(i,l)-tmpstrip(i) )
      enddo
      call pastit (tends,dvmsc(1,l),istrip,nchp,nchptot,1,nn)

      call strip2tile(tz(1,1,l),igrd,tmpstrip,nchp,ijall,
     1                                                 istrip,1,nn)
      do i =1,istrip
      tends(i) = ( th(i,l)-tmpstrip(i) )
      enddo

      call pastit (tends,dtmsc(1,l),istrip,nchp,nchptot,1,nn)

      call strip2tile(qz(1,1,l,1),igrd,tmpstrip,nchp,ijall,
     1                                                 istrip,1,nn)
      do i =1,istrip
      tends(i) = ( sh(i,l)-tmpstrip(i) )
      enddo

      call pastit (tends,dqmsc(1,l,1),istrip,nchp,nchptot,1,nn)

c     do nt = 1,ntracers-ptracers
c      call strip2tile(qz(1,1,L,ptracers+nt),igrd,tmpstrip,nchp,
c    1                                             ijall,istrip,1,nn)
c      do i =1,istrip
c       tends(i) = ( tracers(i,L,nt)-tmpstrip(i) )
c      enddo
c      call pastit(tends,dqmsc(1,L,ptracers+nt),istrip,nchp,
c    .                                     nchptot,1,nn)
c     enddo

      enddo
C *********************************************************************
C **** increment diagnostic arrays for quantities saved in trbflx
C *********************************************************************

C note: the order, logic, and scaling of the heat and moisture flux
C       diagnostics is critical!
C ------------------------------

      if(diagnostics_is_on('EVAP    ',myid) ) then
      do i=1,istrip
      tmpstrip(i) = stqflux(i,nlay) * 86400
      enddo
      call diag_vegtile_fill (tmpstrip,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'EVAP    ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('EFLUX   ',myid) ) then
      do i=1,istrip
      tmpstrip(i) = stqflux(i,nlay) * alhl
      enddo
      call diag_vegtile_fill (tmpstrip,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'EFLUX   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('HFLUX   ',myid) ) then
      do i=1,istrip
      tmpstrip(i) = sttflux(i,nlay) * cp
      enddo
      call diag_vegtile_fill (tmpstrip,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'HFLUX   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('TUFLUX  ',myid) ) then
      call diag_vegtile_fill (stuflux,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'TUFLUX  ', 0, nlay, bi, bj, myid)
      endif
      if(diagnostics_is_on('TVFLUX  ',myid) ) then
      call diag_vegtile_fill (stvflux,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'TVFLUX  ', 0, nlay, bi, bj, myid)
      endif
      if(diagnostics_is_on('TTFLUX  ',myid) ) then
      do l=1,nlay
      do i=1,istrip
      sttflux(i,l) = sttflux(i,l) * cp
      enddo
      enddo
      call diag_vegtile_fill (sttflux,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'TTFLUX  ', 0, nlay, bi, bj, myid)
      endif
      if(diagnostics_is_on('TQFLUX  ',myid) ) then
      do l=1,nlay
      do i=1,istrip
      stqflux(i,l) = stqflux(i,l) * alhl
      enddo
      enddo
      call diag_vegtile_fill (stqflux,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'TQFLUX  ', 0, nlay, bi, bj, myid)
      endif
      if(diagnostics_is_on('RI      ',myid) ) then
      call diag_vegtile_fill (sri,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'RI      ', 0, nlay, bi, bj, myid)
      endif
      if(diagnostics_is_on('KH      ',myid) ) then
      call diag_vegtile_fill (skh,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'KH      ', 0, nlay, bi, bj, myid)
      endif
      if(diagnostics_is_on('KM      ',myid) ) then
      call diag_vegtile_fill (skm,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'KM      ', 0, nlay, bi, bj, myid)
      endif
      if(diagnostics_is_on('CT      ',myid) ) then
      call diag_vegtile_fill (sct,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'CT      ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('CU      ',myid) ) then
      call diag_vegtile_fill (scu,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'CU      ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('WINDS   ',myid) ) then
      call diag_vegtile_fill (swinds,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'WINDS   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('UFLUX   ',myid) ) then
      call diag_vegtile_fill (stuflux(1,nlay),igrd,chfrstr,istrip,nchp,
     & nn,.false., 'UFLUX   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('VFLUX   ',myid) ) then
      call diag_vegtile_fill (stvflux(1,nlay),igrd,chfrstr,istrip,nchp,
     & nn,.false., 'VFLUX   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('USTAR   ',myid) ) then
      call diag_vegtile_fill (sustar,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'USTAR   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('Z0      ',myid) ) then
      call diag_vegtile_fill (sz0,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'Z0      ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('FRQTRB  ',myid) ) then
      call diag_vegtile_fill (frqtrb,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'FRQTRB  ', 0, nlay-1, bi, bj, myid)
      endif
      if(diagnostics_is_on('PBL     ',myid) ) then
      call diag_vegtile_fill (pbldpth,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'PBL     ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('U2M     ',myid) ) then
      call diag_vegtile_fill (stu2m,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'U2M     ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('V2M     ',myid) ) then
      call diag_vegtile_fill (stv2m,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'V2M     ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('T2M     ',myid) ) then
      call diag_vegtile_fill (stt2m,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'T2M     ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('Q2M     ',myid) ) then
      do i=1,istrip
         if( stq2m(i).ne.undef ) then
             tmpstrip(i) = stq2m(i) * 1000
         else
             tmpstrip(i) = undef
         endif
      enddo
      call diag_vegtile_fill (tmpstrip,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'Q2M     ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('U10M    ',myid) ) then
      call diag_vegtile_fill (stu10m,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'U10M    ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('V10M    ',myid) ) then
      call diag_vegtile_fill (stv10m,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'V10M    ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('T10M    ',myid) ) then
      call diag_vegtile_fill (stt10m,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'T10M    ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('Q10M    ',myid) ) then
      do i=1,istrip
         if( stq10m(i).ne.undef ) then
             tmpstrip(i) = stq10m(i) * 1000
         else
             tmpstrip(i) = undef
         endif
      enddo
      call diag_vegtile_fill (tmpstrip,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'Q10M    ', 1, 1, bi, bj, myid)
      endif

C**********************************************************************
C   more diagnostics: land surface model parameters
C**********************************************************************

      if(diagnostics_is_on('TDEEP   ',myid) ) then
      call diag_vegtile_fill (td,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'TDEEP   ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('QCANOPY ',myid) ) then
      call diag_vegtile_fill (qa,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'QCANOPY ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('SMSHAL  ',myid) ) then
      call diag_vegtile_fill (swet1,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'SMSHAL  ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('SMROOT  ',myid) ) then
      call diag_vegtile_fill (swet2,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'SMROOT  ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('SMDEEP  ',myid) ) then
      call diag_vegtile_fill (swet3,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'SMDEEP  ', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('CAPACITY',myid) ) then
      call diag_vegtile_fill (capacity,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'CAPACITY', 1, 1, bi, bj, myid)
      endif
      if(diagnostics_is_on('SNOW    ',myid) ) then
      call diag_vegtile_fill (snowdepth,igrd,chfrstr,istrip,nchp,nn,
     & .false., 'SNOW    ', 1, 1, bi, bj, myid)
      endif
C**********************************************************************
C end regions loop

 2000 continue

C**********************************************************************

C increment the counter for the accumulated fcc and qliq arrays
C ---------------------------------------------------------------------
      imstturblw = imstturblw + 1
      imstturbsw = imstturbsw + 1

C prevent ice or snow from melting
C ---------------------------------------------------------------------
      do i =1,nchptot
      if( (icethk(i).gt.0.).and.(tground(i).gt.tice) ) tground(i)=tice
      enddo

C Update tcanopy and ecanopy from the points of the
C           tground and qground arrays
C ---------------------------------------------------------------------
      do i =1,nchptot
       tcanopy(i) = tground(i)
       ecanopy(i) = qground(i)
      enddo

C Initialize Tendencies and Couplings
C -----------------------------------
      do L = 1,nlay
       do i = 1,ijall
           duturb(i,1,L) = 0.
           dvturb(i,1,L) = 0.
           dtturb(i,1,L) = 0.
           qqgrid(i,1,L) = 0.
          qliqtmp(i,1,L) = 0.
           fcctmp(i,1,L) = 0.
       enddo
       do nt = 1,ntracers
        do i = 1,ijall
           dqturb(i,1,L,nt) = 0.
        enddo
       enddo
      enddo

C Return Tendencies and Couplings to Grid Space
C ---------------------------------------------
      do l = 1,nlay
      call msc2grd(igrd,chfr,dumsc(1,L),nchp,nchptot,fracland,
     &                                              duturb(1,1,L),im,jm)
      call msc2grd(igrd,chfr,dvmsc(1,L),nchp,nchptot,fracland,
     &                                              dvturb(1,1,L),im,jm)
      call msc2grd(igrd,chfr,dtmsc(1,L),nchp,nchptot,fracland,
     &                                              dtturb(1,1,L),im,jm)
      do nt = 1,ntracers
      call msc2grd(igrd,chfr,dqmsc(1,L,nt),nchp,nchptot,fracland,
     &                                           dqturb(1,1,L,nt),im,jm)
      enddo
      call msc2grd(igrd,chfr,  tke(1,L),nchp,nchptot,fracland,
     &                                              qqgrid(1,1,L),im,jm)

      call msc2grd(igrd,chfr, fccmsc(1,L),nchp,nchptot,fracland,
     &                                              fcctmp(1,1,L),im,jm)
      call msc2grd(igrd,chfr,qliqmsc(1,L),nchp,nchptot,fracland,
     &                                             qliqtmp(1,1,L),im,jm)
      enddo

C Reduce clouds from conditionally unstable layer
C -----------------------------------------------
      call ctei ( tz,qz,fcctmp,qliqtmp,plz,pkz,pkht,im*jm,nlay )

C Bump Total Cloud Liquid Water and Fraction by Instantaneous Values
C ------------------------------------------------------------------
      do l = 1,nlay
       do i = 1,ijall
         fccavesw(i,1,L) =  fccavesw(i,1,L) +  fcctmp(i,1,L)
         fccavelw(i,1,L) =  fccavelw(i,1,L) +  fcctmp(i,1,L)
        qliqavelw(i,1,L) = qliqavelw(i,1,L) + qliqtmp(i,1,L)
        qliqavesw(i,1,L) = qliqavesw(i,1,L) + qliqtmp(i,1,L)
       enddo
      enddo
      tmpFac = 1.e6
      CALL DIAGNOSTICS_FILL(fcctmp,'TRBFCC  ',0,nlay,3,bi,bj,myid)
      CALL DIAGNOSTICS_SCALE_FILL( qliqtmp,tmpFac,1,
     &                             'TRBQLIQ ',0,nlay,3,bi,bj,myid)
C**********************************************************************
C And some other variables to be transformed back to grid space:
C Ground Temperature, snow depth and shallow layer ground wetness
      do i = 1,ijall
         tgz(i,1) = 0.
      enddo
      call msc2grd(igrd,chfr,tground ,nchp,nchptot,fracland,tgz ,im,jm)

C *********************************************************************
C **** increment diagnostic array for ground and surface temperatures,
C ***       ground temp tendency, and ground humidity
C *********************************************************************

      call diagnostics_fill(tgz,'TGROUND ',0,1,3,bi,bj,myid)
      call diagnostics_fill(tgz,'TCANOPY ',0,1,3,bi,bj,myid)

      if(diagnostics_is_on('TS      ',myid) ) then
       do i = 1,ijall
        tmpdiag(i,1) = tz(i,1,nlay) * pkht(i,1,nlay)
       enddo
       call diagnostics_fill(tmpdiag,'TS      ',0,1,3,bi,bj,myid)
      endif

      call diagnostics_fill(tgz,'DTG     ',0,1,3,bi,bj,myid)

C *********************************************************************
C ****           increment diagnostic arrays for tendencies        ****
C *********************************************************************
      tmpFac = atimstp * secday
      CALL DIAGNOSTICS_SCALE_FILL(dvturb,tmpFac,1,
     &                            'TURBV   ',0,nlay,3,bi,bj,myid)
      CALL DIAGNOSTICS_SCALE_FILL(duturb,tmpFac,1,
     &                            'TURBU   ',0,nlay,3,bi,bj,myid)
      tmpFac = atimstp * secday * 1000.
      CALL DIAGNOSTICS_SCALE_FILL(dqturb,tmpFac,1,
     &                            'TURBQ   ',0,nlay,3,bi,bj,myid)

      if(diagnostics_is_on('TURBT   ',myid) ) then
       do L = 1,nlay
        do i = 1,ijall
          tmpdiag(i,1) = dtturb(i,1,l) * pkz(i,1,l)*atimstp*secday
        enddo
        call diagnostics_fill(tmpdiag,'TURBT   ',L,1,3,bi,bj,myid)
       enddo
      endif

C pi-weight the theta and moisture tendencies
C -------------------------------------------
      do i =1,ijall
      thtgz(i) = pz(i,1) * atimstp
      enddo
      do l =1,nlay
       do i =1,ijall
        duturb(i,1,l) = duturb(i,1,l)*atimstp
        dvturb(i,1,l) = dvturb(i,1,l)*atimstp
        dtturb(i,1,l) = dtturb(i,1,l)*thtgz(i)
       enddo
       do nt = 1,ntracers
        do i =1,ijall
         dqturb(i,1,l,nt) = dqturb(i,1,l,nt)*thtgz(i)
        enddo
       enddo
      enddo

C *********************************************************************
C ****   zero out the accumulating rainfall and snowfall arrays     ***
C *********************************************************************

      if( time_left.lt.timstp ) then
       do i =1,ijall
         rainlsp(i,1) = 0.
        rainconv(i,1) = 0.
        snowfall(i,1) = 0.
       enddo
      endif

      return
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE TRBFLX (NN,TH,THV,SH,U,V,QQ,PL,PLE,PLK,PLKE,DPSTR,
     1 IWATER,Z0,tracers,ntrace,ntracedim,DTAU,ITRTRB,KMBG,KHBG,QBEG,
     2 TPROF,WU,WV,SRI,ET,EU,SWINDS,sustar,sz0,freqdg,pbldpth,
     3 sct,scu,stu2m,stv2m,stt2m,stq2m,stu10m,stv10m,stt10m,stq10m,
     4 irun,nlev,nltop,NYMD,NHMS,grav,cp,rgas,faceps,virtcon,undef,
     5 dshdthg,dshdshg,dthdthg,dthdshg,eturb,dedqa,dedtc,
     6 hsturb,dhsdqa,dhsdtc,transth,transsh,
     7 ctsave,xxsave,yysave,zetasave,xlsave,khsave,qliq,turbfcc)
C**********************************************************************
C  SUBROUTINE TRBFLX - COMPUTES TURBULENT ADJUSTMENTS TO ATMOSPHERIC
C                       PROFILE
C                    - CALLED FROM PBL DRIVER
C   ARGUMENTS ::
C     INPUT:
C     ------
C    TH            -         POTENTIAL TEMPERATURE PROFILE
C    THV           -         VIRTUAL POTENTIAL TEMPERATURE PROFILE
C    SH            -         SPECIFIC HUMIDITY PROFILE
C    U             -         U - COMPONENT OF WIND PROFILE
C    V             -         V - COMPONENT OF WIND PROFILE
C    QQ            -         TURBULENT KINETIC ENERGY
C    PL            -         EVEN LEVEL PRESSURES
C    PLE           -         EDGE LEVEL PRESSURES
C    PLK           -         EVEN LEVEL PRESSURES ** KAPPA
C    PLKE          -         EDGE LEVEL PRESSURES ** KAPPA
C    DPSTR         -         PRESSURE INTERVALS
C    WATER         -         BIT ARRAY - '1' OVER OCEANS
C    Z0            -         SURFACE ROUGHNESS
C    tracers       -         array of passive tracers
C    ntrace        -         number of tracers to be diffused
C    ntracedim     -         outer dimension of tracers array
C    DTAU          -         TIME CHANGE PER ITERATION OF TRBFLX
C    ITRTRB        -         NUMBER OF ITERATIONS OF TRBFLX
C    KMBG          -         BACKGROUND VALUE OF MOMENTUM TRANSFER COEF
C    KHBG          -         BACKGROUND VALUE OF HEAT TRANSFER COEF
C    NLEV          -         NUMBER OF ATMOSPHERIC LEVELS TO CALCULATE
C    nltop         -         Top level allowed for turbulence
C    QBEG          -         LOGICAL .TRUE. FOR INITIAL START OF GCM
C    TPROF         -         LOGICAL .TRUE. TO CALCULATE PT BY PT DIAGS
C
C     OUTPUT:
C     -------
C    PROFILES RETURNED WITH UPDATED VALUES
C**********************************************************************
      implicit none

C Argument list declarations
      integer nn,irun,nlev,nltop,ntrace,ntracedim,itrtrb,nhms,nymd
      _RL TH(irun,NLEV+1),THV(irun,NLEV+1),SH(irun,NLEV+1)
      _RL U(irun,NLEV+1),V(irun,NLEV+1),QQ(irun,NLEV)
      _RL PL(irun,NLEV),PLE(irun,NLEV+1),PLK(irun,NLEV)
      _RL PLKE(irun,NLEV+1),DPSTR(irun,NLEV)
      integer IWATER(irun)
      _RL Z0(irun)
      _RL tracers(irun,nlev+1,ntracedim)
      _RL dtau,KMBG,KHBG
      LOGICAL QBEG,TPROF
      _RL SWINDS(irun)
      _RL SRI(irun,nlev), ET(irun,nlev)
      _RL EU (irun,nlev)
      _RL WU(irun,nlev)
      _RL WV (irun,nlev), pbldpth(irun)
      _RL sustar(irun), sz0(irun)
      _RL freqdg(irun,nlev-1)
      _RL sct(irun), scu(irun)
      _RL stu2m(irun),stv2m(irun),stt2m(irun),stq2m(irun)
      _RL stu10m(irun),stv10m(irun),stt10m(irun),stq10m(irun)
      _RL grav,cp,rgas,faceps,virtcon,undef
      _RL eturb(irun),dedqa(irun),dedtc(irun)
      _RL hsturb(irun),dhsdqa(irun),dhsdtc(irun)
      _RL dshdthg(irun,nlev),dthdthg(irun,nlev)
      _RL dshdshg(irun,nlev),dthdshg(irun,nlev)
      _RL transth(irun,nlev),transsh(irun,nlev)
      _RL ctsave(irun),xxsave(irun),yysave(irun)
      _RL zetasave(irun),xlsave(irun,nlev),khsave(irun,nlev)
      _RL qliq(irun,nlev),turbfcc(irun,nlev)

C Local Variables
      _RL b1,b3,alpha,halpha,qqmin,qbustr
      PARAMETER ( B1      =   16.6    )
      PARAMETER ( B3      = 1. / B1  )
      PARAMETER ( ALPHA   = 0.1       )
      PARAMETER ( HALPHA = ALPHA * 0.5 )
      PARAMETER ( QQMIN  = 0.005      )
      PARAMETER ( QBUSTR = 2.550952   )
      _RL    argmax, onethrd, z1pem25, b2, two
      PARAMETER (ARGMAX = 30.)
      PARAMETER (ONETHRD = 1./3. )
      PARAMETER (Z1PEM25 = 1.E-25)
      PARAMETER ( B2    =  10.1 )
      PARAMETER ( two   =   2.0 )

      _RL AHS (irun), HS(irun)
      _RL XX  (irun), YY(irun), CU(irun)
      _RL CT(irun),  USTAR(irun)
      _RL RIB(irun),   ZETA(irun), WS(irun)
      _RL DTHS(irun), DELTHS(irun)
      _RL DTHL(irun), DELTHL(irun)
      _RL RIBIN(irun),CUIN(irun)
      _RL CTIN(irun),ZETAIN(irun)
      _RL USTARIN(irun),RHOSIN(irun),Z0IN(irun)
      _RL qqcolmin(irun),qqcolmax(irun),levpbl(irun)

      _RL ADZ1(irun,nlev), DZ1TMP(irun,nlev)
      _RL DZ3(irun,nlev), TEMP(irun,nlev)
      _RL DV(irun,nlev), DTHV(irun,nlev)
      _RL DPK(irun,nlev), STRT(irun,nlev)
      _RL DW2(irun,nlev), RI(irun,nlev)
      _RL RHOZPK(irun,nlev), Q(irun,nlev)
      _RL RIINIT(irun,nlev), DU(irun,nlev)
      _RL QQINIT(irun,nlev), RHOKDZ(irun,nlev)
      _RL RHODZ2(irun,nlev)
      _RL KM(irun,nlev), KH(irun,nlev)

      _RL DELTH  (irun,nlev+1), DELSH (irun,nlev+1)
      _RL FLXFAC (irun,nlev+1)
      _RL FLXFPK (irun,nlev+1)

      _RL ADZ2   (irun,nlev-1), RHODZ1(irun,nlev-1)
      _RL VKZE   (irun,nlev-1), VKZM  (irun,nlev-1)
      _RL XL     (irun,nlev-1), QXLM  (irun,nlev-1)
      _RL QQE    (irun,nlev-1), QE    (irun,nlev-1)
      _RL P3     (irun,nlev-1), XQ    (irun,nlev-1)
      _RL XLDIAG (irun,nlev-1), FLXFCE(irun,nlev-1)

      LOGICAL FIRST,LAST
      integer IBITSTB(irun,nlev),INTQ(irun,nlev)

C arrays for use by moist bouyancy calculation
C -----------------
      _RL TL(irun,NLEV),DTH(irun,NLEV)
      _RL DSH(irun,NLEV)
      _RL SHL(irun,NLEV)
      _RL AA(irun,NLEV),BB(irun,NLEV),SSDEV(irun,NLEV)
      _RL ARG(irun,NLEV),XXZETA(irun),QBYU(irun)
      _RL SVAR(irun,NLEV),Q1M(irun,NLEV)
      _RL FCC(irun,NLEV)
      _RL BETAT(irun,NLEV),BETAW(irun,NLEV)
      _RL BETAL(irun,NLEV),BETAT1(irun,NLEV)
      _RL BETAW1(irun,NLEV),SBAR(irun,NLEV)
      _RL SHSAT(irun,NLEV)

C Some space for variables to be used in called routines
      logical LWATER
      integer IVBITRIB(irun)
      _RL VHZ(irun)
      _RL VH0(irun)
      _RL VPSIM(irun),VAPSIM(irun)
      _RL VPSIG(irun),VPSIHG(irun)
      _RL VTEMP(irun),VDZETA(irun)
      _RL VDZ0(irun),VDPSIM(irun)
      _RL VDPSIH(irun),VZH(irun)
      _RL VXX0(irun),VYY0(irun)
      _RL VAPSIHG(irun),VRIB1(irun),VWS1(irun)
      _RL VPSIH(irun),VZETAL(irun)
      _RL VZ0L(irun),VPSIH2(irun)
      _RL VX0PSIM(irun),VG(irun),VG0(irun),VR1MG0(irun)
      _RL VZ2(irun),VDZSEA(irun),VAZ0(irun),VXNUM1(irun)
      _RL VPSIGB2(irun),VDX(irun),VDXPSIM(irun),VDY(irun)
      _RL VXNUM2(irun),VDEN(irun),VAWS1(irun),VXNUM3(irun)
      _RL VXNUM(irun),VDZETA1(irun),VDZETA2(irun)
      _RL VZCOEF2(irun),VZCOEF1(irun),VTEMPLIN(irun)
      _RL VDPSIMC(irun),VDPSIHC(irun)

      _RL DZITRP(irun,nlev-1),STBFCN(irun,nlev)
      _RL XL0(irun,nlev),Q1(irun,nlev-1)
      _RL WRKIT1(irun,nlev-1)
      _RL WRKIT2(irun,nlev-1)
      _RL WRKIT3(irun,nlev-1)
      _RL WRKIT4(irun,nlev-1)
      INTEGER INT1(irun,nlev), INT2(irun,nlev-1)

      _RL vrt1con,pi,rsq2pi,p5sr,clh,vk,rvk,aitr,gbycp,fac1,fac2
      _RL getcon,dum,errf
      integer istnlv,nlevm1,nlevm2,nlevml,nlevp1,istnm1,istnm2,istnp1
      integer istnml,istnmq,istlmq,nlevmq
      integer i,iter,init,n,LL,L,Lp,Lp1,lmin,lminq,lminq1,ibit

      vk = getcon('VON KARMAN')
      rvk = 1./vk
      AITR   = 1. / FLOAT(ITRTRB)
      ISTNLV = irun * NLEV
      NLEVM1 = NLEV - 1
      NLEVM2 = NLEV - 2
      NLEVP1 = NLEV + 1
      ISTNM1 = irun * NLEVM1
      ISTNM2 = irun * NLEVM2
      ISTNP1 = irun * NLEVP1
      GBYCP  = GRAV / CP

      VRT1CON = 1. + VIRTCON
      PI   = 4. * ATAN(1.)
      RSQ2PI = 1./ ((2.*PI)**0.5)
      P5SR = 0.5**0.5
      CLH = GETCON('LATENT HEAT COND') / CP

C SET INITIAL NUMBER OF ITERATIONS OF SFCFLX
C ------------------------------------------
      N = 6
C DETERMINE IF INITIAL START
C --------------------------
      INIT = 0
      IF(QBEG) INIT = 1
C SET DIAGNOSTIC LOGICALS AND INITIALIZE DIAGNOSTIC ARRAYS
C --------------------------------------------------------
      DO L =1,NLEV
        DO I =1,irun
         wu(I,L) = 0.
         wv(I,L) = 0.
         eu(I,L) = 0.
         et(I,L) = 0.
        ENDDO
      ENDDO
      if (tprof) then
       DO L =1,NLEVM1
        DO I =1,irun
         XLDIAG(I,L) = 0.
        ENDDO
       ENDDO
      endif
      DO L =1,NLEVM1
        DO I =1,irun
         FREQDG(I,L) = 0.
        ENDDO
      ENDDO
      do I =1,irun
      scu(i) = 0.
      enddo
      do I =1,irun
      sct(i) = 0.
      enddo
      do I =1,irun
      pbldpth(i) = 0.
      enddo
      do I =1,irun
      sustar(i) = 0.
      enddo
      do I =1,irun
      sz0(i) = 0.
      enddo
c     do I =1,ISTNM1
c     FREQDG(I,1) = 0.
c     enddo
      do I =1,irun
      stu2m(i) = 0.
      enddo
      do I =1,irun
      stv2m(i) = 0.
      enddo
      do I =1,irun
      stt2m(i) = 0.
      enddo
      do I =1,irun
      stq2m(i) = 0.
      enddo
      do I =1,irun
      stu10m(i) = 0.
      enddo
      do I =1,irun
      stv10m(i) = 0.
      enddo
      do I =1,irun
      stt10m(i) = 0.
      enddo
      do I =1,irun
      stq10m(i) = 0.
      enddo

      IF (INIT.EQ.1) THEN
       DO L =1,NLEVM1
        DO I =1,irun
        XLSAVE(I,L) = 0.
        KHSAVE(I,L) = 0.
        ENDDO
       ENDDO
       DO I = 1,irun
        CTSAVE(I) = 0.
        XXSAVE(I) = 0.
        YYSAVE(I) = 0.
        ZETASAVE(I) = 0.
       ENDDO
      ENDIF

C COMPUTE VERTICAL GRID
C ---------------------
      DO L =1,NLEV
       DO I =1,irun
        ADZ1(I,L) = (CP/GRAV)*(PLKE(I,L+1)-PLKE(I,L))
        ADZ1(I,L) = THV(I,L) * ADZ1(I,L)
        DZ1TMP(I,L) = ADZ1(I,L)
       ENDDO
      ENDDO
      DO L =1,NLEVM1
       DO I =1,irun
        ADZ2(I,L) = 0.5 * (ADZ1(I,L)+ADZ1(I,L+1))
       ENDDO
      ENDDO
C DEPTH HS OF SURFACE LAYER
C -------------------------
      DO 9042 I =1,irun
      HS(I) = 0.5 * ADZ1(I,NLEV)
9042  CONTINUE
C ALPHA * LAYER DEPTHS FOR TRBLEN
C -------------------------------
      DO 9044 I =1,irun
      DZ3(I,1) = HALPHA * ADZ1(I,1)
9044  CONTINUE
      DO L =2,NLEVM1
       DO I =1,irun
        DZ3(I,L) = ALPHA * ADZ1(I,L)
       ENDDO
      ENDDO
      DO 9048 I =1,irun
      DZ3(I,NLEV) = ALPHA * HS(I)
9048  CONTINUE

C VK * HEIGHTS AT MID AND EDGE LEVELS
C -----------------------------------
      DO L =2,NLEV
       DO I =1,irun
        TEMP(I,L) = VK * ADZ1(I,L)
       ENDDO
      ENDDO
      DO 9052 I =1,irun
      VKZE(I,NLEVM1) = TEMP(I,NLEV)
9052  CONTINUE
      DO 100 LL = 2,NLEVM1
      L = NLEV - LL
      LP1 = L + 1
      DO 9054 I =1,irun
      VKZE(I,L) = VKZE(I,LP1) + TEMP(I,LP1)
9054  CONTINUE
 100  CONTINUE
      DO L =1,NLEVM1
       DO I =1,irun
        VKZM(I,L) = VKZE(I,L) - 0.5 * TEMP(I,L+1)
       ENDDO
      ENDDO
C COMPUTE RHO BY DZ AT MID AND EDGE LEVELS
C ----------------------------------------
      DO 200 L = 1,NLEVM1
       LP1 = L + 1
      DO 9058 I =1,irun
       FAC1 = DPSTR(I,L) / ( DPSTR(I,L) + DPSTR(I,LP1) )
       FAC2 = 1. - FAC1
       RHODZ2(I,L) = FAC1 * THV(I,LP1)
       RHODZ2(I,L) = RHODZ2(I,L) + FAC2 * THV(I,L)
9058  CONTINUE
 200  CONTINUE
      DO L =1,NLEVM1
       DO I =1,irun
        RHODZ2(I,L) = (RGAS*0.01) * RHODZ2(I,L)
        TEMP(I,L)   = PLKE(I,L+1) * ADZ2(I,L)
        RHODZ2(I,L) = TEMP(I,L) * RHODZ2(I,L)
        RHODZ2(I,L) = PLE(I,L+1) / RHODZ2(I,L)
        RHOZPK(I,L) = RHODZ2(I,L) * PLKE(I,L+1)
        RHODZ1(I,L) = (RGAS*0.01) * THV(I,L+1)
        TEMP(I,L)   = PLK(I,L+1) * ADZ1(I,L+1)
        RHODZ1(I,L) = TEMP(I,L) * RHODZ1(I,L)
        RHODZ1(I,L) = PL(I,L+1) / RHODZ1(I,L)
       ENDDO
      ENDDO
C COMPUTE FLXFAC FOR LAYERS AND EDGES
C COMPUTE DTG / DT DUE TO RADIATION AND HEAT CONDUCTION THROUGH ICE
C -----------------------------------------------------------------
      DO L =1,NLEV
       DO I =1,irun
        FLXFPK(I,L) = PLE(I,L+1) - PLE(I,L)
        FLXFPK(I,L) = FLXFPK(I,L) * PLK(I,L)
        FLXFPK(I,L) = (GRAV*DTAU*0.01) / FLXFPK(I,L)
       ENDDO
      ENDDO
      DO 9064 I =1,irun
      FLXFPK(I,NLEVP1) = 0.
9064  CONTINUE
      DO 9066 I =1,irun
      IF (IWATER(I).EQ.0 ) FLXFPK(I,NLEVP1) = 1. / PLKE(I,NLEVP1)
9066  CONTINUE
      DO L =1,NLEV
       DO I =1,irun
        FLXFAC(I,L) = FLXFPK(I,L) * PLK(I,L)
       ENDDO
      ENDDO
      DO 9070 I =1,irun
      FLXFAC(I,NLEVP1) = FLXFPK(I,NLEVP1)
9070  CONTINUE
      DO 9074 I =1,irun
      FLXFPK(I,NLEVP1) = CP * FLXFPK(I,NLEVP1)
9074  CONTINUE
      DO L =1,NLEVM1
       DO I =1,irun
        FLXFCE(I,L) = PL(I,L+1) - PL(I,L)
        FLXFCE(I,L) = (GRAV*DTAU*0.01) / FLXFCE(I,L)
       ENDDO
      ENDDO
C COMPUTE RECIPROCALS OF DZ1, DZ2, HS
C -----------------------------------
      DO L =1,NLEV
       DO I =1,irun
        ADZ1(I,L) = 1. / ADZ1(I,L)
       ENDDO
      ENDDO
      DO L =1,NLEVM1
       DO I =1,irun
        ADZ2(I,L) = 1. / ADZ2(I,L)
       ENDDO
      ENDDO
      DO 9088 I =1,irun
      AHS(I) = 1. / HS(I)
9088  CONTINUE
C COMPUTE GRADIENTS OF P**KAPPA
C -----------------------------
      DO L =1,NLEVM1
       DO I =1,irun
        DPK(I,L) = ( PLK(I,L+1)-PLK(I,L) ) * ADZ2(I,L)
       ENDDO
      ENDDO
      DO 9092 I =1,irun
      DPK(I,NLEV) = GBYCP / THV(I,NLEV)
9092  CONTINUE
C INITIALIZE Q ARRAY
C ------------------
      DO L =1,NLEVM1
       DO I =1,irun
        Q(I,L) = 2. * QQ(I,L)
        Q(I,L) = SQRT( Q(I,L) )
       ENDDO
      ENDDO
      FIRST = .TRUE.
      LAST = .FALSE.
C**********************************************************************
C**********************************************************************
C                              MAIN LOOP

      DO 2000 ITER = 1, ITRTRB

      IF ( ITER .GE. ITRTRB ) LAST = .TRUE.

C  CODE FOR MOIST BOUNDARY LAYER - NEW CALCULATION OF DTHV

      IF(ITER.EQ.1) THEN
       DO I = 1,irun
        CT(I) = CTSAVE(I)
        XX(I) = XXSAVE(I)
        YY(I) = YYSAVE(I)
        ZETA(I) = ZETASAVE(I)
       ENDDO
      ENDIF

      DO I = 1,irun
       TL(I,NLEV) = TH(I,NLEV)*PLK(I,NLEV)
       call qsat ( tl(i,nlev),pl(i,nlev),shsat(i,nlev),dum,.false. )
      ENDDO

      DO I = 1,irun
       BB(I,NLEV) = FACEPS*SHSAT(I,NLEV)/(TL(I,NLEV)*TL(I,NLEV))
       AA(I,NLEV) = 1. / (1. + CLH * BB(I,NLEV) )
       BB(I,NLEV) = BB(I,NLEV) * AA(I,NLEV) * plk(I,nlev)
       DTH(I,NLEV) = TH(I,NLEV)-TH(I,NLEVP1)
       DSH(I,NLEV) = SH(I,NLEV)-SH(I,NLEVP1)
       SBAR(I,NLEV) = AA(I,NLEV) * (SH(I,NLEV) - SHSAT(I,NLEV))
       SSDEV(I,NLEV)=CT(I)*(AA(I,NLEV)*DSH(I,NLEV)
     1                  -BB(I,NLEV)*DTH(I,NLEV))
       XXZETA(I) = XX(I)-ZETA(I)
       IF(XXZETA(I).LT.0.1*XX(I)) XXZETA(I)=0.1*XX(I)
       IF(XXZETA(I).LE.0.) XXZETA(I)=0.1
       QBYU(I) =QBUSTR * XXZETA(I)  ** ONETHRD
       SSDEV(I,NLEV) = B2*YY(I)*SSDEV(I,NLEV)*SSDEV(I,NLEV)/QBYU(I)
       SVAR(I,NLEV) = SQRT(SSDEV(I,NLEV))
       IF ( SVAR(I,NLEV).LT.Z1PEM25) SVAR(I,NLEV) = Z1PEM25
       Q1M(I,NLEV) = SBAR(I,NLEV) / SVAR(I,NLEV)
       FCC(I,NLEV) = (1./2.) * ( 1. + ERRF( P5SR*Q1M(I,NLEV) ) )
       SHL(I,NLEV) =  FCC(I,NLEV) * SBAR(I,NLEV)
       ARG(I,NLEV)  = (1./2.)*Q1M(I,NLEV)*Q1M(I,NLEV)
       IF(ARG(I,NLEV).LE.ARGMAX)
     1 SHL(I,NLEV) =  SHL(I,NLEV)+RSQ2PI*SVAR(I,NLEV)*EXP(-ARG(I,NLEV))
       BETAT(I,NLEV) = 1. + VIRTCON*SH(I,NLEV) - VRT1CON*SHL(I,NLEV)
       BETAW(I,NLEV) = VIRTCON *
     1         ( TH(I,NLEV) + CLH * SHL(I,NLEV) * (1./plk(i,nlev)) )
       BETAL(I,NLEV) = (1.+VIRTCON*SH(I,NLEV)-TWO*VRT1CON*SHL(I,NLEV))
     1  * (1./plk(i,nlev)) * CLH - VRT1CON * TH(I,NLEV)
       BETAT1(I,NLEV) = BETAT(I,NLEV) - BB(I,NLEV)*FCC(I,NLEV)
     1        * BETAL(I,NLEV)
       BETAW1(I,NLEV) = BETAW(I,NLEV) + AA(I,NLEV) * FCC(I,NLEV)
     1        * BETAL(I,NLEV)
       DTHV(I,NLEV) = BETAT1(I,NLEV)*DTH(I,NLEV) +
     1       BETAW1(I,NLEV)*DSH(I,NLEV)
       THV(I,NLEVP1) = THV(I,NLEV) - DTHV(I,NLEV)
      ENDDO

C   SURFACE FLUX TRANSFER COEFFICIENTS

       CALL SFCFLX(NN,U(1,NLEV),V(1,NLEV),
     1   THV(1,NLEV),
     2   THV(1,NLEVP1),TH(1,NLEV),TH(1,NLEVP1),
     3   SH(1,NLEV),SH(1,NLEVP1),PLK(1,NLEV),
     4   PLKE(1,NLEVP1),PLE(1,NLEVP1),Z0,
     5   IWATER,HS,AHS,
     6   FIRST,LAST,N,irun,aitr,RHODZ2(1,NLEV),RHOZPK(1,NLEV),
     7   KH(1,NLEV),KM(1,NLEV),USTAR,
     8   XX,YY,CU,
     9   CT,RIB,ZETA,WS,
     1   stu2m,stv2m,stt2m,stq2m,stu10m,stv10m,stt10m,stq10m,
     2   cp,rgas,undef,
     3   lwater, ivbitrib,
     4   VHZ,VPSIM,VAPSIM,VPSIG,VPSIHG,VTEMP,VDZETA,VDZ0,VDPSIM,
     5   VDPSIH,VZH,VXX0,VYY0,VAPSIHG,VRIB1,VWS1,VPSIH,
     9   VZETAL,VZ0L,VPSIH2,VH0,
     1   VX0PSIM,VG,VG0,VR1MG0,VZ2,VDZSEA,VAZ0,VXNUM1,VPSIGB2,VDX,
     2   VDXPSIM,VDY,VXNUM2,VDEN,VAWS1,VXNUM3,VXNUM,VDZETA1,VDZETA2,
     3   VZCOEF2,VZCOEF1,VTEMPLIN,VDPSIMC,VDPSIHC)
CI

       N = 1

C   SET VALUES OF TURBULENT VELOCITY AND KINETIC ENERGY AT THE GROUND

CB
      DO 9098 I =1,irun
      Q(I,NLEV) = QBUSTR * USTAR(I)
      QQ(I,NLEV) = 0.5 * Q(I,NLEV) * Q(I,NLEV)
9098  CONTINUE
CE

C GRADIENTS
C ---------
      DO L =1,NLEVM1
       DO I =1,irun
        DU(I,L) = (   U(I,L)-  U(I,L+1) ) * ADZ2(I,L)
        DV(I,L) = (   V(I,L)-  V(I,L+1) ) * ADZ2(I,L)
       ENDDO
      ENDDO


C    NEW CODE FOR MOIST BOUNDARY LAYER - NEW CALCULATION OF DTHV

      IF(ITER.EQ.1) THEN
       DO L =1,NLEVM1
        DO I =1,irun
         XL(I,L) = XLSAVE(I,L)
        ENDDO
       ENDDO
      ENDIF

      DO L =1,NLEVM1
       DO I =1,irun
        DTH(I,L) = ( TH(I,L)-TH(I,L+1) ) * ADZ2(I,L)
        DSH(I,L) = ( SH(I,L)-SH(I,L+1) ) * ADZ2(I,L)
        TL(I,L) = TH(I,L)*PLK(I,L)
       ENDDO
      ENDDO
      DO LL = 1,NLEVM1
      DO I = 1,irun
       call qsat ( tl(i,LL),pl(i,LL),shsat(i,LL),dum,.false. )
      ENDDO
      ENDDO
      DO L =1,NLEVM1
       DO I =1,irun
        BB(I,L) = FACEPS*SHSAT(I,L)/(TL(I,L)*TL(I,L))
        AA(I,L) = 1. / (1. + CLH * BB(I,L) )
COMMM   BB(I,L) = BB(I,L) * AA(I,L) * plke(I,L+1)
        BB(I,L) = BB(I,L) * AA(I,L)
        SBAR(I,L) = AA(I,L) * (SH(I,L) - SHSAT(I,L))
       ENDDO
      ENDDO
      DO I = 1,irun
COMMM  SSDEV(I,1) = XL(I,1)*(AA(I,1)*DSH(I,1)-BB(I,1)*DTH(I,1))
       SSDEV(I,1) = XL(I,1)*(AA(I,1)*DSH(I,1)-
     1              BB(I,1)*plke(I,2)*DTH(I,1))
       SSDEV(I,1) = B2 * KHSAVE(I,1) * SSDEV(I,1) * SSDEV(I,1)
       SVAR(I,1) = SQRT(SSDEV(I,1))
       IF ( SVAR(I,1).LT.Z1PEM25) SVAR(I,1) = Z1PEM25
      ENDDO
      DO L =2,NLEVM1
       DO I =1,irun
COMMM   SSDEV(I,L) = XL(I,L-1)*(AA(I,L)*DSH(I,L-1)-BB(I,L)*DTH(I,L-1))
        SSDEV(I,L) = XL(I,L-1)*(AA(I,L)*DSH(I,L-1)-
     1               BB(I,L)*plke(I,L)*DTH(I,L-1))
        SSDEV(I,L) = B2 * KHSAVE(I,L-1) * SSDEV(I,L-1) * SSDEV(I,L-1)
        SVAR(I,L) = SQRT(SSDEV(I,L))
COMMM   SSDEV(I,L) = XL(I,L)*(AA(I,L)*DSH(I,L)-BB(I,L)*DTH(I,L))
        SSDEV(I,L) = XL(I,L)*(AA(I,L)*DSH(I,L)-
     1               BB(I,L)*plke(I,L+1)*DTH(I,L))
        SSDEV(I,L) = B2 * KHSAVE(I,L) * SSDEV(I,L) * SSDEV(I,L)
        TEMP(I,L) = SQRT(SSDEV(I,L))
        SVAR(I,L) = (1./2.) * (SVAR(I,L) + TEMP(I,L))
        IF ( SVAR(I,L).LT.Z1PEM25) SVAR(I,L) = Z1PEM25
       ENDDO
      ENDDO
      DO L =1,NLEVM1
       DO I =1,irun
        Q1M(I,L) = SBAR(I,L) / SVAR(I,L)
        FCC(I,L) = (1./2.) * ( 1. + ERRF( P5SR*Q1M(I,L) ) )
        SHL(I,L) =  FCC(I,L) * SBAR(I,L)
        ARG(I,L)  = (1./2.)*Q1M(I,L)*Q1M(I,L)
        IF(ARG(I,L).LE.ARGMAX)
     1        SHL(I,L) =  SHL(I,L)+RSQ2PI*SVAR(I,L)*EXP(-ARG(I,L))
        BETAT(I,L) = 1. + VIRTCON * SH(I,L) - VRT1CON * SHL(I,L)
        BETAW(I,L) = VIRTCON *
     1                ( TH(I,L) + (CLH/plk(I,L)) * SHL(I,L) )
        BETAL(I,L) = ( 1. + VIRTCON*SH(I,L) - TWO*VRT1CON*SHL(I,L) )
     1     * (CLH/plke(I,L+1)) - VRT1CON * TH(I,L)
COMMM   BETAT1(I,L) = BETAT(I,L) - BB(I,L) * FCC(I,L) * BETAL(I,L)
        BETAT1(I,L) = BETAT(I,L) -
     1               BB(I,L)*plk(i,L) * FCC(I,L) * BETAL(I,L)
        BETAW1(I,L) = BETAW(I,L) + AA(I,L) * FCC(I,L) * BETAL(I,L)
       ENDDO
      ENDDO
      DO L =1,NLEVM1
       DO I =1,irun
        DTHV(I,L) =  (1./2.)*((BETAT1(I,L)+BETAT1(I,L+1))*DTH(I,L)
     1               + (BETAW1(I,L)+BETAW1(I,L+1))*DSH(I,L))
       ENDDO
      ENDDO

C GRADIENTS AT THE TOP OF THE SURFACE LAYER
C -----------------------------------------
      DO 9102 I =1,irun
      DU(I,NLEV) = CU(I)*XX(I)*AHS(I)*RVK
      DV(I,NLEV) = V(I,NLEV) * DU(I,NLEV)
      DU(I,NLEV) = U(I,NLEV) * DU(I,NLEV)
      DTHV(I,NLEV) = CT(I) * YY(I) *
     1 ((THV(I,NLEV)-THV(I,NLEVP1)) * RVK)* AHS(I)
9102  CONTINUE

C CALCULATE BRUNT-VAISALA FREQUENCIES, SHEARS, RICHARDSON NUMBERS
C ---------------------------------------------------------------
      DO L =1,NLEV
       DO I =1,irun
        STRT(I,L) = CP * DTHV(I,L) * DPK(I,L)
        DW2(I,L) = DU(I,L) * DU(I,L) + DV(I,L) * DV(I,L)
        IF ( DW2(I,L) .LE. 1.e-4 ) DW2(I,L) = 1.e-4
        RI(I,L) = STRT(I,L) / DW2(I,L)
       ENDDO
      ENDDO
C FILL RICHARDSON NUMBER AND SURFACE WIND DIAGNOSTICS
C       (THOSE NEEDED FROM FIRST TRBFLX ITERATION)
C ---------------------------------------------------
      DO L =1,NLEVM1
       DO I =1,irun
        SRI(I,L) = RI(I,L)
       ENDDO
      ENDDO
      DO 9108 I =1,irun
      SRI(I,NLEV) = RIB(I)
9108  CONTINUE
      DO 9110 I =1,irun
      SWINDS(I) = WS(I)
9110  CONTINUE
C INITIALIZE KH, KM, QE AND P3 AND ELIMINATE SMALL QQ
C ---------------------------------------------------
      DO L =1,NLEVM1
       DO I =1,irun
        KH(I,L) = 0.
        KM(I,L) = 0.
        QQE(I,L) = 0.
        QE(I,L) = 0.
        P3(I,L) = 0.
        IBITSTB(I,L) = 0
        IF ( QQ(I,L) .GT. 1.e-8 ) THEN
         INTQ(I,L) = 1
        ELSE
         INTQ(I,L) = 0
        ENDIF
        IF ( QQ(I,L).LE.1.e-8 ) THEN
         QQ(I,L) = 0.
         Q(I,L) = 0.
        ENDIF
       ENDDO
      ENDDO

       DO 300 LMINQ = 1,NLEVM1
        IBIT = 0
        DO 9116 I = 1,irun
         IF ( QQ(I,LMINQ).GT.1.e-8 ) IBIT = IBIT + 1
 9116   CONTINUE
        IF(IBIT.GE.1)GO TO 310
 300   CONTINUE
       LMINQ = NLEV-1
 310   CONTINUE
       LMINQ  = 1
       LMINQ1 = 1
       IF(LMINQ.GT.1)LMINQ1 = LMINQ - 1
C LENGTH SCALE
C ------------
       CALL TRBLEN(STRT,DW2,DZ3,Q,VKZE,VKZM,DTHV,DPK,DU,DV,XL,QXLM,
     1  NLEV,INIT,LMIN,LMINQ,LMINQ1,CP,INT1,INT2,
     2  DZITRP,STBFCN,XL0,Q1,WRKIT1,WRKIT2,WRKIT3,WRKIT4,irun)
C QE AND DIMENSIONLESS COEFFS FROM LEVEL 2 MODEL
C ----------------------------------------------
      IF( LMIN .LT. NLEV ) THEN
       NLEVML = NLEV - LMIN
        CALL TRBL20(RI(1,LMIN),STRT(1,LMIN),DW2(1,LMIN),XL(1,LMIN),
     1   KM(1,LMIN),KH(1,LMIN),QE(1,LMIN),QQE(1,LMIN),IBITSTB(1,LMIN),
     2   NLEVML,nlev,irun)
      ENDIF
C FOR INITIAL START ONLY : USE EQUILIBRIUM MODEL
C ----------------------------------------------
      IF ( INIT .EQ. 1 ) THEN
      DO L =1,NLEVM1
       DO I =1,irun
        QQ(I,L) = QQE(I,L)
        Q(I,L) = QE(I,L)
       ENDDO
      ENDDO
       INIT = 2
        CALL TRBLEN(STRT,DW2,DZ3,Q,VKZE,VKZM,DTHV,DPK,DU,DV,XL,QXLM,
     1    NLEV,INIT,LMIN,LMINQ,LMINQ1,CP,INT1,INT2,
     2  DZITRP,STBFCN,XL0,Q1,WRKIT1,WRKIT2,WRKIT3,WRKIT4,irun)
       INIT = 0
       GO TO 550
      ENDIF
C DIMENSIONLESS COEFFS AND P3 (Q LE QE)
C -------------------------------------
      IF( LMIN .LT. NLEV ) THEN
       ISTNML = irun * NLEVML
       DO L =LMIN,NLEVM1
        DO I =1,irun
         IF (  (IBITSTB(I,L).EQ.1)  .AND.
     1             ( Q(I,L) .LE. QE(I,L) ) ) THEN
          IBITSTB(I,L) = 1
         ELSE
          IBITSTB(I,L) = 0
         ENDIF
        ENDDO
       ENDDO
       DO L =LMIN,NLEVM1
        DO I =1,irun
         IF(IBITSTB(I,L).EQ.1 ) THEN
          TEMP(I,L) = Q(I,L) / QE(I,L)
          KH(I,L) = TEMP(I,L) * KH(I,L)
          KM(I,L) = TEMP(I,L) * KM(I,L)
         ENDIF
         TEMP(I,L) = 0.01 * QQE(I,L)
         IF((IBITSTB(I,L).EQ.1) .AND.
     1         ( QQ(I,L) .LE. TEMP(I,L) )) THEN
          QQ(I,L) = TEMP(I,L)
          Q(I,L) = 0.1 * QE(I,L)
         ENDIF
         IF(IBITSTB(I,L).EQ.1 ) P3(I,L) = (2.*B3) *
     1             ( QE(I,L) - Q(I,L) )
        ENDDO
       ENDDO
      ENDIF
C DIMENSIONLESS COEFFS AND P3 (Q GT QE)
C -------------------------------------
       NLEVML = NLEV - LMINQ
       CALL TRBL25(Q(1,LMINQ),XL(1,LMINQ),STRT(1,LMINQ),DW2(1,LMINQ),
     1    IBITSTB(1,LMINQ),INTQ(1,LMINQ),KM(1,LMINQ),KH(1,LMINQ),
     2    P3(1,LMINQ),NLEVML,nlev,irun)
C CALCULATE SOURCE TERM P3
C ------------------------
      IF ( LMINQ .LT. LMIN ) THEN
       LMIN = LMINQ
       ISTNML = irun * ( NLEV - LMIN )
      ENDIF
      IF( LMIN .LT. NLEV ) THEN
       DO L =LMIN,NLEVM1
        DO I =1,irun
         P3(I,L) = P3(I,L) * DTAU / XL(I,L)
         TEMP(I,L) = QQE(I,L) * P3(I,L)
         XQ(I,L) = QQE(I,L) - QQ(I,L)
        ENDDO
       ENDDO
       DO L =LMIN,NLEVM1
        DO I =1,irun
         IF( ( (IBITSTB(I,L).EQ.1) .AND.
     1             ( XQ(I,L) .LT. TEMP(I,L) ) )
     2                  .OR.
     3       ( (IBITSTB(I,L).EQ.0) .AND.
     4             ( XQ(I,L) .GT. TEMP(I,L) ) ) )
     5   P3(I,L) = XQ(I,L) / QQE(I,L)
        ENDDO
       ENDDO
      ENDIF
 550  CONTINUE
C DIAGNOSTIC PROFILES : INITIAL RI AND QQ
C ---------------------------------------
      IF ( TPROF .AND. FIRST ) THEN
      DO 9118 I =1,irun
       RIBIN(I) = RIB(I)
       CUIN(I) = CU(I)
       CTIN(I) = CT(I)
       USTARIN(I) = USTAR(I)
       RHOSIN(I) = RHODZ2(I,NLEV)
       Z0IN(I) = Z0(I)
       ZETAIN(I) = ZETA(I)
9118  CONTINUE
      DO L =1,NLEV
       DO I =1,irun
        RIINIT(I,L) = RI(I,L)
        QQINIT(I,L) = QQ(I,L)
       ENDDO
      ENDDO
      ENDIF

C Zero diffusion of TKE above 10 mb
      do L = 1,nlev-1
        do i = 1,irun
c        if(pl(i,L).le.10.) then
c         qxlm(i,L) = 0.
c        endif
         if(pl(i,L).le.150.) then
          qxlm(i,L) = min(qxlm(i,L),5. _d 0)
         endif
        enddo
      enddo

C UPDATE TURBULENT KINETIC ENERGY QQ
C ----------------------------------
      NLEVMQ = NLEV - LMINQ1
      ISTNMQ = irun * NLEVMQ
      DO L =LMINQ1,NLEVM1
       DO I =1,irun
        RHOKDZ(I,L) = RHODZ1(I,L) * QXLM(I,L)
       ENDDO
      ENDDO
       CALL TRBDIF(QQ(1,LMINQ1),P3(1,LMINQ1),RHOKDZ(1,LMINQ1),
     1       FLXFCE(1,LMINQ1),DTHS,DELTHS,NLEVMQ,1,1.0 _d -20,irun)

C SAVE KH BEFORE ADDING DIMENSIONS FOR USE BY MOIST BOUYANCY CALCULATION

      DO L =1,NLEVM1
       DO I =1,irun
        KHSAVE(I,L) = KH(I,L)
       ENDDO
      ENDDO

C   DIMENSIONAL DIFFUSION COEFFS INCLUDING BACKGROUND AMOUNTS

      IF(LMINQ1.GT.1)THEN
       ISTLMQ = irun * (LMINQ1-1)
CB
       DO L =1,LMINQ1-1
        DO I =1,irun
         KM(I,L) = KMBG
         KH(I,L) = KHBG
        ENDDO
       ENDDO
CE
      ENDIF

CB
      DO L =LMINQ1,NLEVM1
       DO I =1,irun
        Q(I,L) = 2. * QQ(I,L)
        Q(I,L) = SQRT(Q(I,L))
        XQ(I,L)= XL(I,L) * Q(I,L)
        KM(I,L)=XQ(I,L)*KM(I,L)+KMBG
        KH(I,L)=XQ(I,L)*KH(I,L)+KHBG
       ENDDO
      ENDDO
CE
C Zero diffusion of u,v,t,q above 10 mb
      do L = 1,nlev-1
        do i = 1,irun
c        if(pl(i,L).le.10.) then
c         kh(i,L) = 0.
c         km(i,L) = 0.
c        endif
         if(pl(i,L).le.150.) then
          kh(i,L) = min(kh(i,L),5. _d 0)
          km(i,L) = min(km(i,L),5. _d 0)
         endif
        enddo
      enddo

C   CALCULATE INTERNAL FLUXES AND UPDATE PROGNOSTIC VARIABLES: TH AND S

      DO L =1,NLEV
       DO I =1,irun
        TEMP(I,L) = RHOZPK(I,L) * KH(I,L)
        DELTH(I,L) = 0.
       ENDDO
      ENDDO
      DO 9132 I =1,irun
      DELTH(I,NLEVP1) = 1.
9132  CONTINUE
       CALL TRBDIF(TH,DELTH,TEMP,FLXFPK,DTHS,DELTHS,NLEV,2,0. _d 0,irun)
      do i = 1,irun
       hsturb(i) = -1.* dths(i)
       dhsdtc(i) = -1.* delths(i)
      enddo
      do L = 1,nlev
       do i = 1,irun
        dthdthg(i,L) = delth(i,L)
       enddo
      enddo
      do L = 1,nlev
       do i = 1,irun
        transth(i,L) = temp(i,L)
       enddo
      enddo

      DO L =1,NLEV
       DO I =1,irun
        RHOKDZ(I,L) = RHODZ2(I,L) * KH(I,L)
        DELSH(I,L) = 0.
       ENDDO
      ENDDO
      DO 9140 I =1,irun
      DELSH(I,NLEVP1) = 1.
9140  CONTINUE

       CALL TRBDIF(SH,DELSH,RHOKDZ,FLXFAC,DTHL,DELTHL,NLEV,
     &                                     2,0. _d 0,irun)
      do i = 1,irun
       eturb(i) = -1.* dthl(i)
       dedqa(i) = -1.* delthl(i)
      enddo
      do L = 1,nlev
       do i = 1,irun
        dshdshg(i,L) = delsh(i,L)
       enddo
      enddo
      do L = 1,nlev
       do i = 1,irun
        transsh(i,L) = rhokdz(i,L)
       enddo
      enddo

C   Update Tracers Due to Turbulent Diffusion

      do i = 1,irun
      rhokdz(i,nlev) = 0.0
      enddo

c     do nt = 1,ntrace
c     do  i = 1,irun
c     tracers(i,nlev+1,nt) = tracers(i,nlev,nt)
c     enddo
c     CALL TRBDIF(tracers(1,1,nt),DELSH,RHOKDZ,FLXFAC,DTHL,DELTHL,
c    .                                         NLEV,4,0. _d 0,irun)
c     enddo

C   CALCULATE INTERNAL FLUXES AND UPDATE PROGNOSTIC VARIABLES: U AND V

      DO L =1,NLEV
       DO I =1,irun
        RHOKDZ(I,L) = RHODZ2(I,L) * KM(I,L)
       ENDDO
      ENDDO
       CALL TRBDIF(U,V,RHOKDZ,FLXFAC,DTHS,DELTHS,NLEV,3,0. _d 0,irun)
C ( FILL DIAGNOSTIC ARRAYS IF REQUIRED )
      DO L =1,NLEV
       DO I =1,irun
        WU(I,L) = WU(I,L) + RHOKDZ(I,L) * ( U(I,L+1) - U(I,L) )
        WV(I,L) = WV(I,L) + RHOKDZ(I,L) * ( V(I,L+1) - V(I,L) )
       ENDDO
      ENDDO
      DO L =1,NLEVM1
       DO I =1,irun
        IF ( QQ(I,L) .GT. QQMIN ) THEN
         IBITSTB(I,L) = 1
        ELSE
         IBITSTB(I,L) = 0
        ENDIF
        IF( IBITSTB(I,L).EQ.1 ) FREQDG(I,L) = FREQDG(I,L) + aitr
       ENDDO
      ENDDO
       do i = 1,irun
        qqcolmin(i) = qq(i,nlev)*0.1
        qqcolmax(i) = qq(i,nlev)
        levpbl(i) = nlev
       enddo
       DO L = nlev-1,1,-1
        DO I = 1,irun
         IF ( (qq(i,l).gt.qqcolmax(I)).and.(levpbl(i).eq.nlev))then
          qqcolmax(i) = qq(i,l)
          qqcolmin(i) = 0.1*qqcolmax(I)
         endif
         if((qq(i,l).lt.qqcolmin(i)).and.(levpbl(i).eq.nlev))
     1                                                levpbl(i)=l
        enddo
       enddo
       do i = 1,irun
        lp = levpbl(i)
        if(lp.lt.nlev)then
         pbldpth(I) = pbldpth(I) + ( (PLE(I,nlev+1)-PLE(I,Lp+2)) +
     1    ( (ple(i,lp+2)-ple(i,lp+1))*(qq(i,lp+1)-qqcolmin(i))
     2          / (qq(i,lp+1)-qq(i,lp)) ) ) * aitr
        else
         pbldpth(I) = pbldpth(I) + ( (PLE(I,nlev+1)-PLE(I,2)) +
     1    ( (ple(i,2)-ple(i,1))*(qq(i,1)-qqcolmin(i))
     2          / qq(i,1) ) ) * aitr
        endif
       enddo
      do i=1,irun
      sustar(i) = sustar(i) + aitr*ustar(i)
      enddo
      do i=1,irun
      sz0(i) = sz0(i) + aitr*z0(i)
      enddo
      DO L =1,NLEV
       DO I =1,irun
        EU(I,L) = EU(I,L) + AITR*KM(I,L)
        ET(I,L) = ET(I,L) + AITR*KH(I,L)
       ENDDO
      ENDDO
      DO I =1,irun
      scu(I) = scu(I) + AITR*cu(I)
      enddo
      DO I =1,irun
      sct(I) = sct(I) + AITR*ct(I)
      enddo
      IF(tprof) then
       DO L =1,NLEVM1
        DO I =1,irun
         XLDIAG(I,L) = XLDIAG(I,L) + AITR*XL(I,L)
        ENDDO
       ENDDO
      endif
      FIRST = .FALSE.

C SAVE XL,CT,XX,YY,ZETA FOR USE BY MOIST BOUYANCY CALCULATION

      IF(ITER.EQ.ITRTRB)THEN
       DO L =1,NLEVM1
        DO I =1,irun
         XLSAVE(I,L) = XL(I,L)
        ENDDO
       ENDDO
       DO I = 1,irun
        CTSAVE(I) = CT(I)
        XXSAVE(I) = XX(I)
        YYSAVE(I) = YY(I)
        ZETASAVE(I) = ZETA(I)
       ENDDO
      ENDIF

      DO L =1,NLEV
       DO I =1,irun
        turbfcc(I,L) = turbfcc(I,L) + fcc(I,L) * aitr
        qliq(I,L) = qliq(I,L) + shl(I,L) * aitr
       ENDDO
      ENDDO

C   END OF MAIN LOOP

 2000 CONTINUE
      DO L =1,NLEV
       DO I =1,irun
        WU(I,L) =  WU(I,L) * AITR
        WV(I,L) =  WV(I,L) * AITR
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       SUBROUTINE SFCFLX(NN,VUS,VVS,VTHV1,VTHV2,VTH1,VTH2,VSH1,
     1      VSH2,VPK,VPKE,VPE,VZ0,IVWATER,VHS,
     2      VAHS,FIRST,LAST,N,IRUN,aitr,VRHO,VRHOZPK,VKH,VKM,
     3      VUSTAR,VXX,VYY,VCU,VCT,VRIB,VZETA,VWS,
     4      stu2m,stv2m,stt2m,stq2m,stu10m,stv10m,stt10m,stq10m,
     5      cp,rgas,undef,
     6      lwater, ivbitrib,
     7      VHZ,VPSIM,VAPSIM,VPSIG,VPSIHG,VTEMP,VDZETA,VDZ0,VDPSIM,
     8      VDPSIH,VZH,VXX0,VYY0,VAPSIHG,VRIB1,VWS1,VPSIH,VZETAL,
     9      VZ0L,VPSIH2,VH0,
     3 VX0PSIM,VG,VG0,VR1MG0,VZ2,VDZSEA,VAZ0,VXNUM1,VPSIGB2,VDX,
     4 VDXPSIM,VDY,VXNUM2,VDEN,VAWS1,VXNUM3,VXNUM,VDZETA1,VDZETA2,
     5 VZCOEF2,VZCOEF1,VTEMPLIN,VDPSIMC,VDPSIHC)
C**********************************************************************
C  SUBROUTINE SFCFLX - COMPUTES SURFACE TRANSFER COEFFICIENTS
C                    - CALLED FROM TRBFLX
C   ARGUMENTS ::
C     INPUT:
C     ------
C    US            -         U - COMPONENT OF SURFACE WIND
C    VS            -         V - COMPONENT OF SURFACE WIND
C    THV1          -         VIRTUAL POTENTIAL TEMPERATURE AT NLAY
C    THV2          -         VIRTUAL POTENTIAL TEMPERATURE AT GROUND
C    TH1           -         POTENTIAL TEMPERATURE AT NLAY
C    TH2           -         POTENTIAL TEMPERATURE AT GROUND
C    SH1           -         SPECIFIC HUMIDITY AT NLAY
C    SH2           -         SPECIFIC HUMIDITY AT GROUND
C    PK            -         EVEN LEVEL PRESSURE ** KAPPA AT LEVEL NLAY
C    PKE           -         EDGE LEVEL PRESSURE ** KAPPA AT GROUND
C    PE            -         SURFACE PRESSURE
C    Z0            -         SURFACE ROUGHNESS
C    WATER         -         BIT ARRAY - '1' OVER OCEANS
C    HS            -         DEPTH OF SURFACE LAYER
C    AHS           -         ONE / HS
C    FIRST         -         LOGICAL .TRUE. FOR FIRST TRBFLX ITERATION
C    LAST          -         LOGICAL .TRUE. FOR LAST TRBFLX ITERATION
C    N             -         NUMBER OF SFCFLX ITERATIONS
C     OUTPUT:
C     -------
C    RHO           -         DENSITY AT 10M HEIGHT
C    RHOZPK        -         RHO * P**K AT THE SURFACE
C    KH            -         HEAT TRANSFER COEFFICIENT (CT*USTAR)
C    KM            -         MOMENTUM TRANSFER COEFFICIENT (CU*USTAR)
C    USTAR         -         FRICTION VELOCITY
C    XX            -         PHIM(ZETA) - DIMENSIONLESS WIND SHEAR
C    YY            -         PHIH(ZETA) - DIMENSIONLESS TEMP GRADIENT
C    CU            -         MOMENTUM TRANSPORT COEFFICIENT
C    CT            -         HEAT TRANSPORT COEFFICIENT
C**********************************************************************
      implicit none

C Argument List Declarations
      integer nn,n,irun
      _RL aitr,cp,rgas,undef
      _RL VUS(IRUN),VVS(IRUN),VTHV1(IRUN),VTHV2(IRUN)
      _RL VTH1(IRUN),VTH2(IRUN),VSH1(IRUN),VSH2(IRUN)
      _RL VPK(IRUN),VPKE(IRUN),VPE(IRUN)
      _RL VZ0(IRUN),VHS(IRUN),VAHS(IRUN)
      integer IVWATER(IRUN)
      LOGICAL FIRST,LAST
      _RL VRHO(IRUN),VRHOZPK(IRUN)
      _RL VKM(IRUN),VKH(IRUN),VUSTAR(IRUN),VXX(IRUN)
      _RL VYY(IRUN),VCU(IRUN),VCT(IRUN),VRIB(IRUN)
      _RL VZETA(IRUN),VWS(IRUN)
      _RL stu2m(irun),stv2m(irun),stt2m(irun),stq2m(irun)
      _RL stu10m(irun),stv10m(irun),stt10m(irun),stq10m(irun)
      LOGICAL LWATER
      integer IVBITRIB(irun)
      _RL VHZ(irun),VPSIM(irun),VAPSIM(irun),VPSIG(irun),VPSIHG(irun)
      _RL VTEMP(irun),VDZETA(irun),VDZ0(irun),VDPSIM(irun)
      _RL VDPSIH(irun),VZH(irun),VXX0(irun),VYY0(irun)
      _RL VAPSIHG(irun),VRIB1(irun),VWS1(irun)
      _RL VPSIH(irun),VZETAL(irun),VZ0L(irun),VPSIH2(irun),VH0(irun)
      _RL VX0PSIM(irun),VG(irun),VG0(irun),VR1MG0(irun)
      _RL VZ2(irun),VDZSEA(irun),VAZ0(irun),VXNUM1(irun)
      _RL VPSIGB2(irun),VDX(irun),VDXPSIM(irun),VDY(irun)
      _RL VXNUM2(irun),VDEN(irun),VAWS1(irun),VXNUM3(irun)
      _RL VXNUM(irun),VDZETA1(irun),VDZETA2(irun)
      _RL VZCOEF2(irun),VZCOEF1(irun),VTEMPLIN(irun)
      _RL VDPSIMC(irun),VDPSIHC(irun)

C Local Variables
      _RL USTMX3,USTZ0S,Z0MIN,H0BYZ0,USTH0S,H0VEG,Z0VEGM,PRFAC
      _RL XPFAC,DIFSQT
      PARAMETER ( USTMX3 =   0.0632456)
      PARAMETER ( USTZ0S =   0.2030325E-5)
      PARAMETER ( Z0MIN  =  USTZ0S/USTMX3)
      PARAMETER ( H0BYZ0 =    30.0    )
      PARAMETER ( USTH0S =  H0BYZ0*USTZ0S )
      PARAMETER ( H0VEG  =   0.01     )
      PARAMETER ( Z0VEGM =   0.005    )
      PARAMETER ( PRFAC  = 0.595864   )
      PARAMETER ( XPFAC  = .55        )
      PARAMETER ( DIFSQT  = 3.872983E-3)

      _RL psihdiag(irun),psimdiag(irun)
      _RL getcon,vk,rvk,vk2,bmdl
      integer iwater,itype
      integer i,iter

      vk = getcon('VON KARMAN')
      rvk = 1./vk
      vk2 = vk*vk
      BMDL    = VK * XPFAC * PRFAC / DIFSQT

C     DETERMINE SURFACE WIND MAGNITUDE AND BULK RICHARDSON NUMBER

      DO 9000 I = 1,IRUN
       VWS(I) = VUS(I) * VUS(I) + VVS(I) * VVS(I)
       IF ( VWS(I) .LE. 1.e-4) VWS(I) = 1.e-4
       VRIB(I) = ( CP * (VPKE(I)-VPK(I)) ) *
     1       (VTHV1(I) - VTHV2(I)) / VWS(I)
       VWS(I) = SQRT( VWS(I) )
 9000 CONTINUE

C     INITIALIZATION (FIRST TRBFLX ITERATION)
C  INITIAL GUESS FOR ROUGHNESS LENGTH Z0 OVER WATER

      IF (.NOT. FIRST) GO TO 100

      IWATER = 0
      DO 9002 I = 1,IRUN
       IF (IVWATER(I).EQ.1)  IWATER = IWATER + 1
 9002 CONTINUE
      LWATER = .FALSE.
      IF(IWATER.GE.1)LWATER = .TRUE.

      IF(LWATER)THEN
       DO 9004 I = 1,IRUN
        IF (IVWATER(I).EQ.1) VZ0(I) = 0.0003
 9004  CONTINUE
      ENDIF
      do i = 1,irun
       vh0(i) = h0byz0 * vz0(i)
       if(vz0(i).ge.z0vegm)vh0(i) = h0veg
       enddo

C     CU AND PSIHG FOR NEUTRALLY STRATIFIED FLOW

      DO 9006 I = 1,IRUN
       VHZ(I) = VHS(I) / VZ0(I)
       VPSIM(I) = LOG( VHZ(I) )
       VAPSIM(I) = 1. / VPSIM(I)
       VCU(I) = VK * VAPSIM(I)
       VUSTAR(I) = VCU(I) * VWS(I)

       VPSIG(I) = VH0(I) * VUSTAR(I) - USTH0S
       if(VPSIG(I).lt.0.)  VPSIG(I) = 0.
       VPSIG(I) = SQRT( VPSIG(I) )
       VPSIG(I) = BMDL * VPSIG(I)
       VPSIHG(I) = VPSIM(I) + VPSIG(I)
 9006 CONTINUE

C     LINEAR CORRECTION FOR ERROR IN ROUGHNESS LENGTH Z0

      IF(LWATER)THEN
       DO 9008 I = 1,IRUN
        VTEMP(I) = 0.
 9008  CONTINUE
       CALL LINADJ(NN,VRIB,VRIB,VWS,
     1  VWS,VZ0,VUSTAR,IVWATER,
     2  VAPSIM,VTEMP,VTEMP,
     3  VTEMP,VTEMP,VTEMP,
     4  VTEMP,VTEMP,1,.TRUE.,IRUN,VDZETA,
     5  VDZ0,VDPSIM,VDPSIH,
     6  IVBITRIB,
     3 VX0PSIM,VG,VG0,VR1MG0,VZ2,VDZSEA,VAZ0,VXNUM1,VPSIGB2,VDX,
     4 VDXPSIM,VDY,VXNUM2,VDEN,VAWS1,VXNUM3,VXNUM,VDZETA1,VDZETA2,
     5 VZCOEF2,VZCOEF1,VTEMPLIN,VDPSIMC,VDPSIHC)
       DO 9010 I = 1,IRUN
        IF ( IVWATER(I).EQ.1 ) THEN
         VCU(I) = VCU(I) * (1. - VDPSIM(I)*VAPSIM(I))
         VZ0(I) = VZ0(I) + VDZ0(I)
         ENDIF
         IF ( IVWATER(I).EQ.1) THEN
         IF ( VZ0(I) .LE. Z0MIN ) VZ0(I) = Z0MIN
         vh0(i) = h0byz0 * vz0(i)
         VPSIG(I) = VH0(I) * VCU(I) * VWS(I) - USTH0S
         if(VPSIG(I).lt.0.)  VPSIG(I) = 0.
         VPSIG(I) = SQRT( VPSIG(I) )
         VPSIG(I) = BMDL * VPSIG(I)
         VPSIHG(I) = VPSIM(I) + VDPSIH(I) + VPSIG(I)
        ENDIF
 9010  CONTINUE
      ENDIF

C  INITIAL GUESS FOR STABILITY PARAMETER ZETA

      DO 9012 I = 1,IRUN
       VZETA(I) = VK2 * VRIB(I) / (VCU(I) * VCU(I) * VPSIHG(I))
 9012 CONTINUE

C  RECOMPUTE CU, ESTIMATE PSIHG AND UPDATE ZETA AND Z0

      DO 9014 I = 1,IRUN
       VZH(I) = VZ0(I) * VAHS(I)
 9014 CONTINUE
      CALL PSI (VZETA,VZH,VPSIM,
     1    VTEMP,IRUN,VXX,VXX0,VYY,
     2    VYY0,2)
      DO 9016 I = 1,IRUN
       VCU(I) = VK / VPSIM(I)
       VPSIG(I) = VH0(I) * VCU(I) * VWS(I) - USTH0S
       if(VPSIG(I).lt.0.)  VPSIG(I) = 0.
       VPSIG(I) = SQRT(VPSIG(I))
       VPSIG(I) = BMDL * VPSIG(I)
       VPSIHG(I) = VPSIM(I) + VPSIG(I)
       VZETA(I) = VK2 * VRIB(I) / (VCU(I) * VCU(I) * VPSIHG(I))
 9016 CONTINUE

      IF(LWATER)THEN
CCCOOOMMMM ADDED 'WHERE WATER'
       DO 9018 I = 1,IRUN
        IF (IVWATER(I).EQ.1) VUSTAR(I) = VCU(I) * VWS(I)
 9018  CONTINUE
        CALL ZCSUB ( VUSTAR,VHZ,IVWATER,.FALSE.,IRUN,VTEMP)
       DO 9020 I = 1,IRUN
        IF (IVWATER(I).EQ.1 ) then
         VZ0(I) = VTEMP(I)
         IF ( VZ0(I) .LE. Z0MIN ) VZ0(I) = Z0MIN
         vh0(i) = h0byz0 * vz0(i)
        endif
 9020  CONTINUE
      ENDIF

      GO TO 125

C  LINEARLY UPDATE ZETA AND Z0 FOR SECOND OR GREATER TRBFLX ITERATION

 100  CONTINUE

      CALL LINADJ(NN,VRIB1,VRIB,VWS1,
     1  VWS,VZ0,VUSTAR,IVWATER,
     2  VAPSIM,VAPSIHG,VPSIH,
     3  VPSIG,VXX,VXX0,
     4  VYY,VYY0,2,LWATER,IRUN,VDZETA,
     5  VDZ0,VDPSIM,VDPSIH,
     6  IVBITRIB,
     3 VX0PSIM,VG,VG0,VR1MG0,VZ2,VDZSEA,VAZ0,VXNUM1,VPSIGB2,VDX,
     4 VDXPSIM,VDY,VXNUM2,VDEN,VAWS1,VXNUM3,VXNUM,VDZETA1,VDZETA2,
     5 VZCOEF2,VZCOEF1,VTEMPLIN,VDPSIMC,VDPSIHC)

      DO 9022 I = 1,IRUN
       VZETA(I) = VZETA(I) + VZETAL(I) * VDZETA(I)
       IF (IVBITRIB(I).EQ.1 )VZETA(I) =
     1       VPSIM(I) * VPSIM(I) * VRIB(I) * VCT(I) * RVK
 9022 CONTINUE

      IF ( LWATER ) THEN
       DO 9024 I = 1,IRUN
        IF (IVWATER(I).EQ.1 ) then
         VZ0(I) = VZ0(I) + VZ0L(I) * VDZ0(I)
         IF (VZ0(I) .LE. Z0MIN ) VZ0(I) = Z0MIN
         vh0(i) = h0byz0 * vz0(i)
        endif
 9024  CONTINUE
      ENDIF

 125  CONTINUE

C  ITERATIVE LOOP - N ITERATIONS
C     COMPUTE CU AND CT

      DO 200 ITER = 1,N
       DO 9026 I = 1,IRUN
        VZH(I) = VZ0(I) * VAHS(I)
 9026  CONTINUE
       CALL PSI (VZETA,VZH,VPSIM,
     1    VPSIH,IRUN,VXX,VXX0,VYY,
     2    VYY0,1)
       DO 9028 I = 1,IRUN
        VCU(I) = VK / VPSIM(I)
        VUSTAR(I) = VCU(I) * VWS(I)

        VPSIG(I) = VH0(I) * VUSTAR(I) - USTH0S
        if(VPSIG(I).lt.0.)  VPSIG(I) = 0.
        VPSIG(I) = SQRT(VPSIG(I))
        VPSIG(I) = BMDL * VPSIG(I)
        VPSIHG(I) = VPSIH(I) + VPSIG(I)

C  LINEAR CORRECTIONS FOR CU, CT, ZETA, AND Z0

        VAPSIM(I) = VCU(I) * RVK
        VAPSIHG(I) = 1. / VPSIHG(I)
        VRIB1(I) = VAPSIM(I) * VAPSIM(I) * VPSIHG(I) * VZETA(I)
 9028  CONTINUE

       ITYPE = 3
       IF(ITER.EQ.N) ITYPE = 4
       IF( (ITYPE.EQ.4) .AND. (.NOT.LAST) ) ITYPE = 5

       CALL LINADJ(NN,VRIB1,VRIB,VWS,
     1  VWS,VZ0,VUSTAR,IVWATER,
     2  VAPSIM,VAPSIHG,VPSIH,
     3  VPSIG,VXX,VXX0,
     4  VYY,VYY0,ITYPE,LWATER,IRUN,VDZETA,
     5  VDZ0,VDPSIM,VDPSIH,
     6  IVBITRIB,
     3 VX0PSIM,VG,VG0,VR1MG0,VZ2,VDZSEA,VAZ0,VXNUM1,VPSIGB2,VDX,
     4 VDXPSIM,VDY,VXNUM2,VDEN,VAWS1,VXNUM3,VXNUM,VDZETA1,VDZETA2,
     5 VZCOEF2,VZCOEF1,VTEMPLIN,VDPSIMC,VDPSIHC)

C  UPDATES OF ZETA, Z0, CU AND CT

       IF (ITYPE.EQ.5) THEN
        DO 9030 I = 1,IRUN
         VZETAL(I) = VZETA(I)
         VZ0L(I) = VZ0(I)
 9030   CONTINUE
       ENDIF

       DO 9032 I = 1,IRUN
        VZETA(I) = VZETA(I) * ( 1. + VDZETA(I) )
        IF (IVBITRIB(I).EQ.1 ) VZETA(I) =
     1        VPSIM(I) * VPSIM(I) * VRIB(I) * VAPSIHG(I)
 9032  CONTINUE

       IF ( LWATER ) THEN
        DO 9034 I = 1,IRUN
         IF (IVWATER(I).EQ.1 ) then
          VZ0(I) = VZ0(I) * ( 1. + VDZ0(I) )
          IF (VZ0(I) .LE. Z0MIN ) VZ0(I) = Z0MIN
          vh0(i) = h0byz0 * vz0(i)
         endif
 9034   CONTINUE
       ENDIF

       IF ( ITER .EQ. N ) THEN
        DO 9036 I = 1,IRUN
         VPSIM(I) = VPSIM(I) + VDPSIM(I)
         VCU(I) = VK / VPSIM(I)
         VUSTAR(I) = VCU(I) * VWS(I)

         VPSIG(I) = VH0(I) * VUSTAR(I) - USTH0S
         if(VPSIG(I).lt.0.)  VPSIG(I) = 0.
         VPSIG(I) = SQRT(VPSIG(I))
         VPSIG(I) = BMDL * VPSIG(I)
         VPSIHG(I) = VPSIH(I) + VDPSIH(I) + VPSIG(I)
         VCT(I) = VK / VPSIHG(I)
 9036   CONTINUE
       ENDIF

C  SAVE VALUES OF RIB AND WS FOR NEXT ITERATION OF TRBFLX

       IF (ITYPE.EQ.5) THEN
        DO 9038 I = 1,IRUN
         VRIB1(I) = VRIB(I)
         VWS1(I) = VWS(I)
 9038   CONTINUE
       ENDIF

 200  CONTINUE

C  CALCULATE RHO-SURFACE ( KG / M**3 )

      IF (FIRST) THEN
       DO I = 1,IRUN
        VTEMP(I) =  10. * VAHS(I) * VZETA(I)
        VZH(I) = VZ0(I) * 0.1
       ENDDO
       CALL PSI (VTEMP,VZH,VHZ,
     1    VPSIH2,IRUN,VHZ,VHZ,VHZ,
     2    VHZ,3)
       DO I = 1,IRUN
        VTEMP(I) = ( VPSIH2(I) + VPSIG(I) ) / VPSIHG(I)
        VRHO(I) = VPKE(I)*( VTH2(I) + VTEMP(I) * (VTH1(I)-VTH2(I)) )
        VRHO(I) = VPE(I)*100. / ( RGAS * VRHO(I) )
       ENDDO
      ENDIF

C interpolate uvtq to 2m and to 10 meters for diagnostic output
C  use psih and psim which represent non-dim change from ground
C                 to specified level
C and multiply theta by surface p**kappa to get temperatures

        do i = 1,irun
         vtemp(i) = 2. * vahs(i) * vzeta(i)
         vzh(i) = vz0(i) * 0.5
         if(vz0(i).ge.2.)vzh(i) = 0.9
        enddo
        call psi(vtemp,vzh,psimdiag,psihdiag,irun,vhz,vhz,vhz,vhz,1)
        do i = 1,irun
         stu2m(i) = (psimdiag(i)/vpsim(i) * vus(i))
         stv2m(i) = (psimdiag(i)/vpsim(i) * vvs(i))
         stt2m(i) = ( (vth2(i) + ((psihdiag(i)+vpsig(i))/vpsihg(i))*
     1                             (vth1(i)-vth2(i))) ) * vpke(i)
         stq2m(i) = (vsh2(i) + ((psihdiag(i)+vpsig(i))/vpsihg(i))*
     1                                             (vsh1(i)-vsh2(i)))
         if(vz0(i).ge.2.)then
          stu2m(i) = UNDEF
          stv2m(i) = UNDEF
          stt2m(i) = UNDEF
          stq2m(i) = UNDEF
         endif
        enddo
        do i = 1,irun
         vtemp(i) = 10. * vahs(i) * vzeta(i)
         vzh(i) = vz0(i) * 0.1
        enddo
        call psi(vtemp,vzh,psimdiag,psihdiag,irun,vhz,vhz,vhz,vhz,1)
        do i = 1,irun
         stu10m(i) = (psimdiag(i)/vpsim(i) * vus(i))
         stv10m(i) = (psimdiag(i)/vpsim(i) * vvs(i))
         stt10m(i) = ( (vth2(i) + ((psihdiag(i)+vpsig(i))/vpsihg(i))*
     1                            (vth1(i)-vth2(i))) ) * vpke(i)
         stq10m(i) = (vsh2(i) + ((psihdiag(i)+vpsig(i))/vpsihg(i))*
     1                                             (vsh1(i)-vsh2(i)))
        enddo

C  EVALUATE TURBULENT TRANSFER COEFFICIENTS

      DO 9044 I = 1,IRUN
       VRHOZPK(I) = VRHO(I) * VPKE(I)
       VKH(I) = VUSTAR(I) * VCT(I)
       VKM(I) = VUSTAR(I) * VCU(I)
 9044 CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE PHI(Z,PHIM,PHIH,IFLAG,N)
C**********************************************************************
C  FUNCTION PHI - SOLVES KEYPS EQUATIONS
C               - CALLED FROM PSI
C  DESCRIPTION OF PARAMETERS
C     Z     -  INPUTED VALUE OF MONIN- OBUKHOV STABILITY PARAMETER ZETA
C               TIMES APPROPRIATE CONSTANT
C     PHIM  -  OUTPUTED SOLUTION OF KEYPS EQUATION FOR MOMENTUM
C     PHIH  -  OUTPUTED SOLUTION OF KEYPS EQUATION FOR SCALARS
C     IFLAG -  FLAG TO DETERMINE IF X IS NEEDED (IFLAG=2), Y IS NEEDED
C                  (IFLAG=3), OR BOTH (IFLAG=1)
C     N     -  LENGTH OF VECTOR TO BE SOLVED
C**********************************************************************
      implicit none

C Argument List Declarations
      integer n,iflag
      _RL PHIM(N),PHIH(N),Z(N)

C Local Variables
      integer I1(N),I2(N)
      _RL ZSTAR(N),E1(N),E2(N),TEMP1(N)

      _RL PHIM0(385),ZLINM1(75),ZLINM2(75),ZLINM3(36)
      _RL ZLOGM1(74),ZLOGM2(75),ZLOGM3(50)
      _RL PHIH0(385),ZLINH1(75),ZLINH2(75),ZLINH3(36)
      _RL ZLOGH1(74),ZLOGH2(75),ZLOGH3(50)
      EQUIVALENCE (PHIM0(1),ZLINM1(1)),(PHIM0(76),ZLINM2(1))
      EQUIVALENCE (PHIM0(151),ZLINM3(1))
      EQUIVALENCE (PHIM0(187),ZLOGM1(1)),(PHIM0(261),ZLOGM2(1))
      EQUIVALENCE (PHIM0(336),ZLOGM3(1))
      EQUIVALENCE (PHIH0(1),ZLINH1(1)),(PHIH0(76),ZLINH2(1))
      EQUIVALENCE (PHIH0(151),ZLINH3(1))
      EQUIVALENCE (PHIH0(187),ZLOGH1(1)),(PHIH0(261),ZLOGH2(1))
      EQUIVALENCE (PHIH0(336),ZLOGH3(1))

       DATA ZLOGM1/
     &             0.697894,0.678839,0.659598,0.640260,
     &  0.620910,0.601628,0.582486,0.563550,0.544877,
     &  0.526519,0.508516,0.490903,0.473708,0.456951,
     &  0.440649,0.424812,0.409446,0.394553,0.380133,
     &  0.366182,0.352695,0.339664,0.327082,0.314938,
     &  0.303222,0.291923,0.281029,0.270528,0.260409,
     &  0.250659,0.241267,0.232221,0.223509,0.215119,
     &  0.207041,0.199264,0.191776,0.184568,0.177628,
     &  0.170949,0.164519,0.158331,0.152374,0.146641,
     &  0.141123,0.135813,0.130702,0.125783,0.121048,
     &  0.116492,0.112107,0.107887,0.103826,0.0999177,
     &  0.0961563,0.0925364,0.0890528,0.0857003,0.0824739,
     &  0.0793690,0.0763810,0.0735054,0.0707380,0.0680749,
     &  0.0655120,0.0630455,0.0606720,0.0583877,0.0561895,
     &  0.0540740,0.0520382,0.0500790,0.0481936,0.0463791/
       DATA ZLOGM2/
     &  0.0446330,0.0429526,0.0413355,0.0397792,0.0382816,
     &  0.0368403,0.0354533,0.0341185,0.0328340,0.0315978,
     &  0.0304081,0.0292633,0.0281616,0.0271013,0.0260809,
     &  0.0250990,0.0241540,0.0232447,0.0223695,0.0215273,
     &  0.0207168,0.0199369,0.0191862,0.0184639,0.0177687,
     &  0.0170998,0.0164560,0.0158364,0.0152402,0.0146664,
     &  0.0141142,0.0135828,0.0130714,0.0125793,0.0121057,
     &  0.0116499,0.0112113,0.0107892,0.0103830,0.999210E-2,
     &  0.961590E-2,0.925387E-2,0.890547E-2,0.857018E-2,0.824752E-2,
     &  0.793701E-2,0.763818E-2,0.735061E-2,0.707386E-2,0.680754E-2,
     &  0.655124E-2,0.630459E-2,0.606722E-2,0.583880E-2,0.561897E-2,
     &  0.540742E-2,0.520383E-2,0.500791E-2,0.481937E-2,0.463792E-2,
     &  0.446331E-2,0.429527E-2,0.413355E-2,0.397793E-2,0.382816E-2,
     &  0.368403E-2,0.354533E-2,0.341185E-2,0.328340E-2,0.315978E-2,
     &  0.304082E-2,0.292633E-2,0.281616E-2,0.271013E-2,0.260809E-2/
       DATA ZLOGM3/
     &  0.250990E-2,0.241541E-2,0.232447E-2,0.223695E-2,0.215273E-2,
     &  0.207168E-2,0.199369E-2,0.191862E-2,0.184639E-2,0.177687E-2,
     &  0.170998E-2,0.164560E-2,0.158364E-2,0.152402E-2,0.146664E-2,
     &  0.141142E-2,0.135828E-2,0.130714E-2,0.125793E-2,0.121057E-2,
     &  0.116499E-2,0.112113E-2,0.107892E-2,0.103830E-2,0.999210E-3,
     &  0.961590E-3,0.925387E-3,0.890547E-3,0.857018E-3,0.824752E-3,
     &  0.793701E-3,0.763818E-3,0.735061E-3,0.707386E-3,0.680754E-3,
     &  0.655124E-3,0.630459E-3,0.606722E-3,0.583880E-3,0.561897E-3,
     &  0.540742E-3,0.520383E-3,0.500791E-3,0.481937E-3,0.463792E-3,
     &  0.446331E-3,0.429527E-3,0.413355E-3,0.397793E-3,0.382816E-3/
       DATA ZLOGH1/
     &             0.640529,0.623728,0.606937,0.590199,
     &  0.573552,0.557032,0.540672,0.524504,0.508553,
     &  0.492843,0.477397,0.462232,0.447365,0.432809,
     &  0.418574,0.404670,0.391103,0.377878,0.364999,
     &  0.352468,0.340284,0.328447,0.316954,0.305804,
     &  0.294992,0.284514,0.274364,0.264538,0.255028,
     &  0.245829,0.236933,0.228335,0.220026,0.211999,
     &  0.204247,0.196762,0.189537,0.182564,0.175837,
     &  0.169347,0.163088,0.157051,0.151231,0.145620,
     &  0.140211,0.134998,0.129974,0.125133,0.120469,
     &  0.115975,0.111645,0.107475,0.103458,0.995895E-1,
     &  0.958635E-1,0.922753E-1,0.888199E-1,0.854925E-1,0.822886E-1,
     &  0.792037E-1,0.762336E-1,0.733739E-1,0.706208E-1,0.679704E-1,
     &  0.654188E-1,0.629625E-1,0.605979E-1,0.583217E-1,0.561306E-1,
     &  0.540215E-1,0.519914E-1,0.500373E-1,0.481564E-1,0.463460E-1/
       DATA ZLOGH2/
     &  0.446034E-1,0.429263E-1,0.413120E-1,0.397583E-1,0.382629E-1,
     &  0.368237E-1,0.354385E-1,0.341053E-1,0.328222E-1,0.315873E-1,
     &  0.303988E-1,0.292550E-1,0.281541E-1,0.270947E-1,0.260750E-1,
     &  0.250937E-1,0.241494E-1,0.232405E-1,0.223658E-1,0.215240E-1,
     &  0.207139E-1,0.199342E-1,0.191839E-1,0.184618E-1,0.177669E-1,
     &  0.170981E-1,0.164545E-1,0.158351E-1,0.152390E-1,0.146653E-1,
     &  0.141133E-1,0.135820E-1,0.130707E-1,0.125786E-1,0.121051E-1,
     &  0.116494E-1,0.112108E-1,0.107888E-1,0.103826E-1,0.999177E-2,
     &  0.961561E-2,0.925360E-2,0.890523E-2,0.856997E-2,0.824733E-2,
     &  0.793684E-2,0.763803E-2,0.735048E-2,0.707375E-2,0.680743E-2,
     &  0.655114E-2,0.630450E-2,0.606715E-2,0.583873E-2,0.561891E-2,
     &  0.540737E-2,0.520379E-2,0.500787E-2,0.481933E-2,0.463789E-2,
     &  0.446328E-2,0.429524E-2,0.413353E-2,0.397790E-2,0.382814E-2,
     &  0.368401E-2,0.354532E-2,0.341184E-2,0.328338E-2,0.315977E-2,
     &  0.304081E-2,0.292632E-2,0.281615E-2,0.271012E-2,0.260809E-2/
       DATA ZLOGH3/
     &  0.250990E-2,0.241540E-2,0.232446E-2,0.223695E-2,0.215273E-2,
     &  0.207168E-2,0.199368E-2,0.191862E-2,0.184639E-2,0.177687E-2,
     &  0.170997E-2,0.164559E-2,0.158364E-2,0.152402E-2,0.146664E-2,
     &  0.141142E-2,0.135828E-2,0.130714E-2,0.125793E-2,0.121057E-2,
     &  0.116499E-2,0.112113E-2,0.107892E-2,0.103830E-2,0.999209E-3,
     &  0.961590E-3,0.925387E-3,0.890546E-3,0.857018E-3,0.824752E-3,
     &  0.793700E-3,0.763818E-3,0.735061E-3,0.707386E-3,0.680754E-3,
     &  0.655124E-3,0.630459E-3,0.606722E-3,0.583880E-3,0.561897E-3,
     &  0.540742E-3,0.520383E-3,0.500791E-3,0.481937E-3,0.463792E-3,
     &  0.446331E-3,0.429527E-3,0.413355E-3,0.397793E-3,0.382816E-3/

       DATA ZLINM1/
     &  0.964508,0.962277,0.960062,0.957863,0.955680,
     &  0.953512,0.951359,0.949222,0.947100,0.944992,
     &  0.942899,0.940821,0.938758,0.936709,0.934673,
     &  0.932652,0.930645,0.928652,0.926672,0.924706,
     &  0.922753,0.920813,0.918886,0.916973,0.915072,
     &  0.913184,0.911308,0.909445,0.907594,0.905756,
     &  0.903930,0.902115,0.900313,0.898522,0.896743,
     &  0.894975,0.893219,0.891475,0.889741,0.888019,
     &  0.886307,0.884607,0.882917,0.881238,0.879569,
     &  0.877911,0.876264,0.874626,0.872999,0.871382,
     &  0.869775,0.868178,0.866591,0.865013,0.863445,
     &  0.861887,0.860338,0.858798,0.857268,0.855747,
     &  0.854235,0.852732,0.851238,0.849753,0.848277,
     &  0.846809,0.845350,0.843900,0.842458,0.841025,
     &  0.839599,0.838182,0.836774,0.835373,0.833980/
       DATA ZLINM2/
     &  0.832596,0.831219,0.829850,0.828489,0.827136,
     &  0.825790,0.824451,0.823121,0.821797,0.820481,
     &  0.819173,0.817871,0.816577,0.815289,0.814009,
     &  0.812736,0.811470,0.810210,0.808958,0.807712,
     &  0.806473,0.805240,0.804015,0.802795,0.801582,
     &  0.800376,0.799176,0.797982,0.796794,0.795613,
     &  0.794438,0.793269,0.792106,0.790949,0.789798,
     &  0.788652,0.787513,0.786380,0.785252,0.784130,
     &  0.783014,0.781903,0.780798,0.779698,0.778604,
     &  0.777516,0.776432,0.775354,0.774282,0.773215,
     &  0.772153,0.771096,0.770044,0.768998,0.767956,
     &  0.766920,0.765888,0.764862,0.763840,0.762824,
     &  0.761812,0.760805,0.759803,0.758805,0.757813,
     &  0.756824,0.755841,0.754862,0.753888,0.752918,
     &  0.751953,0.750992,0.750035,0.749083,0.748136/
       DATA ZLINM3/
     &  0.747192,0.746253,0.745318,0.744388,0.743462,
     &  0.742539,0.741621,0.740707,0.739798,0.738892,
     &  0.737990,0.737092,0.736198,0.735308,0.734423,
     &  0.733540,0.732662,0.731788,0.730917,0.730050,
     &  0.729187,0.728328,0.727472,0.726620,0.725772,
     &  0.724927,0.724086,0.723248,0.722414,0.721584,
     &  0.720757,0.719933,0.719113,0.718296,0.717483,
     &  0.716673/
       DATA ZLINH1/
     &  0.936397,0.932809,0.929287,0.925827,0.922429,
     &  0.919089,0.915806,0.912579,0.909405,0.906284,
     &  0.903212,0.900189,0.897214,0.894284,0.891399,
     &  0.888558,0.885759,0.883001,0.880283,0.877603,
     &  0.874962,0.872357,0.869788,0.867255,0.864755,
     &  0.862288,0.859854,0.857452,0.855081,0.852739,
     &  0.850427,0.848144,0.845889,0.843662,0.841461,
     &  0.839287,0.837138,0.835014,0.832915,0.830841,
     &  0.828789,0.826761,0.824755,0.822772,0.820810,
     &  0.818869,0.816949,0.815050,0.813170,0.811310,
     &  0.809470,0.807648,0.805845,0.804060,0.802293,
     &  0.800543,0.798811,0.797095,0.795396,0.793714,
     &  0.792047,0.790396,0.788761,0.787141,0.785535,
     &  0.783945,0.782369,0.780807,0.779259,0.777724,
     &  0.776204,0.774696,0.773202,0.771720,0.770251/
       DATA ZLINH2/
     &  0.768795,0.767351,0.765919,0.764499,0.763091,
     &  0.761694,0.760309,0.758935,0.757571,0.756219,
     &  0.754878,0.753547,0.752226,0.750916,0.749616,
     &  0.748326,0.747045,0.745775,0.744514,0.743262,
     &  0.742020,0.740787,0.739563,0.738348,0.737141,
     &  0.735944,0.734755,0.733574,0.732402,0.731238,
     &  0.730083,0.728935,0.727795,0.726664,0.725539,
     &  0.724423,0.723314,0.722213,0.721119,0.720032,
     &  0.718952,0.717880,0.716815,0.715756,0.714704,
     &  0.713660,0.712621,0.711590,0.710565,0.709547,
     &  0.708534,0.707529,0.706529,0.705536,0.704549,
     &  0.703567,0.702592,0.701623,0.700660,0.699702,
     &  0.698750,0.697804,0.696863,0.695928,0.694998,
     &  0.694074,0.693155,0.692241,0.691333,0.690430,
     &  0.689532,0.688639,0.687751,0.686868,0.685990/
       DATA ZLINH3/
     &  0.685117,0.684249,0.683386,0.682527,0.681673,
     &  0.680824,0.679979,0.679139,0.678303,0.677472,
     &  0.676645,0.675823,0.675005,0.674191,0.673381,
     &  0.672576,0.671775,0.670978,0.670185,0.669396,
     &  0.668611,0.667830,0.667054,0.666281,0.665512,
     &  0.664746,0.663985,0.663227,0.662473,0.661723,
     &  0.660977,0.660234,0.659495,0.658759,0.658027,
     &  0.657298/

      integer ibit1,ibit2,i

      IBIT1 = 0
      IBIT2 = 0

      DO 9000 I = 1,N
       IF(Z(I).GE.0.15)IBIT1 = IBIT1 + 1
       IF(Z(I).GT.2. )IBIT2 = IBIT2 + 1
 9000 CONTINUE

      IF( IBIT1 .LE. 0 ) GO TO 200

      DO 9002 I = 1,N
       ZSTAR(I)    = 100. * Z(I) - 14.
 9002 CONTINUE

      IF( IBIT2 .LE. 0 ) GO TO 60
      DO 9004 I = 1,N
       TEMP1(I) = Z(I)*0.5
       IF( Z(I) .LE. 2. )TEMP1(I) = 1.
       TEMP1(I) = LOG10(TEMP1(I))
       TEMP1(I) = (TEMP1(I) + 9.3) * 20.
       IF( Z(I) .GT. 2. ) ZSTAR(I) = TEMP1(I)
       IF( Z(I).GT.1.78e10 ) ZSTAR(I) = 384.9999
 9004  CONTINUE

 60    CONTINUE

      DO 9006 I = 1,N
       I1(I) = ZSTAR(I)
       I2(I) = I1(I) + 1
       TEMP1(I) = ZSTAR(I) - I1(I)

 9006  CONTINUE

      IF( IFLAG .GT. 2 ) GO TO 100
       DO 9008 I = 1,N
       if( z(i).ge.0.15 ) then
       E1(I) = PHIM0( I1(I) )
       E2(I) = PHIM0( I2(I) )
       PHIM(I)  = TEMP1(I) * ( E2(I)-E1(I) )
       PHIM(I)  = PHIM(I) +   E1(I)
       endif
 9008  CONTINUE

  100 CONTINUE

      IF( IFLAG .EQ. 2 ) GO TO 200
       DO 9010 I = 1,N
       if( z(i).ge.0.15 ) then
       E1(I) = PHIH0( I1(I) )
       E2(I) = PHIH0( I2(I) )
       PHIH(I)  = TEMP1(I) * ( E2(I)-E1(I) )
       PHIH(I)  = PHIH(I) +   E1(I)
       endif
 9010  CONTINUE

  200 CONTINUE
      IF( IBIT1 .GE. N ) GO TO 500

       DO 9012 I = 1,N
       ZSTAR(I) = -Z(I)
 9012  CONTINUE

      IF( IFLAG .GT. 2 ) GO TO 300
       DO 9014 I = 1,N
       IF( Z(I) .LT. 0.15 ) PHIM(I) = 1. + ZSTAR(I)
     2     *(0.25+ZSTAR(I)*(0.09375+ZSTAR(I)*
     3     (0.03125+0.00732422 * ZSTAR(I))))
 9014  CONTINUE

  300 CONTINUE
      IF( IFLAG .EQ. 2 ) GO TO 500
       DO 9016 I = 1,N
       IF( Z(I) .LT. 0.15 ) THEN
       PHIH(I) =1.+ Z(I) * (0.5+ZSTAR(I)*(0.375+ZSTAR(I)*
     1     (0.5+ZSTAR(I)*(0.8203125+ZSTAR(I)*
     2     (1.5+2.93262*ZSTAR(I))))))
       PHIH(I) = 1. / PHIH(I)
      ENDIF
 9016  CONTINUE

  500 CONTINUE
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE PSI(VZZ,VZH,VPSIM,VPSIH,IRUN,VX,VXS,VY,VYS,IFLAG)
C**********************************************************************
C  SUBROUTINE PSI - DETERMINES DIMENSIONLESS WIND AND
C                    SCALAR PROFILES IN SURFACE LAYER
C                 - CALLED FROM SFCFLX
C  DESCRIPTION OF PARAMETERS
C     ZZ   -  INPUTED VALUE OF MONIN- OBUKHOV STABILITY PARAMETER ZETA
C     ZH   -  INPUTED VALUE OF PBL HEIGHT DIVIDED BY Z0
C     PSIM -  OUTPUTED VALUE OF DIMENSIONLESS WIND
C     PSIH -  OUTPUTED VALUE OF DIMENSIONLESS SCALAR
C     X    -  OUTPUTED VALUE OF PHIM(ZETA)
C     XS   -  OUTPUTED VALUE OF PHIM(ZETA0)
C     Y    -  OUTPUTED VALUE OF PHIH(ZETA)
C     YS   -  OUTPUTED VALUE OF PHIH(ZETA0)
C     IFLAG-  FLAG TO DETERMINE IF CU IS NEEDED (IFLAG=2),
C                  IF CT IS NEEDED (IFLAG=3), OR BOTH (IFLAG=1)
C  SUBPROGRAMS NEEDED
C     PHI  -  COMPUTES SIMILARITY FUNCTION FOR MOMENTUM AND SCALARS
C**********************************************************************
      implicit none

C Argument List Declarations
      integer irun,iflag
      _RL VZZ(IRUN),VZH(IRUN),VPSIM(IRUN),VPSIH(IRUN),
     1     VX(IRUN),VXS(IRUN),VY(IRUN),VYS(IRUN)

C Local Variables
      _RL ZWM,RZWM,Z0M,ZCM,RZCM,CM1,CM2,CM6,CM7,CM8ARG,YCM
      PARAMETER ( ZWM     =    1.    )
      PARAMETER ( RZWM    =  1./ZWM  )
      PARAMETER ( Z0M     =    0.2    )
      PARAMETER ( ZCM     =    42.    )
      PARAMETER ( RZCM    =  1./ZCM  )
      PARAMETER ( CM1     =  1./126. )
      PARAMETER ( CM2     =  1./(6.*CM1)  )
      PARAMETER ( CM6     =  6. / ( 1. + 6.*CM1 )  )
      PARAMETER ( CM7     =  CM2 + ZWM  )
      PARAMETER ( CM8ARG  =  CM7*ZCM*RZWM / (CM2+ZCM)  )
      PARAMETER ( YCM     =  6. / ( 1. + 6.*CM1*ZCM )  )

      integer INTSTB(irun),INTZ0(irun)
      _RL ZZ0(irun),Z(irun),Z2(irun),Z1(irun),Z0(irun)
      _RL X0(irun),X1(irun),Y0(irun),Y1(irun)
      _RL PSI2(irun),TEMP(irun)
      _RL HZ(irun),ARG0(irun),ARG1(irun),DX(irun)
      _RL X0NUM(irun),X1NUM(irun),X0DEN(irun)
      _RL X1DEN(irun),Y1DEN(irun),Z2ZWM(irun)
      _RL cm3,cm4,cm5,cm8
      integer ibit,indx
      integer i

      CM3 =   sqrt( 0.2/CM1-0.01 )
      CM4 =   1./CM3
      CM5 =  (10.-CM1) / (10.*CM1*CM3)
      CM8 =   6. * LOG(CM8ARG)

      DO 9000 I = 1,IRUN
       VPSIM(I) = 0.
       VPSIH(I) = 0.
       VX(I) = 0.
       VXS(I) = 0.
       VY(I) = 0.
       VYS(I) = 0.
       ZZ0(I) = VZH(I)*VZZ(I)
 9000 CONTINUE
      IBIT = 0
      DO 9122 I = 1,IRUN
       IF(VZZ(I).LE.-1.e-7)IBIT = IBIT + 1
 9122 CONTINUE
      DO 9022 I = 1,IRUN
       IF(VZZ(I).LE.-1.e-7)THEN
        INTSTB(I) = 1
       ELSE
        INTSTB(I) = 0
       ENDIF
 9022 CONTINUE

C ****************************************
C *****    UNSTABLE SURFACE LAYER    *****
C ****************************************

      IF(IBIT.LE.0)  GO TO 100

      indx = 0
      DO 9002 I = 1,IRUN
       IF (INTSTB(I).EQ.1)THEN
        indx = indx + 1
        Z(indx) = VZZ(I)
        Z0(indx) = ZZ0(I)
       ENDIF
 9002 CONTINUE

      DO 9004 I = 1,IBIT
       Z(I) = -18. * Z(I)
       Z0(I) = -18. * Z0(I)
 9004 CONTINUE

      CALL PHI( Z,X1,Y1,IFLAG,IBIT )
      CALL PHI( Z0,X0,Y0,IFLAG,IBIT )

C ****************************
C *****    COMPUTE PSIM  *****
C ****************************

      IF(IFLAG.GE.3) GO TO 75

      DO 9006 I = 1,IBIT
       ARG1(I) = 1. - X1(I)
       IF ( Z(I) .LT. 0.013 ) ARG1(I) =
     1        Z(I) * ( 0.25 -  0.09375 * Z(I) )

       ARG0(I)  = 1. - X0(I)
       IF ( Z0(I) .LT. 0.013 ) ARG0(I) =
     1       Z0(I) * ( 0.25 -  0.09375 * Z0(I) )

       ARG1(I) = ARG1(I) * ( 1.+X0(I) )
       ARG0(I) = ARG0(I) * ( 1.+X1(I) )
       DX(I) = X1(I) - X0(I)
       ARG1(I) = ARG1(I) / ARG0(I)
       ARG0(I) = -DX(I) / ( 1. + X1(I)*X0(I) )
       ARG0(I) = ATAN( ARG0(I) )
       ARG1(I) = LOG( ARG1(I) )
       PSI2(I) = 2. * ARG0(I) + ARG1(I)
       PSI2(I) = PSI2(I) + DX(I)
 9006 CONTINUE

      indx = 0
      DO 9008 I = 1,IRUN
       IF( INTSTB(I).EQ.1 ) THEN
        indx = indx + 1
        VPSIM(I) = PSI2(indx)
        VX(I) = X1(indx)
        VXS(I) = X0(indx)
       ENDIF
 9008 CONTINUE

C ****************************
C *****    COMPUTE PSIH  *****
C ****************************

      IF(IFLAG.EQ.2) GO TO 100

  75  CONTINUE
      DO 9010 I = 1,IBIT
       ARG1(I) = 1. - Y1(I)
       IF( Z(I) .LT. 0.0065 ) ARG1(I) =
     1       Z(I) * ( 0.5 -  0.625 * Z(I) )

       ARG0(I)  = 1. - Y0(I)
       IF( Z0(I) .LT. 0.0065 ) ARG0(I) =
     1       Z0(I) * ( 0.5 -  0.625 * Z0(I) )

       ARG1(I) = ARG1(I) * ( 1. + Y0(I) )
       ARG0(I) = ARG0(I) * ( 1. + Y1(I) )
       ARG1(I) = ARG1(I) / ARG0(I)
       PSI2(I) = LOG( ARG1(I) )
       PSI2(I) = PSI2(I) - Y1(I) + Y0(I)
 9010 CONTINUE

      indx = 0
      DO 9012 I = 1,IRUN
       IF( INTSTB(I).EQ.1 ) THEN
       indx = indx + 1
       VPSIH(I) = PSI2(indx)
       VY(I) = Y1(indx)
       VYS(I) = Y0(indx)
       ENDIF
 9012 CONTINUE

C **************************************
C *****    STABLE SURFACE LAYER    *****
C **************************************

  100 CONTINUE
      IBIT = 0
      DO 9114 I = 1,IRUN
       IF(VZZ(I).GT.-1.e-7)THEN
        IBIT = IBIT + 1
       ENDIF
 9114 CONTINUE
      DO 9014 I = 1,IRUN
       IF(VZZ(I).GT.-1.e-7)THEN
        INTSTB(I) = 1
       ELSE
        INTSTB(I) = 0
       ENDIF
 9014 CONTINUE
      IF(IBIT.LE.0)  GO TO 300
      indx = 0
#ifdef CRAY
CDIR$ NOVECTOR
#endif
      DO 9016 I = 1,IRUN
       IF (INTSTB(I).EQ.1)THEN
        indx = indx + 1
        Z(indx) = VZZ(I)
        Z0(indx) = ZZ0(I)
        ARG1(indx) = VZH(I)
       ENDIF
 9016 CONTINUE
#ifdef CRAY
CDIR$ VECTOR
#endif

      DO 9018 I = 1,IBIT
       HZ(I) = 1. / ARG1(I)
       Z1(I) = Z(I)
       Z2(I) = ZWM

       IF ( Z(I) .GT. ZWM ) THEN
        Z1(I) = ZWM
        Z2(I) = Z(I)
       ENDIF

       IF ( Z0(I) .GT. Z0M ) THEN
        Z0(I) = Z0M
        INTZ0(I) = 1
       ELSE
        INTZ0(I) = 0
       ENDIF

       X1NUM(I) = 1. + 5. * Z1(I)
       X0NUM(I) = 1. + 5. * Z0(I)
       X1DEN(I) = 1. / (1. + CM1 * (X1NUM(I) * Z1(I)) )
       X0DEN(I) = 1. + CM1 * (X0NUM(I) * Z0(I))

       IF ( (INTZ0(I).EQ.1) .OR. (Z(I).GT.ZWM) )
     1      HZ(I) = Z1(I) / Z0(I)
       ARG1(I) = HZ(I)*HZ(I)*X0DEN(I)*X1DEN(I)
       ARG1(I) = LOG( ARG1(I) )
       ARG1(I) = 0.5 * ARG1(I)
       ARG0(I) = (Z1(I) + 0.1) * (Z0(I) + 0.1)
       ARG0(I) = CM3 + ARG0(I) * CM4
       ARG0(I) = ( Z1(I) - Z0(I) ) / ARG0(I)
       ARG0(I) = ATAN( ARG0(I) )
       TEMP(I) = ARG1(I) + CM5 * ARG0(I)

       X0(I) = X0NUM(I) / X0DEN(I)
       IF ( INTZ0(I).EQ.1 ) X0(I) = 0.
       Z2ZWM(I) = Z2(I) * RZWM
 9018 CONTINUE

C ****************************
C *****    COMPUTE PSIM  *****
C ****************************

      IF( IFLAG.GE.3 ) GO TO 225

      DO 9020 I = 1,IBIT
       X1(I) = X1NUM(I) * X1DEN(I)
       ARG1(I) = LOG( Z2ZWM(I) )
       PSI2(I) = TEMP(I) + CM6 * ARG1(I)
 9020 CONTINUE

      indx = 0
      DO 9030 I = 1,IRUN
       IF( INTSTB(I).EQ.1 ) THEN
       indx = indx + 1
       VPSIM(I) = PSI2(indx)
       VX(I) = X1(indx)
       VXS(I) = X0(indx)
       ENDIF
 9030 CONTINUE

C ****************************
C *****    COMPUTE PSIH  *****
C ****************************

       IF(IFLAG.EQ.2)GO TO 300

  225 CONTINUE
      DO 9024 I = 1,IBIT
       Y1DEN(I) = 1. + CM1 * ( X1NUM(I) * Z(I) )
       Y1(I) = X1NUM(I) / Y1DEN(I)
       ARG1(I) = CM7 * Z2ZWM(I) / ( CM2 + Z2(I) )
       ARG0(I) = 6.
       IF ( Z2(I) .GT. ZCM ) THEN
        Y1(I) = YCM
        ARG1(I) = Z2(I) * RZCM
        ARG0(I) = YCM
        TEMP(I) = TEMP(I) + CM8
       ENDIF
       ARG1(I) = LOG( ARG1(I) )
       PSI2(I) = TEMP(I) + ARG0(I) * ARG1(I)
 9024 CONTINUE

      indx = 0
      DO 9026 I = 1,IRUN
       IF( INTSTB(I).EQ.1 ) THEN
       indx = indx + 1
       VPSIH(I) = PSI2(indx)
       VY(I) = Y1(indx)
       VYS(I) = X0(indx)
       ENDIF
 9026 CONTINUE

  300 CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE TRBLEN (STRT,DW2,DZ3,Q,VKZE,VKZM,DTHV,DPK,DU,DV,XL,
     1                   QXLM,NLEV,INIT,LMIN,LMINQ,LMINQ1,CP,INT1,INT2,
     2  DZITRP,STBFCN,XL0,Q1,WRKIT1,WRKIT2,WRKIT3,WRKIT4,irun)
C**********************************************************************
C  SUBROUTINE TRBLEN - COMPUTES TURBULENT LENGTH SCALE
C                    - CALLED FROM TRBFLX
C   ARGUMENTS ::
C     INPUT:
C     ------
C    STRT          -         BRUNT VAISALA FREQUENCY
C    DW2           -         SQUARED SHEAR
C    DZ3           -         LAYER THICKNESS FOR LENGTH SCALE CALC.
C    Q             -         TURBULENCE VELOCITY
C    VKZE          -         VK * Z AT LAYER EDGES
C    VKZM          -         VK * Z AT LAYER CENTERS
C    DTHV          -         VERTICAL GRADIENT OF THV
C    DPK           -         VERTICAL GRADIENT OF PK
C    DU            -         VERTICAL GRADIENT OF U
C    DV            -         VERTICAL GRADIENT OF V
C    NLEV          -         NUMBER OF ATMOSPHERIC LEVELS
C    INIT          -         INPUT FLAG : 1 = INITIAL START
C                                         2 = 2ND CALL FOR INITIAL STAR
C                                         0 = NON-INITIAL START
C     OUTPUT:
C     -------
C    XL            -         TURBULENT LENGTH SCALE
C    QXLM          -         TURBULENT LENGTH SCALE * Q AT LAYER CENTER
C    LMIN          -         HIGHEST LAYER WHERE INSTABILITY OCCURS
C    LMINQ         -         HIGHEST LAYER WHERE TURBULENCE OCCURS
C
C   SUBPROGRAMS NEEDED ::
C     TRBITP -  INTERPOLATES TO HEIGHT WHERE RI = RICR
C**********************************************************************
      implicit none

C Argument List Declarations
      integer irun,nlev,init,lmin,lminq,lminq1
      _RL cp
      _RL STRT(irun,NLEV),DW2(irun,NLEV),DZ3(irun,NLEV)
      _RL Q(irun,NLEV),VKZM(irun,NLEV-1),VKZE(irun,NLEV-1)
      _RL DTHV(irun,NLEV),DPK(irun,NLEV),DU(irun,NLEV)
      _RL DV(irun,NLEV)
      _RL QXLM(irun,NLEV-1),XL(irun,NLEV-1)
      _RL DZITRP(irun,nlev-1),STBFCN(irun,nlev)
      _RL XL0(irun,nlev),Q1(irun,nlev-1)
      _RL WRKIT1(irun,nlev-1),WRKIT2(irun,nlev-1)
      _RL WRKIT3(irun,nlev-1)
      _RL WRKIT4(irun,nlev-1)
      INTEGER INT1(irun,nlev), INT2(irun,nlev-1)

C Local Variables
      _RL rf1,rf2,e5,d4,d1,rfc,ricr,alpha,dzcnv,xl0cnv,xl0min
      _RL clmt,clmt53
      PARAMETER ( RF1     = 0.2340678 )
      PARAMETER ( RF2     = 0.2231172 )
      PARAMETER ( E5      = 49.66     )
      PARAMETER ( D4      = 2.6532122E-2 )
      PARAMETER ( D1      = D4 * E5   )
      PARAMETER ( RFC     = 0.1912323 )
      PARAMETER ( RICR    = ( (RF1-RFC)*RFC ) / ( (RF2-RFC)*D1 )  )
      PARAMETER ( ALPHA   = 0.1       )
      PARAMETER ( DZCNV   = 100.      )
      PARAMETER ( XL0CNV  = DZCNV * ALPHA )
      PARAMETER ( XL0MIN  = 1.        )
      PARAMETER ( CLMT    = 0.23      )
      PARAMETER ( CLMT53  = 5. * CLMT / 3. )

      integer ibit,nlevm1,nlevp1,istnlv,istnm1,nlevml,istnml,Lp1
      integer istnmq,istlmq,lminp,lm1,lmin1
      integer i,L,LL

      NLEVM1 = NLEV - 1
      NLEVP1 = NLEV + 1
      ISTNLV = irun * NLEV
      ISTNM1 = irun * NLEVM1

      IF ( INIT.EQ.2 ) GO TO 1200

C COMPUTE DEPTHS OF UNSTABLE LAYERS
C =================================
      DO L =1,NLEV
       DO I =1,irun
         STBFCN(I,L) = STRT(I,L) - RICR * DW2(I,L)
         INT1(I,L) = 0
         IF( STBFCN(I,L).LE.0. ) INT1(I,L) = 1
       ENDDO
      ENDDO

      DO L =1,NLEVM1
       DO I =1,irun
        INT2(I,L) = 0
        IF( (INT1(I,L).EQ.1) .NEQV. (INT1(I,L+1).EQ.1) ) INT2(I,L) = 1
       ENDDO
      ENDDO

      DO 40 LMIN = 1,NLEV
      IBIT = 0
      DO 30 I=1,irun
      IBIT = IBIT + INT1(I,LMIN)
   30 CONTINUE
      IF(IBIT.GE.1) GO TO 50
   40 CONTINUE
      LMIN = NLEVP1
   50 CONTINUE
      LMIN = 1

      DO L =1,NLEVM1
       DO I =1,irun
        XL0(I,L) = 0.
       ENDDO
      ENDDO
      DO 70 I=1,irun
      XL0(I,NLEV) = DZ3(I,NLEV)
   70 CONTINUE

      IF(LMIN.GE.NLEVP1) GOTO 1100
                         LMIN1 = LMIN - 1
      IF(LMIN1.EQ.0)     LMIN1 = 1
         NLEVML = NLEV - LMIN1
         ISTNML = irun*NLEVML
      CALL TRBITP ( STBFCN(1,LMIN1),INT2(1,LMIN1),DTHV(1,LMIN1),
     &                 DPK(1,LMIN1),  DU(1,LMIN1),  DV(1,LMIN1),
     &              DZITRP(1,LMIN1),  NLEVML,
     &              WRKIT1,WRKIT2,WRKIT3,WRKIT4,CP,irun )
      LP1 = LMIN1 + 1

      DO L =LMIN1,NLEVM1
       DO I =1,irun
        INT2(I,L) = 0
        IF ( INT1(I,L).EQ.1 .OR. INT1(I,L+1).EQ.1 ) INT2(I,L) = 1
        IF ( INT2(I,L).EQ.1 )
     &       XL0(I,L) = (0.5+DZITRP(I,L)) * DZ3(I,L+1)
       ENDDO
      ENDDO
      DO 90 I=1,irun
      INT2(I,NLEVM1) = INT1(I,NLEV)
   90 CONTINUE

      DO L =LMIN1,NLEVM1
       DO I =1,irun
        IF ( INT2(I,L).EQ.1 ) THEN
         XL0(I,L+1) = XL0(I,L+1) + ( (0.5-DZITRP(I,L)) * DZ3(I,L+1) )
        ENDIF
       ENDDO
      ENDDO
      IF (LMIN.GT.1) GOTO 400
      DO 110 I=1,irun
      IF( INT1(I,1).EQ.1 ) XL0(I,1) = XL0(I,1) + DZ3(I,1)
  110 CONTINUE
  400 CONTINUE

      LMINP = LMIN + 1
      IF(LMINP.GT.NLEVM1) GOTO 550
      DO 500 L = LMINP,NLEVM1
           LM1 = L-1
      DO 120 I = 1,irun
      IF( INT1(I,LM1).EQ.1 ) XL0(I,L) = XL0(I,L) + XL0(I,LM1)
  120 CONTINUE
  500 CONTINUE
  550 CONTINUE
      IF(LMIN.GT.NLEVM1) GOTO 600
      DO 130 I = 1,irun
      IF( INT1(I,NLEVM1).EQ.1 .AND. INT1(I,NLEV).EQ.1 ) THEN
      XL0(I,NLEV) = XL0(I,NLEV) + XL0(I,NLEVM1)
      ENDIF
  130 CONTINUE
      IF(LMIN.GT.NLEV) GOTO 1100
  600 CONTINUE
      DO 1000 LL  = LMIN,NLEV-1
              L   = NLEVM1 + LMIN - LL
              LP1 = L+1
      DO 140 I = 1,irun
      IF( INT1(I,LP1).EQ.1 ) THEN
      IF( INT1(I,L)  .EQ.1 ) THEN
      XL0(I,L) = XL0(I,LP1)
      ELSE
      XL0(I,L) = XL0(I,L) + XL0(I,LP1)
      ENDIF
      ENDIF
  140 CONTINUE
 1000 CONTINUE
 1100 CONTINUE

      DO L =1,NLEV
       DO I =1,irun
        IF ( XL0(I,L).LT.XL0CNV ) XL0(I,L) = XL0CNV
       ENDDO
      ENDDO

C *********************************************************************
C ****          DETERMINE MIXING LENGTHS FOR STABLE LAYERS          ***
C *********************************************************************

      IF(INIT.EQ.1) GOTO 1400

      IF(LMINQ.GT.1) THEN
                    ISTLMQ = irun * LMINQ1
      DO L =1,LMINQ1
       DO I =1,irun
        INT2(I,L) = 1 - INT1(I,L)
       ENDDO
      ENDDO
      ENDIF
      IF(LMINQ.LT.NLEV) THEN
        ISTNMQ = irun * (NLEV-LMINQ)
       DO L =LMINQ,NLEVM1
        DO I =1,irun
         IF( INT1(I,L).EQ.0 ) THEN
           XL0(I,L) =         Q(I,L) / XL0(I,L)
           XL0(I,L) =       XL0(I,L) * XL0(I,L) + 1.0E-20
           XL0(I,L) =    STBFCN(I,L) + XL0(I,L)
           XL0(I,L) = SQRT( XL0(I,L) )
           XL0(I,L) =         Q(I,L) / XL0(I,L)
         ENDIF
         INT2(I,L)  = 0
         IF( XL0(I,L).LT.XL0MIN ) INT2(I,L)  = 1
        ENDDO
       ENDDO
      ENDIF

 1200 CONTINUE

      IF(INIT.EQ.2) THEN
       DO L =1,NLEVM1
        DO I =1,irun
         INT2(I,L) = 1 - INT1(I,L)
         IF ( INT2(I,L).EQ.1 ) XL0(I,L) = XL0MIN
        ENDDO
       ENDDO
      ENDIF
      DO L =1,NLEVM1
       DO I =1,irun
        IF ( INT2(I,L).EQ.1 ) XL0(I,L) = XL0MIN
       ENDDO
      ENDDO

C *********************************************************************
C ****             LENGTH SCALE XL FROM XL0 AND VKZE              ****
C *********************************************************************

 1400 CONTINUE

      DO L =1,NLEVM1
       DO I =1,irun
        XL(I,L) = XL0(I,L) * VKZE(I,L) / ( XL0(I,L)+VKZE(I,L) )
       ENDDO
      ENDDO

C *********************************************************************
C ****       CLMT53 TIMES Q TIMES LENGTH SCALE AT MID LEVELS        ***
C *********************************************************************

      IF(INIT.EQ.1) GOTO 1700
                   ISTNMQ = irun * (NLEV-LMINQ1)
      DO L =LMINQ1,NLEVM1
       DO I =1,irun
        Q1(I,L) = Q(I,L)
        INT1(I,L) = 0
        IF (    Q(I,L).LE.Q(I,L+1) ) INT1(I,L) = 1
        IF ( INT1(I,L).EQ.1 )  THEN
           XL0(I,L) = XL0(I,L+1)
            Q1(I,L) =   Q(I,L+1)
        ENDIF
       ENDDO
      ENDDO

      DO L =LMINQ1,NLEVM1
       DO I =1,irun
        QXLM(I,L) =   XL0(I,L)*VKZM(I,L)
     &                 / ( XL0(I,L)+VKZM(I,L) )
        QXLM(I,L) = CLMT53 * Q1(I,L)*QXLM(I,L)
       ENDDO
      ENDDO

 1700 CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE TRBITP ( STBFCN,INTCHG,DTHV,DPK,DU,DV,DZITRP,NLEV,
     &                    AAA,BBB,CCC,DDD,CP,irun )
C**********************************************************************
C  SUBROUTINE TRBITP - INTERPOLATES TO THE HEIGHT WHERE RI EQUALS RICR
C                    - CALLED FROM TRBLEN
C   ARGUMENTS ::
C     INPUT:
C     ------
C    STBFCN        -         DTHV * DPK - RICR*( DU*DU + DV*DV)
C    INTCHG        -         INT '1' AT LEVELS WHERE STBFCN CHANGES SIG
C    DTHV          -         VERTICAL GRADIENT OF THV
C    DPK           -         VERTICAL GRADIENT OF PK
C    DU            -         VERTICAL GRADIENT OF U
C    DV            -         VERTICAL GRADIENT OF V
C    NLEV          -         NUMBER OF LEVELS TO BE PROCESSED
C     OUTPUT:
C     -------
C    DZITRP        -         INTERPOLATION COEFFICIENT
C**********************************************************************
      implicit none

C Argument List Declarations
      integer irun,nlev
      _RL cp
      _RL STBFCN(irun,NLEV+1)
      integer INTCHG(irun,NLEV)
      _RL DTHV(irun,NLEV+1),DPK(irun,NLEV+1)
      _RL DU(irun,NLEV+1),DV(irun,NLEV+1)
      _RL DZITRP(irun,NLEV+1)
      _RL AAA(irun,NLEV),BBB(irun,NLEV)
      _RL CCC(irun,NLEV),DDD(irun,NLEV)

C Local Variables
      _RL rf1,rf2,e5,d4,d1,rfc,ricr
      PARAMETER ( RF1     = 0.2340678 )
      PARAMETER ( RF2     = 0.2231172 )
      PARAMETER ( E5      = 49.66     )
      PARAMETER ( D4      = 2.6532122E-2 )
      PARAMETER ( D1      = D4 * E5   )
      PARAMETER ( RFC     = 0.1912323 )
      PARAMETER ( RICR    = ( (RF1-RFC)*RFC ) / ( (RF2-RFC)*D1 )  )

      integer istnlv
      integer I,L

C *********************************************************************
C ****           QUADRATIC INTERPOLATION OF RI TO RICR VIA          ***
C ****           LINEAR INTERPOLATION OF DTHV, DPK, DU & DV         ***
C *********************************************************************

      ISTNLV = irun*NLEV
      DO L =1,NLEV
       DO I =1,irun
        DZITRP(I,L) = 0.
       ENDDO
      ENDDO
      DO L =1,NLEV
       DO I =1,irun
        IF ( INTCHG(I,L).EQ.1 ) THEN
         DDD(I,L) = (         CP  *(DTHV(I,L+1)*DPK(I,L)
     &                            + DTHV(I,L)*DPK(I,L+1)) )
     &            - ( (2.*RICR)  * ( DU(I,L+1)* DU(I,L)
     &                            +   DV(I,L+1)* DV(I,L)) )
         AAA(I,L) = STBFCN(I,L) + STBFCN(I,L+1)
         BBB(I,L) = STBFCN(I,L) - STBFCN(I,L+1)
         CCC(I,L) =            1. / BBB(I,L)
         DZITRP(I,L) = AAA(I,L) * CCC(I,L)
         AAA(I,L) = AAA(I,L) - DDD(I,L)
         DDD(I,L) =        (   DDD(I,L) *    DDD(I,L) )
     &            - 4. * (STBFCN(I,L+1) * STBFCN(I,L) )
         DDD(I,L) = DDD(I,L)*CCC(I,L)*CCC(I,L)
         DDD(I,L) =    SQRT( DDD(I,L) )
        ENDIF

        IF( INTCHG(I,L).EQ.1 .AND. AAA(I,L).NE.0. ) THEN
         DZITRP(I,L) = ( BBB(I,L)*(1.-DDD(I,L)) ) / AAA(I,L)
        ENDIF

        DZITRP(I,L) = 0.5 * DZITRP(I,L)
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE TRBL20 (RI,STRT,DW2,XL,ZKM,ZKH,QE,QQE,INTSTB,NLEV,
     1                        nlay,irun)
C**********************************************************************
C  SUBROUTINE TRBL20 - COMPUTES QE AND DIMLESS COEFS FROM
C                       MELLOR-YAMADA LEVEL 2 MODEL
C                    - CALLED FROM AND FROM TRBFLX
C   ARGUMENTS ::
C     INPUT:
C     ------
C    RI            -         RICHARDSON NUMBER
C    STRT          -         BRUNT VAISALA FREQUENCY
C    DW2           -         SQUARED SHEAR
C    XL            -         TURBULENT LENGTH SCALE
C    NLEV          -         NUMBER OF LEVELS TO BE PROCESSED
C     OUTPUT:
C     -------
C    ZKM           -         MOMENTUM TRANSPORT COEFFICIENT
C    ZKH           -         HEAT TRANSPORT COEFFICIENT
C    QE            -         EQUILIBRIUM TURBULENT VELOCITY SCALE
C    QQE           -         EQUILIBRIUM TURBULENT KINETIC ENERGY
C    BITSTB        -         BIT '1' WHERE QE GREATER THAN ZERO
C**********************************************************************
      implicit none

C Argument List Declarations
      integer nlev,nlay,irun
      _RL RI(irun*NLEV,1),STRT(irun*NLEV,1),DW2(irun*NLEV,1)
      _RL XL(irun*NLEV,1),ZKM(irun*NLEV,1), ZKH(irun*NLEV,1)
      _RL QE(irun*NLEV,1),QQE(irun*NLEV,1)
      INTEGER INTSTB(irun*NLEV,1)
      _RL EE(irun*(nlay-1),1),RF(irun*(nlay-1),1)

C Local Variables
      _RL b1,b2,d3,rf1,rf2,d3b2,d2,e5,d4,d1,d1half,d2half
      _RL rfc,ricr,ch,cm
      PARAMETER ( B1      =   16.6    )
      PARAMETER ( B2      =   10.1    )
      PARAMETER ( D3      = 0.29397643 )
      PARAMETER ( RF1     = 0.2340678 )
      PARAMETER ( RF2     = 0.2231172 )
      PARAMETER ( D3B2    = D3 / RF1  )
      PARAMETER ( D2      = RF1       )
      PARAMETER ( E5      = 49.66     )
      PARAMETER ( D4      = 2.6532122E-2 )
      PARAMETER ( D1      = D4 * E5   )
      PARAMETER ( D1HALF = 0.5 * D1 )
      PARAMETER ( D2HALF = 0.5 * D2 )
      PARAMETER ( RFC     = 0.1912323 )
      PARAMETER ( RICR    = ( (RF1-RFC)*RFC ) / ( (RF2-RFC)*D1 )  )
      PARAMETER ( CH      = 2.5828674 )
      PARAMETER ( CM      = CH / D1   )

      integer istnlv
      integer i

      ISTNLV = irun * NLEV

C *********************************************************************
C ****               COMPUTE FLUX RICHARDSON NUMBER                 ***
C *********************************************************************

      DO 10 I=1,ISTNLV
      EE(I,1) = D1HALF * RI(I,1) + D2HALF
      RF(I,1) = EE(I,1)* EE(I,1)
      RF(I,1) = RF(I,1)- D3*RI(I,1)
      RF(I,1) = SQRT( RF(I,1) )
      RF(I,1) = EE(I,1) - RF(I,1)

      IF( RI(I,1).LE.1.e-4 .AND. RI(I,1).GE.-1.e-4 ) THEN
          RF(I,1) = D3B2*RI(I,1)
      ENDIF

C *********************************************************************
C ****           QE AND DIMENSIONLESS DIFFUSION COEFICIENTS         ***
C ****                   FROM LEVEL 2 CLOSURE MODEL                 ***
C *********************************************************************

      IF( RI(I,1).LT.RICR .AND. RF(I,1).LT.RFC ) THEN
      ZKH(I,1) =   ( RFC-RF(I,1) ) / (1.-RF(I,1))
      ZKM(I,1) =                CM * (RF1-RF(I,1))
      ZKM(I,1) = ZKH(I,1)*ZKM(I,1) / (RF2-RF(I,1))
      ZKH(I,1) = CH      *ZKH(I,1)
       QE(I,1) = ZKM(I,1)*DW2(I,1) - ZKH(I,1)*STRT(I,1)
      ENDIF

      IF( QE(I,1).LT.1.e-14 ) THEN
      INTSTB(I,1) = 0
          QE(I,1) = 0.
      ELSE
      INTSTB(I,1) = 1
          QE(I,1) = B1*QE(I,1)
          QE(I,1) = SQRT( QE(I,1) )
          QE(I,1) = XL(I,1)*QE(I,1)
      ENDIF
      QQE(I,1) = 0.5 * QE(I,1) * QE(I,1)
   10 CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE TRBL25(Q,XL,STRT,DW2,INTSTB,INTQ,ZKM,ZKH,P3,NLEV,
     1                      nlay,irun)
C**********************************************************************
C  SUBROUTINE TRBL25 - COMPUTES P3 AND DIMLESS COEFS FROM
C                       MELLOR-YAMADA LEVEL 2.5 MODEL
C                    - CALLED FROM TRBFLX
C   ARGUMENTS ::
C     INPUT:
C     ------
C    Q             -         TURBULENCE VELOCITY
C    XL            -         TURBULENT LENGTH SCALE
C    STRT          -         BRUNT VAISALA FREQUENCY
C    DW2           -         SQUARED SHEAR
C    BITSTB        -         BIT '1' WHERE QE GREATER THAN ZERO
C    NLEV          -         NUMBER OF LEVELS TO BE PROCESSED
C     OUTPUT:
C     -------
C    ZKM           -         MOMENTUM TRANSPORT COEFFICIENT
C    ZKH           -         HEAT TRANSPORT COEFFICIENT
C    P3            -         PRODUCTION RATE OF TURBULENT KINETIC ENERG
C**********************************************************************
      implicit none

C Argument list Declarations
      integer nlev,nlay,irun
      _RL Q(irun*NLEV,1),XL(irun*NLEV,1),STRT(irun*NLEV,1)
      _RL DW2(irun*NLEV,1)
      INTEGER INTSTB(irun*nlay,1), INTQ(irun*nlay,1)
      _RL ZKM(irun*NLEV,1),ZKH(irun*NLEV,1),P3(irun*NLEV,1)

C Local Variables
      _RL a1,a2,a4,c1,a5,a3,b1,b2,b3,ff2,ff3,ff4
      PARAMETER ( A1      =   0.92    )
      PARAMETER ( A2      =   0.74    )
      PARAMETER ( A4      = 6. * A1 * A1)
      PARAMETER ( C1      =   0.08    )
      PARAMETER ( A5      = 3.*C1*(-1.) )
      PARAMETER ( A3      = A4 * A5*(-1.) )
      PARAMETER ( B1      =   16.6    )
      PARAMETER ( B2      =   10.1    )
      PARAMETER ( B3      = 1. / B1  )
      PARAMETER ( FF2     = 9. * A1 * A2 )
      PARAMETER ( FF3     = (3.*A2*B2) - (9.*A2*A2 )  )
      PARAMETER ( FF4     = (3.*A2*B2) + (12.*A1*A2 )  )

      _RL F2(irun*(nlay-1),1),F3(irun*(nlay-1),1)
      _RL F4(irun*(nlay-1),1),XQ(irun*(nlay-1),1)

      integer istnlv
      integer i

      ISTNLV = irun * NLEV

C *********************************************************************
C ****           P3 AND DIMENSIONLESS DIFFUSION COEFICIENTS         ***
C ****                  FROM LEVEL 2.5 CLOSURE MODEL                ***
C *********************************************************************

      DO 10 I=1,ISTNLV
      IF( INTQ(I,1).EQ.1 .AND. INTSTB(I,1).EQ.0 ) THEN
        XQ(I,1) = XL(I,1)  /      Q(I,1)
        XQ(I,1) = XQ(I,1)  *     XQ(I,1)
      STRT(I,1) = XQ(I,1)  *   STRT(I,1)
       DW2(I,1) = XQ(I,1)  *    DW2(I,1)
        F2(I,1) = 1.+FF2  *   STRT(I,1)
        F3(I,1) = 1.+FF3  *   STRT(I,1)
        F4(I,1) = 1.+FF4  *   STRT(I,1)
       ZKH(I,1) = (F4(I,1) *     F2(I,1))
     &     + A4 * (F3(I,1) *    DW2(I,1))
       ZKH(I,1) = (F2(I,1) + A3*DW2(I,1))
     &          / ZKH(I,1)
       ZKM(I,1) = A1 * (F3(I,1)*ZKH(I,1)+A5)
     &               /  F2(I,1)
       ZKH(I,1) = A2 * ZKH(I,1)
        P3(I,1) = ZKH(I,1)*STRT(I,1) + B3
        P3(I,1) = 2. * ( ZKM(I,1)*DW2(I,1) - P3(I,1) )
        P3(I,1) = P3(I,1)*Q(I,1)

      ENDIF
   10 CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE TRBDIF ( XX1,XX2,RHOKDZ,FLXFAC,DXX1G,DXX2G,NLEV,
     &                    ITYPE,EPSL,irun )

C**********************************************************************
C   ARGUMENTS ::
C     INPUT:
C     ------
C    XX1           -         FIRST PROPERTY TO BE DIFFUSED
C                              (INPUT INCLUDES FORWARD PRODUCTION TERM)
C    XX2           -         SECOND PROPERTY TO BE DIFFUSED (V-WIND)
C                              (INPUT INCLUDES FORWARD PRODUCTION TERM)
C                                             -OR-
C                            CHANGE IN XX1 DUE TO UNIT CHANGE IN THG
C                               (TH OR SH PROFILES)
C                                             -OR-
C                            BACKWARD PRODUCTION TERM (QQ)
C    RHOKDZ        -         RHO * K * WEIGHT / DZ AT INTERFACES
C    FLXFAC        -         G * DT / (DP*WEIGHT) AT EDGES
C    NLEV          -         NUMBER OF ATMOSPHERIC LEVELS
C    ITYPE         -         INTEGER FLAG FOR INPUT TYPE
C                              1 = QQ: COMPUTE BACKWARD PRODUCTION AND
C                                  USE UNDERFLOW CUTOFF
C                              2 = TH OR SH: COMPUTE TENDENCY DUE TO
C                                  SURFACE PERTURBATION
C                              3 = U AND V: COMPUTE BOTH FIELDS
C    EPSL          -         UNDERFLOW CUTOFF CRITERION (QQ ONLY)
C     OUTPUT:
C     ------
C    XX1           -         NEW VALUE RETURNED
C    XX2           -         NEW VALUE RETURNED
C    DXX1G         -         SOURCE TERM FOR XX1 AT GROUND
C    DXX1G         -         SOURCE TERM FOR XX2 AT GROUND
C**********************************************************************
      implicit none

C Argument List Declarations
      integer nlev,itype,irun
      _RL XX1(irun,NLEV+1),XX2(irun,NLEV+1)
      _RL RHOKDZ(irun,NLEV),FLXFAC(irun,NLEV+1)
      _RL DXX1G(irun),DXX2G(irun)
      _RL epsl

      _RL AA(irun,nlev), BB(irun,nlev), CC(irun,nlev+1)
      integer NLEVP1,NLEVX
      integer I,L

      NLEVP1 = NLEV + 1
      NLEVX =  NLEV - 1
      IF(ITYPE.EQ.2) NLEVX =  NLEV

C   DEFINE MATRIX

      DO 10 I=1,irun
      CC(I,1) = 0.
   10 CONTINUE
      DO L =1,NLEVX
       DO I =1,irun
        CC(I,L+1) = RHOKDZ(I,L) * FLXFAC(I,L+1)
       ENDDO
      ENDDO
      DO L =1,NLEV
       DO I =1,irun
        BB(I,L) =   RHOKDZ(I,L) * FLXFAC(I,L)
        AA(I,L) = 1. + CC(I,L) + BB(I,L)
       ENDDO
      ENDDO

C     ADD IMPLICIT BACKWARD FORCING FOR QQ
      IF(ITYPE.EQ.1) THEN
       DO L =1,NLEV
        DO I =1,irun
         AA(I,L) = AA(I,L) - XX2(I,L)
        ENDDO
       ENDDO
      ENDIF

C     SOLVE MATRIX EQUATION FOR XX1
      CALL VTRI0(AA,BB,CC,XX1,XX1,NLEV,irun)

      IF(ITYPE.EQ.2) THEN
C     COMPUTE CHANGE AT SURFACE

      DO 50 I=1,irun
      DXX1G(I) = CC(I,NLEVP1) * ( XX1(I,NLEV)-XX1(I,NLEVP1) )
   50 CONTINUE

C     SOLVE MATRIX FOR SURFACE PERTURBATION
      CALL VTRI1(AA,BB,XX2,NLEV,irun)
      DO 60 I=1,irun
      DXX2G(I) = CC(I,NLEVP1) * ( XX2(I,NLEV)-XX2(I,NLEVP1) )
   60 CONTINUE
      ENDIF

C     SOLVE MATRIX EQUATION FOR XX2

      IF(ITYPE.EQ.3) CALL VTRI2 (AA,BB,CC,XX2,XX2,NLEV,irun)

C     ELIMINATE UNDERFLOW
      IF(ITYPE.EQ.1) THEN
       DO L =1,NLEV
        DO I =1,irun
         IF ( XX1(I,L).LT.EPSL ) XX1(I,L) = 0.
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE VTRI0 ( A,B,C,F,Y,K,irun)
      implicit none

      integer k,irun
      _RL A(irun,K),B(irun,K),C(irun,K),Y(irun,K+1)
      _RL F(irun,K)

      integer i,L,Lm1

      DO 9000 I = 1,irun
       A(I,1) = 1. / A(I,1)
 9000 CONTINUE

      DO 100 L = 2,K
       LM1 = L - 1
       DO 9002 I = 1,irun
        C(I,L) = C(I,L) * A(I,LM1)
        A(I,L) = 1. / ( A(I,L) - B(I,LM1) * C(I,L) )
        F(I,L) = F(I,L) + F(I,LM1) * C(I,L)
 9002  CONTINUE
 100  CONTINUE

      DO 200 L = K,1,-1
       DO 9004 I = 1,irun
       Y(I,L) = (F(I,L) + B(I,L) * Y(I,L+1)) * A(I,L)
 9004  CONTINUE
 200  CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE VTRI1 ( A,B,Y,K,irun)
      implicit none

      integer k,irun
      _RL A(irun,K),B(irun,K),Y(irun,K+1)

      integer i,L

      DO 200 L = K,1,-1
       DO 9000 I = 1,irun
        Y(I,L) = B(I,L) * Y(I,L+1) * A(I,L)
 9000  CONTINUE
 200  CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE VTRI2 ( A,B,C,F,Y,K,irun)
      implicit none

      integer k,irun
      _RL A(irun,K),B(irun,K),C(irun,K),F(irun,K)
      _RL Y(irun,K+1)

      integer i,L

      DO 100 L = 2,K
       DO 9000 I = 1,irun
        F(I,L) = F(I,L) + F(I,L-1) * C(I,L)
 9000  CONTINUE
 100  CONTINUE

      DO 200 L = K,1,-1
       DO 9002 I = 1,irun
        Y(I,L) = (F(I,L) + B(I,L) * Y(I,L+1)) *  A(I,L)
 9002  CONTINUE
 200  CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE LINADJ ( NN,VRIB1,VRIB2,VWS1,VWS2,VZ1,VUSTAR,IWATER,
     1 VAPSIM, VAPSIHG,VPSIH,VPSIG,VX,VX0,VY,VY0,ITYPE,LWATER,IRUN,
     2 VDZETA,VDZ0,VDPSIM,VDPSIH,INTRIB,
     3 VX0PSIM,VG,VG0,VR1MG0,VZ2,VDZSEA,VAZ0,VXNUM1,VPSIGB2,VDX,
     4 VDXPSIM,VDY,VXNUM2,VDEN,VAWS1,VXNUM3,VXNUM,VDZETA1,VDZETA2,
     5 VZCOEF2,VZCOEF1,VTEMPLIN,VDPSIMC,VDPSIHC)

C**********************************************************************
C  ARGUMENTS ::
C     INPUT:
C     ------
C    RIB1          -         BULK RICHARDSON NUMBER OF INPUT STATE
C    RIB2          -         DESIRED BULK RICH NUMBER OF OUTPUT STATE
C    WS1           -         SURFACE WIND SPEED OF INPUT STATE
C    WS2           -         DESIRED SURFACE WIND SPEED OF OUTPUT STATE
C    Z1            -         INPUT VALUE OF ROUGHNESS HEIGHT
C    USTAR         -         INPUT VALUE OF CU * WS
C    WATER         -         BIT ARRAY - '1' WHERE OCEAN
C    APSIM         -         (1/PSIM)
C    APSIHG        -         ( 1 / (PSIH+PSIG) )
C    PSIH          -         NON-DIM TEMP GRADIENT
C    PSIG          -         PSIH FOR THE MOLECULAR LAYER
C    X             -         PHIM(ZETA) - DERIVATIVE OF PSIM
C    X0            -         PHIM(ZETA0)
C    Y             -         PHIH(ZETA) - DERIVATIVE OF PSIH
C    Y0            -         PHIH(ZETA0)
C    ITYPE         -         INTEGER FLAG :
C                               1 = NEUTRAL ADJUSTMENT
C                               2 = ADJ FOR 2ND OR GREATER TRBFLX ITER
C                               3 - 5 = ADJUSTMENT INSIDE LOOP
C                               4 - 5 = ADJUST CU AND CT
C                               5 = PREPARATION FOR ITYPE = 2
C    LWATER        -         LOGICAL - .TRUE. IF THERE ARE WATER POINTS
C     OUTPUT:
C     -------
C    DZETA         -         D LOG ZETA
C    DZ0           -         D Z0 (ITYPE 1) OR D LOG Z0 (ITYPE 2-5)
C    DPSIM         -         D PSIM
C    DPSIH         -         D PSIH
C    BITRIB        -         BIT ARRAY - '1' WHERE RIB1 = 0
C**********************************************************************
      implicit none

C Argument List Declarations
      integer nn,irun,itype
      _RL VRIB1(IRUN),VRIB2(IRUN)
      _RL VWS1(IRUN),VWS2(IRUN),VZ1(IRUN),VUSTAR(IRUN)
      integer IWATER(IRUN)
      _RL VAPSIM(IRUN),VAPSIHG(IRUN)
      _RL VPSIH(IRUN),VPSIG(IRUN),VX(IRUN)
      _RL VX0(IRUN),VY(IRUN),VY0(IRUN)
      LOGICAL LWATER
      _RL VDZETA(IRUN),VDZ0(IRUN),VDPSIM(IRUN)
      _RL VDPSIH(IRUN)
      integer INTRIB(IRUN)
      _RL VX0PSIM(irun),VG(irun),VG0(irun),VR1MG0(irun)
      _RL VZ2(irun),VDZSEA(irun),VAZ0(irun),VXNUM1(irun)
      _RL VPSIGB2(irun),VDX(irun),VDXPSIM(irun),VDY(irun)
      _RL VXNUM2(irun),VDEN(irun),VAWS1(irun),VXNUM3(irun)
      _RL VXNUM(irun),VDZETA1(irun),VDZETA2(irun)
      _RL VZCOEF2(irun),VZCOEF1(irun),VTEMPLIN(irun)
      _RL VDPSIMC(irun),VDPSIHC(irun)

C Local Variables
      _RL xx0max,prfac,xpfac,difsqt,ustz0s,h0byz0,usth0s
      PARAMETER ( XX0MAX  =   1.49821 )
      PARAMETER ( PRFAC  = 0.595864   )
      PARAMETER ( XPFAC  = .55        )
      PARAMETER ( DIFSQT  = 3.872983E-3)
      PARAMETER ( USTZ0S =   0.2030325E-5)
      PARAMETER ( H0BYZ0 =    30.0    )
      PARAMETER ( USTH0S =  H0BYZ0*USTZ0S )

      integer VINT1(irun),VINT2(irun)
      _RL getcon,vk,bmdl,b2uhs
      integer i

      vk = getcon('VON KARMAN')
      BMDL    = VK * XPFAC * PRFAC / DIFSQT
      B2UHS   = BMDL * BMDL * USTH0S

C   COMPUTE X0/PSIM, 1/Z0, G, G0, 1/(1-G0),
C     DEL LOG Z0, D LOG ZO / D USTAR

CCCOOOMMMM ADDED 'WHERE WATER'
      IF ( (ITYPE.EQ.1) .AND. LWATER ) THEN
       DO 9000 I = 1,IRUN
        IF (IWATER(I).EQ.1) VX0PSIM(I) = VAPSIM(I)
 9000  CONTINUE
      ENDIF
      IF ( ITYPE .GE. 3 ) THEN
       DO 9002 I = 1,IRUN
        VX0PSIM(I) = VX0(I) * VAPSIM(I)
 9002  CONTINUE
      ENDIF
      IF ( ITYPE .NE. 2 ) THEN

       DO 9004 I = 1,IRUN
        VDZ0(I) = 0.
        VG(I) = 0.
        VG0(I) = 0.
        VR1MG0(I) = 1.
 9004  CONTINUE

       IF ( LWATER ) THEN
        CALL ZCSUB ( VUSTAR,VDZSEA,IWATER,.TRUE.,IRUN,VZ2)

        DO 9006 I = 1,IRUN
         IF ( IWATER(I).EQ.1) THEN
          VAZ0(I) = 1. / VZ1(I)
          VG(I) = VDZSEA(I) * VAZ0(I)
          VG0(I) = VX0PSIM(I) * VG(I)
          VR1MG0(I) = 1. / ( 1. - VG0(I) )
          VDZ0(I) = ( VZ2(I) - VZ1(I) ) * VR1MG0(I)
         ENDIF
 9006   CONTINUE
       ENDIF
      ENDIF

      IF ( LWATER .AND. (ITYPE.GE.3) ) THEN
       DO 9008 I = 1,IRUN
        IF (IWATER(I).EQ.1) VDZ0(I) = VDZ0(I) * VAZ0(I)
 9008  CONTINUE
      ENDIF

C   COMPUTE NUM1,NUM2,NUM3, DEN

      IF (ITYPE.GE.3) THEN
       DO 9010 I = 1,IRUN
        VXNUM1(I) = 0.
        IF (VRIB1(I).EQ.0.) THEN
         INTRIB(I) = 1
        ELSE
         INTRIB(I) = 0
        ENDIF
        IF ( INTRIB(I).EQ.0 ) VXNUM1(I) = 1. / VRIB1(I)
        VPSIGB2(I) = 0.
        if(vpsig(i).gt.0.)VPSIGB2(I) =
     1        0.5 * ( vpsig(i)*vpsig(i) + b2uhs ) / vpsig(i)
        VDX(I) = VX(I) - VX0(I)
        VDXPSIM(I) = VDX(I) * VAPSIM(I)
        VDY(I) = VY(I) - VY0(I)
        VXNUM3(I) = - VPSIGB2(I)

        IF ( LWATER ) THEN
CCCOOOMMMM ADDED 'WHERE WATER'
         IF (IWATER(I).EQ.1) THEN
          VDXPSIM(I) = VDXPSIM(I) * VR1MG0(I)
          VXNUM3(I) = VXNUM3(I) + VG(I) * ( VY0(I) - VPSIGB2(I) )
          VXNUM2(I) = VY0(I) - VPSIGB2(I) - VX0PSIM(I) * VPSIGB2(I)
          VXNUM2(I) = (VXNUM2(I) * VAPSIHG(I)) - 2. * VX0PSIM(I)
          VXNUM2(I) = VXNUM2(I) * VDZ0(I)
         ENDIF
        ENDIF

        VDEN(I) = VDY(I) + VDXPSIM(I) * VXNUM3(I)
        VDEN(I) = ( 1. + VDEN(I) * VAPSIHG(I) ) - 2. * VDXPSIM(I)
 9010  CONTINUE
      ENDIF

      IF (ITYPE.EQ.5) THEN
       DO 9012 I = 1,IRUN
        VAWS1(I) = VR1MG0(I) / VWS1(I)
        VXNUM3(I) = VXNUM3(I) * VAPSIHG(I)

        IF ( LWATER ) THEN
CCCOOOMMMM ADDED 'WHERE WATER'
         IF(IWATER(I).EQ.1) THEN
          VXNUM3(I) = VXNUM3(I) - 2. * VG0(I)
          VXNUM3(I) = VAWS1(I) * VXNUM3(I)
         ENDIF
        ENDIF
 9012  CONTINUE
      ENDIF

C   COMPUTE D LOG ZETA

      IF (ITYPE.GE.2) THEN
       DO 9014 I = 1,IRUN
        VXNUM(I) = VRIB2(I) - VRIB1(I)
        IF( (VX0(I).GT.XX0MAX).AND.(VXNUM(I).GE.0.) )VXNUM(I) = 0.
        VXNUM(I) = VXNUM1(I) * VXNUM(I)
 9014  CONTINUE
      ENDIF

      IF ( ITYPE.EQ.2 )THEN
       DO 9016 I = 1,IRUN
        VDZETA1(I) = VDZETA(I)
        VXNUM(I) = VXNUM(I) + VXNUM3(I) * ( VWS2(I) - VWS1(I) )
 9016  CONTINUE
      ENDIF

      IF (ITYPE.GE.3) THEN
       DO 9018 I = 1,IRUN
        VDZETA1(I) = VXNUM(I)
        IF(LWATER.AND.(IWATER(I).EQ.1)) VXNUM(I) = VXNUM(I) + VXNUM2(I)
        IF ( VDEN(I) .LT.0.1 ) VDEN(I) = 0.1
 9018  CONTINUE
      ENDIF

      IF (ITYPE.GE.2) THEN
       DO 9020 I = 1,IRUN
        VDZETA(I) = VXNUM(I) / VDEN(I)
 9020  CONTINUE
      ENDIF
      IF (ITYPE.GE.3) THEN
       DO 9022 I = 1,IRUN
        IF ( (VRIB2(I).EQ.0.) .OR. (VDZETA(I).LE.-1.) ) THEN
         VINT1(I) = 1
        ELSE
         VINT1(I) = 0
        ENDIF
        IF ( VINT1(I).EQ.1 ) VDZETA(I) = VDZETA1(I)
 9022  CONTINUE
      ENDIF
      IF (ITYPE.EQ.2) THEN
       DO 9024 I = 1,IRUN
        VDZETA2(I) = VDZETA(I) + VDZETA1(I)
        IF ( (VRIB2(I).EQ.0.) .OR. (VDZETA2(I).LE.-1.) ) THEN
         VINT1(I) = 1
        ELSE
         VINT1(I) = 0
        ENDIF
        IF(VINT1(I).EQ.1)VDZETA(I)=VXNUM1(I)*VRIB2(I) - 1. - VDZETA1(I)
 9024  CONTINUE
      ENDIF

C   COMPUTE D LOG Z0

      IF ( LWATER .AND. (ITYPE.GE.3) )THEN
       DO 9026 I = 1,IRUN
        IF( IWATER(I).EQ.1 ) THEN
         VZCOEF2(I) = VG(I) * VDXPSIM(I)
         VDZ0(I) = VDZ0(I) - VZCOEF2(I) * VDZETA(I)
        ENDIF
 9026  CONTINUE
      ENDIF

      IF ( LWATER .AND. (ITYPE.EQ.5) ) THEN
       DO 9028 I = 1,IRUN
        IF(IWATER(I).EQ.1) VZCOEF1(I) = VG(I) * VAWS1(I)
 9028  CONTINUE
      ENDIF

      IF ( LWATER .AND. (ITYPE.EQ.2) ) THEN
       DO 9030 I = 1,IRUN
        IF (IWATER(I).EQ.1) VDZ0(I) =
     1   VZCOEF1(I) * ( VWS2(I) - VWS1(I) ) - VZCOEF2(I) * VDZETA(I)
 9030  CONTINUE
      ENDIF

C   CALCULATE D PSIM AND D PSIH

      IF ( (ITYPE.EQ.1) .AND. LWATER ) THEN
       DO 9032 I = 1,IRUN
        IF (IWATER(I).EQ.1) THEN
         VDPSIM(I) = - VDZ0(I) * VAZ0(I)
         VDPSIH(I) = VDPSIM(I)
        ENDIF
 9032  CONTINUE
      ENDIF

      IF (ITYPE.GE.3) THEN
       DO 9034 I = 1,IRUN
        VDPSIM(I) = VDX(I) * VDZETA(I)
        VDPSIH(I) = VDY(I) * VDZETA(I)
        IF ( LWATER ) THEN
         IF (IWATER(I).EQ.1 ) THEN
          VDPSIM(I) = VDPSIM(I) - VX0(I) * VDZ0(I)
          VDPSIH(I) = VDPSIH(I) - VY0(I) * VDZ0(I)
         ENDIF
        ENDIF
 9034  CONTINUE
      ENDIF

C   PREVENT OVERCORRECTION OF PSIM OR PSIH FOR UNSTABLE CASE

      IF (ITYPE.GE.4) THEN
       DO 9036 I = 1,IRUN
        VDPSIMC(I) = -0.9 - VDPSIM(I) * VAPSIM(I)
        VDPSIHC(I) = -0.9 *  VPSIH(I) - VDPSIH(I)
        IF ( VDPSIMC(I).GT.0.  ) THEN
         VINT1(I) = 1
        ELSE
         VINT1(I) = 0
        ENDIF
        IF ( VDPSIHC(I).GT.0.  ) THEN
         VINT2(I) = 1
        ELSE
         VINT2(I) = 0
        ENDIF
        VDZETA1(I) = 0.
        IF(VINT1(I).EQ.1) VDZETA1(I) = VDPSIMC(I) / VDXPSIM(I)
        IF((VINT1(I).EQ.1).OR.(VINT2(I).EQ.1)) VTEMPLIN(I) =
     1        VDY(I) + VY0(I) * VG(I) * VDXPSIM(I)
        IF (VINT2(I).EQ.1) then
             VDZETA2(I) =  VDPSIHC(I) / VTEMPLIN(I)
        IF ( VDZETA2(I).LT.VDZETA1(I) ) VDZETA1(I) = VDZETA2(I)
        endif
        IF((VINT1(I).EQ.1).OR.(VINT2(I).EQ.1)) THEN
         VDZETA(I) = VDZETA1(I) + VDZETA(I)
         VDPSIM(I) = VDPSIM(I) + VDX(I) * VR1MG0(I) * VDZETA1(I)
         VDPSIH(I) = VDPSIH(I) + VTEMPLIN(I) * VDZETA1(I)
         IF ( IWATER(I).EQ.1 )
     1     VDZ0(I) = VDZ0(I) - VG(I) * VDXPSIM(I) * VDZETA1(I)
        ENDIF
 9036  CONTINUE
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE ZCSUB (VUSTAR,VDZSEA,IWATER,LDZSEA,IRUN,VZSEA)
C**********************************************************************
C  FUNCTION ZSEA
C  PURPOSE
C     COMPUTES Z0 AS A FUNCTION OF USTAR OVER WATER SURFACES
C  USAGE
C     CALLED BY SFCFLX
C  DESCRIPTION OF PARAMETERS
C     USTAR -  INPUTED VALUE OF SURFACE-STRESS VELOCITY
C     DZSEA -  OUTPUTED VALUE OF DERIVATIVE  D(ZSEA)/D(USTAR)
C     WATER -  INPUTED BIT VECTOR TO DETERMINE WATER POINTS
C     LDZSEA-  LOGICAL FLAG TO DETERMINE IF DZSEA SHOULD BE COMPUTED
C     ZSEA  -  OUTPUTED VALUE OF ROUGHNESS LENGTH
C  SUBPROGRAMS NEEDED
C     NONE
C  RECORD OF MODIFICATIONS
C  REMARKS:
C        COMPUTE ROUGHNESS LENGTH FOR OCEAN POINTS
C          BASED ON FUNCTIONS OF LARGE AND POND
C          AND OF KONDO --- DESIGNED FOR K = .4
C *********************************************************************
      implicit none

C Argument List Delcarations
      integer irun
      _RL VZSEA(IRUN),VUSTAR(IRUN),VDZSEA(IRUN)
      integer IWATER(IRUN)
      LOGICAL LDZSEA

C Local Variables
      _RL USTMX1,USTMX2,USTMX3
      PARAMETER ( USTMX1 =   1.14973  )
      PARAMETER ( USTMX2 =   0.381844 )
      PARAMETER ( USTMX3 =   0.0632456)

      _RL AA(IRUN,5),TEMP(IRUN)
      integer INT2(IRUN),INT3(IRUN),INT4(IRUN)
      integer i,k

      _RL AA1(5),AA2(5),AA3(5),AA4(5)
      DATA AA1/.2030325E-5,0.0,0.0,0.0,0.0/
      DATA AA2/-0.402451E-08,0.239597E-04,0.117484E-03,0.191918E-03,
     1         0.395649E-04/
      DATA AA3/-0.237910E-04,0.228221E-03,-0.860810E-03,0.176543E-02,
     1         0.784260E-04/
      DATA AA4/-0.343228E-04,0.552305E-03,-0.167541E-02,0.250208E-02,
     1         -0.153259E-03/

C**********************************************************************
C*****              LOWER CUTOFF CONDITION FOR USTAR                ***
C**********************************************************************

      DO 9000 I = 1,IRUN
       IF(VUSTAR(I) .LT. 1.e-6)THEN
        INT3(I) = 1
       ELSE
        INT3(I) = 0
       ENDIF
 9000 CONTINUE
      DO 9002 I = 1,IRUN
       IF(INT3(I).EQ.1) VUSTAR(I) = 1.e-6
 9002 CONTINUE

C***********************************
C*****  LOAD THE ARRAY A(I,K)  *****
C***********************************

      DO 9004 I = 1,IRUN
       IF( (VUSTAR(I) .GT. USTMX1) .AND. (IWATER(I).EQ.1) ) THEN
        INT4(I) = 1
       ELSE
        INT4(I) = 0
       ENDIF
 9004 CONTINUE
      DO 9006 I = 1,IRUN
       IF(VUSTAR(I) .GT. USTMX2) THEN
        INT3(I) = 1
       ELSE
        INT3(I) = 0
       ENDIF
 9006 CONTINUE
      DO 9008 I = 1,IRUN
       IF(VUSTAR(I) .GE. USTMX3) THEN
        INT2(I) = 1
       ELSE
        INT2(I) = 0
       ENDIF
 9008 CONTINUE

      DO 100 K=1,5
       DO 9010 I = 1,IRUN
        AA(I,K) = AA1(K)
        IF( INT2(I).EQ.1 )  AA(I,K) = AA2(K)
        IF( INT3(I).EQ.1 )  AA(I,K) = AA3(K)
        IF( INT4(I).EQ.1 )  AA(I,K) = AA4(K)
 9010  CONTINUE
  100 CONTINUE

C********************************************************
C*****  EVALUATE THE ENHANCED POLYNOMIAL FOR ZSEA  *****
C********************************************************

      DO 9012 I = 1,IRUN
       VDZSEA(I)  =  ( AA(I,4) + AA(I,5) * VUSTAR(I) ) * VUSTAR(I)
       VZSEA(I)  =  AA(I,2) + ( AA(I,3) + VDZSEA(I) ) * VUSTAR(I)
       TEMP(I) = AA(I,1) / VUSTAR(I)
       VZSEA(I)  =  VZSEA(I) + TEMP(I)
 9012 CONTINUE

C**********************************************************************
C*****        EVALUATE THE DERIVATIVE DZSEA IF LDZSEA IS TRUE       ***
C**********************************************************************

      IF( LDZSEA ) THEN
       DO 9014 I = 1,IRUN
        VDZSEA(I)  =  3. * VDZSEA(I) -(AA(I,4)*VUSTAR(I) - AA(I,3))
        VDZSEA(I)  =  VDZSEA(I) * VUSTAR(I) - TEMP(I)
 9014  CONTINUE
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE SEAICE ( nocean, timstp, hice,
     &                     eturb,  dedtc,
     &                    hsturb, dhsdtc,
     &                      qice,  dqice,
     &                     swnet,  lwnet, dst4,
     &                       pke,  seaic, tc, qa )
      implicit none
      integer  nocean
      _RL timstp
      _RL eturb(nocean),dedtc(nocean),hsturb(nocean),dhsdtc(nocean)
      _RL swnet(nocean),lwnet(nocean),  dst4(nocean)
      _RL  qice(nocean),dqice(nocean)
      _RL   pke(nocean),   tc(nocean),    qa(nocean)
      _RL seaic(nocean)

C  rho*C = 1.93e6 J/(m**3 K) ; Peixoto & Oort
      _RL rhoC
      parameter (rhoC = 1.93e6)

      _RL faceps,getcon,latent,codt,deltg,hice
      integer i

      faceps = getcon('EPSFAC')
      latent = getcon('HEATI') * getcon('CALTOJ')
C Note hice is in centimeters
      codt   = rhoC * (hice/100) / timstp

C Update TC and QA
C ----------------
      do i =1,nocean
       if( seaic(i).gt.0.0 ) then
        deltg = ( swnet(i)-lwnet(i)-latent*eturb(i)-hsturb(i)+qice(i) )
     &          / ( codt+dst4(i)+latent*dedtc(i)+dhsdtc(i)-dqice(i) )
        qa(i) = qa(i) + (faceps*qa(i)/(tc(i)*tc(i)))*deltg
        tc(i) = tc(i) + deltg
       endif
      enddo

      return
      end
