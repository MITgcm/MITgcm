      subroutine physdrv (nymd,nhms,myid,im,jm,lm,ptracer,ntracer,
     . ndmoist,ndturb,ndlw,ndsw,xlats,xlons,
     . p,u,v,t,q,pl,ple,dpres,pkht,pkl,fracland,landtype,radswt,
     . maxtyp,surftype,tilefrac,phis_var,sea_ice,
     . nchp,chlat,chlon,igrd,nchpland,chfr,ityp,
     . tcanopy,tdeep,ecanopy,swetshal,swetroot,swetdeep,capac,snodep,
     . ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     . stratq,nwatlevs,nwatlats,watlevs,watlats,ozone,nozolevs,
     . albvisdr,albvisdf,albnirdr,albnirdf,emiss,alai,agrn,
     . nozolats,ozolevs,ozolats,co2,cfc11,cfc12,cfc22,methane,n2o,
     . lwdt,lwdtclr,swdt,swdtclr,turbu,turbv,turbt,turbq,
     . moistu,moistv,moistt,moistq,
     . radswg,swgclr,albedo,fdirpar,fdifpar,osr,osrclr,tg0,tg0c,radlwg,
     . st4,dst4,dlwdtg,lwclr,rainlsp,raincon,snowfall,iras,nlwcld,
     . cldtot_lw,clras_lw,cldlsp_lw,nlwlz,lwlz,nswcld,cldtot_sw,
     . clras_sw,cldlsp_sw,nswlz,swlz,imstturbsw,imstturblw,qliqavesw,
     . qliqavelw,fccavesw,fccavelw,qq,u2m,v2m,t2m,q2m,u10m,v10m,t10m,
     . q10m,tgz,gwet,snow,chemo3,chemq,diag2d,ndiag2d,diag3d,ndiag3d)

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
      implicit none

c Diagnostic Common
c -----------------
#include 'diagnostics.h'

c Timers Common
c -------------
#include 'chronos.h'

c Input Parameters
c ----------------
      integer nymd,nhms,myid,im,jm,lm,ptracer,ntracer
      integer maxtyp
      integer nchp,chlat,chlon,igrd,nchpland,chfr,ityp
      integer nwatlevs,nwatlats
      integer nozolevs,nozolats
      integer iras,nlwcld,nlwlz,nswcld,nswlz
      integer imstturbsw,imstturblw
      integer ndiag2d,ndiag3d,ndmoist,ndturb,ndlw,ndsw

      real xlats(im,jm), xlons(im,jm)
      real p(im,jm)
      real u(im,jm,lm), v(im,jm,lm), t(im,jm,lm)
      real q(im,jm,lm,ntracer)
      real pl(im,jm,lm),ple(im,jm,lm+1),dpres(im,jm,lm),pkht(im,jm,lm)
      real radswt(im,jm), fracland(im,jm), landtype(im,jm)
      integer surftype(im,jm,maxtyp)
      real tilefrac(im,jm,maxtyp)
      real phis_var(im,jm), sea_ice(im,jm)
      real tcanopy(nchp),tdeep(nchp),ecanopy(nchp),swetshal(nchp)
      real swetroot(nchp),swetdeep(nchp),capac(nchp),snodep(nchp)
      real ctmt(nchp),xxmt(nchp),yymt(nchp),zetamt(nchp)
      real xlmt(nchp,lm),khmt(nchp,lm),tke(nchp,lm)
      real stratq(nwatlats,nwatlevs )
      real watlats(nwatlats)
      real watlevs(nwatlevs)
      real ozone(nozolats,nozolevs)
      real ozolevs(nozolevs),ozolats(nozlats)
      real co2,cfc11,cfc12,cfc22,methane(lm),n2o(lm)
      real lwdt(im,jm,lm),lwdtclr(im,jm,lm)
      real swdt(im,jm,lm),swdtclr(im,jm,lm)
      real turbu(im,jm,lm),turbv(im,jm,lm),turbt(im,jm,lm)
      real turbq(im,jm,lm)
      real moistu(im,jm,lm),moistv(im,jm,lm),moistt(im,jm,lm)
      real moistq(im,jm,lm)
      real chemo3(im,jm,lm),chemq(im,jm,lm)
      real albvisdr(im,jm),albvisdf(im,jm),albnirdr(im,jm)
      real albnirdf(im,jm),emiss(im,jm)
      real alai(nchp),agrn(nchp)
      real radswg(im,jm),swgclr(im,jm),albedo(im,jm)
      real fdirpar(im,jm),fdifpar(im,jm),osr(im,jm),osrclr(im,jm)
      real tg0(im,jm),tg0c(im,jm),radlwg(im,jm),st4(im,jm),dst4(im,jm)
      real dlwdtg(im,jm),lwclr(im,jm)
      real rainlsp(im,jm),raincon(im,jm),snowfall(im,jm)
      real cldtot_lw(im,jm,lm),clras_lw(im,jm,lm),cldlsp_lw(im,jm,lm)
      real lwlz(im,jm,lm)
      real cldtot_sw(im,jm,lm),clras_sw(im,jm,lm),cldlsp_sw(im,jm,lm)
      real swlz(im,jm,lm)
      real qliqavesw(im,jm,lm),qliqavelw(im,jm,lm)
      real fccavesw(im,jm,lm),fccavelw(im,jm,lm)
      real qq(im,jm,lm)
      real u2m(im,jm),v2m(im,jm),t2m(im,jm),q2m(im,jm),u10m(im,jm)
      real v10m(im,jm),t10m(im,jm),q10m(im,jm),tgz(im,jm)
      real gwet(im,jm),snow(im,jm)
      real diag2d(nchp,ndiag2d),diag3d(im,jm,ndiag3d)

c Local Variables
c ---------------
      logical     alarm
      external    alarm

      integer istrip,npcs
      integer i,j,L,n
      integer ndum,ndpnt
      real dum,akap,getcon
      real ptop
      logical lpnt

      istrip = min( im*jm,71 )
      npcs   = ((im*jm-1)/istrip) + 1

      ptop   =  0.
      akap = getcon('KAPPA')

C **********************************************************************        
C ****                         Initialization                       ****        
C **********************************************************************        
                                                                                
      call get_alarm ( 'pnt',ndum,ndum,ndpnt,ndum )
      lpnt = ndpnt.ne.0

C Compute pl,ple,pkht,pkl,dpres   and pcheck for use by mini-drivers

C Make 'model theta' out of theta, ie, divide by p0**kappa

C **********************************************************************        
C ****                  Call Physics Mini-Drivers                   ****        
C **********************************************************************        
                                                                                
C SHORT WAVE RADIATION
C ====================
      IF ( alarm('radsw') ) THEN

      call swrio ( nymd,nhms,ndsw,myid,istrip,npcs,
     .   p,pl,ple,dpres,pkht,pkl,t,chemq,chemo3,co2,
     .   albvisdr,albvisdf,albnirdr,albnirdf,swdt,swdtclr,
     .   radswg,swgclr,albedo,fdifpar,fdirpar,osr,osrclr,im,jm,lm,
     .   ptop,nswcld,cldtot_sw,cldras_sw,nswlz,swlz,
     .   .false.,diag3d,ndiag3d,imstturbsw,qliqavesw,
     .   fccavesw,landtype,xlats,xlons )

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
      do j=1,jm
      do i=1,im
      tg0c(i,j) = tg0(i,j)
      enddo
      enddo

      call lwrio ( nymd,nhms,istrip,npcs,
     .   p,pl,ple,dpres,pkht,pkl,t,chemq,pkht,chemo3,co2,
     .   cfc11,cfc12,cfc22,methane,n2o,emiss,tgz,radlwg,st4,dst4,
     .   lwdt,dlwdtg,lwdtclr,lwgclr,im,jm,lm,ptop,
     .   nlwcld,cldtot_lw,cldras_lw,nlwlz,lwlz,
     .   .false.,diag3d,ndiag3d,imstturblw,qliqavelw,
     .   fccavelw,landtype )

      ENDIF
                                                                                
C TURBULENCE
C ==========
      IF ( alarm('turb') ) THEN

      call turbio (im,jm,lm,maxtyp,1,nymd,nhms,ndturb,
     .   ptop,p,u,v,t,q,ntracer,ptracer,pl,ple,dpres,pkht,pkl,
     .   ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     .   tgz,gwet,snow,fracland,landtype,
     .   tcanopy,ecanopy,tdeep,swetshal,swetroot,swetdeep,snodep,capac,
     .   nchp,nchpland,chfr,chlt,chlon,igrd,ityp,alai,agrn,
     .   surftype,tilefrac,sea_ice,lpnt,diag3d,ndiag3d,diag2d,ndiag2d,
     .   turbu,turbv,turbt,turbq,radlwg,st4,dst4,radswg,radswt,
     .   fdifpar,fdirpar,rainlsp,rainconv,snowfall,tg0,
     .   imstturblw,imstturbsw,qliqavelw,qliqavesw,fccavelw,fccavesw,qq,
     .   u2m ,v2m ,t2m ,q2m,u10m,v10m,t10m,q10m,myid,comm )

c Add Gravity-Wave Drag Tendency
c ------------------------------
C Comment this out for now - run tropospheric resolutions only.
C Need to see how to get zonal wavelengths on the cube.

c     call gwdrag (myid,p,pl,ple,dpres,pkht,pkl,u,v,t,q,phis_var,
c    .   turbu,turbv,turbt,diag3d,ndiag3d,im,jm,lm,ptop,istrip,npcs,
c    .   imglobal)

      endif
                                                                                
C MOIST PROCESSES
C ===============
      if ( alarm('moist') ) then
      call moistio (ndmoist,istrip,npcs,
     .    p,pl,ple,dpres,pkht,pkl,t,q,ntracer,ptracer,qq,
     .    moistu,moistv,moistt,moistq,im,jm,lm,ptop,iras,
     .    rainlsp,rainconv,snowfall,
     .    nswcld,cldtot_sw,cldras_sw,cldlsp_sw,nswlz,swlz,
     .    nlwcld,cldtot_lw,cldras_lw,cldlsp_lw,nlwlz,lwlz,
     .              .false.,diag3d,ndiag3d,myid)

      endif

      return
      end
