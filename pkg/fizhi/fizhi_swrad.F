C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_swrad.F,v 1.26 2009/04/01 19:54:17 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      subroutine swrio (nymd,nhms,bi,bj,ndswr,myid,istrip,npcs,
     .        low_level,mid_level,im,jm,lm,
     .        pz,plz,plze,dpres,pkht,pkz,tz,qz,oz,co2,
     .        albvisdr,albvisdf,albnirdr,albnirdf,
     .        dtradsw,dtswclr,radswg,swgclr,
     .        fdifpar,fdirpar,osr,osrclr,
     .        ptop,nswcld,cldsw,cswmo,nswlz,swlz,
     .        lpnt,imstturb,qliqave,fccave,landtype,xlats,xlons)

      implicit none

c Input Variables
c ---------------
      integer nymd,nhms,bi,bj,ndswr,myid,istrip,npcs
      integer mid_level,low_level
      integer im,jm,lm
      _RL  ptop
      _RL pz(im,jm),plz(im,jm,lm),plze(im,jm,lm+1),dpres(im,jm,lm)
      _RL pkht(im,jm,lm+1),pkz(im,jm,lm)
      _RL tz(im,jm,lm),qz(im,jm,lm)
      _RL oz(im,jm,lm)
      _RL co2
      _RL albvisdr(im,jm),albvisdf(im,jm),albnirdr(im,jm)
      _RL albnirdf(im,jm)
      _RL radswg(im,jm),swgclr(im,jm),fdifpar(im,jm),fdirpar(im,jm)
      _RL osr(im,jm),osrclr(im,jm),dtradsw(im,jm,lm)
      _RL dtswclr(im,jm,lm)
      integer nswcld,nswlz
      _RL cldsw(im,jm,lm),cswmo(im,jm,lm),swlz(im,jm,lm)
      logical lpnt
      integer imstturb
      _RL qliqave(im,jm,lm),fccave(im,jm,lm)
      integer landtype(im,jm)
      _RL xlats(im,jm),xlons(im,jm)

c Local Variables
c ---------------
      integer   i,j,L,nn,nsecf
      integer   ntmstp,nymd2,nhms2
      _RL      getcon,grav,cp,undef
      _RL      ra,alf,reffw,reffi,tminv

      parameter ( reffw = 10.0 )
      parameter ( reffi = 65.0 )

      _RL tdry(im,jm,lm)
      _RL TEMP1(im,jm)
      _RL TEMP2(im,jm)
      _RL zenith (im,jm)
      _RL cldtot (im,jm,lm)
      _RL cldmxo (im,jm,lm)
      _RL totcld (im,jm)
      _RL cldlow (im,jm)
      _RL cldmid (im,jm)
      _RL cldhi  (im,jm)
      _RL taulow (im,jm)
      _RL taumid (im,jm)
      _RL tauhi  (im,jm)
      _RL tautype(im,jm,lm,3)
      _RL tau(im,jm,lm)
      _RL albedo(im,jm)

      _RL PK(ISTRIP,lm)
      _RL qzl(ISTRIP,lm),CLRO(ISTRIP,lm)
      _RL TZL(ISTRIP,lm)
      _RL OZL(ISTRIP,lm)
      _RL PLE(ISTRIP,lm+1)
      _RL COSZ(ISTRIP)
      _RL dpstrip(ISTRIP,lm)

      _RL albuvdr(ISTRIP),albuvdf(ISTRIP)
      _RL albirdr(ISTRIP),albirdf(ISTRIP)
      _RL difpar (ISTRIP),dirpar (ISTRIP)

      _RL fdirir(istrip),fdifir(istrip)
      _RL fdiruv(istrip),fdifuv(istrip)

      _RL flux(istrip,lm+1)
      _RL fluxclr(istrip,lm+1)
      _RL dtsw(istrip,lm)
      _RL dtswc(istrip,lm)

      _RL taul   (istrip,lm)
      _RL reff   (istrip,lm,2)
      _RL tauc   (istrip,lm,2)
      _RL taua   (istrip,lm)
      _RL tstrip (istrip)
#ifdef ALLOW_DIAGNOSTICS
      logical  diagnostics_is_on
      external diagnostics_is_on
      _RL tmpdiag(im,jm),tmpdiag2(im,jm)
#endif

      logical first
      data first /.true./

C **********************************************************************
C ****                       INITIALIZATION                         ****
C **********************************************************************

      grav  = getcon('GRAVITY')
      cp    = getcon('CP')
      undef = getcon('UNDEF')

      NTMSTP = nsecf(NDSWR)
      TMINV  = 1./float(ntmstp)

C Compute Temperature from Theta
C ------------------------------
      do L=1,lm
      do j=1,jm
      do i=1,im
      tdry(i,j,L) = tz(i,j,L)*pkz(i,j,L)
      enddo
      enddo
      enddo

      if (first .and. myid.eq.1 ) then
      print *
      print *,'Low-Level Clouds are Grouped between levels: ',
     .         lm,' and ',low_level
      print *,'Mid-Level Clouds are Grouped between levels: ',
     .         low_level-1,' and ',mid_level
      print *
      first = .false.
      endif

C **********************************************************************
C ****             CALCULATE COSINE OF THE ZENITH ANGLE             ****
C **********************************************************************

#ifdef FIZHI_USE_FIXED_DAY
      CALL ASTRO ( 20040321,  NHMS,  XLATS,XLONS, im*jm, TEMP1,RA )
                   NYMD2 = NYMD
                   NHMS2 = NHMS
      CALL TICK  ( NYMD2,  NHMS2, NTMSTP )
      CALL ASTRO ( 20040321,  NHMS2, XLATS,XLONS, im*jm, TEMP2,RA )
#else
      CALL ASTRO ( NYMD,   NHMS,  XLATS,XLONS, im*jm, TEMP1,RA )
                   NYMD2 = NYMD
                   NHMS2 = NHMS
      CALL TICK  ( NYMD2,  NHMS2, NTMSTP )
      CALL ASTRO ( NYMD2,  NHMS2, XLATS,XLONS, im*jm, TEMP2,RA )
#endif

      do j = 1,jm
      do i = 1,im
          zenith(I,j) = TEMP1(I,j) + TEMP2(I,j)
      IF( zenith(I,j) .GT. 0.0 ) THEN
          zenith(I,j) = (2./3.)*( zenith(I,j)-TEMP2(I,j)*TEMP1(I,j)
     .                          / zenith(I,j) )
      ENDIF
      ENDDO
      ENDDO

C **********************************************************************
c ****        Compute Two-Dimension Total Cloud Fraction (0-1)      ****
C **********************************************************************

c Initialize Clear Sky Probability for Low, Mid, and High Clouds
c --------------------------------------------------------------
      do j =1,jm
      do i =1,im
      cldlow(i,j) = 0.0
      cldmid(i,j) = 0.0
      cldhi (i,j) = 0.0
      enddo
      enddo

c Adjust cloud fractions and cloud liquid water due to moist turbulence
c ---------------------------------------------------------------------
      if(imstturb.ne.0) then
        do L =1,lm
        do j =1,jm
        do i =1,im
           cldtot(i,j,L)=min(1.0 _d 0,max(cldsw(i,j,L),fccave(i,j,L)/
     $          imstturb))
           cldmxo(i,j,L)=min(1.0 _d 0,cswmo(i,j,L))
           swlz(i,j,L)=swlz(i,j,L)+qliqave(i,j,L)/imstturb
        enddo
        enddo
        enddo
      else
        do L =1,lm
        do j =1,jm
        do i =1,im
         cldtot(i,j,L) =  min( 1.0 _d 0,cldsw(i,j,L) )
         cldmxo(i,j,L) =  min( 1.0 _d 0,cswmo(i,j,L) )
        enddo
        enddo
        enddo
      endif

c Compute 3-D Cloud Fractions
c ---------------------------
      if( nswcld.ne.0 ) then
      do L = 1,lm
      do j = 1,jm
      do i = 1,im
c Compute Low-Mid-High Maximum Overlap Cloud Fractions
c ----------------------------------------------------
      if(     L.lt.mid_level ) then
             cldhi (i,j) = max( cldtot(i,j,L),cldhi (i,j) )
      elseif( L.ge.mid_level .and.
     .        L.lt.low_level ) then
             cldmid(i,j) = max( cldtot(i,j,L),cldmid(i,j) )
      elseif( L.ge.low_level ) then
             cldlow(i,j) = max( cldtot(i,j,L),cldlow(i,j) )
      endif

      enddo
      enddo
      enddo
      endif

c Totcld => Product of Clear Sky Probabilities for Low, Mid, and High Clouds
c --------------------------------------------------------------------------
      do j = 1,jm
      do i = 1,im
      totcld(i,j) = 1.0 - (1.-cldhi (i,j))
     .                  * (1.-cldmid(i,j))
     .                  * (1.-cldlow(i,j))
      enddo
      enddo

#ifdef ALLOW_DIAGNOSTICS

c Compute Cloud Diagnostics
c -------------------------
      if(diagnostics_is_on('CLDFRC  ',myid) ) then
       call diagnostics_fill(totcld,'CLDFRC  ',0,1,3,bi,bj,myid)
      endif

      if(diagnostics_is_on('CLDRAS  ',myid) ) then
       do L=1,lm
       do j=1,jm
       do i=1,im
        tmpdiag(i,j) = cswmo(i,j,L)
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'CLDRAS  ',L,1,3,bi,bj,myid)
      enddo
      endif

      if(diagnostics_is_on('CLDTOT  ',myid) ) then
       do L=1,lm
       do j=1,jm
       do i=1,im
        tmpdiag(i,j) = cldtot(i,j,L)
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'CLDTOT  ',L,1,3,bi,bj,myid)
      enddo
      endif

      if(diagnostics_is_on('CLDLOW  ',myid) ) then
       call diagnostics_fill(cldlow,'CLDLOW  ',0,1,3,bi,bj,myid)
      endif

      if(diagnostics_is_on('CLDMID  ',myid) ) then
       call diagnostics_fill(cldmid,'CLDMID  ',0,1,3,bi,bj,myid)
      endif

      if(diagnostics_is_on('CLDHI   ',myid) ) then
       call diagnostics_fill(cldhi,'CLDHI   ',0,1,3,bi,bj,myid)
      endif

      if(diagnostics_is_on('LZRAD   ',myid) ) then
       do L=1,lm
       do j=1,jm
       do i=1,im
        tmpdiag(i,j) = swlz(i,j,L) * 1.0e6
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'LZRAD   ',L,1,3,bi,bj,myid)
       enddo
      endif

c Albedo Diagnostics
c ------------------
      if(diagnostics_is_on('ALBVISDR',myid) ) then
       call diagnostics_fill(albvisdr,'ALBVISDR',0,1,3,bi,bj,myid)
      endif

      if(diagnostics_is_on('ALBVISDF',myid) ) then
       call diagnostics_fill(albvisdf,'ALBVISDF',0,1,3,bi,bj,myid)
      endif

      if(diagnostics_is_on('ALBNIRDR',myid) ) then
       call diagnostics_fill(albnirdr,'ALBNIRDR',0,1,3,bi,bj,myid)
      endif

      if(diagnostics_is_on('ALBNIRDF',myid) ) then
       call diagnostics_fill(albnirdf,'ALBNIRDF',0,1,3,bi,bj,myid)
      endif

#endif

C Compute Optical Thicknesses and Diagnostics
C -------------------------------------------
      call opthk(tdry,plz,plze,swlz,cldtot,cldmxo,landtype,im,jm,lm,
     .                                                          tautype)

      do L = 1,lm
      do j = 1,jm
      do i = 1,im
      tau(i,j,L) = tautype(i,j,L,1)+tautype(i,j,L,2)+tautype(i,j,L,3)
      enddo
      enddo
      enddo

#ifdef ALLOW_DIAGNOSTICS
      if(diagnostics_is_on('TAUAVE  ',myid) ) then
       do L=1,lm
       do j=1,jm
       do i=1,im
        tmpdiag(i,j) = tau(i,j,L)*100/(plze(i,j,L+1)-plze(i,j,L))
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'TAUAVE  ',L,1,3,bi,bj,myid)
      enddo
      endif

      if(diagnostics_is_on('TAUCLD  ',myid) .and.
     .                 diagnostics_is_on('TAUCLDC ',myid) ) then
       do L=1,lm
       do j=1,jm
       do i=1,im
        if( cldtot(i,j,L).ne.0.0 ) then
        tmpdiag(i,j) = tau(i,j,L)*100/(plze(i,j,L+1)-plze(i,j,L))
        tmpdiag2(i,j) = 1.
        else
        tmpdiag(i,j) = 0.
        tmpdiag2(i,j) = 0.
        endif
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'TAUCLD  ',L,1,3,bi,bj,myid)
       call diagnostics_fill(tmpdiag,'TAUCLDC ',L,1,3,bi,bj,myid)
      enddo
      endif

c Compute Low, Mid, and High Cloud Optical Depth Diagnostics
c ----------------------------------------------------------
      if(diagnostics_is_on('TAULOW  ',myid) .and.
     .                 diagnostics_is_on('TAULOWC ',myid) ) then
       do j = 1,jm
       do i = 1,im
        taulow(i,j) =  0.0
        if( cldlow(i,j).ne.0.0 ) then
         do L = low_level,lm
          taulow(i,j) = taulow(i,j) + tau(i,j,L)
         enddo
         tmpdiag2(i,j) = 1.
        else
         tmpdiag(i,j) = 0.
        endif
       enddo
       enddo
       call diagnostics_fill(taulow,'TAULOW  ',0,1,3,bi,bj,myid)
       call diagnostics_fill(tmpdiag2,'TAULOWC ',0,1,3,bi,bj,myid)
      endif

      if(diagnostics_is_on('TAUMID  ',myid) .and.
     .                 diagnostics_is_on('TAUMIDC ',myid) ) then
       do j = 1,jm
       do i = 1,im
        taumid(i,j) =  0.0
        if( cldmid(i,j).ne.0.0 ) then
         do L = mid_level,low_level+1
          taumid(i,j) = taumid(i,j) + tau(i,j,L)
         enddo
         tmpdiag2(i,j) = 1.
        else
         tmpdiag(i,j) = 0.
        endif
       enddo
       enddo
       call diagnostics_fill(taumid,'TAUMID  ',0,1,3,bi,bj,myid)
       call diagnostics_fill(tmpdiag2,'TAUMIDC ',0,1,3,bi,bj,myid)
      endif

      if(diagnostics_is_on('TAUHI   ',myid) .and.
     .                 diagnostics_is_on('TAUHIC  ',myid) ) then
       do j = 1,jm
       do i = 1,im
        tauhi(i,j) =  0.0
        if( cldhi(i,j).ne.0.0 ) then
         do L = 1,mid_level+1
          tauhi(i,j) = tauhi(i,j) + tau(i,j,L)
         enddo
         tmpdiag2(i,j) = 1.
        else
         tmpdiag(i,j) = 0.
        endif
       enddo
       enddo
       call diagnostics_fill(tauhi,'TAUHI   ',0,1,3,bi,bj,myid)
       call diagnostics_fill(tmpdiag2,'TAUHIC  ',0,1,3,bi,bj,myid)
      endif
#endif

C***********************************************************************
C ****                     LOOP OVER GLOBAL REGIONS                 ****
C **********************************************************************

      do 1000 nn = 1,npcs

C **********************************************************************
C ****                   VARIABLE INITIALIZATION                    ****
C **********************************************************************

      CALL STRIP ( zenith,COSZ,im*jm,ISTRIP,1,NN )

      CALL STRIP ( plze,  ple   ,im*jm,ISTRIP,lm+1,NN)
      CALL STRIP ( pkz ,  pk    ,im*jm,ISTRIP,lm  ,NN)
      CALL STRIP ( dpres,dpstrip,im*jm,ISTRIP,lm  ,NN)
      CALL STRIP ( tdry,  tzl   ,im*jm,ISTRIP,lm  ,NN)
      CALL STRIP ( qz  ,  qzl   ,im*jm,ISTRIP,lm  ,NN)
      CALL STRIP ( oz  ,  ozl   ,im*jm,ISTRIP,lm  ,NN)
      CALL STRIP ( tau ,  taul  ,im*jm,ISTRIP,lm  ,NN)

      CALL STRIP ( albvisdr,albuvdr,im*jm,ISTRIP,1,NN )
      CALL STRIP ( albvisdf,albuvdf,im*jm,ISTRIP,1,NN )
      CALL STRIP ( albnirdr,albirdr,im*jm,ISTRIP,1,NN )
      CALL STRIP ( albnirdf,albirdf,im*jm,ISTRIP,1,NN )

      call strip ( cldtot,clro,im*jm,istrip,lm,nn )

c Partition Tau between Water and Ice Particles
c ---------------------------------------------
      do L= 1,lm
      do i= 1,istrip
              alf = min( max((tzl(i,l)-253.15)/20.,0. _d 0) ,1. _d 0)
      taua(i,L)   = 0.

      if( alf.ne.0.0 .and. alf.ne.1.0 ) then
      tauc(i,L,1) = taul(i,L)/(1.+alf/(1-alf) * (reffi/reffw*0.80) )
      tauc(i,L,2) = taul(i,L)/(1.+(1-alf)/alf * (reffw/reffi*1.25) )
      endif

      if( alf.eq.0.0 ) then
      tauc(i,L,1) = taul(i,L)
      tauc(i,L,2) = 0.0
      endif

      if( alf.eq.1.0 ) then
      tauc(i,L,1) = 0.0
      tauc(i,L,2) = taul(i,L)
      endif

      reff(i,L,1) = reffi
      reff(i,L,2) = reffw
      enddo
      enddo

      call sorad ( istrip,1,1,lm,ple,tzl,qzl,ozl,co2,
     .             tauc,reff,clro,mid_level,low_level,taua,
     .             albirdr,albirdf,albuvdr,albuvdf,cosz,
     .             flux,fluxclr,fdirir,fdifir,dirpar,difpar,
     .             fdiruv,fdifuv )

C **********************************************************************
C ****     Compute Mass-Weighted Theta Tendencies from Fluxes       ****
C **********************************************************************

      do l=1,lm
      do i=1,istrip
      alf = grav*(ple(i,Lm+1)-ptop)/(cp*dpstrip(i,L)*100)
      dtsw (i,L) = alf*( flux   (i,L)-flux   (i,L+1) )/pk(i,L)
      dtswc(i,L) = alf*( fluxclr(i,L)-fluxclr(i,L+1) )/pk(i,L)
      enddo
      enddo

      call paste ( dtsw , dtradsw ,istrip,im*jm,lm,nn )
      call paste ( dtswc, dtswclr ,istrip,im*jm,lm,nn )

      call paste ( flux   (1,1),osr   ,istrip,im*jm,1,nn )
      call paste ( fluxclr(1,1),osrclr,istrip,im*jm,1,nn )

      call paste ( flux   (1,lm+1),radswg,istrip,im*jm,1,nn )
      call paste ( fluxclr(1,lm+1),swgclr,istrip,im*jm,1,nn )

      call paste ( dirpar,fdirpar,istrip,im*jm,1,nn )
      call paste ( difpar,fdifpar,istrip,im*jm,1,nn )

c Calculate Mean Albedo
c ---------------------
      do i=1,istrip
       if( cosz(i).gt.0.0 ) then
        tstrip(i) = 1.0 - flux(i,lm+1)/
     . ( fdirir(i)+fdifir(i)+dirpar(i)+difpar(i) + fdiruv(i)+fdifuv(i) )
        if( tstrip(i).lt.0.0 ) tstrip(i) = undef
       else
        tstrip(i) = undef
       endif
      enddo
      call paste ( tstrip,albedo,istrip,im*jm,1,nn )

 1000 CONTINUE

#ifdef ALLOW_DIAGNOSTICS

c Mean Albedo Diagnostic
c ----------------------
      if(diagnostics_is_on('ALBEDO  ',myid) .and.
     .                 diagnostics_is_on('ALBEDOC ',myid) ) then
       do j=1,jm
       do i=1,im
        if( albedo(i,j).ne.undef ) then
         tmpdiag(i,j) = albedo(i,j)
         tmpdiag2(i,j) = 1.
        else
         tmpdiag(i,j) = 0.
         tmpdiag2(i,j) = 0.
        endif
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'ALBEDO  ',0,1,3,bi,bj,myid)
       call diagnostics_fill(tmpdiag2,'ALBEDOC ',0,1,3,bi,bj,myid)
      endif
#endif

C **********************************************************************
C ****                 ZERO-OUT OR BUMP COUNTERS                    ****
C **********************************************************************

      nswlz    = 0
      nswcld   = 0
      imstturb = 0

      do L = 1,lm
      do j = 1,jm
      do i = 1,im
         fccave(i,j,L) = 0.
        qliqave(i,j,L) = 0.
      enddo
      enddo
      enddo

      return
      end
      subroutine opthk ( tl,pl,ple,lz,cf,cfm,lwi,im,jm,lm,tau )
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Compute cloud optical thickness using temperature and
C    cloud fractions.
C
C  INPUT:
C  ======
C    tl ......... Temperature at Model Layers (K)
C    pl ......... Model Layer Pressures (mb)
C    ple ........ Model Edge  Pressures (mb)
C    lz ......... Diagnosed Convective and Large-Scale Cloud Water (g/g)
C    cf ......... Total Cloud Fraction (Random + Maximum Overlap)
C    cfm ........ Maximum Overlap Cloud Fraction
C    lwi ........ Surface Land Type
C    im ......... Longitudinal dimension
C    jm ......... Latitudinal  dimension
C    lm ......... Vertical     dimension
C
C  OUTPUT:
C  =======
C    tau ........ Cloud Optical Thickness (non-dimensional)
C                  tau(im,jm,lm,1):  Suspended Ice
C                  tau(im,jm,lm,2):  Suspended Water
C                  tau(im,jm,lm,3):  Raindrops
C
C***********************************************************************

      implicit none

      integer  im,jm,lm,i,j,L

      _RL  tl(im,jm,lm)
      _RL  pl(im,jm,lm)
      _RL ple(im,jm,lm+1)
      _RL  lz(im,jm,lm)
      _RL  cf(im,jm,lm)
      _RL cfm(im,jm,lm)
      _RL tau(im,jm,lm,3)
      integer lwi(im,jm)

      _RL dp, alf, fracls, fraccu
      _RL tauice, tauh2o, tauras

c Compute Cloud Optical Depths
c ----------------------------
      do L=1,lm
      do j=1,jm
      do i=1,im
         alf   =  min( max(( tl(i,j,L)-233.15)/20.,0. _d 0), 1. _d 0)
         dp   =  ple(i,j,L+1)-ple(i,j,L)
         tau(i,j,L,1)  = 0.0
         tau(i,j,L,2)  = 0.0
         tau(i,j,L,3)  = 0.0
      if( cf(i,j,L).ne.0.0 ) then

c Determine fraction of large-scale and cumulus clouds
c ----------------------------------------------------
         fracls = ( cf(i,j,L)-cfm(i,j,L) )/cf(i,j,L)
         fraccu = 1.0-fracls

c Define tau for large-scale ice and water clouds
c Note:  tauice is scaled between (0.02 & .2) for:  0 < lz <  2 mg/kg over Land
c Note:  tauh2o is scaled between (0.20 & 20) for:  0 < lz <  5 mg/kg over Land
c Note:  tauh2o is scaled between (0.20 & 12) for:  0 < lz < 50 mg/kg over Ocean
c -------------------------------------------------------------------------------

c Large-Scale Ice
c ---------------
         tauice = max( 0.0002 _d 0, 0.002*min(500*lz(i,j,L)*1000,
     $        1.0 _d 0) )
         tau(i,j,L,1) = fracls*(1-alf)*tauice*dp

c Large-Scale Water
c -----------------
C Over Land
         if( lwi(i,j).le.10 ) then
            tauh2o = max( 0.0020 _d 0, 0.200*min(200*lz(i,j,L)*1000,
     $           1.0 _d 0) )
          tau(i,j,L,3) = fracls*alf*tauh2o*dp
         else
C Non-Precipitation Clouds Over Ocean
          if( lz(i,j,L).eq.0.0 ) then
           tauh2o = .12
           tau(i,j,L,2) = fracls*alf*tauh2o*dp
          else
C Over Ocean
             tauh2o = max( 0.0020 _d 0, 0.120*min( 20*lz(i,j,L)*1000,
     $            1.0 _d 0) )
           tau(i,j,L,3) = fracls*alf*tauh2o*dp
          endif
         endif

c Sub-Grid Convective
c -------------------
         if( tl(i,j,L).gt.210.0 ) then
         tauras = .16
         tau(i,j,L,3) = tau(i,j,L,3) + fraccu*tauras*dp
         else
         tauras = tauice
         tau(i,j,L,1) = tau(i,j,L,1) + fraccu*tauras*dp
         endif

c Define tau for large-scale and cumulus clouds
c ---------------------------------------------
ccc      tau(i,j,L) = ( fracls*( alf*tauh2o + (1-alf)*tauice )
ccc  .                + fraccu*tauras )*dp

      endif

      enddo
      enddo
      enddo

      return
      end
      subroutine sorad(m,n,ndim,np,pl,ta,wa,oa,co2,
     *                 taucld,reff,fcld,ict,icb,
     *                 taual,rsirbm,rsirdf,rsuvbm,rsuvdf,cosz,
     *                 flx,flc,fdirir,fdifir,fdirpar,fdifpar,
     *                 fdiruv,fdifuv)
c********************************************************************
c
c This routine computes solar fluxes due to the absoption by water
c  vapor, ozone, co2, o2, clouds, and aerosols and due to the
c  scattering by clouds, aerosols, and gases.
c
c This is a vectorized code. It computes the fluxes simultaneous for
c  (m x n) soundings, which is a subset of the (m x ndim) soundings.
c  In a global climate model, m and ndim correspond to the numbers of
c  grid boxes in the zonal and meridional directions, respectively.
c
c Ice and liquid cloud particles are allowed to co-exist in any of the
c  np layers. Two sets of cloud parameters are required as inputs, one
c  for ice paticles and the other for liquid particles.  These parameters
c  are optical thickness (taucld) and effective particle size (reff).
c
c If no information is available for reff, a default value of
c  10 micron for liquid water and 75 micron for ice can be used.
c
c Clouds are grouped into high, middle, and low clouds separated by the
c  level indices ict and icb.  For detail, see the subroutine cldscale.
c
c----- Input parameters:
c                                                   units      size
c   number of soundings in zonal direction (m)       n/d        1
c   number of soundings in meridional direction (n)  n/d        1
c   maximum number of soundings in                   n/d        1
c           meridional direction (ndim)
c   number of atmospheric layers (np)                n/d        1
c   level pressure (pl)                              mb       m*ndim*(np+1)
c   layer temperature (ta)                           k        m*ndim*np
c   layer specific humidity (wa)                     gm/gm    m*ndim*np
c   layer ozone concentration (oa)                   gm/gm    m*ndim*np
c   co2 mixing ratio by volumn (co2)               parts/part   1
c   cloud optical thickness (taucld)                 n/d      m*ndim*np*2
c                index 1 for ice particles
c                index 2 for liquid drops
c   effective cloud-particle size (reff)           micrometer m*ndim*np*2
c                index 1 for ice particles
c                index 2 for liquid drops
c   cloud amount (fcld)                            fraction   m*ndim*np
c   level index separating high and middle           n/d        1
c                clouds (ict)
c   level index separating middle and low clouds     n/d        1
c                clouds (icb)
c   aerosol optical thickness (taual)                n/d      m*ndim*np
c   solar ir surface albedo for beam                fraction   m*ndim
c                radiation (rsirbm)
c   solar ir surface albedo for diffuse             fraction   m*ndim
c                radiation (rsirdf)
c   uv + par surface albedo for beam                     fraction   m*ndim
c                radiation (rsuvbm)
c   uv + par surface albedo for diffuse                  fraction   m*ndim
c                radiation (rsuvdf)
c   cosine of solar zenith angle (cosz)            n/d        m*ndim
c
c----- Output parameters
c
c   all-sky flux (downward minus upward) (flx)     fraction   m*ndim*(np+1)
c   clear-sky flux (downward minus upward) (flc)   fraction   m*ndim*(np+1)
c   all-sky direct downward ir (0.7-10 micron)
c                flux at the surface (fdirir)      fraction   m*ndim
c   all-sky diffuse downward ir flux at
c                the surface (fdifir)              fraction   m*ndim
c   all-sky direct downward par (0.4-0.7 micron)
c                flux at the surface (fdirpar)     fraction   m*ndim
c   all-sky diffuse downward par flux at
c                the surface (fdifpar)             fraction   m*ndim
*
c----- Notes:
c
c    (1) The unit of flux is fraction of the incoming solar radiation
c        at the top of the atmosphere.  Therefore, fluxes should
c        be equal to flux multiplied by the extra-terrestrial solar
c        flux and the cosine of solar zenith angle.
c    (2) Clouds and aerosols can be included in any layers by specifying
c        fcld(i,j,k), taucld(i,j,k,*) and taual(i,j,k), k=1,np.
c        For an atmosphere without clouds and aerosols,
c        set fcld(i,j,k)=taucld(i,j,k,*)=taual(i,j,k)=0.0.
c    (3) Aerosol single scattering albedos and asymmetry
c        factors are specified in the subroutines solir and soluv.
c    (4) pl(i,j,1) is the pressure at the top of the model, and
c        pl(i,j,np+1) is the surface pressure.
c    (5) the pressure levels ict and icb correspond approximately
c        to 400 and 700 mb.
c
c**************************************************************************

      implicit none

c-----Explicit Inline Directives

#ifdef CRAY
#ifdef f77
cfpp$ expand (expmn)
#endif
#endif
      _RL expmn

c-----input parameters

      integer m,n,ndim,np,ict,icb
      _RL pl(m,ndim,np+1),ta(m,ndim,np),wa(m,ndim,np),oa(m,ndim,np)
      _RL  taucld(m,ndim,np,2),reff(m,ndim,np,2)
      _RL  fcld(m,ndim,np),taual(m,ndim,np)
      _RL  rsirbm(m,ndim),rsirdf(m,ndim),
     *     rsuvbm(m,ndim),rsuvdf(m,ndim),cosz(m,ndim),co2

c-----output parameters

      _RL  flx(m,ndim,np+1),flc(m,ndim,np+1)
      _RL  fdirir(m,ndim),fdifir(m,ndim)
      _RL  fdirpar(m,ndim),fdifpar(m,ndim)
      _RL  fdiruv(m,ndim),fdifuv(m,ndim)

c-----temporary array

      integer i,j,k
      _RL  cc(m,n,3),tauclb(m,n,np),tauclf(m,n,np)
      _RL  dp(m,n,np),wh(m,n,np),oh(m,n,np),scal(m,n,np)
      _RL  swh(m,n,np+1),so2(m,n,np+1),df(m,n,np+1)
      _RL  sdf(m,n),sclr(m,n),csm(m,n),x

c-----------------------------------------------------------------

      do j= 1, n
       do i= 1, m

         swh(i,j,1)=0.
         so2(i,j,1)=0.

c-----csm is the effective secant of the solar zenith angle
c     see equation (12) of Lacis and Hansen (1974, JAS)

         csm(i,j)=35./sqrt(1224.*cosz(i,j)*cosz(i,j)+1.)

       enddo
      enddo


      do k= 1, np
       do j= 1, n
         do i= 1, m

c-----compute layer thickness and pressure-scaling function.
c     indices for the surface level and surface layer
c     are np+1 and np, respectively.

          dp(i,j,k)=pl(i,j,k+1)-pl(i,j,k)
          scal(i,j,k)=dp(i,j,k)*(.5*(pl(i,j,k)+pl(i,j,k+1))/300.)**.8

c-----compute scaled water vapor amount, unit is g/cm**2

          wh(i,j,k)=1.02*wa(i,j,k)*scal(i,j,k)*
     *              (1.+0.00135*(ta(i,j,k)-240.))
          swh(i,j,k+1)=swh(i,j,k)+wh(i,j,k)

c-----compute ozone amount, unit is (cm-atm)stp.

          oh(i,j,k)=1.02*oa(i,j,k)*dp(i,j,k)*466.7

        enddo
       enddo
      enddo


c-----scale cloud optical thickness in each layer from taucld (with
c     cloud amount fcld) to tauclb and tauclf (with cloud amount cc).
c     tauclb is the scaled optical thickness for beam radiation and
c     tauclf is for diffuse radiation.

      call cldscale(m,n,ndim,np,cosz,fcld,taucld,ict,icb,
     *              cc,tauclb,tauclf)

c-----initialize fluxes for all-sky (flx), clear-sky (flc), and
c     flux reduction (df)

      do k=1, np+1
       do j=1, n
        do i=1, m
          flx(i,j,k)=0.
          flc(i,j,k)=0.
          df(i,j,k)=0.
        enddo
       enddo
      enddo

c-----compute solar ir fluxes

      call solir (m,n,ndim,np,wh,taucld,tauclb,tauclf,reff,ict,icb
     *     ,fcld,cc,taual,csm,rsirbm,rsirdf,flx,flc,fdirir,fdifir)

c-----compute and update uv and par fluxes

      call soluv (m,n,ndim,np,oh,dp,taucld,tauclb,tauclf,reff,ict,icb
     *     ,fcld,cc,taual,csm,rsuvbm,rsuvdf,flx,flc
     *     ,fdirpar,fdifpar,fdiruv,fdifuv)

c-----compute scaled amount of o2 (so2), unit is (cm-atm)stp.

      do k= 1, np
       do j= 1, n
        do i= 1, m
          so2(i,j,k+1)=so2(i,j,k)+165.22*scal(i,j,k)
        enddo
       enddo
      enddo

c-----compute flux reduction due to oxygen following
c      chou (J. climate, 1990). The fraction 0.0287 is the
c      extraterrestrial solar flux in the o2 bands.

       do k= 2, np+1
        do j= 1, n
         do i= 1, m
           x=so2(i,j,k)*csm(i,j)
           df(i,j,k)=df(i,j,k)+0.0287*(1.-expmn(-0.00027*sqrt(x)))
         enddo
        enddo
       enddo

c-----compute scaled amounts for co2 (so2). unit is (cm-atm)stp.

      do k= 1, np
       do j= 1, n
        do i= 1, m
         so2(i,j,k+1)=so2(i,j,k)+co2*789.*scal(i,j,k)
        enddo
       enddo
      enddo

c-----compute and update flux reduction due to co2 following
c     chou (J. Climate, 1990)

      call flxco2(m,n,np,so2,swh,csm,df)

c-----adjust for the effect of o2 cnd co2 on clear-sky fluxes.

      do k= 2, np+1
       do j= 1, n
        do i= 1, m
          flc(i,j,k)=flc(i,j,k)-df(i,j,k)
        enddo
       enddo
      enddo

c-----adjust for the all-sky fluxes due to o2 and co2.  It is
c     assumed that o2 and co2 have no effects on solar radiation
c     below clouds. clouds are assumed randomly overlapped.

      do j=1,n
       do i=1,m
        sdf(i,j)=0.0
        sclr(i,j)=1.0
       enddo
      enddo

      do k=1,np
       do j=1,n
        do i=1,m

c-----sclr is the fraction of clear sky.
c     sdf is the flux reduction below clouds.

         if(fcld(i,j,k).gt.0.01) then
          sdf(i,j)=sdf(i,j)+df(i,j,k)*sclr(i,j)*fcld(i,j,k)
          sclr(i,j)=sclr(i,j)*(1.-fcld(i,j,k))
         endif
          flx(i,j,k+1)=flx(i,j,k+1)-sdf(i,j)-df(i,j,k+1)*sclr(i,j)

        enddo
       enddo
      enddo

c-----adjust for the direct downward ir flux.
      do j= 1, n
       do i= 1, m
         x = (1.-rsirdf(i,j))*fdifir(i,j) + (1.-rsirbm(i,j))*fdirir(i,j)
         x = (-sdf(i,j)-df(i,j,np+1)*sclr(i,j))/x
         fdirir(i,j)=fdirir(i,j)*(1.+x)
         fdifir(i,j)=fdifir(i,j)*(1.+x)
       enddo
      enddo

      return
      end

c********************************************************************

      subroutine cldscale (m,n,ndim,np,cosz,fcld,taucld,ict,icb,
     *                     cc,tauclb,tauclf)

c********************************************************************
c
c   This subroutine computes the covers of high, middle, and
c    low clouds and scales the cloud optical thickness.
c
c   To simplify calculations in a cloudy atmosphere, clouds are
c    grouped into high, middle and low clouds separated by the levels
c    ict and icb (level 1 is the top of the atmosphere).
c
c   Within each of the three groups, clouds are assumed maximally
c    overlapped, and the cloud cover (cc) of a group is the maximum
c    cloud cover of all the layers in the group.  The optical thickness
c    (taucld) of a given layer is then scaled to new values (tauclb and
c    tauclf) so that the layer reflectance corresponding to the cloud
c    cover cc is the same as the original reflectance with optical
c    thickness taucld and cloud cover fcld.
c
c---input parameters
c
c    number of grid intervals in zonal direction (m)
c    number of grid intervals in meridional direction (n)
c    maximum number of grid intervals in meridional direction (ndim)
c    number of atmospheric layers (np)
c    cosine of the solar zenith angle (cosz)
c    fractional cloud cover (fcld)
c    cloud optical thickness (taucld)
c    index separating high and middle clouds (ict)
c    index separating middle and low clouds (icb)
c
c---output parameters
c
c    fractional cover of high, middle, and low clouds (cc)
c    scaled cloud optical thickness for beam radiation (tauclb)
c    scaled cloud optical thickness for diffuse radiation (tauclf)
c
c********************************************************************

      implicit none

c-----input parameters

      integer m,n,ndim,np,ict,icb
      _RL  cosz(m,ndim),fcld(m,ndim,np),taucld(m,ndim,np,2)

c-----output parameters

      _RL  cc(m,n,3),tauclb(m,n,np),tauclf(m,n,np)

c-----temporary variables

      integer i,j,k,im,it,ia,kk
      _RL   fm,ft,fa,xai,taux

c-----pre-computed table

      integer   nm,nt,na
      parameter (nm=11,nt=9,na=11)
      _RL   dm,dt,da,t1,caib(nm,nt,na),caif(nt,na)
      parameter (dm=0.1,dt=0.30103,da=0.1,t1=-0.9031)

c-----include the pre-computed table for cai

#include "cai-dat.h"
      save caib,caif


c-----clouds within each of the high, middle, and low clouds are
c     assumed maximally overlapped, and the cloud cover (cc)
c     for a group is the maximum cloud cover of all the layers
c     in the group

      do j=1,n
       do i=1,m
          cc(i,j,1)=0.0
          cc(i,j,2)=0.0
          cc(i,j,3)=0.0
       enddo
      enddo

      do k=1,ict-1
       do j=1,n
        do i=1,m
           cc(i,j,1)=max(cc(i,j,1),fcld(i,j,k))
        enddo
       enddo
      enddo

      do k=ict,icb-1
       do j=1,n
        do i=1,m
           cc(i,j,2)=max(cc(i,j,2),fcld(i,j,k))
        enddo
       enddo
      enddo

      do k=icb,np
       do j=1,n
        do i=1,m
           cc(i,j,3)=max(cc(i,j,3),fcld(i,j,k))
        enddo
       enddo
      enddo

c-----scale the cloud optical thickness.
c     taucld(i,j,k,1) is the optical thickness for ice particles, and
c     taucld(i,j,k,2) is the optical thickness for liquid particles.

      do k=1,np

         if(k.lt.ict) then
            kk=1
         elseif(k.ge.ict .and. k.lt.icb) then
            kk=2
         else
            kk=3
         endif

       do j=1,n
        do i=1,m

         tauclb(i,j,k) = 0.0
         tauclf(i,j,k) = 0.0

         taux=taucld(i,j,k,1)+taucld(i,j,k,2)
         if (taux.gt.0.05 .and. fcld(i,j,k).gt.0.01) then

c-----normalize cloud cover

           fa=fcld(i,j,k)/cc(i,j,kk)

c-----table look-up

           taux=min(taux,32. _d 0)

           fm=cosz(i,j)/dm
           ft=(log10(taux)-t1)/dt
           fa=fa/da

           im=int(fm+1.5)
           it=int(ft+1.5)
           ia=int(fa+1.5)

           im=max(im,2)
           it=max(it,2)
           ia=max(ia,2)

           im=min(im,nm-1)
           it=min(it,nt-1)
           ia=min(ia,na-1)

           fm=fm-float(im-1)
           ft=ft-float(it-1)
           fa=fa-float(ia-1)

c-----scale cloud optical thickness for beam radiation.
c     the scaling factor, xai, is a function of the solar zenith
c     angle, optical thickness, and cloud cover.

           xai=    (-caib(im-1,it,ia)*(1.-fm)+
     *      caib(im+1,it,ia)*(1.+fm))*fm*.5+caib(im,it,ia)*(1.-fm*fm)

           xai=xai+(-caib(im,it-1,ia)*(1.-ft)+
     *      caib(im,it+1,ia)*(1.+ft))*ft*.5+caib(im,it,ia)*(1.-ft*ft)

           xai=xai+(-caib(im,it,ia-1)*(1.-fa)+
     *     caib(im,it,ia+1)*(1.+fa))*fa*.5+caib(im,it,ia)*(1.-fa*fa)

           xai= xai-2.*caib(im,it,ia)
           xai=max(xai,0.0 _d 0)

           tauclb(i,j,k) = taux*xai

c-----scale cloud optical thickness for diffuse radiation.
c     the scaling factor, xai, is a function of the cloud optical
c     thickness and cover but not the solar zenith angle.

           xai=    (-caif(it-1,ia)*(1.-ft)+
     *      caif(it+1,ia)*(1.+ft))*ft*.5+caif(it,ia)*(1.-ft*ft)

           xai=xai+(-caif(it,ia-1)*(1.-fa)+
     *      caif(it,ia+1)*(1.+fa))*fa*.5+caif(it,ia)*(1.-fa*fa)

           xai= xai-caif(it,ia)
           xai=max(xai,0.0 _d 0)

           tauclf(i,j,k) = taux*xai

         endif

        enddo
       enddo
      enddo

      return
      end
c***********************************************************************

      subroutine solir (m,n,ndim,np,wh,taucld,tauclb,tauclf,reff,
     *  ict,icb,fcld,cc,taual,csm,rsirbm,rsirdf,flx,flc,fdirir,fdifir)

c************************************************************************
c  compute solar flux in the infrared region. The spectrum is divided
c   into three bands:
c
c          band   wavenumber(/cm)  wavelength (micron)
c           1       1000-4400         2.27-10.0
c           2       4400-8200         1.22-2.27
c           3       8200-14300        0.70-1.22
c
c----- Input parameters:                            units      size
c
c   number of soundings in zonal direction (m)       n/d        1
c   number of soundings in meridional direction (n)  n/d        1
c   maximum number of soundings in                   n/d        1
c          meridional direction (ndim)
c   number of atmospheric layers (np)                n/d        1
c   layer water vapor content (wh)                 gm/cm^2    m*n*np
c   cloud optical thickness (taucld)                 n/d      m*ndim*np*2
c          index 1 for ice paticles
c          index 2 for liquid particles
c   scaled cloud optical thickness                   n/d      m*n*np
c          for beam radiation (tauclb)
c   scaled cloud optical thickness                   n/d      m*n*np
c          for diffuse radiation  (tauclf)
c   effective cloud-particle size (reff)           micrometer m*ndim*np*2
c          index 1 for ice paticles
c          index 2 for liquid particles
c   level index separating high and                  n/d      m*n
c          middle clouds (ict)
c   level index separating middle and                n/d      m*n
c          low clouds (icb)
c   cloud amount (fcld)                            fraction   m*ndim*np
c   cloud amount of high, middle, and                n/d      m*n*3
c          low clouds (cc)
c   aerosol optical thickness (taual)                n/d      m*ndim*np
c   cosecant of the solar zenith angle (csm)         n/d      m*n
c   near ir surface albedo for beam                fraction   m*ndim
c                radiation (rsirbm)
c   near ir surface albedo for diffuse             fraction   m*ndim
c                radiation (rsirdf)
c
c----- output (updated) parameters:
c
c   all-sky flux (downward-upward) (flx)           fraction   m*ndim*(np+1)
c   clear-sky flux (downward-upward) (flc)         fraction   m*ndim*(np+1)
c   all-sky direct downward ir flux at
c          the surface (fdirir)                    fraction   m*ndim
c   all-sky diffuse downward ir flux at
c          the surface (fdifir)                    fraction   m*ndim
c
c----- note: the following parameters must be specified by users:
c   aerosol single scattering albedo (ssaal)         n/d      nband
c   aerosol asymmetry factor (asyal)                 n/d      nband
c
c*************************************************************************

      implicit none

c-----Explicit Inline Directives

#ifdef CRAY
#ifdef f77
cfpp$ expand (deledd)
cfpp$ expand (sagpol)
cfpp$ expand (expmn)
#endif
#endif
      _RL expmn

c-----input parameters

      integer m,n,ndim,np,ict,icb
      _RL  taucld(m,ndim,np,2),reff(m,ndim,np,2),fcld(m,ndim,np)
      _RL  tauclb(m,n,np),tauclf(m,n,np),cc(m,n,3)
      _RL  rsirbm(m,ndim),rsirdf(m,ndim)
      _RL  wh(m,n,np),taual(m,ndim,np),csm(m,n)

c-----output (updated) parameters

      _RL  flx(m,ndim,np+1),flc(m,ndim,np+1)
      _RL  fdirir(m,ndim),fdifir(m,ndim)

c-----static parameters

      integer nk,nband
      parameter (nk=10,nband=3)
      _RL  xk(nk),hk(nband,nk),ssaal(nband),asyal(nband)
      _RL  aia(nband,3),awa(nband,3),aig(nband,3),awg(nband,3)

c-----temporary array

      integer ib,ik,i,j,k
      _RL  ssacl(m,n,np),asycl(m,n,np)
      _RL  rr(m,n,np+1,2),tt(m,n,np+1,2),td(m,n,np+1,2),
     *       rs(m,n,np+1,2),ts(m,n,np+1,2)
      _RL  fall(m,n,np+1),fclr(m,n,np+1)
      _RL  fsdir(m,n),fsdif(m,n)

      _RL  tauwv,tausto,ssatau,asysto,tauto,ssato,asyto
      _RL  taux,reff1,reff2,w1,w2,g1,g2
      _RL  ssaclt(m,n),asyclt(m,n)
      _RL  rr1t(m,n),tt1t(m,n),td1t(m,n),rs1t(m,n),ts1t(m,n)
      _RL  rr2t(m,n),tt2t(m,n),td2t(m,n),rs2t(m,n),ts2t(m,n)

c-----water vapor absorption coefficient for 10 k-intervals.
c     unit: cm^2/gm

      data xk/
     1  0.0010, 0.0133, 0.0422, 0.1334, 0.4217,
     2  1.334,  5.623,  31.62,  177.8,  1000.0/

c-----water vapor k-distribution function,
c     the sum of hk is 0.52926. unit: fraction

      data hk/
     1 .01074,.08236,.20673,  .00360,.01157,.03497,
     2 .00411,.01133,.03011,  .00421,.01143,.02260,
     3 .00389,.01240,.01336,  .00326,.01258,.00696,
     4 .00499,.01381,.00441,  .00465,.00650,.00115,
     5 .00245,.00244,.00026,  .00145,.00094,.00000/

c-----aerosol single-scattering albedo and asymmetry factor

      data ssaal,asyal/nband*0.999,nband*0.850/

c-----coefficients for computing the single scattering albedo of
c     ice clouds from ssa=1-(aia(*,1)+aia(*,2)*reff+aia(*,3)*reff**2)

      data aia/
     1  .08938331, .00215346,-.00000260,
     2  .00299387, .00073709, .00000746,
     3 -.00001038,-.00000134, .00000000/

c-----coefficients for computing the single scattering albedo of
c     liquid clouds from ssa=1-(awa(*,1)+awa(*,2)*reff+awa(*,3)*reff**2)

      data awa/
     1  .01209318,-.00019934, .00000007,
     2  .01784739, .00088757, .00000845,
     3 -.00036910,-.00000650,-.00000004/

c-----coefficients for computing the asymmetry factor of ice clouds
c     from asycl=aig(*,1)+aig(*,2)*reff+aig(*,3)*reff**2

      data aig/
     1  .84090400, .76098937, .74935228,
     2  .00126222, .00141864, .00119715,
     3 -.00000385,-.00000396,-.00000367/

c-----coefficients for computing the asymmetry factor of liquid clouds
c     from asycl=awg(*,1)+awg(*,2)*reff+awg(*,3)*reff**2

      data awg/
     1  .83530748, .74513197, .79375035,
     2  .00257181, .01370071, .00832441,
     3  .00005519,-.00038203,-.00023263/

c-----initialize surface fluxes, reflectances, and transmittances

      do j= 1, n
       do i= 1, m
         fdirir(i,j)=0.0
         fdifir(i,j)=0.0
         rr(i,j,np+1,1)=rsirbm(i,j)
         rr(i,j,np+1,2)=rsirbm(i,j)
         rs(i,j,np+1,1)=rsirdf(i,j)
         rs(i,j,np+1,2)=rsirdf(i,j)
         td(i,j,np+1,1)=0.0
         td(i,j,np+1,2)=0.0
         tt(i,j,np+1,1)=0.0
         tt(i,j,np+1,2)=0.0
         ts(i,j,np+1,1)=0.0
         ts(i,j,np+1,2)=0.0
       enddo
      enddo

c-----integration over spectral bands

      do 100 ib=1,nband

c-----compute cloud single scattering albedo and asymmetry factor
c     for a mixture of ice and liquid particles.

       do k= 1, np

        do j= 1, n
         do i= 1, m

           ssaclt(i,j)=1.0
           asyclt(i,j)=1.0

           taux=taucld(i,j,k,1)+taucld(i,j,k,2)
          if (taux.gt.0.05 .and. fcld(i,j,k).gt.0.01) then

           reff1=min(reff(i,j,k,1),130. _d 0)
           reff2=min(reff(i,j,k,2),20.0 _d 0)

           w1=(1.-(aia(ib,1)+(aia(ib,2)+
     *         aia(ib,3)*reff1)*reff1))*taucld(i,j,k,1)
           w2=(1.-(awa(ib,1)+(awa(ib,2)+
     *         awa(ib,3)*reff2)*reff2))*taucld(i,j,k,2)
           ssaclt(i,j)=(w1+w2)/taux

           g1=(aig(ib,1)+(aig(ib,2)+aig(ib,3)*reff1)*reff1)*w1
           g2=(awg(ib,1)+(awg(ib,2)+awg(ib,3)*reff2)*reff2)*w2
           asyclt(i,j)=(g1+g2)/(w1+w2)

          endif

         enddo
        enddo

        do j=1,n
         do i=1,m
            ssacl(i,j,k)=ssaclt(i,j)
         enddo
        enddo
        do j=1,n
         do i=1,m
            asycl(i,j,k)=asyclt(i,j)
         enddo
        enddo

       enddo

c-----integration over the k-distribution function

         do 200 ik=1,nk

          do 300 k= 1, np

           do j= 1, n
            do i= 1, m

             tauwv=xk(ik)*wh(i,j,k)

c-----compute total optical thickness, single scattering albedo,
c     and asymmetry factor.

             tausto=tauwv+taual(i,j,k)+1.0e-8
             ssatau=ssaal(ib)*taual(i,j,k)
             asysto=asyal(ib)*ssaal(ib)*taual(i,j,k)

c-----compute reflectance and transmittance for cloudless layers

             tauto=tausto
             ssato=ssatau/tauto+1.0e-8

c            if (ssato .gt. 0.001) then

c             ssato=min(ssato,0.999999 _d 0)
c             asyto=asysto/(ssato*tauto)

c             call deledd(tauto,ssato,asyto,csm(i,j),
c     *                   rr1t(i,j),tt1t(i,j),td1t(i,j))

c             call sagpol (tauto,ssato,asyto,rs1t(i,j),ts1t(i,j))

c            else

             td1t(i,j)=expmn(-tauto*csm(i,j))
             ts1t(i,j)=expmn(-1.66*tauto)
             tt1t(i,j)=0.0
             rr1t(i,j)=0.0
             rs1t(i,j)=0.0

c            endif

c-----compute reflectance and transmittance for cloud layers

             if (tauclb(i,j,k) .lt. 0.01) then

              rr2t(i,j)=rr1t(i,j)
              tt2t(i,j)=tt1t(i,j)
              td2t(i,j)=td1t(i,j)
              rs2t(i,j)=rs1t(i,j)
              ts2t(i,j)=ts1t(i,j)

             else

              tauto=tausto+tauclb(i,j,k)
              ssato=(ssatau+ssacl(i,j,k)*tauclb(i,j,k))/tauto+1.0e-8
              ssato=min(ssato,0.999999 _d 0)
              asyto=(asysto+asycl(i,j,k)*ssacl(i,j,k)*tauclb(i,j,k))/
     *              (ssato*tauto)

              call deledd(tauto,ssato,asyto,csm(i,j),
     *                    rr2t(i,j),tt2t(i,j),td2t(i,j))

              tauto=tausto+tauclf(i,j,k)
              ssato=(ssatau+ssacl(i,j,k)*tauclf(i,j,k))/tauto+1.0e-8
              ssato=min(ssato,0.999999 _d 0)
              asyto=(asysto+asycl(i,j,k)*ssacl(i,j,k)*tauclf(i,j,k))/
     *              (ssato*tauto)

              call sagpol (tauto,ssato,asyto,rs2t(i,j),ts2t(i,j))

             endif

            enddo
           enddo

c-----the following code appears to be awkward, but it is efficient
c     in certain parallel processors.

           do j=1,n
            do i=1,m
               rr(i,j,k,1)=rr1t(i,j)
            enddo
           enddo
           do j=1,n
            do i=1,m
               tt(i,j,k,1)=tt1t(i,j)
            enddo
           enddo
           do j=1,n
            do i=1,m
               td(i,j,k,1)=td1t(i,j)
            enddo
           enddo
           do j=1,n
            do i=1,m
               rs(i,j,k,1)=rs1t(i,j)
            enddo
           enddo
           do j=1,n
            do i=1,m
               ts(i,j,k,1)=ts1t(i,j)
            enddo
           enddo

           do j=1,n
            do i=1,m
               rr(i,j,k,2)=rr2t(i,j)
            enddo
           enddo
           do j=1,n
            do i=1,m
               tt(i,j,k,2)=tt2t(i,j)
            enddo
           enddo
           do j=1,n
            do i=1,m
               td(i,j,k,2)=td2t(i,j)
            enddo
           enddo
           do j=1,n
            do i=1,m
               rs(i,j,k,2)=rs2t(i,j)
            enddo
           enddo
           do j=1,n
            do i=1,m
               ts(i,j,k,2)=ts2t(i,j)
            enddo
           enddo

 300  continue

c-----flux calculations

       do k= 1, np+1
        do j= 1, n
         do i= 1, m
          fclr(i,j,k) = 0.
          fall(i,j,k) = 0.
         enddo
        enddo
       enddo
       do j= 1, n
        do i= 1, m
         fsdir(i,j) = 0.
         fsdif(i,j) = 0.
        enddo
       enddo

        call cldflx (m,n,np,ict,icb,cc,rr,tt,td,rs,ts,
     *               fclr,fall,fsdir,fsdif)

       do k= 1, np+1
        do j= 1, n
         do i= 1, m
          flx(i,j,k) = flx(i,j,k)+fall(i,j,k)*hk(ib,ik)
         enddo
        enddo
        do j= 1, n
         do i= 1, m
          flc(i,j,k) = flc(i,j,k)+fclr(i,j,k)*hk(ib,ik)
         enddo
        enddo
       enddo

       do j= 1, n
        do i= 1, m
          fdirir(i,j) = fdirir(i,j)+fsdir(i,j)*hk(ib,ik)
          fdifir(i,j) = fdifir(i,j)+fsdif(i,j)*hk(ib,ik)
        enddo
       enddo

  200 continue
  100 continue

      return
      end

c************************************************************************

      subroutine soluv (m,n,ndim,np,oh,dp,taucld,tauclb,tauclf,reff,
     *  ict,icb,fcld,cc,taual,csm,rsuvbm,rsuvdf,flx,flc
     * ,fdirpar,fdifpar,fdiruv,fdifuv)

c************************************************************************
c  compute solar fluxes in the uv+visible region. the spectrum is
c  grouped into 8 bands:
c
c              Band     Micrometer
c
c       UV-C    1.     .175 - .225
c               2.     .225 - .245
c                      .260 - .280
c               3.     .245 - .260
c
c       UV-B    4.     .280 - .295
c               5.     .295 - .310
c               6.     .310 - .320
c
c       UV-A    7.     .320 - .400
c
c       PAR     8.     .400 - .700
c
c----- Input parameters:                            units      size
c
c   number of soundings in zonal direction (m)       n/d        1
c   number of soundings in meridional direction (n)  n/d        1
c   maximum number of soundings in                   n/d        1
c           meridional direction (ndim)
c   number of atmospheric layers (np)                n/d        1
c   layer ozone content (oh)                      (cm-atm)stp m*n*np
c   layer pressure thickness (dp)                    mb       m*n*np
c   cloud optical thickness (taucld)                 n/d      m*ndim*np*2
c          index 1 for ice paticles
c          index 2 for liquid particles
c   scaled cloud optical thickness                   n/d      m*n*np
c          for beam radiation (tauclb)
c   scaled cloud optical thickness                   n/d      m*n*np
c          for diffuse radiation  (tauclf)
c   effective cloud-particle size (reff)           micrometer m*ndim*np*2
c          index 1 for ice paticles
c          index 2 for liquid particles
c   level indiex separating high and                 n/d      m*n
c          middle clouds (ict)
c   level indiex separating middle and               n/d      m*n
c          low clouds (icb)
c   cloud amount (fcld)                            fraction   m*ndim*np
c   cloud amounts of high, middle, and               n/d      m*n*3
c          low clouds (cc)
c   aerosol optical thickness (taual)                n/d      m*ndim*np
c   cosecant of the solar zenith angle (csm)         n/d      m*n
c   uv+par surface albedo for beam                 fraction   m*ndim
c           radiation (rsuvbm)
c   uv+par surface albedo for diffuse              fraction   m*ndim
c           radiation (rsuvdf)
c
c----- output (updated) parameters:
c
c   all-sky net downward flux (flx)                fraction   m*ndim*(np+1)
c   clear-sky net downward flux (flc)              fraction   m*ndim*(np+1)
c   all-sky direct downward par flux at
c          the surface (fdirpar)                   fraction   m*ndim
c   all-sky diffuse downward par flux at
c          the surface (fdifpar)                   fraction   m*ndim
c
c----- note: the following parameters must be specified by users:
c
c   aerosol single scattering albedo (ssaal)         n/d        1
c   aerosol asymmetry factor (asyal)                 n/d        1
c
*
c***********************************************************************

      implicit none

c-----Explicit Inline Directives

#ifdef CRAY
#ifdef f77
cfpp$ expand (deledd)
cfpp$ expand (sagpol)
#endif
#endif

c-----input parameters

      integer m,n,ndim,np,ict,icb
      _RL  taucld(m,ndim,np,2),reff(m,ndim,np,2),fcld(m,ndim,np)
      _RL  tauclb(m,n,np),tauclf(m,n,np),cc(m,n,3)
      _RL  oh(m,n,np),dp(m,n,np),taual(m,ndim,np)
      _RL  rsuvbm(m,ndim),rsuvdf(m,ndim),csm(m,n)

c-----output (updated) parameter

      _RL  flx(m,ndim,np+1),flc(m,ndim,np+1)
      _RL  fdirpar(m,ndim),fdifpar(m,ndim)
      _RL  fdiruv(m,ndim),fdifuv(m,ndim)

c-----static parameters

      integer nband
      parameter (nband=8)
      _RL  hk(nband),xk(nband),ry(nband)
      _RL  asyal(nband),ssaal(nband),aig(3),awg(3)

c-----temporary array

      integer i,j,k,ib
      _RL  taurs,tauoz,tausto,ssatau,asysto,tauto,ssato,asyto
      _RL  taux,reff1,reff2,g1,g2,asycl(m,n,np)
      _RL  td(m,n,np+1,2),rr(m,n,np+1,2),tt(m,n,np+1,2),
     *       rs(m,n,np+1,2),ts(m,n,np+1,2)
      _RL  fall(m,n,np+1),fclr(m,n,np+1),fsdir(m,n),fsdif(m,n)
      _RL  asyclt(m,n)
      _RL  rr1t(m,n),tt1t(m,n),td1t(m,n),rs1t(m,n),ts1t(m,n)
      _RL  rr2t(m,n),tt2t(m,n),td2t(m,n),rs2t(m,n),ts2t(m,n)

c-----hk is the fractional extra-terrestrial solar flux.
c     the sum of hk is 0.47074.

      data hk/.00057, .00367, .00083, .00417,
     *        .00600, .00556, .05913, .39081/

c-----xk is the ozone absorption coefficient. unit: /(cm-atm)stp

      data xk /30.47, 187.2,  301.9,   42.83,
     *         7.09,  1.25,   0.0345,  0.0539/

c-----ry is the extinction coefficient for Rayleigh scattering.
c     unit: /mb.

      data ry /.00604, .00170, .00222, .00132,
     *         .00107, .00091, .00055, .00012/

c-----aerosol single-scattering albedo and asymmetry factor

      data ssaal,asyal/nband*0.999,nband*0.850/

c-----coefficients for computing the asymmetry factor of ice clouds
c     from asycl=aig(*,1)+aig(*,2)*reff+aig(*,3)*reff**2

      data aig/.74625000,.00105410,-.00000264/

c-----coefficients for computing the asymmetry factor of liquid
c     clouds from asycl=awg(*,1)+awg(*,2)*reff+awg(*,3)*reff**2

      data awg/.82562000,.00529000,-.00014866/

c-----initialize surface reflectances and transmittances

      do j= 1, n
       do i= 1, m
         rr(i,j,np+1,1)=rsuvbm(i,j)
         rr(i,j,np+1,2)=rsuvbm(i,j)
         rs(i,j,np+1,1)=rsuvdf(i,j)
         rs(i,j,np+1,2)=rsuvdf(i,j)
         td(i,j,np+1,1)=0.0
         td(i,j,np+1,2)=0.0
         tt(i,j,np+1,1)=0.0
         tt(i,j,np+1,2)=0.0
         ts(i,j,np+1,1)=0.0
         ts(i,j,np+1,2)=0.0
       enddo
      enddo

c-----compute cloud asymmetry factor for a mixture of
c     liquid and ice particles.  unit of reff is micrometers.

      do k= 1, np

       do j= 1, n
        do i= 1, m

           asyclt(i,j)=1.0

           taux=taucld(i,j,k,1)+taucld(i,j,k,2)
          if (taux.gt.0.05 .and. fcld(i,j,k).gt.0.01) then

           reff1=min(reff(i,j,k,1),130. _d 0)
           reff2=min(reff(i,j,k,2),20.0 _d 0)

           g1=(aig(1)+(aig(2)+aig(3)*reff1)*reff1)*taucld(i,j,k,1)
           g2=(awg(1)+(awg(2)+awg(3)*reff2)*reff2)*taucld(i,j,k,2)
           asyclt(i,j)=(g1+g2)/taux

          endif

        enddo
       enddo

       do j=1,n
        do i=1,m
           asycl(i,j,k)=asyclt(i,j)
        enddo
       enddo

      enddo

      do j=1,n
       do i=1,m
        fdiruv(i,j)=0.0
        fdifuv(i,j)=0.0
       enddo
      enddo

c-----integration over spectral bands

      do 100 ib=1,nband

       do 300 k= 1, np

        do j= 1, n
         do i= 1, m

c-----compute ozone and rayleigh optical thicknesses

          taurs=ry(ib)*dp(i,j,k)
          tauoz=xk(ib)*oh(i,j,k)

c-----compute total optical thickness, single scattering albedo,
c     and asymmetry factor

          tausto=taurs+tauoz+taual(i,j,k)+1.0e-8
          ssatau=ssaal(ib)*taual(i,j,k)+taurs
          asysto=asyal(ib)*ssaal(ib)*taual(i,j,k)

c-----compute reflectance and transmittance for cloudless layers

          tauto=tausto
          ssato=ssatau/tauto+1.0e-8
          ssato=min(ssato,0.999999 _d 0)
          asyto=asysto/(ssato*tauto)

          call deledd(tauto,ssato,asyto,csm(i,j),
     *                rr1t(i,j),tt1t(i,j),td1t(i,j))

          call sagpol (tauto,ssato,asyto,rs1t(i,j),ts1t(i,j))

c-----compute reflectance and transmittance for cloud layers

          if (tauclb(i,j,k) .lt. 0.01) then

           rr2t(i,j)=rr1t(i,j)
           tt2t(i,j)=tt1t(i,j)
           td2t(i,j)=td1t(i,j)
           rs2t(i,j)=rs1t(i,j)
           ts2t(i,j)=ts1t(i,j)

          else

           tauto=tausto+tauclb(i,j,k)
           ssato=(ssatau+tauclb(i,j,k))/tauto+1.0e-8
           ssato=min(ssato,0.999999 _d 0)
           asyto=(asysto+asycl(i,j,k)*tauclb(i,j,k))/(ssato*tauto)

           call deledd(tauto,ssato,asyto,csm(i,j),
     *                 rr2t(i,j),tt2t(i,j),td2t(i,j))

           tauto=tausto+tauclf(i,j,k)
           ssato=(ssatau+tauclf(i,j,k))/tauto+1.0e-8
           ssato=min(ssato,0.999999 _d 0)
           asyto=(asysto+asycl(i,j,k)*tauclf(i,j,k))/(ssato*tauto)

           call sagpol (tauto,ssato,asyto,rs2t(i,j),ts2t(i,j))

          endif

         enddo
        enddo

        do j=1,n
         do i=1,m
            rr(i,j,k,1)=rr1t(i,j)
         enddo
        enddo
        do j=1,n
         do i=1,m
            tt(i,j,k,1)=tt1t(i,j)
         enddo
        enddo
        do j=1,n
         do i=1,m
            td(i,j,k,1)=td1t(i,j)
         enddo
        enddo
        do j=1,n
         do i=1,m
            rs(i,j,k,1)=rs1t(i,j)
         enddo
        enddo
        do j=1,n
         do i=1,m
            ts(i,j,k,1)=ts1t(i,j)
         enddo
        enddo

        do j=1,n
         do i=1,m
            rr(i,j,k,2)=rr2t(i,j)
         enddo
        enddo
        do j=1,n
         do i=1,m
            tt(i,j,k,2)=tt2t(i,j)
         enddo
        enddo
        do j=1,n
         do i=1,m
            td(i,j,k,2)=td2t(i,j)
         enddo
        enddo
        do j=1,n
         do i=1,m
            rs(i,j,k,2)=rs2t(i,j)
         enddo
        enddo
        do j=1,n
         do i=1,m
            ts(i,j,k,2)=ts2t(i,j)
         enddo
        enddo

 300  continue

c-----flux calculations

       do k= 1, np+1
        do j= 1, n
         do i= 1, m
          fclr(i,j,k) = 0.
          fall(i,j,k) = 0.
         enddo
        enddo
       enddo
       do j= 1, n
        do i= 1, m
         fsdir(i,j) = 0.
         fsdif(i,j) = 0.
        enddo
       enddo
        call cldflx (m,n,np,ict,icb,cc,rr,tt,td,rs,ts,
     *               fclr,fall,fsdir,fsdif)

       do k= 1, np+1
        do j= 1, n
         do i= 1, m
          flx(i,j,k)=flx(i,j,k)+fall(i,j,k)*hk(ib)
         enddo
        enddo
        do j= 1, n
         do i= 1, m
          flc(i,j,k)=flc(i,j,k)+fclr(i,j,k)*hk(ib)
         enddo
        enddo
       enddo

       if(ib.eq.nband) then
         do j=1,n
          do i=1,m
           fdirpar(i,j)=fsdir(i,j)*hk(ib)
           fdifpar(i,j)=fsdif(i,j)*hk(ib)
         enddo
        enddo
       else
         do j=1,n
          do i=1,m
           fdiruv(i,j)=fdiruv(i,j)+fsdir(i,j)*hk(ib)
           fdifuv(i,j)=fdifuv(i,j)+fsdif(i,j)*hk(ib)
         enddo
        enddo
       endif

 100  continue

      return
      end

c*********************************************************************

      subroutine deledd(tau,ssc,g0,csm,rr,tt,td)

c*********************************************************************
c
c-----uses the delta-eddington approximation to compute the
c     bulk scattering properties of a single layer
c     coded following King and Harshvardhan (JAS, 1986)
c
c  inputs:
c
c     tau: the effective optical thickness
c     ssc: the effective single scattering albedo
c     g0:  the effective asymmetry factor
c     csm: the effective secant of the zenith angle
c
c  outputs:
c
c     rr: the layer reflection of the direct beam
c     tt: the layer diffuse transmission of the direct beam
c     td: the layer direct transmission of the direct beam

c*********************************************************************

      implicit none

c-----Explicit Inline Directives

#ifdef CRAY
#ifdef f77
cfpp$ expand (expmn)
#endif
#endif
      _RL expmn

      _RL  zero,one,two,three,four,fourth,seven,tumin
      parameter (one=1., three=3.)
      parameter (seven=7., two=2.)
      parameter (four=4., fourth=.25)
      parameter (zero=0., tumin=1.e-20)

c-----input parameters
      _RL  tau,ssc,g0,csm

c-----output parameters
      _RL  rr,tt,td

c-----temporary parameters

      _RL  zth,ff,xx,taup,sscp,gp,gm1,gm2,gm3,akk,alf1,alf2,
     *     all1,bll,st7,st8,cll,dll,fll,ell,st1,st2,st3,st4
c
                zth = one / csm

c  delta-eddington scaling of single scattering albedo,
c  optical thickness, and asymmetry factor,
c  K & H eqs(27-29)

                ff  = g0*g0
                xx  = one-ff*ssc
                taup= tau*xx
                sscp= ssc*(one-ff)/xx
                gp  = g0/(one+g0)

c  extinction of the direct beam transmission

                td  = expmn(-taup*csm)

c  gamma1, gamma2, gamma3 and gamma4. see table 2 and eq(26) K & H
c  ssc and gp are the d-s single scattering
c  albedo and asymmetry factor.

                xx  =  three*gp
                gm1 =  (seven - sscp*(four+xx))*fourth
                gm2 = -(one   - sscp*(four-xx))*fourth
                gm3 =  (two   -        zth*xx )*fourth

c  akk is k as defined in eq(25) of K & H

                xx  = gm1-gm2
                akk = sqrt((gm1+gm2)*xx)

c   alf1 and alf2 are alpha1 and alpha2 from eqs (23) & (24) of K & H

                alf1 = gm1 - gm3 * xx
                alf2 = gm2 + gm3 * xx

c  all1 is last term in eq(21) of K & H
c  bll is last term in eq(22) of K & H

                xx  = akk * two
                all1 = (gm3 - alf2 * zth    )*xx*td
                bll = (one - gm3 + alf1*zth)*xx

                xx  = akk * zth
                st7 = one - xx
                st8 = one + xx

                xx  = akk * gm3
                cll = (alf2 + xx) * st7
                dll = (alf2 - xx) * st8

                xx  = akk * (one-gm3)
                fll = (alf1 + xx) * st8
                ell = (alf1 - xx) * st7

                st3 = max(abs(st7*st8),tumin)
                st3 = sign (st3,st7)

                st2 = expmn(-akk*taup)
                st4 = st2 * st2

                st1 =  sscp / ((akk+gm1 + (akk-gm1)*st4) * st3)

c   rr is r-hat of eq(21) of K & H
c   tt is diffuse part of t-hat of eq(22) of K & H

                rr =   ( cll-dll*st4    -all1*st2)*st1
                tt = - ((fll-ell*st4)*td-bll*st2)*st1

                rr = max(rr,zero)
                tt = max(tt,zero)

      return
      end

c*********************************************************************

      subroutine sagpol(tau,ssc,g0,rll,tll)

c*********************************************************************
c-----transmittance (tll) and reflectance (rll) of diffuse radiation
c     follows Sagan and Pollock (JGR, 1967).
c     also, eq.(31) of Lacis and Hansen (JAS, 1974).
c
c-----input parameters:
c
c      tau: the effective optical thickness
c      ssc: the effective single scattering albedo
c      g0:  the effective asymmetry factor
c
c-----output parameters:
c
c      rll: the layer reflection of diffuse radiation
c      tll: the layer transmission of diffuse radiation
c
c*********************************************************************

      implicit none

c-----Explicit Inline Directives

#ifdef CRAY
#ifdef f77
cfpp$ expand (expmn)
#endif
#endif
      _RL expmn

      _RL  one,three,four
      parameter (one=1., three=3., four=4.)

c-----output parameters:

      _RL  tau,ssc,g0

c-----output parameters:

      _RL  rll,tll

c-----temporary arrays

      _RL  xx,uuu,ttt,emt,up1,um1,st1

             xx  = one-ssc*g0
             uuu = sqrt( xx/(one-ssc))
             ttt = sqrt( xx*(one-ssc)*three )*tau
             emt = expmn(-ttt)
             up1 = uuu + one
             um1 = uuu - one
             xx  = um1*emt
             st1 = one / ((up1+xx) * (up1-xx))
             rll = up1*um1*(one-emt*emt)*st1
             tll = uuu*four*emt         *st1

      return
      end

c*******************************************************************

      function expmn(fin)

c*******************************************************************
c compute exponential for arguments in the range 0> fin > -10.
c*******************************************************************
      implicit none
      _RL  fin,expmn

      _RL one,expmin,e1,e2,e3,e4
      parameter (one=1.0, expmin=-10.0)
      parameter (e1=1.0,        e2=-2.507213e-1)
      parameter (e3=2.92732e-2, e4=-3.827800e-3)

      if (fin .lt. expmin) fin = expmin
      expmn = ((e4*fin + e3)*fin+e2)*fin+e1
      expmn = expmn * expmn
      expmn = one / (expmn * expmn)

      return
      end

c*******************************************************************

      subroutine cldflx (m,n,np,ict,icb,cc,rr,tt,td,rs,ts,
     *           fclr,fall,fsdir,fsdif)

c*******************************************************************
c  compute upward and downward fluxes using a two-stream adding method
c  following equations (3)-(5) of Chou (1992, JAS).
c
c  clouds are grouped into high, middle, and low clouds which are
c  assumed randomly overlapped. It involves eight sets of calculations.
c  In each set of calculations, each atmospheric layer is homogeneous,
c  either with clouds or without clouds.

c  input parameters:
c     m:   number of soundings in zonal direction
c     n:   number of soundings in meridional direction
c     np:  number of atmospheric layers
c     ict: the level separating high and middle clouds
c     icb: the level separating middle and low clouds
c     cc:  effective cloud covers for high, middle and low clouds
c     tt:  diffuse transmission of a layer illuminated by beam radiation
c     td:  direct beam tranmssion
c     ts:  transmission of a layer illuminated by diffuse radiation
c     rr:  reflection of a layer illuminated by beam radiation
c     rs:  reflection of a layer illuminated by diffuse radiation
c
c  output parameters:
c     fclr:  clear-sky flux (downward minus upward)
c     fall:  all-sky flux (downward minus upward)
c     fdndir: surface direct downward flux
c     fdndif: surface diffuse downward flux
c*********************************************************************c

      implicit none

c-----input parameters

      integer m,n,np,ict,icb

      _RL  rr(m,n,np+1,2),tt(m,n,np+1,2),td(m,n,np+1,2)
      _RL  rs(m,n,np+1,2),ts(m,n,np+1,2)
      _RL  cc(m,n,3)

c-----temporary array

      integer i,j,k,ih,im,is
      _RL  rra(m,n,np+1,2,2),tta(m,n,np+1,2,2),tda(m,n,np+1,2,2)
      _RL  rsa(m,n,np+1,2,2),rxa(m,n,np+1,2,2)
      _RL  ch(m,n),cm(m,n),ct(m,n),flxdn(m,n,np+1)
      _RL  fdndir(m,n),fdndif(m,n),fupdif
      _RL  denm,xx

c-----output parameters

      _RL  fclr(m,n,np+1),fall(m,n,np+1)
      _RL  fsdir(m,n),fsdif(m,n)

c-----initialize all-sky flux (fall) and surface downward fluxes

      do k=1,np+1
       do j=1,n
        do i=1,m
           fall(i,j,k)=0.0
        enddo
       enddo
      enddo

       do j=1,n
        do i=1,m
           fsdir(i,j)=0.0
           fsdif(i,j)=0.0
        enddo
       enddo

c-----compute transmittances and reflectances for a composite of
c     layers. layers are added one at a time, going down from the top.
c     tda is the composite transmittance illuminated by beam radiation
c     tta is the composite diffuse transmittance illuminated by
c         beam radiation
c     rsa is the composite reflectance illuminated from below
c         by diffuse radiation
c     tta and rsa are computed from eqs. (4b) and (3b) of Chou

c-----for high clouds. indices 1 and 2 denote clear and cloudy
c     situations, respectively.

      do 10 ih=1,2

       do j= 1, n
        do i= 1, m
          tda(i,j,1,ih,1)=td(i,j,1,ih)
          tta(i,j,1,ih,1)=tt(i,j,1,ih)
          rsa(i,j,1,ih,1)=rs(i,j,1,ih)
          tda(i,j,1,ih,2)=td(i,j,1,ih)
          tta(i,j,1,ih,2)=tt(i,j,1,ih)
          rsa(i,j,1,ih,2)=rs(i,j,1,ih)
        enddo
       enddo

       do k= 2, ict-1
        do j= 1, n
         do i= 1, m
          denm = ts(i,j,k,ih)/( 1.-rsa(i,j,k-1,ih,1)*rs(i,j,k,ih))
          tda(i,j,k,ih,1)= tda(i,j,k-1,ih,1)*td(i,j,k,ih)
          tta(i,j,k,ih,1)= tda(i,j,k-1,ih,1)*tt(i,j,k,ih)
     *                  +(tda(i,j,k-1,ih,1)*rr(i,j,k,ih)
     *                  *rsa(i,j,k-1,ih,1)+tta(i,j,k-1,ih,1))*denm
          rsa(i,j,k,ih,1)= rs(i,j,k,ih)+ts(i,j,k,ih)
     *                  *rsa(i,j,k-1,ih,1)*denm
          tda(i,j,k,ih,2)= tda(i,j,k,ih,1)
          tta(i,j,k,ih,2)= tta(i,j,k,ih,1)
          rsa(i,j,k,ih,2)= rsa(i,j,k,ih,1)
         enddo
        enddo
       enddo

c-----for middle clouds

      do 10 im=1,2

       do k= ict, icb-1
        do j= 1, n
         do i= 1, m
          denm = ts(i,j,k,im)/( 1.-rsa(i,j,k-1,ih,im)*rs(i,j,k,im))
          tda(i,j,k,ih,im)= tda(i,j,k-1,ih,im)*td(i,j,k,im)
          tta(i,j,k,ih,im)= tda(i,j,k-1,ih,im)*tt(i,j,k,im)
     *                  +(tda(i,j,k-1,ih,im)*rr(i,j,k,im)
     *                  *rsa(i,j,k-1,ih,im)+tta(i,j,k-1,ih,im))*denm
          rsa(i,j,k,ih,im)= rs(i,j,k,im)+ts(i,j,k,im)
     *                  *rsa(i,j,k-1,ih,im)*denm
         enddo
        enddo
       enddo

  10  continue

c-----layers are added one at a time, going up from the surface.
c     rra is the composite reflectance illuminated by beam radiation
c     rxa is the composite reflectance illuminated from above
c         by diffuse radiation
c     rra and rxa are computed from eqs. (4a) and (3a) of Chou

c-----for the low clouds

      do 20 is=1,2

       do j= 1, n
        do i= 1, m
         rra(i,j,np+1,1,is)=rr(i,j,np+1,is)
         rxa(i,j,np+1,1,is)=rs(i,j,np+1,is)
         rra(i,j,np+1,2,is)=rr(i,j,np+1,is)
         rxa(i,j,np+1,2,is)=rs(i,j,np+1,is)
        enddo
       enddo

       do k=np,icb,-1
        do j= 1, n
         do i= 1, m
          denm=ts(i,j,k,is)/( 1.-rs(i,j,k,is)*rxa(i,j,k+1,1,is) )
          rra(i,j,k,1,is)=rr(i,j,k,is)+(td(i,j,k,is)
     *        *rra(i,j,k+1,1,is)+tt(i,j,k,is)*rxa(i,j,k+1,1,is))*denm
          rxa(i,j,k,1,is)= rs(i,j,k,is)+ts(i,j,k,is)
     *        *rxa(i,j,k+1,1,is)*denm
          rra(i,j,k,2,is)=rra(i,j,k,1,is)
          rxa(i,j,k,2,is)=rxa(i,j,k,1,is)
         enddo
        enddo
       enddo

c-----for middle clouds

      do 20 im=1,2

       do k= icb-1,ict,-1
        do j= 1, n
         do i= 1, m
          denm=ts(i,j,k,im)/( 1.-rs(i,j,k,im)*rxa(i,j,k+1,im,is) )
          rra(i,j,k,im,is)= rr(i,j,k,im)+(td(i,j,k,im)
     *        *rra(i,j,k+1,im,is)+tt(i,j,k,im)*rxa(i,j,k+1,im,is))*denm
          rxa(i,j,k,im,is)= rs(i,j,k,im)+ts(i,j,k,im)
     *        *rxa(i,j,k+1,im,is)*denm
         enddo
        enddo
       enddo

  20  continue

c-----integration over eight sky situations.
c     ih, im, is denotes high, middle and low cloud groups.

      do 100 ih=1,2

c-----clear portion

         if(ih.eq.1) then
           do j=1,n
            do i=1,m
             ch(i,j)=1.0-cc(i,j,1)
            enddo
           enddo

          else

c-----cloudy portion

           do j=1,n
            do i=1,m
             ch(i,j)=cc(i,j,1)
            enddo
           enddo

          endif

      do 100 im=1,2

c-----clear portion

         if(im.eq.1) then

           do j=1,n
            do i=1,m
              cm(i,j)=ch(i,j)*(1.0-cc(i,j,2))
            enddo
           enddo

         else

c-----cloudy portion

           do j=1,n
            do i=1,m
              cm(i,j)=ch(i,j)*cc(i,j,2)
            enddo
           enddo

         endif

      do 100 is=1,2

c-----clear portion

         if(is.eq.1) then

           do j=1,n
            do i=1,m
             ct(i,j)=cm(i,j)*(1.0-cc(i,j,3))
            enddo
           enddo

         else

c-----cloudy portion

           do j=1,n
            do i=1,m
             ct(i,j)=cm(i,j)*cc(i,j,3)
            enddo
           enddo

         endif

c-----add one layer at a time, going down.

       do k= icb, np
        do j= 1, n
         do i= 1, m
          denm = ts(i,j,k,is)/( 1.-rsa(i,j,k-1,ih,im)*rs(i,j,k,is) )
          tda(i,j,k,ih,im)= tda(i,j,k-1,ih,im)*td(i,j,k,is)
          tta(i,j,k,ih,im)=  tda(i,j,k-1,ih,im)*tt(i,j,k,is)
     *         +(tda(i,j,k-1,ih,im)*rr(i,j,k,is)
     *         *rsa(i,j,k-1,ih,im)+tta(i,j,k-1,ih,im))*denm
          rsa(i,j,k,ih,im)= rs(i,j,k,is)+ts(i,j,k,is)
     *         *rsa(i,j,k-1,ih,im)*denm
         enddo
        enddo
       enddo

c-----add one layer at a time, going up.

       do k= ict-1,1,-1
        do j= 1, n
         do i= 1, m
          denm =ts(i,j,k,ih)/(1.-rs(i,j,k,ih)*rxa(i,j,k+1,im,is))
          rra(i,j,k,im,is)= rr(i,j,k,ih)+(td(i,j,k,ih)
     *        *rra(i,j,k+1,im,is)+tt(i,j,k,ih)*rxa(i,j,k+1,im,is))*denm
          rxa(i,j,k,im,is)= rs(i,j,k,ih)+ts(i,j,k,ih)
     *        *rxa(i,j,k+1,im,is)*denm
         enddo
        enddo
       enddo

c-----compute fluxes following eq (5) of Chou (1992)

c     fdndir is the direct  downward flux
c     fdndif is the diffuse downward flux
c     fupdif is the diffuse upward flux

      do k=2,np+1
       do j=1, n
        do i=1, m
         denm= 1./(1.- rxa(i,j,k,im,is)*rsa(i,j,k-1,ih,im))
         fdndir(i,j)= tda(i,j,k-1,ih,im)
         xx = tda(i,j,k-1,ih,im)*rra(i,j,k,im,is)
         fdndif(i,j)= (xx*rsa(i,j,k-1,ih,im)+tta(i,j,k-1,ih,im))*denm
         fupdif= (xx+tta(i,j,k-1,ih,im)*rxa(i,j,k,im,is))*denm
         flxdn(i,j,k)=fdndir(i,j)+fdndif(i,j)-fupdif
        enddo
       enddo
      enddo

       do j=1, n
        do i=1, m
         flxdn(i,j,1)=1.0-rra(i,j,1,im,is)
        enddo
       enddo

c-----summation of fluxes over all (eight) sky situations.

       do k=1,np+1
        do j=1,n
         do i=1,m
           if(ih.eq.1 .and. im.eq.1 .and. is.eq.1) then
             fclr(i,j,k)=flxdn(i,j,k)
           endif
             fall(i,j,k)=fall(i,j,k)+flxdn(i,j,k)*ct(i,j)
         enddo
        enddo
       enddo

        do j=1,n
         do i=1,m
            fsdir(i,j)=fsdir(i,j)+fdndir(i,j)*ct(i,j)
            fsdif(i,j)=fsdif(i,j)+fdndif(i,j)*ct(i,j)
         enddo
        enddo

  100 continue

      return
      end

c*****************************************************************

      subroutine flxco2(m,n,np,swc,swh,csm,df)

c*****************************************************************

c-----compute the reduction of clear-sky downward solar flux
c     due to co2 absorption.

      implicit none

c-----input parameters

      integer m,n,np
      _RL  csm(m,n),swc(m,n,np+1),swh(m,n,np+1),cah(22,19)

c-----output (undated) parameter

      _RL  df(m,n,np+1)

c-----temporary array

      integer i,j,k,ic,iw
      _RL  xx,clog1,wlog,dc,dw,x1,x2,y2

c********************************************************************
c-----include co2 look-up table

#include "cah-dat.h"
c     save cah

c********************************************************************
c-----table look-up for the reduction of clear-sky solar
c     radiation due to co2. The fraction 0.0343 is the
c     extraterrestrial solar flux in the co2 bands.

      do k= 2, np+1
       do j= 1, n
        do i= 1, m
          xx=1./.3
          clog1=log10(swc(i,j,k)*csm(i,j))
          wlog=log10(swh(i,j,k)*csm(i,j))
          ic=int( (clog1+3.15)*xx+1.)
          iw=int( (wlog+4.15)*xx+1.)
          if(ic.lt.2)ic=2
          if(iw.lt.2)iw=2
          if(ic.gt.22)ic=22
          if(iw.gt.19)iw=19
          dc=clog1-float(ic-2)*.3+3.
          dw=wlog-float(iw-2)*.3+4.
          x1=cah(1,iw-1)+(cah(1,iw)-cah(1,iw-1))*xx*dw
          x2=cah(ic-1,iw-1)+(cah(ic-1,iw)-cah(ic-1,iw-1))*xx*dw
          y2=x2+(cah(ic,iw-1)-cah(ic-1,iw-1))*xx*dc
          if (x1.lt.y2) x1=y2
          df(i,j,k)=df(i,j,k)+0.0343*(x1-y2)
        enddo
       enddo
      enddo

      return
      end
