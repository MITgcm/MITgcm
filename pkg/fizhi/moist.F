      subroutine moistio (ndmoist,istrip,npcs,pz,tz,qz,ntracer,ptracer,
     .   pkht,qqz,dumoist,dvmoist,dtmoist,dqmoist,
     .   im,jm,lm,sige,sig,dsig,ptop,
     .   iras,rainlsp,rainconv,snowfall,
     .   nswcld,cldtot_sw,cldras_sw,cldlsp_sw,nswlz,swlz,
     .   nlwcld,cldtot_lw,cldras_lw,cldlsp_lw,nlwlz,lwlz,
     .   lpnt,qdiag,nd,myid)

#include "diagnostics.h"

c Input Variables
c ---------------
      integer ndmoist,istrip,npcs,nd,myid

      integer im,jm,lm                
      real  ptop                      
      real  sige(lm+1)                
      real   sig(lm)                  
      real  dsig(lm)                  

      integer ntracer,ptracer         

      real pz(im,jm)                  
      real tz(im,jm,lm)               
      real qz(im,jm,lm,ntracer)       

      real  pkht(im,jm,lm)            

      real   qqz(im,jm,lm)            

      real dumoist(im,jm,lm)          
      real dvmoist(im,jm,lm)          
      real dtmoist(im,jm,lm)          
      real dqmoist(im,jm,lm,ntracer)  

      integer iras                    
      real   rainlsp(im,jm)           
      real  rainconv(im,jm)           
      real  snowfall(im,jm)           

      integer nswcld,nswlz            
      real  cldlsp_sw(im,jm,lm)       
      real  cldras_sw(im,jm,lm)       
      real  cldtot_sw(im,jm,lm)       
      real       swlz(im,jm,lm)       

      integer nlwcld,nlwlz            
      real  cldlsp_lw(im,jm,lm)       
      real  cldras_lw(im,jm,lm)       
      real  cldtot_lw(im,jm,lm)       
      real       lwlz(im,jm,lm)       

      real  qdiag(im,jm,nd)           
      logical lpnt                    

c Local Variables
c ---------------
      integer    ncrnd,nsecf,nsubmax

      real       fracqq, rh,temp1,temp2,dum
      integer    snowcrit, lup
      parameter (fracqq = 0.1)

      real   cldsr(im,jm,lm)
      real   srcld(istrip,lm)

      real plev
      real cldnow,cldlsp_mem,cldras_mem,cldras,watnow,watmin,cldmin
      real cldprs(im,jm),cldtmp(im,jm)
      real cldhi (im,jm),cldlow(im,jm)
      real cldmid(im,jm),totcld(im,jm)
      integer midlevel,lowlevel

      real   CLDLS(im,jm,lm)  , CPEN(im,jm,lm)
      real    tmpimjm(im,jm)
      real    lsp_new(im,jm)
      real   conv_new(im,jm)
      real   snow_new(im,jm)

      real  qqcolmin(im,jm)
      real  qqcolmax(im,jm)
      integer levpbl(im,jm)

c Gathered Arrays for Variable Cloud Base
c ---------------------------------------
      real    raincgath(im*jm)
      real     pigather(im*jm)
      real     thgather(im*jm,lm)
      real     shgather(im*jm,lm)
      real    pkzgather(im*jm,lm)
      real    pkegather(im*jm,lm)
      real    tmpgather(im*jm,lm)
      real   deltgather(im*jm,lm)
      real   delqgather(im*jm,lm)
      real      ugather(im*jm,lm,ntracer)
      real   delugather(im*jm,lm,ntracer)
      real     deltrnev(im*jm,lm)
      real     delqrnev(im*jm,lm)

      integer  nindeces(lm)
      integer  pblindex(im*jm)
      integer levgather(im*jm)

c Stripped Arrays
c ---------------
      real saveth (istrip,lm)
      real saveq  (istrip,lm)
      real saveu  (istrip,lm,ntracer)
      real usubcl (istrip,   ntracer)

      real     ple(istrip,lm+1), gam(istrip,lm)
      real      TL(ISTRIP,lm)  , SHL(ISTRIP,lm)
      real      PL(ISTRIP,lm)  , PLK(ISTRIP,lm)
      real    PLKE(ISTRIP,lm+1)
      real      TH(ISTRIP,lm)  ,CVTH(ISTRIP,lm)
      real   SHSAT(ISTRIP,lm)  , CVQ(ISTRIP,lm)
      real      UL(ISTRIP,lm,ntracer)
      real     cvu(istrip,lm,ntracer)
      real  CLMAXO(ISTRIP,lm),CLBOTH(ISTRIP,lm)
      real  CLSBTH(ISTRIP,lm)
      real    TMP1(ISTRIP,lm),  TMP2(ISTRIP,lm)
      real    TMP3(ISTRIP,lm),  TMP4(ISTRIP,lm+1)
      real    TMP5(ISTRIP,lm+1)
      integer   ITMP1(ISTRIP,lm), ITMP2(ISTRIP,lm)
      integer   ITMP3(ISTRIP,lm)

      real   PRECIP(ISTRIP), PCMID(ISTRIP), PCNET(ISTRIP)
      real   PCLOW (ISTRIP),    SP(ISTRIP),  PREP(ISTRIP)
      real   PCPEN (ISTRIP,lm)
      integer pbl(istrip),depths(lm)

      real   cldlz(istrip,lm), cldwater(im,jm,lm)
      real   rhfrac(istrip), rhmin, pup, ppbl, rhcrit(istrip,lm)
      real   offset, alpha, rasmax

      logical first
      logical lras
      real    clfrac (istrip,lm)
      real    cldmas (istrip,lm)
      real    detrain(istrip,lm)
      real    psubcld    (istrip), psubcldg (im,jm)
      real    psubcld_cnt(istrip), psubcldgc(im,jm)
      real rnd(lm/2)
      DATA      FIRST /.TRUE./

      integer imstp,nltop,nsubcl,nlras,nsubmin
      integer i,j,iloop,index,l,nn,num,numdeps,nt
      real tmstp,tminv,sday,grav,alhl,cp,elocp,gamfac
      real rkappa,p0kappa,p0kinv,ptopkap,pcheck
      real tice,getcon,pi
 
C **********************************************************************
C ****                     INITIALIZATION                           ****
C **********************************************************************

      IMSTP  = nsecf(NDMOIST)
      TMSTP  = FLOAT(IMSTP)
      TMINV  = 1. /  TMSTP

C Minimum Large-Scale Cloud Fraction at rhcrit
      alpha  = 0.80         
C Difference in fraction between SR and LS Thresshold
      offset = 0.10         
C Large-Scale Relative Humidity Threshold in PLB
      rhmin  = 0.90         
C Maximum Cloud Fraction associated with RAS
      rasmax = 1.00         

          nn = 3*3600.0/tmstp + 1
C Threshold for Cloud Fraction Memory
      cldmin = rasmax*(1.0-tmstp/3600.)**nn   
C Threshold for Cloud Liquid Water Memory
      watmin = 1.0e-8                         

      SDAY   = GETCON('SDAY')
      GRAV   = GETCON('GRAVITY')
      ALHL   = GETCON('LATENT HEAT COND')
      CP     = GETCON('CP')
      ELOCP  = GETCON('LATENT HEAT COND') / GETCON('CP')
      GAMFAC = GETCON('LATENT HEAT COND') * GETCON('EPS') * ELOCP
     .       / GETCON('RGAS')
      RKAPPA = GETCON('KAPPA')
      P0KAPPA  =  1000.0**RKAPPA
      P0KINV   =  1. / P0KAPPA
      PTOPKAP  =  PTOP**RKAPPA
      tice     = getcon('FREEZING-POINT')
      PI       = 4.*atan(1.)

c Determine Upper  Level for Cumulus Convection
c and Total number of Random Clouds to Check
c ---------------------------------------------

      NLTOP = 1
      DO L=1,lm
       PCHECK = (1000.-ptop)*SIG(L) + PTOP
       IF (PCHECK.GE.10.) THEN
        NLTOP = L
        GO TO 2
       ENDIF
      ENDDO
 2    CONTINUE
      ncrnd = (lm-nltop+1)/2

c Determine Minimum Number of Levels in Sub-Cloud (50 mb) Layer
c -------------------------------------------------------------
      nsubmin = lm
      nsubmax = 1
      DO L=lm-1,1,-1
          PCHECK = (1000.-ptop)*SIG(L) + PTOP
      IF( PCHECK.GE.950.0 ) nsubmin = L
      IF( PCHECK.GE.750.0 ) nsubmax = L
      ENDDO

      if(first .and. myid.eq.0) then
       print *
       print *,'Top Level Allowed for Convection : ',nltop,
     .                    ' (',(1000.-ptop)*SIG(nltop) + PTOP,' mb)'
       print *,'          Highest Sub-Cloud Level: ',nsubmax,
     .                    ' (',(1000.-ptop)*SIG(nsubmax) + PTOP,' mb)'
       print *,'          Lowest  Sub-Cloud Level: ',nsubmin,
     .                    ' (',(1000.-ptop)*SIG(nsubmin) + PTOP,' mb)'
       print *,'    Total Number of Random Clouds: ',ncrnd
       print *
       first = .false.
      endif

c And now find PBL depth - the level where qq = fracqq * qq at surface
c --------------------------------------------------------------------
      do j = 1,jm
      do i = 1,im
       qqcolmin(i,j) = qqz(i,j,lm)*fracqq
       qqcolmax(i,j) = qqz(i,j,lm)
         levpbl(i,j) = lm+1
      enddo
      enddo

      DO L = lm-1,1,-1
       DO j = 1,jm
       DO i = 1,im
        IF((qqz(i,j,l).gt.qqcolmax(i,j))
     1                   .and.(levpbl(i,j).eq.lm+1))then
         qqcolmax(i,j) = qqz(i,j,l)
         qqcolmin(i,j) = fracqq*qqcolmax(i,j)
        endif
        if((qqz(i,j,l).lt.qqcolmin(i,j))
     1                   .and.(levpbl(i,j).eq.lm+1))levpbl(i,j)=L+1
       enddo
       enddo
      enddo

      do j = 1,jm
      do i = 1,im
        if(levpbl(i,j).gt.nsubmin) levpbl(i,j) = nsubmin
        if(levpbl(i,j).lt.nsubmax) levpbl(i,j) = nsubmax
      enddo
      enddo


c Set up the array of indeces of subcloud levels for the gathering
c ----------------------------------------------------------------
      index = 0
      do L = nsubmin,nltop,-1
       do j = 1,jm
       do i = 1,im
        if(levpbl(i,j).eq.L) then
         index = index + 1
         pblindex(index) = (j-1)*im + i
        endif
       enddo
       enddo
      enddo

      do index = 1,im*jm
       levgather(index) = levpbl(pblindex(index),1)
        pigather(index) =     pz(pblindex(index),1)
      enddo

      do L = 1,lm
       do index = 1,im*jm
         thgather(index,L) =   tz(pblindex(index),1,L)
         shgather(index,L) =   qz(pblindex(index),1,L,1)
        pkegather(index,L) = pkht(pblindex(index),1,L)
       enddo
      enddo
      do nt = 1,ntracer-ptracer
      do L = 1,lm
       do index = 1,im*jm
        ugather(index,L,nt) = qz(pblindex(index),1,L,nt+ptracer)
       enddo
      enddo
      enddo

      call pkappa(pigather,pkegather,pkzgather,ptop,sige,dsig,im,jm,lm)

c bump the counter for number of calls to convection
c --------------------------------------------------
                        iras = iras + 1
      if( iras.ge.1e9 ) iras = 1 

c select the 'random' cloud detrainment levels for RAS
c ----------------------------------------------------
      call rndcloud(iras,ncrnd,rnd,myid)
 
      do l=1,lm
      do j=1,jm
      do i=1,im
      dtmoist(i,j,l) = 0.
        do nt = 1,ntracer
        dqmoist(i,j,l,nt) = 0.
        enddo
      enddo
      enddo
      enddo

C***********************************************************************
C ****                     LOOP OVER NPCS PEICES                    ****
C **********************************************************************

      DO 1000 NN = 1,NPCS

C **********************************************************************
C ****                   VARIABLE INITIALIZATION                    ****
C **********************************************************************

       CALL STRIP (  pigather, SP      ,im*jm,ISTRIP,1 ,NN )
       CALL STRIP ( pkzgather, PLK     ,im*jm,ISTRIP,lm,NN )
       CALL STRIP ( pkegather, PLKE    ,im*jm,ISTRIP,lm,NN )
       CALL STRIP (  thgather, TH      ,im*jm,ISTRIP,lm,NN )
       CALL STRIP (  shgather, SHL     ,im*jm,ISTRIP,lm,NN )
       CALL STRINT( levgather, pbl     ,im*jm,ISTRIP,1 ,NN )

       do nt = 1,ntracer-ptracer
       call strip ( ugather(1,1,nt), ul(1,1,nt),im*jm,istrip,lm,nn )
       enddo

      do l = 1,lm
      do i = 1,istrip
       PL(I,L) =  SIG(L)*SP(I) + PTOP
      PLE(I,L) = SIGE(L)*SP(I) + PTOP
      enddo
      enddo

      do i = 1,istrip
      PLE(I,lm+1) = SP(I) + PTOP
      enddo

C **********************************************************************
C ****        SETUP FOR RAS CUMULUS PARAMETERIZATION                ****
C **********************************************************************

      DO L = 1,lm
      DO I = 1,ISTRIP
             TH(I,L) = TH(I,L) * P0KAPPA
         CLMAXO(I,L) = 0.
         CLBOTH(I,L) = 0.
         cldmas(I,L) = 0.
        detrain(I,L) = 0.
      ENDDO
      ENDDO

      do L = 1,lm
      depths(L) = 0
      enddo

      numdeps = 0
      do L = nsubmin,nltop,-1
                       nindeces(L) = 0
       do i = 1,istrip
       if(pbl(i).eq.L) nindeces(L) = nindeces(L) + 1
       enddo
       if(nindeces(L).gt.0) then
       numdeps = numdeps + 1
       depths(numdeps) = L
       endif
      enddo


C Initiate a do-loop around RAS for the number of different
C    sub-cloud layer depths in this strip
C --If all subcloud depths are the same, execute loop once
C   Otherwise loop over different subcloud layer depths

      num = 1
      DO iloop = 1,numdeps

       nsubcl = depths(iloop)

c Compute sub-cloud values for Temperature and Spec.Hum.
c ------------------------------------------------------
       DO 600 I=num,num+nindeces(nsubcl)-1
        TMP1(I,2) = 0.
        TMP1(I,3) = 0.
 600   CONTINUE

       NLRAS = NSUBCL - NLTOP + 1
       DO 601 L=NSUBCL,lm
       DO 602 I=num,num+nindeces(nsubcl)-1
        TMP1(I,2) = TMP1(I,2) + (PLE(I,L+1)-PLE(I,L))*TH (I,L)/sp(i)
        TMP1(I,3) = TMP1(I,3) + (PLE(I,L+1)-PLE(I,L))*SHL(I,L)/sp(i)
 602   CONTINUE
 601   CONTINUE
       DO 603 I=num,num+nindeces(nsubcl)-1
        TMP1(I,4) = 1. / ( (PLE(I,lm+1)-PLE(I,NSUBCL))/sp(I) )
         TH(I,NSUBCL) = TMP1(I,2)*TMP1(I,4)
        SHL(I,NSUBCL) = TMP1(I,3)*TMP1(I,4)
 603   CONTINUE

c Save initial value of tracers and compute sub-cloud value
c ---------------------------------------------------------
       DO NT = 1,ntracer-ptracer
          do  L = 1,lm
          do  i = num,num+nindeces(nsubcl)-1
          saveu(i,L,nt) = ul(i,L,nt)
          enddo
          enddo
          DO I=num,num+nindeces(nsubcl)-1
          TMP1(I,2) = 0.
          ENDDO
          DO L=NSUBCL,lm
          DO I=num,num+nindeces(nsubcl)-1
           TMP1(I,2) = TMP1(I,2)+(PLE(I,L+1)-PLE(I,L))*UL(I,L,NT)/sp(i)
          ENDDO
          ENDDO
          DO I=num,num+nindeces(nsubcl)-1
          UL(I,NSUBCL,NT) = TMP1(I,2)*TMP1(I,4)
             usubcl(i,nt) = ul(i,nsubcl,nt)
          ENDDO
       ENDDO

c Compute Pressure Arrays for RAS
c -------------------------------
       DO 111 L=1,lm
       DO 112 I=num,num+nindeces(nsubcl)-1
        TMP4(I,L) = PLE(I,L)
 112   CONTINUE
 111   CONTINUE
       DO I=num,num+nindeces(nsubcl)-1
        TMP5(I,1) = PTOPKAP / P0KAPPA
       ENDDO
       DO L=2,lm
       DO I=num,num+nindeces(nsubcl)-1
        TMP5(I,L) = PLKE(I,L-1)*P0KINV
       ENDDO
       ENDDO
       DO  I=num,num+nindeces(nsubcl)-1
        TMP4(I,lm+1) = PLE (I,lm+1)
        TMP5(I,lm+1) = PLKE(I,lm)*P0KINV
       ENDDO
       DO 113 I=num,num+nindeces(nsubcl)-1
        TMP4(I,NSUBCL+1) = PLE (I,lm+1)
        TMP5(I,NSUBCL+1) = PLKE(I,lm)*P0KINV
 113   CONTINUE

      do i=num,num+nindeces(nsubcl)-1
C Temperature at top of sub-cloud layer
      tmp2(i,1) = TH(i,NSUBCL) * PLKE(i,NSUBCL)/P0KAPPA   
C Pressure    at top of sub-cloud layer
      tmp2(i,2) = tmp4(i,nsubcl)                          
      enddo

C  CHANGED THIS: no RH requirement for RAS
c     call vqsat ( tmp2(num,1),tmp2(num,2),tmp2(num,3),
c    .             dum,.false.,nindeces(nsubcl) )
c     do i=num,num+nindeces(nsubcl)-1
c     rh =  SHL(I,NSUBCL) / tmp2(i,3)
c       if (rh .le. 0.85) then
c         rhfrac(i) = 0.
c       else if (rh .ge. 0.95) then
c         rhfrac(i) = 1.
c       else
c         rhfrac(i) = (rh-0.85)*10.
c       endif
c     enddo
      do i=num,num+nindeces(nsubcl)-1
        rhfrac(i) = 1.
      enddo

C  Compute RH threshold for Large-scale condensation
C  Used in Slingo-Ritter clouds as well - define offset between SR and LS

C Top level of atan func above this rh_threshold = rhmin
      pup = 600.		
      do i=num,num+nindeces(nsubcl)-1
        do L = nsubcl, lm
          rhcrit(i,L) = 1.
        enddo
        do L = 1, nsubcl-1
          pcheck = (1000.-ptop)*sig(L) + ptop
          if (pcheck .le. pup) then
            rhcrit(i,L) = rhmin
          else
            ppbl = (1000.-ptop)*sig(nsubcl) + ptop
            rhcrit(i,L) = rhmin + (1.-rhmin)/(19.) * ((
     .        atan( (2.*(pcheck-pup)/(ppbl-pup)-1.) *
     .       tan(20.*pi/21.-0.5*pi) ) 
     .        + 0.5*pi) * 21./pi - 1.)
          endif
        enddo
      enddo

c Save Initial Values of Temperature and Specific Humidity
c --------------------------------------------------------
      do L = 1,lm
      do i = num,num+nindeces(nsubcl)-1
        saveth(i,L) = th (i,L)
        saveq (i,L) = shl(i,L)
        PCPEN (i,L) = 0.
        CLFRAC(i,L) = 0.
      enddo
      enddo

      CALL RAS ( NN,istrip,nindeces(nsubcl),NLRAS,NLTOP,lm,TMSTP
     1, UL(num,1,1),ntracer-ptracer,TH(num,NLTOP),SHL(num,NLTOP)
     2, TMP4(num,NLTOP), TMP5(num,NLTOP),rnd, ncrnd, PCPEN(num,NLTOP)
     3, CLBOTH(num,NLTOP), CLFRAC(num,NLTOP)
     4, cldmas(num,nltop), detrain(num,nltop)
     8, cp,grav,rkappa,alhl,rhfrac(num),rasmax )

c Compute Diagnostic CLDMAS in RAS Subcloud Layers
c ------------------------------------------------
       do L=nsubcl,lm
        dum = dsig(L)/(1.0-sige(nsubcl))
       do I=num,num+nindeces(nsubcl)-1
        cldmas(i,L) = cldmas(i,L-1) - dum*cldmas(i,nsubcl-1)
       enddo
       enddo

c Update Theta and Moisture due to RAS
c ------------------------------------
       DO L=1,nsubcl
       DO I=num,num+nindeces(nsubcl)-1
        CVTH(I,L) =  (TH (I,L) - saveth(i,l))
        CVQ (I,L) =  (SHL(I,L) - saveq (i,l))
       ENDDO
       ENDDO
       DO L=nsubcl+1,lm
       DO I=num,num+nindeces(nsubcl)-1
        CVTH(I,L) = cvth(i,nsubcl)
        CVQ (I,L) = cvq (i,nsubcl)
       ENDDO
       ENDDO

       DO L=nsubcl+1,lm
       DO I=num,num+nindeces(nsubcl)-1
        TH (I,L) = saveth(i,l) + cvth(i,l)
        SHL(I,L) = saveq (i,l) + cvq (i,l)
       ENDDO
       ENDDO
       DO L=1,lm
       DO I=num,num+nindeces(nsubcl)-1
        CVTH(I,L) =  CVTH(I,L) *P0KINV*SP(I)*tminv
        CVQ (I,L) =  CVQ (I,L) *SP(I)*tminv
       ENDDO
       ENDDO

c Compute Tracer Tendency due to RAS
c ----------------------------------
       do nt = 1,ntracer-ptracer
        DO L=1,nsubcl-1
        DO I=num,num+nindeces(nsubcl)-1
         CVU(I,L,nt) = ( UL(I,L,nt)-saveu(i,l,nt) )*sp(i)*tminv
        ENDDO
        ENDDO
        DO L=nsubcl,lm
        DO I=num,num+nindeces(nsubcl)-1
         if( usubcl(i,nt).ne.0.0 ) then
          cvu(i,L,nt) = ( ul(i,nsubcl,nt)-usubcl(i,nt) ) * 
     .                     ( saveu(i,L,nt)/usubcl(i,nt) )*sp(i)*tminv
         else
          cvu(i,L,nt) = 0.0
         endif
        ENDDO
        ENDDO
       enddo

c Compute Diagnostic PSUBCLD (Subcloud Layer Pressure)
c ----------------------------------------------------
       do i=num,num+nindeces(nsubcl)-1
                             lras = .false.
       do L=nltop,nsubcl
       if( cvq(i,L).ne.0.0 ) lras = .true.
       enddo
       psubcld    (i) = 0.0
       psubcld_cnt(i) = 0.0
       if( lras ) then 
       psubcld    (i) = sp(i)+ptop-ple(i,nsubcl)
       psubcld_cnt(i) = 1.0
       endif
       enddo
      

C End of subcloud layer depth loop (iloop)

       num = num+nindeces(nsubcl)

      ENDDO

C **********************************************************************
C ****                     TENDENCY UPDATES                         ****
C ****    (Keep 'Gathered' tendencies in 'gather' arrays now)       ****
C **********************************************************************

      call paste( CVTH,deltgather,istrip,im*jm,lm,NN )
      call paste(  CVQ,delqgather,istrip,im*jm,lm,NN )
      do nt = 1,ntracer-ptracer
      call paste( CVU(1,1,nt),delugather(1,1,nt),istrip,im*jm,lm,NN )
      enddo

C **********************************************************************
C     And now paste some arrays for filling diagnostics
C (use pkegather to hold detrainment and tmpgather for cloud mass flux)
C **********************************************************************

      if(icldmas .gt.0) call paste( cldmas,tmpgather,istrip,im*jm,lm,NN)
      if(idtrain .gt.0) call paste(detrain,pkegather,istrip,im*jm,lm,NN)
      if(ipsubcld.gt.0) then 
      call paste(psubcld    ,psubcldg ,istrip,im*jm,1,NN)
      call paste(psubcld_cnt,psubcldgc,istrip,im*jm,1,NN)
      endif

C *********************************************************************
C ****         RE-EVAPORATION OF PENETRATING CONVECTIVE RAIN       ****
C *********************************************************************
 
      CALL STRIP ( thgather,TH ,im*jm,ISTRIP,lm,NN)
      CALL STRIP ( shgather,SHL,im*jm,ISTRIP,lm,NN)
      DO L=1,lm
      DO I=1,ISTRIP
           TH(I,L) =  TH(I,L) + CVTH(I,L)*tmstp/SP(I)
          SHL(I,L) = SHL(I,L) +  CVQ(I,L)*tmstp/SP(I)
           TL(I,L) =  TH(I,L)*PLK(I,L)
       saveth(I,L) =  th(I,L)
       saveq (I,L) = SHL(I,L)
      ENDDO
      ENDDO
 
       CALL RNEVP (NN,ISTRIP,lm,TL,SHL,PCPEN,PL,CLFRAC,SP,DSIG,PLKE,
     .  PLK,TH,TMP1,TMP2,TMP3,ITMP1,ITMP2,PCNET,PRECIP,
     .  CLSBTH,TMSTP,1.,cp,grav,alhl,gamfac,cldlz,rhcrit,offset,alpha)

C **********************************************************************
C ****                     TENDENCY UPDATES                         ****
C **********************************************************************

      DO L=1,lm

       DO I =1,ISTRIP
       TMP1(I,L) = sp(i) * (SHL(I,L)-saveq(I,L)) * tminv
       ENDDO
       CALL PSTBMP(TMP1(1,L),delqgather(1,L),ISTRIP,im*jm,1,NN)

       DO I =1,ISTRIP
       TMP1(I,L) = sp(i) * ((TL(I,L)/PLK(I,L))-saveth(i,l)) * tminv
       ENDDO
       CALL PSTBMP(TMP1(1,L),deltgather(1,L),ISTRIP,im*jm,1,NN)

C Paste rain evap tendencies into arrays for diagnostic output
c ------------------------------------------------------------
       if(idtls.gt.0)then
       DO I =1,ISTRIP
        TMP1(I,L) = ((TL(I,L)/PLK(I,L))-saveth(i,l))*plk(i,l)*sday*tminv
       ENDDO
       call paste(tmp1(1,L),deltrnev(1,L),istrip,im*jm,1,NN)
       endif

       if(idqls.gt.0)then
       DO I =1,ISTRIP
        TMP1(I,L) = (SHL(I,L)-saveq(I,L)) * 1000. * sday * tminv
       ENDDO
       call paste(tmp1(1,L),delqrnev(1,L),istrip,im*jm,1,NN)
       endif

      ENDDO

C *********************************************************************
C          Add Non-Precipitating Clouds where the relative 
C                     humidity is less than 100%
C               Apply Cloud Top Entrainment Instability
C *********************************************************************

      do L=1,lm
      do i=1,istrip
      srcld(i,L) = -clsbth(i,L)
      enddo
      enddo

      call srclouds (saveth,saveq,plk,pl,plke,clsbth,cldlz,istrip,lm,
     .                                              rhcrit,offset,alpha)

      do L=1,lm
      do i=1,istrip
      srcld(i,L) = srcld(i,L)+clsbth(i,L)
      enddo
      enddo

C *********************************************************************
C ****                PASTE CLOUD AMOUNTS                          ****
C *********************************************************************

      call paste (  srcld,   cldsr,istrip,im*jm,lm,nn )
      call paste (  cldlz,cldwater,istrip,im*jm,lm,nn )
      call paste ( clsbth,   cldls,istrip,im*jm,lm,nn )
      call paste ( clboth,   cpen ,istrip,im*jm,lm,nn )
 
c compute Total Accumulated Precip for Landsurface Model
c ------------------------------------------------------
      do i = 1,istrip
C  Initialize Rainlsp, Rainconv and Snowfall
      tmp1(i,1) = 0.0   
      tmp1(i,2) = 0.0 
      tmp1(i,3) = 0.0
      enddo

      do i = 1,istrip
      prep(i)   = PRECIP(I) + PCNET(I)
      tmp1(i,1) = PRECIP(I)
      tmp1(i,2) = pcnet(i)
      enddo
c
c  check whether there is snow
c-------------------------------------------------------
c  snow algorthm:
c  if temperature profile from the surface level to 700 mb 
c  uniformaly c  below zero, then precipitation (total) is 
c  snowfall.  Else there is no snow.
c  For version of level 70, the sigma level corresponding
c  to 700mb (assume the surface pressure is 1000mb) is
c  the 13th level from the surface
c   Runhua Yang Aug. 24 98
c  added pcheck for 700mb - sharon sept 18, 1998
c-------------------------------------------------------

        pup = 700.
        do L = lm, 1, -1
          pcheck = (1000.-ptop)*sig(L) + ptop
          if (pcheck .ge. pup) then
            lup = L
          endif
        enddo
        do i = 1,istrip
          snowcrit=0
          do l=lup,lm
            if (saveth(i,l)*plk(i,l).le. tice ) then
             snowcrit=snowcrit+1
            endif
          enddo
          if (snowcrit .eq. (lm-lup+1)) then
            tmp1(i,3) = prep(i)
            tmp1(i,1)=0.0
            tmp1(i,2)=0.0
          endif
        enddo
  
      CALL paste (tmp1(1,1), lsp_new,ISTRIP,im*jm,1,NN)
      CALL paste (tmp1(1,2),conv_new,ISTRIP,im*jm,1,NN)
      CALL paste (tmp1(1,3),snow_new,ISTRIP,im*jm,1,NN)

      if(iprecon.gt.0) then
      CALL paste (pcnet,raincgath,ISTRIP,im*jm,1,NN)
      endif

C *********************************************************************
C ****               End Major Stripped Region                     ****
C *********************************************************************

 1000 CONTINUE

C Large Scale Rainfall, Conv rain, and snowfall
c ---------------------------------------------
      call back2grd ( lsp_new,pblindex, lsp_new,im*jm)
      call back2grd (conv_new,pblindex,conv_new,im*jm)
      call back2grd (snow_new,pblindex,snow_new,im*jm)

      if(iprecon.gt.0) then
      call back2grd (raincgath,pblindex,raincgath,im*jm)
      endif

c Subcloud Layer Pressure
c -----------------------
      if(ipsubcld.gt.0) then
      call back2grd (psubcldg ,pblindex,psubcldg ,im*jm)
      call back2grd (psubcldgc,pblindex,psubcldgc,im*jm)
      endif

      do L = 1,lm
C Delta theta,q, convective, max and ls clouds
c --------------------------------------------
       call back2grd (deltgather(1,L),pblindex, dtmoist(1,1,L)  ,im*jm)
       call back2grd (delqgather(1,L),pblindex, dqmoist(1,1,L,1),im*jm)
       call back2grd (    cpen(1,1,L),pblindex,    cpen(1,1,L)  ,im*jm)
       call back2grd (   cldls(1,1,L),pblindex,   cldls(1,1,L)  ,im*jm)
       call back2grd (cldwater(1,1,L),pblindex,cldwater(1,1,L)  ,im*jm)
       call back2grd ( pkzgather(1,L),pblindex, pkzgather(1,L)  ,im*jm)

C Diagnostics:
c ------------
       if(icldmas.gt.0)call back2grd(tmpgather(1,L),pblindex,
     .                                            tmpgather(1,L),im*jm)
       if(idtrain.gt.0)call back2grd(pkegather(1,L),pblindex,
     .                                            pkegather(1,L),im*jm)
       if(idtls.gt.0)call back2grd(deltrnev(1,L),pblindex,
     .                                             deltrnev(1,L),im*jm)
       if(idqls.gt.0)call back2grd(delqrnev(1,L),pblindex,
     .                                             delqrnev(1,L),im*jm)
       if(icldnp.gt.0)call back2grd(cldsr(1,1,L),pblindex,
     .                                              cldsr(1,1,L),im*jm)
      enddo

c Tracers
c -------
      do nt = 1,ntracer-ptracer
       do L = 1,lm
       call back2grd (delugather(1,L,nt),pblindex,
     .                                 dqmoist(1,1,L,ptracer+nt),im*jm)
       enddo
      enddo


C **********************************************************************
C                          BUMP DIAGNOSTICS
C **********************************************************************

c Determine Level Indices for Low-Mid-High Cloud Regions
c ------------------------------------------------------
      lowlevel = lm
      midlevel = lm
      do L = lm-1,1,-1
      pcheck = (1000.-ptop)*sig(l) + ptop
      if (pcheck.gt.700.0) lowlevel = L
      if (pcheck.gt.400.0) midlevel = L
      enddo

 
c Clear-Sky (Above 400mb) Temperature
c -----------------------------------
      if( itmpuclr.ne.0 .or. isphuclr.ne.0 ) then
      do j = 1,jm
      do i = 1,im
      totcld(i,j) = 0.0
      enddo
      enddo
      do L = 1,midlevel
      do j = 1,jm
      do i = 1,im
       if(cldls(i,j,L).ne.0.0.or.cpen(i,j,L).ne.0.0)totcld(i,j) = 1.0
      enddo
      enddo
      enddo
      do L = 1,lm
       if( itmpuclr.ne.0 ) then
        do i = 1,im*jm
        if( totcld(i,1).eq.0.0 ) then
         qdiag(i,1,itmpuclr +L-1) = qdiag(i,1,itmpuclr +L-1) + 
     .                                          tz(i,1,L)*pkzgather(i,L)
         qdiag(i,1,itmpuclrc+L-1) = qdiag(i,1,itmpuclrc+L-1) + 1.0
        endif
        enddo
       endif
 
       if( isphuclr.ne.0 ) then
        do i = 1,im*jm
        if( totcld(i,1).eq.0.0 ) then
         qdiag(i,1,isphuclr +L-1) = qdiag(i,1,isphuclr +L-1) + 
     .                                                qz(i,1,L,1)*1000.0
         qdiag(i,1,isphuclrc+L-1) = qdiag(i,1,isphuclrc+L-1) + 1.0
        endif
        enddo
       endif
      enddo
      endif

c Sub-Cloud Layer
c -------------------------
      if( ipsubcld.ne.0 ) then
      do j = 1,jm
      do i = 1,im
      qdiag(i,j,ipsubcld ) = qdiag(i,j,ipsubcld ) + psubcldg (i,j)
      qdiag(i,j,ipsubcldc) = qdiag(i,j,ipsubcldc) + psubcldgc(i,j)
      enddo
      enddo
      endif
 
c Non-Precipitating Cloud Fraction
c --------------------------------
      if( icldnp.ne.0 ) then
      do L = 1,lm
      do j = 1,jm
      do i = 1,im
      qdiag(i,j,icldnp+L-1) = qdiag(i,j,icldnp+L-1) + cldsr(i,j,L)
      enddo
      enddo
      enddo
      ncldnp = ncldnp + 1
      endif

c Moist Processes Heating Rate
c ----------------------------
      if(imoistt.gt.0) then
      do L = 1,lm
      do i = 1,im*jm
      qdiag(i,1,imoistt+L-1) = qdiag(i,1,imoistt+L-1) + 
     .                      (dtmoist(i,1,L)*sday*pkzgather(i,L)/pz(i,1))
      enddo
      enddo
      endif

c Moist Processes Moistening Rate
c -------------------------------
      if(imoistq.gt.0) then
      do L = 1,lm
      do j = 1,jm
      do i = 1,im
      qdiag(i,j,imoistq+L-1) = qdiag(i,j,imoistq+L-1) + 
     .                           (dqmoist(i,j,L,1)*sday*1000.0/pz(i,j))
      enddo
      enddo
      enddo
      endif

c Cloud Mass Flux
c ---------------
      if(icldmas.gt.0) then
      do L = 1,lm
      do i = 1,im*jm
      qdiag(i,1,icldmas+L-1) = qdiag(i,1,icldmas+L-1) + tmpgather(i,L)
      enddo
      enddo
      endif

c Detrained Cloud Mass Flux
c -------------------------
      if(idtrain.gt.0) then
      do L = 1,lm
      do i = 1,im*jm
      qdiag(i,1,idtrain+L-1) = qdiag(i,1,idtrain+L-1) + pkegather(i,L)
      enddo
      enddo
      endif

c Grid-Scale Condensational Heating Rate
c --------------------------------------
      if(idtls.gt.0) then
      do L = 1,lm
      do i = 1,im*jm
      qdiag(i,1,idtls+L-1) = qdiag(i,1,idtls+L-1) + deltrnev(i,L)
      enddo
      enddo
      endif

c Grid-Scale Condensational Moistening Rate
c -----------------------------------------
      if(idqls.gt.0) then
      do L = 1,lm
      do i = 1,im*jm
      qdiag(i,1,idqls+L-1) = qdiag(i,1,idqls+L-1) + delqrnev(i,L)
      enddo
      enddo
      endif

c Total Precipitation
c -------------------
      if(ipreacc.gt.0) then
      do j = 1,jm
      do i = 1,im
      qdiag(i,j,ipreacc) = qdiag(i,j,ipreacc)
     .                   +  (  lsp_new(I,j)
     .                      + snow_new(I,j)
     .                      + conv_new(i,j) ) *sday*tminv
      enddo
      enddo
      endif

c Convective Precipitation
c ------------------------
      if(iprecon.gt.0) then
      do i = 1,im*jm
      qdiag(i,1,iprecon) = qdiag(i,1,iprecon) + raincgath(i)*sday*tminv
      enddo
      endif

C **********************************************************************
C ****   Fill Rainfall and Snowfall Arrays for Land Surface Model   ****
C ****        Note:  Precip Rates work when DT(turb)<DT(moist)      ****
C **********************************************************************

      do j = 1,jm
      do i = 1,im
       rainlsp (i,j) = rainlsp (i,j) +  lsp_new(i,j)*tminv
       rainconv(i,j) = rainconv(i,j) + conv_new(i,j)*tminv
       snowfall(i,j) = snowfall(i,j) + snow_new(i,j)*tminv
      enddo
      enddo

C **********************************************************************
C ***        Compute Time-averaged Quantities for Radiation          ***
C ***               CPEN  => Cloud Fraction from RAS                 ***
C ***               CLDLS => Cloud Fraction from RNEVP               ***
C **********************************************************************

      do j = 1,jm
      do i = 1,im
          cldhi (i,j) = 0.
          cldmid(i,j) = 0.
          cldlow(i,j) = 0.
          cldtmp(i,j) = 0.
          cldprs(i,j) = 0.
         tmpimjm(i,j) = 0.
      enddo
      enddo

c Set Moist-Process Memory Coefficient
c ------------------------------------
      cldras_mem = 1.0-tmstp/ 3600.0
      cldlsp_mem = 1.0-tmstp/(3600.0*3)

      do L = 1,lm
      do i = 1,im*jm
       plev = sig(L)*pz(i,1)+ptop
 
c Compute Time-averaged Cloud and Water Amounts for Longwave Radiation
c --------------------------------------------------------------------
       watnow = cldwater(i,1,L)
       if( plev.le.500.0 ) then
        cldras = min( max( cldras_lw(i,1,L)*cldras_mem,cpen(i,1,L)),1.0)
       else
        cldras = 0.0
       endif
       cldlsp = min( max( cldlsp_lw(i,1,L)*cldlsp_mem,cldls(i,1,L)),1.0)

       if( cldras.lt.cldmin ) cldras = 0.0
       if( cldlsp.lt.cldmin ) cldlsp = 0.0

       cldnow = max( cldlsp,cldras )

       lwlz(i,1,L) = ( nlwlz*lwlz(i,1,L) + watnow)/(nlwlz +1)
       cldtot_lw(i,1,L) = (nlwcld*cldtot_lw(i,1,L) + cldnow)/(nlwcld+1)
       cldlsp_lw(i,1,L) = (nlwcld*cldlsp_lw(i,1,L) + cldlsp)/(nlwcld+1)
       cldras_lw(i,1,L) = (nlwcld*cldras_lw(i,1,L) + cldras)/(nlwcld+1)

 
c Compute Time-averaged Cloud and Water Amounts for Shortwave Radiation
c ---------------------------------------------------------------------
       watnow = cldwater(i,1,L)
       if( plev.le.500.0 ) then
        cldras = min( max(cldras_sw(i,1,L)*cldras_mem, cpen(i,1,L)),1.0)
       else
        cldras = 0.0
       endif
       cldlsp = min( max(cldlsp_sw(i,1,L)*cldlsp_mem,cldls(i,1,L)),1.0)

       if( cldras.lt.cldmin ) cldras = 0.0
       if( cldlsp.lt.cldmin ) cldlsp = 0.0

       cldnow = max( cldlsp,cldras )

       swlz(i,1,L) = ( nswlz*swlz(i,1,L) + watnow)/(nswlz +1)
       cldtot_sw(i,1,L) = (nswcld*cldtot_sw(i,1,L) + cldnow)/(nswcld+1)
       cldlsp_sw(i,1,L) = (nswcld*cldlsp_sw(i,1,L) + cldlsp)/(nswcld+1)
       cldras_sw(i,1,L) = (nswcld*cldras_sw(i,1,L) + cldras)/(nswcld+1)

 
c Compute Instantaneous Low-Mid-High Maximum Overlap Cloud Fractions
c ----------------------------------------------------------------------

       if( L.lt.midlevel ) cldhi (i,1) = max( cldnow,cldhi (i,1) )
       if( L.ge.midlevel  .and.
     .      L.lt.lowlevel ) cldmid(i,1) = max( cldnow,cldmid(i,1) )
       if( L.ge.lowlevel ) cldlow(i,1) = max( cldnow,cldlow(i,1) )

c Compute Cloud-Top Temperature and Pressure
c ------------------------------------------
       cldtmp(i,1) =  cldtmp(i,1) + cldnow*pkzgather(i,L)
     .             *  ( tz(i,1,L) + dtmoist(i,1,L)*tmstp/pz(i,1) )
       cldprs(i,1) =  cldprs(i,1) + cldnow*plev
       tmpimjm(i,1) = tmpimjm(i,1) + cldnow

      enddo
      enddo
 
c Compute Instantanious Total 2-D Cloud Fraction
c ----------------------------------------------
      do j = 1,jm
      do i = 1,im
      totcld(i,j) = 1.0 - (1.-cldhi (i,j))
     .                  * (1.-cldmid(i,j))
     .                  * (1.-cldlow(i,j))
      enddo
      enddo
       

C **********************************************************************
C ***       Fill Cloud Top Pressure and Temperature Diagnostic       ***
C **********************************************************************

      if(icldtmp.gt.0) then
      do j = 1,jm
      do i = 1,im
         if( cldtmp(i,j).gt.0.0 ) then
         qdiag(i,j,icldtmp) = qdiag(i,j,icldtmp) + 
     .                       cldtmp(i,j)*totcld(i,j)/tmpimjm(i,j)
         qdiag(i,j,icttcnt) = qdiag(i,j,icttcnt) + totcld(i,j)
         endif
      enddo
      enddo
      endif

      if(icldprs.gt.0) then
      do j = 1,jm
      do i = 1,im
         if( cldprs(i,j).gt.0.0 ) then
         qdiag(i,j,icldprs) = qdiag(i,j,icldprs) + 
     .                       cldprs(i,j)*totcld(i,j)/tmpimjm(i,j)
         qdiag(i,j,ictpcnt) = qdiag(i,j,ictpcnt) + totcld(i,j)
         endif
      enddo
      enddo
      endif
   
C **********************************************************************
C ****                      INCREMENT COUNTERS                      ****
C **********************************************************************
 
       nlwlz    = nlwlz    + 1
       nswlz    = nswlz    + 1

       nlwcld   = nlwcld   + 1
       nswcld   = nswcld   + 1

       nmoistt  = nmoistt  + 1
       nmoistq  = nmoistq  + 1
       npreacc  = npreacc  + 1
       nprecon  = nprecon  + 1

       ncldmas  = ncldmas  + 1
       ndtrain  = ndtrain  + 1

       ndtls  = ndtls  + 1
       ndqls  = ndqls  + 1
 
      RETURN
      END
      SUBROUTINE RAS( NN, LEN, LENC, K, NLTOP, nlayr, DT
     *,               UOI, ntracer, POI, QOI, PRS, PRJ, rnd, ncrnd
     *,               RAINS, CLN, CLF, cldmas, detrain
     *,               cp,grav,rkappa,alhl,rhfrac,rasmax )
C
C*********************************************************************
C*********************** ARIES   MODEL *******************************
C********************* SUBROUTINE  RAS   *****************************
C********************** 16 MARCH   1988 ******************************
C*********************************************************************
C
      PARAMETER (KRMIN=01)
      PARAMETER (ICM=1000)
      PARAMETER (CMB2PA=100.0)
      PARAMETER (rknob = 10.)
C
      integer ntracer
      integer nltop,nlayr
      DIMENSION UOI(len,nlayr,ntracer),   POI(len,K)
      DIMENSION QOI(len,K), PRS(len,K+1), PRJ(len,K+1)
      dimension rnd(ncrnd)
C
      DIMENSION RAINS(len,K), CLN(len,K), CLF(len,K)
      DIMENSION cldmas(len,K), detrain(len,K)
      DIMENSION TCU(len,K), QCU(len,K)
      real ucu(len,K,ntracer)
      DIMENSION ALF(len,K), BET(len,K), GAM(len,K)
     *,         ETA(len,K), HOI(len,K)
     *,         PRH(len,K), PRI(len,K)
      DIMENSION HST(len,K), QOL(len,K), GMH(len,K)
 
      DIMENSION TX1(len), TX2(len), TX3(len), TX4(len), TX5(len)
     *,         TX6(len), TX7(len), TX8(len), TX9(len)
     *,         TX11(len), TX12(len), TX13(len), TX14(len,ntracer)
     *,         TX15(len), TX16(len)
     *,         WFN(len), IA1(len), IA2(len), IA3(len)
      DIMENSION cloudn(len), pcu(len)

      real rhfrac(len),rasmax
 
      DIMENSION IC(ICM),   IRND(icm)
      dimension cmass(len,K)
      LOGICAL SETRAS
 
         do L = 1,k
         do I = 1,LENC
         rains(i,l) = 0.
         enddo
         enddo
 
      p00 = 1000.
      crtmsf = 0.

C The numerator here is the fraction of the subcloud layer mass flux
C      allowed to entrain into the cloud

CCC   FRAC = 1./dt
      FRAC = 0.5/dt

      KM1    = K  - 1
      KP1    = K  + 1
C we want the ras adjustment time scale to be one hour (indep of dt)
      RASBLF = 1./3600.
C
       KPRV  = KM1
C Removed KRMAX parameter
       KCR   = MIN(KM1,nlayr-2)     
       KFX   = KM1 - KCR
       NCMX  = KFX + NCRND
C
       IF (KFX .GT. 0) THEN
        DO NC=1,KFX
         IC(NC) = K - NC
        ENDDO
       ENDIF
C
      IF (NCRND .GT. 0) THEN
       DO I=1,ncrnd
        IRND(I) = (RND(I)-0.0005)*(KCR-KRMIN+1)
        IRND(I) = IRND(I) + KRMIN
       ENDDO
C
       DO NC=1,NCRND
        IC(KFX+NC) = IRND(NC)
       ENDDO
      ENDIF
C
      DO 100 NC=1,NCMX
C
       IF (NC .EQ. 1 ) THEN
        SETRAS = .TRUE.
       ELSE
        SETRAS = .FALSE.
       ENDIF
       IB = IC(NC)

c Initialize Cloud Fraction Array
c -------------------------------
      do i = 1,lenc
      cloudn(i) = 0.0
      enddo

       CALL CLOUD(nn,LEN, LENC, K, NLTOP, nlayr, IB, RASBLF,SETRAS,FRAC
     *,           CP,  ALHL, RKAPPA, GRAV, P00, CRTMSF
     *,           POI, QOI, UOI, Ntracer, PRS, PRJ
     *,           PCU, CLOUDN, TCU, QCU, UCU, CMASS
     *,           ALF, BET, GAM, PRH, PRI, HOI, ETA
     *,           HST, QOL, GMH
     *,           TX1, TX2, TX3, TX4, TX5, TX6, TX7, TX8, TX9
     *,           WFN, TX11, TX12, TX13, TX14, TX15
     *,           IA1,IA2,IA3,rhfrac)

C Compute fraction of grid box into which rain re-evap occurs (clf)
c -----------------------------------------------------------------
      do i = 1,lenc

c mass in detrainment layer
c -------------------------
       tx1(i) = cmb2pa *  (prs(i,ib+1) - prs(i,ib))/(grav*dt)

c ratio of detraining cloud mass to mass in detrainment layer
c -----------------------------------------------------------
       tx1(i) = rhfrac(i)*rknob * cmass(i,ib) / tx1(i)
       if(cmass(i,K).gt.0.) clf(i,ib) = clf(i,ib) + tx1(i)
       if( clf(i,ib).gt.1.) clf(i,ib) = 1.
      enddo
 
c Compute Total Cloud Mass Flux
c *****************************
      do L=ib,k
      do i=1,lenc
       cmass(i,L) = rhfrac(i)*cmass(i,L) * dt
      enddo
      enddo
         
      do L=ib,k
      do i=1,lenc
       cldmas(i,L) = cldmas(i,L) + cmass(i,L)
      enddo
      enddo

      do i=1,lenc
      detrain(i,ib) = detrain(i,ib) + cmass(i,ib)
      enddo

      DO L=IB,K
       DO I=1,LENC
        POI(I,L) = POI(I,L) + TCU(I,L) * DT * rhfrac(i)
        QOI(I,L) = QOI(I,L) + QCU(I,L) * DT * rhfrac(i)
       ENDDO
      ENDDO
      DO NT=1,Ntracer
      DO L=IB,K
       DO I=1,LENC
        UOI(I,L+nltop-1,NT)=UOI(I,L+nltop-1,NT)+UCU(I,L,NT)*DT*rhfrac(i)
       ENDDO
      ENDDO
      ENDDO
      DO I=1,LENC
       rains(I,ib) = rains(I,ib) + PCU(I)*dt * rhfrac(i)
      ENDDO

  100 CONTINUE
 
c Fill Convective Cloud Fractions based on 3-D Rain Amounts
c ---------------------------------------------------------
      do L=k-1,1,-1
      do i=1,lenc
         tx1(i)   = 100*(prs(i,L+1)-prs(i,L))/grav
         cln(i,L) = min(1600*rains(i,L)/tx1(i),rasmax )
      enddo
      enddo

      RETURN
      END

      subroutine rndcloud (iras,nrnd,rnd,myid)
      implicit none
      integer n,iras,nrnd,myid
      real random_numbx
      real rnd(nrnd)
      integer irm
      parameter (irm = 1000)
      real random(irm)
      integer i,mcheck,numrand,iseed,index
      logical first
      data    first /.true./
      integer iras0
      data    iras0 /0/
      save random, iras0
 
      if(nrnd.eq.0.)then
       do i = 1,nrnd
        rnd(i) = 0
       enddo
       if(first .and. myid.eq.0) print *,' NO RANDOM CLOUDS IN RAS '
       go to 100
      endif

      mcheck = mod(iras-1,irm/nrnd)

c First Time In From a Continuing RESTART (IRAS.GT.1) or Reading a New RESTART
c ----------------------------------------------------------------------------
      if( first.and.(iras.gt.1) .or. iras.ne.iras0+1 )then
       if( myid.eq.0 ) print *, 'Recreating Rand Numb Array in RNDCLOUD'
       if( myid.eq.0 ) print *, 'IRAS: ',iras,'  IRAS0: ',iras0
       numrand = mod(iras,irm/nrnd) * nrnd
       iseed   = iras * nrnd - numrand
       call random_seedx(iseed)
       do i = 1,irm
        random(i) = random_numbx()
       enddo
       index = (iras-1)*nrnd

c Multiple Time In But have Used Up all 1000 numbers (MCHECK.EQ.0)
c ----------------------------------------------------------------
      else if (mcheck.eq.0) then
          iseed = (iras-1)*nrnd
          call random_seedx(iseed)
          do i = 1,irm
           random(i) = random_numbx()
          enddo
          index = iseed

c Multiple Time In But have NOT Used Up all 1000 numbers (MCHECK.NE.0)
c --------------------------------------------------------------------
      else
          index = (iras-1)*nrnd
      endif

          index = mod(index,irm)
      if( index+nrnd.gt.1000 ) index=1000-nrnd
 
      do n = 1,nrnd
       rnd(n) = random(index+n)
      enddo
 
 100  continue
      first = .false.
      iras0 = iras
      return
      end

      real function random_numbx()
      implicit none
#if CRAY
      real ranf
      random_numbx = ranf()
#endif
#if SGI
      real rand
      random_numbx = rand()
#endif
      return
      end
      subroutine random_seedx (iseed)
      implicit none
      integer  iseed
#if CRAY
      call ranset (iseed)
#endif
#if SGI
      integer*4   seed
                  seed = iseed
      call srand (seed)
#endif
      return
      end
 
      SUBROUTINE CLOUD(nn,LEN, LENC, K, NLTOP, nlayr, IC, RASALF, 
     *,                 SETRAS, FRAC
     *,                 CP,  ALHL, RKAP, GRAV, P00, CRTMSF
     *,                 POI, QOI, UOI, Ntracer, PRS,  PRJ
     *,                 PCU, CLN, TCU, QCU, UCU, CMASS
     *,                 ALF, BET, GAM, PRH, PRI, HOL, ETA
     *,                 HST, QOL, GMH
     *,                 TX1, TX2, TX3, TX4, TX5, TX6, TX7, TX8, ALM
     *,                 WFN, AKM, QS1, CLF, UHT, WLQ
     *,                 IA, I1, I2,rhfrac)
C
C*********************************************************************
C******************** Relaxed Arakawa-Schubert ***********************
C********************* Plug Compatible Version  **********************
C************************ SUBROUTINE CLOUD ***************************
C************************* 23 JULY    1992 ***************************
C*********************************************************************
C*********************************************************************
C*********************************************************************
C************************** Developed By *****************************
C**************************              *****************************
C************************ Shrinivas Moorthi **************************
C************************       and         **************************
C************************  Max J. Suarez *****************************
C************************                *****************************
C******************** Laboratory for Atmospheres *********************
C****************** NASA/GSFC, Greenbelt, MD 20771 *******************
C*********************************************************************
C*********************************************************************
C
C   The calculations of Moorthi and Suarez (1992, MWR) are 
C   contained in the CLOUD routine.
C   It is probably advisable, at least initially, to treat CLOUD
C   as a black box that computes the single cloud adjustments. RAS,
C   on the other hand, can be tailored to each GCMs configuration
C   (ie, number and placement of levels, nature of boundary layer,
C    time step and frequency with which RAS is called).
C
C
C  Input:
C  ------
C
C     LEN     : The inner dimension of update and input arrays.
C
C     LENC    : The run: the number of soundings processes in a single call.
C               RAS works on the first LENC of the LEN soundings
C               passed. This allows working on pieces of the world
C               say for multitasking, without declaring temporary arrays
C               and copying the data to and from them.  This is an f77
C               version. An F90 version would have to allow more
C               flexibility in the argument declarations.  Obviously
C               (LENC<=LEN).  
C 
C     K       : Number of vertical layers (increasing downwards).
C               Need not be the same as the number of layers in the
C               GCM, since it is the outer dimension. The bottom layer
C               (K) is the subcloud layer. 
C
C     IC      : Detrainment level to check for presence of convection
C
C     RASALF  : Relaxation parameter (< 1.) for present cloud-type
C
C     SETRAS  : Logical parameter to control re-calculation of
C               saturation specific humidity and mid level P**kappa
C
C     FRAC    : Fraction of the PBL (layer K) mass allowed to be used
C               by a cloud-type in time DT
C
C     CP      : Specific heat at constant pressure
C
C     ALHL    : Latent Heat of condensation
C
C     RKAP    : R/Cp, where R is the gas constant
C
C     GRAV    : Acceleration due to gravity
C
C     P00     : A reference pressure in hPa, useually 1000 hPa
C
C     CRTMSF  : Critical value of mass flux above which cloudiness at 
C               the detrainment layer of that cloud-type is assumed.
C               Affects only cloudiness calculation.
C
C     POI     : 2D array of dimension (LEN,K) containing potential
C               temperature. Updated but not initialized by RAS.
C
C     QOI     : 2D array of dimension (LEN,K) containing specific
C               humidity. Updated but not initialized by RAS.
C
C     UOI     : 3D array of dimension (LEN,K,NTRACER) containing tracers
C               Updated but not initialized by RAS.
C
C     PRS     : 2D array of dimension (LEN,K+1) containing pressure
C               in hPa at the interfaces of K-layers from top of the 
C               atmosphere to the bottom. Not modified.
C
C     PRJ     : 2D array of dimension (LEN,K+1) containing (PRS/P00) **
C               RKAP.  i.e. Exner function at layer edges. Not modified.
C
C     rhfrac  : 1D array of dimension (LEN) containing a rel.hum. scaling
C               fraction. Not modified.
C
C  Output:
C  -------
C
C     PCU     : 1D array of length LEN containing accumulated
C               precipitation in mm/sec.
C
C     CLN     : 2D array of dimension (LEN,K) containing cloudiness
C               Note:  CLN is bumped but NOT initialized
C
C     TCU     : 2D array of dimension (LEN,K) containing accumulated 
C               convective heating (K/sec). 
C
C     QCU     : 2D array of dimension (LEN,K) containing accumulated 
C               convective drying (kg/kg/sec).
C
C     CMASS   : 2D array of dimension (LEN,K) containing the
C               cloud mass flux (kg/sec). Filled from cloud top
C               to base.
C
C  Temporaries:
C
C      ALF, BET, GAM, ETA, PRH, PRI, HOI, HST, QOL, GMH are temporary
C               2D real arrays of dimension of at least (LENC,K) where LENC is 
C               the horizontal dimension over which convection is invoked.
C
C
C    TX1, TX2, TX3, TX4, TX5, TX6, TX7, TX8, TX9, AKM, QS1, CLF, UHT
C    VHT, WLQ  WFN are temporary real arrays of length at least LENC
C
C    IA, I1, and I2 are temporary integer arrays of length LENC 
C
C
C************************************************************************
C
C

      PARAMETER (DAYLEN=86400.0,  HALF=0.5,  ONE=1.0, ZERO=0.0)
      PARAMETER (CMB2PA=100.0)
      PARAMETER (RHMAX=0.9999)
C
      integer nltop,ntracer,nlayr
      DIMENSION POI(LEN,K),  QOI(LEN,K),  PRS(LEN,K+1)
     *,         PRJ(LEN,K+1)
     *,         TCU(LEN,K),  QCU(LEN,K),  CMASS(LEN,K), CLN(LEN)
      real uoi(len,nlayr,ntracer)
      DIMENSION ALF(LEN,K), BET(LEN,K),  GAM(LEN,K)
     *,         PRH(LEN,K), PRI(LEN,K)
      DIMENSION AKM(LENC),   WFN(LENC)
      DIMENSION HOL(LENC,K), QOL(LENC,K),  ETA(LENC,K), HST(LENC,K)
     *,         GMH(LENC,K), ALM(LENC),    WLQ(LENC),   QS1(LENC)
     *,         TX1(LENC),   TX2(LENC), TX3(LENC),   TX4(LENC)
     *,         TX5(LENC),   TX6(LENC), TX7(LENC),   TX8(LENC)
     *,         CLF(LENC),   PCU(LENC)
      DIMENSION IA(LENC),    I1(LENC),  I2(LENC)
      real      rhfrac(len)
      real ucu(len,k,ntracer),uht(len,ntracer)
      LOGICAL SETRAS

      integer nt
 
c Explicit Inline Directives
c --------------------------
#if CRAY
#if f77
cfpp$ expand (qsat)
#endif
#endif

      RKAPP1 = 1.0  + RKAP
      ONEBCP = 1.0  / CP
      ALBCP  = ALHL * ONEBCP
      ONEBG  = 1.0  / GRAV
      CPBG   = CP   * ONEBG
      TWOBAL = 2.0 / ALHL
C
      KM1 = K  - 1
      IC1 = IC + 1
C
C      SETTIING ALF, BET, GAM, PRH, AND PRI : DONE ONLY WHEN SETRAS=.T.
C

      IF (SETRAS) THEN

         DO 2050 L=1,K
         DO 2030 I=1,LENC
          PRH(I,L) = (PRJ(I,L+1)*PRS(I,L+1) - PRJ(I,L)*PRS(I,L))
     *                            / ((PRS(I,L+1)-PRS(I,L)) * RKAPP1)
 2030    CONTINUE
 2050    CONTINUE

         DO 2070 L=1,K
         DO 2060 I=1,LENC
          TX5(I) = POI(I,L) * PRH(I,L)
          TX1(I) = (PRS(I,L) + PRS(I,L+1)) * 0.5
          TX3(I) = TX5(I)
          CALL QSAT(TX3(I), TX1(I), TX2(I), TX4(I), .TRUE.)
          ALF(I,L) = TX2(I) - TX4(I) * TX5(I)
          BET(I,L) = TX4(I) * PRH(I,L)
          GAM(I,L) = 1.0 / ((1.0 + TX4(I)*ALBCP) * PRH(I,L))
          PRI(I,L) = (CP/CMB2PA) / (PRS(I,L+1) - PRS(I,L))
 2060    CONTINUE
 2070    CONTINUE

      ENDIF
C
C
      DO 10 L=1,K
      DO 10 I=1,LEN
      TCU(I,L) = 0.0
      QCU(I,L) = 0.0
      CMASS(I,L) = 0.0
   10 CONTINUE

      do nt = 1,ntracer
      do L=1,K
      do I=1,LENC
      ucu(I,L,nt) = 0.0
      enddo
      enddo
      enddo
C
      DO 30 I=1,LENC
      TX1(I)   = PRJ(I,K+1) * POI(I,K)
      QS1(I)   = ALF(I,K) + BET(I,K)*POI(I,K)
      QOL(I,K) = MIN(QS1(I)*RHMAX,QOI(I,K))

      HOL(I,K) = TX1(I)*CP + QOL(I,K)*ALHL
      ETA(I,K) = ZERO
      TX2(I)   = (PRJ(I,K+1) - PRJ(I,K)) * POI(I,K) * CP
   30 CONTINUE
C
      IF (IC .LT. KM1) THEN
         DO 3703 L=KM1,IC1,-1
         DO 50 I=1,LENC
         QS1(I)   = ALF(I,L) + BET(I,L)*POI(I,L)
         QOL(I,L) = MIN(QS1(I)*RHMAX,QOI(I,L))
C
         TEM1 = TX2(I) + PRJ(I,L+1) * POI(I,L) * CP 
         HOL(I,L) = TEM1 + QOL(I,L )* ALHL
         HST(I,L) = TEM1 + QS1(I)   * ALHL

         TX1(I)   = (PRJ(I,L+1) - PRJ(I,L)) * POI(I,L)
         ETA(I,L) = ETA(I,L+1) + TX1(I)*CPBG
         TX2(I)   = TX2(I)     + TX1(I)*CP
   50    CONTINUE
C
 3703    CONTINUE
      ENDIF


      DO 70 I=1,LENC
      HOL(I,IC) = TX2(I)
      QS1(I)    = ALF(I,IC) + BET(I,IC)*POI(I,IC)
      QOL(I,IC) = MIN(QS1(I)*RHMAX,QOI(I,IC)) 
c
      TEM1      = TX2(I) + PRJ(I,IC1) * POI(I,IC) * CP 
      HOL(I,IC) = TEM1 + QOL(I,IC) * ALHL
      HST(I,IC) = TEM1 + QS1(I)    * ALHL
C
      TX3(I   ) = (PRJ(I,IC1) - PRH(I,IC)) * POI(I,IC)
      ETA(I,IC) = ETA(I,IC1) + CPBG * TX3(I)
   70 CONTINUE
C
      DO 130 I=1,LENC
      TX2(I) = HOL(I,K)  - HST(I,IC)
      TX1(I) = ZERO

  130 CONTINUE
C
C     ENTRAINMENT PARAMETER
C
      DO 160 L=IC,KM1
      DO 160 I=1,LENC
      TX1(I) = TX1(I) + (HST(I,IC) - HOL(I,L)) * (ETA(I,L) - ETA(I,L+1))
  160 CONTINUE
C
      LEN1 = 0
      LEN2 = 0
      ISAV = 0
      DO 195 I=1,LENC
      IF (TX1(I) .GT. ZERO .AND. TX2(I) .GT. ZERO
     .                     .AND. rhfrac(i).ne.0.0 ) THEN
         LEN1      = LEN1 + 1
         IA(LEN1)  = I
         ALM(LEN1) = TX2(I) / TX1(I)
      ENDIF
  195 CONTINUE
C
      LEN2 = LEN1
      if (IC1 .lt. K) then
         DO 196 I=1,LENC
         IF (TX2(I) .LE. 0.0 .AND. (HOL(I,K) .GT. HST(I,IC1))
     .                       .AND. rhfrac(i).ne.0.0 ) THEN
            LEN2      = LEN2 + 1
            IA(LEN2)  = I
            ALM(LEN2) = 0.0
         ENDIF
  196    CONTINUE
      endif
C
      IF (LEN2 .EQ. 0) THEN 
         DO 5010 I=1,LENC*K
         HST(I,1) = 0.0
         QOL(I,1) = 0.0
 5010    CONTINUE
         DO 5020 I=1,LENC
         PCU(I) = 0.0
 5020    CONTINUE
         RETURN
      ENDIF
      LEN11 = LEN1 + 1
C
C     NORMALIZED MASSFLUX
C
      DO 250 I=1,LEN2
      ETA(I,K) = 1.0
      II       = IA(I)
      TX2(I)   = 0.5 * (PRS(II,IC) + PRS(II,IC1))
      TX4(I)   = PRS(II,K)
  250 CONTINUE
C
      DO 252 I=LEN11,LEN2
      WFN(I)   = 0.0
      II       = IA(I)
      IF (HST(II,IC1) .LT. HST(II,IC)) THEN
         TX6(I) = (HST(II,IC1)-HOL(II,K))/(HST(II,IC1)-HST(II,IC))
      ELSE
         TX6(I) = 0.0
      ENDIF
      TX2(I) = 0.5 * (PRS(II,IC1)+PRS(II,IC1+1)) * (1.0-TX6(I))
     *                             + TX2(I)      * TX6(I)
  252 CONTINUE
C
      CALL ACRITN(LEN2, TX2, TX4, TX3)
C
      DO 260 L=KM1,IC,-1
      DO 255 I=1,LEN2
      TX1(I) = ETA(IA(I),L)
  255 CONTINUE
      DO 260 I=1,LEN2
      ETA(I,L) = 1.0 + ALM(I) * TX1(I)
  260 CONTINUE
C
C     CLOUD WORKFUNCTION
C
      IF (LEN1 .GT. 0) THEN
         DO 270 I=1,LEN1
         II = IA(I)
         WFN(I) = - GAM(II,IC) * (PRJ(II,IC1) - PRH(II,IC))
     *                         *  HST(II,IC) * ETA(I,IC1)
  270    CONTINUE
      ENDIF
C
      DO 290 I=1,LEN2
      II = IA(I)
      TX1(I) = HOL(II,K)
  290 CONTINUE
C
      IF (IC1 .LE. KM1) THEN

         DO 380 L=KM1,IC1,-1
         DO 380 I=1,LEN2
         II = IA(I)
         TEM = TX1(I) + (ETA(I,L) - ETA(I,L+1)) * HOL(II,L)
C
         PCU(I) = PRJ(II,L+1) - PRH(II,L)
         TEM1   = ETA(I,L+1) * PCU(I)
         TX1(I) = TX1(I)*PCU(I)
C
         PCU(I) = PRH(II,L) - PRJ(II,L)
         TEM1   = (TEM1 + ETA(I,L) * PCU(I)) * HST(II,L)
         TX1(I) = TX1(I) + TEM*PCU(I)
C
         WFN(I) = WFN(I) + (TX1(I) - TEM1) * GAM(II,L)
         TX1(I) = TEM
  380    CONTINUE
      ENDIF
C
      LENA = 0
      IF (LEN1 .GT. 0) THEN
         DO 512 I=1,LEN1
         II = IA(I)
         WFN(I) = WFN(I) + TX1(I) * GAM(II,IC)*(PRJ(II,IC1)-PRH(II,IC))
     *                   - TX3(I)
         IF (WFN(I) .GT. 0.0) THEN
            LENA = LENA + 1
            I1(LENA) = IA(I)
            I2(LENA) = I
            TX1(LENA) = WFN(I)
            TX2(LENA) = QS1(IA(I))
            TX6(LENA) = 1.0
         ENDIF
  512    CONTINUE
      ENDIF
      LENB = LENA
      DO 515 I=LEN11,LEN2
      WFN(I) = WFN(I) - TX3(I)
      IF (WFN(I) .GT. 0.0 .AND. TX6(I) .GT. 0.0) THEN
         LENB = LENB + 1
         I1(LENB)  = IA(I)
         I2(LENB)  = I
         TX1(LENB) = WFN(I)
         TX2(LENB) = QS1(IA(I))
         TX4(LENB) = TX6(I)
      ENDIF
  515 CONTINUE
C
      IF (LENB .LE. 0) THEN
         DO 5030 I=1,LENC*K
         HST(I,1) = 0.0
         QOL(I,1) = 0.0
 5030    CONTINUE
         DO 5040 I=1,LENC
         PCU(I) = 0.0
 5040    CONTINUE
         RETURN
      ENDIF

C
      DO 516 I=1,LENB
      WFN(I) = TX1(I)
      QS1(I) = TX2(I)
  516 CONTINUE
C
      DO 520 L=IC,K
      DO 517 I=1,LENB
      TX1(I) = ETA(I2(I),L)
  517 CONTINUE
      DO 520 I=1,LENB
      ETA(I,L) = TX1(I)
  520 CONTINUE
C
      LENA1 = LENA + 1
C
      DO 510 I=1,LENA
      II = I1(I)
      TX8(I) = HST(II,IC) - HOL(II,IC)
  510 CONTINUE
      DO 530 I=LENA1,LENB
      II = I1(I)
      TX6(I) = TX4(I)
      TEM    = TX6(I) * (HOL(II,IC)-HOL(II,IC1)) + HOL(II,IC1)
      TX8(I) = HOL(II,K) - TEM

      TEM1   = TX6(I) * (QOL(II,IC)-QOL(II,IC1)) + QOL(II,IC1)
      TX5(I) = TEM    - TEM1 * ALHL
      QS1(I) = TEM1   + TX8(I)*(ONE/ALHL)
      TX3(I) = HOL(II,IC)
  530 CONTINUE
C
C
      DO 620 I=1,LENB
      II = I1(I)
      WLQ(I) = QOL(II,K) - QS1(I)     * ETA(I,IC)
      TX7(I) = HOL(II,K)
  620 CONTINUE
      DO NT=1,Ntracer
      DO 621 I=1,LENB
      II = I1(I)
      UHT(I,NT) = UOI(II,K+nltop-1,NT)-UOI(II,IC+nltop-1,NT) * ETA(I,IC)
  621 CONTINUE
      ENDDO
C
      DO 635 L=KM1,IC,-1
      DO 630 I=1,LENB
      II = I1(I)
      TEM    = ETA(I,L) - ETA(I,L+1)
      WLQ(I) = WLQ(I) + TEM * QOL(II,L)
  630 CONTINUE
  635 CONTINUE
      DO NT=1,Ntracer
      DO L=KM1,IC,-1
      DO I=1,LENB
      II = I1(I)
      TEM    = ETA(I,L) - ETA(I,L+1)
      UHT(I,NT) = UHT(I,NT) + TEM * UOI(II,L+nltop-1,NT)
      ENDDO
      ENDDO
      ENDDO
C
C     CALCULATE GS AND PART OF AKM (THAT REQUIRES ETA)
C
      DO 690 I=1,LENB
      II = I1(I)
c     TX7(I)     = HOL(II,K)
      TEM        = (POI(II,KM1) - POI(II,K)) / (PRH(II,K) - PRH(II,KM1))
      HOL(I,K)   = TEM * (PRJ(II,K)-PRH(II,KM1))*PRH(II,K)*PRI(II,K)
      HOL(I,KM1) = TEM * (PRH(II,K)-PRJ(II,K))*PRH(II,KM1)*PRI(II,KM1)
      AKM(I)     = ZERO
      TX2(I)     = 0.5 * (PRS(II,IC) + PRS(II,IC1))
  690 CONTINUE

      IF (IC1 .LE. KM1) THEN
         DO 750 L=KM1,IC1,-1
         DO 750 I=1,LENB
         II = I1(I)
         TEM      = (POI(II,L-1) - POI(II,L)) * ETA(I,L)
     *                            / (PRH(II,L) - PRH(II,L-1))
C
         HOL(I,L)   = TEM * (PRJ(II,L)-PRH(II,L-1)) * PRH(II,L)
     *                    *  PRI(II,L)  + HOL(I,L)
         HOL(I,L-1) = TEM * (PRH(II,L)-PRJ(II,L)) * PRH(II,L-1)
     *                                              * PRI(II,L-1)
C
         AKM(I)   = AKM(I) - HOL(I,L)
     *            * (ETA(I,L)   * (PRH(II,L)-PRJ(II,L)) +
     *               ETA(I,L+1) * (PRJ(II,L+1)-PRH(II,L))) / PRH(II,L)
  750    CONTINUE
      ENDIF
C
C
      CALL RNCL(LENB, TX2, TX1, CLF)

      DO 770 I=1,LENB
      TX2(I) = (ONE - TX1(I)) * WLQ(I)
      WLQ(I) = TX1(I) * WLQ(I)
C
      TX1(I) = HOL(I,IC)
  770 CONTINUE
      DO 790 I=LENA1, LENB
      II = I1(I)
      TX1(I) = TX1(I) + (TX5(I)-TX3(I)+QOL(II,IC)*ALHL)*(PRI(II,IC)/CP)
  790 CONTINUE

      DO 800 I=1,LENB
      HOL(I,IC) = TX1(I) - TX2(I) * ALBCP * PRI(I1(I),IC)
  800 CONTINUE

      IF (LENA .GT. 0) THEN
         DO 810 I=1,LENA
         II = I1(I)
         AKM(I) = AKM(I) - ETA(I,IC1) * (PRJ(II,IC1) - PRH(II,IC)) 
     *                                * TX1(I) / PRH(II,IC)
  810    CONTINUE
      ENDIF
c
C     CALCULATE GH
C
      DO 830 I=1,LENB
      II = I1(I)
      TX3(I)   =  QOL(II,KM1) - QOL(II,K)
      GMH(I,K) = HOL(I,K) + TX3(I) * PRI(II,K) * (ALBCP)

      AKM(I)   = AKM(I) + GAM(II,KM1)*(PRJ(II,K)-PRH(II,KM1)) 
     *                               * GMH(I,K)
      TX3(I)   =  zero
  830 CONTINUE
C
      IF (IC1 .LE. KM1) THEN
         DO 840 L=KM1,IC1,-1
         DO 840 I=1,LENB
         II = I1(I)
         TX2(I) = TX3(I)
         TX3(I) = (QOL(II,L-1) - QOL(II,L)) * ETA(I,L)
         TX2(I) = TX2(I) + TX3(I)
C
         GMH(I,L) = HOL(I,L) + TX2(I)   * PRI(II,L) * (ALBCP*HALF)
  840    CONTINUE
C
C
      ENDIF
      DO 850 I=LENA1,LENB
      TX3(I) = TX3(I) + TWOBAL
     *       * (TX7(I) - TX8(I) - TX5(I) - QOL(I1(I),IC)*ALHL)
  850 CONTINUE
      DO 860 I=1,LENB
      GMH(I,IC) = TX1(I) + PRI(I1(I),IC) * ONEBCP
     *          * (TX3(I)*(ALHL*HALF) + ETA(I,IC) * TX8(I))
  860 CONTINUE
C
C     CALCULATE HC PART OF AKM
C
      IF (IC1 .LE. KM1) THEN
         DO 870 I=1,LENB
         TX1(I) = GMH(I,K)
  870    CONTINUE
         DO 3725 L=KM1,IC1,-1
         DO 880 I=1,LENB
         II = I1(I)
         TX1(I) = TX1(I) + (ETA(I,L) - ETA(I,L+1)) * GMH(I,L)
         TX2(I) = GAM(II,L-1) * (PRJ(II,L) - PRH(II,L-1))
  880    CONTINUE
C
         IF (L .EQ. IC1) THEN
            DO 890 I=LENA1,LENB
            TX2(I) = ZERO
  890       CONTINUE
         ENDIF
         DO 900 I=1,LENB
         II = I1(I)
         AKM(I) = AKM(I) + TX1(I) * 
     *          (TX2(I) + GAM(II,L)*(PRH(II,L)-PRJ(II,L)))
  900    CONTINUE
 3725    CONTINUE
      ENDIF
C
      DO 920 I=LENA1,LENB
      II = I1(I)
      TX2(I) = 0.5 * (PRS(II,IC) + PRS(II,IC1))
     *       + 0.5*(PRS(II,IC+2) - PRS(II,IC)) * (ONE-TX6(I))
c
      TX1(I) = PRS(II,IC1)
      TX5(I) = 0.5 * (PRS(II,IC1) + PRS(II,IC+2))
C
      IF ((TX2(I) .GE. TX1(I)) .AND. (TX2(I) .LT. TX5(I))) THEN
         TX6(I)     = ONE - (TX2(I) - TX1(I)) / (TX5(I) - TX1(I))
C
         TEM        = PRI(II,IC1) / PRI(II,IC)
         HOL(I,IC1) = HOL(I,IC1) + HOL(I,IC) * TEM
         HOL(I,IC)  = ZERO
C
         GMH(I,IC1) = GMH(I,IC1) + GMH(I,IC) * TEM
         GMH(I,IC)  = ZERO
      ELSEIF (TX2(I) .LT. TX1(I)) THEN
         TX6(I) = 1.0
      ELSE
         TX6(I) = 0.0
      ENDIF
  920 CONTINUE
C
C
      DO I=1,LENC
      PCU(I) = 0.0
      ENDDO

      DO 970 I=1,LENB
      II = I1(I)
      IF (AKM(I) .LT. ZERO .AND. WLQ(I) .GE. 0.0) THEN
         WFN(I) = - TX6(I) * WFN(I) * RASALF / AKM(I)
      ELSE
         WFN(I) = ZERO
      ENDIF
      TEM       = (PRS(II,K+1)-PRS(II,K))*(CMB2PA*FRAC)
      WFN(I)    = MIN(WFN(I), TEM)
C
C     compute cloud amount
C
CC    TX1(I) = CLN(II)
CC    IF (WFN(I) .GT. CRTMSF)  TX1(I) = TX1(I) + CLF(I)
CC    IF (TX1(I) .GT. ONE)  TX1(I) = ONE
C
C     PRECIPITATION
C
      PCU(II) =  WLQ(I) * WFN(I) * ONEBG
C
C     CUMULUS FRICTION AT THE BOTTOM LAYER
C
      TX4(I)   = WFN(I) * (1.0/ALHL)
      TX5(I)   = WFN(I) * ONEBCP
  970 CONTINUE
C
C     compute cloud mass flux for diagnostic output
C
      DO L = IC,K
      DO I=1,LENB
       II = I1(I)
       if(L.lt.K)then
       CMASS(II,L) =  ETA(I,L+1) * WFN(I) * ONEBG
       else
       CMASS(II,L) =  WFN(I) * ONEBG
       endif
      ENDDO
      ENDDO
C
CC    DO 975 I=1,LENB
CC    II = I1(I)
CC    CLN(II) = TX1(I)
CC975 CONTINUE
C
C     THETA AND Q CHANGE DUE TO CLOUD TYPE IC
C
 
c     TEMA = 0.0
c     TEMB = 0.0
      DO 990 L=IC,K
      DO 980 I=1,LENB
      II = I1(I)
      TEM      = (GMH(I,L) - HOL(I,L)) * TX4(I)
      TEM1     =  HOL(I,L) * TX5(I)
C
      TCU(II,L) = TEM1 / PRH(II,L)
      QCU(II,L) = TEM
  980 CONTINUE

c     I = I1(IP1)
c
c     TEM  = (PRS(I,L+1)-PRS(I,L)) * (ONEBG*100.0)
c     TEMA = TEMA +  TCU(I,L) * PRH(I,L) * TEM * (CP/ALHL)
c     TEMB = TEMB +  QCU(I,L)            * TEM
C
  990 CONTINUE
C
c Compute Tracer Tendencies
c -------------------------
      do nt = 1,ntracer

c Tracer Tendency at the Bottom Layer
c -----------------------------------
      DO 995 I=1,LENB
      II = I1(I)
      TEM    = half*TX5(I) * PRI(II,K)
      TX1(I) = (UOI(II,KM1+nltop-1,nt) - UOI(II,K+nltop-1,nt))
      ucu(II,K,nt) = TEM * TX1(I)
  995 CONTINUE

c Tracer Tendency at all other Levels
c -----------------------------------
      DO 1020 L=KM1,IC1,-1
      DO 1010 I=1,LENB
      II = I1(I)
      TEM = half*TX5(I) * PRI(II,L)
      TEM1   = TX1(I)
      TX1(I) = (UOI(II,L-1+nltop-1,nt)-UOI(II,L+nltop-1,nt)) * ETA(I,L)
      TX3(I) = (TX1(I) + TEM1) * TEM
 1010 CONTINUE
      DO 1020 I=1,LENB
      II = I1(I)
      ucu(II,L,nt) = TX3(I)
 1020 CONTINUE

      DO 1030 I=1,LENB
      II = I1(I)
      IF (TX6(I) .GE. 1.0) THEN
         TEM    = half*TX5(I) * PRI(II,IC)
      ELSE
         TEM = 0.0
      ENDIF
      TX1(I) = (TX1(I) + UHT(I,nt) + UHT(I,nt)) * TEM
 1030 CONTINUE
      DO 1040 I=1,LENB
      II = I1(I)
      ucu(II,IC,nt) = TX1(I)
 1040 CONTINUE

      enddo
C
C     PENETRATIVE CONVECTION CALCULATION OVER
C

      RETURN
      END
      SUBROUTINE RNCL(LEN, PL, RNO, CLF)
C
C
C*********************************************************************
C********************** Relaxed Arakawa-Schubert *********************
C************************   SUBROUTINE  RNCL  ************************
C**************************** 23 July 1992 ***************************
C*********************************************************************

      PARAMETER (P5=500.0,  P8=800.0, PT8=0.8, PT2=0.2)
      PARAMETER (PFAC=PT2/(P8-P5))
C
      PARAMETER (P4=400.0,    P6=401.0)
      PARAMETER (P7=700.0,    P9=900.0)
      PARAMETER (CUCLD=0.5,CFAC=CUCLD/(P6-P4))
C
      DIMENSION PL(LEN),  RNO(LEN), CLF(LEN)
 
      DO 10 I=1,LEN
                           rno(i) = 1.0
ccc   if( pl(i).le.400.0 ) rno(i) = max( 0.75, 1.0-0.0025*(400.0-pl(i)) )

ccc   IF ( PL(I).GE.P7 .AND. PL(I).LE.P9 ) THEN
ccc     RNO(I) = ((P9-PL(I))/(P9-P7)) **2
ccc   ELSE IF (PL(I).GT.P9) THEN
ccc     RNO(I) = 0.
ccc   ENDIF

      CLF(I) = CUCLD
C
CARIESIF (PL(I) .GE. P5 .AND. PL(I) .LE. P8) THEN
CARIES    RNO(I) = (P8-PL(I))*PFAC + PT8
CARIESELSEIF (PL(I) .GT. P8 ) THEN
CARIES    RNO(I) = PT8
CARIESENDIF
CARIES
      IF (PL(I) .GE. P4 .AND. PL(I) .LE. P6) THEN
         CLF(I) = (P6-PL(I))*CFAC
      ELSEIF (PL(I) .GT. P6 ) THEN
         CLF(I) = 0.0
      ENDIF
   10 CONTINUE
C
      RETURN
      END
      SUBROUTINE ACRITN ( LEN,PL,PLB,ACR )
 
C*********************************************************************
C********************** Relaxed Arakawa-Schubert *********************
C************************** SUBROUTINE  ACRIT    *********************
C******************  modified August 28, 1996   L.Takacs  ************
C****                                                            *****
C****  Note:  Data obtained from January Mean After-Analysis     *****
C****         from 4x5 46-layer GEOS Assimilation                *****
C****                                                            *****
C*********************************************************************
 
      real PL(LEN), PLB(LEN), ACR(LEN)

      parameter  (lma=18)
      real      p(lma)
      real      a(lma)

      data p  / 93.81, 111.65, 133.46, 157.80, 186.51,
     .         219.88, 257.40, 301.21, 352.49, 409.76,
     .         471.59, 535.04, 603.33, 672.79, 741.12,
     .         812.52, 875.31, 930.20/

      data a  / 3.35848, 3.13645, 2.48072, 2.08277, 1.53364,
     .          1.01971,  .65846,  .45867,  .38687,  .31002,
     .           .25574,  .20347,  .17254,  .15260,  .16756,
     .           .09916,  .10360,  .05880/

 
      do L=1,lma-1
      do i=1,len
         if( pl(i).ge.p(L)   .and.
     .       pl(i).le.p(L+1)) then
         temp = ( pl(i)-p(L) )/( p(L+1)-p(L) )
         acr(i) = a(L+1)*temp + a(L)*(1-temp)
         endif
      enddo
      enddo

      do i=1,len
      if( pl(i).lt.p(1)   ) acr(i) = a(1)
      if( pl(i).gt.p(lma) ) acr(i) = a(lma)
      enddo

      do i=1,len
      acr(i) = acr(i) * (plb(i)-pl(i))
      enddo

      RETURN
      END
       SUBROUTINE RNEVP(NN,IRUN,NLAY,TL,QL,RAIN,PL,CLFRAC,SP,DSIG,PLKE,
     1   PLK,TH,TEMP1,TEMP2,TEMP3,ITMP1,ITMP2,RCON,RLAR,CLSBTH,tmscl,
     2   tmfrc,cp,gravity,alhl,gamfac,cldlz,RHCRIT,offset,alpha)
 
      PARAMETER (ZM1P04 = -1.04E-4 )
      PARAMETER (ZERO = 0.)
      PARAMETER (TWO89= 2.89E-5)
      PARAMETER ( ZP44= 0.44)
      PARAMETER ( ZP01= 0.01)
      PARAMETER ( ZP1 = 0.1 )
      PARAMETER ( ZP001= 0.001)
      PARAMETER ( HALF= 0.5)
      PARAMETER ( ZP578 = 0.578 )
      PARAMETER ( ONE = 1.)
      PARAMETER ( THOUSAND = 1000.)
      PARAMETER ( Z3600 = 3600.)
C
       DIMENSION TL(IRUN,NLAY),QL(IRUN,NLAY),RAIN(IRUN,NLAY),
     $ PL(IRUN,NLAY),CLFRAC(IRUN,NLAY),SP(IRUN),TEMP1(IRUN,NLAY),
     $ TEMP2(IRUN,NLAY),PLKE(IRUN,NLAY),
     $ RCON(IRUN),RLAR(IRUN),DSIG(NLAY),PLK(IRUN,NLAY),TH(IRUN,NLAY),
     $ TEMP3(IRUN,NLAY),ITMP1(IRUN,NLAY),
     $ ITMP2(IRUN,NLAY),CLSBTH(IRUN,NLAY)
C
       DIMENSION EVP9(IRUN,NLAY)
       real water(irun),crystal(irun)
       real watevap(irun),iceevap(irun)
       real fracwat,fracice, tice,rh,fact,dum

       real cldlz(irun,nlay)
       real rhcrit(irun,nlay), rainmax(irun)
       real offset, alpha

c Explicit Inline Directives
c --------------------------
#if CRAY
#if f77
cfpp$ expand (qsat)
#endif
#endif

       tice = getcon('FREEZING-POINT')

       fracwat = 0.70
       fracice = 0.01

       NLAYM1 = NLAY - 1
       IRNLAY = IRUN*NLAY
       IRNLM1 = IRUN*(NLAY-1)
 
       RPHF = Z3600/tmscl
 
       ELOCP   = alhl/cp
       CPOG    = cp/gravity

       DO I = 1,IRUN
          RLAR(I) = 0.
         water(i) = 0.
       crystal(i) = 0.
       ENDDO

       do L = 1,nlay
       do i = 1,irun
          EVP9(i,L) = 0.
         TEMP1(i,L) = 0.
         TEMP2(i,L) = 0.
         TEMP3(i,L) = 0.
        CLSBTH(i,L) = 0.
         cldlz(i,L) = 0.
       enddo
       enddo
 
C RHO(ZERO) / RHO FOR TERMINAL VELOCITY APPROX.
c ---------------------------------------------
      DO L = 1,NLAY
      DO I = 1,IRUN
        TEMP2(I,L) = PL(I,L)*ZP001
        TEMP2(I,L) = SQRT(TEMP2(I,L))
      ENDDO
      ENDDO

C INVERSE OF MASS IN EACH LAYER
c -----------------------------
      DO L = 1,NLAY
      DO I = 1,IRUN
      TEMP3(I,L) = SP(I) * DSIG(L)
      TEMP3(I,L) = GRAVITY*ZP01 / TEMP3(I,L)
      ENDDO
      ENDDO
 
C DO LOOP FOR MOISTURE EVAPORATION ABILITY AND CONVEC EVAPORATION.
c ----------------------------------------------------------------
      DO 100 L=1,NLAY

      DO I = 1,IRUN
        TEMP1(I,3) = TL(I,L)
        TEMP1(I,4) = QL(I,L)
      ENDDO

      DO 50 N=1,2
       IF(N.EQ.1)RELAX=HALF
       IF(N.GT.1)RELAX=ONE

      DO I = 1,IRUN
       call qsat ( temp1(i,3),pl(i,L),temp1(i,2),temp1(i,6),.true. )
       TEMP1(I,5)=TEMP1(I,2)-TEMP1(I,4)
       TEMP1(I,6)=TEMP1(I,6)*ELOCP
       TEMP1(I,5)=TEMP1(I,5)/(ONE+TEMP1(I,6))
       TEMP1(I,4)=TEMP1(I,4)+TEMP1(I,5)*RELAX
       TEMP1(I,3)=TEMP1(I,3)-TEMP1(I,5)*ELOCP*RELAX
      ENDDO
  50  CONTINUE

      DO I = 1,IRUN
          EVP9(I,L) = (TEMP1(I,4) -  QL(I,L))/TEMP3(I,L)
C convective detrained water
         cldlz(i,L) = rain(i,L)*temp3(i,L)  
       if(  tl(i,L).gt.tice-20.) then
           water(i) =   water(i) + rain(i,L)
       else
         crystal(i) = crystal(i) + rain(i,L)
       endif
      ENDDO
 
C**********************************************************************
C  FOR CONVECTIVE PRECIP, FIND THE "EVAPORATION EFFICIENCY" USING     *
C                   KESSLERS PARAMETERIZATION                         *
C**********************************************************************
 
      DO 20 I=1,IRUN

        iceevap(i) = 0.
        watevap(i) = 0.

       if( (evp9(i,L).gt.0.) .and. (crystal(i).gt.0.) ) then
       iceevap(I) = EVP9(I,L)*fracice
       IF(iceevap(i).GE.crystal(i)) iceevap(i) = crystal(i)
       EVP9(I,L)=EVP9(I,L)-iceevap(I)
       crystal(i) = crystal(i) - iceevap(i)
       endif

C and now warm precipitate
       if( (evp9(i,L).gt.0.) .and. (water(i).gt.0.) ) then
       exparg    = ZM1P04*tmscl*((water(i)*RPHF*TEMP2(I,L))**ZP578)
       AREARAT = ONE-(EXP(EXPARG))
       watevap(I) = EVP9(I,L)*AREARAT*fracwat
       IF(watevap(I).GE.water(i)) watevap(I) = water(i)
       EVP9(I,L)=EVP9(I,L)-watevap(I)
       water(i) = water(i) - watevap(i)
       endif

       QL(I,L) = QL(I,L)+(iceevap(i)+watevap(i))*TEMP3(I,L)
       TL(I,L) = TL(I,L)-(iceevap(i)+watevap(i))*TEMP3(I,L)*ELOCP

  20  CONTINUE

  100 CONTINUE

      do i = 1,irun
      rcon(i) = water(i) + crystal(i)
      enddo

C**********************************************************************
C Large Scale Precip
C**********************************************************************

      DO 200 L=1,NLAY
      DO I = 1,IRUN
        rainmax(i) = rhcrit(i,L)*evp9(i,L) +
     .               ql(i,L)*(rhcrit(i,L)-1.)/temp3(i,L)

         if (rainmax(i).LE.0.0) then
           call qsat( tl(i,L),pl(i,L),rh,dum,.false.)
           rh = ql(i,L)/rh

           if( rhcrit(i,L).eq.1.0 ) then
           fact = 1.0
           else
           fact = min( 1.0, alpha + (1.0-alpha)*( rh-rhcrit(i,L)) /
     1                                          (1.0-rhcrit(i,L)) )
           endif

C Do not allow clouds above 10 mb
           if( pl(i,L).ge.10.0 ) CLSBTH(I,L) = fact  
           RLAR(I) = RLAR(I)-rainmax(I)
           QL(I,L) = QL(I,L)+rainmax(I)*TEMP3(I,L)
           TL(I,L) = TL(I,L)-rainmax(I)*TEMP3(I,L)*ELOCP
C Large-scale water
        cldlz(i,L) = cldlz(i,L) - rainmax(i)*temp3(i,L)  
         ENDIF
      ENDDO

      DO I=1,IRUN
         IF((RLAR(I).GT.0.0).AND.(rainmax(I).GT.0.0))THEN
          RPOW=(RLAR(I)*RPHF*TEMP2(I,L))**ZP578
          EXPARG  = ZM1P04*tmscl*RPOW
          AREARAT = ONE-(EXP(EXPARG))
          TEMP1(I,7) = rainmax(I)*AREARAT
          IF(TEMP1(I,7).GE.RLAR(I)) TEMP1(I,7) = RLAR(I)
          RLAR(I) =    RLAR(I)-TEMP1(I,7)
          QL(I,L) =    QL(I,L)+TEMP1(I,7)*TEMP3(I,L)
          TL(I,L) =    TL(I,L)-TEMP1(I,7)*TEMP3(I,L)*ELOCP
         ENDIF
      ENDDO

  200 CONTINUE

      RETURN
      END
      subroutine srclouds (th,q,plk,pl,plke,cloud,cldwat,irun,irise,
     1                                                rhc,offset,alpha)
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Compute non-precipitating cloud fractions
C    based on Slingo and Ritter (1985).
C    Remove cloudiness where conditionally unstable.
C
C  INPUT:
C  ======
C    th ......... Potential Temperature (irun,irise)
C    q .......... Specific  Humidity    (irun,irise)
C    plk ........ P**Kappa at mid-layer (irun,irise)
C    pl ......... Pressure at mid-layer (irun,irise)
C    plke ....... P**Kappa at edge      (irun,irise+1)
C    irun ....... Horizontal   dimension
C    irise ...... Vertical     dimension
C
C  OUTPUT:
C  =======
C    cloud ...... Cloud Fraction        (irun,irise)
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      implicit none
      integer  irun,irise

      real   th(irun,irise)
      real    q(irun,irise)
      real  plk(irun,irise)
      real   pl(irun,irise)
      real plke(irun,irise+1)

      real tempth(irun)
      real tempqs(irun)
      real dhstar(irun)
      real  cloud(irun,irise)
      real cldwat(irun,irise)
      real     qs(irun,irise)

      real cp, alhl, getcon, akap, pcheck
      real ratio, temp, pke, elocp
      real rhcrit,rh,dum,pbar,tbar
      integer i,L,ntradesu,ntradesl

      real factor
      real rhc(irun,irise)
      real offset,alpha

c Explicit Inline Directives
c --------------------------
#if CRAY
#if f77
cfpp$ expand (qsat)
#endif
#endif

      cp     = getcon('CP')
      alhl   = getcon('LATENT HEAT COND')
      elocp  = alhl/cp
      akap   = getcon('KAPPA')

      do L = 1,irise
      do i = 1,irun
                  temp = th(i,L)*plk(i,L)
      call qsat ( temp,pl(i,L),qs(i,L),dum,.false. )
      enddo
      enddo

      do L  = 2,irise
      do i  = 1,irun
      rh    = q(i,L)/qs(i,L)

      rhcrit = rhc(i,L) - offset
      ratio  = alpha*(rh-rhcrit)/offset
 
      if(cloud(i,L).eq.  0.0 .and. ratio.gt.0.0 ) then
         cloud(i,L) = min( ratio,1.0 )
      endif
 
      enddo
      enddo
 
c Reduce clouds from conditionally unstable layer
c -----------------------------------------------
      call ctei ( th,q,cloud,cldwat,pl,plk,plke,irun,irise )

      return
      end

      subroutine ctei ( th,q,cldfrc,cldwat,pl,plk,plke,im,lm )
      implicit none
      integer im,lm
      real  th(im,lm),q(im,lm),plke(im,lm+1),cldwat(im,lm)
      real plk(im,lm),pl(im,lm),cldfrc(im,lm)
      integer i,L
      real    getcon,cp,alhl,elocp,cpoel,t,p,s,qs,dqsdt,dq
      real    k,krd,kmm,f

      cp     = getcon('CP')
      alhl   = getcon('LATENT HEAT COND')
      cpoel  = cp/alhl
      elocp  = alhl/cp

      do L=lm,2,-1
      do i=1,im
          dq = q(i,L)+cldwat(i,L)-q(i,L-1)-cldwat(i,L-1)
      if( dq.eq.0.0 ) dq = 1.0e-20
      k = 1.0 + cpoel*plke(i,L)*( th(i,L)-th(i,L-1) ) / dq

      t = th(i,L)*plk(i,L)
      p = pl(i,L)
      call qsat ( t,p,qs,dqsdt,.true. )
   
      krd = ( cpoel*t*(1+elocp*dqsdt) )/( 1 + 1.608*dqsdt*t )

      kmm = ( 1+elocp*dqsdt )*( 1 + 0.392*cpoel*t )
     .    / ( 2+(1+1.608*cpoel*t)*elocp*dqsdt )

      s = ( (k-krd)/(kmm-krd) )
      f = 1.0 - min( 1.0, max(0.0,1.0-exp(-s)) )

      cldfrc(i,L) = cldfrc(i,L)*f
      cldwat(i,L) = cldwat(i,L)*f

      enddo
      enddo

      return
      end

      subroutine back2grd(gathered,indeces,scattered,irun)
      implicit none
      integer i,irun,indeces(irun)
      real gathered(irun),scattered(irun)
      real temp(irun)
      do i = 1,irun
       temp(indeces(i)) = gathered(i)
      enddo
      do i = 1,irun
       scattered(i) = temp(i)
      enddo
      return
      end
