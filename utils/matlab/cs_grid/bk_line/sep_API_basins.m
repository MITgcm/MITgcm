
krd=1; kpr=1; kgr=0; kwr=1;
%krd=0; kpr=0; kgr=1; kwr=0; % <- execute a 2nd time & draw some plot

if krd == 1,
%- set ncdf=1 to load MNC (NetCDF) grid-files ;
%   or ncdf=0 to load MDS (binary) grid-files :
 ncdf=0;
%rac='/home/jmc/grid_cs32/';
 rac='grid_files/';

%- load: bkl_Ylat, bkl_Npts, bkl_Flg, bkl_IJuv, bkl_Xsg, bkl_Ysg, bkl_Zon
 bk_lineF=[rac,'isoLat_cs32_59'];
 load(bk_lineF);
 ydim=length(bkl_Ylat); ydimC=ydim+1;
 fprintf([' load bk_line description from: ',bk_lineF,'.mat \n']);

%- load 'Nu','Nv','Iu','Iv':
 namf=[rac,'open_basins_section'];
 load(namf);
 fprintf(['       and API separation from: ',namf,'.mat : O.K.\n']);

 namf='maskC_bas.bin';
 fid=fopen([rac,namf],'r','b'); mskBasC=fread(fid,'real*4'); fclose(fid);

 G=load_grid(rac,0+ncdf);
 xcs=G.xC; ycs=G.yC; xcg=G.xG; ycg=G.yG; arc=G.rAc;
 ncx=G.dims(1); nc=G.dims(2); ncp=nc+1;
%mskBasC=rdda([rac,namf],[ncx nc 3],1,'real*4','b');
 mskBasC=reshape(mskBasC,[ncx nc 3]);

 xc1=reshape(xcs,1,ncx*nc);
 yc1=reshape(ycs,1,ncx*nc);
%xg1=reshape(xcg,1,ncx*nc);
%yg1=reshape(ycg,1,ncx*nc);
%xg6=split_Z_cub(xcg);
%yg6=split_Z_cub(ycg);
 xc6=split_C_cub(xcs,1);
 yc6=split_C_cub(ycs,1);

 hFacC=G.hFacC; hFacW=G.hFacW; hFacS=G.hFacS;
 mskC=min(1,hFacC(:,:,1)); mskC=ceil(mskC);
 mskW=min(1,hFacW(:,:,1)); mskW=ceil(mskW);
 mskS=min(1,hFacS(:,:,1)); mskS=ceil(mskS);
 mskW=reshape(mskW,1,ncx*nc);
 mskS=reshape(mskS,1,ncx*nc);

end

if kpr ==1,
 [xdum,xPA,yPA,xAI,yAI,xIP,yIP]=line_sep(0);

%- define Bas-Sep point (as U,V point) for each Lat-band
%  the list of U,V point
 MxSiz=ceil(nc*nc/2/ydimC);
 nbpSep=zeros(3,ydimC);
 ijSep=zeros(3,ydimC,MxSiz); typSep=zeros(3,ydimC,MxSiz);
 n2s=zeros(3,1); jl2s=zeros(3,MxSiz);
 xc2s=zeros(3,MxSiz,2); yc2s=zeros(3,MxSiz,2);
 for b=1:3,
  fprintf(' b= %i ; Nu,Nv= %3i %3i \n',b,Nu(b),Nv(b));
%-
  nU=Nu(b); I=Iu(b,1:nU); xSp=zeros(2,nU);
   i0=rem(I-1,ncx);j1=1+fix((I-1)/ncx);
   i1=1+rem(i0,nc); k1=1+fix(i0/nc);
   for i=1:nU,
    jl=1+bkl_Zon(I(i));
    nn=1+nbpSep(b,jl);
    nbpSep(b,jl)=nn;
    ijSep(b,jl,nn)=I(i);
    typSep(b,jl,nn)=1;
    if i1(i) == 1,
     xx=xc6(i1(i),1+j1(i),k1(i)); yy=yc6(i1(i),1+j1(i),k1(i));
     [Im]=find( xc1 == xx & yc1 == yy );
     if length(Im) ~= 1, fprintf(' stop A \n'); return ; end
    else Im=I(i)-1; end
    jj=1+bkl_Zon(Im);
    if jj ~= jl,
     nn=1+nbpSep(b,jj);
     nbpSep(b,jj)=nn;
     ijSep(b,jj,nn)=I(i);
     typSep(b,jj,nn)=-1;
     if (mskW(I(i)) == 1),
      nn=n2s(b)+1; n2s(b)=nn;
      jl2s(b,nn)=max(jj,jl);
      xc2s(b,nn,1)=xc1(Im);   yc2s(b,nn,1)=yc1(Im);
      xc2s(b,nn,2)=xc1(I(i)); yc2s(b,nn,2)=yc1(I(i));
     end
    end
    xSp(1,i)=xc6(1+i1(i),1+j1(i),k1(i));
    xSp(2,i)=xc6( i1(i), 1+j1(i),k1(i));
    ySepU(b,i)=(yc6(1+i1(i),1+j1(i),k1(i))+yc6(i1(i),1+j1(i),k1(i)))/2;
   end
   var=reshape(xcg,[1 ncx*nc]);
   xSp(1,:)=var(I)-180+rem(xSp(1,:)-var(I)+3*180,360);
   xSp(2,:)=var(I)-180+rem(xSp(2,:)-var(I)+3*180,360);
   xSepU(b,1:nU)=(xSp(1,:)+xSp(2,:))/2;
%-
  nV=Nv(b); J=Iv(b,1:nV); xSp=zeros(2,nV);
   i0=rem(J-1,ncx);j1=1+fix((J-1)/ncx);
   i1=1+rem(i0,nc); k1=1+fix(i0/nc);
   for i=1:nV,
    jl=1+bkl_Zon(J(i));
    nn=1+nbpSep(b,jl);
    nbpSep(b,jl)=nn;
    ijSep(b,jl,nn)=J(i);
    typSep(b,jl,nn)=2;
    if j1(i) == 1,
     xx=xc6(1+i1(i),j1(i),k1(i)); yy=yc6(1+i1(i),j1(i),k1(i));
     [Jm]=find( xc1 == xx & yc1 == yy );
     if length(Jm) ~= 1, fprintf(' stop B \n'); return ; end
    else Jm=J(i)-ncx; end
    jj=1+bkl_Zon(Jm);
    if jj ~= jl,
     nn=1+nbpSep(b,jj);
     nbpSep(b,jj)=nn;
     ijSep(b,jj,nn)=J(i);
     typSep(b,jj,nn)=-2;
     if (mskS(J(i)) == 1),
      nn=n2s(b)+1; n2s(b)=nn;
      jl2s(b,nn)=max(jj,jl);
      xc2s(b,nn,1)=xc1(Jm); yc2s(b,nn,1)=yc1(Jm);
      xc2s(b,nn,2)=xc1(J(i)); yc2s(b,nn,2)=yc1(J(i));
     end
    end
    xSp(1,i)=xc6(1+i1(i),1+j1(i),k1(i));
    xSp(2,i)=xc6(1+i1(i), j1(i), k1(i));
    ySepV(b,i)=(yc6(1+i1(i),1+j1(i),k1(i))+yc6(1+i1(i),j1(i),k1(i)))/2;
   end
   xSp(1,:)=var(J)-180+rem(xSp(1,:)-var(J)+3*180,360);
   xSp(2,:)=var(J)-180+rem(xSp(2,:)-var(J)+3*180,360);
   xSepV(b,1:nV)=(xSp(1,:)+xSp(2,:))/2;
 end
   NbpSep=max(nbpSep');
   fprintf(' NbpSep = %2i %2i %2i \n',NbpSep);
   fprintf(' Dbl Sep: %2i %2i %2i \n',n2s);

end

if kwr ==1,
 %- reduce size :
 mxSiz=max(NbpSep);
 ij_Sep=ijSep(:,:,1:mxSiz); tp_Sep=typSep(:,:,1:mxSiz); np_Sep=nbpSep;

 namf=['sepBas_cs',int2str(nc),'_',int2str(ydimC)];
 save(namf,'np_Sep','ij_Sep','tp_Sep');
 fprintf(['write Basin Separation on file:',namf,'.mat : O.K. \n']);
end

if kgr ==1,
 figure(1); clf;
 shift=-1; cbV=1; ccB=[0 0]; AxBx=[-182 182 -90 90];
 for bp=1:3, subplot(310+bp);
%for bp=2:2,
%AxBx=[80 181  30  91];
%AxBx=[80 181 -91 -30];
 var=mskBasC(:,:,1)+2*mskBasC(:,:,2)+3*mskBasC(:,:,3);
 ccB=[-1 5]; var(find(mskC==0))=NaN;
 grph_CS(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx);
 hold on ;
 [L0]=plot(xPA,yPA,'*b-',xAI,yAI,'*b-',xIP,yIP,'*b-'); set(L0,'MarkerSize',6);
 for b=bp:bp,
  xloc=squeeze(xc2s(b,:,:)); yloc=squeeze(yc2s(b,:,:));
  for i=1:n2s(b), jl=jl2s(b,i)-1;
   ie=bkl_Npts(jl)+1;
   [L1]=plot(bkl_Xsg(1:ie,jl),bkl_Ysg(1:ie,jl),'r-');
  %set(L1,'Color',[.9 .2 .0],'LineWidth',1); % set(L1,'LineStyle','-');
   [L2]=plot(xloc(i,:),yloc(i,:),'xw-');
   set(L2,'LineWidth',2);
  end
  [L3]=plot(xSepU(b,1:Nu(b)),ySepU(b,1:Nu(b)),'m*'); set(L3,'MarkerSize',6);
  [L4]=plot(xSepV(b,1:Nv(b)),ySepV(b,1:Nv(b)),'mo'); set(L4,'MarkerSize',6);
 end
 hold off
 end
end

return
