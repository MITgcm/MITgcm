krd=1; kpr=1; kgr=0; kwr=1;
% krd=0; kpr=0; kgr=1; kwr=0; % <- execute a 2nd time & draw some plot
ijprt=65;

if krd == 1,

%- set ncdf=1 to load MNC (NetCDF) grid-files ;
%   or ncdf=0 to load MDS (binary) grid-files :
 ncdf=0;

%rac='/home/jmc/grid_cs32/';
 rac='grid_files/';
 bk_lineF=[rac,'isoLat_cube32_59'];

%- load broken lines :
% bkl_Ylat,bkl_Npts,bkl_Flg,bkl_Iuv,bkl_Juv,bkl_Xsg,bkl_Ysg
 load(bk_lineF);

%- load grid-files :
 G=load_grid(rac,10+ncdf);
 xcs=G.xC; ycs=G.yC; xcg=G.xG; ycg=G.yG; arc=G.rAc;
 ncx=G.dims(1); nc=G.dims(2);

%load_cs;
%ncx=size(xcs,1); nc=size(xcs,2);

%xc6=split_C_cub(xcs,1);
%yc6=split_C_cub(ycs,1);
%xg6=split_Z_cub(xcg);
%yg6=split_Z_cub(ycg);
 xc1=reshape(xcs,ncx*nc,1);
 yc1=reshape(ycs,ncx*nc,1);

%rac='res_r1/';
%hFacC=rdmds([rac,'hFacC']);
%mskC=min(1,hFacC); mskC=ceil(mskC);

 fprintf([' load bk_line description from file=',bk_lineF,'\n']);
end

ydim=length(bkl_Ylat); ylat=zeros(ydim+2,1);
ylat(2:ydim+1)=bkl_Ylat; ylat(1)=-90; ylat(ydim+2)=90;
bkl_IJuv=bkl_Iuv+ncx*(bkl_Juv-1);
Yg1=zeros(ydim,1); Yg2=Yg1;
for jl=1:ydim,
  ie=bkl_Npts(jl)+1;
  Yg1(jl)=min(bkl_Ysg(1:ie,jl)); Yg2(jl)=max(bkl_Ysg(1:ie,jl));
end

if kpr == 1,

 bkl_Zon=-ones(ncx*nc,1);
 for ij=1:ncx*nc,
  xx=xc1(ij); yy=yc1(ij);
  [J2]=find(Yg1 >= yy);
  if length(J2)==0, jl2=ydim+1; else jl2=min(J2); end
  [J1]=find(Yg2 <= yy);
  if length(J1)==0, jl1=0; else jl1=max(J1); end
  fprintf('ij,xx,yy= %4i %6.1f %6.1f ; jl1,jl2= %2i %2i',ij, xx, yy, jl1,jl2);
  %- solve for intersection bkl_X,Y with x=xx :
  jL1=-1; jL2=-1; yXX=zeros(1,1+jl2-jl1);
  for jl=jl1:jl2
   if ij==ijprt, fprintf('\n  jl= %i ',jl);end
   l=1+jl-jl1;
   if jl == 0, yXX(l)=-90 ;
   elseif jl == ydim+1, yXX(l)=90;
   else
    ie=bkl_Npts(jl)+1;
    xloc=bkl_Xsg(1:ie,jl); yloc=bkl_Ysg(1:ie,jl); yloc(ie+1)=yloc(1);
    xloc2=xloc; xloc2(1:ie-1)=xloc(2:ie); xloc2(ie)=xloc(1)+360;
    if ij==ijprt, fprintf('; xloc(1)= %6.1f ',xloc(1)); end
    [I]=find( xloc <= xx & xx < xloc2 );
    if length(I) ~= 1, fprintf(' Stop A \n');return; end
    if ij==ijprt, fprintf('; x,y_loc= %6.1f %6.1f %5.1f %5.1f ', ...
                            xloc(I),xloc2(I),yloc(I),yloc(I+1)); end
    if xloc2(I) == xloc(I),
     yXX(l)=yloc(I);
    else
     yXX(l)=yloc(I)+(yloc(I+1)-yloc(I))*(xx-xloc(I))/(xloc2(I)-xloc(I));
    end
   end
   if ij==ijprt, fprintf('; yXX= %6.1f',yXX(l)); end
   if l>1 & yXX(l) < yXX(l-1), fprintf(' Stop B \n');return; end
   if yXX(l) <= yy, jL1=jl; end
   if yXX(l) > yy & jL2==-1, jL2=jl; end
  end
  if jL2 == jL1+1, bkl_Zon(ij)=jL1;
   fprintf('; jL1= %2i\n',jL1);
  else fprintf(' Stop C : %i %i %6.1f %6.1f\n',jL1,jL2,xx,yy);
   fprintf(' yXX :');fprintf(' %9.5f',yXX(1:1+jl2-jl1)); fprintf('\n');
   return;
  end
 end % <- ij loop

end

if kwr==1,
 namf=['isoLat_cs',int2str(nc),'_',int2str(ydim)];
%- save to matlab file:
% bkl_Ylat, bkl_Npts, bkl_Flg, bkl_IJuv, bkl_Xsg, bkl_Ysg, bkl_Zon
 save(namf,'bkl_Ylat','bkl_Npts','bkl_Flg','bkl_IJuv','bkl_Xsg','bkl_Ysg','bkl_Zon');
 fprintf([' ==> Save %i lines on matlab file: ',namf,'\n'],ydim);
end

if kgr==1,
 figure(1); clf;
 shift=-1; cbV=2; ccB=[0 0]; AxBx=[-180 180 -90 90];
%var=zeros(ncx,nc) ; var(1+rem(ijprt-1,ncx),1+fix((ijprt-1)/ncx))=1 ; ccB=[-1 1];
 var=reshape(bkl_Zon,ncx,nc) ; ccB=[-1 50];
 grph_CS(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx);
 hold on ;
 for jl=jl1:jl2,
  ie=bkl_Npts(jl)+1;
  [L]=plot(bkl_Xsg(1:ie,jl),bkl_Ysg(1:ie,jl),'r-');
  set(L,'Color',[0 0 0],'LineWidth',2); % set(L,'LineStyle','-');
 end
 plot(xx*ones(1,1+jl2-jl1),yXX,'ro-');
 hold off
end
