
% $Header: /u/gcmpack/MITgcm/utils/matlab/cs_grid/bk_line/def_API_msk.m,v 1.3 2008/11/11 23:05:14 jmc Exp $
% $Name:  $

krd=1; kpr=2; kgr=0; kwr=1;
%krd=0; kpr=0; kgr=1; kwr=0; % <- execute a 2nd time & draw some plot
%set_axis
%krd=1; kpr=3; kgr=3; kwr=1; % <- generate Continent Mask

if krd == 1,
 rac='grid_files/';
 load_cs;
 mskC=rdmds([rac,'hFacC']);
 mskC=min(1,mskC); mskC=ceil(mskC);
 mskW=rdmds([rac,'hFacW']);
 mskW=min(1,mskW); mskW=ceil(mskW);
 mskS=rdmds([rac,'hFacS']);
 mskS=min(1,mskS); mskS=ceil(mskS);
 ncx=size(mskC,1); nc=size(mskC,2); ncp=nc+1;
 xc1=reshape(xcs,1,ncx*nc);
 yc1=reshape(ycs,1,ncx*nc);
 xg1=reshape(xcg,1,ncx*nc);
 yg1=reshape(ycg,1,ncx*nc);
 mk1=reshape(mskC(:,:,1),1,ncx*nc);
%xg6=split_Z_cub(xcg);
%yg6=split_Z_cub(ycg);
 xc6=split_C_cub(xcs,1);
 yc6=split_C_cub(ycs,1);
end

if kpr == 1 | kpr == 2,
%- define basin mask for Tracer pt: =1: Atlantic ; =2: Indian ; =3: Pacific
 [xcb,xPA,yPA,xAI,yAI,xIP,yIP]=line_sep(yc1);
 mbs=3*ones(1,ncx*nc);
 mbs(find( xc1>xcb(1,:) & xc1<xcb(2,:)) )=1;
 mbs(find( xc1>xcb(2,:) & xc1<xcb(3,:)) )=2;
 bas=reshape(mbs,ncx,nc);

%- define basin mask for U,V pt: Atlantic=1 ; Indian=2 ; Pacific=3
 mskBasU=zeros(ncx,nc,3); mskBasV=zeros(ncx,nc,3); mskBasC=zeros(ncx,nc,3);
 varU=zeros(nc,nc);varV=zeros(nc,nc);
 msk6=split_C_cub(bas,1);
 for b=1:3,
   mskBasC(:,:,b)=abs(b-bas);
   mskB=abs(b-msk6); mskB=1-min(1,mskB);
  for k=1:6,
   i1=1+nc*(k-1);i2=nc*k;
   var=mskB(1:ncp,2:ncp,k);
   varU(1:nc,:)=max(var(1:nc,:),var(2:ncp,:));
   mskBasU(i1:i2,:,b)=varU(1:nc,:);
   var=mskB(2:ncp,1:ncp,k);
   varV(:,1:nc)=max(var(:,1:nc),var(:,2:ncp));
   mskBasV(i1:i2,:,b)=varV(1:nc,:);
  end
 end
 mskBasC=1-min(1,mskBasC);

end

if kpr==2,

%- find list of separation points :
 mskU=zeros(ncx,nc); mskV=zeros(ncx,nc);
 msk6=msk6+max(2,msk6)-2;
  for k=1:6,
   i1=1+nc*(k-1);i2=nc*k;
   var=msk6(1:ncp,2:ncp,k);
   varU(1:nc,:)=var(2:ncp,:)-var(1:nc,:);
   mskU(i1:i2,:)=varU(1:nc,:);
   var=msk6(2:ncp,1:ncp,k);
   varV(:,1:nc)=var(:,2:ncp)-var(:,1:nc);
   mskV(i1:i2,:)=varV(1:nc,:);
  end
 %mskU=mskU.*mskW(:,:,1); mskV=mskV.*mskS(:,:,1);
  mskU=reshape(mskU,1,ncx*nc); mskV=reshape(mskV,1,ncx*nc);
  for b=1:3,
   [I]=find(abs(mskU)==b);
   Nu(b)=length(I); Iu(b,1:Nu(b))=I;
   i0=rem(I-1,ncx);j1=1+fix((I-1)/ncx);
   i1=1+rem(i0,nc); k1=1+fix(i0/nc);
   for i=1:Nu(b),
     xSepU(b,i)=(xc6(1+i1(i),1+j1(i),k1(i))+xc6(i1(i),1+j1(i),k1(i)))/2;
     ySepU(b,i)=(yc6(1+i1(i),1+j1(i),k1(i))+yc6(i1(i),1+j1(i),k1(i)))/2;
   end
   [J]=find(abs(mskV)==b);
   Nv(b)=length(J); Iv(b,1:Nv(b))=J;
   i0=rem(J-1,ncx);j1=1+fix((J-1)/ncx);
   i1=1+rem(i0,nc); k1=1+fix(i0/nc);
   for i=1:Nv(b),
     xSepV(b,i)=(xc6(1+i1(i),1+j1(i),k1(i))+xc6(1+i1(i),j1(i),k1(i)))/2;
     ySepV(b,i)=(yc6(1+i1(i),1+j1(i),k1(i))+yc6(1+i1(i),j1(i),k1(i)))/2;
   end
%----
%  xSepU(b,1:Nu(b))=xg1(I); ySepU(b,1:Nu(b))=yg1(I);
%  xSepV(b,1:Nv(b))=xg1(J); ySepV(b,1:Nv(b))=yg1(J);
   fprintf('Sep. Line %i , Open Pts : %i %i \n',b,Nu(b),Nv(b));
  end

end

if kpr == 3,
%-- create a mask for each set of connected continents:
% 1 = Asia+Europe+Africa ; 2 = Americas ; 2 = Antarctica ; 4 = Australia
%-  start from 1 point:
 x0cont=[60 -70 140 140]; nbCont=length(x0cont);
 y0cont=[40   0 -85 -25];
 ContinC=zeros(ncx,nc); locMskZ=zeros(ncp+1,ncp+1,6);
 for k=1:nbCont,
  locMskC=1.-mskC(:,:,1);
  fprintf('Cont. k= %i , Starting Point: %7.3f , %7.3f\n',k,x0cont(k),y0cont(k));
  dd=abs(xcs-x0cont(k))+abs(ycs-y0cont(k));
  [I J]=find(dd==min(dd(:)));
  for i=1:length(I),
   if mskC(I(i),J(i),1)==0, locMskC(I(i),J(i))=2; end
  end
  if max(locMskC(:)) < 2,
    error 'Starting point in water => need to pick an other point'
  end
  it=0; nSum=0; mSum=sum(locMskC(:));
  fprintf(' it=%i , mSum= %i\n',it,mSum);
% for it=1:1,
  while mSum > nSum,
   it=it+1; nSum=mSum;
   msk2=split_C_cub(locMskC,2);
   msk2(1,1,:)=0; msk2(ncp+1,ncp+1,:)=0; msk2(ncp+1,1,:)=0; msk2(1,ncp+1,:)=0;
   for n=1:6,
    tmp=min(msk2(:,:,n),1);
    var=msk2(:,:,n)-1; var=max(var,0);
    var(2:ncp,2:ncp)=var(2:ncp,2:ncp) ...
                    +var(1:nc,2:ncp)+var(3:ncp+1,2:ncp) ...
                    +var(2:ncp,1:nc)+var(2:ncp,3:ncp+1) ...
                    +var(1:nc,1:nc)+var(3:ncp+1,3:ncp+1) ...
                    +var(1:nc,3:ncp+1)+var(3:ncp+1,1:nc) ...
  ; var=var.*tmp;
    var=min(var,1);
    msk2(:,:,n)=msk2(:,:,n)+var;
   end
   msk2=min(msk2,2);
   var=msk2(2:ncp,2:ncp,:);locMskC=reshape(permute(var,[1 3 2]),[ncx nc]);
   mSum=sum(locMskC(:));
   fprintf(' it=%i , mSum= %i\n',it,mSum);
%- exit infinite loop if something is wrong,
   if it > ncx,
    mSum=nSum; fprintf('Exit loop (k=%i) at it= %i\n',k,it);
   end
  end
  locMskC=max(locMskC,1)-1;
  msk2=split_C_cub(locMskC,2);
  msk2(1,1,:)=0; msk2(ncp+1,ncp+1,:)=0; msk2(ncp+1,1,:)=0; msk2(1,ncp+1,:)=0;
  for n=1:6,
    var=msk2(:,:,n);
    var(2:ncp+1,2:ncp+1)=var(2:ncp+1,2:ncp+1)+var(1:ncp,1:ncp) ...
                        +var(1:ncp,2:ncp+1)+var(2:ncp+1,1:ncp);
    var=min(var,1);
    msk2(:,:,n)=var;
  end
  ContinC=ContinC+k*locMskC;
  locMskZ=locMskZ+k*msk2;
 end
 ContinZ=zeros(ncx*nc+2,1);
 var=locMskZ(2:ncp,2:ncp,:);
 ContinZ(1:ncx*nc)=reshape(permute(var,[1 3 2]),[ncx*nc 1]);
 ContinZ(ncx*nc+1)=locMskZ(2,ncp+1,1);
 ContinZ(ncx*nc+2)=locMskZ(ncp+1,2,2);
end

if kwr==1,
 if kpr==1 | kpr==2,
  namf='maskC_bas.bin';
  fid=fopen(namf,'w','b'); fwrite(fid,mskBasC,'real*4'); fclose(fid);
  fprintf(['Write Atl-Ind-Pac Bass. mask on file:',namf,' : O.K. \n']);
  namf='maskW_bas.bin';
  fid=fopen(namf,'w','b'); fwrite(fid,mskBasU,'real*4'); fclose(fid);
  fprintf(['Write Atl-Ind-Pac Bass. mask on file:',namf,' : O.K. \n']);
  namf='maskS_bas.bin';
  fid=fopen(namf,'w','b'); fwrite(fid,mskBasV,'real*4'); fclose(fid);
  fprintf(['Write Atl-Ind-Pac Bass. mask on file:',namf,' : O.K. \n']);
 end
 if kpr==2,
  namf='open_basins_section';
  save(namf,'Nu','Nv','Iu','Iv');
  fprintf(['write Basin connection on file:',namf,'.mat : O.K. \n']);
 end
 if kpr==3,
  namf='mask_Cont.bin';
  fid=fopen(namf,'w','b');
  fwrite(fid,ContinZ,'real*4');
  fwrite(fid,ContinC,'real*4');
  fclose(fid);
  fprintf('Write %i Continents mask on file: %s : O.K. \n',nbCont,namf);
 end
end

if kgr==1,
%yy1=[-89.5:1:89.5]; [x3b]=line_sep(yy1);
 figure(1); clf;
 shift=-1; cbV=2; ccB=[0 0]; AxBx=[-180 180 -90 90];
 var=bas ; ccB=[-1.5 4.5];  var(find(mskC(:,:,1)==0))=NaN;
 grph_CS(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx);
 hold on ;
%plot(x3b(1,:),yy1,'r-');
%plot(x3b(2,:),yy1,'r-');
%plot(x3b(3,:),yy1,'r-');
 plot(xPA,yPA,'*b-');
 plot(xAI,yAI,'*b-');
 plot(xIP,yIP,'*b-');
 for b=1:3,
  plot(xSepU(b,1:Nu(b)),ySepU(b,1:Nu(b)),'r*');
  plot(xSepV(b,1:Nv(b)),ySepV(b,1:Nv(b)),'ro');
 end
 hold off
end

%----
if kgr==3,
 figure(2); clf;
 shift=-1; cbV=1; ccB=[0 0]; AxBx=[-180 180 -90 90];
 ccB=[-1 nbCont+1];
 subplot(211)
 var=ContinC;
 var(find(mskC(:,:,1)==1))=NaN;
 grph_CS(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx);
 subplot(212)
 var=ContinZ; ccB=[-1 nbCont+1];
 grph_CSz(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx);
end

return
