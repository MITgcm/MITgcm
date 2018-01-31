
krd=1; kpr=2; kgr=0; kwr=1;
%krd=0; kpr=0; kgr=1; kwr=0; % <- execute a 2nd time & draw some plot
%set_axis
%krd=1; kpr=3; kgr=3; kwr=1; % <- generate Continent Mask

if krd == 1,
%- set ncdf=1 to load MNC (NetCDF) grid-files ;
%   or ncdf=0 to load MDS (binary) grid-files :
 ncdf=0;
%rac='/home/jmc/grid_cs32/';
 rac='grid_files/';
 G=load_grid(rac,ncdf);
 xcs=G.xC; ycs=G.yC; xcg=G.xG; ycg=G.yG; arc=G.rAc;
 mskC=G.hFacC; mskW=G.hFacW; mskS=G.hFacS;
 mskC=min(1,mskC); mskC=ceil(mskC);
 mskW=min(1,mskW); mskW=ceil(mskW);
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
%- Separation bs=1 : Atl/Ind ; bs=2 : Ind/Pac ; bs=3 : Pac/Atl.
%  Defining msk6 as above (msk6 =1: Atl, =2: Ind, =4: Pac)
%  allows to get separation number as gradient (=mskU,mskV) of msk6
  for bs=1:3,
   [I]=find(abs(mskU)==bs);
   Nu(bs)=length(I); Iu(bs,1:Nu(bs))=I;
   i0=rem(I-1,ncx);j1=1+fix((I-1)/ncx);
   i1=1+rem(i0,nc); k1=1+fix(i0/nc);
   xx=zeros(2,Nu(bs));
   for i=1:Nu(bs),
     xx(1,i)=xc6(1+i1(i),1+j1(i),k1(i)); 
     xx(2,i)=xc6( i1(i), 1+j1(i),k1(i));
     ySepU(bs,i)=(yc6(1+i1(i),1+j1(i),k1(i))+yc6(i1(i),1+j1(i),k1(i)))/2;
   end 
   var=reshape(xcg,[1 ncx*nc]);
   xx(1,:)=var(I)-180+rem(xx(1,:)-var(I)+3*180,360);
   xx(2,:)=var(I)-180+rem(xx(2,:)-var(I)+3*180,360);
   xSepU(bs,[1:Nu(bs)])=(xx(1,:)+xx(2,:))/2;
   [J]=find(abs(mskV)==bs);
   Nv(bs)=length(J); Iv(bs,1:Nv(bs))=J;
   i0=rem(J-1,ncx);j1=1+fix((J-1)/ncx);
   i1=1+rem(i0,nc); k1=1+fix(i0/nc);
   xx=zeros(2,Nv(bs));
   for i=1:Nv(bs),
     xx(1,i)=xc6(1+i1(i),1+j1(i),k1(i)); 
     xx(2,i)=xc6(1+i1(i), j1(i), k1(i));
     ySepV(bs,i)=(yc6(1+i1(i),1+j1(i),k1(i))+yc6(1+i1(i),j1(i),k1(i)))/2;
   end
   xx(1,:)=var(J)-180+rem(xx(1,:)-var(J)+3*180,360);
   xx(2,:)=var(J)-180+rem(xx(2,:)-var(J)+3*180,360);
   xSepV(bs,[1:Nv(bs)])=(xx(1,:)+xx(2,:))/2;
%----
   fprintf('Sep. Line %i , Open Pts : %i %i \n',bs,Nu(bs),Nv(bs));
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
 shift=-1; cbV=1; ccB=[0 0]; AxBx=[-182 182 -91 91];
 var=bas ; ccB=[-1.5 5.5];  var(find(mskC(:,:,1)==0))=NaN;
 grph_CS(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx);
 hold on ;
%plot(x3b(1,:),yy1,'r-');
%plot(x3b(2,:),yy1,'r-');
%plot(x3b(3,:),yy1,'r-');
 plot(xPA,yPA,'*b-');
 plot(xAI,yAI,'*b-');
 plot(xIP,yIP,'*b-');
 for bs=1:3,
  plot(xSepU(bs,1:Nu(bs)),ySepU(bs,1:Nu(bs)),'r*');
  plot(xSepV(bs,1:Nv(bs)),ySepV(bs,1:Nv(bs)),'ro');
 end
 hold off
%figure(2); clf;
%for b=1:3,
%  subplot(310+b);
%  var=mskBasC(:,:,b); titv='mskBasC';
% %var=mskBasU(:,:,b); titv='mskBasU';
% %var=mskBasV(:,:,b); titv='mskBasV';
%  grph_CS(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx);
%  title([titv,' : nBas= ',int2str(b)]);
%end
 figure(3); clf; bs=3;
 shift=-1; cbV=1; ccB=[0 0]; AxBx=[-182 182 -91 91]; ccB=[-1 1]*5;
 subplot(211);
 var=reshape(mskU,[ncx nc]) ;
 grph_CS(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx);
 hold on; plot(xSepU(bs,1:Nu(bs)),ySepU(bs,1:Nu(bs)),'m*'); hold off
 title(['mskU and SepU for separation bs= ',int2str(bs)]);
 subplot(212);
 var=reshape(mskV,[ncx nc]) ;
 grph_CS(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx);
 hold on; plot(xSepV(bs,1:Nv(bs)),ySepV(bs,1:Nv(bs)),'mo'); hold off
 title(['mskV and SepV for separation bs= ',int2str(bs)]);

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
