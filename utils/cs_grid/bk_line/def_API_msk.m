
krd=1; kpr=2; kgr=0; kwr=1;
%krd=0; kpr=0; kgr=1; kwr=0; % <- execute a 2nd time & draw some plot
%set_axis

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

if kpr >0,
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

if kwr==1,
 namf='maskC_bas.bin';
 fid=fopen(namf,'w','b'); fwrite(fid,mskBasC,'real*4'); fclose(fid);
 fprintf(['Write Atl-Ind-Pac Bass. mask on file:',namf,' : O.K. \n']);
 namf='maskW_bas.bin';
 fid=fopen(namf,'w','b'); fwrite(fid,mskBasU,'real*4'); fclose(fid);
 fprintf(['Write Atl-Ind-Pac Bass. mask on file:',namf,' : O.K. \n']);
 namf='maskS_bas.bin';
 fid=fopen(namf,'w','b'); fwrite(fid,mskBasV,'real*4'); fclose(fid);
 fprintf(['Write Atl-Ind-Pac Bass. mask on file:',namf,' : O.K. \n']);
 namf='open_basins_section';
 save(namf,'Nu','Nv','Iu','Iv');
 fprintf(['write Basin connection on file:',namf,'.mat : O.K. \n']);
end

if kgr==1, 
%yy1=[-89.5:1:89.5]; [x3b]=line_sep(yy1);

 figure(1); clf;
 shift=0; cbV=2; ccB=[0 0]; AxBx=[-180 180 -90 90];
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

return
