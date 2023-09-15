% script to generate broken lines that folows the cube-sphere grid
% and stay close as possible to latitude-target "yLat":
%- method:
% Tag grid-cell center pts (fct of where lat is within yLat separation)
% and set bk-line where 2 adjacent tagC match a crossing of yLat boundary
%-----------------------
%- load definition of the grid (needs to be done at the 1rst call):

if size(who('krd'),1) > 0,
 fprintf('krd is defined and = %i \n',krd);
else
 fprintf('krd undefined ; set to 1 \n'); krd=1 ;
end
if krd==1,
 more off ;
%- set ncdf=1 to load MNC (NetCDF) grid-files ;
%   or ncdf=0 to load MDS (binary) grid-files :
 ncdf=0;
 gDir='grid_files/';
 G=load_grid(gDir,10+ncdf);
 xcs=G.xC; ycs=G.yC; xcg=G.xG; ycg=G.yG;
%- 1rst try: skip writing to file
 kwr=0; kplot=2;
else
%- write/save to file:
 kwr=1; kplot=0;
end
%- output arrays:
 yLat=-87:3:87;
%yLat=-88:2:88;
 ydim=length(yLat);

%------------
n1h=size(xcs,1); n2h=size(xcs,2);
if n1h == 6*n2h, nc=n2h;
elseif n1h*6 == n2h, nc=n1h;
else
 error([' grid var size: ',int2str(n1h),' x ',int2str(n2h),' does not fit regular cube !']);
end
nPg=nc*nc*6; ncp=nc+1; nPp2=nPg+2;

%- storage arrays:
 savNpts=zeros(1,ydim);
 savFlag=zeros(6*ncp,ydim);
 savIJuv=zeros(6*ncp,ydim);

%-------------------------------------------------------------------------------
%- A) first step: Tag each grid-cell center point
%  tagC(i,j) = jl if yLat(jl) <= yC(i,j) < yLat(jl+1)

tagC=-ones(n1h,n2h);
yFull=[-90 yLat +90];
for jl=0:ydim,
  %- there is a shift of 1 between yFull and yLat so that yLat(jl) = yFull(jl+1)
  var=(ycs-yFull(jl+1)).*(yFull(jl+2)-ycs);
  tagC(find(var > 0))=jl;
  tagC(find(ycs == yFull(jl+1)))=jl;
end
%tagC(find(ycs < yLat(1)))=0;
%tagC(find(ycs >= yLat(end)))=ydim;

%- B) second step: split to 6 faces and check how big the jump is from 2 neighbor cells
tagC6f = split_C_cub(tagC);

MxJump=0;
nMxdbg=10; %- Max number of debug-print per face
for n=1:6,
 var=tagC6f(1:nc,2:ncp,n)-tagC6f(2:ncp,2:ncp,n); var=abs(var);
 jumpUpt=max(var(:));
 if jumpUpt > 1,
   [I J]=find(var > 1);
   nbJu=length(I);
   fprintf('-- face # %i , %i points with a tagC jump across U.pt:\n',n,nbJu);
   for il=1:min(nbJu,nMxdbg),
     i=I(il); j=J(il)+1;
     fprintf(' -i, j = %i, %i ; x,y= %8.3f, %8.3f ; tagC= %i ( %i )\n', ...
        i-1,j-1,xx1(i,j,n),yy1(i,j,n),tagC6f(i,j,n),var(i,j-1))
     i=I(il)+1; j=J(il)+1;
     fprintf(' +i, j = %i, %i ; x,y= %8.3f, %8.3f ; tagC= %i ( %i )\n', ...
        i-1,j-1,xx1(i,j,n),yy1(i,j,n),tagC6f(i,j,n),var(i-1,j-1))
   end
 end
 var=tagC6f(2:ncp,1:nc,n)-tagC6f(2:ncp,2:ncp,n); var=abs(var);
 jumpVpt=max(var(:));
 if jumpVpt > 1,
   [I J]=find(var > 1);
   nbJv=length(I);
   fprintf('-- face # %i , %i points with a tagC jump across V.pt:\n',n,nbJv);
   for il=1:min(nbJv,nMxdbg),
     i=I(il)+1; j=J(il);
     fprintf(' i, -j = %i, %i ; x,y= %8.3f, %8.3f ; tagC= %i ( %i )\n', ...
        i-1,j-1,xx1(i,j,n),yy1(i,j,n),tagC6f(i,j,n),var(i-1,j))
     i=I(il)+1; j=J(il)+1;
     fprintf(' i, +j = %i, %i ; x,y= %8.3f, %8.3f ; tagC= %i ( %i )\n', ...
        i-1,j-1,xx1(i,j,n),yy1(i,j,n),tagC6f(i,j,n),var(i-1,j-1))
   end
 end
 MxJump=max( MxJump, jumpUpt );
 MxJump=max( MxJump, jumpVpt );
end
fprintf(' max tagC Jump between 2 neighbor = %i\n',MxJump);

if MxJump == 1,
%===============================================================================
% Case with no Jump > 1 in adjacent tagC:
%-------------------------------------------------------------------------------
%- C.1) third step: Tag U & V point all in one pass:
%  tagU(i,j) = +jl if tagC6f(i,j)= jl & tagC6f(i-1,j)= jl-1
%  tagU(i,j) = -jl if tagC6f(i,j)= jl-1 & tagC6f(i-1,j)= jl
%
%  tagV(i,j) = +jl if tagC6f(i,j)= jl & tagC6f(i,j-1)= jl-1
%  tagV(i,j) = -jl if tagC6f(i,j)= jl-1 & tagC6f(i,j-1)= jl
% Note: this way we recover tagU(i,j) == +/- jj if yLat(jl) == Latitude-of-U-pt(i,j)

  tagU=zeros(nc,nc,6); tagV=tagU;
  for jl=1:ydim,
    var=abs(tagC6f(1:nc,2:ncp,:)-jl+1)+abs(tagC6f(2:ncp,2:ncp,:)-jl);
    tagU(var==0)=jl;
    var=abs(tagC6f(1:nc,2:ncp,:)-jl)+abs(tagC6f(2:ncp,2:ncp,:)-jl+1);
    tagU(var==0)=-jl;
    var=abs(tagC6f(2:ncp,1:nc,:)-jl+1)+abs(tagC6f(2:ncp,2:ncp,:)-jl);
    tagV(var==0)=jl;
    var=abs(tagC6f(2:ncp,1:nc,:)-jl)+abs(tagC6f(2:ncp,2:ncp,:)-jl+1);
    tagV(var==0)=-jl;
  end

%-------------------------------------------------------------------------------
%- D.1) fourth step: make a list for each jl:
%  --> put back (reshape) tagU & tagV in 1 long vector
%   for each jl in 1:ydim
%     listU = find(abs(tagU)==jl); nU=length(listU); flag(1:nU)=sign(tagU)*1;
%     listV = find(abs(tagV)==jl); nV=length(listV); flag(1:nV)=sign(tagV)*2;
  tagU1d=reshape(tagU,[nc*nc*6 1]);
  tagV1d=reshape(tagV,[nc*nc*6 1]);
  for jl=1:ydim,
    [ijU]=find(abs(tagU1d)==jl);
    nU=length(ijU);
    [ijV]=find(abs(tagV1d)==jl);
    nV=length(ijV);
    %-
    n1=1; n2=nU;
    savFlag(n1:n2,jl)=sign(tagU1d(ijU));
    savIJuv(n1:n2,jl)=ijU;
    %-
    n1=nU+1; n2=nU+nV;
    savFlag(n1:n2,jl)=2*sign(tagV1d(ijV));
    savIJuv(n1:n2,jl)=ijV;
    savNpts(jl)=n2;
  end
  mx_pts=max(savNpts); [J]=find(savNpts==mx_pts);
  fprintf('Max Npts = %i (x %i) e.g. for jl= %i\n',mx_pts,length(J),J(1));

else
%===============================================================================
% Case with at least one Jump > 1 in adjacent tagC:
%  since one velocity pt can be listed in more than 1 bk-line, need to
%  process step (c) and (D) within jl loop.
%-------------------------------------------------------------------------------

 %tagU=zeros(nc,nc,6); tagV=tagU;
  for jl=1:ydim,
   tagU=zeros(nc,nc,6); tagV=tagU;

%- C.2) third step: Tag U & V point for this latitude yLat(jl):
%  tagU(i,j) = +jl if tagC6f(i,j) >= jl & tagC6f(i-1,j) < jl
%  tagU(i,j) = -jl if tagC6f(i,j) < jl & tagC6f(i-1,j) >= jl
%
%  tagV(i,j) = +jl if tagC6f(i,j) >= jl & tagC6f(i,j-1) < jl
%  tagV(i,j) = -jl if tagC6f(i,j) < jl & tagC6f(i,j-1) >= jl
%
   %- hard to include case == jl with this:
   %var=(tagC6f(1:nc,2:ncp,:)-jl).*(tagC6f(2:ncp,2:ncp,:)-jl);
   %tagU(var < 0)=jl;
   %tagU=tagU.*sign(tagC6f(2:ncp,2:ncp,:)-tagC6f(1:nc,2:ncp,:));

    var=zeros(nc,nc,6);
    tmp=tagC6f(1:nc,2:ncp,:); var(tmp < jl)=1;
    tmp=tagC6f(2:ncp,2:ncp,:); var(tmp < jl)=0;
    tagU(var == 1)=jl;

    var=zeros(nc,nc,6);
    tmp=tagC6f(2:ncp,2:ncp,:); var(tmp < jl)=1;
    tmp=tagC6f(1:nc,2:ncp,:); var(tmp < jl)=0;
    tagU(var == 1)=-jl;

    var=zeros(nc,nc,6);
    tmp=tagC6f(2:ncp,1:nc,:); var(tmp < jl)=1;
    tmp=tagC6f(2:ncp,2:ncp,:); var(tmp < jl)=0;
    tagV(var == 1)=jl;

    var=zeros(nc,nc,6);
    tmp=tagC6f(2:ncp,2:ncp,:); var(tmp < jl)=1;
    tmp=tagC6f(2:ncp,1:nc,:); var(tmp < jl)=0;
    tagV(var == 1)=-jl;

%-------------------------------------------------------------------------------
%- D.1) fourth step: make a list for this latitude line yLat(jl):
%  --> put back (reshape) tagU & tagV in 1 long vector
%   for each jl in 1:ydim
%     listU = find(abs(tagU)==jl); nU=length(listU); flag(1:nU)=sign(tagU)*1;
%     listV = find(abs(tagV)==jl); nV=length(listV); flag(1:nV)=sign(tagV)*2;
    tagU1d=reshape(tagU,[nc*nc*6 1]);
    tagV1d=reshape(tagV,[nc*nc*6 1]);
    %---
    [ijU]=find(abs(tagU1d)==jl);
    nU=length(ijU);
    [ijV]=find(abs(tagV1d)==jl);
    nV=length(ijV);
    %-
    n1=1; n2=nU;
    savFlag(n1:n2,jl)=sign(tagU1d(ijU));
    savIJuv(n1:n2,jl)=ijU;
    %-
    n1=nU+1; n2=nU+nV;
    savFlag(n1:n2,jl)=2*sign(tagV1d(ijV));
    savIJuv(n1:n2,jl)=ijV;
    savNpts(jl)=n2;

  end

end

%===============================================================================
%- a quick plot to check:
if kplot > 0,
figure(1); clf; colormap('jet');
 ccB=[0 0]; shift=-1; cbV=1; AxBx=[-180 180 -90 90]; kEnv=0;
 var=tagC; titv='tagC';
%var=tagU; titv='tagU';
%var=tagV; titv='tagV';
 if strcmp(titv,'tagC'), ccB=[0 ydim];
 else
  if n2h == nc,
   var=reshape(permute(var,[1 3 2]),[n1h n2h]);
  else
   var=reshape(var,[n1h n2h]);
  end
  ccB=[-ydim ydim];
 end
 fprintf(' %s : min,Max= %9.5g , %9.5g \n',titv,min(var(:)),max(var(:)));
 if kplot == 1,
   grph_CS(var,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx,kEnv);
 else
   grph_CS_6t(var,ccB(1),ccB(2),nc,'tagC');
 end
end

%--------------------------------------
%- output : save bk-line into a matlab ".mat" file:
%------------------------------
if ydim > 1 & kwr == 1,

%- write smaller arrays : reduce size from nc*6 to Max(savNpts) :
  mxNpts=max(savNpts);

%- array to save in matlab "*.mat" file:
%  bkl_yLat = broke-line (bk-line) latidude-target vector
%  bkl_Npts = number of velocity points contributing to each bk-line
%  bkl_Flag = list to select sign and component (U or V) to use in bk-line
%  bkl_IJuv = index list of velocity points for each bk-line
%  bkl_tagC = 2-D map of grid-cell center tags, according to latitude band "yLat"

  bkl_Flag=zeros(mxNpts,ydim);
  bkl_IJuv=zeros(mxNpts,ydim);

  bkl_yLat=yLat ;
  bkl_Npts=savNpts ;
  bkl_Flag=savFlag(1:mxNpts,:);
  bkl_IJuv=savIJuv(1:mxNpts,:);
  if n2h == nc,
   bkl_tagC=permute(reshape(tagC,[nc 6 nc]),[1 3 2]);
  else
   bkl_tagC=reshape(tagC,[nc nc 6]);
  end

 namf=['isoLat_',int2str(ydim),'_cs',int2str(nc)];
 save(namf,'bkl_yLat','bkl_Npts','bkl_Flag','bkl_IJuv','bkl_tagC');
 fprintf([' ==> Save %i lines on matlab file: ',namf,'\n'],ydim);

end
%------------------------------

return
