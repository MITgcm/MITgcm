% main script to generate broken lines that folows the cubic grid
% and stay close as possible to a given latitude
%-----------------------

%- load definition of the grid (needs to be done at the 1rst call):

if size(who('krd'),1) > 0,
 fprintf('krd is defined and = %i \n',krd);
else
 fprintf('krd undefined ; set to 1 \n'); krd=1 ;
end   
if krd==1, 
 more off
%rac='/home/jmc/grid_cs32/';
 rac='grid_files/';
 load_grid_bk_line ;
%- 1rst try:
 ydim=1; jl=1; yl=35; kwr=0; 
else
%- do the real calculation and write to file:
 ydim=2; kwr=1;
end

%------------
nc=size(xcs,2) ; ncp=nc+1 ;

%- output arrays:
 ylat=-87:3:87;
if ydim > 1,  ydim=length(ylat); end
 savNpts=zeros(ydim,1);
 savFlg=zeros(6*nc,ydim);
 savIuv=zeros(6*nc,ydim); savJuv=zeros(6*nc,ydim);
 savXsg=zeros(6*nc,ydim); savYsg=zeros(6*nc,ydim);

kplot=zeros(ydim,1);
%kplot(1:1:ydim)=1;
kplot(1)=1;
nf1=1; nf2=6; %- allow to treat only few faces (debug) : n=nf1:nf2,

%------------------------------
%- face 3 & 6 : long cover ]-180;180]
%- face 4 : long in 2 part ]-180;-135] + ]135;180]; 
% => put together (in xx2) with xMid=180
xMid=zeros(6,1); 
xMid(4)=180; 

%-- define large value for X,Y position :
XYout=1000;

xx1=split_Z_cub(xcg);
yy2=split_Z_cub(ycg);
%- dx,dy not used but would be  necessary to compute the length of broken-lines
% [dy6t,dx6t]=split_UV_cub(dyg,dxg);
xx2=xx1;
for n=1:6,
 if xMid(n) ~= 0,
   dec=xMid(n)-180;
   xx2(:,:,n)=rem( xx2(:,:,n)+360-dec, 360) +dec ;
 end
 fprintf('face  %i : xMid= %8.3f ; Xmin,Xmax = %8.3f %8.3f \n', ...
         n, xMid(n), min(min(xx2(:,:,n))), max(max(xx2(:,:,n))) );
end

%-- define dylat = width of Lat. band that contains the broken line:
ddy=zeros(ncp,ncp,6); 
ddy(1:nc,:,:)=yy2(1:nc,:,:)-yy2(2:ncp,:,:); ddy=abs(ddy);
dyImx=max(max(max(ddy))) ;
ddy=zeros(ncp,ncp,6); 
ddy(:,1:nc,:)=yy2(:,1:nc,:)-yy2(:,2:ncp,:); ddy=abs(ddy);
dyJmx=max(max(max(ddy)));
dylat=sqrt(2)*max(dyImx,dyJmx);
fprintf('dyImx,dyJmx = %7.3f %7.3f ; ==> select dylat= %9.6f \n', ...
         dyImx,dyJmx,dylat);
%------------------------------

%-----------------------------------------
%-- illustrate with a 2.D plot :
if max(kplot) > 0,
 face_color=zeros(7,3); lineThick=3*ones(7);
 face_color(1:6,3)=1; 
 face_color(3,:)=[0 1 0]; face_color(6,:)=face_color(3,:);
 face_color(7,:)=[1 0 0]; lineThick(7)=2;

 figure(1); clf;
 %subplot(212);
 if ydim == 1, Ygraph=yl ; else Ygraph=90; end
 grph_bk_line(zeros(6*nc,nc),xcs,ycs,xcg,ycg,-25,40,Ygraph); 
 AA=axis; 
 if ydim == 1,
  h(1)=line(AA(1:2),[yl yl]);
  h(2)=line(AA(1:2),[yl-dylat/2 yl-dylat/2]);
  h(3)=line(AA(1:2),[yl+dylat/2 yl+dylat/2]);
  set(h,'Color',[0 0 0],'LineWidth',1,'LineStyle','--');
 end
 hold on
end
%------------------------------

%-- Start loop on jl : 
for jl=1:ydim,
 if ydim > 1, yl=ylat(jl) ; end
 fprintf(' Define broken line closest to yl= %8.3f \n',yl);
%======================================================================

%--------------------------------------
% 1rst step : fort each "yl" and for each face;
% find the broken-line closest to yl, starting from the West side = min(x)

nMx6t=zeros(6,1);
savI=zeros(nc*6,6); savJ=zeros(nc*6,6); savF=zeros(nc*6,6);
isav=zeros(nc*6,6); jsav=zeros(nc*6,6); xsav=zeros(nc*6,6);

find_bk_line
%- for n=nf1:nf2, 

%--------------------------------------
%- define "segments" = continuous part of the broken line :

 ncut=zeros(6,1); icut=zeros(nc,6,6); xcut=zeros(nc,4,6); ycut=zeros(nc,4,6);

clean_bk_line

%-- plot the results:
if kplot(jl) == 2 | ( max(kplot) > 0 & misfit > 0 ) , 
 for n=nf1:nf2, if ncut(n) > 0,
  yloc=zeros(6*nc,1);
  for i=1:nMx6t(n), yloc(i)=yy2(isav(i,n),jsav(i,n),n); end;
  clear P ; np=0; 
  for in=1:ncut(n), if icut(in,6,n) >= 0, 
%  is=3; ie=4;
%  if icut(in,is,n)*icut(in,ie,n)==0 | icut(in,is,n) >= icut(in,ie,n), 
%    is=1; ie=2; end
   is=3; if icut(in,is,n) <= 0, is=1 ; end
   ie=4; if icut(in,ie,n) <= 0, ie=2 ; end
   if icut(in,is,n) > icut(in,ie,n), is=1 ; ie=2 ; end
   if icut(in,is,n) > 0 & icut(in,ie,n) > 0 & icut(in,is,n) < icut(in,ie,n), 
     np=np+1;
     P(np)=plot(xsav(icut(in,is,n):icut(in,ie,n),n), ...
                yloc(icut(in,is,n):icut(in,ie,n)),'*');
  end ; end; end
  if np > 0,
   set(P,'Color',face_color(n,:),'LineWidth',lineThick(n),'LineStyle','-'); end
 end; end ; 
end

if misfit > 0, 
 fprintf('misfit= %i , xyfit= %i ; ==> must do something ! \n', ...
    misfit,xyfit); 
 return
end

%--------------------------------------
%- output : put together the 6 faces & save to a file (if kwr =1) :

save_bk_line 

if kplot(jl) > 0 | ( max(kplot) > 0 &  misfit > 0 ),
 titr=sprintf('nMx6t= %i %i %i %i %i %i ;  yLat= %8.3f',nMx6t,yl);
 title(titr);
 if lineThick(7) > 0,
  nPts = savNpts(jl)+1;
  P0=plot(savXsg(1:nPts,jl),savYsg(1:nPts,jl));
 %set(P0,'Color',face_color(7,:),'LineWidth',lineThick(7),'LineStyle','.');
  set(P0,'Color',face_color(7,:),'LineWidth',lineThick(7),'LineStyle','-');
end ; end

if misfit > 0, 
 fprintf('misfit= %i , Pb in connecting segment jl,yLat= %i %8.3f \n', ...
          misfit,jl,yl);
 return
end

%======================================================================
end %- end jl loop

if max(kplot) > 0, hold off ; end

%------------------------------
%-- save to a matlab file :
if ydim > 1 & kwr == 1,

%- write smaller arrays : reduce size from nc*6 to Max(savNpts) :
  
  mxNpt=1+max(savNpts);
  bkl_Npts=savNpts ; mxNpt=1+max(savNpts);
  bkl_Flg=zeros(mxNpt,ydim);
  bkl_Iuv=zeros(mxNpt,ydim); bkl_Juv=zeros(mxNpt,ydim);
  bkl_Xsg=zeros(mxNpt,ydim); bkl_Ysg=zeros(mxNpt,ydim);

  bkl_Ylat=ylat ;
  bkl_Npts=savNpts ; 
  bkl_Flg=savFlg(1:mxNpt,:);
  bkl_Iuv=savIuv(1:mxNpt,:); bkl_Juv=savJuv(1:mxNpt,:);
  bkl_Xsg=savXsg(1:mxNpt,:); bkl_Ysg=savYsg(1:mxNpt,:);

dyMean=round((ylat(ydim)-ylat(1))/ydim) ;
namf=['isoLat_cube',int2str(nc),'_',int2str(ydim)];

save(namf,'bkl_Ylat','bkl_Npts','bkl_Flg','bkl_Iuv','bkl_Juv','bkl_Xsg','bkl_Ysg');

fprintf([' ==> Save %i lines on matlab file: ',namf,'\n'],ydim);

end
%------------------------------

return
%------------------------------
