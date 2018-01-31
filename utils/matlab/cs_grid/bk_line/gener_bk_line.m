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
 more off ;
%- set ncdf=1 to load MNC (NetCDF) grid-files ;
%   or ncdf=0 to load MDS (binary) grid-files :
 ncdf=0;
%rac='/home/jmc/grid_cs32/';
 rac='grid_files/';
 G=load_grid(rac,10+ncdf);
 xcs=G.xC; ycs=G.yC; xcg=G.xG; ycg=G.yG; arc=G.rAc;
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
 savFlg=zeros(6*ncp,ydim);
 savIuv=zeros(6*ncp,ydim); savJuv=zeros(6*ncp,ydim);
 savXsg=zeros(6*ncp,ydim); savYsg=zeros(6*ncp,ydim);

kplot=zeros(ydim,1);
kplot(1:ydim)=1;
%kplot(1)=1;
nf1=1; nf2=6; %- allow to treat only few faces (debug) : n=nf1:nf2,

%------------------------------
%- face 3 & 6 : long cover ]-180;180]
%- face 4 : long in 2 part ]-180;-135] + ]135;180];
% => put together (in xx2) with xMid=180
xMid=zeros(6,1);
xMid(4)=180;

%-- define large value for X,Y position :
XYout=1000;

nPxy=nc*6*nc; nPp2=nPxy+2;
yg2=zeros(nPp2,1); yg2([1:nPxy],1)=reshape(ycg,[nPxy 1]);
xg2=zeros(nPp2,1); xg2([1:nPxy],1)=reshape(xcg,[nPxy 1]);
%-- add missing corner:
xg2(nPxy+1)=xg2(1); yg2(nPxy+1)=yg2(1+2*nc);
xg2(nPxy+2)=xg2(1+3*nc); yg2(nPxy+2)=yg2(1);

%------------------------------
yy2=split_Z_cub(yg2);
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
%-- set long precision ; deal with long=-180 / +180 separation
epsil=dylat*1.e-10;
dd0=abs(xg2+180); ddd=dd0; ddd(find(ddd<epsil))=200; dd1=min(ddd(:));
ddd=abs(xg2-180);          ddd(find(ddd<epsil))=200; dd2=min(ddd(:));
if ( dd1 < dylat/10 | dd2 < dylat/10 ),
  fprintf(' date change line is poorly defined : dd1= %g , dd2= %g\n',dd1,dd2);
  fprintf(' needs to be +/-180 (+/- esilon=%10.3e) ==> Stop here\n',epsil);
  return
end
%-- change long=-180 (+/- epsil) to +180
[II]=find(dd0<epsil); xg2(II)=xg2(II)+360; clear II;
%---------
xx1=split_Z_cub(xg2);
%- dx,dy not used but would be  necessary to compute the length of broken-lines
% [dy6t,dx6t]=split_UV_cub(dyg,dxg);
xx2=xx1;
for n=1:6,
 if xMid(n) ~= 0,
   dec=xMid(n)-180;
   xx2(:,:,n)=rem( xx2(:,:,n)+360-dec, 360) +dec ;
 end
 fprintf('face  %i : x1 min,max= %8.3f %8.3f', ...
             n, min(min(xx1(:,:,n))), max(max(xx1(:,:,n))) );
 fprintf(' ; xMid= %8.3f ; x2 min,max= %8.3f %8.3f \n', ...
             xMid(n), min(min(xx2(:,:,n))), max(max(xx2(:,:,n))) );
end
%------------------------------

%-----------------------------------------
%-- illustrate with a 2.D plot :
if max(kplot) > 0,
 face_color=zeros(7,3); lineThick=3*ones(7);
 face_color(1,:)=[1 0 0]; face_color(4,:)=face_color(1,:);
 face_color(2,:)=[0 1 0]; face_color(5,:)=face_color(2,:);
 face_color(3,:)=[0 0 1]; face_color(6,:)=face_color(3,:);
 face_color(7,:)=[1 0 1]; lineThick(7)=2;

 figure(1); clf;
 %subplot(212);
 ccB=[-25 40]; shift=-1; cbV=2; AxBx=[-180 180 -90 90]; kEnv=0;
 if ydim == 1, AxBx(3:4)=[-15 15]+yl ; end
 grph_CS(zeros(6*nc,nc),xcs,ycs,xg2,yg2,ccB(1),ccB(2),shift,cbV,AxBx,kEnv);
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

%nMx6t=zeros(6,1);
%savI=zeros(nc*6,6); savJ=zeros(nc*6,6); savF=zeros(nc*6,6);
%isav=zeros(nc*6,6); jsav=zeros(nc*6,6); xsav=zeros(nc*6,6);
%find_bk_line
[savI,savJ,savF,isav,jsav,xsav,nMx6t] = ...
find_bk_line( nf1,nf2,nc,ydim,yl,dylat,XYout,xMid,xx1,xx2,yy2,xcs,ycs );

%--------------------------------------
%- define "segments" = continuous part of the broken line :

%ncut=zeros(6,1); icut=zeros(nc,6,6); xcut=zeros(nc,4,6); ycut=zeros(nc,4,6);
%clean_bk_line
[ncut,icut,xcut,ycut,misfit,xyfit] = ....
clean_bk_line( nf1,nf2,nc,ydim,yl,dylat,xMid,xx1,xx2,yy2, ...
               savI,savJ,savF,isav,jsav,xsav,nMx6t );

%-- plot the results:
if kplot(jl) == 2 | ( max(kplot) > 0 & misfit > 0 ),
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
%fprintf( ...
%'n,in= %i %i ; is,ict,x,y= %i %i %6.2f %6.2f ; ie,ict,x,y= %i %i %6.2f %6.2f\n', ...
%   n,in,is,icut(in,is,n),xsav(icut(in,is,n),n),yloc(icut(in,is,n)), ...
%        ie,icut(in,ie,n),xsav(icut(in,ie,n),n),yloc(icut(in,ie,n)) );
  end ; end; end
  if np > 0,
   set(P,'Color',face_color(n,:),'LineWidth',lineThick(n),'LineStyle','-'); end
 end; end
end

if misfit > 0,
 fprintf('misfit= %i , xyfit= %i ; ==> must do something ! \n', ...
    misfit,xyfit);
 return
end

%--------------------------------------
%- output : put together the pieces of bk-lines from the 6 faces :

%save_bk_line
[svNpts,svFlg,svIuv,svJuv,svXsg,svYsg,svXx1,svYy1]= ...
save_bk_line( nf1,nf2,nc,ydim,yl,dylat,XYout,xMid,xx1,yy2,yy2, ...
              savI,savJ,savF,isav,jsav,xsav,ncut,icut,xcut,ycut );
%- easier to debug this way:
savNpts(jl)=svNpts;
savFlg(:,jl)=svFlg;
savIuv(:,jl)=svIuv;
savJuv(:,jl)=svJuv;
savXsg(:,jl)=svXx1;
savYsg(:,jl)=svYy1;

%-- check that this broken-line is different from any previous one :
check_bk_line( nc,ydim,jl,ylat,savNpts,savFlg,savIuv,savJuv,savXsg,savYsg );

if kplot(jl) == 1 | ( max(kplot) > 0 &  misfit > 0 ),
 titr=sprintf('nMx6t= %i %i %i %i %i %i ;  yLat= %8.3f',nMx6t,yl);
 title(titr);
 if lineThick(7) > 0,
   clear Pu ict
   np=1; ict(1)=0;
   sNp = savNpts(jl)+1;
   xloc=savXsg(1:sNp,jl);
   yloc=savYsg(1:sNp,jl);
   ddMx=0; % ddMx=dylat/cos(yl*pi/180);
   if ddMx > 0,
    dd=abs(xloc(2:sNp)-xloc(1:sNp-1));
    fprintf(' np, Max(dd) : %i %8.3f\n',np,max(dd(:)) );
    while max(dd(:)) > ddMx,
     [I]=find(dd==max(dd(:)));
     ict(np+1)=I(1);
     xloc(I+1:sNp+1)=xloc(I:sNp);
     yloc(I+1:sNp+1)=yloc(I:sNp);
     sNp=sNp+1; np=np+1;
     dec=xloc(I+2)-180;
     xloc(I+1)=dec+rem(xloc(I+1)-dec+360,360);
     for j=1:np-1, if ict(j) > I, ict(j)=ict(j)+1; end; end
     dd=abs(xloc(2:sNp)-xloc(1:sNp-1));
     for j=2:np, dd(ict(j))=0; end
     fprintf(' np, Max(dd) : %i %8.3f\n',np,max(dd(:)) );
     if np > 10, dd=0; end
    end
   end
   ict(np+1)=sNp;
   for j=1:np,
     Pu(j)=plot(xloc(1+ict(j):ict(j+1)),yloc(1+ict(j):ict(j+1)));
   end
  %Pu=plot(savXsg(1:nPts,jl),savYsg(1:nPts,jl));
   set(Pu,'Color',face_color(7,:),'LineWidth',lineThick(7),'LineStyle','-');
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
