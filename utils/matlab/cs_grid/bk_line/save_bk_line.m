function [svNpts,svFlg,svIuv,svJuv,svXsg,svYsg,svXx1,svYy1]=save_bk_line( ...
         nf1,nf2,nc,ydim,yl,dylat,XYout,xMid,xx0,yy0,yy1, ...
         savI,savJ,savF,isav,jsav,xsav,ncut,icut,xcut,ycut);
% [svNpts,svFlg,svIuv,svJuv,svXsg,svYsg,svXx1,svYy1]=save_bk_line( ...
%        nf1,nf2,nc,ydim,yl,dylat,XYout,xMid,xx0,yy0,yy1, ...
%        savI,savJ,savF,isav,jsav,xsav,ncut,icut,xcut,ycut);
%- output : put together the pieces of bk-lines from the 6 faces

%--------------------------------------

%- output arrays:
% ylat=-87:3:87;
% if ydim > 1,  ydim=length(ylat); end
% savNpts=zeros(ydim,1);
% savFlg=zeros(6*nc,ydim);
% savIuv=zeros(6*nc,ydim); savJuv=zeros(6*nc,ydim);
% savXsg=zeros(6*nc,ydim); savYsg=zeros(6*nc,ydim);
  ncp=nc+1;
  svNpts=zeros(1,1);
  svFlg=zeros(6*ncp,1);
  svIuv=zeros(6*ncp,1); svJuv=zeros(6*ncp,1);
  svXsg=zeros(6*ncp,1); svYsg=zeros(6*ncp,1);
  svXx1=zeros(6*ncp,1); svYy1=zeros(6*ncp,1);

ii=0; misfit=0;
for n=nf1:nf2, for in=1:ncut(n),
%if icut(in,3,n) >= 0  & icut(in,4,n) > icut(in,3,n),
 if icut(in,6,n) >= 0 & icut(in,3,n) > 0  & icut(in,4,n) > icut(in,3,n),
  ii=ii+1; ninv(ii)=n; jinv(ii)=in;
  x1loc(ii)=xcut(in,3,n) ;
end; end ; end ; nSegm=ii; 
x2loc=zeros(nSegm,1); x2loc=x1loc(1:nSegm);

it=0;
for ns=1:nSegm,
  Xmn=min(x2loc); ii=find(x2loc==Xmn);
  n=ninv(ii); in=jinv(ii);
  if it == 0,
   svXx1(1)=xcut(in,3,n); 
   svYy1(1)=ycut(in,3,n); 
   svXsg(1)=xx0(isav(icut(in,3,n),n),jsav(icut(in,3,n),n),n);
   svYsg(1)=yy0(isav(icut(in,3,n),n),jsav(icut(in,3,n),n),n);
  else
   if svXx1(it+1) ~= xcut(in,3,n) | svYy1(it+1) ~= ycut(in,3,n),
    fprintf('=> conection Pb: previous Seg end= %8.3f %8.3f \n', ...
            svXx1(it+1),svYy1(it+1) );
    fprintf(['  next Seg: ns= %i ; n,in,icut= %i %i %i %i ;', ...
             ' X,Y= %8.3f %8.3f \n'], ...
            ns,n,in,icut(in,3,n),icut(in,4,n),xcut(in,3,n),ycut(in,3,n));
    misfit=misfit+1;
   end
  end
  for i=icut(in,3,n):icut(in,4,n)-1,
   it = it + 1;
   svFlg(it)=savF(i,n);
   svIuv(it)=savI(i,n)+nc*(n-1);
   svJuv(it)=savJ(i,n);
%- a hack to distinguish between for isolat / great-circle cases:
   svXx1(it+1)=xsav(i+1,n);
   svYy1(it+1)=yy1(isav(i+1,n),jsav(i+1,n),n);
   svXsg(it+1)=xx0(isav(i+1,n),jsav(i+1,n),n);
   svYsg(it+1) =yy0(isav(i+1,n),jsav(i+1,n),n);
   
  end
  x2loc(ii)=XYout;
end
 svNpts=it;

%--------------------------------------------------------------------
return
