
%--------------------------------------
%- output : put together the 6 faces & save to a file (if kwr =1) :

%- output arrays:
% ylat=-87:3:87;
% if ydim > 1,  ydim=length(ylat); end
% savNpts=zeros(ydim,1);
% savFlg=zeros(6*nc,ydim);
% savIuv=zeros(6*nc,ydim); savJuv=zeros(6*nc,ydim);
% savXsg=zeros(6*nc,ydim); savYsg=zeros(6*nc,ydim);

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
   savXsg(1,jl)=xcut(in,3,n); 
   savYsg(1,jl)=ycut(in,3,n); 
  else
   if savXsg(it+1,jl) ~= xcut(in,3,n) | savYsg(it+1,jl) ~= ycut(in,3,n),
    fprintf('=> conection Pb: previous Seg end= %8.3f %8.3f \n', ...
            savXsg(it+1,jl),savYsg(it+1,jl) );
    fprintf(['  next Seg: ns= %i ; n,in,icut= %i %i %i %i ;', ...
             ' X,Y= %8.3f %8.3f \n'], ...
            ns,n,in,icut(in,3,n),icut(in,4,n),xcut(in,3,n),ycut(in,3,n));
    misfit=misfit+1;
   end
  end
  for i=icut(in,3,n):icut(in,4,n)-1,
   it = it + 1;
   savFlg(it,jl)=savF(i,n);
   savIuv(it,jl)=savI(i,n)+nc*(n-1);
   savJuv(it,jl)=savJ(i,n);
   savXsg(it+1,jl)=xsav(i+1,n);
   savYsg(it+1,jl)=yy2(isav(i+1,n),jsav(i+1,n),n);
  end
  x2loc(ii)=XYout;
end
 savNpts(jl)=it;

%------------------------------------------------------
if jl > 1 & ydim < 6*nc,
%-- check that this broken-line is different from any previous one :
 jdif=ones(ydim,1);
 for j=1:jl-1,
  if savNpts(j)==it, 
   jdif(j)= max(abs(savXsg(1:it+1,j)-savXsg(1:it+1,jl))) ;
   jdif(j)= jdif(j)+max(abs(savYsg(1:it+1,j)-savYsg(1:it+1,jl))) ;
  end
 end
 [JJ]=find(jdif == 0);
 if length(JJ) ~= 0, 
   fprintf('WARNING => line identical to Ylat=');
   for j=1:length(JJ), fprintf(' %8.3f,',ylat(JJ(j))); end ; fprintf('\n');
 end
end
%------------------------------------------------------

%--------------------------------------------------------------------
return
