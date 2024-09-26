kgr=3; nRgr=3; kwr=1;
dbug=1;

%- set ncdf=1 to load MNC (NetCDF) grid-files ;
%   or ncdf=0 to load MDS (binary) grid-files :
 ncdf=0;
 gDir='grid_files/';
 G=load_grid(gDir,10+ncdf);
 xcs=G.xC; ycs=G.yC; xcg=G.xG; ycg=G.yG;

%------------
n1h=size(xcs,1); n2h=size(xcs,2);
if n1h == 6*n2h, nc=n2h;
elseif n1h*6 == n2h, nc=n1h;
else
 error([' grid var size: ',int2str(n1h),' x ',int2str(n2h),' does not fit regular cube !']);
end
nPg=nc*nc*6; ncp=nc+1; nPp2=nPg+2; n6c=nc*6;
%------------

%--------------------------------
%- when stored in long-vector, use "compact" convention (i.e., 1 face after the other)
 if n2h == nc,
   xcg=permute(reshape(xcg,[nc 6 nc]),[1 3 2]);
   ycg=permute(reshape(ycg,[nc 6 nc]),[1 3 2]);
 end
 xcg=reshape(xcg,[nPg 1]); ycg=reshape(ycg,[nPg 1]);
%- add the 2 missing corners:
 xcg(nPg+1)=xcg(1); ycg(nPg+1)=ycg(1+2*nc*nc);
 xcg(nPg+2)=xcg(1+3*nc*nc); ycg(nPg+2)=ycg(1);
 fprintf(' add 1rst corner: %i %8.3f %8.3f\n',nPg+1,xcg(nPg+1,1),ycg(nPg+1,1));
 fprintf(' add  2nd corner: %i %8.3f %8.3f\n',nPg+2,xcg(nPg+2,1),ycg(nPg+2,1));
%--------------------------------

%- make a "small list" of Z point (= with no double point)
xx1=xcg; yy1=ycg;

xx2=split_Z_cub(xcg);
yy2=split_Z_cub(ycg);
xx3=reshape(xx2,ncp*ncp*6,1); yy3=reshape(yy2,ncp*ncp*6,1);

%- store number of "round" (nRd) and for each round:
%  list_N = number of Strean-Function (Psi) to assign
%  list_C = list of Current Psi point to assign
%  list_P = list of Previously assigned Psi point to add to
%  listUV = list of transport (U or V) to use from P to C
%  list_T = list to select sign and component (U or V) to use
%  tagC   = just to keep track of which Psi point has been assigned
%--
list_C=zeros(n6c,n6c); list_P=zeros(n6c,n6c);
listUV=zeros(n6c,n6c); list_T=zeros(n6c,n6c);
list_N=zeros(1,n6c); tagC=zeros(nPp2,1);

%-- start from the N.Pole:
[IJ]=find(yy1==max(yy1));
nRd=1; nbp=length(IJ); list_N(1,nRd)=nbp;
 list_C(1:nbp,nRd)=IJ; tagC(IJ(1:nbp),1)=nRd;

doRd=1;
 fprintf('nRd= %i ; nbp= %i \n',nRd,nbp);
while doRd > 0,
%- next round :
 n2p=0;
 list_c=zeros(n6c,1); list_p=zeros(n6c,1);
 listuv=zeros(n6c,1); list_t=zeros(n6c,1);

%- for all point of the previous list, find the neighbour point not yet tagged:
 for n=1:nbp,
%- find the 4 neighbours:
   ij=list_C(n,nRd);
   i=1+rem(ij-1,nc); j=fix((ij-1)/nc); k=1+fix(j/nc); j=1+rem(j,nc);
   if ij > nPg | i*j==1,
%-- Corner point:
     nbv=3;
     [I]=find( xx3==xx1(ij) & yy3==yy1(ij) ) ;
     if dbug>0, fprintf('Corner: i,j,k= %2i %2i %i ',i,j,k); end
     if length(I) ~= 3, fprintf('Pb C: stop\n'); return; end
     for l=1:length(I),
       i2=I(l);
       j2=fix((i2-1)/ncp); k2=1+fix(j2/ncp); j2=1+rem(j2,ncp); i2=1+rem(i2-1,ncp);
       if dbug>0, fprintf(';  l=%i: %2i %2i %i ',l,i2,j2,k2); end
       if ij==nPg+2,
%-      3 times (nc,1) :
        lc(l)=nc+(j2-1)*nc+(k2-1)*nc*nc; luv(l)=lc(l); typ(l)=-2;
       elseif ij==nPg+1,
%-      3 times (1,nc) :
        lc(l)=i2+(nc-1)*nc+(k2-1)*nc*nc; luv(l)=lc(l); typ(l)=1;
       elseif i2==ncp & j2==1,
        lc(2)=nc+(j2-1)*nc+(k2-1)*nc*nc; luv(2)=lc(2); typ(2)=-2;
       elseif i2==1 & j2==ncp,
        lc(2)=i2+(nc-1)*nc+(k2-1)*nc*nc; luv(2)=lc(2); typ(2)=1;
       end
     end
     if dbug>0, fprintf('\n'); end
     if ij <= nPg,
       lc(1)=ij+1;   luv(1)=ij;     typ(1)=2;
       lc(3)=ij+nc;  luv(3)=ij;     typ(3)=-1;
     end
%    fprintf('Pb C: stop\n'); return;
   else
%-- Not a corner point:
    nbv=4;
%--- set 1rst neighbour i+1 :
    lc(1)=ij+1;   luv(1)=ij;     typ(1)=2;
    if i==nc,
%-  edge point:
      [I]=find( xx1==xx2(i+1,j,k) & yy1==yy2(i+1,j,k) ) ;
      if length(I) ~= 1, fprintf('Pb A-i: stop\n'); return; end
      lc(1)=I(1);
    end
%--- set 2nd  neighbour i-1 :
    if i==1,
%-  edge point:
      [I]=find( xx3==xx1(ij) & yy3==yy1(ij) ) ;
      if dbug>1, fprintf('Edge: i,j,k= %2i %2i %i ',i,j,k); end
      if length(I) ~= 2, fprintf('Pb E-i: stop\n'); return; end
      for l=1:length(I),
        i2=I(l);
        j2=fix((i2-1)/ncp); k2=1+fix(j2/ncp); j2=1+rem(j2,ncp); i2=1+rem(i2-1,ncp);
        if dbug>1, fprintf('; l= %i : i,j,k_2= %2i %2i %i ',l,i2,j2,k2); end
        if k2 ~= k & i2==ncp,
         lc(2)=nc+(j2-1)*nc+(k2-1)*nc*nc; luv(2)=lc(2); typ(2)=-2;
        elseif k2 ~= k & j2==ncp,
         lc(2)=i2+(nc-1)*nc+(k2-1)*nc*nc; luv(2)=lc(2); typ(2)=1;
        end
      end
      if dbug>1, fprintf('\n'); end
    else
      lc(2)=ij-1;   luv(2)=ij-1;   typ(2)=-2;
    end
%--- set 3rd  neighbour j+1 :
    lc(3)=ij+nc;  luv(3)=ij;     typ(3)=-1;
    if j==nc,
%-  edge point:
      [I]=find( xx1==xx2(i,j+1,k) & yy1==yy2(i,j+1,k) ) ;
      if length(I) ~= 1, fprintf('Pb A-j: stop\n'); return; end
      lc(3)=I(1);
    end
%--- set 4th neighbour j-1 :
    if j==1,
%-  edge point:
      [I]=find( xx3==xx1(ij) & yy3==yy1(ij) ) ;
      if dbug>1, fprintf('Edge: i,j,k= %2i %2i %i ',i,j,k); end
      if length(I) ~= 2, fprintf('Pb E-j: stop\n'); return; end
      for l=1:length(I),
        i2=I(l);
        j2=fix((i2-1)/ncp); k2=1+fix(j2/ncp); j2=1+rem(j2,ncp); i2=1+rem(i2-1,ncp);
        if dbug>1, fprintf('; l= %i : i,j,k_2= %2i %2i %i ',l,i2,j2,k2); end
        if k2 ~= k & j2==ncp,
         lc(4)=i2+(nc-1)*nc+(k2-1)*nc*nc; luv(4)=lc(4); typ(4)=1;
        elseif k2 ~= k & i2==ncp,
         lc(4)=nc+(j2-1)*nc+(k2-1)*nc*nc; luv(4)=lc(4); typ(4)=-2;
        end
      end
      if dbug>1, fprintf('\n'); end
    else
      lc(4)=ij-nc; luv(4)=ij-nc; typ(4)=1;
    end
   end
%- keep the untagged points in a temp list:
   for l=1:nbv, if tagC(lc(l))==0,
    n2p=n2p+1;
    list_p(n2p)=ij;  list_c(n2p)=lc(l);
    listuv(n2p)=luv(l); list_t(n2p)=typ(l);
   end; end;
 end
 if kgr == 1 & nRd==nRgr,
  for n=1:n2p,
   xloc=[xx1(list_p(n)) xx1(list_c(n))];
   xloc(2)=xloc(1)+rem(xloc(2)-xloc(1)+540,360)-180;
   yloc=[yy1(list_p(n)) yy1(list_c(n))];
   plot(xloc,yloc,'bx-');
   if n==1, hold on ; end
  end
  hold off ; grid
 elseif kgr == 2 & nRd==nRgr,
  xloc=xx1(list_c(1:n2p)); yloc=yy1(list_c(1:n2p));
  plot(xloc,yloc,'ro'); hold on ;
  xloc=xx1(list_p(1:n2p)); yloc=yy1(list_p(1:n2p));
  plot(xloc,yloc,'bo');
  hold off ; grid ;
 end
 if n2p == 0, doRd=0; else nRd=nRd+1 ; end
%- deal with non-single points:
 nbp=0;
 for n=1:n2p,
  ij=list_c(n); dbl=0;
  if n > 1, [I]=find(list_C(1:nbp,nRd)==ij); dbl=length(I); end
  if dbl==0,
   nbp=nbp+1;
   list_C(nbp,nRd)=list_c(n);
   list_P(nbp,nRd)=list_p(n);
   listUV(nbp,nRd)=listuv(n);
   list_T(nbp,nRd)=list_t(n);
   tagC(ij,1)=nRd;
  elseif dbl==1,
%- choose between 2 origin points: select the smaller Delta-X
   xx=xx1(ij);
   dx1=xx1(list_P(I,nRd)); dx1=rem(dx1-xx+540,360)-180;
   dx2=xx1(list_p(n));     dx2=rem(dx2-xx+540,360)-180;
   if abs(dx2) < abs(dx1),
%- replace I with n :
    list_C(I,nRd)=list_c(n);
    list_P(I,nRd)=list_p(n);
    listUV(I,nRd)=listuv(n);
    list_T(I,nRd)=list_t(n);
   else
%-  remove point n :
    list_c(n)=0;
   end
  else
   fprintf('Pb D: stop\n'); return;
  end
 end
 if nbp > 0,
  list_N(1,nRd)=nbp;
  fprintf('nRd= %i ; nbp= %i \n',nRd,nbp);
 end
%if nRd > 64, doRd=0; end
end

 if kgr == 3,
  for n=2:nRd,
   if n == nRgr, linC='r-o'; else linC='b-'; end
   for i=1:list_N(1,n),
    xloc=[xx1(list_P(i,n)) xx1(list_C(i,n))];
    yloc=[yy1(list_P(i,n)) yy1(list_C(i,n))];
    plot(xloc,yloc,linC);
    if i==1 & n==2, hold on ; end
   end
  end
  hold off ; AA=axis;
  AA(1)=max(-180,AA(1)); AA(2)=min(183,AA(2));
  AA(3)=max(-90,AA(3)); AA(4)=min(90,AA(4));
  axis(AA); grid
 elseif kgr == 4,
  for n=1:nRd,
   if n == nRd, linC='ro'; else linC='ko'; end
   nbp=list_N(1,n);
   xloc=xx1(list_C(1:nbp,n)); yloc=yy1(list_C(1:nbp,n));
   plot(xloc,yloc,linC);
   if n==1, hold on ; end
  end
  xloc=xx1(list_P(1:nbp,nRd)); yloc=yy1(list_P(1:nbp,nRd));
  plot(xloc,yloc,'bx');
  hold off ; grid ;
 end

if kwr==1,
%-- copy to the right sixe arrays:
 psiNx=max(list_N);
 psiNy=nRd;
%- store number of pass (nRd) and for each pass:
%  psi_N = number of Strean-Function (Psi) to assign
%  psi_C = list of Current Psi point to assign
%  psi_P = list of Previously assigned Psi point to add to
%  psiUV = list of transport (U or V) to use from P to C
%  psi_T = list to select sign and component (U or V) to use
%--
 psi_N=zeros(1,psiNy);
 psi_C=zeros(psiNx,psiNy); psi_P=zeros(psiNx,psiNy);
 psiUV=zeros(psiNx,psiNy); psi_T=zeros(psiNx,psiNy);
 psi_N=list_N(1,1:psiNy);
 psi_C=list_C(1:psiNx,1:psiNy);
 psi_P=list_P(1:psiNx,1:psiNy);
 psiUV=listUV(1:psiNx,1:psiNy);
 psi_T=list_T(1:psiNx,1:psiNy);
%- write to a file :
 namf=['psiLine_N2S_cs',int2str(nc)];
 fprintf(['write on file : ',namf,'.mat ']);
 save(namf,'psi_N','psi_C','psi_P','psiUV','psi_T');
 fprintf(' <== O.K. \n');
end
