 prefix='dynStD';
 flxPfx=''; wsCor=0;
 namA={'std'};
%namA={'r02','t00'};
 Nexp=size(namA,2);

%- to read recond set of stats (time-mean):
flxPfx='flxStD'; wsCor=1;

nItMx=1e10*ones(1,Nexp); %nItMx(3)=11;
nItMx=2400*ones(1,Nexp);
namLg=namA ; namLg=strrep(namLg,'_','\_');
%-----------
%- test if the variable krd is define :
if size(who('krd'),1) > 0,
 fprintf('krd is defined and = %i \n',krd);
else
 fprintf('krd undefined ; set to 1 \n'); krd=1 ;
end
if krd > 0,
%- define list of fields to read in:
 clear listV ;
%listV={'Eta','U','W','T','S','ADVx_TH','ADVr_TH','AB_gT','AB_gS'};
%listV={'Eta','U','W','T','S','ADVx_TH','ADVr_TH','gSinAB'};
%- or take all them:
 clear listV ; listV='all_flds';
%-----------

%- start to read the longest record:
  n=1; rf=-1; if strcmp(char(listV),'all_flds'), rf=0; end
  [ntA(n),rList,tim,vv1,listV] = ...
    read_StD(prefix,namA(n),listV);
  nIt=ntA(n); nk=size(vv1,1); nRg=size(vv1,3);
%- set global dims: & load vvA --> vvB
  nbV=size(listV,2);
  nrec=nIt; n3d=nk; nReg=nRg;
  vvA=zeros(n3d,nrec,nReg,5,nbV,Nexp); tiA=zeros(nrec,2,Nexp);
  vvA(1:nk,1:nIt,1:nRg,:,:,n)=vv1; tiA(1:nIt,:,n)=tim;
%----
 for n=2:Nexp,
  [ntA(n),rList,tim,vv1,listV] = ...
    read_StD(prefix,namA(n),listV);
  nIt=ntA(n); nk=size(vv1,1); nRg=size(vv1,3);
  if (nrec < nIt),
    fprintf('\n');
    error([' Nb of records=',int2str(nIt),' exceeds nrec=',int2str(nrec)]);
  end
  if (n3d < nk),
    fprintf('\n');
    error([' Nb of Levels=',int2str(nk),' exceeds n3d=',int2str(n3d)]);
  end
  vvA(1:nk,1:nIt,1:nRg,:,:,n)=vv1; tiA(1:nIt,:,n)=tim;
 end;
 if krd == 2,
   fprintf('save to "sav_StD.mat" file ...');
   save('sav_StD.mat','vvA','tiA','ntA','rList','listV');
   fprintf(' done\n')
 end
elseif krd < 0,
 fprintf('load from "sav_StD.mat" file ...');
 load sav_StD
 fprintf(' done\n'); nbV=size(listV,2);
end
if krd ~= 0,
 ttA=squeeze(tiA(:,2,:));
 ttA=ttA/3600; titT='hrs';  ttA=ttA/24; titT='days';
%ttA=ttA/30 ; titT='month'; ttA=ttA/12 ; titT='year';
end
%---------------------------------------------------------
if krd ~= 0,
 if length(flxPfx) > 0 ,
   n=1;
   [nIt2,rList2,tim2,vv2,listV2] = read_StD(flxPfx,namA(n),'all_flds');
   nk2=size(vv2,1); nbV2=size(listV2,2);
   vvB=zeros(nk2,nIt2,5,nbV2,Nexp); tiB=zeros(nIt2,2,Nexp);
   % save only 1 region (3rd index)
   vvB(:,:,:,:,n)=vv2(:,:,1,:,:); tiB(:,:,n)=tim2;
  for n=2:Nexp,
   [nt2,rListi2,tim2,vv2,listV2] = read_StD(flxPfx,namA(n),listV2);
   vvB(:,1:nt2,:,:,n)=vv2(:,:,1,:,:); tiB(1:nt2,:,n)=tim2;
  end
 else listV2=listV; nIt2=nrec-1; end
end
%=========================================================

ttax1=0 ; ttax2=0 ; ttay=zeros(nbV,2);
%-- fixed time axis bound :
% ttax1=15.; ttax2=20.;
%-- fixed Y axis bound :
% ttay(4,:)=[0 0.6];
%-----------
fprintf('Total length: ntA=');fprintf(' %i ,',ntA); fprintf(' \n');
for n=1:Nexp,
fprintf(' exp %i : time(d):%10.2f ->%10.2f \n', n,ttA(1,n),ttA(ntA(n),n) );
end;
%--

list_on=zeros(1,nbV);
nbG=8;
%nbG=min(nbG,nbV); list_on(1:nbG)=1 ;
 list_on(3:4)=1;
%list_on(1:6)=[1 1 1 1 1 1];

isA=ones(1,Nexp); ieA=ntA;
%- limit the length :
%isA=isA*31 ; % drop the 1rst mnth (1 Monitor/d)
%ieA(:)=240; %isA(:)=1;

lThick=0; %- default line thickness
linA={'k-','b-','r-','g-','m-','c-'};
% lThick=1.5;
linA={'k-','b--','r-','g-','m-','c-'};
 lThick=1.5;

ieA=min(ieA,nItMx);
%titall='AIM , Cubic-G (32x32) , cpl-FM Forcing' ;
%titall='Global Ocean, Cubic-G (32x32) , CORE Forc (2)' ;
titall='Advect\_xz (nlfs) test experiment' ;

%=========================================================

for ng=1:nbV,
%-------------------
 flag=list_on(ng); kl=0;
 vv1=vvA(:,:,:,:,ng,:); namV=char(listV(ng));
 titv=strrep(namV,'_','\_');
%if strcmp(namV,'Eta'), vv1=vv1/100; titv='Eta [mb]'; end
%if strcmp(namV,'T'), kl=1; end		% <-- to get surf.Temp
%if ng == 1, flag=2*list_on(1) ; end
 if kl > 0, titv=[titv,'\_',int2str(kl)];
   fprintf([' var= ',namV,' at level k= %i \n'],kl);
 end

 if flag == 1
%--
 %figure(ng); set(ng,'position',[100+100*ng 60+40*ng 500 700]);clf;
  figure(ng); PP=get(ng,'position'); set(ng,'position',[PP(1:2) 500 700]);clf;
  var=squeeze(vv1(1+kl,:,1,:,:)); dd=zeros(5,Nexp); av=zeros(5,Nexp);
  for n=1:Nexp,
   dd(:,n)=max(var(isA(n):ieA(n),:,n))-min(var(isA(n):ieA(n),:,n));
   av(:,n)=mean(var(isA(n):ieA(n),:,n));
  end
  for nv=1:4,
    subplot(410+nv); ttmn=' Mx-mn:'; ttav=' Av:';
    for n=1:Nexp,
      LL(n)=plot(ttA(isA(n):ieA(n),n),var(isA(n):ieA(n),nv,n),char(linA(n)));
      if n == 1, hold on ; end ;
      ttmn=sprintf([ttmn,' %2.1e ;'],dd(nv,n));
      ttav=sprintf([ttav,' %3.2e ;'],av(nv,n));
    end ; hold off ;
    if lThick > 0, set(LL,'LineWidth',lThick); end
    AA=axis ; dAA=AA(4)-AA(3);
    if AA(3)*AA(4) <= 0, AA(3)=min(AA(3),-dAA/10); AA(4)=max(AA(4),dAA/10); end
    if ttax1 < ttax2, AA(1)=ttax1; AA(2)=ttax2; end;
    axis(AA); grid ;
    if nv == 1, title(['Avr ',titv,'  ',ttmn]); end
    if nv == 2, title(['Std-Dev ',titv,'  ',ttav]); end
    if nv == 3, title(['min ',titv,'  ',ttav]);
              legend(namLg,'Location','best'); end
    if nv == 4, title(['Max ',titv,'  ',ttav]); end
  end ; xlabel(titT);
%--
  axes('position',[.01,.01,.99,.99],'Visible','off');
  T=text(0.5,0.97,titall);
  set(T,'HorizontalAlignment','center','FontSize',12);
  Td=text(0.99,0.01,date);
  set(Td,'HorizontalAlignment','right','FontSize',6);
%---
 end

%-------------------
end

%=========================================================
 %-- get mean-depth + surface-level index from a pair of 3d,2d vars:
 nm3d='T';   j3d=0; [I]=strcmp(listV,nm3d); if sum(I) == 1, j3d=find(I==1); end
 nm2d='Eta'; j2d=0; [I]=strcmp(listV,nm2d); if sum(I) == 1, j2d=find(I==1); end
 if j2d*j3d > 0,
   vol=squeeze(vvA(:,1,1,5,j3d,:)); area=squeeze(vvA(1,1,1,5,j2d,:));
   hAvr=vol(1,:)'./area;
   for n=1:Nexp, [I]=find(vol(2:end,n) > 0.); kSrf(n)=I(1); end
   fprintf(' from j3d,j2d= %i,%i volume ratio, get:\n',j3d,j2d);
   for n=1:Nexp, fprintf('  - kSrf= %i , mean-Depth=%7.1f\n',kSrf(n),hAvr(n)); end
 else
   fprintf(' missing vars "%s" or "%s" --> use default kSrf,hAvr\n',nm3d,nm2d);
   kSrf=ones(Nexp,1); hAvr=kSrf;
 end
 %---------
 trF={'TH','SLT'}; nbTr=length(trF);
  nSav=4; titV={'Total','Wsrf','gAB','Resid'}; ndt=nrec-1;
  ndt=nrec-1; savVar=zeros(ndt,nSav*nbTr,Nexp);
  ttD=(ttA(1:nrec-1,:)+ttA(2:nrec,:))*0.5;
 for j=1:nbTr,
   sNam=char(trF(j)); namD=['W',sNam,'MASS']; namAB=['AB_g',sNam(1)];
   jTr=0; [I]=strcmp(listV,sNam(1));
   if sum(I) == 1, jTr=find(I==1); end
   jWs=0; [I]=strcmp(listV2,namD);
   if sum(I) == 1, jWs=find(I==1); end
   jAB=0; [I]=strcmp(listV2,namAB);
   if sum(I) == 1, jAB=find(I==1); end
  fprintf(' %3s : jTr,jWs,jAB= %i , %i , %i \n',sNam,jTr,jWs,jAB);
  j1= 1 + (j-1)*nSav; j3=j1+3;
  if jTr > 0,
    %-- save time series:
    locVar=reshape(vvA(1,:,1,1,jTr,:),[nrec Nexp]);
    %- rate of change: do finite difference from difference between record:
    locVar(1:nrec-1,:)=locVar(2:nrec,:)-locVar(1:nrec-1,:);
    locVar=reshape(locVar(1:ndt,:),[ndt Nexp]);
    %  divided by time-interval between record:
    delTA=reshape(tiA(2:nrec,2,:)-tiA(1:nrec-1,2,:),[ndt Nexp]);
    recDTA=delTA; recDTA(find(delTA==0))=-1; recDTA=1./recDTA; recDTA=max(recDTA,0);
    %-- save time series:
    savVar(1:ndt,j1,:)=locVar.*recDTA;
    savVar(1:ndt,j3,:)=savVar(1:ndt,j1,:);
  end
  if jAB > 0, jj=j1+2;
    tmpVar=reshape(vvB(1,:,1,jAB,:),[nIt2 Nexp]);
    savVar(1:ndt,jj,:)=tmpVar(1:ndt,:);
  end
  if jTr*jWs > 0, jj=j1+1;
    tmpVar=zeros(nIt2,Nexp);
    for n=1:Nexp, tmpVar(:,n)=squeeze(vvB(1+kSrf(n),:,1,jWs,n)); end
    tmpVar=tmpVar./repmat(hAvr',[nIt2 1]); %- divide by mean depth
    savVar(1:ndt,jj,:)=tmpVar(1:ndt,:);
  end
  if jTr*jWs*wsCor > 0, jj=j1+1;
    savVar(:,j3,:)=savVar(:,j3,:)+savVar(:,jj,:);
  end
  if jAB > 0, jj=j1+2;
    savVar(:,j3,:)=savVar(:,j3,:)-savVar(:,jj,:);
  end
 end
%-------------------
  ieB=max(isA,ieA-1);
  for j2=1:nbTr,
   ng=8+j2;
   figure(ng); PP=get(ng,'position'); set(ng,'position',[PP(1:2) 500 700]);clf;
   for j1=1:nSav,
    nv=j1+(j2-1)*nSav;
    tit_fg=[char(trF(j2)),' tendency : ',char(titV(j1))];
    nsp=nSav*100+10+j1; subplot(nsp)
    for n=1:Nexp,
      LL(n)=plot(ttD(isA(n):ieB(n),n),savVar(isA(n):ieB(n),nv,n),char(linA(n)));
      if n == 1, hold on ; end ;
    end ; hold off ;
    if lThick > 0, set(LL,'LineWidth',lThick); end
    AA=axis ; dAA=AA(4)-AA(3);
    if AA(3)*AA(4) <= 0, AA(3)=min(AA(3),-dAA/10); AA(4)=max(AA(4),dAA/10); end
    if ttax1 < ttax2, AA(1)=ttax1; AA(2)=ttax2; end;
    axis(AA); grid ;
    title(tit_fg);
   end
   xlabel(titT);
  end

