nx=20; ny=16; nz=23;

fld1=readbin('KPPviscA.0000000010.data',[nx ny]);
fld2=readbin('KPPviscAz.0000000011.data',[nx ny]);
clf, subplot(311),mypcolor(fld1');colorbar
subplot(312),mypcolor(fld2');colorbar
subplot(313),mypcolor(fld2'-fld1');colorbar

fld1=readbin('KPPdiffS.0000000010.data',[nx ny]);
fld2=readbin('KPPdiffKzS.0000000011.data',[nx ny]);
clf, subplot(311),mypcolor(fld1');colorbar
subplot(312),mypcolor(fld2');colorbar
subplot(313),mypcolor(fld2'-fld1');colorbar

fld1=readbin('KPPdiffT.0000000010.data',[nx ny]);
fld2=readbin('KPPdiffKzT.0000000011.data',[nx ny]);
clf, subplot(311),mypcolor(fld1');colorbar
subplot(312),mypcolor(fld2');colorbar
subplot(313),mypcolor(fld2'-fld1');colorbar

fld1=readbin('KPP_ghat.0000000010.data',[nx ny]);
fld2=readbin('KPPghat.0000000011.data',[nx ny]);
clf, subplot(311),mypcolor(fld1');colorbar
subplot(312),mypcolor(fld2');colorbar
subplot(313),mypcolor(fld2'-fld1');colorbar

fld1=readbin('KPP_hbl.0000000010.data',[nx ny]);
fld2=readbin('KPPhbl.0000000011.data',[nx ny]);
fld3=readbin('KPPmld.0000000010.data',[nx ny]);
clf, subplot(311),mypcolor(fld1');colorbar
subplot(312),mypcolor(fld2');colorbar
subplot(313),mypcolor(fld3');colorbar

fld1=readbin('KPPfrac.0000000010.data',[nx ny]);
mypcolor(fld1');colorbar

%%%%%%%%%%%%%%%
delZ=[0 10 10 15 20 20 25 35 50 75 100 150 200 ...
      275 350 415 450 500 500 500 500 500 500 500];
top=cumsum(delZ(1:23));
bot=cumsum(delZ(2:24));
dpt=-(top+bot)/2;
MLD=readbin('KPPmld.0000000010.data',[nx ny]);
T=readbin('T.0000000011.data',[nx ny nz]);
S=readbin('S.0000000011.data',[nx ny nz]);
R=rho(S,T,0*T);
DR=rho(S(:,:,1),T(:,:,1)-.8,0)-R(:,:,1);
mld=0*MLD;
for i=1:nx
  for j=1:ny
    if DR(i,j)>0
      tmp=squeeze(R(i,j,:)-R(i,j,1));
      in=closest(DR(i,j),tmp,0);
      mld(i,j)=(dpt(in(1))*abs(tmp(in(2))-DR(i,j)) + ...
                dpt(in(2))*abs(tmp(in(1))-DR(i,j))) / ...
               abs(R(i,j,in(2))-R(i,j,in(1)));
    end
  end
end
clf, subplot(311),mypcolor(fld3');caxis([0 70]),colorbar
subplot(312),mypcolor(-mld');caxis([0 70]),colorbar
subplot(313),mypcolor(-mld'-fld3');colorbar

%%%%%%%%%%%%%%%%%%%%%%%
ix=10; iy=8;
s=squeeze(S(ix,iy,:));
t=squeeze(T(ix,iy,:));
p=pressure(abs(dpt),26);
r=rho(s,t,0);
dr=
clf, plot(s,dpt,t,dpt,r-r(1),dpt,it,dpt,dr+dpt*0,dpt,'linewidth',3)
legend('S','T','\sigma','dr',4)
axis([0 37 -5450 0]), grid
title(MLD(ix,iy))
