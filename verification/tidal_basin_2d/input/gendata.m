nx=80;ny=1;nz=30;
H=4200;
z0=400;

disp('Constant DZ')
dz=H/nz*ones(1,nz);
zf=cumsum([0 -dz]); zc=(zf(1:end-1)+zf(2:end))/2;
disp(['delZ=' sprintf('%4.0f.,',dz)])
t0=30+10/4000*zc;
disp('Linear T')
disp(['tRef=' sprintf('%2.5f,',t0)])
t0=20+20*exp(zc/300);t0(find(zc>=-z0))=max( t0(find(zc<-z0)) );
disp('Expontential T')
disp(['tRef=' sprintf('%2.5f,',t0)])

disp('Variable DZ')
dz=gen_dz(nz,H,10,300);
zf=cumsum([0 -dz]); zc=(zf(1:end-1)+zf(2:end))/2;
disp(['delZ=' sprintf('%4.0f.,',dz)])
t0=30+10/4000*zc;
disp('Linear T')
disp(['tRef=' sprintf('%2.5f,',t0)])

subplot(121)
plot(t0,zc)
xlabel('T (^oC)');ylabel('Depth (m)')
set(gca,'Fontsize',14)
axis([19.5 30.5 -4000 0])
print -djpeg100 -r90 t0lin.jpg
print -depsc2 t0lin.eps

disp('Expontential T')
t0=20+20*exp(zc/300);t0(find(zc>=-z0))=max( t0(find(zc<-z0)) );
disp(['tRef=' sprintf('%2.5f,',t0)])

plot(t0([1:end end]),[zc zf(end)])
xlabel('T (^oC)');ylabel('Depth (m)')
set(gca,'Fontsize',14)
axis([19.5 24.5 -4200 0])
print -djpeg100 -r90 t0exp.jpg
print -depsc2 t0exp.eps
