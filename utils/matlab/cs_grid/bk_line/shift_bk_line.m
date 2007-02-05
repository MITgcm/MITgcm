function [nABpts,svFlg,svIJuv,svXsg,svYsg] = shift_bk_line( ...
         nc,ydim,jl,xa,ya,xb,yb,savNpts,savFlg,savIuv,savJuv,savXsg,savYsg);

[I]=find(savXsg(:,jl) == xa & savYsg(:,jl) == ya);
if length(I) == 1, fprintf(' line position of A : %i ;',I);
else error(' A disapear !'); end
if I > 1,
%- shift so that it starts from A:
sNp=savNpts(jl);
svFlg([1:sNp])=savFlg([I:sNp 1:I-1],jl);
svIuv([1:sNp])=savIuv([I:sNp 1:I-1],jl);
svJuv([1:sNp])=savJuv([I:sNp 1:I-1],jl);
svIJuv=svIuv+6*nc*(svJuv-1);
svXsg([1:sNp+1])=savXsg([I:sNp 1:I],jl);
svYsg([1:sNp+1])=savYsg([I:sNp 1:I],jl);
end

[J]=find(svXsg == xb & svYsg == yb);
if length(J) == 1, fprintf(' line position of B : %i\n',J+I-1);
else error(' B disapear !'); end
nABpts=J-1;

return
