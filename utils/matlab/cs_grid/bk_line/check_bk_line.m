function check_bk_line( ...
         nc,ydim,jl,ylat,savNpts,savFlg,savIuv,savJuv,savXsg,savYsg);

%------------------------------------------------------
if jl > 1 & ydim < 6*nc,
%-- check that this broken-line is different from any previous one :
 jdif=ones(ydim,1);
 it=savNpts(jl);
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

return
