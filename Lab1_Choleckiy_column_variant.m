clear all;
n=100;
A=rand(n,n);  % Создаём рандомизированную матрицу
A=0.5*(A+A'); % Делаем матрицу симметричной и положительно-определенной
A=A+n*eye(n); % Делаем сумму чисел на главной диагонали больше суммы остальных
B=A;          % Дублируем матрицу А

% матричный вариант
%for k=1:n
%   A(k:n,k)=A(k:n,k)/sqrt(A(k,k));
%   A(k+1:n,k+1:n)=A(k+1:n,k+1:n)-A(k+1:n,k)*(A(k+1:n,k))';
%end 

% векторный вариант
%for k=1:n
%   A(k:n,k)=A(k:n,k)/sqrt(A(k,k));
%   for j=k+1:n
%     A(j:n,j)=A(j:n,j)-A(j:n,k)*A(j,k);
%   end
%end

% скалярный вариант
%for k=1:n
%   mem=sqrt(A(k,k));
%   for i=k:n
%     A(i,k)=A(i,k)/mem;
%   end
%   for j=k+1:n
%     for i=j:n
%       A(i,j)=A(i,j)-A(i,k)*A(j,k);
%     end
%   end
%end
 

G=tril(A,0);
norm(B-G*G')
