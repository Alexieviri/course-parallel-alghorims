!                  ОБЪЯВЛЕНИЕ ПЕРЕМЕННЫХ
 implicit none ! запрет на неявное объявление переменных
 real(8), allocatable :: A(:,:),C(:,:),G(:,:);  ! сеточная функция и аналитическое решение
 real(8) mem;                 ! шаги сеточной области по времени и пространству, их отношение
 integer N;                  ! число узлов по времени и пространству
 integer k,i,j;                ! параметры циклов
 real    ::  T1,T2            ! замер времени
 
 N=1000;
 allocate (A(1:N,1:N)); allocate (C(1:N,1:N)); allocate (G(1:N,1:N));! выделение памяти под U и аналитическое решение

!Предварительная подготовка матрицы
 call random_number(A); G=0; !Заполняем матрицу случайными числами
 A=0.5*(A+transpose(A));  !Делаем матрицу симметричной положительно определённой
 do i=1,N                 !Делаем сумму чисел на главной диагонали больше суммы остальных
   A(i,i)=float(N)+A(i,i);
 enddo
 C=A;                     !Дублируем матрицу А


call cpu_time(T1)

! Строчный алгоритм разложения холецкого
do k=1,n
  A(k:n,k)=A(k:n,k)/sqrt(A(k,k));
  do i=k+1,n
    A(i,k+1:i)=A(i,k+1:i)-A(i,k)*A(k+1:i,k);
  enddo
enddo  

call cpu_time(T2)

print *, T2 - T1

! Находим погрешность
 do j=1,N
   G(j:n,j)=A(j:n,j);
 enddo
 print*, maxval(abs(C-matmul(G,transpose(G))));

deallocate (A); deallocate (C); deallocate (G);! освобождение памяти

end program
