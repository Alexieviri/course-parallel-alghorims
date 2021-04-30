!                  ОБЪЯВЛЕНИЕ ПЕРЕМЕННЫХ
 implicit none ! запрет на неявное объявление переменных
 real(8), allocatable :: A(:,:),C(:,:),G(:,:);  ! сеточная функция и аналитическое решение
 real(8) mem;                 ! шаги сеточной области по времени и пространству, их отношение
 integer N;                  ! число узлов по времени и пространству
 integer k,i,j;                ! параметры циклов

 N=3;                           !  задаем размер матрице
 allocate (A(1:N,1:N)); allocate (C(1:N,1:N)); allocate (G(1:N,1:N));! выделение памяти под U и аналитическое решение

! ПОДГОТОВКА МАТРИЦЫ
 call random_number(A); G=0; !Заполняем матрицу случайными числами
 A=0.5*(A+transpose(A));  !Делаем матрицу симметричной положительно определённой
 do i=1,N                 !Делаем сумму чисел на главной диагонали больше суммы остальных
   A(i,i)=float(N)+A(i,i);
 enddo
 C=A;                     !Дублируем матрицу А

! ВЕКТОРНЫЙ ВАРИАНТ разложения Холецкого
! do k=1,n
!   A(k:n,k)=A(k:n,k)/sqrt(A(k,k));
!   do j=k+1,n
!     A(j:n,j)=A(j:n,j)-A(j:n,k)*A(j,k);
!   enddo
! enddo

! СКАЛЯРНЫЙ ВАРИАНТ разложения Холецкого
! do k=1,n
!    mem=sqrt(A(k,k));
!    do i=k,n
!      A(i,k)=A(i,k)/mem;
!    enddo
!    do j=k+1,n
!      do i=j,n
!        A(i,j)=A(i,j)-A(i,k)*A(j,k);
!      enddo
!    enddo
! enddo

! ПОГРЕШНОСТЬ
 do j=1,N
   G(j:n,j)=A(j:n,j);
 enddo
 print*, maxval(abs(C-matmul(G,transpose(G))));

deallocate (A); deallocate (C); deallocate (G);! освобождение памяти

end program
