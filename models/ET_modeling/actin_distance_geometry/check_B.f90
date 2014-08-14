program convert
implicit none
! vars
!	real :: x,y
	character(100):: inFileName = 'principal_axes_M.txt', &
		outFileName

!	real,dimension(n,len) :: c
	real,dimension(3) :: com,vec,i1,i2,i3,a1,a2,a3,v1,v2,v3,com_l
!	real,dimension(3,3) :: B
	integer :: i,ios
!	integer,dimension(n,n) :: D

! begin


i1=(/-23.944,-6.852,-43.818/) ! L coords of cys374 Calpha
i2=(/21.609,12.382,-16.318/) ! M
i3=(/-18.019,-17.192,11.182/) !N

a1=w_to_body(i1,-1) 
a2=w_to_body(i2,0) 
a3=w_to_body(i3,1) 

	open(2,file='out.txt',iostat=ios,status='replace')
		write(2,*)a1(:)
		write(2,*)a2(:)
		write(2,*)a3(:)

	close(2,iostat=ios)


contains
! --------------
function A(i)
use transform_coords
implicit none

	integer,intent(in) :: i
	real,dimension(3,3) :: A

! vars
	real :: theta

! begin
	theta=theta0+i*theta1

	A(1,1)=cos(theta)
	A(1,2)=sin(theta)
	A(1,3)=0.0
	A(2,1)=-sin(theta)
	A(2,2)=cos(theta)
	A(2,3)=0.0
	A(3,1)=0.0
	A(3,2)=0.0
	A(3,3)=1.0
end function A

!----------------------
function t(i)
use transform_coords
implicit none

	integer,intent(in) :: i
	real,dimension(3) :: t

! vars
	real :: theta

! begin
	theta=theta0+i*theta1

	t(1)=r*cos(theta)
	t(2)=r*sin(theta)
	t(3)=z0+i*z1

end function t


!----------------
function w_to_local(v,i)
!use transform_coords
!use h_files, only :: A,t
implicit none	
	real,dimension(3),intent(in) :: v
	integer,intent(in) :: i	
	real,dimension(3) :: w_to_local 

! begin
	w_to_local = matmul(A(i),(v-t(i)))

end function w_to_local


!---------------
function w_to_body(v,i)
use transform_coords ! B
!use h_files, only :: A,t
implicit none	
	real,dimension(3),intent(in) :: v
	integer,intent(in) :: i			
	real,dimension(3) :: w_to_body 

! begin
	w_to_body = matmul(B,matmul(A(i),(v-t(i))))

end function w_to_body 


end program

