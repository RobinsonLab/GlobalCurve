program convert
implicit none
! vars
!	real :: x,y
	character(100):: inFileName = 'principal_axes_M.txt', &
		outFileName

!	real,dimension(n,len) :: c
	real,dimension(3) :: com,vec,i1,i2,i3,a1,a2,a3,v1,v2,v3,com_l
	real,dimension(3,3) :: B
	integer :: i,ios
!	integer,dimension(n,n) :: D

! begin

	open(2,file=inFileName,iostat=ios,status='old')
!	do i=1,len
        READ(2,*)com(:)
!	end do
		read(2,*)vec(:)
		read(2,*)v2(:) ! the principal axes
		read(2,*)v3(:)
		read(2,*)v1(:)
	close(2,iostat=ios)


com_l=w_to_local(com,0) ! should be 0,0,0
a1=v1/sqrt(dot_product(v1,v1))
a2=v2/sqrt(dot_product(v2,v2))
a3=v3/sqrt(dot_product(v3,v3))

i1=(/0.999989,0.004596,0.0/)
i2=(/-0.004596,0.999989,0.0 /)
i3=(/0.0, 0.0, 1.0 /)

	B(1,1)=dot_product(a1,i1)
	B(1,2)=dot_product(a1,i2)
	B(1,3)=dot_product(a1,i3)
	B(2,1)=dot_product(a2,i1)
	B(2,2)=dot_product(a2,i2)
	B(2,3)=dot_product(a2,i3)
	B(3,1)=dot_product(a3,i1)
	B(3,2)=dot_product(a3,i2)
	B(3,3)=dot_product(a3,i3)
	
	open(2,file='out.txt',iostat=ios,status='replace')
	do i=1,3
		write(2,*)B(:,i)
	enddo

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

!function w_to_body(v,i)
!use transform_coords ! B
!use h_files, only :: A,t
!implicit none	
		
!	real,dimension(3) :: w_to_body 

! begin
!	w_to_local = B*A(i)*(v-t(i))

!end funciton


end program

