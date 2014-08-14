program convert
implicit none
	integer,parameter :: len=2961
	integer,parameter :: bins=10
! vars
!	real :: x,y

	character(100):: inFileName = 'actin_M_coordinates.txt', &
		outFileName

	real,dimension(3,len) :: c
	real,dimension(3) :: com,vec,i1,i2,i3,a1,a2,a3,v1,v2,v3,ct
	real,dimension(len) :: dist
	integer :: i,j,bin,ios
!	integer,dimension(bins) :: bin	
	real :: d_cut,max_dist,d


! begin

! just playing here

	v=b_to_w((/20,0,0/),0)

!	pause


	open(2,file=inFileName,iostat=ios,status='old')
	do i=1,len
        READ(2,*)c(:,i)
	end do
	max_dist=0.0
	do i=1,len
        ct(:)=w_to_body(c(:,i),0)
		d=sqrt(ct(2)**2+ct(3)**2)
		dist(i)=d
		if (d>max_dist) max_dist=d
	end do

! make a histogram
	
	open(2,file='out.txt',iostat=ios,status='replace')

	do i=1,bins
	bin=0
	d_cut=max_dist/bins*i	
		do j=1,len
			if (dist(j)<=d_cut) bin=bin+1
		enddo
		write(2,*)d_cut,bin
	enddo
	bin=0
		do j=1,len
			if (dist(j)<=24) bin=bin+1
		enddo
		write(2,*)24.0,bin
	


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

