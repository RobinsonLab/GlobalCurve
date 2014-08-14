program convert
implicit none
	integer,parameter :: len=2961
	real,parameter :: r_max = 30.0
	integer,parameter :: acc_lim=2
	integer,parameter :: gs=50
	real,parameter :: rise=25.0
! vars
!	real :: x,y

	character(100):: inFileName = 'calc_TE.in', &
		outFileName

	real,dimension(3,len) :: c
	real,dimension(3) :: com,vec,i1,i2,i3,a1,a2,a3,v1,v2,v3,ct
	real,dimension(len) :: dist
	integer :: i,j,k
	integer :: bin,ios
!	integer,dimension(bins) :: bin	
!	real :: d_cut,max_dist,d
	real,dimension(3) :: res,res_b
	real,dimension(3,-acc_lim:acc_lim) :: acc,acc_w
	real,dimension(-acc_lim:acc_lim) :: E
!	real,dimension(-gs:gs,-gs:gs) :: E_tot,f_0,coords
	real :: den,te,r,d,E_t,f1,f2,R_0,d_obs,d_sav
	real,dimension(3) :: v,don
!	real,dimension(1,-) :: coords




! begin
!	v=b_to_w((/20.0,20.0,20.0/),0)
! world coordinates of the acceptor at residue 0
	open(2,file=inFileName,iostat=ios,status='old')
!	do i=1,len
        READ(2,*)res(:)! world coordinates of the acceptor in chain 0
		read(2,*)R_0
!	end do
    close(2,iostat=ios)
	res_b=w_to_b(res,0)

! get the positions of the acceptor probes in body coords -2,-1,0,1,2

	open(2,file='out.txt',iostat=ios,status='replace')

	do i=-acc_lim,acc_lim
		acc(:,i)=w_to_b(b_to_w(res_b,i),0)
		acc_w(:,i)=b_to_w(res_b,i)
		write(2,*)acc(:,i)
	enddo


! do a grid search along the actin surface radius = r_max

	don(1)=rise
	do i=-gs,gs
		don(2)=real(i)/gs*r_max
		do j=-gs,gs
			don(3)=real(j)/gs*r_max
!			coords(i,j)=don(2:3)
			r=sqrt(dot_product(don(2:3),don(2:3)))
			if (r <=r_max) then
				den=1.0
				do k=-acc_lim,acc_lim
					v=don-acc(:,k)
					d=sqrt(dot_product(v,v))
					te=(R_0/d)**6
					E(k)=te
					den=den+te
					if (k==0) d_sav=d
				enddo
				E(:)=E(:)/den
!				E_tot(i,j)=sum(E)  ! only care about total TE and
!				f_0(i,j)=E(0)/E_tot(i,j) ! frac goint to chain 0
				E_t=sum(E)				
				f1=E_t-E(0) ! here f is the et to other residues
				d_obs=R_0*(1./E_t-1)**(1./6.)
				f2=(d_sav-d_obs)/d_sav
			else
!				E_tot(i,j)=0.0
!				f_0(i,j)=0.0
				E_t=0.0				
				f1=0.0
				f2=0.0
			endif
			write(2,'(6F10.3)')don,E_t,f1,f2
		enddo ! j
	enddo ! i


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
function w_to_l(v,i)
!use transform_coords
!use h_files, only :: A,t
implicit none	
	real,dimension(3),intent(in) :: v
	integer,intent(in) :: i	
	real,dimension(3) :: w_to_l

! begin
	w_to_l = matmul(A(i),(v-t(i)))

end function w_to_l


!---------------
function w_to_b(v,i)
use transform_coords ! B
!use h_files, only :: A,t
implicit none	
	real,dimension(3),intent(in) :: v
	integer,intent(in) :: i			
	real,dimension(3) :: w_to_b

! begin
	w_to_b = matmul(B,matmul(A(i),(v-t(i))))

end function w_to_b

function b_to_w(v,i)
use transform_coords ! B
!use h_files, only :: A,t
implicit none	
	real,dimension(3),intent(in) :: v
	integer,intent(in) :: i			
	real,dimension(3) :: b_to_w

! begin
	b_to_w = matmul(transpose(A(i)),matmul(transpose(B),v))+t(i)

end function b_to_w






end program

