function get_distance(current_model,current_data) result(E_t)
	use h_struct
	use h_vars
	use numerical_libraries, only : binom 
!	use transform_coords
	implicit none
		
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: E_t
! This procedure calculates the the total transfer efficiency in "num" experiments
! For each experiment, we need an R_0 and the world coordinates of the acceptor attached
! to actin_0.  Both are input as model parameters

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

! Local params

	integer,parameter :: acc_lim = 2
	real,parameter :: offset = 100.
	integer,parameter :: num_actin = (2*acc_lim+1)
	integer,parameter :: num_config = 2**num_actin

	! we use an offset because the minimization procedure can blow up when a parameter 
	! value goes through 0.
! Local Parameters

	integer :: numexp ! ,num_life
	real,dimension(size(current_model%model%param(2)%param_basis)) :: alpha,R_0
	real,dimension(3,size(current_model%model%param(2)%param_basis)) :: acc_w,acc_b
	real,dimension(3,size(current_model%model%param(2)%param_basis),&
							-acc_lim:acc_lim) :: acc
	! so acc is a 3,numexp,-acc_lim:acc_lim matrix with x,y,z coords for each meas in a col.

	integer :: i,j,k,num_item
	real,dimension(-acc_lim:acc_lim) :: Eff
	real :: te,d,Effy !den,
	real,dimension(3) :: v,don
	real,dimension(num_config) :: Eff_T
	real,dimension(size(current_model%model%param(2)%param_basis),0:num_actin) :: prob
! Begin 


! get the goodies

numexp=current_data%len

don=param_list(current_model%model%param(1)%param_basis)%val
alpha=param_list(current_model%model%param(2)%param_basis)%val
R_0=param_list(current_model%model%param(3)%param_basis)%val
acc_w(1,:)=param_list(current_model%model%param(4)%param_basis)%val
acc_w(2,:)=param_list(current_model%model%param(5)%param_basis)%val
acc_w(3,:)=param_list(current_model%model%param(6)%param_basis)%val

! acc_w (1,:) stores the x coords of all expts, acc_w(2,:) stores the y coords of all expts, ...


! set the probabilities

	do j=0,num_actin
		prob(:,j)=alpha(:)**j * (1-alpha(:))**(num_actin-j)
	enddo

! calculate the acceptor positions
! first get the body coordinates
! note that the world coords correspond to actin_0

	do i=1,numexp
		acc_b(:,i)=w_to_b(acc_w(:,i),0)
	enddo !i

! construct the matrix of acceptor positions.
! acc(1,2,3), where 1: {x,y,z}
! 2: experiment #
! 3: actin position {-2,-1,0,1,2}

	don=don-offset
!	don(2)=don(2)-offset
	do i=1,numexp
		do j=-acc_lim,acc_lim
			acc(:,i,j)=w_to_b(b_to_w(acc_b(:,i),j),0)
!		write(2,*)acc(:,i)
		enddo !j
	enddo !i

! calc the transfer efficiencies

	do i=1,numexp
!		den=1.0


! calc the Eff_i for a given experiment

! lines added to figure out f_0
		Eff(:)=0.
!		do k=-acc_lim,acc_lim
		do k=0,0
			v=don-acc(:,i,k)
			d=sqrt(dot_product(v,v))
			te=(R_0(i)/d)**6
			Eff(k)=te
!			den=den+te
	!		if (k==0) d_sav=d
		enddo !k

		E_t(i)=0.

		do j=0,num_config-1
			num_item=0
			Effy=0.
			do k=0,num_actin-1
				if (btest(j,k)) then 
					Effy=Effy+Eff(k-acc_lim)
					num_item=num_item+1
				endif
			enddo !k
			E_t(i)=E_t(i)+prob(i,num_item)*(Effy/(1.+Effy))		
		enddo !j


	enddo !i

return

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


end function get_distance
