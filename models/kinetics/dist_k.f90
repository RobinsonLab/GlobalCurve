function dist_k(current_model,current_data,decay_dist) result(fluor)
	use h_params ! default%ET_fac
	use h_struct
!	use nr, only: qromb
!	use h_routines, only : trapezoidal_integrate
	use numerical_libraries, only : qdag
	use h_vars
	use h_dist_struct
!	use h_utils

	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	interface
		function decay_dist(r) ! should be gaus_decay, gamma_decay or similar routine
			real,intent(in) :: r
		end function decay_dist
	end interface
	real,dimension(current_data%len) :: fluor 

! intent(in)
!	h_vars
	!	type(param_struct),dimension(:),intent(in) :: param_list

! intent(out)
!	h_dist_struct
	!	gp%t,R0,num_life,alpha,tau,p1,p2


! Local Parameters
	integer,parameter :: irule=2
	real,parameter :: errabs=0.0,errel=0.01
! Local Var's
!	real,dimension(current_data%len) :: fluorD,fluorDA
	real,dimension(size(current_model%model%param(1)%param_basis)) :: alpha
!	real,dimension(size(current_model%model%param(4)%param_basis)) :: params
	real :: R0,p1,p2,tcal,dt
	integer :: i,j
	real :: rmin,rmax,t,labRatio
	integer :: numchan,num_life

	real :: res,erest

! Begin

! num_life=size(current_model%model%param(1)%param_basis)
	numchan=current_data%len
	alpha=param_list(current_model%model%param(1)%param_basis)%val
!	alpha=alpha/sum(alpha)
!	tau=param_list(current_model%model%param(2)%param_basis)%val
!	R0=param_list(current_model%model%param(3)%param_basis(1))%val
	p1=param_list(current_model%model%param(2)%param_basis(1))%val ! ~ mean
	p2=param_list(current_model%model%param(2)%param_basis(2))%val ! ~ std
	dt=param_list(current_model%model%param(2)%param_basis(3))%val ! ~ std	
!	labRatio=param_list(current_model%model%param(5)%param_basis(1))%val
!	tcal=current_data%tcal
	num_life=size(alpha)

! set up the parameters in gp

!	gp%R0=R0
	gp%num_life=num_life
	gp%alpha(1:num_life)=alpha
!	gp%tau(1:num_life)=tau
	gp%p1=p1
	gp%p2=p2

!	rmin=mu-default%numdev*sigma
!	rmax=mu+default%numdev*sigma


!	rmin=p1/default%ET_fac
!	rmax=p1*default%ET_fac
	rmin=p1-1.5*p2
	rmax=p1+1.5*p2

	if (rmin<0) rmin=1.0
!	rmax=p1+3*p2


	do i=1,numchan
		t=current_data%x_pts(i)
		gp%t=t+dt
		call qdag(decay_dist,rmin,rmax,errabs,errel,irule,res,erest)
		fluor(i)=res
!		fluorD(i)= dot_product(alpha,exp(-t/tau))
	enddo ! i

!	fluor=labRatio*fluorDA+(1.- labRatio)*fluorD ! index over all times

	return
end function

