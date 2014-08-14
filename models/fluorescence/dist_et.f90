function dist_et(current_model,current_data,decay_dist,dist) result(fluor)
	use h_params ! default%r_min,r_max
	use h_struct
!	use nr, only: qromb
!	use h_routines, only : trapezoidal_integrate
	use numerical_libraries
	use h_vars
	use h_dist_struct ! gp
	use h_utils

	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	interface
		function decay_dist(t) ! should be gaus_decay, gamma_decay or similar routine
			implicit none	
			real,intent(in) :: t
			real :: decay_dist
		end function decay_dist
		function dist(r)
			implicit none	
			real,intent(in) :: r
			real :: dist
		end function dist
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
	real,dimension(current_data%len) :: fluorD,fluorDA
	real,dimension(size(current_model%model%param(1)%param_basis)) :: tau,alpha
	real,dimension(size(current_model%model%param(4)%param_basis)) :: params
	real :: R0,p1,p2,tcal
	integer :: i,j
	real :: t,labRatio,rmin,rmax
	integer :: numchan,num_life

	real :: res,erest
	real :: norm
! Begin

! num_life=size(current_model%model%param(1)%param_basis)
	numchan=current_data%len
	alpha=param_list(current_model%model%param(1)%param_basis)%val
	alpha=alpha/sum(alpha)
	tau=param_list(current_model%model%param(2)%param_basis)%val
	R0=param_list(current_model%model%param(3)%param_basis(1))%val
	p1=param_list(current_model%model%param(4)%param_basis(1))%val
	p2=param_list(current_model%model%param(4)%param_basis(2))%val
	labRatio=param_list(current_model%model%param(5)%param_basis(1))%val
	tcal=current_data%tcal
	num_life=size(tau)

! set up the parameters in gp

	gp%R0=R0
	gp%num_life=num_life
	gp%alpha(1:num_life)=alpha
	gp%tau(1:num_life)=tau
	gp%p1=p1
	gp%p2=p2

!	rmin=mu-default%numdev*sigma
!	rmax=mu+default%numdev*sigma
	rmin=default%r_min*R0
	rmax=default%r_max*R0

!	rmin=R0/default%ET_fac
!	rmax=R0*default%ET_fac
!	rmax=p1+3*p2

! need to be sure the probability distribution is normalized
! if there is probability outside of rmin..rmax than norm < 1.0
	call qdag(dist,rmin,rmax,errabs,errel,irule,norm,erest)

	do i=1,numchan
		t=tcal*i
		gp%t=t
		call qdag(decay_dist,rmin,rmax,errabs,errel,irule,res,erest)
		fluorDA(i)=res
		fluorD(i)= dot_product(alpha,exp(-t/tau))
	enddo ! i

	fluor=labRatio*fluorDA/norm+(1.- labRatio)*fluorD ! index over all times

	return
end function

