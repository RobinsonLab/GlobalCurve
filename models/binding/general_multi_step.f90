function general_multi_step(current_model,current_data) result(bind)
!	this implements the general two step reaction where
!	A <-> B <-> C, ... with rate constants k_1,k_-1,k_2,k_-2,... = p1,m1,p2,m2,...
!	one must identify which species are contributing to the observed fluorescence change
!		through a supplied vector.


!	regardless, the time evolution of all species A,B,C are calculated.
	use h_struct
	use numerical_libraries
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bind

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

! Local Parameters
	real :: K0,kp1,km1
	real,dimension(current_data%len) :: Ca
	real,dimension(size(current_model%model%param(1)%param_basis)) :: p,m

! Hill eqn.

! Begin
! numchan=expt_data%len

	p(:)=param_list(current_model%model%param(1)%param_basis(:))%val
	m(:)=param_list(current_model%model%param(2)%param_basis(:))%val
	alpha=param_list(current_model%model%param(3)%param_basis(:))%val
	num_step_rxn=size(p)
	num_species=num_step_rxn+1



! the matrix that we are going to set up is singular and tridiagonal.
! store it in banded form.


! solve for the eigenvectors and eigenvalues


! solve the initial value problem y=Ax
	x(:)=0.
	x(1)=1.
	
	CALL LSARG (num_species, A, num_species, c, 1, x) ! solve system by iterative refinement

! construct the solution

		




kp1=param_list(current_model%model%param(1)%param_basis(3))%val



Ca=current_data%x_pts
lambda_f=(K0*Ca/(1.+K0*Ca))*kp1+km1


return
end function general_multi_step
