function acc_gamma_dist_et(current_model,param_list,p) result(fluor)
	use h_struct
	use h_routines
	use h_utils
	implicit none	
	type(model_struct), pointer :: current_model
	type(param_struct),dimension(:),intent(in) :: param_list
	type (data_struct),pointer :: p	
	real,dimension(p%len) :: fluor 

! Local Var's
real,dimension(p%len) :: fluor1,fluor2
type(model_struct), pointer :: temp_model
integer :: nx

real,dimension(size(current_model%model%param(1)%param_basis)) :: tau,alpha
real :: R0,x0,s,lambda,tcal,mu,sigma
real,dimension(default%pts_per_dist,size(tau)) :: k,k2
! real,dimension(p%len,default%pts_per_dist) :: storeNbars,storeNstar
real,dimension(default%pts_per_dist) :: Nnot,sNnot,rsix,Nstar,Nstar2,Nbar,Nbar2
integer :: i,j,N
real :: dx,xmin,xmax,t
integer :: numchan !,num_life
real :: norm
integer :: num_life
interface
	function lifetime(current_model,param_list,expt_data) result(fluor)
		use h_struct
		use h_utils
		implicit none	
		type(model_struct), pointer :: current_model
		type(param_struct),dimension(:),intent(in) :: param_list
		type (data_struct),pointer :: expt_data	
		real,dimension(expt_data%len) :: fluor 
	end function lifetime
end interface

! this model expects 5 arrays of params
! array #		description
!	1			donor alpha
!	2			donor tau
!	3			R0,mu
!	4			acceptor alpha
!	5			acceptor tau

! Begin
	numchan=p%len
	alpha=param_list(current_model%model%param(1)%param_basis)%val
	tau=param_list(current_model%model%param(2)%param_basis)%val
	R0=param_list(current_model%model%param(3)%param_basis(1))%val
	x0=param_list(current_model%model%param(3)%param_basis(2))%val
	s=param_list(current_model%model%param(3)%param_basis(3))%val
	lambda=param_list(current_model%model%param(3)%param_basis(4))%val
	tcal=p%tcal

! scale the alpha's so that the sum of alpha's = 1
!call normalize_alpha(alpha,norm)
!param_list(current_model%model_param)%val=param_list(current_model%model_param)%val*norm
!param_list(current_model%model%param(1)%param_basis)%val=alpha
	
	N=default%pts_per_dist
	mu=x0+s/lambda
	sigma=sqrt(s/lambda**2.)
	xmin=x0
	xmax=mu+default%numdev*1.3*sigma
	dx=(xmax-xmin)/(N-1)
	call calc_gamma(R0,x0,s,lambda,N,xmin,dx,Nnot,rsix)
!	call calc_rsix(RO,rsix)
	num_life=size(tau)

	forall (j=1:num_life)
		k(:,j)=(1.+rsix(:))/tau(j)
		k2(:,j)=rsix(:)/tau(j)
	end forall !j
	fluor1(:)=0.	
	do j=1,num_life
		sNnot(:)=Nnot(:)*alpha(j) ! sNnot = alpha scaled Nnot
		Nstar(:)=sNnot(:)
		Nbar2(:)=exp((-k2(:,j)*tcal))
		do i=1,numchan ! i is the time counter
			t=tcal*i
			Nbar(:)=exp((-k(:,j)*t))
			Nstar2(:)=Nstar(:)*Nbar2(:) ! Nstar is from the previous time pt.
			Nstar(:)=sNnot(:)*Nbar(:)
!		fluor(i)=trapezoidal_integrate(Nnot,N,dr) ! for testing total area under the gaussian
 			fluor1(i) = fluor1(i)+trapezoidal_integrate(Nstar2,N,dx)
		enddo !i
	enddo !j


	allocate(temp_model)
	allocate(temp_model%model)
	allocate(temp_model%model%param(2))
	temp_model%model%param(1)%param_basis=>current_model%model%param(4)%param_basis
	temp_model%model%param(2)%param_basis=>current_model%model%param(5)%param_basis
	fluor2=lifetime(temp_model,param_list,p)

! ido = 0 ! for now
! nx = ny = p%len
! x = fluor2
! y = fluor3
! ipad = 1 (nonperiodic data)
! nz = p%nz set in readans
! z = p%z
! zhat = p%zhat
! xwk = p%xwk
! ywk = p%ywk
! wk = p%wk


! borrow workspace from p to do the convolution
	nx=p%len
	call r2onv(0,nx,fluor1,nx,fluor2,1,p%nz,p%z,p%zhat, &
		p%xwk,p%ywk,p%wk)
	fluor(:)=p%z(1:nx)

!	fluor=simple_convolute(fluor1,fluor2) 	! use the fourier transform method above.  Results
											! are equivalent - I checked
	deallocate(temp_model%model%param,temp_model)

end function acc_gamma_dist_et
