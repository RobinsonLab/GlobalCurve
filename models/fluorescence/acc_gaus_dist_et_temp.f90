function acc_gaus_dist_et(current_model,param_list,p) result(fluor)
	use h_utils
	use h_models
	use h_struct
	use h_routines, only : trapezoidal_integrate
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
real :: R0,mu,sigma,tcal
real,dimension(default%pts_per_dist,size(tau)) :: k,k2,Nbar
real,dimension(p%len,default%pts_per_dist) :: storeNbars,storeNstar
real,dimension(default%pts_per_dist) :: Nnot,rsix,Nstar,Nbars
integer :: i,j,N
real :: dr,rmin,rmax,t
integer :: numchan !,num_life
real :: norm
integer :: num_life

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
	mu=param_list(current_model%model%param(3)%param_basis(2))%val
	sigma=param_list(current_model%model%param(3)%param_basis(3))%val
	tcal=p%tcal

! scale the alpha's so that the sum of alpha's = 1
!call normalize_alpha(alpha,norm)
!param_list(current_model%model_param)%val=param_list(current_model%model_param)%val*norm
!param_list(current_model%model%param(1)%param_basis)%val=alpha
	
	N=default%pts_per_dist
	call calc_gaussian(R0,mu,sigma,N,Nnot,rsix)
	rmin=mu-default%numdev*sigma
	rmax=mu+default%numdev*sigma
	dr=(rmax-rmin)/(N-1)
	num_life=size(tau)

	forall (j=1:num_life)
		k(:,j)=(1.+rsix(:))/tau(j)
		k2(:,j)=1./tau(j)
	end forall !j

! calculate the exponential across time
	do i=1,numchan ! i is the time counter
		t=tcal*i
		Nbar(:,:)=exp((-k2(:,:)*t))-exp((-k(:,:)*t))
		Nbars(:)= matmul(Nbar(:,:),alpha(:))! /tau(:)) ! sum of exponentials weighted by alpha
		Nstar(:)=Nnot(:)*Nbars(:)
		storeNbars(i,:)=Nbars(:) ! these show you the distribution evolution in time
		storeNstar(i,:)=Nstar(:)
!		fluor(i)=trapezoidal_integrate(Nnot,N,dr) ! for testing total area under the gaussian
 		fluor1(i)=trapezoidal_integrate(Nstar,N,dr)
	enddo !i


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

!	fluor=simple_convolute(fluor1,fluor2) 	
	deallocate(temp_model%model%param,temp_model)

end function acc_gaus_dist_et
