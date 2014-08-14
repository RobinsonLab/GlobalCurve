function acc_diffuse(current_model,param_list,p) result(fluor)
	use h_struct
	use h_routines
	implicit none	
	type(model_struct), pointer :: current_model
	type(param_struct),dimension(:),intent(in) :: param_list
	type (data_struct),pointer :: p	
	real,dimension(p%len) :: fluor 

! Local Parameters
	real,dimension(p%len) :: fluor1,fluor2
	type(model_struct), pointer :: temp_model
	integer :: nx
	
	real,dimension(size(current_model%model%param(1)%param_basis)) :: tau,alphad
	real :: R0,mu,sigma,tcal,D	
	real,allocatable,dimension(:,:) :: Nbar,Nstar
	integer :: i,j,N,m
	integer :: numchan,num_life
! N = number of lattice points to evaluate psi
! numChan = number of time points to produce intensities for
	real,dimension(default%pts_per_dist) :: b,psi,chi,psiStar,psiStar2,Nnot,sNnot,rsix
	real,dimension(default%pts_per_dist - 1) :: a,c
! psi is N_bar
	real,dimension(default%pts_per_dist,size(tau)) :: k,k2
	real :: r,dt,dr,alpha,beta,gamma,rmin,rmax ! tau_sum
	logical :: savePsi = .false.
	real :: norm
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
! Begin
num_life=size(current_model%model%param(1)%param_basis)
numchan=p%len
alphad=param_list(current_model%model%param(1)%param_basis)%val
tau=param_list(current_model%model%param(2)%param_basis)%val
R0=param_list(current_model%model%param(3)%param_basis(1))%val
mu=param_list(current_model%model%param(3)%param_basis(2))%val
sigma=param_list(current_model%model%param(3)%param_basis(3))%val
D=param_list(current_model%model%param(3)%param_basis(4))%val
tcal=p%tcal

! scale the alpha's so that the sum of alpha's = 1
!call normalize_alpha(alpha,norm)
!param_list(current_model%model_param)%val=param_list(current_model%model_param)%val*norm
!param_list(current_model%model%param(1)%param_basis)%val=alpha


	N=default%pts_per_dist

!distribution limits

!tau_sum=sum(alpha/tau)
rmin=mu-default%numdev*sigma
rmax=mu+default%numdev*sigma

	call calc_gaussian(R0,mu,sigma,N,Nnot,rsix)

! coef1 is a constant and is the coef of the H matrix
! coef2 is a function of r and is the coef of the J matrix
! k the transfer rate is a function of r
! none of A or B is an explicit function of time so we need only calculate
! A and B once.

	dr=(rmax-rmin)/(N-1)


! lets naively set dt = tcal; it may have to be less but we limit it to
! n*dt=tcal where n is a positive integer
	dt = tcal

! ONLY need to set up the matrices once because they do not depend on time

! main do over all the lifetime compartments
	fluor1(:)=0
	if (savePsi) then
		allocate(Nbar(N,numchan))
		allocate(Nstar(N,numchan))
		Nbar(:,:)=0.
		Nstar(:,:)=0.
	endif

	forall (j=1:num_life)
		k(:,j)=(1.+rsix(:))/tau(j)
		k2(:,j)=rsix(:)/tau(j)
	end forall !j



! **** finished setup

do m=1,num_life
	psi(:)=1.
	sNnot(:)=Nnot(:)*alphad(m)
	psiStar(:)=sNnot(:)

! assume reflecting boundary conditions so...
! near wall
	alpha=0.5*(1.+k(1,m)*dt/2.)
	b(1)=alpha ! + 1. ?
	c(1)=0.
! far wall
	alpha=0.5*(1.+k(N,m)*dt/2.)
	a(N-1)=0.
	b(N)=alpha
!in between
	gamma=-D*dt/(4.*dr**2)
	do i=2,N-1
		r=rmin+(i-1)*dr 
		alpha=0.5*(1.+k(i,m)*dt/2.)
		beta=D*dt*(r-mu)/(8.*dr*sigma**2)
		a(i-1)=beta+gamma
		b(i)=alpha-2.*gamma
		c(i)=-beta+gamma
	end do

! j=0
! calculate the space integral of N* which gives fluorescence intensity at time index j



! have to be careful here because x(1) is 1 time step after diffusion has
! occured so we have an offset of tcal to consider.
! Not sure if we need to include the time zero.  Probably not because at 
! t=0, no fluorescence has occured.

! the inputs to calculate how psi changes in time are good for a given
! value of m.

do j=1,numchan ! numchan is the number of time steps
! the number of photons that get transferred to the acceptor is just an exponential
! involving the ET rate times the current time Nstar (i.e. before diffusion and sink term
! items have acted on the current Nstar.
! Now the algorithm approximates the exponential as follows: e^(-kt) = e^(-kt/2)*e^(-kt/2)
! = e^(-kt/2) / e^(kt/2) ~= (1-kt/2)/(1+kt/2) which is good to about 10^-6.
	psiStar2(:)=psiStar(:)*exp(-k2(:,m)*dt)
	call tri_ge(a,b,c,N,chi,psi) ! solve Q*chi=psi for chi
	psi=chi-psi
! so now we have a new psi for time point j*dt
	psiStar=psi*sNnot
	if (savePsi) then
		Nbar(:,j)=Nbar(:,j)+alphad(m)*psi(:) ! Nbar is psi(x,t) where psi is the normalized donor distr.
		Nstar(:,j)=Nstar(:,j)+psiStar(:)
	endif
! calculate the space integral of N* which gives fluorescence intensity at time index j

! we could save all the integrals of psi with respect to r in a matrix
! with indices j x m.  Then do a matmult with alpha at the end.  Instead do
! the matrix multiplications as the integrals come out and save us from
! having to allocate a matrix integral_psi(j_max:m_max)
	fluor1(j)=fluor1(j)+trapezoidal_integrate(psiStar2,N,dr) ! integrate over r

end do !j

end do !m
! just seeing what the rise would look like it the acceptor tau was infinity
!	xtemp(1)=x(1,2)
!	do j=2,numchan
!		xtemp(j)=xtemp(j-1)+x(j,2)
!	end do

! pause
	if (savePsi) then
		deallocate(Nbar,Nstar)
	endif

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
return ! with fluor and Nbar and Nstar
end function acc_diffuse
