function diffuse(current_model,current_data) result(fluor)
! function passes the calculated fluorescence decay.  Fluor must be post-processed acording
! to frequency or time-domain by the calling function expt_fit.
	use h_params ! default%r_min,r_max
	use h_struct
	use h_vars
	use h_routines
	use h_utils, only : gaus_dist
	use h_dist_struct ! gp
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

! Local Parameters
	integer,parameter :: irule=2
	real,parameter :: errabs=0.0,errel=0.01

! Local Variables
	real,dimension(size(current_model%model%param(1)%param_basis)) :: tau,alphad
	real :: R0,mu,sigma,tcal,D	
	real,allocatable,dimension(:,:) :: Nbar,Nstar
	integer :: i,j,N,m
	integer :: numchan,num_life
! N = number of lattice points to evaluate psi
! numChan = number of time points to produce intensities for
	real,dimension(default%pts_per_dist) :: b,psi,chi,psiStar,Nnot,rsix
	real,dimension(default%pts_per_dist - 1) :: a,c
! psi is N_bar
	real :: r,k,dt,dr,alpha,beta,gamma,t,rmin,rmax ! tau_sum
	logical :: savePsi = .true.!.false. 
	real :: norm,erest
	real,dimension(current_data%len) :: fluorD,fluorDA
	real :: labRatio
! Begin
	num_life=size(current_model%model%param(1)%param_basis)
	numchan=current_data%len
	alphad=param_list(current_model%model%param(1)%param_basis)%val
	tau=param_list(current_model%model%param(2)%param_basis)%val
	R0=param_list(current_model%model%param(3)%param_basis(1))%val
	mu=param_list(current_model%model%param(3)%param_basis(2))%val ! mu
	sigma=param_list(current_model%model%param(3)%param_basis(3))%val ! sigma
	D=param_list(current_model%model%param(3)%param_basis(4))%val
	labRatio=param_list(current_model%model%param(4)%param_basis(1))%val
	tcal=current_data%tcal

	gp%R0=R0
	gp%num_life=num_life
	gp%alpha(1:num_life)=alpha
	gp%tau(1:num_life)=tau
	gp%p1=mu
	gp%p2=sigma

! scale the alpha's so that the sum of alpha's = 1
!call normalize_alpha(alpha,norm)
!param_list(current_model%model_param)%val=param_list(current_model%model_param)%val*norm
!param_list(current_model%model%param(1)%param_basis)%val=alpha


	N=default%pts_per_dist

!distribution limits

!tau_sum=sum(alpha/tau)
	rmin=default%r_min*R0
	rmax=default%r_max*R0

!	rmin=mu-default%numdev*sigma
!	rmax=mu+default%numdev*sigma

!	Nnot=dist(N,rmin,dr,params)

	dr=(rmax-rmin)/(N-1)

	do i=1,N
		r=rmin+(i-1)*dr
		Nnot(i)=gaus_dist(r)
! note that given definitions of rmin and rmax, r is always >=0.
!		if (r > 0.) then
			rsix(i)=(R0/r)**6
!		else
!			rsix(i)=1.0E+9
!		endif
	enddo !i


!	call calc_gaussian(R0,mu,sigma,N,Nnot,rsix)

! coef1 is a constant and is the coef of the H matrix
! coef2 is a function of r and is the coef of the J matrix
! k the transfer rate is a function of r
! none of A or B is an explicit function of time so we need only calculate
! A and B once.


! lets naively set dt = tcal; it may have to be less but we limit it to
! n*dt=tcal where n is a positive integer
	dt = tcal

! ONLY need to set up the matrices once because they do not depend on time

! main do over all the lifetime compartments
	fluorDA(:)=0
	if (savePsi) then
		allocate(Nbar(N,numchan))
		allocate(Nstar(N,numchan))
		Nbar(:,:)=0.
		Nstar(:,:)=0.
	endif


! **** finished setup

do m=1,num_life
	psi(:)=1.
! assume reflecting boundary conditions so...
! near wall
	k=(1+rsix(1))/tau(m)
	alpha=0.5*(1.+k*dt/2.)
	b(1)=alpha ! + 1. ?
	c(1)=0.
! far wall
	k=(1+rsix(N))/tau(m)
	alpha=0.5*(1.+k*dt/2.)
	a(N-1)=0.
	b(N)=alpha
!in between
	gamma=-D*dt/(4.*dr**2)
	do i=2,N-1
		r=rmin+(i-1)*dr 
		k=(1+rsix(i))/tau(m)
		alpha=0.5*(1.+k*dt/2.)
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
	call tri_ge(a,b,c,N,chi,psi) ! solve Q*chi=psi for chi
	psi=chi-psi
! so now we have a new psi for time point j*dt
	psiStar=psi*Nnot
	if (savePsi) then
		Nbar(:,j)=Nbar(:,j)+alphad(m)*psi(:) ! Nbar is psi(x,t) where psi is the normalized donor distr.
		Nstar(:,j)=Nstar(:,j)+alphad(m)*psiStar(:)
	endif
! calculate the space integral of N* which gives fluorescence intensity at time index j

! we could save all the integrals of psi with respect to r in a matrix
! with indices j x m.  Then do a matmult with alpha at the end.  Instead do
! the matrix multiplications as the integrals come out and save us from
! having to allocate a matrix integral_psi(j_max:m_max)
!select case(current_model%flag)
!	case (0) ! calc donor decay
		fluorDA(j)=fluorDA(j)+alphad(m)*trapezoidal_integrate(psiStar,N,dr)! /tau(m) ! integrate over r
!	case (1) ! calc acceptor sensitized excitation
!		fluor(j)=fluor(j)+alphad(m)*trapezoidal_integrate(psiStar*rsix,N,dr)! /tau(m)
!end select

end do !j

end do !m
! just seeing what the rise would look like it the acceptor tau was infinity
!	xtemp(1)=x(1,2)
!	do j=2,numchan
!		xtemp(j)=xtemp(j-1)+x(j,2)
!	end do

	do i=1,numchan
		t=tcal*i
		fluorD(i)= dot_product(alphad,exp(-t/tau))
	enddo ! i

	call qdag(gaus_dist,rmin,rmax,errabs,errel,irule,norm,erest)

	fluor=labRatio*fluorDA/norm+(1.- labRatio)*fluorD ! index over all times



! pause
	if (savePsi) then
		deallocate(Nbar,Nstar)
	endif
return ! with fluor and Nbar and Nstar
end function diffuse
