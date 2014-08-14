function dist_et(current_model,current_data,dist) result(fluor)
	use h_struct
	use h_utils, only : calc_rsix
	use h_vars
	use h_routines, only : trapezoidal_integrate
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	interface
		function dist(N,rmin,dr,params)
			integer,intent(in) :: N
			real,intent(in) :: rmin,dr
			real,dimension(:) :: params
			real,dimension(N) :: dist
		end function dist
	end interface
	real,dimension(current_data%len) :: fluor 

! would be better to use compiler directives to determine if the save... vars should be
! declared and defined.

! can come back at a later time and have this function read from a vector 
! x_calc which specifies the x_values to calculate y_vals at.
! for now just let it count from 1 to numchan

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

! Local Parameters

	real,dimension(size(current_model%model%param(1)%param_basis)) :: tau,alpha
	real,dimension(size(current_model%model%param(4)%param_basis)) :: params
	real :: R0,mu,sigma,tcal
	real,dimension(default%pts_per_dist,size(tau)) :: k
	real,dimension(current_data%len,default%pts_per_dist) :: storeNbar,storeNstar
	real,dimension(default%pts_per_dist) :: Nnot,sNnot,rsix,Nstar,Nbar
	integer :: i,j,N
	real :: dr,rmin,rmax,t
	integer :: numchan !,num_life
	real :: norm
	integer :: num_life
!	integer,pointer,dimension(:) :: param_ptr

! Begin

! num_life=size(current_model%model%param(1)%param_basis)
	numchan=current_data%len
	alpha=param_list(current_model%model%param(1)%param_basis)%val
	tau=param_list(current_model%model%param(2)%param_basis)%val
	R0=param_list(current_model%model%param(3)%param_basis(1))%val
!	mu=param_list(current_model%model%param(3)%param_basis(2))%val
!	sigma=param_list(current_model%model%param(3)%param_basis(3))%val
	params=param_list(current_model%model%param(4)%param_basis(:))%val
	tcal=current_data%tcal
	num_life=size(tau)
	N=default%pts_per_dist
	Nnot=dist(N,rmin,dr,params) ! Nnot returns rmin and dr.
	rsix=calc_rsix(N,rmin,dr,R0)

	forall (j=1:num_life)
		k(:,j)=(1.+rsix(:))/tau(j) ! index is over N
	end forall !j


! redoing things here

! storeNbar and store Nstar are dimensioned:
!	current_data%len , pts_per_dist = (t,N)
! k has dimensions (N,num_life)
! Nbar and Nstar have dimensions (N)

! first assemble the Nbar then get N star then do the integrals on Nstar
! for each time point.



	do j=1,num_life
		do i=1,numchan
			t=tcal*i
			Nbar(:,i)=Nbar(:,i)+alpha(j)*exp((-k(:,j)*t)) ! index is over N
		enddo ! i
	enddo ! j
	Nstar= spread(Nnot,dim=2,ncopies=t)*Nbar  elementwise multiply (N,t)*(N,t)
! now integrate over N using trapezoidal rule
	forall (i=1:numchan)
		fluor(i)=dr*(0.5*(Nstar(1,i)+Nstar(N,i))+sum(Nstar(2:N-1,i)))
	end forall

	storeNbar(:,:) = 0.
	storeNstar(:,:) = 0.
	fluor(:)=0.	
	do j=1,num_life
		sNnot(:)=Nnot(:)*alpha(j) ! sNnot = alpha scaled Nnot
!		Nstar(:)=sNnot(:)
		do i=1,numchan ! i is the time counter
			t=tcal*i
			Nbar(:)=exp((-k(:,j)*t))
			Nstar(:)=sNnot(:)*Nbar(:)
!		fluor(i)=trapezoidal_integrate(Nnot,N,dr) ! for testing total area under the gaussian
			storeNbar(i,:)=storeNbar(i,:)+alpha(j)*Nbar(:) ! these show you the distribution evolution in time
			storeNstar(i,:)=storeNstar(i,:)+Nstar(:)
 			fluor(i) = fluor(i)+trapezoidal_integrate(Nstar,N,dr)
		enddo !i
	enddo !j

	return
end function dist_et
