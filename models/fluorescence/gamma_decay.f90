function gamma_decay(r)
	use h_params ! PI
	use h_dist_struct
	use h_utils, only: gamma_dist
	implicit none
	real,intent(in) :: r
	real :: gamma_decay

! intent(in)
!	gp%t,R0,num_life,alpha,tau

!	Local Params
	real :: decay,gamma
	integer :: j	

!	Begin

	decay=0.
	do j=1,gp%num_life
		decay=decay+gp%alpha(j)*exp(-gp%t/gp%tau(j)*(1.+(gp%R0/r)**6))
	enddo ! j
	gamma=gamma_dist(r)
	gamma_decay=gamma*decay
end function gamma_decay



