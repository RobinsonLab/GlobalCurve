function gaus_decay(r)
	use h_params ! PI
	use h_dist_struct
	use h_utils, only: gaus_dist
	implicit none
	real,intent(in) :: r
	real :: gaus_decay

! r is of dimension 1.

! intent(in)
!	gp%t,R0,num_life,alpha,tau

!	Local Params
	real :: decay,gaus
	integer :: j	

!	Begin

	decay=0.
	do j=1,gp%num_life
		decay=decay+gp%alpha(j)*exp(-gp%t/gp%tau(j)*(1.+(gp%R0/r)**6))
	enddo ! j
	gaus=gaus_dist(r)
	gaus_decay=gaus*decay
end function gaus_decay






