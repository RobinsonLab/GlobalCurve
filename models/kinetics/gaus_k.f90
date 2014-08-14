function gaus_k(k)
	use h_params ! PI
	use h_dist_struct
	use h_utils, only: gaus_dist
	implicit none
	real,intent(in) :: k
	real :: gaus_k

! r is of dimension 1.

! intent(in)
!	gp%t,R0,num_life,alpha,tau

!	Local Params
	real :: decay,gaus
	integer :: j	

!	Begin

	decay=0.
	do j=1,gp%num_life
		decay=decay+gp%alpha(j)*exp(-gp%t*k)
	enddo ! j
	gaus=gaus_dist(k)
	gaus_k=gaus*decay
end function gaus_k






