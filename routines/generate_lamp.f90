subroutine generate_lamp(tcal,fwhm,lim,lamp)
! use monte-carlo simulation to generate a gaussian shaped lamp function with full width
! at half maximum = fwhm
	use h_params
	use numerical_libraries
	implicit none
	real,intent(in) :: tcal
	real,intent(in) :: fwhm
	integer,intent(in) :: lim
	real,dimension(CHAN_MAX),intent(out) :: lamp

!Local Var's	
	integer,parameter :: max_cts=100000
	real,dimension(max_cts) :: gdrv
	real :: sigma
	real :: mean
	integer :: i,channel

!Begin
	lamp(:)=0.
	sigma = fwhm/2.354
	mean = 5*sigma
	!calculations are done in time then time is discretized into channels
	!generate an event of a Gaussian distributed random variable using the Box-Muller method
	!The midpoint of the gaussian is to the right by 4 sigma.

	! for now just call the isml routine.
	call rnnoa (max_cts,gdrv)
	! this gives standard normal deviates: z=(t-mean)/sigma
	! => t = z*sigma + mean
	gdrv=gdrv*sigma + mean
	! now convert to counts. We have tcal which is time/chanel.  So chanel=time/tcal. Chanel
	! as defined here will be real.  We need to round up (= ceiling) to make channel an integer
	do i=1,max_cts
		channel=ceiling(gdrv(i)/tcal) + lim +5 !lim is an offset + 5 for good measure
		if ( (channel >= lim) .and. (channel <= CHAN_MAX) ) lamp(channel)=lamp(channel)+1
	enddo !i
	return
end subroutine generate_lamp

