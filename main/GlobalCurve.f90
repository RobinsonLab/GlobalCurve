program GlobalCurve
	use h_routines
	implicit none
! Local Var's
	integer :: ios

! Begin
	ios=main()
	if(.not. ios) then
		write (*,*) 'Program terminated normally'
	else
		write (*,*) 'Program terminated with error: ',ios
	endif
	! garbage collection
	call destructor_logic()
	call destructor_general()
	call destructor_particular()
end program GlobalCurve



