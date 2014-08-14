function get_ca(t)
	use h_routine_specific_vars
	use numerical_libraries, only: csval
	implicit none
	real,intent(in) :: t
	real :: get_ca

! Local Params
	real,parameter :: CA_MIN=1.E-8	
! Begin

	get_ca=CSVAL(t,tsv%NINTV,tsv%BREAK,tsv%CSCOEF)
	if (get_Ca < CA_MIN) get_ca=CA_MIN
end function get_ca
