function minimize() result(res)
! currently only marquadt-levenberg minimization is supported
	use h_params
	use h_routines
	implicit none

	integer :: res


! Begin
	select case(default%minimizaton_type)
	case (1)
		res=marquadt()
	case (2)
		res=cluster()
	end select

! if res > 0 then the return value indicates a pathologic variable
	return
end function