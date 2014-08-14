subroutine expt_type_parser(current_expt)
	use h_struct
	use h_utils, only: error_handler
	implicit none

	type(expt_struct), pointer :: current_expt

	! intent(out)
	!	current_expt%class
	!	current_expt%data_type

	! the reason %class and %data_type have to be defined is for use in case statements


! Local Var's

! Begin
	if (current_expt%expt_type == 'cns') then ! constraint - introduce a constraint
												! on params
		current_expt%class = 0 ! no convolution
		current_expt%data_type = 4
	elseif (current_expt%expt_type == 'gtd') then ! globals time domain
		current_expt%class = 1
		current_expt%data_type = 1
	elseif (current_expt%expt_type == 'kin') then ! kinetics
		current_expt%class = 0
		current_expt%data_type = 2
	elseif (current_expt%expt_type == 'bnd') then ! ligand binding
		current_expt%class = 0
		current_expt%data_type = 3
	elseif (current_expt%expt_type == 'idk') then ! input driven kinetics
		current_expt%class = 0
		current_expt%data_type = 5



!	elseif (current_expt%expt_type == 'vir') then	! virtual data type - introduce 
!													! a constraint on params
!		current_expt%class = 0
!		current_expt%data_type = 4
	else
		call error_handler('Unrecognized experiment type')
	endif

end subroutine
