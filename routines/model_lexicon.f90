subroutine model_lexicon(model_name,model_id)
	use h_params
	use h_models
	use h_utils
	use h_routine_specific_vars
	implicit none
	character(STRING_LEN),intent(in) :: model_name
	integer(ADDR_LEN),intent(out) :: model_id
! Begin

! fluorescence models
! total decay
	if (index(model_name,'lifetime') > 0) then
 		model_id=loc(lifetime)
 	else if (index(model_name,'discrete ET') > 0) then
 		model_id=loc(disc_et)
	else if (index(model_name,'gaus dist ET') > 0) then
		model_id=loc(gaus_dist_et)
	else if (index(model_name,'gamma dist ET') > 0) then
		model_id=loc(gamma_dist_et)
!	else if (index(model_name,'acc gaus dist ET') > 0) then
!		model_id=loc(acc_gaus_dist_et)
!	else if (index(model_name,'acc gamma dist ET') > 0) then
!		model_id=loc(acc_gamma_dist_et)
!	else if (index(model_name,'acc diffuse') > 0) then
!		model_id=loc(acc_diffuse)
	else if (index(model_name,'gaus diffuse') > 0) then
		model_id=loc(diffuse)
! anisotropy decay
	else if (index(model_name,'anisotropy') > 0) then
		model_id=loc(lifetime) ! anisotropy and lifetime decay look the same
	else if (index(model_name,'vert_polarized') > 0) then
		model_id=loc(vert_polarized)
	else if (index(model_name,'horiz_polarized') > 0) then
		model_id=loc(horiz_polarized)

! binding models	
	else if (index(model_name,'hill') > 0) then
		model_id=loc(hill)
	else if (index(model_name,'simulate_ca_gr_titr') > 0) then
		model_id=loc(simulate_ca_gr_titr)
	else if (index(model_name,'frac_sat') > 0) then
		model_id=loc(frac_sat)
	else if (index(model_name,'three_state_simple_model') > 0) then
		model_id=loc(three_state_simple_model)

! distance models
	else if (index(model_name,'get_distance') > 0) then
		model_id=loc(get_distance)
	

! kinetics models
	else if (index(model_name,'gaus_dist_k') > 0) then
		model_id=loc(gaus_dist_k)

! constraint models
	else if (index(model_name,'simple_constrain') > 0) then
		model_id=loc(simple_constrain)
	else if (index(model_name,'Ka_constrain') > 0) then
		model_id=loc(Ka_constrain)
	else if (index(model_name,'frac') > 0) then
		model_id=loc(frac)
! physiology models

!	else if (index(model_name,'four_step_A_M_force') > 0) then
!		model_id=loc(four_step_A_M_force)
!		if (.not. associated(fsv)) allocate(fsv)
!	else if (index(model_name,'four_step_A_M_norm_force') > 0) then
!		model_id=loc(four_step_A_M_norm_force)
!		if (.not. associated(fsv)) allocate(fsv)
!	else if (index(model_name,'four_step_A_M_ATPase') > 0) then
!		model_id=loc(four_step_A_M_ATPase)
!		if (.not. associated(fsv)) allocate(fsv)
!	else if (index(model_name,'four_step_A_M_norm_ATPase') > 0) then
!		model_id=loc(four_step_A_M_norm_ATPase)
!		if (.not. associated(fsv)) allocate(fsv)


! steady state force

	else if (index(model_name,'three_state_SS_force_NEM_S1') > 0) then
		model_id=loc(three_state_SS_force_NEM_S1)
		if (.not. associated(tsv)) allocate(tsv)
	else if (index(model_name,'three_state_SS_force_TnC_extr') > 0) then
		model_id=loc(three_state_SS_force_TnC_extr)
		if (.not. associated(tsv)) allocate(tsv)
	else if (index(model_name,'three_state_SS_ATPase_TnC_extr') > 0) then
		model_id=loc(three_state_SS_ATPase_TnC_extr)
		if (.not. associated(tsv)) allocate(tsv)	
	else if (index(model_name,'three_state_SS_force_S1_extr') > 0) then
		model_id=loc(three_state_SS_force_S1_extr)
		if (.not. associated(tsv)) allocate(tsv)
	else if (index(model_name,'three_state_SS_ATPase_S1_extr') > 0) then
		model_id=loc(three_state_SS_ATPase_S1_extr)
		if (.not. associated(tsv)) allocate(tsv)	

	else if (index(model_name,'three_state_SS_force_RU_g') > 0) then
		model_id=loc(three_state_SS_force_RU_g)
		if (.not. associated(tsv)) allocate(tsv)
	else if (index(model_name,'three_state_SS_ATPase_RU_g') > 0) then
		model_id=loc(three_state_SS_ATPase_RU_g)
		if (.not. associated(tsv)) allocate(tsv)


	else if (index(model_name,'three_state_SS_force') > 0) then
		model_id=loc(three_state_SS_force)
		if (.not. associated(tsv)) allocate(tsv)
	else if (index(model_name,'three_state_SS_ATPase') > 0) then
		model_id=loc(three_state_SS_ATPase)
		if (.not. associated(tsv)) allocate(tsv)

! old way of calculating force

	else if (index(model_name,'three_state_A_M_force') > 0) then
		model_id=loc(three_state_A_M_force)
		if (.not. associated(tsv)) allocate(tsv)
	else if (index(model_name,'three_state_A_M_norm_force') > 0) then
		model_id=loc(three_state_A_M_norm_force)
		if (.not. associated(tsv)) allocate(tsv)
	else if (index(model_name,'three_state_A_M_ATPase') > 0) then
		model_id=loc(three_state_A_M_ATPase)
		if (.not. associated(tsv)) allocate(tsv)
	else if (index(model_name,'three_state_A_M_norm_ATPase') > 0) then
		model_id=loc(three_state_A_M_norm_ATPase)
		if (.not. associated(tsv)) allocate(tsv)
	else if (index(model_name,'three_state_A_M_bound') > 0) then
		model_id=loc(three_state_A_M_bound)
		if (.not. associated(tsv)) allocate(tsv)

	else if (index(model_name,'three_state_diazo_2_force') > 0) then
		model_id=loc(three_state_diazo_2_force)
		if (.not. associated(tsv)) allocate(tsv)

!	else if (index(model_name,'three_state_bapta_force') > 0) then
!		model_id=loc(three_state_bapta_force)
!		if (.not. associated(tsv)) allocate(tsv)

	else if (index(model_name,'three_state_ktr_force') > 0) then
		model_id=loc(three_state_ktr_force)
		if (.not. associated(tsv)) allocate(tsv)
	else if (index(model_name,'three_state_ktr_ATPase') > 0) then
		model_id=loc(three_state_ktr_ATPase)
		if (.not. associated(tsv)) allocate(tsv)
	else if (index(model_name,'three_state_ktr_bound') > 0) then
		model_id=loc(three_state_ktr_bound)
		if (.not. associated(tsv)) allocate(tsv)

	else if (index(model_name,'isometric_twitch_force') > 0) then
		model_id=loc(isometric_twitch_force)
		if (.not. associated(tsv)) allocate(tsv)

	else if (index(model_name,'isotonic_twitch_ATPase') > 0) then
		model_id=loc(isotonic_twitch_ATPase)
		if (.not. associated(tsv)) allocate(tsv)


! error
	else 
		call error_handler('Unrecognized model name')
	endif
return
end subroutine model_lexicon