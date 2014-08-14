# Microsoft Developer Studio Project File - Name="globalCurve" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) QuickWin Application" 0x0107

CFG=globalCurve - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "globalCurve.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "globalCurve.mak" CFG="globalCurve - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "globalCurve - Win32 Release" (based on "Win32 (x86) QuickWin Application")
!MESSAGE "globalCurve - Win32 Debug" (based on "Win32 (x86) QuickWin Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "globalCurve - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /libs:qwin /nologo /warn:nofileopt
# ADD F90 /compile_only /libs:qwin /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /machine:I386 /nodefaultlib:"dfconsol.lib"
# ADD LINK32 kernel32.lib pgplot.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /machine:I386 /nodefaultlib:"dfconsol.lib" /out:"c:\code\bin\globalCurve.exe" /libpath:"c:\code\lib"

!ELSEIF  "$(CFG)" == "globalCurve - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /libs:qwin /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /libs:qwin /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /debug /machine:I386 /nodefaultlib:"dfconsol.lib" /pdbtype:sept
# ADD LINK32 kernel32.lib c:/local/lib/pgplot.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /incremental:no /debug /machine:I386 /nodefaultlib:"dfconsol.lib" /pdbtype:sept /libpath:"c:\code\lib"

!ENDIF 

# Begin Target

# Name "globalCurve - Win32 Release"
# Name "globalCurve - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Group "main"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\main\GlobalCurve.f90
DEP_F90_GLOBA=\
	".\Debug\h_routines.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\main\main.f90
DEP_F90_MAIN_=\
	".\Debug\h_params.mod"\
	".\Debug\h_rigorous.mod"\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# End Group
# Begin Group "utilities"

# PROP Default_Filter ""
# Begin Group "distributions_et_utilities"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\utilities\calc_gamma_mean_hw.f90
DEP_F90_CALC_=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\calc_gaus_mean_hw.f90
DEP_F90_CALC_G=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\calc_mom_gamma.f90
DEP_F90_CALC_M=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_utils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\calc_mom_gaus.f90
DEP_F90_CALC_MO=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_utils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\gamma_dist.f90
DEP_F90_GAMMA=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\gaus_dist.f90
DEP_F90_GAUS_=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	
# End Source File
# End Group
# Begin Source File

SOURCE=.\utilities\append_string.f90
# End Source File
# Begin Source File

SOURCE=.\utilities\bisect.f90
DEP_F90_BISEC=\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\bisect2.f90
DEP_F90_BISECT=\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\derivative.f90
# End Source File
# Begin Source File

SOURCE=.\utilities\dump_raw.f90
DEP_F90_DUMP_=\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\error_handler.f90
DEP_F90_ERROR=\
	".\Debug\h_params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\first_order_anharmonic.f90
DEP_F90_FIRST=\
	".\Debug\h_params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\gaussian.f90
DEP_F90_GAUSS=\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\get_ca.f90
DEP_F90_GET_C=\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\harmonic.f90
DEP_F90_HARMO=\
	".\Debug\h_params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\make_out_file_name.f90
# End Source File
# Begin Source File

SOURCE=.\utilities\nitems.f90
# End Source File
# Begin Source File

SOURCE=.\utilities\normalize_alpha.f90
# End Source File
# Begin Source File

SOURCE=.\utilities\round.f90
# End Source File
# Begin Source File

SOURCE=.\utilities\simple_convolute.f90
# End Source File
# Begin Source File

SOURCE=.\utilities\trim_string.f90
DEP_F90_TRIM_=\
	".\Debug\h_params.mod"\
	
# End Source File
# End Group
# Begin Group "models"

# PROP Default_Filter ""
# Begin Group "fluorescence"

# PROP Default_Filter ""
# Begin Group "dist_ET"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\fluorescence\dist_et.f90
DEP_F90_DIST_=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\fluorescence\gamma_decay.f90
DEP_F90_GAMMA_=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_utils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\fluorescence\gamma_dist_et.f90
DEP_F90_GAMMA_D=\
	".\Debug\h_fluor_models.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\fluorescence\gaus_decay.f90
DEP_F90_GAUS_D=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_utils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\fluorescence\gaus_dist_et.f90
DEP_F90_GAUS_DI=\
	".\Debug\h_fluor_models.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# End Group
# Begin Group "diffuse"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\fluorescence\diffuse.f90
DEP_F90_DIFFU=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# End Group
# Begin Group "anisotropy"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\fluorescence\horiz_polarized.f90
DEP_F90_HORIZ=\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\fluorescence\vert_polarized.f90
DEP_F90_VERT_=\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# End Group
# Begin Source File

SOURCE=.\models\fluorescence\disc_et.f90
DEP_F90_DISC_=\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\fluorescence\lifetime.f90
DEP_F90_LIFET=\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\fluorescence\order_1_anis.f90
DEP_F90_ORDER=\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\fluorescence\order_2_anis.f90
DEP_F90_ORDER_=\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# End Group
# Begin Group "constraints"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\constraints\frac.f90
DEP_F90_FRAC_=\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\constraints\Ka_constrain.f90
DEP_F90_KA_CO=\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\constraints\simple_constrain.f90
DEP_F90_SIMPL=\
	".\Debug\h_struct.mod"\
	
# End Source File
# End Group
# Begin Group "binding"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\binding\ca_gr_2_titr.f90
DEP_F90_CA_GR=\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\binding\frac_sat.f90
DEP_F90_FRAC_S=\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\binding\hill.f90
DEP_F90_HILL_=\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\binding\three_state_simple_model.f90
DEP_F90_THREE=\
	".\Debug\h_struct.mod"\
	
# End Source File
# End Group
# Begin Group "kinetics"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\kinetics\dist_k.f90
DEP_F90_DIST_K=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\kinetics\gaus_dist_k.f90
DEP_F90_GAUS_DIS=\
	".\Debug\h_kinetics_models.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\kinetics\gaus_k.f90
DEP_F90_GAUS_K=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_utils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\kinetics\sum_of_exponentials.f90
DEP_F90_SUM_O=\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# End Group
# Begin Group "general"

# PROP Default_Filter ""
# End Group
# Begin Group "physiology"

# PROP Default_Filter ""
# Begin Group "four_step_A_M"

# PROP Default_Filter ""
# End Group
# Begin Group "three_state_A_M"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\physiology\three_state_A_M\calc_three_state_A_M.f90
DEP_F90_CALC_T=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\nr.mod"\
	".\Debug\nrtype.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_A_M\odeint2.f90
DEP_F90_ODEIN=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\nrtype.mod"\
	".\Debug\nrutil.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_A_M\stiff2.f90
DEP_F90_STIFF=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\nr.mod"\
	".\Debug\nrtype.mod"\
	".\Debug\nrutil.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_A_M\three_state_A_M_ATPase.f90
DEP_F90_THREE_=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_A_M\three_state_A_M_bound.f90
DEP_F90_THREE_S=\
	".\Debug\h_models.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_A_M\three_state_A_M_force.f90
DEP_F90_THREE_ST=\
	".\Debug\h_models.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_A_M\three_state_A_M_norm_ATPase.f90
DEP_F90_THREE_STA=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_A_M\three_state_A_M_norm_force.f90
DEP_F90_THREE_STAT=\
	".\Debug\h_models.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_A_M\three_state_model.f90
DEP_F90_THREE_STATE=\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\nrtype.mod"\
	
# End Source File
# End Group
# Begin Group "three_state_ktr"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\physiology\three_state_ktr\calc_three_state_ktr.f90
DEP_F90_CALC_TH=\
	".\Debug\h_params.mod"\
	".\Debug\h_physiology_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\nr.mod"\
	".\Debug\nrtype.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_ktr\odeint_time.f90
DEP_F90_ODEINT=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\nrtype.mod"\
	".\Debug\nrutil.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_ktr\three_state_ktr_ATPase.f90
DEP_F90_THREE_STATE_=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_ktr\three_state_ktr_bound.f90
DEP_F90_THREE_STATE_K=\
	".\Debug\h_models.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_ktr\three_state_ktr_force.f90
DEP_F90_THREE_STATE_KT=\
	".\Debug\h_models.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# End Group
# Begin Group "three_state_diazo_2"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\physiology\three_state_diazo_2\calc_three_state_diazo_2.f90
DEP_F90_CALC_THR=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\nr.mod"\
	".\Debug\nrtype.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_diazo_2\three_state_diazo_2_force.f90
DEP_F90_THREE_STATE_D=\
	".\Debug\h_models.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# End Group
# Begin Group "isometric_twitch"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\physiology\isometric_twitch\calc_isometric_twitch.f90
DEP_F90_CALC_I=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\nr.mod"\
	".\Debug\nrtype.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\isometric_twitch\isometric_twitch_force.f90
DEP_F90_ISOME=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\isometric_twitch\odeint_time_contraction.f90
DEP_F90_ODEINT_=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\nrtype.mod"\
	".\Debug\nrutil.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\isometric_twitch\stiff3.f90
DEP_F90_STIFF3=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\nr.mod"\
	".\Debug\nrtype.mod"\
	".\Debug\nrutil.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\isometric_twitch\three_state_t_model.f90
DEP_F90_THREE_STATE_T=\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_utils.mod"\
	
# End Source File
# End Group
# Begin Group "isotonic_twitch"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\physiology\isotonic_twitch\calc_isotonic_twitch.f90
DEP_F90_CALC_IS=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\nr.mod"\
	".\Debug\nrtype.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\isotonic_twitch\isotonic_twitch_atpase.f90
DEP_F90_ISOTO=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\isotonic_twitch\load_clamp.f90
DEP_F90_LOAD_=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\isotonic_twitch\odeint_time_isotonic.f90
DEP_F90_ODEINT_T=\
	".\Debug\h_physiology_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\nrtype.mod"\
	".\Debug\nrutil.mod"\
	
# End Source File
# End Group
# Begin Group "three_state_SS"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\physiology\three_state_SS\three_state_SS.f90
DEP_F90_THREE_STATE_S=\
	".\Debug\h_params.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\nrtype.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_SS\three_state_SS_ATPase.f90
DEP_F90_THREE_STATE_SS=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_SS\three_state_SS_ATPase_RU_g.f90
DEP_F90_THREE_STATE_SS_=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_SS\three_state_SS_ATPase_S1_extr.f90
DEP_F90_THREE_STATE_SS_A=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_SS\three_state_SS_ATPase_TnC_extr.f90
DEP_F90_THREE_STATE_SS_AT=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_SS\three_state_SS_force.f90
DEP_F90_THREE_STATE_SS_F=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_SS\three_state_SS_force_NEM_S1.f90
DEP_F90_THREE_STATE_SS_FO=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_SS\three_state_SS_force_RU_g.f90
DEP_F90_THREE_STATE_SS_FOR=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_SS\three_state_SS_force_S1_extr.f90
DEP_F90_THREE_STATE_SS_FORC=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\models\physiology\three_state_SS\three_state_SS_force_TnC_extr.f90
DEP_F90_THREE_STATE_SS_FORCE=\
	".\Debug\h_models.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# End Group
# End Group
# Begin Group "dist_meas"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\models\ET_modeling\get_distance.f90
DEP_F90_GET_D=\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	".\Debug\numerical_libraries.mod"\
	".\Debug\transform_coords.mod"\
	
# End Source File
# End Group
# End Group
# Begin Group "routines"

# PROP Default_Filter ""
# Begin Group "nr_routines"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\utilities\polint.f90
DEP_F90_POLIN=\
	".\Debug\nrtype.mod"\
	".\Debug\nrutil.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\qromb.f90
DEP_F90_QROMB=\
	".\Debug\nr.mod"\
	".\Debug\nrtype.mod"\
	".\Debug\nrutil.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utilities\trapzd.f90
DEP_F90_TRAPZ=\
	".\Debug\nrtype.mod"\
	".\Debug\nrutil.mod"\
	
# End Source File
# End Group
# Begin Source File

SOURCE=.\routines\add_noise_to_data.f90
DEP_F90_ADD_N=\
	".\Debug\h_struct.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\calc_chi_sqr.f90
DEP_F90_CALC_C=\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\calc_covariance.f90
DEP_F90_CALC_CO=\
	".\Debug\h_params.mod"\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\calc_curvature_gradient.f90
DEP_F90_CALC_CU=\
	".\Debug\h_params.mod"\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\cluster.f90
DEP_F90_CLUST=\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\const_dest.f90
DEP_F90_CONST=\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\convolute.f90
DEP_F90_CONVO=\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\expt_fit.f90
DEP_F90_EXPT_=\
	".\Debug\h_models.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\expt_type_parser.f90
DEP_F90_EXPT_T=\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\generate_lamp.f90
DEP_F90_GENER=\
	".\Debug\h_params.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\get_default.f90
DEP_F90_GET_DE=\
	".\Debug\h_params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\logic.f90
DEP_F90_LOGIC=\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\marquadt.f90
DEP_F90_MARQU=\
	".\Debug\h_params.mod"\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\minimize.f90
DEP_F90_MINIM=\
	".\Debug\h_params.mod"\
	".\Debug\h_routines.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\model_lexicon.f90
DEP_F90_MODEL=\
	".\Debug\h_models.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_utils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\output_results.f90
DEP_F90_OUTPU=\
	".\Debug\h_fluor_wrapper_models.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_plots.mod"\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\plot_points.f90
# End Source File
# Begin Source File

SOURCE=.\routines\readans.f90
DEP_F90_READA=\
	".\Debug\h_params.mod"\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\rieman_integrate.f90
# End Source File
# Begin Source File

SOURCE=.\routines\shift.f90
DEP_F90_SHIFT=\
	".\Debug\h_utils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\routines\trapezoidal_integrate.f90
# End Source File
# Begin Source File

SOURCE=.\routines\tri_ge.f90
# End Source File
# Begin Source File

SOURCE=.\routines\tri_mat_mult.f90
# End Source File
# End Group
# Begin Group "rigorous"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\rigorous\area_under_the_curve.f90
DEP_F90_AREA_=\
	".\Debug\h_struct.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\arr_write.f90
DEP_F90_ARR_W=\
	".\Debug\h_rigorous.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\asymptotic_elipse.f90
DEP_F90_ASYMP=\
	".\Debug\h_params.mod"\
	".\Debug\h_rigorous.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\corners.f90
DEP_F90_CORNE=\
	".\Debug\h_params.mod"\
	".\Debug\h_rigorous.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\crunch.f90
DEP_F90_CRUNC=\
	".\Debug\h_params.mod"\
	".\Debug\h_rigorous.mod"\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\do_bootstrap.f90
DEP_F90_DO_BO=\
	".\Debug\h_params.mod"\
	".\Debug\h_rigorous.mod"\
	".\Debug\h_routines.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\f_conf.f90
DEP_F90_F_CON=\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\output_rig_res.f90
DEP_F90_OUTPUT=\
	".\Debug\h_fluor_wrapper_models.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\pos.f90
DEP_F90_POS_F=\
	".\Debug\h_params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\rigorous.f90
DEP_F90_RIGOR=\
	".\Debug\h_params.mod"\
	".\Debug\h_rigorous.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\sides_3D.f90
DEP_F90_SIDES=\
	".\Debug\h_params.mod"\
	".\Debug\h_rigorous.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\update_fixed_param_val.f90
DEP_F90_UPDAT=\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rigorous\wacko_func.f90
DEP_F90_WACKO=\
	".\Debug\h_params.mod"\
	".\Debug\h_rigorous.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# End Group
# Begin Group "plots"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\plots\binding_plot.f90
DEP_F90_BINDI=\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\plots\color_scheme.f90
DEP_F90_COLOR=\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\plots\plot_driver.f90
DEP_F90_PLOT_=\
	".\Debug\h_params.mod"\
	".\Debug\h_plots.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\plots\time_dom_fluor_plot.f90
DEP_F90_TIME_=\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\plots\time_dom_fluor_simul_plot.f90
DEP_F90_TIME_D=\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# End Group
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\header\h_binding_models.f90
DEP_F90_H_BIN=\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_constr_models.f90
DEP_F90_H_CON=\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_dist_models.f90
DEP_F90_H_DIS=\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\h_dist_struct.f90
# End Source File
# Begin Source File

SOURCE=.\header\h_fluor_models.f90
DEP_F90_H_FLU=\
	".\Debug\h_dist_struct.mod"\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_utils.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_fluor_wrapper_models.f90
DEP_F90_H_FLUO=\
	".\Debug\h_fluor_models.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_general_models.f90
DEP_F90_H_GEN=\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_kinetics_models.f90
DEP_F90_H_KIN=\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_models.f90
DEP_F90_H_MOD=\
	".\Debug\h_binding_models.mod"\
	".\Debug\h_constr_models.mod"\
	".\Debug\h_dist_models.mod"\
	".\Debug\h_fluor_models.mod"\
	".\Debug\h_fluor_wrapper_models.mod"\
	".\Debug\h_general_models.mod"\
	".\Debug\h_kinetics_models.mod"\
	".\Debug\h_physiology_models.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_params.f90
# End Source File
# Begin Source File

SOURCE=.\header\h_physiology_models.f90
DEP_F90_H_PHY=\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\nr.mod"\
	".\Debug\nrtype.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_plots.f90
DEP_F90_H_PLO=\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_rigorous.f90
DEP_F90_H_RIG=\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_routine_specific_vars.f90
# End Source File
# Begin Source File

SOURCE=.\header\h_routines.f90
DEP_F90_H_ROU=\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\h_vars.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_struct.f90
DEP_F90_H_STR=\
	".\Debug\h_params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_utils.f90
DEP_F90_H_UTI=\
	".\Debug\h_params.mod"\
	".\Debug\h_routine_specific_vars.mod"\
	".\Debug\h_struct.mod"\
	".\Debug\numerical_libraries.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_vars.f90
DEP_F90_H_VAR=\
	".\Debug\h_params.mod"\
	".\Debug\h_struct.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\h_w_vars.f90
# End Source File
# Begin Source File

SOURCE=.\header\Nr.f90
DEP_F90_NR_F9=\
	".\Debug\nrtype.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\Nrtype.f90
# End Source File
# Begin Source File

SOURCE=.\header\Nrutil.f90
DEP_F90_NRUTI=\
	".\Debug\nrtype.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\header\transform_coords.f90
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=.\bind_constr1.dat
# End Source File
# Begin Source File

SOURCE=.\Cys374_Mg.dat
# End Source File
# Begin Source File

SOURCE=.\zeros.dat
# End Source File
# End Target
# End Project
