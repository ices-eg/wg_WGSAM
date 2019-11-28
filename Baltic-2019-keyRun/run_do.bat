H:
cd "H:\_C_drev\SMS-git\Baltic-2019-keyRun\"
del *.?0?
del *.out
del *.mcm
del *.bin
del admodel.*
del *.csv
del *.std
del *.bar
del *.mc2
del *.cor
del *.psv
del *.ecm
del *.xls
del *.html
del mcout*.all
del _*.txt
del OP_BATCH*.*
del SMS.o*
del *.wmf
del *.png
del *.ps
del *.lg
del *.log
del ud.dat
del gradient.dat
del op_*.out
del iter*.*
del stom_and_noise*.in
del canum_and_noise*
del survey_and_noise*
del baseline*.*
del HCR_prob.dat
del HCR_yield.dat
del HCR_SSB.dat
del *.par
del *.rep
del *.hst
del *.eva
del *.tmp
del amoeba*.*
del covariance_*.*
del forecast*.*
H:
cd "H:\_C_drev\SMS-git\Baltic-2019-keyRun\"
del /f sms.rep 
del /f sms.par 
del /f sms.std 
sms -nox -ind run_ms0.dat -nohess  >run_out0_0.lg
copy /Y "sms.par" "run_ms0.par"
copy /Y "sms.rep" "run_ms0.rep"
H:
cd "H:\_C_drev\SMS-git\Baltic-2019-keyRun\"
del /f "sms.rep 
del /f "sms.par 
del /f "sms.std 
sms -nox -gbs 1500000000  -nohess  -ind run_ms1.dat  -ainp run_ms0.par -phase 2 >run_out1_0.lg
copy /Y "sms.par" "run_ms1.par"
copy /Y "sms.rep" "run_ms1.rep"
H:
cd "H:\_C_drev\SMS-git\Baltic-2019-keyRun\"
del /f "sms.rep 
del /f "sms.par 
del /f "sms.std 
sms -nox -gbs 1500000000  -nohess  -ind run_ms2.dat -ainp run_ms1.par -phase 2 >run_out2_0.lg
copy /Y "sms.par" "run_ms2.par"
copy /Y "sms.rep" "run_ms2.rep"
copy /Y "sms.std" "run_ms2.std"
H:
cd "H:\_C_drev\SMS-git\Baltic-2019-keyRun\"
del /f "sms.rep 
del /f "sms.par 
del /f "sms.std 
sms -nox -gbs 1500000000   -ind run_ms3.dat -ainp run_ms2.par -phase 2 >run_out3_0.lg
copy /Y "sms.par" "run_ms3.par"
copy /Y "sms.rep" "run_ms3.rep"
copy /Y "sms.std" "run_ms3.std"
