C:
cd "C:\mv\sms\NS_63-10-key-run\"
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
del *.wmf
del *.lg
del *.log
del ud.dat
del HCR_prob.dat
del HCR_yield.dat
del HCR_SSB.dat
del *.par
del *.rep
del *.hst
del *.eva
del *.tmp
del amoeba*.*
C:
cd "C:\mv\sms\NS_63-10-key-run\"
del /f "C:\mv\sms\NS_63-10-key-run\sms.rep 
del /f "C:\mv\sms\NS_63-10-key-run\sms.par 
del /f "C:\mv\sms\NS_63-10-key-run\sms.std 
sms  -nox -ind run_ms0.dat -nohess  >run_out0_0.lg
copy /Y "C:\mv\sms\NS_63-10-key-run\sms.par" "C:\mv\sms\NS_63-10-key-run\run_ms0.par" 
copy /Y "C:\mv\sms\NS_63-10-key-run\sms.rep" "C:\mv\sms\NS_63-10-key-run\run_ms0.rep" 
C:
cd "C:\mv\sms\NS_63-10-key-run\"
del /f "C:\mv\sms\NS_63-10-key-run\sms.rep 
del /f "C:\mv\sms\NS_63-10-key-run\sms.par 
del /f "C:\mv\sms\NS_63-10-key-run\sms.std 
sms  -nox -gbs 1500000000  -nohess  -ind run_ms1.dat  -ainp run_ms0.par -phase 2 >run_out1_0.lg
copy /Y "C:\mv\sms\NS_63-10-key-run\sms.par" "C:\mv\sms\NS_63-10-key-run\run_ms1.par" 
copy /Y "C:\mv\sms\NS_63-10-key-run\sms.rep" "C:\mv\sms\NS_63-10-key-run\run_ms1.rep" 
C:
cd "C:\mv\sms\NS_63-10-key-run\"
del /f "C:\mv\sms\NS_63-10-key-run\sms.rep 
del /f "C:\mv\sms\NS_63-10-key-run\sms.par 
del /f "C:\mv\sms\NS_63-10-key-run\sms.std 
sms  -nox -gbs 1500000000  -nohess  -ind run_ms2.dat -ainp run_ms1.par -phase 2 >run_out2_0.lg
copy /Y "C:\mv\sms\NS_63-10-key-run\sms.par" "C:\mv\sms\NS_63-10-key-run\run_ms2.par" 
copy /Y "C:\mv\sms\NS_63-10-key-run\sms.rep" "C:\mv\sms\NS_63-10-key-run\run_ms2.rep" 
copy /Y "C:\mv\sms\NS_63-10-key-run\sms.std" "C:\mv\sms\NS_63-10-key-run\run_ms2.std" 
C:
cd "C:\mv\sms\NS_63-10-key-run\"
del /f "C:\mv\sms\NS_63-10-key-run\sms.rep 
del /f "C:\mv\sms\NS_63-10-key-run\sms.par 
del /f "C:\mv\sms\NS_63-10-key-run\sms.std 
sms  -nox -gbs 1500000000   -ind run_ms3.dat -ainp run_ms2.par -phase 2 >run_out3_0.lg
copy /Y "C:\mv\sms\NS_63-10-key-run\sms.par" "C:\mv\sms\NS_63-10-key-run\run_ms3.par" 
copy /Y "C:\mv\sms\NS_63-10-key-run\sms.rep" "C:\mv\sms\NS_63-10-key-run\run_ms3.rep" 
copy /Y "C:\mv\sms\NS_63-10-key-run\sms.std" "C:\mv\sms\NS_63-10-key-run\run_ms3.std" 
