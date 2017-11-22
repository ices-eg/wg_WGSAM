C:
cd "C:\mv\SMS-git\NS_key-2017-ver05\"
del /f "sms.rep 
del /f "sms.par 
del /f "sms.std 
sms -nox -gbs 1500000000   -ind run_ms3.dat -ainp run_ms2.par -phase 2 >run_out3_0.lg
copy /Y "sms.par" "run_ms3.par"
copy /Y "sms.rep" "run_ms3.rep"
copy /Y "sms.std" "run_ms3.std"
