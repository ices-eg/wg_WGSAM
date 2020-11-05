C:
cd "C:\_C_drev\SMS-git\NorthSeaKeyRun_2020_withF\"
sms -nox -gbs 1500000000   -ind run_ms3.dat -ainp run_ms2.par -phase 2 >run_out3_0.lg
copy /Y "sms.par" "run_ms3.par"
copy /Y "sms.rep" "run_ms3.rep"
copy /Y "gradient.dat" "run_ms3.grd"
copy /Y "sms.std" "run_ms3.std"
