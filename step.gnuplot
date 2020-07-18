set term png
set output 'rough.png'
plot 'rough.csv' using 0:1
set output 'smooth.png'
plot 'smooth.csv' using 0:1
