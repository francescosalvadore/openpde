#!/bin/bash

for((i=0;i<1;i++)); do # fake loop to use | gnuplot at the end

#OUTPUT TO FILE echo "set term gif animate delay 0.04 enhanced;"
#OUTPUT TO FILE echo "set output \"animation.gif\";"

    for FILE in $(ls out*); do
        #echo "set xlabel \"Label\""
        #echo "set ylabel \"Label2\""
        #echo "set term png"
        #echo "set output \"${FILE}.png\""
        echo "plot \"${FILE}\" using 1 w l;"
        echo "pause 1.0/5."
    done 
done | gnuplot -persist
