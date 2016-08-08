#!/bin/bash

for((i=0;i<1;i++)); do # fake loop to use | gnuplot at the end

    # (1) ANIMATED GIF
    # echo "set term gif animate delay 0.04 enhanced;"
    # echo "set output \"animation.gif\";"

    for FILE in $(ls out*); do
        #echo "set xlabel \"Label\""
        #echo "set ylabel \"Label2\""
        # (2) MANY PNGs
        #echo "set term png"
        #echo "set output \"${FILE}.png\""
        # (3) TO SCREEN
        echo "set grid; show grid"
        #echo "set pm3d;"
        echo "set zrange [] writeback" # to fix the yrange
        #echo "plot \"${FILE}\" using 1 w lp;"
        echo "splot \"${FILE}\" using 1:2:3;"
        echo "set zrange restore"
        echo "pause 1.0/5."
    done 

done | gnuplot -persist
