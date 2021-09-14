#!/bin/sh

for i in `ls $1`; do
  for j in `ls $1/$i`; do
    pkg_energy=$(grep energy-pkg $1/$i/$j | awk '{print $1}')
    ram_energy=$(grep energy-ram $1/$i/$j | awk '{print $1}')
    runtime=$(grep "seconds time elapsed" $1/$i/$j | awk '{print $1}' | sed 's/,/./g')

    if [[ "$pkg_energy" == *\,*\.* ]]; then
      pkg_energy=$(echo "$pkg_energy" | sed 's/,//')
    else
      pkg_energy=$(echo "$pkg_energy" | sed 's/,/./')
    fi

    if [[ "$ram_energy" == *\,*\.* ]]; then
      ram_energy=$(echo "$ram_energy" | sed 's/,//')
    else
      ram_energy=$(echo "$ram_energy" | sed 's/,/./')
    fi

    total_energy=$(echo "scale=2; $pkg_energy + $ram_energy" | bc)

    echo "$j: $total_energy" >> $1/$i/energy.txt
    echo "$j: $runtime" >> $1/$i/runtime.txt 
  done
done

echo Done with all