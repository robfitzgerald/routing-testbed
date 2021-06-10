#!/bin/bash
declare -a workers=("03" "04" "08" "11" "12" "14" "15")

partitions=7
experiment="20210523-rand-grid"
dir_path="../${experiment}"

original_dir=$(pwd)
cd "$dir_path" || exit

for (( i = 0; i < partitions; i++ )); do
    worker="${workers[i]}"
    cp -r "${i}" "${experiment}"
    tar -czvf "${worker}.tar.gz" "${experiment}"
    rm -r "${experiment}"
done

tar -czvf "${experiment}.tar.gz" ./*.tar.gz

cd "$original_dir" || exit
