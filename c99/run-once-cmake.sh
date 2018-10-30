#!/bin/bash

if [[ ! -d build ]]; then
    mkdir -p build
fi

cd build
cmake .. -DCMAKE_BUILD_TYPE=DEBUG && cmake --build . && ctest --output-on-failure
