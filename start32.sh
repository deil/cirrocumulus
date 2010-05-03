#!/bin/bash

cd agent/src
erl -pa ../../agent/ebin -pa ../../eresye/ebin -setcookie cirrocumulus
cd ..
