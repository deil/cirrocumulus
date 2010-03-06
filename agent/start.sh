#!/bin/bash

rm -rf ebin/cirrocumulus.beam
erlc src/cirrocumulus.erl
mv cirrocumulus.beam ebin/
erl -pa ebin