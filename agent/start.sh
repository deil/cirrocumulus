#!/bin/bash

rm -rf ebin/cirrocumulus.beam
rm -rf ebin/script_server.beam
erlc src/cirrocumulus.erl
erlc src/script_server.erl
mv cirrocumulus.beam ebin/
mv script_server.beam ebin/
erl -pa ebin