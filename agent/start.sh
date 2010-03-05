#!/bin/bash

rm -rf ebin/echo_client.beam
erlc src/echo_client.erl
mv echo_client.beam ebin/
erl -pa ebin