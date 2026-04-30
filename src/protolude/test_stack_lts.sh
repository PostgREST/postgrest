#!/usr/bin/env bash
set -e

stack build --resolver lts-4.0
stack build --resolver lts-5.0
stack build --resolver lts-6.0
stack build --resolver lts-7.0
stack build --resolver lts-8.0
stack build --resolver lts-9.0
stack build --resolver lts-10.0
stack build --resolver lts-11.0
stack build --resolver lts-12.0
stack build --resolver lts-13.0
stack build --resolver lts-14.0
stack build --resolver lts-15.0
stack build --resolver lts-16.0
stack build --resolver lts-17.0
stack build --resolver lts-18.0
stack build --resolver lts-19.0
