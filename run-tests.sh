#!/bin/bash
npx shadow-cljs compile test
./node_modules/karma/bin/karma start --single-run