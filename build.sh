#!/bin/sh -x
export ACME=${USERPROFILE}/Downloads/acme0.97win/acme
export VICE=${USERPROFILE}/Downloads/GTK3VICE-3.8-win64/bin
mkdir build 2>/dev/null
${ACME}/acme -f cbm -o build/int64_ml.bin -l build/int64.lbl int64.asm
[ $? -eq 0 ] && ${VICE}/c1541 int64.d64 -attach int64.d64 8 -delete int64.ml -delete int64_ml.bin -write build/int64_ml.bin int64_ml.bin
[ $? -eq 0 ] && rm d64_files/*
[ $? -eq 0 ] && ${VICE}/c1541 int64.d64 -attach int64.d64 8 -cd d64_files -extract
[ $? -eq 0 ] && ${VICE}/x64sc -moncommands build/int64.lbl -autostart int64.d64 >/dev/null 2>&1 &
