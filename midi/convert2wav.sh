#!/bin/bash

for filename in *.mid; do
    timidity --output-mode=w --output-mono --output-8bit --output-file="../sounds/${filename%.mid}.wav" "$filename"
done
