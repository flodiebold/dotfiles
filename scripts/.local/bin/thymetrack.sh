#!/bin/bash

mkdir -p ~/.logs/thyme/$(date -u +%Y/%m)
thyme track -o ~/.logs/thyme/$(date -u +%Y/%m/%d.json)
