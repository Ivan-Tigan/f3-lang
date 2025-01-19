#!/bin/bash
wget https://github.com/Ivan-Tigan/f3-lang/releases/download/v1.0.0/f3-lang_1.0_amd64.snap
sudo snap install --dangerous f3-lang_1.0_amd64.snap
sudo snap alias f3-lang.f3 f3
rm f3-lang_1.0_amd64.snap