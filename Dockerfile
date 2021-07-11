FROM ubuntu:20.04

RUN apt update -qq
RUN apt -y install curl wget libtesseract-dev tesseract-ocr imagemagick libva-dev snapd chromium-browser libtinfo-dev neovim ripgrep
# RUN service snapd start
# RUN snap install chromium

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack setup

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -- -y
RUN . /root/.cargo/env
RUN curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

COPY ./ /openmemex/
