FROM ubuntu:20.04

# see https://askubuntu.com/questions/909277/avoiding-user-interaction-with-tzdata-when-installing-certbot-in-a-docker-contai
ARG DEBIAN_FRONTEND=noninteractive

RUN apt update -qq
RUN apt -y install curl wget libtesseract-dev tesseract-ocr imagemagick libva-dev snapd chromium-browser libtinfo-dev neovim ripgrep
# RUN service snapd start
# RUN snap install chromium

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
RUN . /root/.cargo/env && curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

RUN stack setup
