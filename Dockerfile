FROM ubuntu:18.04

RUN apt update -qq
RUN apt -y install curl wget libtesseract-dev tesseract-ocr imagemagick libva-dev snapd
RUN snap install chromium

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack setup

COPY ./ /openmemex/
RUN stack build

