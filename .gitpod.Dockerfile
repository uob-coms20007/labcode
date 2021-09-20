FROM docker.io/gitpod/workspace-full

USER gitpod

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=9.0.1

RUN    curl --proto 'https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
    && echo "source /home/gitpod/.ghcup/env" >> /home/gitpod/.bashrc \
    && . /home/gitpod/.ghcup/env
