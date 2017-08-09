FROM heroku/cedar:14

ENV GHCVER 8.0.2
ENV CABALVER 1.22

RUN mkdir -p /app/user
WORKDIR /app/user

# Create app user, required? by Heroku
RUN useradd -d /app/user -m app

# Run bundler to cache dependencies
ADD . /app/user

# haskell

ONBUILD USER root
RUN apt-get update && apt-get install -y --no-install-recommends software-properties-common \
  && add-apt-repository -y ppa:hvr/ghc \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
    cabal-install-$CABALVER \
    ghc-$GHCVER \
  && rm -rf /var/lib/apt/lists/*

ENV PATH /opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH

ENV HOME /app/user
ENV PORT 3000

RUN gpg --recv-key --keyserver keyserver.ubuntu.com D6CF60FD
# Changing trust level to 4 = marginally trust
RUN echo E595AD4214AFA6BB15520B23E40D74D6D6CF60FD:4: | \
    gpg --import-ownertrust

# We need to install stackage somehow
RUN cabal update
# RUN cabal install 'http-client==0.4.29' 'stackage'

ENV PATH $HOME/.cabal/bin:$PATH
# RUN stk update --verify --hashes

RUN cabal install 'warp >=3.0' 'wai-app-static >=3.0' 'waitra >=0.0.3'

# Build the app

ONBUILD USER root
ONBUILD RUN chown -R app /app/user
ONBUILD USER app

ONBUILD WORKDIR /app/user
# ONBUILD RUN stk update
# ONBUILD RUN stk install

WORKDIR /app/user
RUN export HOST="127.0.0.1"
RUN /bin/bash -c "source /app/user/database_envs.sh \
&& cabal --sandbox-config-file=/app/src/heroku.cabal.sandbox.config sandbox --sandbox=/app/src/dist-heroku/.heroku-cabal-sandbox init \
&& cabal --sandbox-config-file=/app/src/heroku.cabal.sandbox.config install --dependencies-only \
&& cabal --sandbox-config-file=/app/src/heroku.cabal.sandbox.config configure \
&& cabal --sandbox-config-file=/app/src/heroku.cabal.sandbox.config build --builddir=/app/src/dist-heroku \
&& strip --strip-unneeded /app/src/dist-heroku/build/sendai-subway-api/sendai-subway-api"
RUN mkdir -p /app/user/dist/build/sendai-subway-api
RUN cp /app/src/dist-heroku/build/sendai-subway-api/sendai-subway-api /app/user/dist/build/sendai-subway-api

# Cleanup to make slug smaller
ONBUILD RUN rm -rf /app/.cabal /app/.stackage /app/.ghc

ONBUILD EXPOSE 3000
