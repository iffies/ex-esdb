################# Variables ################
#ARG ELIXIR_VERSION=1.17.2
#ARG OTP_VERSION=27.0.1
#ARG OS_VERSION=bullseye-20240722-slim
# ARG ELIXIR_VERSION=1.17.3
# ARG OTP_VERSION=27.1.2
# ARG OS_VERSION=bullseye-20241111-slim
# ARG OS_TYPE=debian

ARG ELIXIR_VERSION=1.18.3
ARG OTP_VERSION=27.3
ARG OS_VERSION=bullseye-20250224-slim
ARG OS_TYPE=debian

ARG BUILDER_IMAGE="hexpm/elixir:${ELIXIR_VERSION}-erlang-${OTP_VERSION}-${OS_TYPE}-${OS_VERSION}"
ARG RUNNER_IMAGE="${OS_TYPE}:${OS_VERSION}"
##########################################
################# BUILDER ################
##########################################
FROM ${BUILDER_IMAGE} AS builder

RUN apt-get update -y && \
  apt-get upgrade -y --autoremove && \
  apt-get clean 

# && \
# rm -f /var/lib/apt/lists/*_*

RUN apt-get install -y pkg-config openssl curl build-essential git
# prepare build dir
WORKDIR /build_space

# install hex + rebar
RUN mix local.hex --force && \
  mix local.rebar --force

# set build ENV
ARG MIX_ENV=prod
ENV MIX_ENV=prod

ENV OTPROOT=/usr/lib/erlang
ENV ERL_LIBS=/usr/lib/erlang/lib

# copy sources
COPY lib/ lib/
COPY src/ src/
COPY priv/ priv/

COPY config/config.exs config/prod.exs config/runtime.exs  config/
COPY mix.exs ./
#COPY mix.exs mix.lock ./
# install mix dependencies
RUN MIX_ENV="prod" mix do deps.get --only "prod", deps.update --all, deps.compile
# build assets
RUN MIX_ENV="prod" mix do compile, release scarab_es
## Release
#RUN MIX_ENV="prod" mix release scarab_es

###################################
########### RUNTIME ###############
###################################

FROM ${RUNNER_IMAGE} AS scarab_es

ARG ERTS_VERSION=erts-15.2.3

RUN echo "!!!!!!!! Building runner image...!!!!!!!" && sleep 2

RUN apt-get update -y && \
  apt-get upgrade -y --autoremove && \
  apt-get clean && \
  apt-get install -y pkg-config openssl libncurses5 locales ca-certificates curl systemd && \
  rm -f /var/lib/apt/lists/*_* 
#
# Set the locale
#RUN locale-gen en_US.UTF-8
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
#
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

WORKDIR /system

RUN mkdir /data

RUN chown -R nobody /data

COPY --from=builder --chown=nobody /build_space/_build/prod/rel/scarab_es ./

COPY run-scarab-es.sh .
COPY check-scarab-es.sh .

RUN chmod +x run-scarab-es.sh check-scarab-es.sh

ARG PATH="${PATH}:/system/${ERTS_VERSION}/bin"

# RUN chown nobody /system

VOLUME /data

EXPOSE 4369 9000-9100


# USER nobody

#VOLUME /data

ENV HOME=/system
ENV MIX_ENV="prod"
ENV PATH="${PATH}"

ARG SCARAB_COOKIE
ARG SCARAB_DATA
ARG SCARAB_STORE

# RUN ls -lha && sleep 10

# RUN epmd -names
#

HEALTHCHECK --interval=10s --timeout=3s \
  CMD ["./check-scarab-es.sh"]

CMD ["./run-scarab-es.sh"]
