FROM hseeberger/scala-sbt:11.0.14.1_1.6.2_2.12.15
LABEL maintainer="alexander.dobrynin@th-koeln.de"

WORKDIR /lwm
COPY . .
RUN sbt clean stage
CMD target/universal/stage/bin/lwm-reloaded -Dconfig.file=conf/application-prod.conf