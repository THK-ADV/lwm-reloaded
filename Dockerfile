FROM hseeberger/scala-sbt:11.0.8_1.3.13_2.12.12
LABEL maintainer="alexander.dobrynin@th-koeln.de"

WORKDIR /lwm
COPY . .
RUN sbt compile
CMD sbt run