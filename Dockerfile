FROM hseeberger/scala-sbt:15.0.1_1.4.7_2.12.12
LABEL maintainer="alexander.dobrynin@th-koeln.de"

WORKDIR /lwm
COPY . .
RUN sbt compile
CMD sbt run