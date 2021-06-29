FROM openjdk:15.0.1
LABEL maintainer="Alexander.Dobrynin@th-koeln.de"

COPY target/universal/lwm-reloaded-1.0 /lwm
CMD /lwm/bin/lwm-reloaded