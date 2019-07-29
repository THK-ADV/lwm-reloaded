FROM openjdk:12.0
LABEL maintainer="Alexander.Dobrynin@th-koeln.de"
EXPOSE 9000
VOLUME /lwm/conf

COPY target/universal/lwm-reloaded-1.0-SNAPSHOT /lwm
CMD /lwm/bin/lwm-reloaded -Dplay.http.secret.key='WpP@1v@/ss/UDQ5XkG@depjlg?QcV4vzG@?1Pv[_:FqmnxFX:sGE`7C_>mn_BIl<'