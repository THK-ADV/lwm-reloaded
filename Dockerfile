FROM openjdk:15.0.1
LABEL maintainer="Alexander.Dobrynin@th-koeln.de"

COPY target/universal/lwm-reloaded-1.0 /lwm
CMD /lwm/bin/lwm-reloaded -Dplay.http.secret.key='WpP@1v@/ss/UDQ5XkG@depjlg?QcV4vzG@?1Pv[_:FqmnxFX:sGE`7C_>mn_BIl<'