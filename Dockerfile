FROM erlang:alpine

COPY app.erl app.erl
COPY domain.hrl domain.hrl
COPY domain.erl domain.erl
COPY optimizer.hrl optimizer.hrl
COPY optimizer.erl optimizer.erl
COPY server.erl server.erl

COPY run.sh run.sh

RUN erlc app.erl
RUN erlc domain.erl
RUN erlc optimizer.erl
RUN erlc server.erl

EXPOSE 8696

CMD /bin/sh run.sh