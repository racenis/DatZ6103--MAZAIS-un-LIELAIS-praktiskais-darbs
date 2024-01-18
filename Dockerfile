
# sākam ar elles mašīnu uz kuras ir jau uzinstalēts erlangs
FROM erlang:alpine

# iekopējam programmas kodu
COPY app.erl app.erl
COPY domain.hrl domain.hrl
COPY domain.erl domain.erl
COPY optimizer.hrl optimizer.hrl
COPY optimizer.erl optimizer.erl
COPY server.erl server.erl

# iekopējam html/javascript kodu
COPY web/index.html web/index.html
COPY web/not_found.html web/not_found.html

# iekopējam palaišanas skriptu
COPY run.sh run.sh

# nokompilējam programmu
RUN erlc app.erl
RUN erlc domain.erl
RUN erlc optimizer.erl
RUN erlc server.erl

# atveram programmas http/tcp portu 8696
EXPOSE 8696

# palaižam ieslēgšanās skriptu
CMD /bin/sh run.sh
