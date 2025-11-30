FROM ivantsoninski/f3
COPY index.f3 /data/
WORKDIR /data
CMD sh -c 'echo "$F3_CONFIG" > /tmp/config.f3 && cat /data/index.f3 >> /tmp/config.f3 && f3 run /tmp/config.f3'