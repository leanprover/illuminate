FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends inkscape fontconfig && \
    rm -rf /var/lib/apt/lists/*

COPY . /tmp/illuminate/
RUN mkdir -p /usr/share/fonts/dejavu && \
    cp /tmp/illuminate/fonts/DejaVuSans.ttf /tmp/illuminate/fonts/DejaVuSansMono.ttf /usr/share/fonts/dejavu/ && \
    cp /tmp/illuminate/fonts.conf /etc/fonts/conf.d/99-illuminate.conf && \
    cp /tmp/illuminate/render.sh /usr/local/bin/render-svg && \
    chmod +x /usr/local/bin/render-svg && \
    rm -rf /tmp/illuminate && \
    fc-cache -f

ENTRYPOINT ["render-svg"]
