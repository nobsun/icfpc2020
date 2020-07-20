FROM icfpcontest2020/haskell:latest

ARG STACK_RESOLVER=lts-16.3

RUN apt-get update \
  && apt-get install -y libcurl4-openssl-dev \
  && apt-get install -y libglfw3-dev libxi-dev libxrandr-dev libxxf86vm-dev libxcursor-dev libxinerama-dev \
  && apt-get install -y libglu1-mesa-dev \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN stack update && \
  stack install --resolver ${STACK_RESOLVER} \
    JuicyPixels \
    curl \
    GLFW-b \
    OpenGL
