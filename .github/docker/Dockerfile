FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-happy-arbitrary"]
COPY --chown=builder:users . /work/hs-happy-arbitrary
RUN ["stack", "install", "happy-arbitrary"]

FROM scratch
COPY --from=build /home/builder/.local/ /
