FROM haskell:9.2.5

WORKDIR /opt/goofy-bot

RUN cabal update

COPY ./goofy-bot.cabal /opt/goofy-bot/goofy-bot.cabal

RUN cabal build --only-dependencies

COPY . /opt/goofy-bot
RUN cabal install

CMD ["goofy-bot"]
