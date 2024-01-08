FROM racket/racket:8.11-full

RUN raco pkg install -i --auto --no-docs rhombus-prototype

WORKDIR /app

COPY telebot telebot
RUN raco pkg install -i --auto --no-docs --copy ./telebot

COPY eval-bot eval-bot
RUN raco pkg install -i --auto --no-docs --name app --copy ./eval-bot

EXPOSE 8000

CMD ["racket", "-l", "eval-bot"]
