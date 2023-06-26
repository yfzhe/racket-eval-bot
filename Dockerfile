FROM racket/racket:8.8-full

WORKDIR /app

COPY eval-bot eval-bot
RUN raco pkg install -i --auto --no-docs --name app --copy ./eval-bot

EXPOSE 8000

CMD ["racket", "-l", "eval-bot"]
