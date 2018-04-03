# telegram-bot
Sample (very basic) telegram bot in Scala

## HOW TO

1. Procure an API key from BotFather
2. Create a file named `.passwd` in the folder where this README is located
3. Edit the file and put the key there as follows:
```
api_token = "000000000:XXXXXXXXXX_XXXXXXXX_-XXXXXXXXXXXXXX"
```
4. Run `sbt run`

You can now communicate with the bot. This sample bot just returns back the the input wrapped in `r: Some(` `)`

## BUGS/TODO

Does not support editing messages