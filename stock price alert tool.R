# R-stock-price-alert-tool
# R script that alerts you via email when a certain stock leaves a price channel
# Call your libraries
# Email bradley.lindblad@gmail.com with suggestions, comments
libs <- c('quantmod','mailR')
lapply(libs, require, character.only=T)

# manage list of stocks, sender/receipients, and mailserve:userpwd here; you could import Excel/CSV/etc of managed lists too
list.alerts <- list(
  LIT=list(x = 'LIT', y = 33.8, z = 44.4),
  IEP=list(x = 'IEP', y = 46.8, z = 62.4),
  HRI=list(x = 'HRI', y = 55.8, z = 74.4),
  RDWR=list(x = 'RDWR', y = 17.73, z = 23.7),
  TSLA=list(x = 'TSLA', y = 290.9, z = 331.2)
  ) # new line purely for readability
mailservice <- list(user=c("<youremail@gmail.com>"), pwd=c("pwd"))
receipients <- c("<receipient@email.com>")

# Build fn to alert when a stock goes above or below a certain price
# Function is called "price_alert_fn"
price_alert_fn <- function(x = ticker, y = alert.if.price.less.than, z = alert.if.price.greater.than){
  # Although this helps for readability, I would consider not renaming to reduce lines and possible 'clutter'
  stock <- x
  less_than_target <- y
  greater_than_target <- z
  last_close <- as.numeric(tail(as.data.frame(getSymbols(stock, src = 'google', auto.assign = FALSE)),n=1)[,4])
  
  # This aspect of the function sends you an email with the mailR package if the last close satisfies one of your criteria
  # This example uses Gmail, but other services will work. Replace "youremail@gmail" with your email, obvi
    if ((last_close - greater_than_target)>0 || (last_close - less_than_target)<0){
      tgt <- ifelse((last_close - less_than_target)<0, paste('less than target ', less_than_target, sep=""), paste('greater than target ', greater_than_target, sep=""))
      send.mail(from = mailservice$user,
                to = receipients,
                subject = "Alert - Action on a Stock in Your Portfolio",
                body = paste0('"',sprintf("alert, %s met for: %s last close: %s",tgt,stock,last_close),'"'),
                smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = mailservice$user, passwd = mailservice$pwd, ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
    }
  }

#execute
lapply(list.alerts,function(x) do.call(price_alert_fn,x))
