#rs connect 
library(rsconnect)
rsconnect::setAccountInfo(name='connect',
                          token='44555A1416207089F8DF837436975462',
                          secret='6J/s1eNLXDC4mYTuN7KR/w1aYsoT6TsqaDhH8cvX')
options(rsconnect.max.bundle.size = '80GB')
deployApp()