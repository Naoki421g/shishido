#初期化
rm(list=ls())
#読み込み
df <- read.csv('final_report_exp3.csv',header=TRUE, fileEncoding = "UTF-8")
#dfの型を確認
is.data.frame(df)
#数値型
df$haito <- as.numeric(df$配当性向)
df$zyunshisan <- as.numeric(df$純資産比率)
df$shisan <- as.numeric(df$総資産)
df$rieki_r <- as.numeric(df$実績経常利益)
df$rieki_p <- as.numeric(df$予想経常利益)

hist(df$zyunshisan)
#配当
#全体傾向なので歪度尖度の標準誤差は不要
#df_haito_under20 <- df$haito[df$haito<quantile(df$haito,0.9)]
d_haito <- df$haito
m_haito <- mean(d_haito)
sd_haito <- sd(d_haito)
sk_haito <- mean((d_haito-m_haito)^3/sd_haito^3)
ku_haito <- mean((d_haito-m_haito)^4/sd_haito^4)
max_haito <- max(d_haito)
min_haito <- min(d_haito)
mid_haito <- quantile(d_haito,0.5)
print(m_haito)
print(sd_haito)
print(sk_haito)
print(ku_haito)
print(max_haito)
print(min_haito)
print(mid_haito)
hist(d_haito,xlim=c(0,300),breaks=1000,main='配当性向(300％以下)',xlab='配当性向',ylab='頻度')

#負債
#全体傾向なので歪度尖度の標準誤差は不要
#df$husai <- (1-0.01*df$zyunshisan)/0.01*df$zyunshisan
df$husai <- 1-0.01*df$zyunshisan
d_husai <- df$husai
m_husai <- mean(d_husai)
sd_husai <- sd(d_husai)
sk_husai <- mean((d_husai-m_husai)^3/sd_husai^3)
ku_husai <- mean((d_husai-m_husai)^4/sd_husai^4)
max_husai <- max(d_husai)
min_husai <- min(d_husai)
mid_husai <- quantile(d_husai,0.5)
print(m_husai)
print(sd_husai)
print(sk_husai)
print(ku_husai)
print(max_husai)
print(min_husai)
print(mid_husai)
hist(d_husai,main='負債比率',xlab='負債比率',ylab='頻度')

#上位〇％
df_husai_top20 <- df$husai[df$husai>quantile(df$husai,0.7)]
hist(df_husai_top20)

df$optimism <- (df$rieki_p - df$rieki_r)/df$shisan
hist(df$optimism,breaks=seq(min(df$optimism)-0.01,max(df$optimism)+0.01,0.01),main='optimism',xlab='optimism',ylab='頻度')
#calculate_opti
#全体傾向なので歪度尖度の標準誤差は不要
d_opti <- df$optimism
m_opti <- mean(d_opti)
sd_opti <- sd(d_opti)
sk_opti <- mean((d_opti-m_opti)^3/sd_opti^3)
ku_opti <- mean((d_opti-m_opti)^4/sd_opti^4)
max_opti <- max(d_opti)
min_opti <- min(d_opti)
mid_opti <- quantile(d_opti,0.5)
print(m_opti)
print(sd_opti)
print(sk_opti)
print(ku_opti)
print(max_opti)
print(min_opti)
print(mid_opti)

df_optimism_top20 <- df$optimism[df$optimism>quantile(df$optimism,0.8)]
hist(df_optimism_top20)

df_new <- df[df$haito<quantile(df$haito,0.4) & 
               df$husai>quantile(df$husai,0.5) & 
               df$optimism>quantile(df$optimism,0.8),]
nrow(df_new)
write.csv(df_new,'accounting_data.csv',row.names = FALSE, fileEncoding = "UTF-8")
df_new$企業名
df_new$証券コード


