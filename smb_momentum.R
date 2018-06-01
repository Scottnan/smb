# utils
next_dates <- function(dates, lags) {

  date_seq <- sort(unique(dates))
  dt_dates <- data.table(DATE = date_seq,
                         NXT_DATE = shift(date_seq, n = lags, type = "lead"),
                         key = "DATE")
  dates_to <- dt_dates[J(dates), NXT_DATE]
}



# 从历史收益率的角度进行大中小盘的配置
library(GCAMCQT)
library(data.table)

from_to <- c(20070101, 20180525)
adjust_from_to <- from_to_adjusting(as_from_to(from_to), 12 * 2)
idx <- read_gildata("qt_indexquote", from_to = adjust_from_to)$qt_indexquote

# 沪深300 - 3145
# 中证500 - 4978
# 中证1000 - 39144
smb_idx <- idx[INNERCODE %in% c(3145, 4978, 39144), .(INNERCODE, TRADINGDAY, CLOSEPRICE)]
smb_idx[, RTN := CLOSEPRICE / shift(CLOSEPRICE) - 1, by = INNERCODE]
smb_bmk <- smb_idx[, .(RTN = mean(RTN)), by = TRADINGDAY]

# 依据动量进行权重配比
# 初始资产权重等权，运用长中短期理论进行权重配置

# 进行周度，月度，3M的收益率统计
adjust_week_dates <- factor_dates(adjust_from_to, freq = "weekly")
week_dates <- factor_dates(from_to, freq = "weekly")
smb_idx <- smb_idx[TRADINGDAY %in% adjust_week_dates]
smb_idx[, `:=` (NXT_WEEK_DATE = next_dates(TRADINGDAY, 1L),
                NXT_MONTH_DATE = next_dates(TRADINGDAY, 4L),
                NXT_QUARTER_DATE = next_dates(TRADINGDAY, 12L))]
smb_idx[, `:=` (NXT_WEEK_RTN = ashare_rtns(INNERCODE, TRADINGDAY, NXT_WEEK_DATE)$RTN,
                NXT_MONTH_RTN = ashare_rtns(INNERCODE, TRADINGDAY, NXT_MONTH_DATE)$RTN,
                NXT_QUARTER_RTN = ashare_rtns(INNERCODE, TRADINGDAY, NXT_QUARTER_DATE)$RTN)]

# term dates
short_term_dates <- purrr::map(setNames(week_dates, week_dates), function(x) {
  dates <- as.Date(sort(union(week_dates, adjust_week_dates)), origin="1970-01-01")
  pos <- which(dates == x)
  res <- dates[max(1L, (pos - 3 * 52)) : (pos - 1L)]
})
mid_term_dates <- purrr::map(setNames(week_dates, week_dates), function(x) {
  dates <- as.Date(sort(union(week_dates, adjust_week_dates)), origin="1970-01-01")
  pos <- which(dates == x)
  res <- dates[max(1L, (pos - 3 * 52)) : (pos - 4L)]
})
long_term_dates <- purrr::map(setNames(week_dates, week_dates), function(x) {
  dates <- as.Date(sort(union(week_dates, adjust_week_dates)), origin="1970-01-01")
  pos <- which(dates == x)
  res <- dates[max(1L, (pos - 3 * 52)) : (pos - 12L)]
})

# get_rtn
get_short_quantile <- function(code, date) {
  short_term_rtns <- smb_idx[NXT_MONTH_DATE %in% c(short_term_dates[[date]] , as.Date(date)) & INNERCODE == code, NXT_WEEK_RTN]
  short_quantile <- (2 * rank(short_term_rtns) / length(short_term_rtns) - 1)[length(short_term_rtns)]
  return(short_quantile)
}
get_mid_quantile <- function(code, date) {
  mid_term_rtns <- smb_idx[NXT_MONTH_DATE %in% c(mid_term_dates[[date]], as.Date(date)) & INNERCODE == code, NXT_MONTH_RTN]
  mid_quantile <- (2 * rank(mid_term_rtns) / length(mid_term_rtns) - 1)[length(mid_term_rtns)]
  return(mid_quantile)
}
get_long_quantile <- function(code, date) {
  long_term_rtns <- smb_idx[NXT_MONTH_DATE %in% c(long_term_dates[[date]], as.Date(date)) & INNERCODE == code, NXT_QUARTER_RTN]
  long_quantile <- (2 * rank(long_term_rtns) / length(long_term_rtns) - 1)[length(long_term_rtns)]
  return(long_quantile)
}
get_rtn <- function(code, date) {
  short <- unlist(purrr::map2(code, as.character(date), get_short_quantile))
  mid <- unlist(purrr::map2(code, as.character(date), get_mid_quantile))
  long <- unlist(purrr::map2(code, as.character(date), get_long_quantile))
  return(data.table(short, mid, long))
}

# get weight table
imbanlance_ratio <- 2
weight_tbl <- rbind(data.table(INNERCODE = 3145, DATE = week_dates),
                    data.table(INNERCODE = 4978, DATE = week_dates),
                    data.table(INNERCODE = 39144, DATE = week_dates))
weight_tbl[, c("SHORT", "MID", "LONG") := get_rtn(INNERCODE, DATE)]
weight_tbl[, "MEAN" := rowMeans(weight_tbl[, .(SHORT, MID, LONG)])]
weight_tbl[, c("SHORT", "MID", "LONG") := NULL]
weight_tbl <- dcast(weight_tbl, DATE~INNERCODE)
setnames(weight_tbl, c(as.character(3145), as.character(4978), as.character(39144)), c("BIG", "MID", "SMALL"))
weight_tbl[, `:=` (BIG = (1 +  BIG) / 3,
                   MID = (1 +  MID) / 3,
                   SMALL = (1 +  SMALL) / 3)]
weight_tbl[, `:=` (BIG = BIG / (BIG + MID + SMALL),
                   MID = MID / (BIG + MID + SMALL),
                   SMALL = SMALL / (BIG + MID + SMALL))]
weight_tbl[, `:=` (BIG = imbanlance_ratio * (BIG - 1/3) + 1/3,
                   MID = imbanlance_ratio * (MID - 1/3) + 1/3,
                   SMALL = imbanlance_ratio * (SMALL - 1/3) + 1/3)]
weight_tbl[BIG<0, BIG := 0]
weight_tbl[MID<0, MID := 0]
weight_tbl[SMALL<0, SMALL := 0]
weight_tbl[, `:=` (BIG = BIG / (BIG + MID + SMALL),
                   MID = MID / (BIG + MID + SMALL),
                   SMALL = SMALL / (BIG + MID + SMALL))]
# cal return
rtn <- smb_idx[TRADINGDAY %in% week_dates, .(INNERCODE, TRADINGDAY, RTN)]
rtn <- dcast(rtn, TRADINGDAY~INNERCODE)
setnames(rtn, c(as.character(3145), as.character(4978), as.character(39144)),
         c("BIG_RTN", "MID_RTN", "SMALL_RTN"))
rtn_tbl <- merge(weight_tbl, rtn, by.x="DATE", by.y="TRADINGDAY")
rtn_tbl[, "RTN" := BIG * BIG_RTN + MID * MID_RTN + SMALL * SMALL_RTN]
tmp <- merge(rtn_tbl, smb_bmk, by.x="DATE", by.y="TRADINGDAY")
writexl::write_xlsx(tmp, "a.xlsx")
