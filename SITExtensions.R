
# SIT Extensions

# Prepare data
make.data.proxy <- function()
{
  load.packages('quantmod')
  raw.data = env()
  filename = 'data/TR_CC-CRB.xlt'
  # TRJ_CRB file was downloaded from the
  # http://www.corecommodityllc.com/CoreIndexes.aspx
  # select TR/CC-CRB Index-Total Return and click "See Chart"
  # on Chart page click "Download to Spreadsheet" link
  # copy TR_CC-CRB, downloaded file, to data folder
  if(file.exists(filename)) {
    temp = extract.table.from.webpage( join(readLines(filename)), 'EODValue' )
    temp = join( apply(temp, 1, join, ','), '\n' )
    raw.data$CRB = make.stock.xts( read.xts(temp, format='%m/%d/%y' ) )
  }
  filename = 'data/TB3M.Rdata'
  if(!file.exists(filename)) {
    TB3M = quantmod::getSymbols('DTB3', src='FRED', auto.assign = FALSE)
    save(TB3M, file=filename)
  }
  load(file=filename)
  TB3M[] = ifna.prev(TB3M)
  raw.data$TB3M = make.stock.xts(processTBill(TB3M, timetomaturity = 1/4, 261))
  filename = 'data/TB3Y.Rdata'
  if(!file.exists(filename)) {
    TB3Y = quantmod::getSymbols('DGS3', src='FRED', auto.assign = FALSE)
    save(TB3Y, file=filename)
  }
  load(file=filename)
  TB3Y[] = ifna.prev(TB3Y)
  raw.data$TB3Y = make.stock.xts(processTBill(TB3Y, timetomaturity = 3, 261))
  filename = 'data/TB10Y.Rdata'
  if(!file.exists(filename)) {
    TB10Y = quantmod::getSymbols('DGS10', src='FRED', auto.assign = FALSE)
    save(TB10Y, file=filename)
  }
  load(file=filename)
  TB10Y[] = ifna.prev(TB10Y)
  raw.data$TB10Y = make.stock.xts(processTBill(TB10Y, timetomaturity = 10, 261))
  filename = 'data/TB20Y.Rdata'
  if(!file.exists(filename)) {
    TB20Y = quantmod::getSymbols('GS20', src='FRED', auto.assign = FALSE)
    save(TB20Y, file=filename)
  }
  load(file=filename)
  TB20Y[] = ifna.prev(TB20Y)
  raw.data$TB20Y = make.stock.xts(processTBill(TB20Y, timetomaturity = 20, 12))
  filename = 'data/GOLD.Rdata'
  if(!file.exists(filename)) {
    GOLD = bundes.bank.data.gold()
    save(GOLD, file=filename)
  }
  load(file=filename)
  raw.data$GOLD = make.stock.xts(GOLD)
  filename = 'data/NAREIT.xls'
  if(!file.exists(filename)) {
    url = 'http://returns.reit.com/returns/MonthlyHistoricalReturns.xls'
    download.file(url, filename,  mode = 'wb')
  }
  load.packages('readxl')
  temp = read_excel(filename, sheet='Index Data', skip=7)
  i.nna <- !is.na(temp$Date)
  NAREIT = make.xts(temp$Index[i.nna], as.Date(temp$Date[i.nna]))
  raw.data$NAREIT = make.stock.xts(NAREIT)
  tickers = '
  COM = DBC;GSG + CRB
  RExUS = [RWX] + VNQ + VGSIX
  RE = [RWX] + VNQ + VGSIX
  RE.US = [ICF] + VGSIX
  EMER.EQ = [EEM] + VEIEX
  EMER.FI = [EMB] + PREMX
  GOLD = [GLD] + GOLD,
  US.CASH = [BIL] + TB3M,
  SHY + TB3Y,
  US.HY = [HYG] + VWEHX
  US.BOND = [AGG] + VBMFX
  INTL.BOND = [BWX] + BEGBX
  JAPAN.EQ = [EWJ] + FJPNX
  EUROPE.EQ = [IEV] + FIEUX
  US.SMCAP = IWM;VB + NAESX
  TECH.EQ = [QQQ] + ^NDX
  US.EQ = [VTI] + VTSMX + VFINX
  US.MID = [VO] + VIMSX
  EAFE = [EFA] + VDVIX + VGTSX
  MID.TR = [IEF] + VFITX
  CORP.FI = [LQD] + VWESX
  TIPS = [TIP] + VIPSX + LSGSX
  LONG.TR = [TLT] + VUSTX
  '
  # EAFE = [EFA] + VDMIX + VGTSX

  data.proxy = env()
  getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data.proxy, raw.data = raw.data, auto.assign = T)
  data.proxy.raw = raw.data
  save(data.proxy.raw, file='data/data.proxy.raw.Rdata',compress='gzip')
  save(data.proxy, file='data/data.proxy.Rdata',compress='gzip')
}

# Check data range (xts object)
f.dataRange <- function(x)
{
  stopifnot(is.xts(x))

  range(index(x))
}


bt.end.dates <- function (b, c.colNames="End")
{
  temp = lapply(b, function(x) index(last(x)))
  temp$dates = NULL
  temp$prices = NULL
  temp$weight = NULL
  temp$execution.price = NULL
  temp$symbolnames = NULL
  temp = temp[order(sapply(temp, function(x) x))]
  out = t(t(sapply(temp, function(x) as.character(x))))
  colnames(out) = c.colNames
  out
}

bt.start.dates <- function (b, c.colNames="Start")
{
  temp = lapply(b, function(x) index(x[1]))
  temp$dates = NULL
  temp$prices = NULL
  temp$weight = NULL
  temp$execution.price = NULL
  temp$symbolnames = NULL
  temp = temp[order(sapply(temp, function(x) x))]
  out = t(t(sapply(temp, function(x) as.character(x))))
  colnames(out) = c.colNames
  out
}


plotbt.strategy.sidebyside <-
function (..., perfromance.metric = spl("System,Trade,Period"),
          perfromance.fn = "bt.detail.summary", return.table = FALSE,
          make.plot = TRUE)
{
  models = variable.number.arguments(...)
  out = list()
  for (i in 1:len(models)) {
    temp <- match.fun(perfromance.fn)(models[[i]])[perfromance.metric]
    c.temp_names <- names( temp )
    c.n <- length( c.temp_names )
    if ( c.n > 1)
    {
      for (c.i in 2:c.n)
        temp[[1]] <- c(temp[[1]], temp[[c.i]])
      temp <- temp[1]
    }
    out[names(models)[i]] = temp
  }
  temp = list2matrix(out, keep.names = F)
  if (make.plot)
    plot.table(temp, smain = perfromance.metric[1])
  if (return.table)
    return(temp)
}


bt.apply.matrix <-
function (b, xfun = Cl, ...) 
{
  out = b
  out[] = NA
  nsymbols = ncol(b)
  xfun = match.fun(xfun)
  for (i in 1:nsymbols) {
    temp.data <- coredata(b[, i])
    if (!all(is.na(temp.data)))
    {
      msg = try(xfun(temp.data, ...), silent = TRUE)
      if (class(msg)[1] == "try-error") 
        warning(i, msg, "\n")
      else out[, i] = msg
    }
  }
  return(out)
}

getSymbols.extra<-
function (Symbols = NULL, env = parent.frame(), getSymbols.fn = getSymbols, 
          raw.data = new.env(), set.symbolnames = F, auto.assign = T, 
          try.extend = T, ...) 
{
  Symbols = parse.expr(Symbols)
  if (len(Symbols) < 1) 
    return(Symbols)
  map = map.symbols(Symbols)
  Symbols = unique(unlist(map))
  Symbols = setdiff(Symbols, ls(raw.data, all.names = T))
  data = new.env()
  if (len(Symbols) > 0) 
    match.fun(getSymbols.fn)(Symbols, env = data, auto.assign = T, 
                             ...)
  for (n in ls(raw.data, all.names = T)) data[[n]] = raw.data[[n]]
  print ("getSymbols.extra:")
  for (n in ls(data, all.names = T))
  {
    temp_ <- range(index(data[[n]]))
    print ( paste0(" Symbol ", n, ": ", paste0(rep(" ", 15 - nchar(n)), collapse=""), paste0(temp_, collapse=" : ") ) )
  }
  if (set.symbolnames) 
    env$symbolnames = names(map)
  for (s in names(map)) {
    env[[s]] = data[[gsub("\\^", "", map[[s]][1])]]
    if (try.extend) 
      if (len(map[[s]]) > 1) 
        for (i in 2:len(map[[s]])) if (is.null(data[[gsub("\\^", 
                                                          "", map[[s]][i])]])) 
          cat("Not Downloaded, main =", s, "missing", 
              gsub("\\^", "", map[[s]][i]), "\n", sep = "\t")
    else env[[s]] = extend.data(env[[s]], data[[gsub("\\^", 
                                                     "", map[[s]][i])]], scale = T)
    if (!auto.assign) 
      return(env[[s]])
  }
}


extend.data <-
function (current, hist, scale = F, echo.off=TRUE) 
{
  colnames(current) = sapply(colnames(current), function(x) last(spl(x, 
                                                                     "\\.")))
  colnames(hist) = sapply(colnames(hist), function(x) last(spl(x, 
                                                               "\\.")))
  close.index = find.names("Close", hist)
  if (len(close.index) == 0) 
    close.index = 1
  adjusted.index = find.names("Adjusted", hist)
  if (len(adjusted.index) == 0) 
    adjusted.index = close.index
  if (scale) {
    cur.close.index = find.names("Close", current)
    if (len(cur.close.index) == 0) 
      cur.close.index = 1
    cur.adjusted.index = find.names("Adjusted", current)
    if (len(cur.adjusted.index) == 0) 
      cur.adjusted.index = cur.close.index
    common = merge(current[, cur.close.index], hist[, close.index], 
                   join = "inner")
    scale = as.numeric(common[1, 1])/as.numeric(common[1, 
                                                       2])
    scale <- round (scale, 12)
    if (!echo.off) print (paste("Scale after rounding: ", scale))
      
    if (close.index == adjusted.index) 
      hist = hist * scale
    else {
      hist[, -adjusted.index] = hist[, -adjusted.index] * 
        scale
      common = merge(current[, cur.adjusted.index], hist[, 
                                                         adjusted.index], join = "inner")
      scale = as.numeric(common[1, 1])/as.numeric(common[1, 
                                                         2])
      hist[, adjusted.index] = hist[, adjusted.index] * 
        scale
    }
  }
  hist = hist[format(index(current[1]) - 1, "::%Y:%m:%d"), 
              , drop = F]
  if (ncol(hist) != ncol(current)) 
    hist = rep.col(hist[, adjusted.index], ncol(current))
  else hist = hist[, colnames(current)]
  colnames(hist) = colnames(current)
  rbind(hist, current)
}


f.extXTS <- function(d.x, c.n = 3, c.r = 1)
{
  d.l <- last(d.x)
  res <- xts(rep.row (d.l, c.n), seq(index(d.l), by="day", length.out = c.n))
  
  i.working <- format(index(res), "%w") %in% c("1", "2", "3", "4", "5")
  head(res[i.working, ], c.r)
}


f.get.G10 <- function (type = spl("currency"), c.from = "1970-01-01") 
{
  if (type[1] != "currency") {
    cat("Warning:", type[1], "is not yet implemented in getG10 function\n")
    return()
  }
  map = "\nFX          FX.NAME        \nDEXUSAL     U.S./Australia \nDEXUSUK     U.S./U.K.      \nDEXCAUS     Canada/U.S.    \nDEXNOUS     Norway/U.S.    \nDEXUSEU     U.S./Euro      \nDEXJPUS     Japan/U.S.     \nDEXUSNZ     U.S./NewZealand\nDEXSDUS     Sweden/U.S.    \nDEXSZUS     Switzerland/U.S.\n"
  map = matrix(scan(text = map, what = "", quiet = T), nc = 2, 
               byrow = T)
  colnames(map) = map[1, ]
  map = data.frame(map[-1, ], stringsAsFactors = F)
  convert.index = grep("DEXUS", map$FX, value = T)
  load.packages("quantmod")
  data.fx <- new.env()
  quantmod::getSymbols(map$FX, src = "FRED", from = c.from, 
                       env = data.fx, auto.assign = T)
  for (i in ls(data.fx)) data.fx[[i]] = na.omit(data.fx[[i]])
  for (i in convert.index) data.fx[[i]] = 1/data.fx[[i]]
  for (i in ls(data.fx)) data.fx[[i]] = data.fx[[i]][paste0(c.from, "/")]
  
  bt.prep(data.fx, align = "remove.na")
  
  fx = bt.apply(data.fx, "[")
  return(fx)
}
