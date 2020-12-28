# flowtranz 2020


# source("~/R/flowtrans2.r" )

require( stringr )
require( corrplot )
require( reshape2 )
require( ggplot2 )
require( gdata )
require( ggpubr )

require( stringr )
require( readxl )

ft.data.dir = "/mnt/store0/data/flowtrans/data/"
ft.xls.dir  = "/mnt/store0/data/flowtrans/data/xls/"
ft.date    = "20201214"

ft.stat.dir = paste0( "/mnt/store0/data/flowtrans/stat-", ft.date, "/" )


## contingency

ft.cp.fig12 = function( X, main, TYPE=FALSE, ischemic = FALSE, save = FALSE )
{
	if ( ischemic )
	{
		if ( TYPE )
		{
			XT = table( X$ROI, X$TYPE )
		}
		else
		{
			XT = table( X$ROI, X$type )
		}
		
	}
	else
	{
		if ( TYPE )
		{
			XT = table( X$roi, X$TYPE )
		}
		else
		{
			XT = table( X$roi, X$type )
		}
		
	}
	
	chisq = chisq.test( XT )

	contrib <- 100*chisq$residuals^2/chisq$statistic
	
	
	CR = chisq$residuals
	
	i = which( is.na(CR ))
	if ( length(i) > 0 ) CR[i] = 0
	
	par(pin=c(8,8))              ##  (width, height) in inches    
	par(omi=c(0.5,.5,0.5,0.5))        ## (bottom, left, top, right)  in inches  
	par(xpd=TRUE)
	corrplot(CR, tl.srt = 0, tl.cex = 1.2, is.cor = FALSE, main="\n"	,
				tl.col = "black", tl.offset=1, mar = c(2, 0, 1, 0), cl.ratio = 0.3, cl.align = "l", cl.offset=0.5) #cl.pos = "n")

# 	corrplot(contrib, tl.srt = 0, tl.cex = 1.5, is.cor = FALSE, main=""	,
# 				tl.col = "black", tl.offset=1 mar = c(2, 0, 1, 0))
				
				
				
	if ( save )
	{
                dir.create( ft.pdf, showWarnings = FALSE)
	
		dev.copy2pdf( device=x11, file= paste0(ft.pdf, "contingency-", main, ".pdf") )
	}

	return( chisq )
}






## descriptiv & wilcox csv i/o
ft.write.stat.csv = function( X, file.name.start, stat = "descr" )
{
	suppressWarnings( dir.create( ft.stat.dir ) )

	file.name    = paste0( ft.stat.dir, file.name.start, "-", stat, ".csv")
	file.name.hu = paste0( ft.stat.dir, file.name.start, "-", stat, "_hu.csv")

	write.csv  ( X, file.name )
	write.table( X, file.name.hu, sep=";", dec="," )
}

ft.read.csv = function( data )
{
	q = read.csv( paste0(ft.data.dir, toupper(data), "-", ft.date, ".csv" ) )
	return(q)
}

ft.write.csv = function( DF, data )
{
	write.csv( DF, paste0(ft.data.dir, toupper(data), "-", ft.date, ".csv"), row.names=FALSE ) 
}


ft.read.all.xls = function(data = "cbf")
{
	F = dir(path=ft.xls.dir, pattern=glob2rx("*.xlsx"), full.names = TRUE)

	X = NULL
	for ( f in F )
	{
		id = str_split_fixed( basename(f), "\\.", 2 )[,1]
		message( "read ", f, "(", data, ")")
		x = ft.read.xls( f, data)

		if ( !is.null(x) )
		{
			x$id = id
			x = x[, c( ncol(x), 1: (ncol(x)-1))]
			X = rbind(X,x)
		}
	}


 	Y = data.frame( id = X$id, roi = X$roi, code = X$code, type = X$type )

   	Y$ROI = "ISCHEMIC"
    i = which( as.numeric( substr( X$roi,4,4)) > 6 )
    if ( length(i) > 0 ) Y$ROI[ i ]  = "NON-ISCHEMIC"
	Y$ROI  = factor( Y$ROI )


	
	Y$TYPE = "I"
	i = which( X$type == 2 | X$type == 3 )
	if ( length(i) > 0 ) Y$TYPE[ i ] = "II&III"
	
	i = which( X$type > 3 )
	if ( length(i) > 0 ) Y$TYPE[ i ] = "IV&V"
	
	Y$TYPE = factor( Y$TYPE )
	
	
	i = which( X$type == 1 )
	if ( length(i) > 0 ) Y$type[ i ] = "I"

	i = which( X$type == 2 )
	if ( length(i) > 0 ) Y$type[ i ] = "II"

	i = which( X$type == 3 )
	if ( length(i) > 0 ) Y$type[ i ] = "III"
	
	i = which( X$type == 4 )
	if ( length(i) > 0 ) Y$type[ i ] = "IV"

	i = which( X$type == 5 )
	if ( length(i) > 0 ) Y$type[ i ] = "V"
	
	
 	Y$type = factor( Y$type )
	

    Y$NAmp = X$NAmp
    Y$PAmp = X$PAmp
    Y$TAmp = X$TAmp
    Y$Duration = X$length
	Y$OT   = X$OT
	Y$baseline = X$baseline
    Y$OTC  = X$OTC
    Y$NSlope = NA # X$TAmp / ( X$NPC - X$OTC )
    Y$PSlope = NA # X$TAmp / ( X$PPC - X$OTC )


	i = which( Y$type == "IV" | Y$type == "V" )
		Y$NSlope[i] =  X$TAmp[i] / ( X$NPC[i] - X$OTC[i] )

	i = which( Y$type == "I" )
		Y$PSlope[i] =  X$TAmp[i] / ( X$PPC[i] - X$OTC[i] )

	i = which( Y$type == "II" | Y$type == "III" )
	{
		Y$PSlope[i] =  X$PAmp[i] / ( X$PPC[i] - X$NPC[i] )
		Y$NSlope[i] =  X$NAmp[i] / ( X$NPC[i] - X$OTC[i] )
	}


	return(Y)
}

ft.read.xls = function( file, data )
{    
    cbf = 1
    
    s = cbf
    if ( data == "cbf" ) s = cbf
    if ( data == "oxy" ) s = cbf+1
    if ( data == "deoxy" ) s = cbf+2
    
    #d = as.matrix( read.xlsx( file, sheetIndex=s, startRow=1, stringsAsFactors=FALSE ) )
    d = as.matrix(  suppressMessages( read_excel( file, sheet=s ) ) )

	if (  ncol(d) > 1 )
	{
		f = d[,1]
		m = t(d[,-1])
		x = as.data.frame( m)
		colnames(x) = f
		x$roi = str_split_fixed( rownames(x), "\\.", 2)[,1]
		rownames(x) = NULL
		x = x[, c( ncol(x), 1: (ncol(x)-1))]

		colnames(x) = c( 'roi', 'code', 'OT', 'baseline', 'NAmp', 'PAmp', 'TAmp', 'type', 'length', 'NP', 'PP', 'OTC', 'NPC', 'PPC' )


		for ( i in 2:ncol(x))
		{
			x[, i] = as.numeric(as.character(x[,i]))
		}


		x$rNPT = x$NPC - x$OTC
		x$rPPT = x$PPC - x$OTC
		x$dNPT = x$PPC - x$NPC
			
		x$dAmp = x$PAmp - x$NAmp
		x$gNP  = x$NAmp / x$rNPT
		x$gPP1 = x$dAmp / x$dNPT
		x$gPP2 = x$PAmp / x$rPPT
			
			
		
	
		attr(x, "data") = data    
		return(x)
	}
	return(NULL)
}



#################################################################xxxxx
# OLD 
# D = ft.read.files( "*-deoxy.csv" )
# O = ft.read.files( "*-oxy.csv" )
# C = ft.read.files( "*-cbf.csv" )


## OLD
ft.read.files2.old = function( pat )
{
    X = ft.read.files( pat )
	X = X[!is.na(X$type ), ]

    Y = data.frame( id = X$id, roi = X$roi, type = X$type, ROI = X$ROI, TYPE = X$TYPE )

    Y$NAmp = X$NAmp
    Y$PAmp = X$PAmp
    Y$TAmp = X$TAmp
    Y$Duration = X$Length
    Y$OTC  = X$OTC
    Y$NSlope = NA # X$TAmp / ( X$NPC - X$OTC )
    Y$PSlope = NA # X$TAmp / ( X$PPC - X$OTC )


	Y$NSlope[  Y$type == "IV" | Y$type == "V"  ] =  X$TAmp / ( X$NPC - X$OTC )
	Y$PSlope[  Y$type == "I"                   ] =  X$TAmp / ( X$PPC - X$OTC )
	Y$PSlope[  Y$type == "II" | Y$type == "III"] =  X$TAmp / ( X$PPC - X$NPC )


    return(Y)
}


ft.read.files.old = function( pat )
{
	DF = data.frame()

	F =dir( ft.csv.dir, pattern=pat, full.names = TRUE  )

	for ( f in F )
	{
		#cat( "read ", f, "\n" )
		df = ft.read.csv(f)
		
		df$id = paste0( "JL", str_split_fixed( f, "_" , 4 )[,3] )
		
		DF = rbind( DF, df )
	}

	return(DF)
}


ft.read.csv.old = function( file )
{
	q = read.csv( file, header=FALSE, stringsAsFactors=FALSE )
	M = toupper(as.matrix(q))
	
	
	roi1 = which( M == "ROI1", arr.ind=TRUE )	
	
	DF = data.frame()
	
	for ( s in 1:nrow(roi1) )
	{
		# roi1-3
		r1 = roi1[s,1]
		c1 = roi1[s,2]
		
		cmax = ifelse( s == nrow(roi1), ncol(M), roi1[s+1,2] )
		
		r2 = r1
		c2 = which( M[r2,] == "ROI2", arr.ind=TRUE )[1]
		
		r3 = r1
		c3 = which( M[r3,] == "ROI3", arr.ind=TRUE )[1]
		
		# roi 4-6
		r4 = which( M[,c1] == "ROI4", arr.ind=TRUE )[1]
		c4 = c1

		r5 = which( M[,c2] == "ROI5", arr.ind=TRUE )[1]
		c5 = c2
		
		r6 = which( M[,c3] == "ROI6", arr.ind=TRUE )[1]
		c6 = c3
		
		# roi 7-9
		r7 = which( M[,c1] == "ROI7", arr.ind=TRUE )[1]
		c7 = c1

		r8 = which( M[,c2] == "ROI8", arr.ind=TRUE )[1]
		c8 = c2
		
		r9 = which( M[,c3] == "ROI9", arr.ind=TRUE )[1]
		c9 = c3
		
		
		df = ft.get.peak.data( M, s, r1, c1, c2 )
		DF = rbind( DF, df )
		
		df = ft.get.peak.data( M, s, r2, c2, c3 )
		DF = rbind( DF, df )
		
		df = ft.get.peak.data( M, s, r3, c3, cmax )
		DF = rbind( DF, df )
		
		
		df = ft.get.peak.data( M, s, r4, c4, c5 )
		DF = rbind( DF, df )
		
		df = ft.get.peak.data( M, s, r5, c5, c6 )
		DF = rbind( DF, df )
		
		df = ft.get.peak.data(M, s,  r6, c6, cmax )
		DF = rbind( DF, df )
		

		df = ft.get.peak.data( M, s, r7, c7, c8 )
		DF = rbind( DF, df )
		
		df = ft.get.peak.data( M, s, r8, c8, c9 )
		DF = rbind( DF, df )
		
		df = ft.get.peak.data( M, s, r9, c9, cmax )
		DF = rbind( DF, df )
	}

	
	
	DF$roi  = factor( DF$roi, levels=sprintf("ROI%d",1:9 ))
	
    # ROI 7,8,9 (non-ischemic) versus ROI (1,2,3,4,5,6) között is.
    
    DF$ROI = "ISCHEMIC"
    
    i = which( as.numeric( substr( DF$roi,4,4)) > 6 )
    if ( length(i) > 0 ) DF$ROI[ i ]  = "NON-ISCHEMIC"
	
	# I., típus 1 (monofázisos emelkedés), II., típus 2 és 3 (bifázisos), III., típus 4-5 (monofázisos csökkenés).
	

	i = which( DF$type > 5 )
	if ( length(i) > 0 ) DF$type[ i ] = NA

	
	DF$TYPE = "I"

	
	
	
	i = which( DF$type == 2 | DF$type == 3 )
	if ( length(i) > 0 ) DF$TYPE[ i ] = "II&III"
	
	i = which( DF$type > 3 )
	if ( length(i) > 0 ) DF$TYPE[ i ] = "IV&V"
	
	DF$ROI  = factor( DF$ROI )
	DF$TYPE = factor( DF$TYPE )
	
	
	i = which( DF$type == 1 )
	if ( length(i) > 0 ) DF$type[ i ] = "I"

	i = which( DF$type == 2 )
	if ( length(i) > 0 ) DF$type[ i ] = "II"

	i = which( DF$type == 3 )
	if ( length(i) > 0 ) DF$type[ i ] = "III"
	
	i = which( DF$type == 4 )
	if ( length(i) > 0 ) DF$type[ i ] = "IV"

	i = which( DF$type == 5 )
	if ( length(i) > 0 ) DF$type[ i ] = "V"
	
	
 	DF$type = factor( DF$type )
	
	
	
	
	return(DF)
}



ft.get.peak.data = function( M, s, r, c, c.max )
{
	seq = sprintf( "seq%02d", s )
	roi = M[r,c]
	DF = data.frame()

	repeat {
	
		time = as.numeric( M[r+1,c])
		
		if ( is.na(time) ) { break }
		
		df = data.frame (
			roi  = roi,
			type     = as.numeric( M[r+7,c+1]),
			OT       = as.numeric( M[r+2,c+1]),
			baseline = as.numeric( M[r+3,c+1]),
			NAmp      = as.numeric( M[r+4,c+1]),
			PAmp      = as.numeric( M[r+5,c+1]),
			TAmp      = as.numeric( M[r+6,c+1]),
			Length   = as.numeric( M[r+8,c+1]),
# 			NP       = as.numeric( M[r+9,c+1]),
# 			PP       = as.numeric( M[r+10,c+1]),
			OTC      = as.numeric( M[r+11,c+1]),
			NPC      = as.numeric( M[r+12,c+1]),
			PPC      = as.numeric( M[r+13,c+1])
			)
			
			
			df$rNPT = df$NPC - df$OTC
			df$rPPT = df$PPC - df$OTC
			df$dNPT = df$PPC - df$NPC
			
			df$dAmp = df$PAmp - df$NAmp
			df$gNP  = df$NAmp / df$rNPT
			df$gPP1 = df$dAmp / df$dNPT
			df$gPP2 = df$PAmp / df$rPPT
			
			
			
			
			DF = rbind(DF,df)
	
			c = c + 2
	}
			
	
	return( DF )		
		
		
}
