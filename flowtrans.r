# flow transfer

# source("~/R/flowtrans.r" )

require( stringr )
require( corrplot )
require( reshape2 )
require( ggplot2 )
require( gdata )
 
 
ft.pdf   = "pdf-20180907/"
ft.curve = "curve-pdf-20180915/"
ft.curve.file = "curves-for-presentation-9rat-4seq.xls"

##curves


ft.curves.1 = function(  )
{
	s.names = c("CBF",   "OXY",   "DEOXY") #sheetNames( ft.curve.file )

	C = read.xls( ft.curve.file, sheet=1 ) / 1e6
	O = read.xls( ft.curve.file, sheet=2 ) / 1e8
	D = read.xls( ft.curve.file, sheet=3 ) / 1e8

# 	C = C - mean(as.matrix(C))
# 	O = O - mean(as.matrix(O))
# 	D = D - mean(as.matrix(D))
	
	range = c(-4,5 )
	par( mfrow=c(3,3) )
	par(mar=rep(0.5,4))
	for( roi in 1:9 )
	{
		cC = C[,roi]
		cO = O[,roi]
		cD = D[,roi]

		cC = cC - mean(cC)
		cD = cD - mean(cD)
		cO = cO - mean(cO)
		
		tp = 2*(1:length(cC))
		
 		plot ( tp, cO, type="l" , xlab="", ylab="", col= "darkgreen", ylim=range, lwd=1.5 , xaxt="n", yaxt="n")
		lines( tp, cD, type="l" , xlab="", ylab="", col= "deepskyblue3", ylim=range, lwd=1.5, xaxt="n", xaxt="n" )
		lines( tp, cC, type="l" , xlab="", ylab="", col= "firebrick3", ylim=range, lwd=1.5, xaxt="n", xaxt="n" )
	
	}

	dev.copy2pdf( device=x11, file= paste0(ft.curve, "allcurves.pdf"), width=19, height=8 )
	
}




ft.curves.0 = function( sht )
{
	curve.file = "curves-for-presentation-9rat-4seq.xls"
	
	s.names = sheetNames( "curves-for-presentation-9rat-4seq.xls" )

    dir.create( ft.curve, showWarnings = FALSE)

	q = read.xls( "curves-for-presentation-9rat-4seq.xls", sheet=sht )
	
	for( roi in 1:9 )
	{
		plot( q[,roi], type="l" , xlab="", ylab="")
	
	
		dev.copy2pdf( device=x11, file= paste0(ft.curve, "curve-", s.names[sht], "-", colnames(q)[roi], ".pdf"), width=8, height=4 )

		
	
	}

	
	
	
	
}




## figures for presentation 2018.09.05.


ft.stats = function()
{
    D = ft.read.files( "*-deoxy.csv" )
    O = ft.read.files( "*-oxy.csv" )
    C = ft.read.files( "*-cbf.csv" )

	df = ft.stat( D, "deoxy" )
	df = rbind( df, ft.stat( O, "oxy" ) )
	df = rbind( df, ft.stat( C, "cbf" ) )
	
	return( df )
}


ft.stat = function(X, table)
{
	sa = as.numeric(summary( X$TAmp ))
	ka = kruskal.test( TAmp~ROI, X )

	sd = as.numeric(summary( X$Length )	)
	kd = kruskal.test( Length~ROI, X )
	
	
	r = ft.stat.row( table, "total amplitude", sa ,ka )
	r = rbind( r, ft.stat.row( table, "duration", sd ,kd ))
	
	return(r)
}


ft.stat.row =function( table, param, s ,k )
{
	df = data.frame( data = table, 
					param = param,
					min = s[1], max = s[6], mean=s[4], median=s[3], p = k$p.value )
	return(df)
}



ft.jit.figures = function()
{
    D = ft.read.files( "*-deoxy.csv" )
    O = ft.read.files( "*-oxy.csv" )
    C = ft.read.files( "*-cbf.csv" )

    ft.jitter( D, "jitter-deoxy.pdf" )
    ft.jitter( O, "jitter-oxy.pdf" )
    ft.jitter( C, "jitter-cbf.pdf" )

}


ft.jitter = function( X , pdf.file = NULL )
{

    cx = colnames(X)
    
#     i = na.omit(match( c( "roi", "type", "NAmp", "PAmp", "TAmp","Length","OTC", "gPP2", "gNP", "dAmp" ),cx))
    i = na.omit(match( c( "roi", "type", "NAmp", "PAmp", "TAmp","Length","OTC", "gPP2" ),cx))
    
    X= X[,i]
    
    M = melt(X)

    par_names <- c(
                    `NAmp` = "negative amplitude [ uM ]",
                    `PAmp` = "positive amplitude [ uM ]",
                    `TAmp` = "total amplitude [ uM ]",
                    `Length` = "duration [ frame ]",
                    `OTC` = "onset time [ frame ]",
                    `gPP2` = "positive amplitude gradient [ uM/frame ]"
                    )
    
    if ( !is.null( pdf.file ) )
    {
        if ( str_detect( pdf.file, "cbf") )
        {
            x = str_replace( par_names, "uM", "%CBF" )
            names(x) = names(par_names)
            par_names = x
        }
    }
    
    p = ggplot( M,aes(roi,value,col=type))
    p = p + geom_jitter( size=1, alpha=.5, width=.2) + facet_wrap(~variable, scales="free",  labeller = as_labeller(par_names))
    p = p + theme( axis.text  = element_text(face = "bold", color = "gray30", size = 8) )
    p = p + theme( axis.title = element_blank() ) #face = "bold", color = "black", size = 12) )
    
    
    p = p + theme( legend.text  = element_text(face = "bold", color = "black", size = 8) )
    p = p + theme( legend.title = element_text(face = "bold", color = "black", size = 8) )

    p = p + theme( strip.text = element_text(face = "bold", color = "black", size = 10) )
    
#     p = p + theme( strip.text = element_blank() )
    print(p)

    
    if ( !is.null( pdf.file ) )
    {
        ggsave( paste0(ft.pdf, pdf.file), width=12, height=8 )
    }

    
}






ft.cp.figures = function()
{
	D = ft.read.files( "*-deoxy.csv" )
	O = ft.read.files( "*-oxy.csv" )
	C = ft.read.files( "*-cbf.csv" )
	
	ft.cp.figures.from.df( D, "DEOXY" )
	ft.cp.figures.from.df( O, "OXY" )
	ft.cp.figures.from.df( C, "CBF" )
	
}


ft.cp.figures.from.df = function( X,main )
{
	ft.cp.fig12( X, paste0(main, "-1"), ischemic=FALSE, TYPE=FALSE , save=TRUE)
	ft.cp.fig12( X, paste0(main, "-2"), ischemic=FALSE, TYPE=TRUE  , save=TRUE)
	ft.cp.fig12( X, paste0(main, "-3"), ischemic=TRUE,  TYPE=FALSE , save=TRUE)
	ft.cp.fig12( X, paste0(main, "-4"), ischemic=TRUE,  TYPE=TRUE  , save=TRUE)
}

ft.cp.fig12 = function( X, main, TYPE=false, ischemic = FALSE, save = FALSE )
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



## first trials .....
ft.baseline.stat = function(X, c)
{
	par = colnames(X)[c]
	i = which( is.na( X$baseline ) )
	
	a = aggregate( X$baseline[-i], list(X[-i,c]), mean ) 
	b = aggregate( X$baseline[-i], list(X[-i,c]), sd) 
	c = aggregate( X$baseline[-i], list(X[-i,c]), length) 


	DF = data.frame( ROI = a$Group.1, mean = a$x, sd=b$x, count = c$x )
	colnames(DF)[1] = par
	
	return(DF)
}


ft.contstat = function( X, main, save = FALSE )
{
	XT = table( X$ROI, X$TYPE )
	
	chisq = chisq.test( XT )

	
	corrplot(chisq$residuals, tl.srt = 0, tl.cex = 1.5, is.cor = FALSE, main=paste("\n\n", main, "contingency") )
	
	if ( save )
	{
		dev.copy2pdf( device=x11, file= paste0("contingency-", main, ".pdf") )
	}

	return( chisq )
}


# 
# D = ft.read.files( "*-deoxy.csv" )
# O = ft.read.files( "*-oxy.csv" )
# C = ft.read.files( "*-cbf.csv" )

ft.read.files = function( pat )
{
	DF = data.frame()

	F =dir( "xls/", pattern=pat, full.names = TRUE  )

	for ( f in F )
	{
		cat( "read ", f, "\n" )
		df = ft.read.csv(f)
		
		df$id = paste0( "JL", str_split_fixed( f, "_" , 4 )[,3] )
		
		DF = rbind( DF, df )
	}

	return(DF)
}


ft.read.csv = function( file )
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
