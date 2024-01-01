#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)





post.test.prob = function(sens,spec,pretest,like.rat){
  like.rat(sens=sens,spec=spec,pretest=pretest)*pretest
}


pre.tests = seq(0,1,0.005)
get.post.tests = function(sens,spec,pre.tests,like.rat){
  post.tests = c()
  for(i in 1:length(pre.tests)){
    post.tests[i]=post.test.prob(sens=sens,spec=spec,pretest=pre.tests[i],like.rat)
  }
  post.tests
}

get.t.posts = function(tests,like.rat){
  test.posts=list()
  for(t in tests){
    test.posts[[t$nm]]=list(
      m=get.post.tests(sens=t$sens$m,spec=t$spec$m,pre.tests,like.rat),
      l=get.post.tests(sens=t$sens$l,spec=t$spec$l,pre.tests,like.rat),
      u=get.post.tests(sens=t$sens$u,spec=t$spec$u,pre.tests,like.rat)
    )
    
  }
  test.posts
}


tests=list(
  veritor=list(
    nm='BD Veritor',
    sens=list(m=.712,l=.568,u=.823),
    spec=list(m=0.992,l=.986,u=0.996)
  ),
  Binax=list(
    nm='BinaxNOW',
    sens=list(m=.685,l=.543,u=.799),
    spec=list(m=0.994,l=0.982,u=0.998)
  ),
  Clinitest = list(
    nm='CLINITEST',
    sens=list(m=.752,l=.498,u=.903),
    spec=list(m=.987,l=.972,u=.994)
  ),
  Coris = list(
    nm="Coris",
    sens=list(m=.484,l=.361,u=.610),
    spec=list(m=.991,l=.981,u=0.996)
  ),
  Innova =  list(
    nm="Innova",
    sens=list(m=.722,l=.471,u=.884),
    spec=list(m=.993,l=.941,u=.999)
  ),
  LumiraDx =  list(
    nm="LumiraDx",
    sens=list(m=.827,l=.732,u=.894),
    spec=list(m=.969,l=.944,u=.983)
  ),
  Panbio =  list(
    nm="Panbio",
    sens=list(m=.719,l=.668,u=.764),
    spec=list(m=.995,l=.992,u=0.997)
  ),
  Rapigen =  list(
    nm="Rapigen",
    sens=list(m=.620,l=.467,u=.752),
    spec=list(m=.985,l=.940,u=.996)
  ),
  Sofia =  list(
    nm="Sofia",
    sens=list(m=.764,l=.709,u=.812),
    spec=list(m=.990,l=.983,u=0.994)
  ),
  StandardF =  list(
    nm="StandardF",
    sens=list(m=.664,l=.573,u=.744),
    spec=list(m=.979,l=.969,u=.985)
  ),
  StandardQ =  list(
    nm="StandardQ",
    sens=list(m=.709,l=.659,u=.755),
    spec=list(m=.988,l=.982,u=0.992)
  ),
  StandardQnasal =  list(
    nm="StandardQnasal",
    sens=list(m=.814,l=.738,u=.872),
    spec=list(m=.991,l=.984,u=0.995)
  )
)

test.names = as.vector(unlist(lapply(tests,function(x){x['nm']})))

plot.p = function(lns,post.label,xlab,
                  leg.labels,main){
  
  lwd.point = 2
  plot(c(0,0),pch=0,xlim=c(0,1),ylim=c(0,1),
       xlab=xlab,ylab=post.label,
       lty=1,lwd=lwd.point,main=main,col='white')
  
  xseq = seq(0,1,0.1)
  yseq = seq(0,1,0.1)
  grid.col=rgb(0,0,0,alpha=0.1)
  for(x in xseq){
    abline(v=x,col=grid.col)
  }
  
  for(y in yseq){
    abline(h=y,col=grid.col)
  }
  colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  nl = length(lns)
  for (i in 1:nl){
    lines(pre.tests,lns[[i]]$m,lty=1,lwd=lwd.point,col=colorBlindBlack8[i])
    lines(pre.tests,lns[[i]]$u,lty=2,col=colorBlindBlack8[i])
    lines(pre.tests,lns[[i]]$l,lty=2,col=colorBlindBlack8[i])  
    
    #rgbval = paste(as.vector(col2rgb(colorBlindBlack8[i])), collapse = " ")
    #lines(pre.tests,lns[[i]]$u,lty=2,col=rgb(rgbval[1],rgbval[2],rgbval[3]))
    #lines(pre.tests,lns[[i]]$l,lty=2,col=rgb(rgbval[1],rgbval[2],rgbval[3]))
    
  }
  
  
  #legend("topleft",legend=leg.labels,bg='white',
  #       lty=rep(1,nl),col=colorBlindBlack8[c(1:nl)],lwd=c(lwd.point,lwd.point))
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Post-test probability of a Covid-19 rapid test"),

    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          HTML("<p>Welcome! If you are taking a test because you are sick - sorry that you are sick! 
               This page is designed to help with the interpretation of a Covid-19 test result. Use this tool at your own risk; interpreting a test is difficult for the following reasons.</p>"),
          #HTML("<p><ul><li></li></ul></p>"),
          HTML("<p><ul><li>Since the sensitivity and specificity estimates used in the plots here do not take into account covariates (this need not be the case; see <a href='https://www.fharrell.com/post/backwards-probs/'>here</a> for more discussion), pre-test probability cannot take into account information that impacts sensitivity and specificity. This includes, for example, symptoms, which were shown to affect sensitivity by <a href='https://www.cdc.gov/mmwr/volumes/70/wr/pdfs/mm7003e3-H.pdf'>
              Prince-Guerra et al., 2021</a>. </li></ul></p>"),
          HTML("<p><ul><li>Because they are possibly independent of the test result, and therefore independent of sensitivity and specificity, covariates like season and sick contacts might be fair game to take into consideration for pre-test probability. However, it is difficult to know.</li></ul></p>"),
          HTML("<p><ul><li>It's important to consider the output here as a piece of evidence, and not the true answer. If you do have symptoms, for example, note that your true post-test probability will be greater than what you compute here.</li></ul></p>"),
          HTML("<p><ul><li>Test characteristics are estimated using samples that may not generalize to your situation. For more on the estimates used here, see <a href='https://pubmed.ncbi.nlm.nih.gov/35617375/'>
              Brummer et al., 2022.</a> </li></ul></p>"),
          HTML("<p>For more discussion on the points above, see
               <a href='https://github.com/samuelweisenthal/BinaxNOWpostTestProb/blob/main/covid.post.test.pdf'>
              here</a>.</p>"),
          HTML("<p>All this said, this tool might be helpful in some cases. To use it, select a test from the drop-down menu. Select your test result. Find your pre-test probability on the horizontal axis. Trace up to find your post-test probability.</p>"),
          HTML("<p>As said, use at your own risk. I am though always interested to improve the tool. Feel free to send any feedback to samweisenthal@gmail.com.</p>")
          
          ),
        
    
        
        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(column(4,
          selectInput("variable", "Test brand:",
                      test.names)),
          column(4, offset = 1,
          selectInput("result", "Test result:",
                      c("Negative","Positive")),
          )),
          
          plotOutput("distPlot"),
          HTML("<p>The plots above are based on the following formulas, which are applications of <a href='https://en.wikipedia.org/wiki/Bayes%27_theorem'>
              Bayes'rule:</a></p>"),
          withMathJax("$$p(dz+|test-)=\\frac{(1-p(test+|dz+))}{(1-p(test+|dz+))p(dz+) + p(test-|dz-)(1-p(dz+))}p(dz+),$$"),
          withMathJax("$$p(dz+|test+)=\\frac{p(test+|dz+)}{p(test+|dz+)p(dz+) + (1-p(test-|dz-))(1-p(dz+))}p(dz+).$$"),
          #withMathJax("where $p(dz+|test-)$ is post-test probability, $p(test+|dz+)$ is sensitivity, $p(test-|dz-)$ is specificity, and $p(dz+)$ is pre-test probability."),
          HTML("<p>The sensitivity, p(dz+|test+), and specificity, p(dz-|test-), of each test are based on the pooled estimates from 
               <a href='https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1004011'>
              Brummer et al, 2022.</a> You can see that these sensitivity and specificity expressions do not depend on covariates, such as symptoms. The dashed lines correspond to the bounds of the confidence intervals reported in the same paper.</p>")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R

        test.ix = which(test.names==input$variable)
        
        test.data = tests[[test.ix]]
        mymain = test.data$nm
        
        if (input$result=='Negative'){
          like.rat=function(sens,spec,pretest){
            (1-sens)/((1-sens)*pretest+(spec)*(1-pretest))        
          }
          
          ylab =    "Post-test probability: p(dz+|test-)"
          
        }else{
          
          like.rat=function(sens,spec,pretest){
            (sens)/((sens)*pretest+(1-spec)*(1-pretest))        
          }
          
          
          ylab = "Post-test probability: p(dz+|test+)"
        }
        
        
        
        
        test.posts = get.t.posts(tests[test.ix],like.rat)
        plot.p(test.posts,
               post.label=ylab,xlab="Pre-test probability: p(dz+)",
               leg.labels=names(test.posts),main=mymain
        )
        
        
    
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #      xlab = 'Waiting time to next eruption (in mins)',
        #      main = mymain)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
