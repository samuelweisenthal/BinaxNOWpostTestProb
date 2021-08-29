# To compute

like.rat=function(sens,spec,pretest){(1-sens)/((1-sens)*pretest+(spec)*(1-pretest))}
pretest=0.2

post.test.prob = function(sens,spec,pretest){
  like.rat(sens=sens,spec=spec,pretest=pretest)*pretest
}


pre.tests = seq(0,1,0.01)
get.post.tests = function(sens,spec,pretests){
post.tests = c()
for(i in 1:length(pre.tests)){
  post.tests[i]=post.test.prob(sens=sens,spec=spec,pretest=pre.tests[i])
}
post.tests
}


post.tests.6=get.post.tests(sens=0.64,spec=1,pretests)
plot(pre.tests,post.tests.6,type='l',xlab="Pre.test: P(dz+)",ylab="Post.test: P(dz+|test-)",lty=1)
post.tests.36=get.post.tests(sens=0.36,spec=1,pretests)
lines(pre.tests,post.tests.36,lty=2)
#abline(v=0.2)
legend("topleft",c("sens=0.70 (Prince-Guerra 2021)","sens=0.36 (Pollack 2021)"),lty=c(1,2))
#post.test.knox
#abline(h=post.test.knox) 

# With serial testing, you update your value on the x axis to reflect your previous post test probability
