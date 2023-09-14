
two_sample_dfr_tests = function(x1, x2, n_bootstrap, name){
  #-------------------------------------------------------------------------------------
  # Test whether the null hypothesis, H0, that states that E(x1)=E(x2) should be 
  # rejected on the basis of two synchronous samples, x1 and x2.
  # The one-sided alternative hypothesis is tested: 
  # 
  # !!!
  # H1: x1 < x2 
  # !!!
  # 
  # If the reverse H1 is to be tested, just swap x1 & x2
  # 
  # 1. Student's t-test
  # 2. Bootstrap
  # 
  # Args
  # 
  # X1, x2 - the 2 samples
  # n_bootstrap - size of the bootstrap sample to be computed
  # name - name of the score (for print only)
  # 
  # Returns:
  # 
  # p_Student, p_bootstr
  # 
  # M Tsy 2022 Nov
  #-------------------------------------------------------------------------------------
  
  n=length(x1)
  u=x1 - x2
  
  #------------------------------------
  # Student-t criterion
  # NB: H0 ==> Ex(u)=0.
  
  S=sd(u)
  u_mean =mean(u)
  Sm=S/sqrt(n)
  t=u_mean / Sm
  # message(t)
  p_Student = pt(t, (n-1))  # one-sided signif level 
  
  #------------------------------------
  # Bootstrap.
  # Efron Tibshirani ch 16
  # H0: Ex(u) = 0
  #
  # 1) Create F0 -- distr under H0. Take the empir distr and shift it to have zero mean:
  #    u0 = (u[1]-u_mean,..., u[n]-u_mean)
  
  u0 = u - u_mean
  
  # 2) Generate bootstrap samples from u0 and compute u_mean_b[i_b]
  
  d_b  = matrix(0, nrow=n_bootstrap, ncol=n)
  
  for (i_b in 1:n_bootstrap){
    data  = sample(u0, size=n, replace=TRUE)
    d_b[i_b,]  = data
  }
  
  # in how many samples mean_over_n (x1_b - sppt) < m_observed?
  
  d_bm =apply(d_b,  1, mean)
  
  N=sum( d_bm < u_mean  )
  p_bootstr = N/n_bootstrap
  
  # hist(d_bm)
  # abline(v=m)
  # mean(d_bm)
  # 
  # plot(u)
  # abline(h=0)
  # 
  # plot(u0)
  # abline(h=0)
  
  message(paste(name, ":"))
  message("p_Student=", signif(p_Student,3))
  message("p_bootstr=", signif(p_bootstr,3))
  
  return(list(p_Student = p_Student, p_bootstr = p_bootstr))
}
